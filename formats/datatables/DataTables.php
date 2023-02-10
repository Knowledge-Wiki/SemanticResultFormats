<?php

namespace SRF;

use Html;
use SMW\Query\ResultPrinters\TableResultPrinter;
use SMW\Query\ResultPrinters\PrefixParameterProcessor;
use SMW\DIWikiPage;
use SMW\Message;
use SMW\Query\PrintRequest;
use SMW\Query\QueryStringifier;
use SMW\Utils\HtmlTable;
use SMWDataValue;
use SMWDIBlob as DIBlob;
use SMWQueryResult as QueryResult;
use SMWResultArray as ResultArray;

/**
 * DataTables and SMWAPI.
 *
 * @since 1.9
 * @license GPL-2.0-or-later
 *
 * @author mwjames
 * @author thomas-topway-it
 */
class DataTables extends TableResultPrinter {

	/**
	 * @var HtmlTable
	 */
	private $htmlTable;

	private $prefixParameterProcessor;

	private $countValue;

	public function getParamDefinitions( array $definitions ) {

		$params = parent::getParamDefinitions( $definitions );

		$params['class'] = [
			'name' => 'class',
			'message' => 'smw-paramdesc-table-class',
			'default' => 'srf-datatable',
		];

		$params['transpose'] = [
			'type' => 'boolean',
			'default' => false,
			'message' => 'smw-paramdesc-table-transpose',
		];

		$params['sep'] = [
			'message' => 'smw-paramdesc-sep',
			'default' => '',
		];

		$params['theme'] = [
			'message' => 'srf-paramdesc-theme',
			'default' => 'basic',
			'values' => [ 'bootstrap', 'basic' ] // feel free to add more designs
		];

		$params['pagelength'] = [
			'message' => 'srf-paramdesc-pagelength',
			'default' => '20',
		];

		$params['lengthmenu'] = [
			'type' => 'string',
			'message' => 'srf-paramdesc-lengthmenu',
			'default' => '',
		];

		$params['columnstype'] = [
			'type' => 'string',
			'message' => 'srf-paramdesc-datatables-columnstype',
			'default' => '',
		];

		$params['prefix'] = [
			'message' => 'smw-paramdesc-prefix',
			'default' => 'none',
			'values' => [ 'all', 'subject', 'none', 'auto' ],
		];

		return $params;
	}

	/**
	 * @see ResultPrinter::getResultText
	 *
	 * {@inheritDoc}
	 */
	protected function getResultText( QueryResult $res, $outputMode ) {
		$query = $res->getQuery();
		$this->countValue = $query->getOption( 'max' );
		$this->prefixParameterProcessor = new PrefixParameterProcessor( $query, $this->params['prefix'] );

		$this->isHTML = ( $outputMode === SMW_OUTPUT_HTML );
		$this->isDataTable = true;
		$class = isset( $this->params['class'] ) ? $this->params['class'] : '';

		// if ( strpos( $class, 'datatable' ) !== false && $this->mShowHeaders !== SMW_HEADERS_HIDE ) {
		// 	$this->isDataTable = true;
		// }

		$this->htmlTable = new HtmlTable();

		$columnClasses = [];
		$headerList = [];

		// Default cell value separator
		if ( !isset( $this->params['sep'] ) || $this->params['sep'] === '' ) {
			$this->params['sep'] = '<br>';
		}

		// building headers
		if ( $this->mShowHeaders != SMW_HEADERS_HIDE ) {
			$isPlain = $this->mShowHeaders == SMW_HEADERS_PLAIN;
			foreach ( $res->getPrintRequests() as /* SMWPrintRequest */ $pr ) {
				$attributes = [];
				$columnClass = str_replace( [ ' ', '_' ], '-', strip_tags( $pr->getText( SMW_OUTPUT_WIKI ) ) );
				$attributes['class'] = $columnClass;
				// Also add this to the array of classes, for
				// use in displaying each row.
				$columnClasses[] = $columnClass;

				// #2702 Use a fixed output on a requested plain printout
				$mode = $this->isHTML && $isPlain ? SMW_OUTPUT_WIKI : $outputMode;
				$text = $pr->getText( $mode, ( $isPlain ? null : $this->mLinker ) );
				$headerList[] = $pr->getCanonicalLabel();
				$this->htmlTable->header( ( $text === '' ? '&nbsp;' : $text ), $attributes );
			}
		}

		$rowNumber = 0;

		while ( $subject = $res->getNext() ) {
			$rowNumber++;
			$this->getRowForSubject( $subject, $outputMode, $columnClasses );

			$this->htmlTable->row(
				[
					'data-row-number' => $rowNumber
				]
			);
		}

		// print further results footer
		if ( $this->linkFurtherResults( $res ) ) {
			$link = $this->getFurtherResultsLink( $res, $outputMode );

			$this->htmlTable->cell(
					$link->getText( $outputMode, $this->mLinker ),
					[ 'class' => 'sortbottom', 'colspan' => $res->getColumnCount() ]
			);

			$this->htmlTable->row( [ 'class' => 'smwfooter' ] );
		}

		$tableAttrs = [ 'class' => $class ];

		if ( $this->mFormat == 'broadtable' ) {
			$tableAttrs['width'] = '100%';
			$tableAttrs['class'] .= ' broadtable';
		}

		if ( $this->isDataTable ) {
			$this->addDataTableAttrs(
				$res,
				$headerList,
				$tableAttrs
			);
		}

		$transpose = $this->mShowHeaders !== SMW_HEADERS_HIDE && ( $this->params['transpose'] ?? false );

		$html = $this->htmlTable->table(
			$tableAttrs,
			$transpose,
			$this->isHTML
		);

		if ( $this->isDataTable ) {

			// Simple approximation to avoid a massive text reflow once the DT JS
			// has finished processing the HTML table
			$count = ( $this->params['transpose'] ?? false ) ? $res->getColumnCount() : $res->getCount();
			$height = ( min( ( $count + ( $res->hasFurtherResults() ? 1 : 0 ) ), 10 ) * 50 ) + 40;

			$html = Html::rawElement(
				'div',
				[
					'class' => 'smw-datatable smw-placeholder is-disabled smw-flex-center' . (
						$this->params['class'] !== '' ? ' ' . $this->params['class'] : ''
					),
					'style'     => "height:{$height}px;"
				],
				Html::rawElement(
					'span',
					[
						'class' => 'smw-overlay-spinner medium flex'
					]
				) . $html
			);
		}

		return $html;
	}

	/**
	 * Gets a table cell for all values of a property of a subject.
	 *
	 * @since 1.6.1
	 *
	 * @param SMWResultArray $resultArray
	 * @param int $outputMode
	 * @param string $columnClass
	 *
	 * @return string
	 */
	protected function getCellForPropVals( ResultArray $resultArray, $outputMode, $columnClass ) {
		/** @var SMWDataValue[] $dataValues */
		$dataValues = [];

		while ( ( $dv = $resultArray->getNextDataValue() ) !== false ) {
			$dataValues[] = $dv;
		}

		$printRequest = $resultArray->getPrintRequest();
		$printRequestType = $printRequest->getTypeID();

		$cellTypeClass = " smwtype$printRequestType";

		// We would like the cell class to always be defined, even if the cell itself is empty
		$attributes = [
			'class' => $columnClass . $cellTypeClass
		];

		$content = null;

		if ( count( $dataValues ) > 0 ) {
			$sortKey = $dataValues[0]->getDataItem()->getSortKey();
			$dataValueType = $dataValues[0]->getTypeID();

			// The data value type might differ from the print request type - override in this case
			if ( $dataValueType !== '' && $dataValueType !== $printRequestType ) {
				$attributes['class'] = "$columnClass smwtype$dataValueType";
			}

			if ( is_numeric( $sortKey ) ) {
				$attributes['data-sort-value'] = $sortKey;
			}

			if ( $this->isDataTable && $sortKey !== '' ) {
				$attributes['data-order'] = $sortKey;
			}

			$alignment = trim( $printRequest->getParameter( 'align' ) );

			if ( in_array( $alignment, [ 'right', 'left', 'center' ] ) ) {
				$attributes['style'] = "text-align:$alignment;";
			}

			$width = htmlspecialchars(
				trim( $printRequest->getParameter( 'width' ) ),
				ENT_QUOTES
			);

			if ( $width ) {
				$attributes['style'] = ( isset( $attributes['style'] ) ? $attributes['style'] . ' ' : '' ) . "width:$width;";
			}

			$content = $this->getCellContent(
				$dataValues,
				$outputMode,
				$printRequest->getMode() == PrintRequest::PRINT_THIS
			);
		}

		// Sort the cell HTML attributes, to make test behavior more deterministic
		ksort( $attributes );

		$this->htmlTable->cell( $content, $attributes );
	}

	/**
	 * Gets the contents for a table cell for all values of a property of a subject.
	 *
	 * @since 1.6.1
	 *
	 * @param SMWDataValue[] $dataValues
	 * @param $outputMode
	 * @param boolean $isSubject
	 *
	 * @return string
	 */
	protected function getCellContent( array $dataValues, $outputMode, $isSubject ) {
		$values = [];

		foreach ( $dataValues as $dv ) {
			$dataValueMethod = $this->prefixParameterProcessor->useLongText( $isSubject ) ? 'getLongText' : 'getShortText';

			// Restore output in Special:Ask on:
			// - file/image parsing
			// - text formatting on string elements including italic, bold etc.
			if ( $outputMode === SMW_OUTPUT_HTML && $dv->getDataItem() instanceof DIWikiPage && $dv->getDataItem()->getNamespace() === NS_FILE ||
				$outputMode === SMW_OUTPUT_HTML && $dv->getDataItem() instanceof DIBlob ) {
				// Too lazy to handle the Parser object and besides the Message
				// parse does the job and ensures no other hook is executed
				$value = Message::get(
					[ 'smw-parse', $dv->$dataValueMethod( SMW_OUTPUT_WIKI, $this->getLinker( $isSubject ) ) ],
					Message::PARSE
				);
			} else {
				$value = $dv->$dataValueMethod( $outputMode, $this->getLinker( $isSubject ) );
			}


			$values[] = $value === '' ? '&nbsp;' : $value;
		}

		$sep = strtolower( $this->params['sep'] );

		if ( !$isSubject && $sep === 'ul' && count( $values ) > 1 ) {
			$html = '<ul><li>' . implode( '</li><li>', $values ) . '</li></ul>';
		} elseif ( !$isSubject && $sep === 'ol' && count( $values ) > 1 ) {
			$html = '<ol><li>' . implode( '</li><li>', $values ) . '</li></ol>';
		} else {
			$html = implode( $this->params['sep'], $values );
		}

		return $html;
	}

	/**
	 * @see ResultPrinter::getResources
	 */
	protected function getResources() {

/*
		$resourceFormatter = new ResourceFormatter();
		$resourceFormatter->registerResources( [
			'ext.srf.datatables',
			'ext.srf.datatables.' . $this->params['theme']
		] );
*/

		return [
			'modules' => [
				'ext.srf.datatables',
				'ext.srf.datatables.' . $this->params['theme'],
				// 'smw.tableprinter.datatable.styles'
				// 'ext.smw.deferred'
			],
			'styles' => [
			],
			'messages' => [
				"smw-format-datatable-emptytable",
				"smw-format-datatable-info",
				"smw-format-datatable-infoempty",
				"smw-format-datatable-infofiltered",
				"smw-format-datatable-infothousands",
				"smw-format-datatable-lengthmenu",
				"smw-format-datatable-loadingrecords",
				"smw-format-datatable-processing",
				"smw-format-datatable-search",
				"smw-format-datatable-zerorecords",
				"smw-format-datatable-first",
				"smw-format-datatable-last",
				"smw-format-datatable-next",
				"smw-format-datatable-previous",
				"smw-format-datatable-sortascending",
				"smw-format-datatable-sortdescending",
				"smw-format-datatable-toolbar-export"
			],
			'targets' => [ 'mobile', 'desktop' ]
		];
	}

	/**
	 * Gets a single table row for a subject, ie page.
	 *
	 * @since 1.6.1
	 *
	 * @param SMWResultArray[] $subject
	 * @param int $outputMode
	 * @param string[] $columnClasses
	 *
	 * @return string
	 */
	private function getRowForSubject( array $subject, $outputMode, array $columnClasses ) {
		foreach ( $subject as $i => $field ) {
			// $columnClasses will be empty if "headers=hide"
			// was set.
			if ( array_key_exists( $i, $columnClasses ) ) {
				$columnClass = $columnClasses[$i];
			} else {
				$columnClass = null;
			}

			$this->getCellForPropVals( $field, $outputMode, $columnClass );
		}
	}

	private function addDataTableAttrs( $res, $headerList, &$tableAttrs ) {

		$tableAttrs['class'] = 'datatable smw-deferred-query';
		$tableAttrs['width'] = '100%';
		$tableAttrs['style'] = 'opacity:.0; display:none;';

		$tableAttrs['data-column-sort'] = json_encode(
			[
				'list'  => $headerList,
				'sort'  => $this->params['sort'],
				'order' => $this->params['order']
			]
		);

		$tableAttrs['data-query'] = QueryStringifier::toJson(
			$res->getQuery()
		);

		$tableAttrs['data-count'] = $this->countValue;
		$tableAttrs['data-theme'] = $this->params['theme'];
		$tableAttrs['data-columnstype'] = ( !empty( $this->params['columnstype'] ) ? $this->params['columnstype'] : null );
		$tableAttrs['data-collation'] = !empty( $GLOBALS['smwgEntityCollation'] ) ? $GLOBALS['smwgEntityCollation'] : $GLOBALS['wgCategoryCollation'];
	}

}
