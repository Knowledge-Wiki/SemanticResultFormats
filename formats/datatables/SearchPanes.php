<?php
/**
 * SRF DataTables and SMWAPI.
 *
 * @see http://datatables.net/
 *
 * @license GPL-2.0-or-later
 * @author thomas-topway-it for KM-A
 */

namespace SRF\DataTables;

use SMW\DataTypeRegistry;
use SMW\DataValueFactory;
use SMW\DIProperty;
use SMW\DIWikiPage;
use SMW\Query\PrintRequest;
use SMW\Services\ServicesFactory as ApplicationFactory;
use SMW\SQLStore\QueryEngine\QuerySegment;
use SMW\SQLStore\QueryEngineFactory;
use SMW\SQLStore\SQLStore;
use SMW\SQLStore\TableBuilder\FieldType;
use SMWDataItem as DataItem;
use SMWQueryProcessor;
use SRF\DataTables;

class SearchPanes {

	private array $searchPanesLog = [];

	private ?QueryEngineFactory $queryEngineFactory = null;

	private $connection;

	public function __construct(
		private DataTables $datatables
	) {
	}

	public function getSearchPanes( array $printRequests, array $searchPanesOptions ): array {
		$this->queryEngineFactory = new QueryEngineFactory( $this->datatables->store );
		$this->connection = $this->datatables->store->getConnection( 'mw.db.queryengine' );

		$ret = [];
		foreach ( $printRequests as $i => $printRequest ) {
			if ( count( $searchPanesOptions['columns'] ) && !in_array( $i, $searchPanesOptions['columns'] ) ) {
				continue;
			}

			$parameterOptions = $this->datatables->printoutsParametersOptions[$i];

			$searchPanesParameterOptions = ( array_key_exists( 'searchPanes', $parameterOptions ) ?
				$parameterOptions['searchPanes'] : [] );

			if ( array_key_exists( 'show', $searchPanesParameterOptions ) && $searchPanesParameterOptions['show'] === false ) {
				continue;
			}

			$canonicalLabel = ( $printRequest->getMode() !== PrintRequest::PRINT_THIS ?
				$printRequest->getCanonicalLabel() : '' );

			$ret[$i] = $this->getPanesOptions( $printRequest, $canonicalLabel, $searchPanesOptions, $searchPanesParameterOptions );
		}

		return $ret;
	}

	public function getLog(): array {
		return $this->searchPanesLog;
	}

	private function getPanesOptions(
		PrintRequest $printRequest,
		string $canonicalLabel,
		array $searchPanesOptions,
		array $searchPanesParameterOptions
	): array {
		if ( empty( $canonicalLabel ) ) {
			return $this->searchPanesMainlabel( $printRequest, $searchPanesOptions, $searchPanesParameterOptions );
		}

		$queryParams = [
			'limit' => $this->datatables->query->getLimit(),
			'offset' => $this->datatables->query->getOffset(),
			'mainlabel' => $this->datatables->query->getMainlabel()
		];
		$queryParams = SMWQueryProcessor::getProcessedParams( $queryParams, [] );

		$isCategory = $printRequest->getMode() === PrintRequest::PRINT_CATS;

		$newQuery = SMWQueryProcessor::createQuery(
			$this->datatables->query->getQueryString() . ( !$isCategory ? '[[' . $canonicalLabel . '::+]]' : '' ),
			$queryParams,
			SMWQueryProcessor::INLINE_QUERY,
			''
		);

		$queryDescription = $newQuery->getDescription();
		$queryDescription->setPrintRequests( [ $printRequest ] );

		$conditionBuilder = $this->queryEngineFactory->newConditionBuilder();
		$rootid = $conditionBuilder->buildCondition( $newQuery );

		QuerySegment::$qnum = 0;
		$querySegmentList = $conditionBuilder->getQuerySegmentList();

		$querySegmentListProcessor = $this->queryEngineFactory->newQuerySegmentListProcessor();
		$querySegmentListProcessor->setQuerySegmentList( $querySegmentList );
		$querySegmentListProcessor->process( $rootid );

		$qobj = $querySegmentList[$rootid];

		$property = new DIProperty( DIProperty::newFromUserLabel( $printRequest->getCanonicalLabel() ) );
		$propTypeid = $property->findPropertyValueType();

		$where = trim($qobj->where);

		$where = preg_replace('/\s*LIMIT\s+\d+\s*/i', '', $where);

		while (preg_match('/^\((.*)\)$/s', $where, $matches)) {
			$where = trim($matches[1]);
		}
		$where = trim($where);
		if ($where) {
			$where = '(' . $where . ')';
		}


		if ( $isCategory ) {
			$sql_options = ' LIMIT 1';

			$query = 'SELECT COUNT(*) as count' .
					 ' FROM ' . "$qobj->joinTable AS $qobj->alias" . $qobj->from .
					 ' JOIN ' . $this->connection->tableName( 'smw_fpt_inst' ) . " AS insts ON $qobj->alias.smw_id = insts.s_id" .
					 ( $where ? ' WHERE ' . $where : '' ) .
					 $sql_options;

			$res = $this->connection->query(
				$query,
				__METHOD__
			);
			$row = $res->fetchRow();
			$dataLength = (int)( $row['count'] ?? 0 );
			if ( !$dataLength ) {
				return [];
			}

			$groupBy = "i.smw_id";
			$orderBy = "count DESC, $groupBy ASC";
			$sql_options =
				' GROUP BY ' . $groupBy .
				' HAVING ' . 'count >= ' . $searchPanesOptions['minCount'] .
				' ORDER BY ' . $orderBy .
				' LIMIT ' . $dataLength;

			$query = "SELECT COUNT($groupBy) AS count, i.smw_id, i.smw_title, i.smw_namespace, i.smw_iw, i.smw_sort, i.smw_subobject" .
					 ' FROM ' . "$qobj->joinTable AS $qobj->alias" . $qobj->from .
					 ' JOIN ' . $this->connection->tableName( 'smw_fpt_inst' ) . " AS insts ON $qobj->alias.smw_id = insts.s_id" .
					 ' JOIN ' . $this->connection->tableName( SQLStore::ID_TABLE ) . " AS i ON i.smw_id = insts.o_id" .
					 ( $where ? ' WHERE ' . $where : '' ) .
					 $sql_options;

			$res = $this->connection->query(
				$query,
				__METHOD__
			);

			$isIdField = true;
		} else {
			$tableid = $this->datatables->store->findPropertyTableID( $property );
			$querySegmentList = array_reverse( $querySegmentList );

			$entityIdManager = $this->datatables->store->getObjectIds();
			$pid = $entityIdManager->getSMWPropertyID( $property );

			$p_alias = null;
			foreach ( $querySegmentList as $segment ) {
				if ( $segment->joinTable === $tableid ) {
					if ( isset( $segment->where ) && $segment->where !== '' ) {
						$pattern = '/' . preg_quote( $segment->alias, '/' ) . '\.p_id\s*=\s*' . preg_quote( (string)$pid, '/' ) . '/';
						if ( preg_match( $pattern, $segment->where ) ) {
							$p_alias = $segment->alias;
							break;
						}
					} else {
						if ( $p_alias === null ) {
							 $p_alias = $segment->alias;
						}
					}
				}
			}

			if ( empty( $p_alias ) ) {
				$this->searchPanesLog[] = [
					'canonicalLabel' => $printRequest->getCanonicalLabel(),
					'error' => '$p_alias is null',
					'tableid' => $tableid,
					'expected_pid' => $pid,
				];
				return [];
			}


			$sql_options = ' LIMIT 1';

			$query = 'SELECT COUNT(*) as count' .
					 ' FROM ' . "$qobj->joinTable AS $qobj->alias" . $qobj->from .
					 ( $where ? ' WHERE ' . $where : '' ) .
					 $sql_options;


			$res = $this->connection->query(
				$query,
				__METHOD__
			);
			$row = $res->fetchRow();
			$dataLength = (int)( $row['count'] ?? 0 );

			if ( !$dataLength ) {
				return [];
			}

			list( $diType, $isIdField, $fields, $groupBy, $orderBy ) = $this->fetchValuesByGroup( $property, $p_alias, $propTypeid );

			$sql_options =
				' GROUP BY ' . $groupBy .
				' HAVING ' . 'count >= ' . $searchPanesOptions['minCount'] .
				' ORDER BY ' . $orderBy .
				' LIMIT ' . $dataLength;

			$query = 'SELECT ' . implode( ',', $fields ) .
					 ' FROM ' . "$qobj->joinTable AS $qobj->alias" . $qobj->from .
					 ( !$isIdField ? '' : " JOIN " . $this->connection->tableName( SQLStore::ID_TABLE ) . " AS `i` ON ($p_alias.o_id = i.smw_id)" ) .
					 ( $where ? ' WHERE ' . $where . ( !$isIdField ? '' : ' AND' .
					 ' i.smw_iw!=' . $this->connection->addQuotes( SMW_SQL3_SMWIW_OUTDATED ) .
					 ' AND i.smw_iw!=' . $this->connection->addQuotes( SMW_SQL3_SMWDELETEIW ) ) : '' ) .
					 $sql_options;


			$res = $this->connection->query(
				$query,
				__METHOD__
			);
		}

		$diTypeForHandler = $isIdField ? DataItem::TYPE_WIKIPAGE : $diType;
		$diHandler = $this->datatables->store->getDataItemHandlerForDIType( $diTypeForHandler );

		$groups = [];

		$outputMode = SMW_OUTPUT_HTML;
		$isSubject = false;

		$threshold = !empty( $searchPanesParameterOptions['threshold'] ) ?
			$searchPanesParameterOptions['threshold'] : $searchPanesOptions['threshold'];

		$this->searchPanesLog[] = [
			'canonicalLabel' => $printRequest->getCanonicalLabel(),
			'threshold' => $threshold,
            'dataLength' => $dataLength ?? 'N/A',
		];


		$propTypeId = $isIdField ? '_wpg' : $propTypeid;

		foreach ( $res as $row ) {
			try {
                if ( $isIdField ) {
                    $dataItem = $diHandler->dataItemFromDBKeys( [
                        $row->smw_title,
                        intval( $row->smw_namespace ),
                        $row->smw_iw,
                        '',
                        $row->smw_subobject
                    ] );
                } else {
                    $dbkeys = [];
                    $expectedFields = $diHandler->getFetchFields();

                    foreach ( $expectedFields as $field => $type ) {
                        if ( property_exists( $row, $field ) ) {
                            if ( $row->$field === null || $row->$field === '' ) {
                                if ( strpos( $field, 'sortkey' ) !== false || strpos( $field, 'serialized' ) !== false ) {
                                    $dbkeys[] = '0';
                                } else {
                                    $dbkeys[] = '';
                                }
                            } else {
                                $dbkeys[] = $row->$field;
                            }
                        } else {
                            if ( strpos( $field, 'sortkey' ) !== false || strpos( $field, 'serialized' ) !== false ) {
                                $dbkeys[] = '0';
                            } else {
                                $dbkeys[] = '';
                            }
                        }
                    }

                    $finalDbKeys = count( $dbkeys ) > 1 ? $dbkeys : ( !empty( $dbkeys ) ? $dbkeys[0] : '' );

                    $dataItem = $diHandler->dataItemFromDBKeys( $finalDbKeys );
                }

                $dataValue = DataValueFactory::getInstance()->newDataValueByItem(
                    $dataItem,
                    $property
                );

                if ( $printRequest->getOutputFormat() ) {
                    $dataValue->setOutputFormat( $printRequest->getOutputFormat() );
                }

                $cellContentArray = $this->datatables->getCellContent(
                    $printRequest->getCanonicalLabel(),
                    [ $dataValue ],
                    $outputMode,
                    $isSubject
                );
                $cellContent = $cellContentArray['display'] ?? '';

                if ( !array_key_exists( $cellContent, $groups ) ) {
                    $groups[$cellContent] = [
                        'count' => 0,
                        'value' => $dataValue->getWikiValue(),
                        'minDate' => null,
                        'maxDate' => null
                    ];
                }

                $groups[$cellContent]['count'] += (int)($row->count ?? 1);

                $value = $dataValue->getWikiValue();

                switch ( $dataItem->getDiType() ) {
                    case DataItem::TYPE_NUMBER:
                        if ( $outputFormat === '-u' ) {
                            $value = '*';
                        } else {
                            $value = $dataValue->getNumber();
                        }
                        break;
                    case DataItem::TYPE_BLOB:
                        $value = $dataValue->getWikiValue();
                        break;
                    case DataItem::TYPE_BOOLEAN:
                        $value = $dataValue->getWikiValue();
                        break;
                    case DataItem::TYPE_URI:
                        $value = $dataValue->getWikiValue();
                        break;
                    case DataItem::TYPE_TIME:
                        $currentDate = $dataItem->asDateTime()->getTimestamp();
                        $value = $dataValue->getISO8601Date();
                        if ( $currentDate < $groups[$cellContent]['minDate'] ) {
                            $groups[$cellContent]['minDate'] = $currentDate;
                        }
                        if ( $currentDate > $groups[$cellContent]['maxDate'] ) {
                            $groups[$cellContent]['maxDate'] = $currentDate;
                        }
                        break;
                    case DataItem::TYPE_GEO:
                        $value = $dataValue->getWikiValue();
                        break;
                    case DataItem::TYPE_CONTAINER:
                        $value = $dataValue->getWikiValue();
                        break;
                    case DataItem::TYPE_WIKIPAGE:
                        $title_ = $dataValue->getTitle();
                        if ( $title_ ) {
                            $value = $title_->getFullText();
                        } else {
                            $value = $dataValue->getWikiValue();
                            $this->searchPanesLog[] = [
                                'canonicalLabel' => $printRequest->getCanonicalLabel(),
                                'error' => 'TYPE_WIKIPAGE title is null',
                                'wikiValue' => $value,
                            ];
                        }
                        break;
                    case DataItem::TYPE_CONCEPT:
                        $value = $dataValue->getWikiValue();
                        break;
                    case DataItem::TYPE_PROPERTY:
                        break;
                    case DataItem::TYPE_NOTYPE:
                        $value = $dataValue->getWikiValue();
                        break;
                    default:
                        $value = $dataValue->getWikiValue();
                        break;
                }

                $groups[$cellContent]['value'] = $value;

                if ( $propTypeId === '_dat' ) {
                    $dataValue->setOutputFormat( 'raw' );
                    $rawValue = $dataValue->getWikiValue();
                    if ( strpos( $rawValue, '/' ) !== false ) {
                        $rawValue = explode( '/', $rawValue )[0];
                    }
                    $rawValue = explode( ' ', $rawValue )[0];
                    $ts = strtotime( $rawValue );
                    if ( $ts !== false ) {
                        if ( $groups[$cellContent]['minDate'] === null || $ts < $groups[$cellContent]['minDate'] ) {
                            $groups[$cellContent]['minDate'] = $ts;
                        }
                        if ( $groups[$cellContent]['maxDate'] === null || $ts > $groups[$cellContent]['maxDate'] ) {
                            $groups[$cellContent]['maxDate'] = $ts;
                        }
                    }
                }
            } catch ( \SMW\SQLStore\EntityStore\Exception\DataItemHandlerException $e ) {
                error_log( 'SearchPanes DataItemHandlerException for ' . $printRequest->getCanonicalLabel() . ': ' . $e->getMessage() .
                           '. Row data: ' . json_encode( $row, JSON_UNESCAPED_UNICODE ) .
                           '. Constructed dbKeys: ' . json_encode( $finalDbKeys ?? $dbkeys ?? 'N/A', JSON_UNESCAPED_UNICODE )
                );
                continue;
            } catch ( \Exception $e ) {
                error_log( 'SearchPanes Unexpected Exception for ' . $printRequest->getCanonicalLabel() . ': ' . $e->getMessage() .
                           '. Row data: ' . json_encode( $row, JSON_UNESCAPED_UNICODE )
                );
                continue;
            }
		}

		arsort( $groups, SORT_NUMERIC );

		$ret = [];
		foreach ( $groups as $content => $value ) {
			if ( array_key_exists( 'minDate', $value ) && $value['minDate'] !== null && $value['maxDate'] !== null && $value['minDate'] != $value['maxDate'] ) {
				$value['value'] = '>' . date( 'c', $value['minDate'] ) . ']][[' . $printRequest->getCanonicalLabel() . '::<' . date( 'c', $value['maxDate'] );
			}

			$ret[] = [
				'label' => $content,
				'count' => $value['count'],
				'value' => $value['value']
			];
		}

		return $ret;
	}

	/**
	 * @see ByGroupPropertyValuesLookup
	 */
	private function fetchValuesByGroup( DIProperty $property, string $p_alias, string $propTypeId ): array {
		$tableid = $this->datatables->store->findPropertyTableID( $property );

		$proptables = $this->datatables->store->getPropertyTables();

		if ( $tableid === '' || !isset( $proptables[$tableid] ) ) {
			return [];
		}

		$connection = $this->datatables->store->getConnection( 'mw.db' );

		$propTable = $proptables[$tableid];
		$isIdField = false;

		$diHandler = $this->datatables->store->getDataItemHandlerForDIType(
			$propTable->getDiType()
		);

		foreach ( $diHandler->getFetchFields() as $field => $fieldType ) {
			if ( !$isIdField && $fieldType === FieldType::FIELD_ID ) {
				$isIdField = true;
			}
		}

		$groupBy = $diHandler->getLabelField();
		$pid = '';

		if ( $groupBy === '' ) {
			$groupBy = $diHandler->getIndexField();
		}

		$groupBy = "$p_alias.$groupBy";
		$orderBy = "count DESC, $groupBy ASC";

		$diType = $propTable->getDiType();

		if ( $diType === DataItem::TYPE_WIKIPAGE ) {
			$fields = [
				"i.smw_id",
				"i.smw_title",
				"i.smw_namespace",
				"i.smw_iw",
				"i.smw_subobject",
				"i.smw_hash",
				"i.smw_sort",
				"COUNT( $groupBy ) as count"
			];

			$groupBy = "$p_alias.o_id, i.smw_id";
			$orderBy = "count DESC, i.smw_sort ASC";
		} elseif ( $diType === DataItem::TYPE_BLOB ) {
			$fields = [ "$p_alias.o_hash, $p_alias.o_blob", "COUNT( $p_alias.o_hash ) as count" ];

			$groupBy = ( $propTypeId !== '_keyw' ? "$p_alias.o_hash, $p_alias.o_blob"
					: "$p_alias.o_hash" );

		} elseif ( $diType === DataItem::TYPE_URI ) {
			$fields = [ "$p_alias.o_serialized, $p_alias.o_blob", "COUNT( $p_alias.o_serialized ) as count" ];
			$groupBy = "$p_alias.o_serialized, $p_alias.o_blob";
		} elseif ( $diType === DataItem::TYPE_NUMBER ) {
			$fields = [ "$p_alias.o_serialized,$p_alias.o_sortkey, COUNT( $p_alias.o_serialized ) as count" ];
			$groupBy = "$p_alias.o_serialized,$p_alias.o_sortkey";
			$orderBy = "count DESC, $p_alias.o_sortkey DESC";
		} else {
			$fields = [ "$groupBy", "COUNT( $groupBy ) as count" ];
		}

		return [ $diType, $isIdField, $fields, $groupBy, $orderBy ];
	}

	private function searchPanesMainlabel( PrintRequest $printRequest, array $searchPanesOptions, array $searchPanesParameterOptions ): array {
		if ( $searchPanesOptions['minCount'] > 1 ) {
			return [];
		}

		$threshold = !empty( $searchPanesParameterOptions['threshold'] ) ?
			$searchPanesParameterOptions['threshold'] : $searchPanesOptions['threshold'];

		$this->searchPanesLog[] = [
			'canonicalLabel' => 'mainLabel',
			'threshold' => $threshold,
		];


		$query = $this->datatables->query;
		$queryDescription = $query->getDescription();
		$queryDescription->setPrintRequests( [] );

		$conditionBuilder = $this->queryEngineFactory->newConditionBuilder();
		$rootid = $conditionBuilder->buildCondition( $query );

		QuerySegment::$qnum = 0;
		$querySegmentList = $conditionBuilder->getQuerySegmentList();

		$querySegmentListProcessor = $this->queryEngineFactory->newQuerySegmentListProcessor();

		$querySegmentListProcessor->setQuerySegmentList( $querySegmentList );

		$querySegmentListProcessor->process( $rootid );

		$qobj = $querySegmentList[$rootid];

		$sql_options =
			' ORDER BY t';

		$sortfields = implode( ',', $qobj->sortfields );
		$sortfields = $sortfields ? ',' . $sortfields : '';

		$res = $this->connection->query(
			"SELECT $qobj->alias.smw_id AS id," .
			"$qobj->alias.smw_title AS t," .
			"$qobj->alias.smw_namespace AS ns," .
			"$qobj->alias.smw_iw AS iw," .
			"$qobj->alias.smw_subobject AS so," .
			"$qobj->alias.smw_sortkey AS sortkey" .
			"$sortfields" .
			' FROM ' . "$qobj->joinTable AS $qobj->alias" . $qobj->from .
			$qobj->where .
			$sql_options,
			__METHOD__
		);

		$diHandler = $this->datatables->store->getDataItemHandlerForDIType(
			DataItem::TYPE_WIKIPAGE
		);

		$outputMode = SMW_OUTPUT_HTML;
		$isSubject = false;
		$groups = [];
		foreach ( $res as $row ) {
			$dataItem = $diHandler->dataItemFromDBKeys( [
				$row->t,
				intval( $row->ns ),
				$row->iw,
				'',
				$row->so
			] );

			$dataValue = DataValueFactory::getInstance()->newDataValueByItem(
				$dataItem
			);

			if ( $printRequest->getOutputFormat() ) {
				$dataValue->setOutputFormat( $printRequest->getOutputFormat() );
			}

			$cellContent = $this->datatables->getCellContent(
				$printRequest->getCanonicalLabel(),
				[ $dataValue ],
				$outputMode,
				$isSubject
			)['display'];

			if ( !array_key_exists( $cellContent, $groups ) ) {
				$groups[$cellContent] = [ 'count' => 0, 'value' => '' ];
			}

			$groups[$cellContent]['count']++;
			$groups[$cellContent]['value'] = $dataValue->getTitle()->getText();
		}

		arsort( $groups, SORT_NUMERIC );

		$ret = [];
		foreach ( $groups as $content => $value ) {
			$ret[] = [
				'label' => $content,
				'value' => $value['value'],
				'count' => $value['count']
			];
		}

		return $ret;
	}

}
