" Vim syntax file
" Language:    Lasso 9
" Maintainer:  Brad Lindsay <sluggy.fan@gmail.com>
" Last Change: 2012
"

if exists("b:current_syntax")
  finish
endif

" Get SQL Formatting for strings
syn include @sqlTop syntax/sql.vim
syn cluster sqlTop remove=sqlString,mysqlString,sqlComment,mysqlComment

syn case ignore

syn keyword lassoBoolean false true

syn keyword lassoType array action_param action_params boolean bytes capture curl currency database_registry date dateandtime decimal delve dir duration eacher file generateForEachKeyed generateForEachUnKeyed generateSeries inline_type integer list list_node literal locale map map_node net_tcp net_tcpssl net_udp net_udppacket null object pair pairup paramdesc pdf_barcode pdf_chunk pdf_color pdf_doc pdf_font pdf_hyphenator pdf_image pdf_list pdf_paragraph pdf_phrase pdf_read pdf_table pdf_text pdf_typebase percent queue repeat serialization_element serialization_object_identity_compare serialization_reader serialization_writer serialization_writer_ref serialization_writer_standin set scientific sqlite_column sqlite_columnScanner sqlite_currentrow sqlite_db sqlite_expressionGenerator sqlite_query_stat sqlite_results sqlite_table stack staticarray string tie timeonly tree_base tree_node tree_nullNode user_registry void xml_element xml_namednodemap_attr xml_node xml_nodelist zip zip_file

syn keyword lassoKeyword abort define local var variable
syn keyword lassoDatabase column field inline records resultset rows

syn keyword lassoConditional else if match

syn keyword lassoRepeat iterate loop loop_abort loop_continue loop_count loop_key loop_value while

syn keyword lassoTypeLabel data import private protected public thread type
syn keyword lassoTraitLabel provide require trait
syn keyword lassoLabel case

syn keyword lassoException handle handle_error handle_failure protect 

syn keyword lassoInclude include include_once library library_once

" Operators
syn match lassoOperator `[-+*/%]`
syn match lassoLogicalOperator `!\|?>\@!\|&&\|||`
syn keyword lassoLogicalOperator and not or
syn match lassoAssignmentOperator `=\|:=\|+=\|-=\|/=\|%=`
syn match lassoComparisonOperator `===\|==\|!==\|!=\|<=\|>=\|<\|?\@<!>\|>>\|!>>`
syn match lassoTargetOperator `->\|&`
syn match lassoAssociationOperator `=>`

syn match lassoThreadVariable `\$[a-zA-Z_][a-zA-Z0-9_.]*`
syn match lassoLocalVariable `#[a-zA-Z_][a-zA-Z0-9_.]*` 
syn match lassoLocalVariable `#1`

syn region lassoDoubleQuote start=/"/ skip=/\\"/ end=/"/ contains=@sqlTop
syn region lassoSingleQuote start=/'/ skip=/\\'/ end=/'/ contains=@sqlTop
syn region lassoBacktickQuote start=/`/ end=/`/ contains=@sqlTop

syn match lassoComment `//.*`
syn region lassoComment  start="/\*" end="\*/"


hi def link lassoBoolean             Boolean 
hi def link lassoString              String 
hi def link lassoDoubleQuote         lassoString
hi def link lassoSingleQuote         lassoString
hi def link lassoBacktickQuote       lassoString
hi def link lassoVariable            Identifier
hi def link lassoThreadVariable      lassoVariable
hi def link lassoLocalVariable       lassoVariable
hi def link lassoComment             Comment
hi def link lassoType                Type
hi def link lassoKeyword             Keyword
hi def link lassoDatabase            lassoKeyword 
hi def link lassoLabel               Label
hi def link lassoTypeLabel           lassoLabel
hi def link lassoTraitLabel          lassoLabel
hi def link lassoConditional         Conditional
hi def link lassoRepeat              Repeat 
hi def link lassoInclude             Include 
hi def link lassoException           Exception
hi def link lassoOperator            Operator
hi def link lassoLogicalOperator     lassoOperator
hi def link lassoAssignmentOperator  lassoOperator
hi def link lassoComparisonOperator  lassoOperator
hi def link lassoTargetOperator      lassoOperator
hi def link lassoAssociationOperator lassoOperator


let b:current_syntax = "lasso"
