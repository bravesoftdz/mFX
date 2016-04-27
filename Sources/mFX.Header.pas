unit mFX.Header;

interface

{$I mFX.Inc}

Uses System.SysUtils;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

type
  TFXDBName            = String;

  PFXInt               =^FXInt;
  FXInt                = LongInt;                                               // 32 bit signed
  FXUInt               = LongWord;                                              // 32 bit unsigned
  PFXLong              =^FXLong;
  FXLong               = LongInt;                                               // 32 bit signed
  PFXULong             =^FXULong;
  FXULong              = LongWord;                                              // 32 bit unsigned
  PFXShort             =^FXShort;
  FXShort              = SmallInt;                                              // 16 bit signed
  PFXUShort            =^FXUShort;
  FXUShort             = Word;                                                  // 16 bit unsigned
  PFXFloat             =^FXFloat;
  FXFloat              = Single;                                                // 32 bit
  PFXDouble            =^FXDouble;
  FXDouble             = Double;                                                // ???
  PFXByte              =^FXByte;
  FXByte               = Byte;                                                  // 8 bit unsigned
  PISC_LONG            =^ISC_LONG;
  ISC_LONG             = LongInt;                                               // 32 bit signed
  PUISC_LONG           =^UISC_LONG;
  UISC_LONG            = LongWord;                                              // 32 bit unsigned
  PFXInt64             =^FXInt64;
  FXInt64              = Int64;                                                 // 64 bit signed
  PPISC_STATUS         =^PISC_STATUS;
  PISC_STATUS          =^ISC_STATUS;
  ISC_STATUS           = NativeInt;                                             // 32/64 bit signed
  PUISC_STATUS         =^UISC_STATUS;
  UISC_STATUS          = NativeUInt;                                            // 32/64 bit unsigned
  PFXVoid              =^FXVoid;
  FXVoid               = Pointer;

  PISC_DATE            =^ISC_DATE;
  ISC_DATE             = ISC_LONG;

  PISC_TIME            =^ISC_TIME;
  ISC_TIME             = ISC_LONG;

  PISC_TIMESTAMP = ^ISC_TIMESTAMP;
  ISC_TIMESTAMP = record
    timestamp_date: ISC_DATE;
    timestamp_time: ISC_TIME;
    end;

  PISC_QUAD =^TISC_QUAD;
  TISC_QUAD = record
    gds_quad_high : ISC_LONG;
    gds_quad_low  : UISC_LONG;
    end;


//typedef int (*FB_SHUTDOWN_CALLBACK)(const int reason, const int mask, void* arg);
  FB_SHUTDOWN_CALLBACK = Function(reason:FXInt;mask:FXInt;arg:PFXVoid) : FXInt;


  {____________________________________________________________________________}
  {* Some blob ctl structs   *}
  {* from IB help files for  *}
  {* implementing UDFs .     *}
  {* -- Taken from docs, not *}
  {*    in original ibase.h  *}
  {____________________________________________________________________________}
  TISC_BlobGetSegment = function(BlobHandle: PFXInt;
                                 Buffer: PFXByte;
                                 BufferSize: FXLong;
                                 var ResultLength: FXLong): FXShort; cdecl;
  TISC_BlobPutSegment = procedure(BlobHandle: PFXInt;
                                  Buffer: PFXByte;
                                  BufferLength: FXShort); cdecl;
  PISC_Blob = ^TISC_Blob;
  TISC_Blob = record
    GetSegment         : TISC_BlobGetSegment;
    BlobHandle         : PFXInt;
    SegmentCount       : FXLong;
    MaxSegmentLength   : FXLong;
    TotalSize          : FXLong;
    PutSegment         : TISC_BlobPutSegment;
    end;

  {____________________________________________________________________________}
  (** Firebird Handle Definitions **)
  {____________________________________________________________________________}
  TISC_ATT_HANDLE               = PFXVoid;
  PISC_ATT_HANDLE               =^TISC_ATT_HANDLE;
  TISC_BLOB_HANDLE              = PFXVoid;
  PISC_BLOB_HANDLE              =^TISC_BLOB_HANDLE;
  TISC_DB_HANDLE                = PFXVoid;
  PISC_DB_HANDLE                =^TISC_DB_HANDLE;
  TISC_FORM_HANDLE              = PFXVoid;
  PISC_FORM_HANDLE              =^TISC_FORM_HANDLE;
  TISC_REQ_HANDLE               = PFXVoid;
  PISC_REQ_HANDLE               =^TISC_REQ_HANDLE;
  TISC_STMT_HANDLE              = PFXVoid;
  PISC_STMT_HANDLE              =^TISC_STMT_HANDLE;
  TISC_SVC_HANDLE               = PFXVoid;
  PISC_SVC_HANDLE               =^TISC_SVC_HANDLE;
  TISC_TR_HANDLE                = PFXVoid;
  PISC_TR_HANDLE                =^TISC_TR_HANDLE;
  TISC_WIN_HANDLE               = PFXVoid;
  PISC_WIN_HANDLE               =^TISC_WIN_HANDLE;
  TISC_CALLBACK                 = procedure;
  ISC_SVC_HANDLE                = ISC_LONG;

  PISC_BLOB_DESC =^TISC_BLOB_DESC;
  TISC_BLOB_DESC = record
    blob_desc_subtype           : FXShort;
    blob_desc_charset           : FXShort;
    blob_desc_segment_size      : FXShort;
    blob_desc_field_name        : array[0..31] of FXByte;
    blob_desc_relation_name     : array[0..31] of FXByte;
    end;

  {____________________________________________________________________________}
  (** Blob control structure  **)
  {____________________________________________________________________________}
  TISC_BLOB_CTL_SOURCE_FUNCTION = function: ISC_STATUS;
  PISC_BLOB_CTL                 =^TISC_BLOB_CTL;
  TISC_BLOB_CTL = record
    (** Source filter **)
    ctl_source                  : TISC_BLOB_CTL_SOURCE_FUNCTION;
    (** Argument to pass to source filter **)
    ctl_source_handle           : PISC_BLOB_CTL;
    ctl_to_sub_type             : FXShort;                                    // Target type
    ctl_from_sub_type           : FXShort;                                      // Source type
    ctl_buffer_length           : FXUShort;                                      // Length of buffer
    ctl_segment_length          : FXUShort;                                    // Length of current segment
    ctl_bpb_length              : FXUShort;                                      // Length of blob parameter
    (** block **)
    ctl_bpb                     : PFXByte;                                      // Address of blob parameter
    (** block **)
    ctl_buffer                  : PFXByte;                                      // Address of segment buffer
    ctl_max_segment             : ISC_LONG;                                      // Length of longest segment
    ctl_number_segments         : ISC_LONG;                                     // Total number of segments
    ctl_total_length            : ISC_LONG;                                    // Total length of blob
    ctl_status                  : PISC_STATUS;                                  // Address of status vector
    ctl_data                    : array[0..7] of NativeInt;                     // Application specific data
    end;

  {____________________________________________________________________________}
  (** Blob stream definitions **)
  {____________________________________________________________________________}
  PISC_BLOB_STREAM              =^TISC_BLOB_STREAM;
  TISC_BLOB_STREAM = record
    bstr_blob                   : PFXVoid;                                    // Blob handle
    bstr_buffer                 : PFXByte;                                      // Address of buffer
    bstr_ptr                    : PFXByte;                                      // Next character
    bstr_length                 : FXShort;                                      // Length of buffer
    bstr_cnt                    : FXShort;                                      // Characters in buffer
    bstr_mode                   : FXByte;                                      // (mode) ? OUTPUT : INPUT
    end;

  {____________________________________________________________________________}
  (** Declare the extended SQLDA **)
  {____________________________________________________________________________}
  TXSQLVAR = record
    sqltype                     : FXShort;                                      // datatype of field
    sqlscale                    : FXShort;                                      // scale factor
    sqlsubtype                  : FXShort;                                      // datatype subtype - BLOBs & text types only
    sqllen                      : FXShort;                                      // length of data area
    sqldata                     : FXVoid;                                       // address of data
    sqlind                      : PFXShort;                                     // address of indicator
    // variable
    sqlname_length              : FXShort;                                      // length of sqlname field
    sqlname                     : array[0..31] of AnsiChar;                     // name of field, name length + space for NULL
    relname_length              : FXShort;                                      // length of relation name
    relname                     : array[0..31] of AnsiChar;                     // field's relation name + space for NULL 
    ownname_length              : FXShort;                                      // length of owner name 
    ownname                     : array[0..31] of AnsiChar;                     // relation's owner name + space for NULL
    aliasname_length            : FXShort;                                      // length of alias name
    aliasname                   : array[0..31] of AnsiChar;                     // relation's alias name + space for NULL 
    ///<summary> http://www.ibase.ru/unicode_faq.html GetCharsetSize ?</summary>
    function CharsetSize : Integer;
    ///<summary> http://www.ibase.ru/unicode_faq.html GetCharsetSize ?</summary>
    function MaxChars: Integer;
    end;
  PXSQLVAR                      = ^TXSQLVAR;

  TXSQLDA = record
    version                     : FXShort;                                      // version of this XSQLDA 
    sqldaid                     : array[0..7] of AnsiChar;                      // XSQLDA name field
    sqldabc                     : ISC_LONG;                                     // length in bytes of SQLDA
    sqln                        : FXShort;                                      // number of fields allocated
    sqld                        : FXShort;                                      // actual number of fields *
    sqlvar                      : array[0..0] of TXSQLVAR;                      // first field address 
    end;
  PXSQLDA                       = ^TXSQLDA;

Const
  SQL_VARYING                    =        448;
  SQL_TEXT                       =        452;
  SQL_DOUBLE                     =        480;
  SQL_FLOAT                      =        482;
  SQL_LONG                       =        496;
  SQL_Short                      =        500;
  SQL_TIMESTAMP                  =        510;
  SQL_BLOB                       =        520;
  SQL_D_FLOAT                    =        530;
  SQL_ARRAY                      =        540;
  SQL_QUAD                       =        550;
  SQL_TIME                       =        560;
  SQL_DATE                       =        570;
  SQL_BIGINT                     =        580;

  {____________________________________________________________________________}
  (** This record type is for passing arguments to       **)
  (** isc_start_transaction (See docs)                   **)
  {____________________________________________________________________________}
Type
  TISC_START_TRANS = record
    db_handle      : PISC_DB_HANDLE;
    tpb_length     : FXUShort;
    tpb_address    : PFXByte;
    end;

  {____________________________________________________________________________}
  (** This record type is for passing arguments to       **)
  (** isc_start_multiple (see docs)                      **)
  {____________________________________________________________________________}
  TISC_TEB = record
    db_handle      : PISC_DB_HANDLE;
    tpb_length     : FXLong;
    tpb_address    : PFXByte;
    end;
  PISC_TEB = ^TISC_TEB;
  PISC_TEB_ARRAY = array of TISC_TEB;

  {____________________________________________________________________________}
  (** OSRI database functions **)
  {____________________________________________________________________________}

Tisc_attach_database = function (status_vector            : PISC_STATUS;
                                 db_name_length           : FXShort;
                                 db_name                  : PAnsiChar;
                                 db_handle                : PISC_DB_HANDLE;
                                 parm_buffer_length        : FXShort;
                                 parm_buffer              : PFXByte): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_blob_default_desc = procedure  (descriptor           : PISC_BLOB_DESC;
                                 table_name               : PFXByte;
                                 column_name              : PFXByte);
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_blob_gen_bpb = function    (status_vector            : PISC_STATUS;
         to_descriptor,
                                 from_descriptor          : PISC_BLOB_DESC;
                                 bpb_buffer_length        : FXUShort;
                                 bpb_buffer               : PFXByte;
                                 bpb_length               : PFXUShort): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_blob_info = function       (status_vector            : PISC_STATUS;
                                 blob_handle              : PISC_BLOB_HANDLE;
                                 item_list_buffer_length  : FXShort;
                                  item_list_buffer         : PFXByte;
                                 result_buffer_length     : FXShort;
                                 result_buffer            : PFXByte): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_blob_lookup_desc = function (status_vector           : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 trans_handle             : PISC_TR_HANDLE;
                                 table_name,
                                 column_name              : PAnsiChar;
                                 descriptor               : PISC_BLOB_DESC;
                                 global                   : PFXByte): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_blob_set_desc = function   (status_vector            : PISC_STATUS;
                                 table_name,
                                 column_name              : PAnsiChar;
                                 subtype,
                                 charset,
                                 segment_size             : FXShort;
                                 descriptor               : PISC_BLOB_DESC): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_cancel_blob = function     (status_vector            : PISC_STATUS;
                                 blob_handle              : PISC_BLOB_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_cancel_events = function   (status_vector            : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 event_id                 : PISC_LONG): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_close_blob = function      (status_vector            : PISC_STATUS;
                                 blob_handle              : PISC_BLOB_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_commit_retaining = function (status_vector           : PISC_STATUS;
         tran_handle              : PISC_TR_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_commit_transaction = function  (status_vector        : PISC_STATUS;
                                 tran_handle              : PISC_TR_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_create_blob = function     (status_vector            : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 tran_handle              : PISC_TR_HANDLE;
                                 blob_handle              : PISC_BLOB_HANDLE;
                                 blob_id                  : PISC_QUAD): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_create_blob2 = function    (status_vector            : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 tran_handle              : PISC_TR_HANDLE;
                                 blob_handle              : PISC_BLOB_HANDLE;
                                 blob_id                  : PISC_QUAD;
                                 bpb_length               : FXShort;
                                 bpb_address              : PFXByte): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_create_database = function (status_vector            : PISC_STATUS;
                                 isc_arg2                 : FXShort;
                                 isc_arg3                 : PAnsiChar;
                                 db_handle                : PISC_DB_HANDLE;
                                 isc_arg5                  : FXShort;
                                 isc_arg6                 : PAnsiChar;
                                 isc_arg7                 : FXShort): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_database_info = function   (status_vector            : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 item_list_buffer_length  : FXShort;
                                 item_list_buffer         : PFXByte;
                                 result_buffer_length     : FXShort;
                                 result_buffer            : PFXByte): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_detach_database = function (status_vector            : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_drop_database = function   (status_vector            : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_allocate_statement = function (status_vector    : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 stmt_handle              : PISC_STMT_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_alloc_statement2 = function (status_vector      : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 stmt_handle              : PISC_STMT_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_describe = function   (status_vector            : PISC_STATUS;
                                 stmt_handle              : PISC_STMT_HANDLE;
                                 dialect                  : FXUShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_describe_bind = function  (status_vector        : PISC_STATUS;
                                 stmt_handle              : PISC_STMT_HANDLE;
                                 dialect                  : FXUShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_exec_immed2 = function (status_vector           : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 tran_handle              : PISC_TR_HANDLE;
                                 length                   : FXUShort;
                                 statement                : PAnsiChar;
                                 dialect                  : FXUShort;
                                 in_xsqlda,
                                 out_xsqlda               : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_execute = function    (status_vector            : PISC_STATUS;
                                 tran_handle              : PISC_TR_HANDLE;
                                 stmt_handle              : PISC_STMT_HANDLE;
                                 dialect                  : FXUShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_execute2 = function   (status_vector            : PISC_STATUS;
                                 tran_handle              : PISC_TR_HANDLE;
                                 stmt_handle              : PISC_STMT_HANDLE;
                                 dialect                  : FXUShort;
                                 in_xsqlda,
                                 out_xsqlda               : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_execute_immediate = function (status_vector     : PISC_STATUS;
                                 db_handle                : PISC_DB_HANDLE;
                                 tran_handle              : PISC_TR_HANDLE;
                                 length                   : FXUShort;
                                 statement                : PAnsiChar;
                                 dialect                  : FXUShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_fetch = function      (status_vector            : PISC_STATUS;
                                 stmt_handle              : PISC_STMT_HANDLE;
                                 dialect                  : FXUShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_finish = function    (db_handle                : PISC_DB_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_free_statement = function (status_vector        : PISC_STATUS;
                                 stmt_handle              : PISC_STMT_HANDLE;
                                 options                  : FXUShort): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_insert = function     (status_vector            : PISC_STATUS;
                                 stmt_handle              : PISC_STMT_HANDLE;
                                 arg3                     : FXUShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_prepare = function    (status_vector            : PISC_STATUS;
                                 tran_handle              : PISC_TR_HANDLE;
                                 stmt_handle              : PISC_STMT_HANDLE;
                                 length                   : FXUShort;
                                 statement                : PFXByte;
                                 dialect                  : FXUShort;
                                 xsqlda                   : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_set_cursor_name = function (status_vector        : PISC_STATUS;
                                 stmt_handle               : PISC_STMT_HANDLE;
                                 cursor_name               : PAnsiChar;
                                 cursor_type               : FXUShort): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_sql_info = function   (status_vector             : PISC_STATUS;
                                 stmt_handle               : PISC_STMT_HANDLE;
                                 item_length               : FXShort;
                                 items                     : PFXByte;
                                 buffer_length             : FXShort;
                                 buffer                    : PFXByte): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_event_block = function     (event_buffer               : PPAnsiChar;
                                 result_buffer              : PPAnsiChar;
                                 id_count                   : FXUShort;
                                 Event1, Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event9,Event10, Event11, Event12, Event13, Event14, Event15, Event16, Event17, Event18, Event19, Event20: PAnsiChar): ISC_LONG;
                                 cdecl;

Tisc_que_events = function      (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 event_id                  : PISC_LONG;
                                 length                    : FXShort;
                                 event_buffer              : PAnsiChar;
                                 event_function            : TISC_CALLBACK;
                                 event_function_arg        : PFXVoid): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_event_counts = procedure   (status_vector             : PISC_STATUS;
                                 buffer_length             : FXShort;
                                 event_buffer              : PAnsiChar;
                                 result_buffer             : PAnsiChar);
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_free = function           (isc_arg1                   : FXVoid): ISC_LONG;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_get_segment = function     (status_vector             : PISC_STATUS;
                                 blob_handle               : PISC_BLOB_HANDLE;
                                 actual_seg_length         : PFXUShort;
                                 seg_buffer_length         : FXUShort;
                                 seg_buffer                : PFXByte): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_get_slice = function       (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : PISC_QUAD;
                                 isc_arg5                  : FXShort;
                                 isc_arg6                  : PAnsiChar;
                                 isc_arg7                  : FXShort;
                                 isc_arg8                  : PISC_LONG;
                                 isc_arg9                  : ISC_LONG;
                                 isc_arg10                 : PFXVoid;
                                 isc_arg11                 : PISC_LONG): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_interprete_old = function  (buffer                    : PAnsiChar;
                                 status_vector             : PPISC_STATUS): FXUInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_interprete = function      (buffer                    : PAnsiChar;
                                 bufferlen                 : FXUInt;
                                 status_vector             : PPISC_STATUS): FXUInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_open_blob = function       (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 blob_handle               : PISC_BLOB_HANDLE;
                                 blob_id                   : PISC_QUAD): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_open_blob2 = function      (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 blob_handle               : PISC_BLOB_HANDLE;
                                 blob_id                   : PISC_QUAD;
                                 bpb_length                : FXShort;
                                 bpb_buffer                : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_prepare_transaction2 = function (status_vector        : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 msg_length                : FXShort;
                                 msg                       : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_print_sqlerror = procedure (sqlcode                   : FXShort;
                                 status_vector             : PISC_STATUS);
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_print_status = function   (status_vector              : PISC_STATUS): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_put_segment = function     (status_vector             : PISC_STATUS;
                                 blob_handle               : PISC_BLOB_HANDLE;
                                 seg_buffer_len            : FXUShort;
                                 seg_buffer                : PFXByte): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_put_slice = function       (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : PISC_QUAD;
                                 isc_arg5                  : FXShort;
                                 isc_arg6                  : PAnsiChar;
                                 isc_arg7                  : FXShort;
                                 isc_arg8                  : PISC_LONG;
                                 isc_arg9                  : ISC_LONG;
                                 isc_arg10                 : PFXVoid): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_rollback_retaining = function (status_vector         : PISC_STATUS;
                                 tran_handle              : PISC_TR_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_rollback_transaction = function (status_vector        : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_start_multiple = function  (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 db_handle_count           : FXShort;
                                 teb_vector_address        : PISC_TEB): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_start_transaction = function (status_vector           : PISC_STATUS;
                                tran_handle               : PISC_TR_HANDLE;
                                db_handle_count           : FXShort;
                                db_handle                 : PISC_DB_HANDLE;
                                tpb_length                : FXUShort;
                                tpb_address               : PAnsiChar): ISC_STATUS;
                                cdecl;

Tisc_sqlcode = function        (status_vector             : PISC_STATUS): ISC_LONG;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}


Tisc_sql_interprete = procedure (sqlcode                   : FXShort;
                                 buffer                    : PAnsiChar;
                                 buffer_length             : FXShort);
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_transaction_info = function (status_vector            : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 item_list_buffer_length   : FXShort;
                                 item_list_buffer          : PAnsiChar;
                                 result_buffer_length      : FXShort;
                                 result_buffer             : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_transact_request = function (status_vector            : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PAnsiChar;
                                 isc_arg6                   : FXUShort;
                                 isc_arg7                  : PAnsiChar;
                                 isc_arg8                  : FXUShort;
                                 isc_arg9                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_portable_integer = function (buffer                   : PAnsiChar;
                                 length                    : FXShort): FXInt64;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

{_______________________________________________________________________________
/*************************************/
/* Security Functions and structures */
/*************************************/
_______________________________________________________________________________}
Const
  sec_uid_spec                    = $01;
  sec_gid_spec                    = $02;
  sec_server_spec                 = $04;
  sec_password_spec               = $08;
  sec_group_name_spec             = $10;
  sec_first_name_spec             = $20;
  sec_middle_name_spec            = $40;
  sec_last_name_spec              = $80;
  sec_dba_user_name_spec          = $100;
  sec_dba_password_spec           = $200;

  sec_protocol_tcpip              = 1;
  sec_protocol_netbeui            = 2;
  sec_protocol_spx                = 3;
  sec_protocol_local              = 4;

Type
  PISC_UserSecData = ^TISC_UserSecData;
  TISC_UserSecData = record
    sec_flags    : FXShort;                                                      // which fields are specified
    uid          : FXInt;                                                        // the user's id
    gid          : FXInt;                                                        // the user's group id
    protocol     : FXInt;                                                        // protocol to use for connection
    server       : PAnsiChar;                                                   // server to administer
    user_name    : PAnsiChar;                                                   // the user's name
    password     : PAnsiChar;                                                   // the user's password
    group_name   : PAnsiChar;                                                   // the group name
    first_name   : PAnsiChar;                                                    // the user's first name
    middle_name  : PAnsiChar;                                                   // the user's middle name
    last_name    : PAnsiChar;                                                    // the user's last name
    dba_user_name: PAnsiChar;                                                   // the dba user name
    dba_password : PAnsiChar;                                                   // the dba password
    end;

  Tisc_add_user = function      (status_vector             : PISC_STATUS;
                                 user_sec_data             : PISC_UserSecData): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_delete_user = function     (status_vector             : PISC_STATUS;
                                 user_sec_data             : PISC_UserSecData): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_modify_user = function     (status_vector             : PISC_STATUS;
                                 user_sec_data             : PISC_UserSecData): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

  {____________________________________________________________________________}
  (**  Other OSRI functions          **)
  {____________________________________________________________________________}

Tisc_compile_request = function (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg4                  : FXShort;
                                 isc_arg5                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_compile_request2 = function (status_vector            : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg4                  : FXShort;
                                 isc_arg5                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

// This function always returns error since FB 3.0.
Tisc_ddl = function             (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : FXShort;
                                 isc_arg5                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_prepare_transaction = function (status_vector         : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}


Tisc_receive = function         (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg3,
                                 isc_arg4                  : FXShort;
                                 isc_arg5                  : PFXVoid;
                                 isc_arg6                  : FXShort): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_receive2 = function        (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg3,
                                 isc_arg4                  : FXShort;
                                 isc_arg5                  : PFXVoid;
                                 isc_arg6,
                                 isc_arg7                  : FXShort;
                                 isc_arg8                  : FXLong): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_reconnect_transaction = function (status_vector       : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : FXShort;
                                 isc_arg5                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_release_request = function (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_request_info = function    (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg3                  : FXShort;
                                 isc_arg4                  : FXShort;
                                 isc_arg5                  : PAnsiChar;
                                 isc_arg6                  : FXShort;
                                 isc_arg7                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_seek_blob = function       (status_vector             : PISC_STATUS;
                                 blob_handle               : PISC_BLOB_HANDLE;
                                 isc_arg3                  : FXShort;
                                 isc_arg4                  : ISC_LONG;
                                 isc_arg5                  : PISC_LONG): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_send = function            (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
                                 isc_arg3,
                                 isc_arg4                  : FXShort;
                                 isc_arg5                  : PFXVoid;
                                 isc_arg6                  : FXShort): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_start_and_send = function  (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4,
                                 isc_arg5                  : FXShort;
                                 isc_arg6                  : PFXVoid;
                                 isc_arg7                  : FXShort): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_start_request = function   (status_vector             : PISC_STATUS;
                                 request_handle            : PISC_REQ_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : FXShort): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_unwind_request = function  (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : FXShort): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_wait_for_event = function  (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 length                    : FXShort;
                                 event_buffer,
                                 result_buffer             : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

  {____________________________________________________________________________}
  (** Other Sql functions       **)
  {____________________________________________________________________________}

Tisc_close = function           (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_declare = function         (status_vector             : PISC_STATUS;
                                 isc_arg2,
                                 isc_arg3                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_describe = function        (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar;
                                 isc_arg3                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_describe_bind = function   (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar;
                                 isc_arg3                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_execute = function         (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PAnsiChar;
                                 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_execute_immediate = function (status_vector           : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : PFXShort;
                                 isc_arg5                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_fetch = function           (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar;
                                 isc_arg3                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_open = function            (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PAnsiChar;
                                 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_prepare = function         (status_vector             : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : PAnsiChar;
                                 isc_arg5                  : PFXShort;
                                 isc_arg6                  : PAnsiChar;
                                 isc_arg7                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

  {____________________________________________________________________________}
  (** Other Dynamic sql functions       **)
  {____________________________________________________________________________}

  Tisc_dsql_execute_m = function  (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 statement_handle          : PISC_STMT_HANDLE;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PAnsiChar;
                                 isc_arg6                  : FXUShort;
                                 isc_arg7                  : FXUShort;
                                 isc_arg8                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_execute2_m = function (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 statement_handle          : PISC_STMT_HANDLE;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PAnsiChar;
                                 isc_arg6                  : FXUShort;
                                 isc_arg7                  : FXUShort;
                                 isc_arg8                  : PAnsiChar;
                                 isc_arg9                  : FXUShort;
                                 isc_arg10                 : PAnsiChar;
                                 isc_arg11                 : FXUShort;
                                 isc_arg12                 : FXUShort;
                                 isc_arg13                 : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_execute_immediate_m = function (status_vector    : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PAnsiChar;
                                 isc_arg6                  : FXUShort;
                                 isc_arg7                  : FXUShort;
                                 isc_arg8                  : PAnsiChar;
                                 isc_arg9                  : FXUShort;
                                 isc_arg10                 : FXUShort;
                                 isc_arg11                 : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_exec_immed3_m = function  (status_vector         : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PAnsiChar;
                                 isc_arg6                  : FXUShort;
                                 isc_arg7                  : FXUShort;
                                 isc_arg8                  : PAnsiChar;
                                 isc_arg9                  : FXUShort;
                                 isc_arg10                 : FXUShort;
                                 isc_arg11                 : PAnsiChar;
                                 isc_arg12                 : FXUShort;
                                 isc_arg13                 : PAnsiChar;
                                 isc_arg14                 : FXUShort;
                                 isc_arg15                 : FXUShort;
                                 isc_arg16                 : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_fetch_m = function    (status_vector             : PISC_STATUS;
                                 statement_handle          : PISC_STMT_HANDLE;
                                 isc_arg3                  : FXUShort;
                                 isc_arg4                  : PAnsiChar;
                                 isc_arg5                  : FXUShort;
                                 isc_arg6                  : FXUShort;
                                 isc_arg7                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_insert_m = function   (status_vector             : PISC_STATUS;
                                 statement_handle          : PISC_STMT_HANDLE;
                                 isc_arg3                  : FXUShort;
                                 isc_arg4                  : PAnsiChar;
                                 isc_arg5                  : FXUShort;
                                 isc_arg6                  : FXUShort;
                                 isc_arg7                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_prepare_m = function  (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 statement_handle          : PISC_STMT_HANDLE;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PAnsiChar;
                                 isc_arg6                  : FXUShort;
                                 isc_arg7                  : FXUShort;
                                 isc_arg8                  : PAnsiChar;
                                 isc_arg9                  : FXUShort;
                                 isc_arg10                 : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_dsql_release = function    (status_vector             : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_close = function(status_vector             : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_declare = function  (status_vector         : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar;
                                 isc_arg3                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_describe = function (status_vector         : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar;
                                 isc_arg3                  : FXUShort;
                                 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_describe_bind = function (status_vector    : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar;
                                 isc_arg3                  : FXUShort;
                                 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_execute = function  (status_vector         : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PAnsiChar;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_execute2 = function (status_vector         : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PAnsiChar;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PXSQLDA;
                                 isc_arg6                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_execute_immed = function (status_vector    : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PAnsiChar;
                                 isc_arg6                  : FXUShort;
                                 isc_arg7                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_fetch = function(status_vector             : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar;
                                 isc_arg3                  : FXUShort;
                                 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

(*$ifdef SCROLLABLE_CURSORS*)
Tisc_embed_dsql_fetch2 = function  (status_vector         : PISC_STATUS;
                                isc_arg2                  : PAnsiChar;
                                isc_arg3                  : FXUShort;
                                isc_arg4                  : PXSQLDA;
                                isc_arg5                  : FXUShort;
                                isc_arg6                  : FXLong): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}
(*$endif*)

Tisc_embed_dsql_open = function (status_vector             : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PAnsiChar;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_open2 = function (status_vector            : PISC_STATUS;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg3                  : PAnsiChar;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PXSQLDA;
                                 isc_arg6                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_insert = function (status_vector           : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar;
                                 isc_arg3                  : FXUShort;
                                 isc_arg4                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_prepare = function  (status_vector         : PISC_STATUS;
                                 db_handle                 : PISC_DB_HANDLE;
                                 tran_handle               : PISC_TR_HANDLE;
                                 isc_arg4                  : PAnsiChar;
                                 isc_arg5                  : FXUShort;
                                 isc_arg6                  : PAnsiChar;
                                 isc_arg7                  : FXUShort;
                                 isc_arg8                  : PXSQLDA): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_embed_dsql_release = function  (status_vector         : PISC_STATUS;
                                 isc_arg2                  : PAnsiChar): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

  {____________________________________________________________________________}
  (** Other Blob functions       **)
  {____________________________________________________________________________}

Tisc_Blob_open = function       (blob_handle               : TISC_BLOB_HANDLE;
                                 isc_arg2                  : PAnsiChar;
                                 isc_arg3                  : FXInt): PISC_BLOB_STREAM;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_Blob_put = function        (isc_arg1                  : char;
                                 isc_arg2                  : PISC_BLOB_STREAM): FXInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_Blob_close = function      (isc_arg1                  : PISC_BLOB_STREAM): FXInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_BLOB_get = function        (isc_arg1                  : PISC_BLOB_STREAM): FXInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_Blob_display = function    (isc_arg1                  : PISC_QUAD;
                                 db_handle                 : TISC_DB_HANDLE;
                                 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PAnsiChar): FXInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_Blob_dump = function       (isc_arg1                  : PISC_QUAD;
                                 db_handle                 : TISC_DB_HANDLE;
                                 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PAnsiChar): FXInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_Blob_edit = function       (isc_arg1                  : PISC_QUAD;
                                 db_handle                 : TISC_DB_HANDLE;
                                 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PAnsiChar): FXInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_Blob_load = function       (isc_arg1                  : PISC_QUAD;
                                 db_handle                 : TISC_DB_HANDLE;
                                 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PAnsiChar): FXInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_Blob_text_dump = function  (isc_arg1                  : PISC_QUAD;
                                 db_handle                 : TISC_DB_HANDLE;
                                 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PAnsiChar): FXInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_Blob_text_load = function  (isc_arg1                  : PISC_QUAD;
                                 db_handle                 : TISC_DB_HANDLE;
                                 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PAnsiChar): FXInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_Bopen = function           (isc_arg1                  : PISC_QUAD;
                                 db_handle                 : TISC_DB_HANDLE;
                                 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PAnsiChar): FXInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_Bopen2 = function          (isc_arg1                  : PISC_QUAD;
                                 db_handle                 : TISC_DB_HANDLE;
                                 tran_handle               : TISC_TR_HANDLE;
                                 isc_arg4                  : PAnsiChar;
                                 isc_arg5                  : FXUShort): PISC_BLOB_STREAM;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

  {____________________________________________________________________________}
  (** Other Misc functions       **)
  {____________________________________________________________________________}

Tisc_ftof = function            (isc_arg1                  : PAnsiChar;
                                 isc_arg2                  : FXUShort;
                                 isc_arg3                  : PAnsiChar;
                                 isc_arg4                  : FXUShort): ISC_LONG;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_print_blr = function       (isc_arg1                  : PAnsiChar;
                                 isc_arg2                  : TISC_CALLBACK;
                                 isc_arg3                  : PFXVoid;
                                 isc_arg4                  : FXShort): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_set_debug = procedure     (isc_arg1                  : FXInt);
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_qtoq = procedure           (isc_arg1                  : PISC_QUAD;
                                 isc_arg2                  : PISC_QUAD);
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_vtof = procedure           (isc_arg1                  : PAnsiChar;
                                 isc_arg2                  : PAnsiChar;
                                 isc_arg3                  : FXUShort);
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_vtov = procedure           (isc_arg1                  : PAnsiChar;
                                 isc_arg2                  : PAnsiChar;
                                 isc_arg3                  : FXShort);
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_version = function         (db_handle                 : PISC_DB_HANDLE;
                                 isc_arg2                  : TISC_CALLBACK;
                                 isc_arg3                  : PFXVoid): FXInt;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_reset_fpe = function      (isc_arg1                  : FXUShort): ISC_LONG;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

  {____________________________________________________________________________}
  (** Service manager functions             **)
  {____________________________________________________________________________}

  Tisc_service_attach = function(status_vector             : PISC_STATUS;
                                 isc_arg2                  : FXUShort;
                                 isc_arg3                  : PAnsiChar;
                                 service_handle            : PISC_SVC_HANDLE;
                                 isc_arg5                  : FXUShort;
                                 isc_arg6                  : PFXByte): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_service_detach = function (status_vector             : PISC_STATUS;
                                service_handle            : PISC_SVC_HANDLE): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_service_query = function   (status_vector             : PISC_STATUS;
                                 service_handle            : PISC_SVC_HANDLE;
                                 recv_handle               : PISC_SVC_HANDLE;
                                 isc_arg4                  : FXUShort;
                                 isc_arg5                  : PFXByte;
                                 isc_arg6                  : FXUShort;
                                 isc_arg7                  : PFXByte;
                                 isc_arg8                  : FXUShort;
                                 isc_arg9                  : PFXByte): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

Tisc_service_start = function  (status_vector             : PISC_STATUS;
                                service_handle            : PISC_SVC_HANDLE;
                                recv_handle               : PISC_SVC_HANDLE;
                                isc_arg4                  : FXUShort;
                                isc_arg5                  : PFXByte): ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

  {_____________________________________________________________________________
/* Shutdown and cancel */
  _____________________________________________________________________________}

  Tfb_shutdown = function (a:FXUInt;b:FXInt):FXInt;

  Tfb_shutdown_callback = function (status_vector:PISC_STATUS;
                                 callback:FB_SHUTDOWN_CALLBACK;
                                b:FXInt;
                                c:PFXVoid):ISC_STATUS;
                                {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                                {$IFDEF LINUX} cdecl; {$ENDIF}

  Tfb_cancel_operation = function (status_vector : PISC_STATUS;
                                db_handle : PISC_DB_HANDLE;
                                b:FXUShort):ISC_STATUS;
                               {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                               {$IFDEF LINUX} cdecl; {$ENDIF}

  {_____________________________________________________________________________
/* Ping the connection */
  _____________________________________________________________________________}
  Tfb_ping = function (         status_vector : PISC_STATUS;
                                db_handle : PISC_DB_HANDLE;
                                b:FXUShort):ISC_STATUS;
                               {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
                               {$IFDEF LINUX} cdecl; {$ENDIF}

  {_____________________________________________________________________________
/********************/
/* Object interface */
/********************/
  _____________________________________________________________________________}
  Tfb_get_database_handle = function ( status_vector : PISC_STATUS;db_handle : PISC_DB_HANDLE;arg:PFXVoid):ISC_STATUS;{$IFDEF MSWINDOWS} stdcall; {$ENDIF}{$IFDEF LINUX} cdecl; {$ENDIF}
  Tfb_get_transaction_handle = function ( status_vector : PISC_STATUS;trans_handle:PISC_TR_HANDLE;arg:PFXVoid):ISC_STATUS;{$IFDEF MSWINDOWS} stdcall; {$ENDIF}{$IFDEF LINUX} cdecl; {$ENDIF}

  {_____________________________________________________________________________
/********************************/
/* Client information functions */
/********************************/
  _____________________________________________________________________________}
  Tisc_get_client_version = procedure(buffer : PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  Tisc_get_client_major_version = function : Integer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  Tisc_get_client_minor_version = function : Integer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  {_____________________________________________________________________________
/*******************************************/
/* Set callback for database crypt plugins */
/*******************************************/
  _____________________________________________________________________________}
  Tfb_database_crypt_callback = function(status_vector : PISC_STATUS;arg:PFXVoid):ISC_STATUS;{$IFDEF MSWINDOWS} stdcall; {$ENDIF}{$IFDEF LINUX} cdecl; {$ENDIF}

  {____________________________________________________________________________}
  (** Constants!!!                      **)
  {____________________________________________________________________________}
  const
    DSQL_close     = 1;
    DSQL_drop      = 2;

    FX_DateOffset     =    15018;
    FX_TimeCoeff      = 86400000;//24*60*60*1000

    SQLDA_VERSION1 = 1;

    /// maximum events handled per block by Firebird
    FX_MAX_EVENT_BLOCK   = 20;
    /// maximum event name length
    FX_MAX_EVENT_LENGTH  = 64;

    FX_DefaultBlobSegmentSize = 16 * 1024;



  {____________________________________________________________________________}
  (** Actions to pass to the blob filter (ctl_source) **)
  {____________________________________________________________________________}
  isc_blob_filter_open           =          0;
  isc_blob_filter_get_segment    =          1;
  isc_blob_filter_close          =          2;
  isc_blob_filter_create         =          3;
  isc_blob_filter_put_segment    =          4;
  isc_blob_filter_alloc          =          5;
  isc_blob_filter_free           =          6;
  isc_blob_filter_seek           =          7;

  {____________________________________________________________________________}
  (** Blr definitions **)
  // In pascal, how does one deal with the below "#define"?
  // blr_word(n) ((n) % 256), ((n) / 256)
  {____________________________________________________________________________}
  blr_text                       =         14;
  blr_text2                      =         15;
  blr_Short                      =          7;
  blr_long                       =          8;
  blr_quad                       =          9;
  blr_float                      =         10;
  blr_double                     =         27;
  blr_d_float                    =         11;
  blr_timestamp                  =         35;
  blr_varying                    =         37;
  blr_varying2                   =         38;
  blr_blob                       =        261;
  blr_cstring                    =         40;
  blr_cstring2                   =         41;
  blr_blob_id                    =         45;
  blr_sql_date                   =         12;
  blr_sql_time                   =         13;
  blr_int64                      =         16;
  blr_date                       =         blr_timestamp;


  blr_inner                      =          0;
  blr_left                       =          1;
  blr_right                      =          2;
  blr_full                       =          3;

  blr_gds_code                   =          0;
  blr_sql_code                   =          1;
  blr_exception                  =          2;
  blr_trigger_code               =          3;
  blr_default_code               =          4;

  blr_version4                   =          4;
  blr_version5                   =          5;
  blr_eoc                        =         76;
  blr_end                        =         -1;

  blr_assignment                 =          1;
  blr_begin                      =          2;
  blr_dcl_variable               =          3;
  blr_message                    =          4;
  blr_erase                      =          5;
  blr_fetch                      =          6;
  blr_for                        =          7;
  blr_if                         =          8;
  blr_loop                       =          9;
  blr_modify                     =         10;
  blr_handler                    =         11;
  blr_receive                    =         12;
  blr_select                     =         13;
  blr_send                       =         14;
  blr_store                      =         15;
  blr_label                      =         17;
  blr_leave                      =         18;
  blr_store2                     =         19;
  blr_post                       =         20;

  blr_literal                    =         21;
  blr_dbkey                      =         22;
  blr_field                      =         23;
  blr_fid                        =         24;
  blr_parameter                  =         25;
  blr_variable                   =         26;
  blr_average                    =         27;
  blr_count                      =         28;
  blr_maximum                    =         29;
  blr_minimum                    =         30;
  blr_total                      =         31;
  blr_add                        =         34;
  blr_subtract                   =         35;
  blr_multiply                   =         36;
  blr_divide                     =         37;
  blr_negate                     =         38;
  blr_concatenate                =         39;
  blr_substring                  =         40;
  blr_parameter2                 =         41;
  blr_from                       =         42;
  blr_via                        =         43;
  blr_user_name                  =         44;
  blr_null                       =         45;

  blr_eql                        =         47;
  blr_neq                        =         48;
  blr_gtr                        =         49;
  blr_geq                        =         50;
  blr_lss                        =         51;
  blr_leq                        =         52;
  blr_containing                 =         53;
  blr_matching                   =         54;
  blr_starting                   =         55;
  blr_between                    =         56;
  blr_or                         =         57;
  blr_and                        =         58;
  blr_not                        =         59;
  blr_any                        =         60;
  blr_missing                    =         61;
  blr_unique                     =         62;
  blr_like                       =         63;

  blr_stream                     =         65;
  blr_set_index                  =         66;
  blr_rse                        =         67;
  blr_first                      =         68;
  blr_project                    =         69;
  blr_sort                       =         70;
  blr_boolean                    =         71;
  blr_ascending                  =         72;
  blr_descending                 =         73;
  blr_relation                   =         74;
  blr_rid                        =         75;
  blr_union                      =         76;
  blr_map                        =         77;
  blr_group_by                   =         78;
  blr_aggregate                  =         79;
  blr_join_type                  =         80;
  blr_rows                       =         81;

//sub parameters for blr_rows 
  blr_ties                       =          0;
  blr_percent                    =          1;

  blr_agg_count                  =         83;
  blr_agg_max                    =         84;
  blr_agg_min                    =         85;
  blr_agg_total                  =         86;
  blr_agg_average                =         87;
  blr_parameter3                 =         88;
  blr_run_count                  =        118;
  blr_run_max                    =         89;
  blr_run_min                    =         90;
  blr_run_total                  =         91;
  blr_run_average                =         92;
  blr_agg_count2                 =         93;
  blr_agg_count_distinct         =         94;
  blr_agg_total_distinct         =         95;
  blr_agg_average_distinct       =         96;

  blr_function                   =        100;
  blr_gen_id                     =        101;
  blr_prot_mask                  =        102;
  blr_upcase                     =        103;
  blr_lock_state                 =        104;
  blr_value_if                   =        105;
  blr_matching2                  =        106;
  blr_index                      =        107;
  blr_ansi_like                  =        108;
  blr_bookmark                   =        109;
  blr_crack                      =        110;
  blr_force_crack                =        111;
  blr_seek                       =        112;
  blr_find                       =        113;

  blr_continue                   =          0;
  blr_forward                    =          1;
  blr_backward                   =          2;
  blr_bof_forward                =          3;
  blr_eof_backward               =          4;

  blr_lock_relation              =        114;
  blr_lock_record                =        115;
  blr_set_bookmark               =        116;
  blr_get_bookmark               =        117;
  blr_rs_stream                  =        119;
  blr_exec_proc                  =        120;
  blr_begin_range                =        121;
  blr_end_range                  =        122;
  blr_delete_range               =        123;
  blr_procedure                  =        124;
  blr_pid                        =        125;
  blr_exec_pid                   =        126;
  blr_singular                   =        127;
  blr_abort                      =        128;
  blr_block                      =        129;
  blr_error_handler              =        130;
  blr_cast                       =        131;
  blr_release_lock               =        132;
  blr_release_locks              =        133;
  blr_start_savepoint            =        134;
  blr_end_savepoint              =        135;
  blr_find_dbkey                 =        136;
  blr_range_relation             =        137;
  blr_delete_ranges              =        138;

  blr_plan                       =        139;
  blr_merge                      =        140;
  blr_join                       =        141;
  blr_sequential                 =        142;
  blr_navigational               =        143;
  blr_indices                    =        144;
  blr_retrieve                   =        145;

  blr_relation2                  =        146;
  blr_rid2                       =        147;
  blr_reset_stream               =        148;
  blr_release_bookmark           =        149;
  blr_set_generator              =        150;
  blr_ansi_any                   =        151;
  blr_exists                     =        152;
  blr_cardinality                =        153;

  blr_record_version             =        154;    (** get tid of record **)
  blr_stall                      =        155;    (** fake server stall **)
  blr_seek_no_warn               =        156;
  blr_find_dbkey_version         =        157;
  blr_ansi_all                   =        158;

  blr_extract                    = 159;

  (* sub parameters for blr_extract *)
  blr_extract_year               =   0;
  blr_extract_month              =   1;
  blr_extract_day               =   2;
  blr_extract_hour               =   3;
  blr_extract_minute             =   4;
  blr_extract_second             =   5;
  blr_extract_weekday            =   6;
  blr_extract_yearday            =   7;

  blr_current_date               = 160;
  blr_current_timestamp          = 161;
  blr_current_time               = 162;

  {____________________________________________________________________________}
  (* These verbs were added in 6.0, primarily to support 64-bit integers *)
  blr_add2                       = 163;
  blr_subtract2                  = 164;
  blr_multiply2                  = 165;
  blr_divide2                    = 166;
  blr_agg_total2                 = 167;
  blr_agg_total_distinct2        = 168;
  blr_agg_average2               = 169;
  blr_agg_average_distinct2      = 170;
  blr_average2                   = 171;
  blr_gen_id2                    = 172;
  blr_set_generator2             = 173;

  {____________________________________________________________________________}
  (** Bit assignments in RDB$SYSTEM_FLAG **)
  {____________________________________________________________________________}
  RDB_system                     = 1;
  RDB_id_assigned                = 2;




{_______________________________________________________________________________
/*********************************/
/* Information call declarations */
/*********************************/
/* Common, structural codes */
_______________________________________________________________________________}
  isc_info_end                   =          1;
  isc_info_truncated             =          2;
  isc_info_error                 =          3;
  isc_info_data_not_ready         =          4;
  isc_info_length                 =        126;
  isc_info_flag_end               =        127;

{_______________________________________________________________________________
/******************************/
/* Database information items */
/******************************/
_______________________________________________________________________________}
  isc_info_db_id                 =          4;
  isc_info_reads                 =          5;
  isc_info_writes                =          6;
  isc_info_fetches               =          7;
  isc_info_marks                 =          8;
  isc_info_implementation        =         11;
  isc_info_version               =         12;
  isc_info_base_level            =         13;
  isc_info_page_size             =         14;
  isc_info_num_buffers           =         15;
  isc_info_limbo                 =         16;
  isc_info_current_memory        =         17;
  isc_info_max_memory            =         18;
  isc_info_window_turns          =         19;
  isc_info_license               =         20;
  isc_info_allocation            =         21;
  isc_info_attachment_id         =         22;
  isc_info_read_seq_count        =         23;
  isc_info_read_idx_count        =         24;
  isc_info_insert_count          =         25;
  isc_info_update_count          =         26;
  isc_info_delete_count          =         27;
  isc_info_backout_count         =         28;
  isc_info_purge_count           =         29;
  isc_info_expunge_count         =         30;
  isc_info_sweep_interval        =         31;
  isc_info_ods_version           =         32;
  isc_info_ods_minor_version     =         33;
  isc_info_no_reserve            =         34;
  isc_info_logfile               =         35;
  isc_info_cur_logfile_name      =         36;
  isc_info_cur_log_part_offset   =         37;
  isc_info_num_wal_buffers       =         38;
  isc_info_wal_buffer_size       =         39;
  isc_info_wal_ckpt_length       =         40;
  isc_info_wal_cur_ckpt_interval =         41;
  isc_info_wal_prv_ckpt_fname    =         42;
  isc_info_wal_prv_ckpt_poffset  =         43;
  isc_info_wal_recv_ckpt_fname   =         44;
  isc_info_wal_recv_ckpt_poffset =         45;
  isc_info_wal_grpc_wait_usecs   =         47;
  isc_info_wal_num_io            =         48;
  isc_info_wal_avg_io_size       =         49;
  isc_info_wal_num_commits       =         50;
  isc_info_wal_avg_grpc_size     =         51;
  isc_info_forced_writes         =         52;
  isc_info_user_names            =         53;
  isc_info_page_errors           =         54;
  isc_info_record_errors         =         55;
  isc_info_bpage_errors          =         56;
  isc_info_dpage_errors          =         57;
  isc_info_ipage_errors          =         58;
  isc_info_ppage_errors          =         59;
  isc_info_tpage_errors          =         60;
  isc_info_set_page_buffers      =         61;
  isc_info_db_SQL_dialect        =         62;
  isc_info_db_read_only          =         63;
  isc_info_db_size_in_pages      =         64;
  //* Values 65 -100 unused to avoid conflict with InterBase */
  frb_info_att_charset           =        101;
  isc_info_db_class              =        102;
  isc_info_firebird_version      =        103;
  isc_info_oldest_transaction    =        104;
  isc_info_oldest_active         =        105;
  isc_info_oldest_snapshot       =        106;
  isc_info_next_transaction      =        107;
  isc_info_db_provider           =        108;
  isc_info_active_transactions   =        109;
  isc_info_active_tran_count     =        110;
  isc_info_creation_date         =        111;
  isc_info_db_file_size          =        112;
  fb_info_page_contents          =        113;

  fb_info_implementation         =        114;

  fb_info_page_warns             =        115;
  fb_info_record_warns           =        116;
  fb_info_bpage_warns            =        117;
  fb_info_dpage_warns            =        118;
  fb_info_ipage_warns            =        119;
  fb_info_ppage_warns            =        120;
  fb_info_tpage_warns            =        121;
  fb_info_pip_errors             =        122;
  fb_info_pip_warns              =        123;

//isc_info_db_last_value


{_______________________________________________________________________________
/**************************************/
/* Database information return values */
/**************************************/
_______________________________________________________________________________}
  isc_info_db_impl_rdb_vms       =          1;
  isc_info_db_impl_rdb_eln       =          2;
  isc_info_db_impl_rdb_eln_dev   =          3;
  isc_info_db_impl_rdb_vms_y     =          4;
  isc_info_db_impl_rdb_eln_y     =          5;
  isc_info_db_impl_jri           =          6;
  isc_info_db_impl_jsv           =          7;
  isc_info_db_impl_isc_apl_68K   =         25;
  isc_info_db_impl_isc_vax_ultr  =         26;
  isc_info_db_impl_isc_vms       =         27;
  isc_info_db_impl_isc_sun_68k   =         28;
  isc_info_db_impl_isc_os2       =         29;
  isc_info_db_impl_isc_sun4      =         30;
  isc_info_db_impl_isc_hp_ux     =         31;
  isc_info_db_impl_isc_sun_386i  =         32;
  isc_info_db_impl_isc_vms_orcl  =         33;
  isc_info_db_impl_isc_mac_aux   =         34;
  isc_info_db_impl_isc_rt_aix    =         35;
  isc_info_db_impl_isc_mips_ult  =         36;
  isc_info_db_impl_isc_xenix     =         37;
  isc_info_db_impl_isc_dg        =         38;
  isc_info_db_impl_isc_hp_mpexl  =         39;
  isc_info_db_impl_isc_hp_ux68K  =         40;
  isc_info_db_impl_isc_sgi       =         41;
  isc_info_db_impl_isc_sco_unix  =         42;
  isc_info_db_impl_isc_cray      =         43;
  isc_info_db_impl_isc_imp       =         44;
  isc_info_db_impl_isc_delta     =         45;
  isc_info_db_impl_isc_next      =         46;
  isc_info_db_impl_isc_dos       =         47;
  isc_info_db_impl_m88K          =         48;
  isc_info_db_impl_unixware      =         49;
  isc_info_db_impl_isc_winnt     =         48;
  isc_info_db_impl_isc_epson     =         49;
  isc_info_db_impl_alpha_osf     =         52;
  isc_info_db_impl_alpha_vms     =         53;
  isc_info_db_impl_netware_386   =         54;
  isc_info_db_impl_win_only      =         55;
  isc_info_db_impl_ncr_3000      =         56;
  isc_info_db_impl_winnt_ppc     =         57;
  isc_info_db_impl_dg_x86        =         58;
  isc_info_db_impl_sco_ev        =         59;
  isc_info_db_impl_i386          =         60;

  isc_info_db_impl_freebsd       =         61;
  isc_info_db_impl_netbsd        =         62;
  isc_info_db_impl_darwin_ppc    =         63;
  isc_info_db_impl_sinixz        =         64;

  isc_info_db_impl_linux_sparc   =         65;
  isc_info_db_impl_linux_amd64   =         66;

  isc_info_db_impl_freebsd_amd64 =         67;

  isc_info_db_impl_winnt_amd64   =         68;

  isc_info_db_impl_linux_ppc     =         69;
  isc_info_db_impl_darwin_x86    =         70;
  isc_info_db_impl_linux_mipsel  =         71;
  isc_info_db_impl_linux_mips    =         72;
  isc_info_db_impl_darwin_x64    =         73;
  isc_info_db_impl_sun_amd64     =         74;

  isc_info_db_impl_linux_arm     =         75;
  isc_info_db_impl_linux_ia64    =         76;

  isc_info_db_impl_darwin_ppc64  =         77;
  isc_info_db_impl_linux_s390x   =         78;
  isc_info_db_impl_linux_s390    =         79;

  isc_info_db_impl_linux_sh      =         80;
  isc_info_db_impl_linux_sheb    =         81;
  isc_info_db_impl_linux_hppa    =         82;
  isc_info_db_impl_linux_alpha   =         83;
  isc_info_db_impl_linux_arm64   =         84;
  isc_info_db_impl_linux_ppc64el =         85;
  isc_info_db_impl_linux_ppc64   =         86;


  isc_info_db_class_access       =          1;
  isc_info_db_class_y_valve      =          2;
  isc_info_db_class_rem_int      =          3;
  isc_info_db_class_rem_srvr     =          4;
  isc_info_db_class_pipe_int     =          7;
  isc_info_db_class_pipe_srvr    =          8;
  isc_info_db_class_sam_int      =          9;
  isc_info_db_class_sam_srvr     =         10;
  isc_info_db_class_gateway      =         11;
  isc_info_db_class_cache        =         12;
  isc_info_db_class_classic_access =       13;
  isc_info_db_class_server_access=         14;

  isc_info_db_code_rdb_eln       =          1;
  isc_info_db_code_rdb_vms       =          2;
  isc_info_db_code_interbase     =          3;
  isc_info_db_code_firebird      =          4;


{_______________________________________________________________________________
/*****************************/
/* Request information items */
/*****************************/
_______________________________________________________________________________}
  isc_info_number_messages       =          4;
  isc_info_max_message           =          5;
  isc_info_max_send              =          6;
  isc_info_max_receive           =          7;
  isc_info_state                 =          8;
  isc_info_message_number        =          9;
  isc_info_message_size          =         10;
  isc_info_request_cost          =         11;
  isc_info_access_path           =         12;
  isc_info_req_select_count      =         13;
  isc_info_req_insert_count      =         14;
  isc_info_req_update_count      =         15;
  isc_info_req_delete_count      =         16;


{_______________________________________________________________________________
/*********************/
/* Access path items */
/*********************/
_______________________________________________________________________________}
  isc_info_rsb_end               =          0;
  isc_info_rsb_begin             =          1;
  isc_info_rsb_type              =          2;
  isc_info_rsb_relation          =          3;
  isc_info_rsb_plan              =          4;

{_______________________________________________________________________________
/*************/
/* RecordSource (RSB) types */
/*************/
_______________________________________________________________________________}
  isc_info_rsb_unknown           =          1;
  isc_info_rsb_indexed           =          2;
  isc_info_rsb_navigate          =          3;
  isc_info_rsb_sequential        =          4;
  isc_info_rsb_cross             =          5;
  isc_info_rsb_sort              =          6;
  isc_info_rsb_first             =          7;
  isc_info_rsb_boolean           =          8;
  isc_info_rsb_union             =          9;
  isc_info_rsb_aggregate         =         10;
  isc_info_rsb_merge             =         11;
  isc_info_rsb_ext_sequential    =         12;
  isc_info_rsb_ext_indexed       =         13;
  isc_info_rsb_ext_dbkey         =         14;
  isc_info_rsb_left_cross        =         15;
  isc_info_rsb_select            =         16;
  isc_info_rsb_sql_join          =         17;
  isc_info_rsb_simulate          =         18;
  isc_info_rsb_sim_cross         =         19;
  isc_info_rsb_once              =         20;
  isc_info_rsb_procedure         =         21;

{_______________________________________________________________________________
/**********************/
/* Bitmap expressions */
/**********************/
_______________________________________________________________________________}
  isc_info_rsb_and               =          1;
  isc_info_rsb_or                =          2;
  isc_info_rsb_dbkey             =          3;
  isc_info_rsb_index             =          4;

  isc_info_req_active            =          2;
  isc_info_req_inactive          =          3;
  isc_info_req_send              =          4;
  isc_info_req_receive           =          5;
  isc_info_req_select            =          6;
  isc_info_req_sql_stall         =          7;

{_______________________________________________________________________________
/**************************/
/* Blob information items */
/**************************/
_______________________________________________________________________________}
  isc_info_blob_num_segments     =          4;
  isc_info_blob_max_segment      =          5;
  isc_info_blob_total_length     =          6;
  isc_info_blob_type             =          7;

{_______________________________________________________________________________
/*********************************/
/* Transaction information items */
/*********************************/
_______________________________________________________________________________}
  isc_info_tra_id                =          4;
  isc_info_tra_oldest_interesting=          5;
  isc_info_tra_oldest_snapshot   =          6;
  isc_info_tra_oldest_active     =          7;
  isc_info_tra_isolation         =          8;
  isc_info_tra_access            =          9;
  isc_info_tra_lock_timeout      =         10;
  fb_info_tra_dbpath             =         11;

// isc_info_tra_isolation responses
   isc_info_tra_consistency      =          1;
   isc_info_tra_concurrency      =          2;
   isc_info_tra_read_committed   =          3;

// isc_info_tra_read_committed options
   isc_info_tra_no_rec_version   =           0;
   isc_info_tra_rec_version      =           1;

// isc_info_tra_access responses
   isc_info_tra_readonly         =           0;
   isc_info_tra_readwrite        =           1;

{_______________________________________________________________________________
/*************************/
/* SQL information items */
/*************************/
_______________________________________________________________________________}
  isc_info_sql_select             =   4;
  isc_info_sql_bind               =   5;
  isc_info_sql_num_variables      =   6;
  isc_info_sql_describe_vars      =   7;
  isc_info_sql_describe_end       =   8;
  isc_info_sql_sqlda_seq          =   9;
  isc_info_sql_message_seq        =  10;
  isc_info_sql_type               =  11;
  isc_info_sql_sub_type           =  12;
  isc_info_sql_scale              =  13;
  isc_info_sql_length             =  14;
  isc_info_sql_null_ind           =  15;
  isc_info_sql_field              =  16;
  isc_info_sql_relation           =  17;
  isc_info_sql_owner              =  18;
  isc_info_sql_alias              =  19;
  isc_info_sql_sqlda_start        =  20;
  isc_info_sql_stmt_type          =  21;
  isc_info_sql_get_plan           =  22;
  isc_info_sql_records            =  23;
  isc_info_sql_batch_fetch        =  24;
  isc_info_sql_relation_alias     =  25;
  isc_info_sql_explain_plan       =  26;
  isc_info_sql_stmt_flags         =  27;

{_______________________________________________________________________________
/*********************************/
/* SQL information return values */
/*********************************/
_______________________________________________________________________________}
  isc_info_sql_stmt_select        = 1;
  isc_info_sql_stmt_insert        = 2;
  isc_info_sql_stmt_update        = 3;
  isc_info_sql_stmt_delete        = 4;
  isc_info_sql_stmt_ddl           = 5;
  isc_info_sql_stmt_get_segment   = 6;
  isc_info_sql_stmt_put_segment   = 7;
  isc_info_sql_stmt_exec_procedure= 8;
  isc_info_sql_stmt_start_trans   = 9;
  isc_info_sql_stmt_commit        =10;
  isc_info_sql_stmt_rollback      =11;
  isc_info_sql_stmt_select_for_upd=12;
  isc_info_sql_stmt_set_generator =13;


 

{_______________________________________________________________________________
  (* Database parameter block stuff *)
_______________________________________________________________________________}
  isc_dpb_version1               =   1;
  isc_dpb_version2               =   2;
  isc_dpb_cdd_pathname           =   1;
  isc_dpb_allocation             =   2;
  isc_dpb_journal                =   3;
  isc_dpb_page_size              =   4;
  isc_dpb_num_buffers            =   5;
  isc_dpb_buffer_length          =   6;
  isc_dpb_debug                  =   7;
  isc_dpb_garbage_collect        =   8;
  isc_dpb_verify                 =   9;
  isc_dpb_sweep                  =  10;
  isc_dpb_enable_journal         =  11;
  isc_dpb_disable_journal        =  12;
  isc_dpb_dbkey_scope            =  13;
  isc_dpb_number_of_users        =  14;
  isc_dpb_trace                  =  15;
  isc_dpb_no_garbage_collect     =  16;
  isc_dpb_damaged                =  17;
  isc_dpb_license                =  18;
  isc_dpb_sys_user_name          =  19;
  isc_dpb_encrypt_key            =  20;
  isc_dpb_activate_shadow        =  21;
  isc_dpb_sweep_interval         =  22;
  isc_dpb_delete_shadow          =  23;
  isc_dpb_force_write            =  24;
  isc_dpb_begin_log              =  25;
  isc_dpb_quit_log               =  26;
  isc_dpb_no_reserve             =  27;
  isc_dpb_user_name              =  28;
  isc_dpb_password               =  29;
  isc_dpb_password_enc           =  30;
  isc_dpb_sys_user_name_enc      =  31;
  isc_dpb_interp                 =  32;
  isc_dpb_online_dump            =  33;
  isc_dpb_old_file_size          =  34;
  isc_dpb_old_num_files          =  35;
  isc_dpb_old_file               =  36;
  isc_dpb_old_start_page         =  37;
  isc_dpb_old_start_seqno        =  38;
  isc_dpb_old_start_file         =  39;
  isc_dpb_drop_walfile           =  40;
  isc_dpb_old_dump_id            =  41;
  isc_dpb_wal_backup_dir         =  42;
  isc_dpb_wal_chkptlen           =  43;
  isc_dpb_wal_numbufs            =  44;
  isc_dpb_wal_bufsize            =  45;
  isc_dpb_wal_grp_cmt_wait       =  46;
  isc_dpb_lc_messages            =  47;
  isc_dpb_lc_ctype               =  48;
  isc_dpb_cache_manager          =  49;
  isc_dpb_shutdown               =  50;
  isc_dpb_online                 =  51;
  isc_dpb_shutdown_delay         =  52;
  isc_dpb_reserved               =  53;
  isc_dpb_overwrite              =  54;
  isc_dpb_sec_attach             =  55;
  isc_dpb_disable_wal            =  56;
  isc_dpb_connect_timeout        =  57;
  isc_dpb_dummy_packet_interval  =  58;
  isc_dpb_gbak_attach            =  59;
  isc_dpb_sql_role_name          =  60;
  isc_dpb_set_page_buffers       =  61;
  isc_dpb_working_directory      =  62;
  isc_dpb_SQL_dialect            =  63;
  isc_dpb_set_db_readonly        =  64;
  isc_dpb_set_db_SQL_dialect     =  65;
  isc_dpb_gfix_attach            =  66;
  isc_dpb_gstat_attach           =  67;
  isc_dpb_set_db_charset         =  68;
//isc_dpb_gsec_attach            =  69;  /* deprecated */
  isc_dpb_address_path           =  70;
  isc_dpb_process_id             =  71;
  isc_dpb_no_db_triggers         =  72;
  isc_dpb_trusted_auth           =  73;
  isc_dpb_process_name           =  74;
  isc_dpb_trusted_role           =  75;
  isc_dpb_org_filename           =  76;
  isc_dpb_utf8_filename          =  77;
  isc_dpb_ext_call_depth         =  78;
  isc_dpb_auth_block             =  79;
  isc_dpb_client_version         =  80;
  isc_dpb_remote_protocol        =  81;
  isc_dpb_host_name              =  82;
  isc_dpb_os_user                =  83;
  isc_dpb_specific_auth_data     =  84;
  isc_dpb_auth_plugin_list       =  85;
  isc_dpb_auth_plugin_name       =  86;
  isc_dpb_config                 =  87;
  isc_dpb_nolinger               =  88;
  isc_dpb_last_dpb_constant      =  isc_dpb_nolinger;


{_______________________________________________________________________________
/**************************************************/
/* clumplet tags used inside isc_dpb_address_path */
/*             and isc_spb_address_path */
/**************************************************/

/* Format of this clumplet is the following:

 <address-path-clumplet> ::=
  isc_dpb_address_path <byte-clumplet-length> <address-stack>

 <address-stack> ::=
  <address-descriptor> |
  <address-stack> <address-descriptor>

 <address-descriptor> ::=
  isc_dpb_address <byte-clumplet-length> <address-elements>

 <address-elements> ::=
  <address-element> |
  <address-elements> <address-element>

 <address-element> ::=
  isc_dpb_addr_protocol <byte-clumplet-length> <protocol-string> |
  isc_dpb_addr_endpoint <byte-clumplet-length> <remote-endpoint-string>

 <protocol-string> ::=
  "TCPv4" |
  "TCPv6" |
  "XNET" |
  "WNET" |
  ....

 <remote-endpoint-string> ::=
  <IPv4-address> | // such as "172.20.1.1"
  <IPv6-address> | // such as "2001:0:13FF:09FF::1"
  <xnet-process-id> | // such as "17864"
  ...
*/
_______________________________________________________________________________}

  isc_dpb_address       =  1;
  isc_dpb_addr_protocol =  1;
  isc_dpb_addr_endpoint =  2;

{_______________________________________________________________________________
/*********************************/
/* isc_dpb_verify specific flags */
/*********************************/
_______________________________________________________________________________}
  isc_dpb_pages                  =          1;
  isc_dpb_records                =          2;
  isc_dpb_indices                =          4;
  isc_dpb_transactions           =          8;
  isc_dpb_no_update              =         16;
  isc_dpb_repair                 =         32;
  isc_dpb_ignore                 =         64;

{_______________________________________________________________________________
/***********************************/
/* isc_dpb_shutdown specific flags */
/***********************************/
_______________________________________________________________________________}
  isc_dpb_shut_cache             =     1;
  isc_dpb_shut_attachment        =     2;
  isc_dpb_shut_transaction       =     4;
  isc_dpb_shut_force             =     8;
  isc_dpb_shut_mode_mask         =   $70;

  isc_dpb_shut_default           =   $00;
  isc_dpb_shut_normal            =   $10;
  isc_dpb_shut_multi             =   $20;
  isc_dpb_shut_single            =   $30;
  isc_dpb_shut_full              =   $40;

{_______________________________________________________________________________
/*************************************/
/* Transaction parameter block stuff */
/*************************************/
_______________________________________________________________________________}
  isc_tpb_version1               =          1;
  isc_tpb_version3               =          3;
  isc_tpb_consistency            =          1;
  isc_tpb_concurrency            =          2;
  isc_tpb_shared                 =          3;
  isc_tpb_protected              =          4;
  isc_tpb_exclusive              =          5;
  isc_tpb_wait                   =          6;
  isc_tpb_nowait                 =          7;
  isc_tpb_read                   =          8;
  isc_tpb_write                  =          9;
  isc_tpb_lock_read              =         10;
  isc_tpb_lock_write             =         11;
  isc_tpb_verb_time              =         12;
  isc_tpb_commit_time            =         13;
  isc_tpb_ignore_limbo           =         14;
  isc_tpb_read_committed         =         15;
  isc_tpb_autocommit             =         16;
  isc_tpb_rec_version            =         17;
  isc_tpb_no_rec_version         =         18;
  isc_tpb_restart_requests       =         19;
  isc_tpb_no_auto_undo           =         20;
  isc_tpb_lock_timeout           =         21;
  isc_tpb_last_tpb_constant      =         isc_tpb_lock_timeout;

{_______________________________________________________________________________
/************************/
/* Blob Parameter Block */
/************************/
_______________________________________________________________________________}
  isc_bpb_version1               =          1;
  isc_bpb_source_type            =          1;
  isc_bpb_target_type            =          2;
  isc_bpb_type                   =          3;
  isc_bpb_source_interp          =          4;
  isc_bpb_target_interp          =          5;
  isc_bpb_filter_parameter       =          6;
  isc_bpb_storage                =          7;

  isc_bpb_type_segmented         =          0;
  isc_bpb_type_stream            =          1;
  isc_bpb_storage_main           =          0;
  isc_bpb_storage_temp           =          2;


{_______________________________________________________________________________
/*********************************/
/* Service parameter block stuff */
/*********************************/
_______________________________________________________________________________}
  isc_spb_version1               = 1;
  isc_spb_current_version        = 2;
  isc_spb_version                 = isc_spb_current_version;
  isc_spb_version3               = 3;
  isc_spb_user_name              = isc_dpb_user_name;
  isc_spb_sys_user_name          = isc_dpb_sys_user_name;
  isc_spb_sys_user_name_enc      = isc_dpb_sys_user_name_enc;
  isc_spb_password               = isc_dpb_password;
  isc_spb_password_enc           = isc_dpb_password_enc;
  isc_spb_command_line           = 105;
  isc_spb_dbname                 = 106;
  isc_spb_verbose                = 107;
  isc_spb_options                = 108;
  isc_spb_address_path           = 109;
  isc_spb_process_id             = 110;
  isc_spb_trusted_auth           = 111;
  isc_spb_process_name           = 112;
  isc_spb_trusted_role           = 113;
  isc_spb_verbint                = 114;
  isc_spb_auth_block             = 115;
  isc_spb_auth_plugin_name       = 116;
  isc_spb_auth_plugin_list       = 117;
  isc_spb_utf8_filename          = 118;
  isc_spb_client_version         = 119;
  isc_spb_remote_protocol        = 120;
  isc_spb_host_name              = 121;
  isc_spb_os_user                = 122;
  isc_spb_config                 = 123;
  isc_spb_expected_db            = 124;
  isc_spb_connect_timeout        = isc_dpb_connect_timeout;
  isc_spb_dummy_packet_interval  = isc_dpb_dummy_packet_interval;
  isc_spb_sql_role_name          = isc_dpb_sql_role_name;
// This will not be used in protocol 13, therefore may be reused
  isc_spb_specific_auth_data     = isc_spb_trusted_auth;

{_______________________________________________________________________________
/*****************************
 * Service action items      *
 *****************************/
_______________________________________________________________________________}
  isc_action_svc_backup           =  1;                                         // Starts database backup process on the server
  isc_action_svc_restore          =  2;                                         // Starts database restore process on the server
  isc_action_svc_repair           =  3;                                         // Starts database repair process on the server
  isc_action_svc_add_user         =  4;                                         // Adds a new user to the security database
  isc_action_svc_delete_user      =  5;                                         // Deletes a user record from the security database
  isc_action_svc_modify_user      =  6;                                         // Modifies a user record in the security database
  isc_action_svc_display_user     =  7;                                         // Displays a user record from the security database
  isc_action_svc_properties       =  8;                                         // Sets database properties
  isc_action_svc_add_license      =  9;                                         // Adds a license to the license file
  isc_action_svc_remove_license   = 10;                                         // Removes a license from the license file
  isc_action_svc_db_stats         = 11;                                         // Retrieves database statistics
  isc_action_svc_get_ib_log       = 12;                                         // Retrieves the Firebird log file from the server
  isc_action_svc_get_fb_log       = 12;                                         // Retrieves the Firebird log file from the server */
  isc_action_svc_nbak             = 20;                                         // Incremental nbackup */
  isc_action_svc_nrest            = 21;                                         // Incremental database restore */
  isc_action_svc_trace_start      = 22;                                         // Start trace session
  isc_action_svc_trace_stop       = 23;                                         // Stop trace session
  isc_action_svc_trace_suspend    = 24;                                         // Suspend trace session
  isc_action_svc_trace_resume     = 25;                                         // Resume trace session
  isc_action_svc_trace_list       = 26;                                         // List existing sessions
  isc_action_svc_set_mapping      = 27;                                         // Set auto admins mapping in security database
  isc_action_svc_drop_mapping     = 28;                                         // Drop auto admins mapping in security database
  isc_action_svc_display_user_adm = 29;                                         // Displays user(s) from security database with admin info
  isc_action_svc_last             = 30;                                         // keep it last !

{_______________________________________________________________________________
/*****************************
 * Service information items *
 *****************************/
_______________________________________________________________________________}
  isc_info_svc_svr_db_info        = 50;                                         // Retrieves the number of attachments and databases
  isc_info_svc_get_license        = 51;                                         // Retrieves all license keys and IDs from the license file
  isc_info_svc_get_license_mask   = 52;                                         // Retrieves a bitmask representing licensed options on the server
  isc_info_svc_get_config         = 53;                                         // Retrieves the parameters and values for IB_CONFIG
  isc_info_svc_version            = 54;                                         // Retrieves the version of the services manager
  isc_info_svc_server_version     = 55;                                         //Retrieves the version of the Firebird server
  isc_info_svc_implementation     = 56;                                         // Retrieves the implementation of the Firebird server
  isc_info_svc_capabilities       = 57;                                         // Retrieves a bitmask representing the server's capabilities
  isc_info_svc_user_dbpath        = 58;                                         // Retrieves the path to the security database in use by the server
  isc_info_svc_get_env            = 59;                                         // Retrieves the setting of $INTERBASE
  isc_info_svc_get_env_lock       = 60;                                         // Retrieves the setting of $INTERBASE_LCK
  isc_info_svc_get_env_msg        = 61;                                         // Retrieves the setting of $INTERBASE_MSG
  isc_info_svc_line               = 62;                                         // Retrieves 1 line of service output per call
  isc_info_svc_to_eof             = 63;                                         // Retrieves as much of the server output as will fit in the supplied buffer
  isc_info_svc_timeout            = 64;                                         // Sets / signifies a timeout value for reading service information
  isc_info_svc_get_licensed_users = 65;                                         // Retrieves the number of users licensed for accessing the server
  isc_info_svc_limbo_trans        = 66;                                         // Retrieve the limbo transactions
  isc_info_svc_running            = 67;                                         // Checks to see if a service is running on an attachment
  isc_info_svc_get_users          = 68;                                         // Returns the user information from isc_action_svc_display_users
  isc_info_svc_auth_block         = 69;                                          // Sets authentication block for service query() call */
  isc_info_svc_stdin              = 78;                                          // Returns maximum size of data, needed as stdin for service */

{_______________________________________________________________________________
/******************************************************
 * Parameters for isc_action_{add|del|mod|disp)_user  *
 ******************************************************/
_______________________________________________________________________________}
  isc_spb_sec_userid              =  5;
  isc_spb_sec_groupid             =  6;
  isc_spb_sec_username            =  7;
  isc_spb_sec_password            =  8;
  isc_spb_sec_groupname           =  9;
  isc_spb_sec_firstname           = 10;
  isc_spb_sec_middlename          = 11;
  isc_spb_sec_lastname            = 12;
  isc_spb_sec_admin               = 13;

{_______________________________________________________________________________
/*****************************************
 * Parameters for isc_action_svc_backup  *
 *****************************************/
_______________________________________________________________________________}
  isc_spb_bkp_file                =   5;
  isc_spb_bkp_factor              =   6;
  isc_spb_bkp_length              =   7;
  isc_spb_bkp_skip_data           =   8;

  isc_spb_bkp_ignore_checksums    = $01;
  isc_spb_bkp_ignore_limbo        = $02;
  isc_spb_bkp_metadata_only       = $04;
  isc_spb_bkp_no_garbage_collect  = $08;
  isc_spb_bkp_old_descriptions    = $10;
  isc_spb_bkp_non_transportable   = $20;
  isc_spb_bkp_convert             = $40;
  isc_spb_bkp_expand              = $80;
  isc_spb_bkp_no_triggers         = $8000;

{_______________________________________________________________________________
/********************************************
 * Parameters for isc_action_svc_properties *
 ********************************************/
_______________________________________________________________________________}
  isc_spb_prp_page_buffers          =  5;
  isc_spb_prp_sweep_interval        =  6;
  isc_spb_prp_shutdown_db           =  7;
  isc_spb_prp_deny_new_attachments  =  9;
  isc_spb_prp_deny_new_transactions = 10;
  isc_spb_prp_reserve_space         = 11;
  isc_spb_prp_write_mode            = 12;
  isc_spb_prp_access_mode           = 13;
  isc_spb_prp_set_sql_dialect       = 14;

  isc_spb_prp_activate              = $0100;
  isc_spb_prp_db_online             = $0200;
  isc_spb_prp_nolinger              = $0400;

  isc_spb_prp_force_shutdown        = 41;
  isc_spb_prp_attachments_shutdown  = 42;
  isc_spb_prp_transactions_shutdown = 43;
  isc_spb_prp_shutdown_mode         = 44;
  isc_spb_prp_online_mode           = 45;

{_______________________________________________________________________________
/********************************************
 * Parameters for isc_spb_prp_shutdown_mode *
 *            and isc_spb_prp_online_mode   *
 ********************************************/
_______________________________________________________________________________}
  isc_spb_prp_sm_normal             = 0;
  isc_spb_prp_sm_multi              = 1;
  isc_spb_prp_sm_single             = 2;
  isc_spb_prp_sm_full               = 3;


{_______________________________________________________________________________
/********************************************
 * Parameters for isc_spb_prp_reserve_space *
 ********************************************/
_______________________________________________________________________________}
  isc_spb_prp_res_use_full          = 35;
  isc_spb_prp_res                   = 36;

{_______________________________________________________________________________
/******************************************
 * Parameters for isc_spb_prp_write_mode  *
 ******************************************/
_______________________________________________________________________________}
  isc_spb_prp_wm_async              = 37;
  isc_spb_prp_wm_sync               = 38;


{_______________________________________________________________________________
 /******************************************
 * Parameters for isc_spb_prp_access_mode *
 ******************************************/
_______________________________________________________________________________}
  isc_spb_prp_am_readonly           = 39;
  isc_spb_prp_am_readwrite          = 40;

{_______________________________________________________________________________
 /*****************************************
 * Parameters for isc_action_svc_repair  *
 *****************************************/
_______________________________________________________________________________}
  isc_spb_rpr_commit_trans          = 15;
  isc_spb_rpr_rollback_trans        = 34;
  isc_spb_rpr_recover_two_phase     = 17;
  isc_spb_tra_id                    = 18;
  isc_spb_single_tra_id             = 19;
  isc_spb_multi_tra_id              = 20;
  isc_spb_tra_state                 = 21;
  isc_spb_tra_state_limbo           = 22;
  isc_spb_tra_state_commit          = 23;
  isc_spb_tra_state_rollback        = 24;
  isc_spb_tra_state_unknown         = 25;
  isc_spb_tra_host_site             = 26;
  isc_spb_tra_remote_site           = 27;
  isc_spb_tra_db_path               = 28;
  isc_spb_tra_advise                = 29;
  isc_spb_tra_advise_commit         = 30;
  isc_spb_tra_advise_rollback       = 31;
  isc_spb_tra_advise_unknown        = 33;

  isc_spb_rpr_validate_db           = $01;
  isc_spb_rpr_sweep_db              = $02;
  isc_spb_rpr_mend_db               = $04;
  isc_spb_rpr_list_limbo_trans      = $08;
  isc_spb_rpr_check_db              = $10;
  isc_spb_rpr_ignore_checksum       = $20;
  isc_spb_rpr_kill_shadows          = $40;
  isc_spb_rpr_full                  = $80;

{_______________________________________________________________________________
/*****************************************
 * Parameters for isc_action_svc_restore *
 *****************************************/
________________________________________________________________________________}
  isc_spb_res_skip_data             = isc_spb_bkp_skip_data;
  isc_spb_res_buffers               =  9;
  isc_spb_res_page_size             = 10;
  isc_spb_res_length                = 11;
  isc_spb_res_access_mode           = 12;
  isc_spb_res_fix_fss_data          = 13;
  isc_spb_res_fix_fss_metadata      = 14;
  isc_spb_res_metadata_only         = isc_spb_bkp_metadata_only;
  isc_spb_res_deactivate_idx        =$0100;
  isc_spb_res_no_shadow             =$0200;
  isc_spb_res_no_validity           =$0400;
  isc_spb_res_one_at_a_time         =$0800;
  isc_spb_res_replace               =$1000;
  isc_spb_res_create                =$2000;
  isc_spb_res_use_all_space         =$4000;

{_______________________________________________________________________________
/******************************************
 * Parameters for isc_spb_res_access_mode  *
 ******************************************/
________________________________________________________________________________}
  isc_spb_res_am_readonly           = isc_spb_prp_am_readonly;
  isc_spb_res_am_readwrite          = isc_spb_prp_am_readwrite;

{_______________________________________________________________________________
/*******************************************
 * Parameters for isc_info_svc_svr_db_info *
 *******************************************/
________________________________________________________________________________}
  isc_spb_num_att                   = 5;
  isc_spb_num_db                    = 6;

{_______________________________________________________________________________
/*****************************************
 * Parameters for isc_info_svc_db_stats  *
 *****************************************/
________________________________________________________________________________}
  isc_spb_sts_data_pages            =$001;
  isc_spb_sts_db_log                =$002;
  isc_spb_sts_hdr_pages             =$004;
  isc_spb_sts_idx_pages             =$008;
  isc_spb_sts_sys_relations         =$010;
  isc_spb_sts_record_versions       =$020;
//#define isc_spb_sts_table      0x40
  isc_spb_sts_nocreation            =$080;
  isc_spb_sts_encryption            =$100;

{_______________________________________________________________________________
/***********************************/
/* Server configuration key values */
/***********************************/

/* Not available in Firebird 1.5 */

/***************************************
 * Parameters for isc_action_svc_nbak  *
 ***************************************/
________________________________________________________________________________}
  isc_spb_nbk_level                 = 5;
  isc_spb_nbk_file                  = 6;
  isc_spb_nbk_direct                = 7;
isc_spb_nbk_no_triggers             = $01;

{_______________________________________________________________________________
/***************************************
 * Parameters for isc_action_svc_trace *
 ***************************************/
________________________________________________________________________________}
  isc_spb_trc_id                    = 1;
  isc_spb_trc_name                  = 2;
  isc_spb_trc_cfg                   = 3;

{_______________________________________________________________________________
/******************************************/
/* Array slice description language (SDL) */
/******************************************/
________________________________________________________________________________}
  isc_sdl_version1               =          1;
  isc_sdl_eoc                    =         -1;
  isc_sdl_relation               =          2;
  isc_sdl_rid                    =          3;
  isc_sdl_field                  =          4;
  isc_sdl_fid                    =          5;
  isc_sdl_struct                 =          6;
  isc_sdl_variable               =          7;
  isc_sdl_scalar                 =          8;
  isc_sdl_tiny_integer           =          9;
  isc_sdl_short_integer          =         10;
  isc_sdl_long_integer           =         11;
//isc_sdl_literal                =         12;
  isc_sdl_add                    =         13;
  isc_sdl_subtract               =         14;
  isc_sdl_multiply               =         15;
  isc_sdl_divide                 =         16;
  isc_sdl_negate                 =         17;
//isc_sdl_eql                    =         18;
//isc_sdl_neq                    =         19;
//isc_sdl_gtr                    =         20;
//isc_sdl_geq                    =         21;
//isc_sdl_lss                    =         22;
//isc_sdl_leq                    =         23;
//isc_sdl_and                    =         24;
//isc_sdl_or                     =         25;
//isc_sdl_not                    =         26;
//isc_sdl_while                  =         27;
//isc_sdl_assignment             =         28;
//isc_sdl_label                  =         29;
//isc_sdl_leave                  =         30;
  isc_sdl_begin                  =         31;
  isc_sdl_end                    =         32;
  isc_sdl_do3                    =         33;
  isc_sdl_do2                    =         34;
  isc_sdl_do1                    =         35;
  isc_sdl_element                =         36;

{_______________________________________________________________________________
/********************************************/
/* International text interpretation values */
/********************************************/
________________________________________________________________________________}
//isc_interp_eng_ascii           =          0;
//isc_interp_jpn_sjis            =          5;
//isc_interp_jpn_euc             =          6;


(*******************)
(** Blob Subtypes **)
(*******************)

(** types less than zero are reserved for customer use **)
  isc_blob_untyped               =          0;

(** internal subtypes **)

  isc_blob_text                  =          1;
  isc_blob_blr                   =          2;
  isc_blob_acl                   =          3;
  isc_blob_ranges                =          4;
  isc_blob_summary               =          5;
  isc_blob_format                =          6;
  isc_blob_tra                   =          7;
  isc_blob_extfile               =          8;

(** the range 20-30 is reserved for dBASE and Paradox types **)

//isc_blob_formatted_memo        =         20;
//isc_blob_paradox_ole           =         21;
//isc_blob_graphic               =         22;
//isc_blob_dbase_ole             =         23;
//isc_blob_typed_binary          =         24;


{_______________________________________________________________________________
/***********************************/
/* Masks for fb_shutdown_callback  */
/***********************************/
________________________________________________________________________________}
  fb_shut_confirmation             = 1;
  fb_shut_preproviders             = 2;
  fb_shut_postproviders            = 4;
  fb_shut_finish                   = 8;
  fb_shut_exit                     = 16;

{_______________________________________________________________________________
/****************************************/
/* Shutdown reasons, used by engine     */
/* Users should provide positive values */
/****************************************/
________________________________________________________________________________}
  fb_shutrsn_svc_stopped           = -1;
  fb_shutrsn_no_connection         = -2;
  fb_shutrsn_app_stopped           = -3;
//fb_shutrsn_device_removed        = -4;
  fb_shutrsn_signal                = -5;
  fb_shutrsn_services              = -6;
  fb_shutrsn_exit_called           = -7;

{_______________________________________________________________________________
/****************************************/
/* Cancel types for fb_cancel_operation */
/****************************************/
________________________________________________________________________________}
fb_cancel_disable                 = 1;
fb_cancel_enable                  = 2;
fb_cancel_raise                   = 3;
fb_cancel_abort                   = 4;

{_______________________________________________________________________________
/********************************************/
/* Debug information items          */
/********************************************/
________________________________________________________________________________}
fb_dbg_version                   = 1;
fb_dbg_end                       = 255;
fb_dbg_map_src2blr               = 2;
fb_dbg_map_varname               = 3;
fb_dbg_map_argument              = 4;
fb_dbg_subproc                   = 5;
fb_dbg_subfunc                   = 6;
fb_dbg_map_curname               = 7;

// sub code for fb_dbg_map_argument
fb_dbg_arg_input                 = 0;
fb_dbg_arg_output                = 1;


/// <summary>FXFree</summary>
procedure FXFree(var P:Pointer);overload;inline;
/// <summary>FXFree</summary>
procedure FXFree(var P:PFXByte);overload;inline;

/// <summary>FXAlloc</summary>
procedure FXAlloc(var P:Pointer;Const NewSize: Integer;Const RAZ:Boolean);overload;inline;
/// <summary>FXAlloc</summary>
procedure FXAlloc(var P:PFXByte;Const NewSize: Integer;Const RAZ:Boolean);overload;inline;
/// <summary>FXAlloc</summary>
procedure FXReAlloc(var P:Pointer;Const OldSize, NewSize: Integer;Const RAZ:Boolean);overload;inline;
/// <summary>FXAlloc</summary>
procedure FXReAlloc(var P:PFXByte;Const OldSize, NewSize: Integer;Const RAZ:Boolean);overload;inline;
/// <summary>FXAlloc</summary>
procedure FXReAlloc(var P:PXSQLDA;Const OldSize, NewSize: Integer;Const RAZ:Boolean);overload;inline;


/// <summary>vax_integer</summary>
function vax_integer(Const buffer:PFXByte;Const length:FXShort):FXLong;overload;inline;
/// <summary>vax_integer</summary>
function vax_integer(Const buffer:PFXByte;Const OffSet:FXLong;Const length:FXShort):FXLong;overload;inline;
/// <summary>vax_Short</summary>
function vax_Short(Const buffer:PFXByte;Const length:FXShort):FXShort;overload;inline;
/// <summary>vax_Short</summary>
function vax_Short(Const buffer:PFXByte;Const OffSet:FXLong;Const length:FXShort):FXShort;overload;inline;


(** XSQLDA_LENGTH is defined in C as a macro, but in Pascal we must defined it as a function... **)
(*  The C-macro reads like this: XSQLDA_LENGTH(n)  (sizeof (XSQLDA) + (n-1) * sizeof (XSQLVAR)) *)
function XSQLDA_LENGTH(Const n: FXLong): FXLong;inline;

(** Service manager functions **)
(** #define ADD_SPB_LENGTH(p, length)  {*(p)++ = (length); \ *(p)++ = (length) >> 8;} *)
procedure add_spb_length(var p: PAnsiChar;Const length: integer);inline;
(** #define ADD_SPB_NUMERIC(p, data)  {*(p)++ = (data); \ *(p)++ = (data) >> 8; \ *(p)++ = (data) >> 16; \ *(p)++ = (data) >> 24;} *)
procedure add_spb_numeric(var p: PAnsiChar;Const data: integer);

(** Encode/Decode Date functions **)
procedure DecodeSQLDate(Const v:PISC_DATE;out DateTime:TDateTime);inline;overload;
procedure DecodeSQLDate(Const v:PISC_DATE;out TimeStamp:TTimeStamp);inline;overload;
function EncodeDateTime2SQLDate(Const DateTime:TDateTime):ISC_DATE;inline;
function EncodeSQLDate2TimeStamp(Const TimeStamp:TTimeStamp):ISC_DATE;inline;

procedure DecodeSQLTime(Const v:PISC_TIME;out DateTime:TDateTime);inline;overload;
procedure DecodeSQLTime(Const v:PISC_TIME;out TimeStamp:TTimeStamp);inline;overload;
function EncodeDateTime2SQLTime(Const DateTime:TDateTime):ISC_TIME;inline;
function EncodeTimeStamp2SQLTime(Const TimeStamp:TTimeStamp):ISC_TIME;inline;

procedure DecodeSQLTimeStamp(Const v:PISC_TIMESTAMP;out TimeStamp:TTimeStamp);inline;overload;
procedure DecodeSQLTimeStamp(Const v:PISC_TIMESTAMP;out DateTime:TDateTime);inline;overload;
function EncodeDateTime2SQLTimeStamp(Const DateTime:TDateTime):ISC_TIMESTAMP;inline;
function EncodeTimeStamp2SQLTimeStamp(Const TimeStamp:TTimeStamp):ISC_TIMESTAMP;inline;

implementation

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure FXFree(var P:Pointer);
Var SavP:FXVoid;
Begin
  if Assigned(P) then Begin
    SavP:=P;P:=nil;
    FreeMem(SavP);
    end
End;
{______________________________________________________________________________}
procedure FXFree(var P:PFXByte);
Var SavP:PFXByte;
Begin
  if Assigned(P) then Begin
    SavP:=P;P:=nil;
    FreeMem(SavP);
    end
End;
{______________________________________________________________________________}
procedure FXAlloc(var P:Pointer;Const NewSize: Integer;Const RAZ:Boolean);
var pb:PFXByte;
    i:Integer;
begin
  if Assigned(P) then
    ReallocMem(P, NewSize) else
    GetMem(P, NewSize);

  if RAZ then Begin
    pb:=P;
    for i := 0 to Pred(NewSize) do Begin
      Byte(pb^):=0;Inc(pb);
    end end;
end;
{______________________________________________________________________________}
procedure FXAlloc(var P:PFXByte;Const NewSize: Integer;Const RAZ:Boolean);
var pb:PFXByte;
    i:Integer;
begin
  if Assigned(P) then
    ReallocMem(P, NewSize) else
    GetMem(P, NewSize);

  if RAZ then Begin
    pb:=P;
    for i := 0 to Pred(NewSize) do Begin
      Byte(pb^):=0;Inc(pb);
    end end;
end;
{______________________________________________________________________________}
procedure FXReAlloc(var P:Pointer;Const OldSize, NewSize: Integer;Const RAZ:Boolean);
var pb:PFXByte;
    i:Integer;
begin
  if Assigned(P) then
    ReallocMem(P, NewSize) else
    GetMem(P, NewSize);

  if RAZ then Begin
    pb:=P;
    Inc(pb,OldSize);
    for i := OldSize to Pred(NewSize) do Begin
      Byte(pb^):=0;Inc(pb);
    end end;
end;
{______________________________________________________________________________}
procedure FXReAlloc(var P:PFXByte;Const OldSize, NewSize: Integer;Const RAZ:Boolean);
var pb:PFXByte;
    i:Integer;
begin
  if Assigned(P) then
    ReallocMem(P, NewSize) else
    GetMem(P, NewSize);

  if RAZ then Begin
    pb:=P;
    Inc(pb,OldSize);
    for i := OldSize to Pred(NewSize) do Begin
      Byte(pb^):=0;Inc(pb);
    end end;
end;
{______________________________________________________________________________}
procedure FXReAlloc(var P:PXSQLDA;Const OldSize, NewSize: Integer;Const RAZ:Boolean);
var pb:PFXByte;
    i:Integer;
begin
  if Assigned(P) then
    ReallocMem(P, NewSize) else
    GetMem(P, NewSize);

  if RAZ then Begin
    pb:=Pointer(P);
    Inc(pb,OldSize);
    for i := OldSize to Pred(NewSize) do Begin
      Byte(pb^):=0;Inc(pb);
    end end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function vax_integer(Const buffer:PFXByte;Const OffSet:FXLong;Const length:FXShort):FXLong;
Var Shift,l:Integer;
    pb:PFXByte;
    b:Byte;
Begin
  l:=length;
  shift:=0;
  Result:=0;
  pb:=buffer;
  Inc(pb,OffSet);
  while (l > 0) do Begin
    Dec(l);
    b:=pb^;Inc(pb);
    Result:=Result+(b shl shift);
    Inc(shift,8);
    end;
End;
{______________________________________________________________________________}
function vax_integer(Const buffer:PFXByte;Const length:FXShort):FXLong;
Var Shift,l:Integer;
    pb:PFXByte;
    b:Byte;
Begin
  l:=length;
  shift:=0;
  Result:=0;
  pb:=buffer;
  while (l > 0) do Begin
    Dec(l);
    b:=pb^;Inc(pb);
    Result:=Result+(b shl shift);
    Inc(shift,8);
    end;
End;
{______________________________________________________________________________}
function vax_short(Const buffer:PFXByte;Const OffSet:FXLong;Const length:FXShort):FXShort;
Var Shift,l:Integer;
    pb:PFXByte;
    b:Byte;
Begin
  l:=length;
  shift:=0;
  Result:=0;
  pb:=buffer;Inc(pb,Offset);
  while (l > 0) do Begin
    Dec(l);
    b:=pb^;Inc(pb);
    Result:=Result+(b shl shift);
    Inc(shift,8);
    end;
End;
{______________________________________________________________________________}
function vax_short(Const buffer:PFXByte;Const length:FXShort):FXShort;
Var Shift,l:Integer;
    pb:PFXByte;
    b:Byte;
Begin
  l:=length;
  shift:=0;
  Result:=0;
  pb:=buffer;
  while (l > 0) do Begin
    Dec(l);
    b:=pb^;Inc(pb);
    Result:=Result+(b shl shift);
    Inc(shift,8);
    end;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function XSQLDA_LENGTH(Const n:FXLong):FXLong;
begin
  result := SizeOf(TXSQLDA) + ((n - 1) * SizeOf(TXSQLVAR));
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure add_spb_length(var p: PAnsiChar;Const length: integer);
begin
  p^ := AnsiChar(length);
  Inc (p);
  p^ := AnsiChar(length shr 8);
  Inc (p);
end;
{______________________________________________________________________________}
procedure add_spb_numeric(var p: PAnsiChar;Const  data: integer);
begin
  p^ := AnsiChar(data);
  Inc (p);
  p^ := AnsiChar(data shr 8);
  Inc (p);
  p^ := AnsiChar(data shr 16);
  Inc (p);
  p^ := AnsiChar(data shr 24);
  Inc (p);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure DecodeSQLDate(Const v:PISC_DATE;out DateTime:TDateTime);
begin
  DateTime:=v^-FX_DateOffset;
end;
{______________________________________________________________________________}
procedure DecodeSQLDate(Const v:PISC_DATE;out TimeStamp:TTimeStamp);
begin
  TimeStamp.Date:=v^-FX_DateOffset+DateDelta;
  TimeStamp.Time:=0;
end;
{______________________________________________________________________________}
function EncodeDateTime2SQLDate(Const DateTime:TDateTime):ISC_DATE;
Begin
  Result:=Trunc(DateTime)+FX_DateOffset;
End;
{______________________________________________________________________________}
function EncodeSQLDate2TimeStamp(Const TimeStamp:TTimeStamp):ISC_DATE;inline;
Begin
  Result:=TimeStamp.Date+FX_DateOffset-DateDelta;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure DecodeSQLTime(Const v:PISC_TIME;out DateTime:TDateTime);
begin
  DateTime:=v^ / 10 / FX_TimeCoeff
end;
{______________________________________________________________________________}
procedure DecodeSQLTime(Const v:PISC_TIME;out TimeStamp:TTimeStamp);
begin
  TimeStamp.Date:=0;
  TimeStamp.Time:=v^ div 10;
end;
{______________________________________________________________________________}
function EncodeDateTime2SQLTime(Const DateTime:TDateTime):ISC_TIME;inline;
Begin
  Result:=Round(Frac(DateTime) * 10 * FX_TimeCoeff);
End;
{______________________________________________________________________________}
function EncodeTimeStamp2SQLTime(Const TimeStamp:TTimeStamp):ISC_TIME;inline;
Begin
  Result:=TimeStamp.Time * 10
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure DecodeSQLTimeStamp(Const v:PISC_TIMESTAMP;out DateTime:TDateTime);
begin
  DateTime:=(v^.timestamp_date-FX_DateOffset)+(v^.timestamp_time / 10 / FX_TimeCoeff)
end;
{______________________________________________________________________________}
procedure DecodeSQLTimeStamp(Const v:PISC_TIMESTAMP;out TimeStamp: TTimeStamp);
begin
  TimeStamp.Date:=v^.timestamp_date-FX_DateOffset+DateDelta;
  TimeStamp.Time:=v^.timestamp_time div 10;
end;
{______________________________________________________________________________}
function EncodeDateTime2SQLTimeStamp(Const DateTime:TDateTime):ISC_TIMESTAMP;
Begin
  Result.timestamp_date:=Trunc(DateTime)+FX_DateOffset;
  Result.timestamp_time:=Round(Frac(DateTime) * 10 * FX_TimeCoeff);
End;
{______________________________________________________________________________}
function EncodeTimeStamp2SQLTimeStamp(Const TimeStamp:TTimeStamp):ISC_TIMESTAMP;inline;
Begin
  Result.timestamp_date:=TimeStamp.Date+FX_DateOffset-DateDelta;
  Result.timestamp_time:=TimeStamp.Time*10;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TXSQLVAR.MaxChars: Integer;
Begin
  // $FF supprime id collate
  case sqlsubtype and $FF of
    0, 1, 2, 10, 11, 12, 13, 14, 19, 21, 22, 39, 45, 46, 47, 50, 51, 52, 53, 54, 55, 58 :
      Result := sqllen;
    5, 6, 8, 44, 56, 57, 64 :
      Result := sqllen div 2;
    3 :
      Result := sqllen div 3;
    4, 59 :
      Result := sqllen div 4;
    else
      Result := 0;
    end;
end;
{______________________________________________________________________________}
function TXSQLVAR.CharsetSize : Integer;
Begin
  // $FF supprime id collate
  case sqlsubtype and $FF of
    0, 1, 2, 10, 11, 12, 13, 14, 19, 21, 22, 39, 45, 46, 47, 50, 51, 52, 53, 54, 55, 58 :
      Result := 1;
    5, 6, 8, 44, 56, 57, 64 :
      Result := 2;
    3 :
      Result := 3;
    4, 59 :
      Result := 4;
    else
      Result := 0;
 end;

End;

end.

