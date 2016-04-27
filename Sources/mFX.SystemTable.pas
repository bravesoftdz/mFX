unit mFX.SystemTable;

interface

{$I mFX.Inc}

Uses mFX.Consts;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Const
  _QRY_Charset_ =
    'SELECT RDB$CHARACTER_SET_ID, RDB$CHARACTER_SET_NAME, RDB$BYTES_PER_CHARACTER'+sLinebreak+
    'FROM RDB$CHARACTER_SETS'+sLinebreak+
    'Where RDB$CHARACTER_SET_ID=:p0';
    _QRY_Charset_CharSet              =  0;
    _QRY_Charset_Name                 =  1;
    _QRY_Charset_BytesPerChar         =  2;


  _QRY_ArrayDim_ =
    'SELECT RDB$LOWER_BOUND, RDB$UPPER_BOUND'+sLinebreak+
    'FROM RDB$FIELD_DIMENSIONS DIM'+sLinebreak+
    'WHERE (DIM.RDB$FIELD_NAME=:%s)'+sLinebreak+
    'ORDER BY DIM.RDB$DIMENSION';
    _QRY_ArrayDim_LOWER_BOUND         =  0;
    _QRY_ArrayDim_UPPER_BOUND         =  1;

  _QRY_Domains_ =
    'SELECT FLD.RDB$FIELD_TYPE,FLD.RDB$FIELD_SUB_TYPE,FLD.RDB$FIELD_SCALE,FLD.RDB$FIELD_PRECISION,'+sLinebreak+
    '       FLD.RDB$FIELD_LENGTH,FLD.RDB$CHARACTER_SET_ID,FLD.RDB$CHARACTER_LENGTH,'+sLinebreak+
    '       FLD.RDB$FIELD_NAME,FLD.RDB$DESCRIPTION,'+sLinebreak+
    '       FLD.RDB$SEGMENT_LENGTH, FLD.RDB$NULL_FLAG, FLD.RDB$DEFAULT_SOURCE,'+sLinebreak+
    '       FLD.RDB$VALIDATION_SOURCE, FLD.RDB$DIMENSIONS'+sLinebreak+
    'FROM RDB$FIELDS FLD'+sLinebreak+
    'WHERE NOT (FLD.RDB$FIELD_NAME STARTING WITH ''RDB$'')'+sLinebreak+
    'ORDER BY FLD.RDB$FIELD_NAME';
    _QRY_Domain_FieldType             =  0;
    _QRY_Domain_FieldSubType          =  1;
    _QRY_Domain_FieldScale            =  2;
    _QRY_Domain_FieldPrecision        =  3;
    _QRY_Domain_FieldLen              =  4;
    _QRY_Domain_FieldCharSet          =  5;
    _QRY_Domain_FieldCharLen          =  6;
    _QRY_Domain_Name                  =  7;
    _QRY_Domain_DESCR                 =  8;
    _QRY_Domain_SegLen                =  9;
    _QRY_Domain_NullFlag              = 10;
    _QRY_Domain_DefaultSource         = 11;
    _QRY_Domain_ValidationSource      = 12;
    _QRY_Domain_Dimensions            = 13;

  _QRY_Tables_ =
    'SELECT REL.RDB$RELATION_NAME, REL.RDB$DESCRIPTION'+sLinebreak+
    'FROM RDB$RELATIONS REL'+sLinebreak+
    'WHERE (REL.RDB$VIEW_BLR IS NULL)'+sLinebreak;
    _QRY_Table_Name_                 =  0;
    _QRY_Table_Descr_                =  1;

  _QRY_Views_ =
    'SELECT REL.RDB$RELATION_NAME, REL.RDB$DESCRIPTION,REL.RDB$VIEW_BLR'+sLinebreak+
    'FROM RDB$RELATIONS REL'+sLinebreak+
    'WHERE (REL.RDB$VIEW_BLR IS NOT NULL)'+sLinebreak;
    _QRY_View_QRY_Table_Name_        =  0;
    _QRY_View_Descr_                 =  1;
    _QRY_View_View_                  =  2;

  _QRY_Table_Fields_ =
    'SELECT RFR.RDB$RELATION_NAME'+sLinebreak+
    '      ,FLD.RDB$FIELD_TYPE,FLD.RDB$FIELD_SUB_TYPE,FLD.RDB$FIELD_SCALE,FLD.RDB$FIELD_PRECISION'+sLinebreak+
    '      ,FLD.RDB$FIELD_LENGTH,FLD.RDB$CHARACTER_SET_ID,FLD.RDB$CHARACTER_LENGTH'+sLinebreak+
    '      ,RFR.RDB$NULL_FLAG, FLD.RDB$VALIDATION_SOURCE, FLD.RDB$DIMENSIONS'+sLinebreak+
    '      ,RFR.RDB$FIELD_NAME, RFR.RDB$DESCRIPTION'+sLinebreak+
    '      ,FLD.RDB$SEGMENT_LENGTH, RFR.RDB$DEFAULT_SOURCE'+sLinebreak+
    '      ,RFR.RDB$FIELD_SOURCE, FLD.RDB$COMPUTED_SOURCE'+sLinebreak+
    'FROM RDB$RELATION_FIELDS RFR'+sLinebreak+
    'JOIN RDB$FIELDS FLD ON (RFR.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME) '+sLinebreak;
    _QRY_Table_Field_Table_Name       =  0;
    _QRY_Table_Field_FieldType        =  1;
    _QRY_Table_Field_FieldSubType     =  2;
    _QRY_Table_Field_FieldScale       =  3;
    _QRY_Table_Field_FieldPrecision   =  4;
    _QRY_Table_Field_FieldLen         =  5;
    _QRY_Table_Field_FieldCharSet     =  6;
    _QRY_Table_Field_FieldCharLen     =  7;
    _QRY_Table_Field_NullFlag         =  8;
    _QRY_Table_Field_ValidationSource =  9;
    _QRY_Table_Field_Dimensions       = 10;
    _QRY_Table_Field_Name             = 11;
    _QRY_Table_Field_Descr            = 12;
    _QRY_Table_Field_SegLen           = 13;
    _QRY_Table_Field_DefaultSource    = 14;
    _QRY_Table_Field_Domain           = 15;
    _QRY_Table_Field_ComputedSource   = 16;

  _QRY_ForeignKeys_ =
    'SELECT A.RDB$CONSTRAINT_NAME as Name,A.rdb$index_name as IndexName,A.RDB$RELATION_NAME as RelationName'+sLinebreak+
    '      ,C.RDB$RELATION_NAME as FKRelationName'+sLinebreak+
    '      ,B.RDB$UPDATE_RULE as UpdateRule, B.RDB$DELETE_RULE as DeleteRule'+sLinebreak+
    'FROM RDB$REF_CONSTRAINTS B'+sLinebreak+
    'left join RDB$RELATION_CONSTRAINTS A on (A.RDB$CONSTRAINT_NAME = B.RDB$CONSTRAINT_NAME)'+sLinebreak+
    'left join RDB$RELATION_CONSTRAINTS C on (B.RDB$CONST_NAME_UQ = C.RDB$CONSTRAINT_NAME)'+sLinebreak+
    'WHERE (A.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'')'+sLinebreak;
    _QRY_ForeignKey_Name              =  0;
    _QRY_ForeignKey_IndexName         =  1;
    _QRY_ForeignKey_RelationName      =  2;
    _QRY_ForeignKey_FKRelationName    =  3;
    _QRY_ForeignKey_UpdateRule        =  4;
    _QRY_ForeignKey_DeleteRule        =  5;

  _QRY_PrimaryKeys_ =
    'SELECT a.RDB$RELATION_NAME, a.RDB$CONSTRAINT_NAME, a.RDB$CONSTRAINT_TYPE, a.rdb$index_name, i.RDB$DESCRIPTION'+sLinebreak+
    'FROM RDB$RELATION_CONSTRAINTS a'+sLinebreak+
    'left join RDB$INDICES i on (a.rdb$index_name=i.rdb$index_name)'+sLinebreak+
    'WHERE (a.RDB$CONSTRAINT_TYPE in (''PRIMARY KEY'',''UNIQUE''))'+sLinebreak;
    _QRY_PrimaryKey_RelationName_     =  0;
    _QRY_PrimaryKey_ConstraintName_   =  1;
    _QRY_PrimaryKey_ConstraintType_   =  2;
    _QRY_PrimaryKey_IndexName_        =  3;
    _QRY_PrimaryKey_Description_      =  4;

  _QRY_Indexes_ =
    'SELECT IDX.RDB$INDEX_NAME, IDX.RDB$RELATION_NAME,IDX.RDB$INDEX_TYPE,IDX.RDB$UNIQUE_FLAG,IDX.RDB$INDEX_INACTIVE'+sLinebreak+
    'FROM RDB$INDICES IDX'+sLinebreak+
    'LEFT JOIN RDB$RELATIONS RELC ON (IDX.RDB$RELATION_NAME = RELC.RDB$RELATION_NAME)'+sLinebreak+
    'WHERE ((IDX.RDB$SYSTEM_FLAG IS NULL)or(IDX.RDB$SYSTEM_FLAG=0))'+sLinebreak+
    '  AND (NOT EXISTS ('+sLinebreak+
    '                   SELECT RC.RDB$INDEX_NAME FROM RDB$RELATION_CONSTRAINTS RC'+sLinebreak+
    '                   WHERE RC.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME'+sLinebreak+
    '      )           )';
    _QRY_Index_Name                   =  0;
    _QRY_Index_RelationName           =  1;
    _QRY_Index_TypeFlag               =  2;
    _QRY_Index_UniqueFlag             =  3;
    _QRY_Index_ActiveFlag             =  4;

  _QRY_Index_Fields_ =
    'Select ISG.RDB$FIELD_POSITION,ISG.RDB$FIELD_NAME'+sLinebreak+
    'FROM RDB$INDEX_SEGMENTS ISG'+sLinebreak;
    _QRY_Index_Field_Position         =  0;
    _QRY_Index_Field_Name             =  1;

  ///	<summary>
  ///	  Get Check Constraints
  ///	</summary>
  ///	<remarks>
  ///	  Each Constraint seem to be create as two trigger
  ///	  (Insert&amp;Update)Select distinct does not work as we have blobs , so
  ///	  use group by
  ///  May be we should use select RDB$CONSTRAINT_NAME,RDB$RELATION_NAME from RDB$RELATION_CONSTRAINTS where (RDB$CONSTRAINT_TYPE=''CHECK'')
  ///	</remarks>
  _QRY_CheckConstraints_ =
    'SELECT CHK.RDB$CONSTRAINT_NAME, TRG.RDB$RELATION_NAME, TRG.RDB$TRIGGER_SOURCE, TRG.RDB$DESCRIPTION, Count(*)'+sLinebreak+
    'FROM RDB$TRIGGERS TRG'+sLinebreak+
    'JOIN RDB$CHECK_CONSTRAINTS CHK ON TRG.RDB$TRIGGER_NAME = CHK.RDB$TRIGGER_NAME'+sLinebreak+
    'Where TRG.RDB$SYSTEM_FLAG=3'+sLinebreak+
    'Group By 1,2,3,4'+sLinebreak;
    _QRY_CheckConstraint_Name         =  0;
    _QRY_CheckConstraint_Relation     =  1;
    _QRY_CheckConstraint_Source       =  2;
    _QRY_CheckConstraint_Descr        =  3;

  _QRY_Triggers_ =
    'SELECT TRI.RDB$TRIGGER_NAME, TRI.RDB$DESCRIPTION, TRI.RDB$RELATION_NAME, TRI.RDB$TRIGGER_SEQUENCE,'+sLinebreak+
    '       TRI.RDB$TRIGGER_TYPE, TRI.RDB$TRIGGER_INACTIVE'+sLinebreak+
    'FROM RDB$TRIGGERS TRI'+sLinebreak+
    'left join RDB$CHECK_CONSTRAINTS C ON C.RDB$TRIGGER_NAME = TRI.RDB$TRIGGER_NAME'+sLinebreak+
    'WHERE ((TRI.RDB$SYSTEM_FLAG IS NULL)or(TRI.RDB$SYSTEM_FLAG=0))'+sLinebreak+
    '  AND (C.RDB$TRIGGER_NAME IS NULL)';
    _QRY_Trigger_Name                 =  0;
    _QRY_Trigger_Descr                =  1;
    _QRY_Trigger_RelationName         =  2;
    _QRY_Trigger_Seq                  =  3;
    _QRY_Trigger_Type                 =  4;
    _QRY_Trigger_Inactive             =  5;
  _QRY_Trigger_Source_ =
    'SELECT TRI.RDB$TRIGGER_SOURCE'+sLinebreak+
    'FROM RDB$TRIGGERS TRI'+sLinebreak+
    'WHERE (TRI.RDB$TRIGGER_NAME=:p0)';

  _QRY_Procs_ =
    'SELECT PROCS.RDB$PROCEDURE_NAME, PROCS.RDB$DESCRIPTION'+sLinebreak+
    'FROM RDB$PROCEDURES PROCS'+sLinebreak+
    'ORDER BY PROCS.RDB$PROCEDURE_NAME';
    _QRY_Proc_Name                    =  0;
    _QRY_Proc_Descr                   =  1;
  _QRY_Proc_Source_ =
    'SELECT PROCS.RDB$PROCEDURE_SOURCE'+sLinebreak+
    'FROM RDB$PROCEDURES PROCS'+sLinebreak+
    'Where (PROCS.RDB$PROCEDURE_NAME=:p0)';
  _QRY_Proc_Fields_ =
    'SELECT FLD.RDB$FIELD_TYPE,FLD.RDB$FIELD_SUB_TYPE,FLD.RDB$FIELD_SCALE,FLD.RDB$FIELD_PRECISION,'+sLinebreak+
    '       FLD.RDB$FIELD_LENGTH,FLD.RDB$CHARACTER_SET_ID,FLD.RDB$CHARACTER_LENGTH,'+sLinebreak+
    '       FLD.RDB$SEGMENT_LENGTH,'+sLinebreak+
    '       PRM.RDB$PROCEDURE_NAME, PRM.RDB$PARAMETER_NAME,PRM.RDB$PARAMETER_TYPE,PRM.RDB$PARAMETER_NUMBER'+sLinebreak+
    'FROM RDB$PROCEDURE_PARAMETERS PRM'+sLinebreak+
    'JOIN RDB$FIELDS FLD ON (PRM.RDB$FIELD_SOURCE=FLD.RDB$FIELD_NAME)'+sLinebreak+
    'WHERE (PRM.RDB$PROCEDURE_NAME=:p0)'+sLinebreak+
    'ORDER BY PRM.RDB$PARAMETER_TYPE,PRM.RDB$PARAMETER_NUMBER'+sLinebreak;
    _QRY_Proc_Field_Type              =  0;
    _QRY_Proc_Field_SubType           =  1;
    _QRY_Proc_Field_Scale             =  2;
    _QRY_Proc_Field_Precision         =  3;
    _QRY_Proc_Field_Len               =  4;
    _QRY_Proc_Field_CharSet           =  5;
    _QRY_Proc_Field_CharLen           =  6;
    _QRY_Proc_Field_SegLen            =  7;
    _QRY_Proc_Field_ProcName          =  8;
    _QRY_Proc_Field_ParamName         =  9;
    _QRY_Proc_Field_ParamType         = 10;
    _QRY_Proc_Field_ParamNumber       = 11;

  _QRY_UDFs_ =
    'SELECT UDF.RDB$FUNCTION_NAME, UDF.RDB$MODULE_NAME, UDF.RDB$ENTRYPOINT, UDF.RDB$RETURN_ARGUMENT, UDF.RDB$DESCRIPTION'+sLinebreak+
    'FROM RDB$FUNCTIONS UDF'+sLinebreak+
    'WHERE ((UDF.RDB$SYSTEM_FLAG IS NULL)or(UDF.RDB$SYSTEM_FLAG=0))'+sLinebreak+
    'ORDER BY UDF.RDB$FUNCTION_NAME';
    _QRY_UDF_Name                     =  0;
    _QRY_UDF_ModuleName               =  1;
    _QRY_UDF_EntryPoint               =  2;
    _QRY_UDF_ReturnArg                =  3;
    _QRY_UDF_DESCRIPTION              =  4;
  _QRY_UDF_Fields_ =
    'SELECT FLD.RDB$FIELD_TYPE,FLD.RDB$FIELD_SUB_TYPE,FLD.RDB$FIELD_SCALE,FLD.RDB$FIELD_PRECISION,'+sLinebreak+
    '       FLD.RDB$FIELD_LENGTH,FLD.RDB$CHARACTER_SET_ID,FLD.RDB$CHARACTER_LENGTH,'+sLinebreak+
    '       FLD.RDB$ARGUMENT_POSITION,FLD.RDB$MECHANISM'+sLinebreak+
    'FROM RDB$FUNCTION_ARGUMENTS FLD'+sLinebreak+
    'WHERE RDB$FUNCTION_NAME=:p0'+sLinebreak+
    'ORDER BY RDB$ARGUMENT_POSITION';
    _QRY_UDF_Field_Type               =  0;
    _QRY_UDF_Field_SubType            =  1;
    _QRY_UDF_Field_Scale              =  2;
    _QRY_UDF_Field_Precision          =  3;
    _QRY_UDF_Field_Len                =  4;
    _QRY_UDF_Field_CharSet            =  5;
    _QRY_UDF_Field_CharLen            =  6;
    _QRY_UDF_Field_Position           =  7;
    _QRY_UDF_Field_Mechanism          =  8;

  _QRY_Exceptions_ =
    'SELECT exc.RDB$EXCEPTION_NAME, exc.RDB$DESCRIPTION, exc.RDB$MESSAGE, exc.RDB$EXCEPTION_NUMBER'+sLinebreak+
    'FROM RDB$EXCEPTIONS exc'+sLinebreak+
    'ORDER BY exc.RDB$EXCEPTION_NAME';
    _QRY_Exception_Name               =  0;
    _QRY_Exception_Descr              =  1;
    _QRY_Exception_Message            =  2;
    _QRY_Exception_Number             =  3;

  _QRY_Generators_ =
    'SELECT gen.RDB$GENERATOR_NAME'+sLinebreak+
    'FROM RDB$GENERATORS gen'+sLinebreak+
    'WHERE (gen.RDB$SYSTEM_FLAG IS NULL OR gen.RDB$SYSTEM_FLAG <> 1)'+sLinebreak;
    _QRY_Generators_Name              =  0;


  _QRY_Roles_ =
    'Select ROL.RDB$ROLE_NAME, ROL.RDB$OWNER_NAME'+sLinebreak+
    'FROM RDB$ROLES ROL'+sLinebreak;
    _QRY_Role_ROLE_NAME               =  0;
    _QRY_Role_OWNER_NAME              =  1;

  _QRY_Dependencies_ =
    'select distinct'+sLinebreak+
    '       d.RDB$DEPENDENT_NAME,d.RDB$DEPENDENT_TYPE'+sLinebreak+
    '       ,case when d.RDB$DEPENDENT_TYPE in (3) then (Select f.RDB$RELATION_NAME from RDB$RELATION_FIELDS f where (f.RDB$FIELD_SOURCE=d.RDB$DEPENDENT_NAME)) else null end as Relation'+sLinebreak+
    '       ,case when d.RDB$DEPENDENT_TYPE in (3) then (Select f.RDB$FIELD_NAME    from RDB$RELATION_FIELDS f where (f.RDB$FIELD_SOURCE=d.RDB$DEPENDENT_NAME)) else null end as Field'+sLinebreak+
    '      ,d.RDB$DEPENDED_ON_NAME,d.RDB$DEPENDED_ON_TYPE'+sLinebreak+
    'from RDB$DEPENDENCIES d'+sLinebreak+
    'join RDB$TYPES ot on (d.RDB$DEPENDED_ON_TYPE=ot.RDB$TYPE)'+sLinebreak;
    _QRY_Dependency_DependentObj      =  0;
    _QRY_Dependency_DependentType     =  1;
    _QRY_Dependency_DependentRelation =  2;
    _QRY_Dependency_DependentField    =  3;
    _QRY_Dependency_DependedOnObj     =  4;
    _QRY_Dependency_DependedOnType    =  5;

implementation

end.

