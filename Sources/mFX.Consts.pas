unit mFX.Consts;

interface

{$I mFX.Inc}

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

const
  mFX_Dialect     = 3;
  ibTrue          ='T';
  ibFalse         ='F';

ResourceString
  /// generic strings used in code
  SDatabaseFilter     = 'Database Files (*.gdb)|*.gdb|All files (*.*)|*.*';
  SDisconnectDatabase = 'Database is currently connected. Disconnect and continue?';
  SCommitTransaction  = 'Transaction is currently Active. Rollback and continue?';
  SExecute            = 'E&xecute';
  SNoDataSet          = 'No dataset association';
  SSQLGenSelect       = 'Must select at least one key field and one update field';
  SSQLNotGenerated    = 'Update SQL statements not generated, exit anyway?';
  SIBUpdateSQLEditor  = '&UpdateSQL Editor...';
  SIBDataSetEditor    = '&Dataset Editor...';
  SSQLDataSetOpen     = 'Unable to determine field names for %s';
  SDefaultTransaction = '%s, Default';

  /// generic strings used in editor code
  SFXSuccessConnect   = 'Successful Connection';

  /// strings used in error messages
  SUnknownException          = 'Unknown Error'+sLinebreak+'%s';
  SAliasNotLocal             = 'Alias %s not local';
  SFirebirdMissing           = 'Firebird library not loaded';
  SIB60feature               = '%s is an Firebird 6 function. Please upgrade to Firebird 6 to use this functonality';
  SNotSupported              = 'Unsupported feature';
  SNotPermitted              = 'Not permitted';

  SDPBConstantUnknown        = 'DPB Constant (%s) is unknown';
  SDPBConstantInvalidValue   = 'DPB Constant (%s) Invalid Value <%s>';

  STPBConstantUnknown        = 'TPB Constant (%s) is unknown';
  STPBConstantInvalidValue   = 'TPB Constant (%s) Invalid Value <%s>';

  SDatabaseClosed            = 'Cannot perform operation -- DB is not open';
  SDatabaseOpen              = 'Cannot perform operation -- DB is currently open';
  SDatabaseNotAssigned       = 'Cannot perform operation -- DB not assigned !';
  SDatabaseNameMissing       = 'Empty DB Alias';
  STransactionNotAssigned    = 'Cannot perform operation -- TR not assigned !';
  SNotInTransaction          = 'Transaction is not active';
  SInTransaction             = 'Transaction is active';
  SDatabaseNotInTransaction  = 'DB not listed in transaction DBs';
  SNoDatabasesInTransaction  = 'No databases are listed in transaction component';
  SXSQLDAIndexOutOfRange     = 'XSQLDA index out of range';
  SInvalidStatementHandle    = 'Invalid statement handle';
  SSQLOpen                   = 'SQL Open';
  SSQLClosed                 = 'SQL Closed';
  SDatasetOpen               = 'Dataset open';
  SUnsupportedQueryType      = 'Unsupported Query Type (%d)';
  SUnknownSQLDataType        = 'Unknown SQL Data type (%d)';
  SInvalidDataConversion     = 'Invalid data conversion';
  SBlobCannotBeRead          = 'Blob stream cannot be read';
  SBlobCannotBeWritten       = 'Blob stream cannot be written';
  SBlobNotInitialized        = 'Blob Not Initialized';
  SEmptyQuery                = 'Empty query';
  SFieldNotFound             = 'Field "%s" not found';
  SFieldSizeMismatch         = 'Size Mismatch - Field %s size is too small for data';
  SStringTooLarge            = 'Trying to store a string of length %d into a field that can only contain %d';
  
  SSQLParseError             = 'SQL Parse Error:'+sLinebreak+'%s';
  SUserAbort                 = 'User abort';
  SCantEndSharedTransaction  = 'Can''t end a shared transaction '+sLinebreak+
                               'unless it is forced and equal to the transaction''s TimeoutAction';
  SEmptySQLStatement         = 'Empty SQL Statement';
  SIsASelectStatement        = 'use Open for a Select Statement';
  SNoTableName               = 'No Table Name assigned';

  SInvalidEvents             = 'Some Event are Empty or too long';
  SNoEventsRegistered        = 'No Events Registered';
  SEventAlreadyRegistered    = 'Events already registered';

  SSQLDialectInvalid         = 'SQL Dialect Invalid';
  SSPBConstantNotSupported   = 'SPB Constant <%s> Not supported';
  SSPBConstantUnknown        = 'SPB Constant <%s> Unknown';
  SServiceActive             = 'Cannot perform operation -- service is not attached';
  SServiceInActive           = 'Cannot perform operation -- service is attached';
  SQueryParamsError          = 'Query Parameters missing or incorrect';
  SStartParamsError          = 'start Parameters missing or incorrect';
  SOutputParsingError        = 'Unexpected Output buffer value';
  SUseSpecificProcedures     = 'Generic ServiceStart not applicable: Use Specific Procedures to set configuration params';

  SEOFInComment              = 'EOF in comment detected';
  SEOFInString               = 'EOF in string detected';
  SParamNameExpected         = 'Parameter name expected';

  SUnknownPlan               = 'Unknown Error - Can''t retrieve plan';

  
implementation

end.


