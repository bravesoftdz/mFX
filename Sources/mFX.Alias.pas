unit mFX.Alias;

interface

{$I mFX.Inc}

uses System.SysUtils, System.StrUtils,
  mFX.Header, mFX.Consts, mFX.Classes, mFX.Utils;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}
{$M-}

Type
  TFXProtocol = (
    fxTCP,
    fxLocal
    );

  TFXDBAlias = class(TObject)
  protected
    fAlias              : TFXDBName;
    fProtocol           : TFXProtocol;
    fServerName         : String;
    fTCPPort            : String;
    fFileName           : String;

    /// <summary>Set Protocol</summary>
    procedure SetProtocol(Const Value:TFXProtocol);

    /// <summary>Get ServerName</summary>
    function GetServerName:String;
    /// <summary>Set ServerName</summary>
    procedure SetServerName(Const Value:String);

    /// <summary>Check Alias assigned</summary>
    procedure CheckFileName;
    /// <summary>Get FullFileName</summary>
    function GetFullFileName:String;
    /// <summary>Get FileName</summary>
    function GetFileName:String;
    /// <summary>Get FileNameNoExt</summary>
    function GetFileNameNoExt:String;

    /// <summary>Set Alias</summary>
    procedure SetAlias(const Value: TFXDBName);

  public
    /// <summary>Combine DBName Serveur:FileName</summary>
    Class function BuildAlias(Const aProtocol:TFXProtocol;Const aServerName,aPort,aFileName:String):String;
    /// <summary>Parse Alias</summary>
    Class procedure ParseAlias(Const aAlias:TFXDBName;Out aProtocol:TFXProtocol;Out aServerName,aPort,aFileName:String);
    /// <summary>ExtractServerName</summary>
    Class function ExtractServerName(Const Value:TFXDBName):String;
    /// <summary>Return the DataBase FileName </summary>
    Class function ExtractFullFileName(Const Value:TFXDBName):String;
    /// <summary>Return the DataBase FileName </summary>
    Class function ExtractFileName(Const Value:TFXDBName):String;
    /// <summary>Return the DataBase FileName without ext</summary>
    Class function ExtractFileNameNoExt(Const Value:TFXDBName):String;

    /// <summary>RAZ Alise</summary>
    procedure RAZ;
    /// <summary>Assign </summary>
    procedure Assign(Const Source: TFXDBAlias);

    /// <summary>Compare FileName</summary>
    function IsSameServer(const Value: TFXDBAlias):Boolean;overload;
    /// <summary>Compare FileName</summary>
    function IsSameServer(const Value: String):Boolean;overload;

    /// <summary>SetDatabaseName</summary>
    procedure ChgFileName(const aFileName:String);
    /// <summary>Compare FileName</summary>
    function IsSameFileName(const Value: String):Boolean;overload;
    /// <summary>Compare ServerName </summary>
    function IsSameFileName(const Value: TFXDBAlias):Boolean;overload;

    /// <summary>Check Alias assigned</summary>
    procedure CheckAlias;
    /// <summary>SetDatabaseName</summary>
    procedure ChgAlias(const aServerName,aPort,aFileName:String);overload;
    /// <summary>SetDatabaseName</summary>
    procedure ChgAlias(Const aProtocol:TFXProtocol;const aServerName,aPort,aFileName:String);overload;

    property Alias             : TFXDBName     read fAlias              write SetAlias;
    property Protocol          : TFXProtocol   read fProtocol           write SetProtocol;
    property ServerName        : String        read GetServerName       write SetServerName;
    property FullFileName      : String        read GetFullFileName;
    property FileName          : String        read GetFileName;
    property FileNameNoExt     : String        read GetFileNameNoExt;
    property TCPPort           : String        read fTCPPort;

  end;

implementation

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXDBAlias.RAZ;
begin
  fProtocol     := fxTCP;
  fServerName   := EmptyStr;
  fTCPPort      := EmptyStr;
  fFileName     := EmptyStr;
  fAlias        := EmptyStr;
end;
{______________________________________________________________________________}
procedure TFXDBAlias.Assign(Const Source: TFXDBAlias);
Begin
  fProtocol     := Source.fProtocol;
  fServerName   := Source.fServerName;
  fTCPPort      := Source.fTCPPort;
  fFileName     := Source.fFileName;
  fAlias        := Source.fAlias;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
Class function TFXDBAlias.BuildAlias(Const aProtocol:TFXProtocol;Const aServerName,aPort,aFileName:String):String;
Var sn,fn:String;
Begin
  // Build Alias
  case aProtocol of
    fxLocal:Begin
      Result     := aFileName;
      end;
    else Begin
      if aFileName<>EmptyStr then Begin
        fn:=AnsiReplaceStr(aFileName,'\','/');
        // Adjust ServerName
        if aServerName=EmptyStr Then
          sn:='localhost' else
          sn:=aServerName;
        // Check TCP Port
        if (aPort<>'') then
          Result:=sn+'/'+aPort+':'+fn else
          Result:=sn+':'+fn
      end else
        Result:=EmptyStr
    end end;
end;
{______________________________________________________________________________}
Class procedure TFXDBAlias.ParseAlias(Const aAlias:TFXDBName;Out aProtocol:TFXProtocol;Out aServerName,aPort,aFileName:String);
Var i,j,l:Integer;
Begin
  aProtocol  :=fxLocal;
  aServerName:='';
  aPort      :='';
  aFileName  :=aAlias;

  //Scan till first ':'
  i:=1;
  l:=Length(aAlias);
  while (i<=l) do Begin
    Case aAlias[i] of
      ':':Begin
         //ServerName Separator !!!!  localhost:c:/Restomax/Data.gdb
         if i>2 then Begin
           aServerName:=Trim(Copy(aAlias,1,i-1));
           aFileName  :=Trim(Copy(aAlias,i+1,MaxInt));
           aProtocol  :=fxTCP;
           end;
         Break;
         End;
      '\','/':Begin
         //Can Be Port Separator !!!!
         j:=i;
         inc(i);
         while (i<=l) do Begin
           Case aAlias[i] of
             '0'..'9':Begin
               //OK Can be Port
               Inc(i);
               end;
             ':':Begin
               //localhost/3051:c:/Restomax/Data.gdb
               aServerName:=Trim(Copy(aAlias,1,j-1));
               aPort      :=Trim(Copy(aAlias,j+1,i-j-1));
               aFileName  :=Trim(Copy(aAlias,i+1,MaxInt));
               aProtocol  :=fxTCP;
               Break;
               end;
             else Begin
               Break;
           end end end;
         Break;
      end end;
    inc(i);
    end;

  //Adjust ServerName
  if aServerName='' then
    aServerName:='LOCALHOST';

End;
{______________________________________________________________________________}
Class function TFXDBAlias.ExtractServerName(Const Value:TFXDBName):String;
Var sn,fn,pn:String;
    p:TFXProtocol;
Begin
  ParseAlias(Value,p,sn,pn,fn);
  Result:=sn
end;
{______________________________________________________________________________}
Class function TFXDBAlias.ExtractFullFileName(Const Value:TFXDBName):String;
Var sn,fn,pn:String;
    p:TFXProtocol;
Begin
  ParseAlias(Value,p,sn,pn,fn);
  Result:=fn
end;
{______________________________________________________________________________}
Class function TFXDBAlias.ExtractFileName(Const Value:TFXDBName):String;
Var sn,fn,pn:String;
    p:TFXProtocol;
    I:Integer;
Begin
  ParseAlias(Value,p,sn,pn,fn);
  I := LastDelimiter('/' + '\' + DriveDelim, fn);
  Result := Copy(fn, I + 1, MaxInt);
end;
{______________________________________________________________________________}
Class function TFXDBAlias.ExtractFileNameNoExt(Const Value:TFXDBName):String;
Var fn:String;
Begin
  fn:=TFXDBAlias.ExtractFileName(Value);
  Result := ChangeFileExt(fn,EmptyStr);
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXDBAlias.SetProtocol(Const Value:TFXProtocol);
Begin
  if fProtocol<>Value Then Begin
    fProtocol:=Value;
    fAlias:=TFXDBAlias.BuildAlias(fProtocol,fServerName,fTCPPort,fFileName)
    end
end;
{______________________________________________________________________________}
function TFXDBAlias.GetServerName:String;
Begin
  if (fServerName=EmptyStr) then Begin
    Result:='localhost'
  End else
    Result:=fServerName
end;
{______________________________________________________________________________}
procedure TFXDBAlias.SetServerName(Const Value:String);
Begin
  if fServerName<>Value then Begin
    fServerName:=Value;
    fAlias:=TFXDBAlias.BuildAlias(fProtocol,fServerName,fTCPPort,fFileName)
    end;
end;
{______________________________________________________________________________}
function TFXDBAlias.IsSameServer(const Value: String):Boolean;
Begin
  case fProtocol of
    fxLocal:Begin
      Result:=(Value=EmptyStr)or(SameText(Value,'localhost'));
      end;
    else Begin
      Result:=SameText(fServerName,Value)
    end end;
End;
{______________________________________________________________________________}
function TFXDBAlias.IsSameServer(const Value: TFXDBAlias):Boolean;
Begin
  case fProtocol of
    fxLocal:Begin
      case Value.fProtocol of
        fxLocal:Begin
          Result:=True
          end;
        else Begin
          Result:=(Value.fServerName=EmptyStr)or(SameText(Value.fServerName,'localhost'));
      end end end;
    else Begin
      case Value.fProtocol of
        fxLocal:Begin
          Result:=(fServerName=EmptyStr)or(SameText(fServerName,'localhost'));
          end;
        else Begin
          Result:=(SameText(fServerName,Value.fServerName))
    end end end end;
End;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXDBAlias.CheckFileName;
begin
  if fFileName=EmptyStr then
    FXRaiseClientError(Self,fxceDatabaseNameMissing);
end;
{______________________________________________________________________________}
Function TFXDBAlias.GetFullFileName:String;
Begin
  CheckFileName;
  Result := fFileName
end;
{______________________________________________________________________________}
Function TFXDBAlias.GetFileName:String;
var I:Integer;
Begin
  CheckFileName;
  I := LastDelimiter('/' + '\' + DriveDelim, fFileName);
  Result := Copy(fFileName, I + 1, MaxInt);
end;
{______________________________________________________________________________}
function TFXDBAlias.GetFileNameNoExt:String;
Begin
  CheckFileName;
  Result:=ChangeFileExt(GetFileName,EmptyStr);
end;
{______________________________________________________________________________}
function TFXDBAlias.IsSameFileName(const Value: String):Boolean;
Var s1,s2:String;
Begin
  CheckFileName;
  s1:=AnsiReplaceStr(fFileName,'\','/');
  s2:=AnsiReplaceStr(Value,'\','/');
  Result:=SameText(s1,s2)
end;
{______________________________________________________________________________}
function TFXDBAlias.IsSameFileName(const Value: TFXDBAlias):Boolean;
Var s1,s2:String;
Begin
  Self.CheckFileName;
  Value.CheckFileName;
  s1:=AnsiReplaceStr(Self.fFileName,'\','/');
  s2:=AnsiReplaceStr(Value.fFileName,'\','/');
  Result:=SameText(s1,s2)
end;
{______________________________________________________________________________}
procedure TFXDBAlias.ChgFileName(Const aFileName:String);
Begin
  if fFileName<>aFileName then Begin
    fFileName:=aFileName;
    fAlias:=TFXDBAlias.BuildAlias(fProtocol,fServerName,fTCPPort,fFileName)
    end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
procedure TFXDBAlias.CheckAlias;
begin
  if fAlias=EmptyStr then
    FXRaiseClientError(Self,fxceDatabaseNameMissing);
end;
{______________________________________________________________________________}
procedure TFXDBAlias.SetAlias(const Value:TFXDBName);
Begin
  if Value<>EmptyStr then Begin
    Self.ParseAlias(Value,fProtocol,fServerName,fTCPPort,fFileName);
    fAlias:=TFXDBAlias.BuildAlias(fProtocol,fServerName,fTCPPort,fFileName)
  end else
    RAZ;
end;
{______________________________________________________________________________}
procedure TFXDBAlias.ChgAlias(const aServerName,aPort,aFileName:String);
Begin
  fProtocol     := fxTCP;
  fServerName   := aServerName;
  fTCPPort      := aPort;
  fFileName     := aFileName;
  fAlias:=TFXDBAlias.BuildAlias(fProtocol,fServerName,fTCPPort,fFileName)
end;
{______________________________________________________________________________}
procedure TFXDBAlias.ChgAlias(Const aProtocol:TFXProtocol;const aServerName,aPort,aFileName:String);
Begin
  fProtocol     := aProtocol;
  fServerName   := aServerName;
  fTCPPort      := aPort;
  fFileName     := aFileName;
  fAlias:=TFXDBAlias.BuildAlias(fProtocol,fServerName,fTCPPort,fFileName)
end;

end.

