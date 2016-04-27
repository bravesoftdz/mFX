unit mFX.Import;

interface

{$I mFX.Inc}

Uses Classes, Sysutils, Types, Variants, DB, DBWeb, HTTPProd,
  mFX.SQL, mFX.Export;

Type
  TFXImportAssignParam = procedure(Const Params:TFXSQLVAR;Var Value:Variant) of Object;

  TFXImport = Class(TFXCustomSQL)
  private
    fFmt            : TFXExportFmt;
    fDoDelete       : Boolean;
    fFileName       : String;
    fOnlyRelation   : String;
    fAssignParam    : TFXImportAssignParam;
    fInsertSQL      : TFXCustomSQL;
    function ImportXML:Boolean;
    function ImportXML2DataSet(Const TheDataSet:TDataSet):Boolean;
  public
    Constructor Create( AOwner: TComponent ); Override;
    destructor Destroy; override;
    function Import2DataSet(Const TheDataSet:TDataSet):Boolean;
    function Import:Boolean;
  published
    property Fmt         : TFXExportFmt         read fFmt          write fFmt         default xfXML;
    property FileName    : String               read fFileName     write fFileName;
    property OnlyRelation: String               read fOnlyRelation write fOnlyRelation;
    property DoDelete    : Boolean              read fDoDelete     write fDoDelete;
    property AssignParam : TFXImportAssignParam read fAssignParam  write fAssignParam;
  end;

implementation

uses mFX.Consts,
  JvSimpleXml;

{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
Constructor TFXImport.Create( AOwner: TComponent );
Begin
  inherited;
  fInsertSQL:=TFXCustomSQL.Create(nil);
  fFmt:=xfXML;
end;
{______________________________________________________________________________}
destructor TFXImport.Destroy;
Begin
  fInsertSQL.Free;
  inherited;
end;
{______________________________________________________________________________}
function TFXImport.Import:Boolean;
Begin
  Case fFmt of
    xfXML:Result:=ImportXML
    else Result:=False;
    end;
end;
{______________________________________________________________________________}
function TFXImport.Import2DataSet(Const TheDataSet:TDataSet):Boolean;
Begin
  Case fFmt of
    xfXML:Result:=ImportXML2DataSet(TheDataSet);
    else Result:=False;
    end;
end;
{______________________________________________________________________________}
{______________________________________________________________________________}
{______________________________________________________________________________}
function TFXImport.ImportXML:Boolean;
Var InsertFields,InsertValues,InsertQuery:String;
    UpdateValues,UpdateWhere,UpdateQuery:String;
    XMLEl,XMLField,XMLRecord:TJvSimpleXMLElem;
    XMLProp:TJvSimpleXMLProp;
    XML:TJvSimpleXML;
    p,pp:TFXSQLVAR;
    r,fn:String;
    i,j:Integer;
    v:Variant;
Begin
  Result:=False;
  XML:=TJvSimpleXML.Create(nil);
  try XML.FileName:=Self.fFileName;

      //Find Relation !!!!
      XMLProp:=XML.Root.Properties.ItemNamed['Name'];
      if (XMLProp=nil) Then
        Raise Exception.Create('Relation Name not found');
      r:=XMLProp.Value;
      if (r=EmptyStr) Then
        Raise Exception.Create('Empty Relation Name');
      if fOnlyRelation<>EmptyStr Then Begin
        if fOnlyRelation<>r Then Begin
          Raise Exception.CreateFmt('Invalid Source'+CRLF+'Relation %s <> %s',[r,fOnlyRelation]);
        end end;

      //Build Queries
      XMLEl:=XML.Root.Items.ItemNamed['Header'];
      if (XMLEl=nil) Then
        Raise Exception.Create('Header Node not found');
      XMLEl:=XMLEl.Items.ItemNamed['Table'];
      if (XMLEl=nil) Then
        Raise Exception.Create('Table Node not found');
      InsertFields:=EmptyStr;
      InsertValues:=EmptyStr;
      UpdateValues:=EmptyStr;
      UpdateWhere :=EmptyStr;
      for i:=0 To Pred(XMLEl.Items.Count) do begin
        XMLField:=XMLEl.Items[i];
        XMLProp:=XMLField.Properties.ItemNamed['Name'];
        if (XMLProp=nil) Then
          Raise Exception.Create('Field Name not found');
        fn:=XMLProp.Value;
        if (fn=EmptyStr) Then
          Raise Exception.Create('Empty Field Name');
        InsertFields:=InsertFields+','+fn;
        InsertValues:=InsertValues+',:'+fn;
        XMLProp:=XMLField.Properties.ItemNamed['Inkey'];
        if (XMLProp=nil) Then
          Raise Exception.Create('Field Key not found');
        if XMLProp.BoolValue Then
          UpdateWhere:=UpdateWhere+'and('+fn+'=:'+fn+')' else
          UpdateValues:=UpdateValues+','+fn+'=:'+fn;
        end;
      if InsertFields=EmptyStr Then
        Raise Exception.Create('Invalid Fields Definition');
      Delete(InsertFields,1,1);
      Delete(InsertValues,1,1);
      if (UpdateWhere<>EmptyStr)and(UpdateValues<>EmptyStr) Then Begin
        Delete(UpdateWhere,1,3);
        Delete(UpdateValues,1,1);
        UpdateQuery:='Update '+r+' Set '+UpdateValues+' Where '+UpdateWhere;
      end else
        Raise Exception.Create('Invalid Fields Definition (No key !)');
      InsertQuery:='Insert into '+r+' ('+InsertFields+')Values('+InsertValues+');';
      fInsertSQL.Database:=Self.Database;
      fInsertSQL.Transaction:=Self.Transaction;
      fInsertSQL.SQL.Text:=InsertQuery;
      Self.Start_TR;

      //Delete existing data ???
      if fDoDelete Then Begin
        Self.SQL.Text:='Delete from '+r;
        Self.ExecQuery;
        Self.Close;
        end;

      //Loop on data ..
      Self.SQL.Text:=UpdateQuery;
      Self.Prepare;
      XMLEl:=XML.Root.Items.ItemNamed['Records'];
      if (XMLEl=nil) Then
        Raise Exception.Create('Records Data not found');
      for i:=0 To Pred(XMLEl.Items.Count) do begin
        XMLRecord:=XMLEl.Items[i];
        Assert(XMLRecord.Name='Record');
        for j:=0 To Pred(Self.Params.Count) do begin
          p:=Self.Params[j];
          p.Clear;
          end;
        for j:=0 To Pred(XMLRecord.Items.Count) do begin
          XMLField:=XMLRecord.Items[j];
          Assert(XMLField.Name='RecordField');
          XMLProp:=XMLField.Properties.ItemNamed['Name'];
          if (XMLProp=nil) Then
            Raise Exception.Create('Field Name not found');
          fn:=XMLProp.Value;
          p:=Self.ParamByName(fn);
          if p=nil then
            Raise Exception.CreateFmt('Params %s not found',[fn]);
          v:=XMLField.Value;
          if Assigned(fAssignParam) then
            fAssignParam(p,v);
          if VarIsNull(v) then
            p.Clear else
            p.Value:=v;
          end;
        Self.ExecQuery;
        if Self.RowsAffected=0 Then Begin
          //Insert !!!!
          fInsertSQL.Prepare;
          for j:=0 To Pred(fInsertSQL.Params.Count) do begin
            p:=fInsertSQL.Params[j];
            pp:=Self.ParamByName(p.Name);
            if pp=nil Then
              Raise Exception.CreateFmt('Params %s not found',[p.Name]);
            p.Assign(pp);
            end;
          fInsertSQL.ExecQuery;
          Assert(fInsertSQL.RowsAffected=1);
          fInsertSQL.Close;
        end end;
      Self.Close;
  finally
      XML.Free;
  end
end;
{______________________________________________________________________________}
function TFXImport.ImportXML2DataSet(Const TheDataSet:TDataSet):Boolean;
Begin
  Result:=False;
end;

end.
