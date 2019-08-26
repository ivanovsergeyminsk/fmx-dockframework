unit View.DockableFormDefault;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Layouts, System.Rtti, FMX.Grid.Style,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.DB, Data.Bind.Components, Data.Bind.Grid,
  Data.Bind.DBScope, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid, FireDAC.Stan.StorageBin,
  FMX.Objects, FMX.DockFramework.DockProvider;

type
  TFormDockDefault = class(TForm)
    Layout2: TLayout;
    Grid1: TGrid;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    FDMemTable1: TFDMemTable;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    FDMemTable1Test1: TLargeintField;
    FDMemTable1test2: TWideStringField;
    FDMemTable1test3: TFloatField;
    FDMemTable1test4: TBooleanField;
    Rectangle1: TRectangle;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    DockProvider1: TDockProvider;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDockDefault: TFormDockDefault;

implementation

{$R *.fmx}

end.
