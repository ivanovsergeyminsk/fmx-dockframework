unit View.DockableForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.Layouts, FMX.Objects,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.StorageBin, System.Rtti, FMX.Grid.Style, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.DockFramework.DockProvider,
  FMX.StdCtrls;

type
  TFormDock = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Rectangle1: TRectangle;
    FDMemTable1: TFDMemTable;
    FDMemTable1Test1: TLargeintField;
    FDMemTable1test2: TWideStringField;
    FDMemTable1test3: TFloatField;
    FDMemTable1test4: TBooleanField;
    Grid1: TGrid;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    DockProvider1: TDockProvider;
    Button1: TButton;
    Button2: TButton;
    TabItem4: TTabItem;
    TabItem5: TTabItem;
    TabItem6: TTabItem;
    TabItem7: TTabItem;
    TabItem8: TTabItem;
    TabItem9: TTabItem;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDock: TFormDock;

implementation

{$R *.fmx}


procedure TFormDock.Button1Click(Sender: TObject);
begin
  self.BorderStyle := TFmxFormBorderStyle.None;
end;

procedure TFormDock.Button2Click(Sender: TObject);
begin
  self.BorderStyle := TFmxFormBorderStyle.Sizeable;
end;

end.
