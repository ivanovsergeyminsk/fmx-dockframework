unit View.DockableFormDefault;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Layouts, FMX.DockFramework.DockForm;

type
  TFormDockDefault = class(TForm)
    Layout2: TLayout;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    DockForm1: TDockForm;
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
