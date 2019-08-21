unit View.SimpleForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Layouts;

type
  TFormSimple = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    procedure TabItem1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure TabItem1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSimple: TFormSimple;

implementation

{$R *.fmx}

procedure TFormSimple.TabItem1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
//
end;

procedure TFormSimple.TabItem1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
//
end;

end.
