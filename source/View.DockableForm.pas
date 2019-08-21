unit View.DockableForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.DockFramework.DockForm;

type
  TFormDock = class(TForm)
    DockForm1: TDockForm;

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDock: TFormDock;

implementation

{$R *.fmx}

end.
