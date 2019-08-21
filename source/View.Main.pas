unit View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ExtCtrls,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Messaging, FMX.Layouts,
  FMX.Objects, FMX.ScrollBox, FMX.Memo,


  View.DockableForm, View.SimpleForm, FMX.DockFramework.DockManger
  ;
type
  TViewMain = class(TForm)
    ButtonNewDockableForm: TButton;
    ButtonNewSimpleForm: TButton;
    Layout1: TLayout;
    DockManger1: TDockManger;
    Button1: TButton;
    procedure ButtonNewDockableFormClick(Sender: TObject);
    procedure ButtonNewSimpleFormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    I: integer;
  end;

var
  ViewMain: TViewMain;

implementation

uses
  View.DockableFormDefault;

{$R *.fmx}

procedure TViewMain.Button1Click(Sender: TObject);
var
  Dockable: TFormDockDefault;
begin
  inc(I);
  Application.CreateForm(TFormDockDefault, Dockable);
  Dockable.Caption := 'Docktable Default'+I.ToString;
  Dockable.Show;
end;

procedure TViewMain.ButtonNewDockableFormClick(Sender: TObject);
var
  Dockable: TFormDock;
begin
  inc(I);
  Application.CreateForm(TFormDock, Dockable);
  Dockable.Caption := 'Docktable '+I.ToString;
  Dockable.Show;
end;

procedure TViewMain.ButtonNewSimpleFormClick(Sender: TObject);
var
  Simple: TFormSimple;
begin
  Inc(I);
  Application.CreateForm(TFormSimple, Simple);
  Simple.Caption := 'Simple'+I.ToString;
  Simple.Show;
end;

procedure TViewMain.FormCreate(Sender: TObject);
begin
  I := 0;
end;

end.
