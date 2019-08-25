unit View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ExtCtrls,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Messaging, FMX.Layouts,
  FMX.Objects, FMX.ScrollBox, FMX.Memo,


  View.DockableForm, View.SimpleForm, FMX.DockFramework.DockManger,
  FMX.MultiView
  ;
type
  TViewMain = class(TForm)
    ButtonNewDockableForm: TButton;
    ButtonNewSimpleForm: TButton;
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    MultiView1: TMultiView;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    DockManger1: TDockManger;
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
  Dockable.Label1.Text :=   Dockable.Caption;
  Dockable.Show;
end;

procedure TViewMain.ButtonNewDockableFormClick(Sender: TObject);
var
  Form1: TFormDock;
begin
  inc(I);
  Application.CreateForm(TFormDock, Form1);
  Form1.Caption := 'Docktable '+I.ToString;
  Form1.Show;
end;

procedure TViewMain.ButtonNewSimpleFormClick(Sender: TObject);
var
  Form2: TFormSimple;
begin
  Inc(I);
  Application.CreateForm(TFormSimple, Form2);
  Form2.Caption := 'Simple'+I.ToString;
  Form2.Show;
end;

procedure TViewMain.FormCreate(Sender: TObject);
begin
  I := 0;
end;

end.
