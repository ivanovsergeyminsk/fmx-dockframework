unit View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ExtCtrls,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Messaging, FMX.Layouts,
  FMX.Objects, FMX.ScrollBox, FMX.Memo,


  View.DockableForm, View.SimpleForm,
  FMX.MultiView,   View.DockableFormDefault, FMX.DockFramework.DockManager
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
    DockManager1: TDockManager;
    procedure ButtonNewDockableFormClick(Sender: TObject);
    procedure ButtonNewSimpleFormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FormDockableDefault: TFormDockDefault;
    FormDockable: TFormDock;
  public
    { Public declarations }
    I: integer;
  end;

var
  ViewMain: TViewMain;

implementation

{$R *.fmx}

procedure TViewMain.Button1Click(Sender: TObject);
begin
  inc(I);
  Application.CreateForm(TFormDockDefault, FormDockableDefault);
  FormDockableDefault.Caption := 'Docktable Default'+I.ToString;
  FormDockableDefault.Label1.Text :=   FormDockableDefault.Caption;
  FormDockableDefault.Show;
end;

procedure TViewMain.ButtonNewDockableFormClick(Sender: TObject);
begin
  inc(I);
  Application.CreateForm(TFormDock, FormDockable);
  FormDockable.Caption := 'Docktable '+I.ToString;
  FormDockable.Show;
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

initialization
  ReportMemoryLeaksOnShutdown := true;


end.
