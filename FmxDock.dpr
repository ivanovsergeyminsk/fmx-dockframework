program FmxDock;

uses
  System.StartUpCopy,
  FMX.Forms,
  View.Main in 'source\View.Main.pas' {ViewMain},
  View.SimpleForm in 'source\View.SimpleForm.pas' {FormSimple},
  View.DockableForm in 'source\View.DockableForm.pas' {FormDock},
  View.DockableFormDefault in 'source\View.DockableFormDefault.pas' {FormDockDefault};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewMain, ViewMain);
  Application.CreateForm(TFormSimple, FormSimple);
  Application.CreateForm(TFormDock, FormDock);
  Application.CreateForm(TFormDockDefault, FormDockDefault);
  Application.Run;
end.
