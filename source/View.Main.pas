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
    Memo1: TMemo;
    DockManger1: TDockManger;
    procedure ButtonNewDockableFormClick(Sender: TObject);
    procedure ButtonNewSimpleFormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ViewMain: TViewMain;

implementation

{$R *.fmx}

procedure TViewMain.ButtonNewDockableFormClick(Sender: TObject);
var
  Dockable: TFormDock;
begin
  Dockable := TFormDock.Create(Self);

  Dockable.Show;
end;

procedure TViewMain.ButtonNewSimpleFormClick(Sender: TObject);
var
  Simple: TFormSimple;
begin
  Simple := TFormSimple.Create(self);
  Simple.Show;
end;

procedure TViewMain.FormCreate(Sender: TObject);
begin
//  MessageManager := TMessageManager.DefaultManager;
//
//  MessageManager.SubscribeToMessage(TMessage<UnicodeString>, procedure(const Sender: TObject; const M: TMessage)
//  var
//    Value: string;
//    ValueM: TArray<string>;
//  begin
//    Value := (M as TMessage<UnicodeString>).Value;
//    ValueM := Value.Split(['|']);
//
//    if ValueM[0].Equals('DOCK_MANAGER') then begin
//      if ValueM[1].Equals('MOVING') then begin
//        self.DockManger1.DockToolVisible := true;
//
//      end else if ValueM[1].Equals('MOVED') then begin
//        self.DockManger1.DockToolVisible := false;
//      end;
//
//    end;
//
//
//    if Memo1.Lines.Count > 100 then
//      Memo1.Lines.Delete(Memo1.Lines.Count - 1);
//
//    Memo1.Lines.Insert(0,Value);
//  end
//  );
end;

end.
