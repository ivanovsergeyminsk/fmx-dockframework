unit FMX.DockFramework.DockProvider;

interface

uses
  System.SysUtils, System.Classes, FMX.Forms,
  System.Generics.Collections, System.SyncObjs,
  Winapi.Windows,
  FMX.DockFramework.DockTypes,
  FMX.DockFramework.DockMessages
  ;

type
  TCustomDockProvider = class(TComponent)
  private
    class function TryGetDockForm(Hwnd: HWND; var DockForm: TCustomDockProvider): boolean;
    class procedure AddOrSetDockForm(Hwnd: HWND; DockForm: TCustomDockProvider);
  private
    FisInit: boolean;
    FState: TDockElement;
    FDocks: TDocks;
    FForm: TForm;
    FBorderStyle: TFmxFormBorderStyle;

    FHwnd : HWND;
    FOldWndProc : LONG_PTR;
    procedure InitWinHookMessage;

    procedure SendMesssageMoving;
    procedure SendMessageMoved;
    procedure SendMessageDock;

    procedure SubscribeFormCreated;
    procedure SubscribeFormActivate;
  protected
    function GetState: TDockElement;
    function GetDocks: TDocks;

    procedure SetState(const Value: TDockElement);
    procedure SetDocks(const Value: TDocks);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Form: TForm read FForm;
  published
    property State: TDockElement read GetState write SetState;
    property Docks: TDocks read GetDocks write SetDocks;
  end;

  TCustomDockContainer = class(TCustomDockProvider)

  end;

  TDockProvider = class(TCustomDockProvider)
  published
    property State default TDockElement.none;
    property Docks default AllDocks;
  end;

procedure Register;

implementation

uses
  FMX.Platform.Win, Winapi.Messages, System.Messaging;

var
  CS: TCriticalSection;
  DockFormList: TDictionary<HWND, TCustomDockProvider>;

type
  THackForm = class(TForm);

procedure Register;
begin
  RegisterComponents('Dock Framework', [TDockProvider]);
end;

function DockFormWndProc(Hwnd : HWND; Msg : UINT; WParam : WPARAM; LParam : LPARAM) : LRESULT; stdcall;
var
  DockProvider: TCustomDockProvider;
begin
  Result := 0;
  if not TCustomDockProvider.TryGetDockForm(Hwnd, DockProvider) then exit;

  if (Msg = WM_MOVING) then
    DockProvider.SendMesssageMoving;
  if (Msg = WM_CAPTURECHANGED) then
   DockProvider.SendMessageMoved;

  Result := CallWindowProc(Ptr(DockProvider.FOldWndProc), Hwnd, Msg, WParam, LParam);
end;

{ TDockForm }

class procedure TCustomDockProvider.AddOrSetDockForm(Hwnd: HWND; DockForm: TCustomDockProvider);
begin
  CS.Enter;
  try
    DockFormList.AddOrSetValue(Hwnd, DockForm);
  finally
    CS.Leave;
  end;
end;

constructor TCustomDockProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FisInit := false;

  if AOwner is TForm
    then FForm := AOwner as TForm
    else FForm := nil;

  FBorderStyle := FForm.BorderStyle;
  if (not (csDesignInstance in ComponentState)) and (not (csDesigning in ComponentState)) then begin
    SubscribeFormActivate;
//    InitWinHookMessage;

    //Пересоздание формы необходима для перехвата WM
    SubscribeFormCreated;
    if assigned(FForm) then begin
      THackForm(FForm).Recreate;
    end;
  end;
end;


destructor TCustomDockProvider.Destroy;
begin

  inherited Destroy;
end;

function TCustomDockProvider.GetDocks: TDocks;
begin
  result := FDocks;
end;

function TCustomDockProvider.GetState: TDockElement;
begin
  result := FState;
end;

procedure TCustomDockProvider.InitWinHookMessage;
begin
  if not assigned(FForm) then exit;

  FHwnd      := FmxHandleToHwnd(FForm.Handle);
  FOldWndProc := GetWindowLongPtr(FHwnd, GWL_WNDPROC);
  TCustomDockProvider.AddOrSetDockForm(FHwnd, self);

  SetWindowLongPtr(FHwnd, GWL_WNDPROC, LONG_PTR(@DockFormWndProc));
end;

procedure TCustomDockProvider.SendMessageDock;
var
  MessageManager: TMessageManager;
  LMessage: TDockMessageDock;
  Value: TDockDock;
begin
  Value.Dock        := State;
  Value.AccessDocks := Docks;
  LMessage := TDockMessageDock.Create(Value);

  MessageManager := TMessageManager.DefaultManager;
  MessageManager.SendMessage(self, LMessage, true);
end;

procedure TCustomDockProvider.SendMessageMoved;
var
  MessageManager: TMessageManager;
  LMessage: TDockMessageMoved;
  Value: TDockMove;
begin
  Value.MousePosition := Screen.MousePos;
  Value.AccessDocks   := Docks;
  LMessage := TDockMessageMoved.Create(Value);

  MessageManager := TMessageManager.DefaultManager;
  MessageManager.SendMessage(self, LMessage, true);
end;

procedure TCustomDockProvider.SendMesssageMoving;
var
  MessageManager: TMessageManager;
  LMessage: TDockMessageMoving;
  Value: TDockMove;
begin
  Value.MousePosition := Screen.MousePos;
  Value.AccessDocks   := Docks;
  LMessage := TDockMessageMoving.Create(Value);

  MessageManager := TMessageManager.DefaultManager;
  MessageManager.SendMessage(self, LMessage, true);
end;

procedure TCustomDockProvider.SetDocks(const Value: TDocks);
begin
  FDocks := Value;
end;

procedure TCustomDockProvider.SetState(const Value: TDockElement);
begin
  FState := Value;

  if not (FState in [TDockElement.none]) then
    FForm.BorderStyle := TFmxFormBorderStyle.None
  else
    FForm.BorderStyle := FBorderStyle;
end;

procedure TCustomDockProvider.SubscribeFormActivate;
var
  MessageManager: TMessageManager;
begin
  MessageManager := TMessageManager.DefaultManager;

  MessageManager.SubscribeToMessage(TFormActivateMessage, procedure(const Sender: TObject; const M: TMessage)
  var
    Form: TCommonCustomForm;
  begin
    Form := TFormActivateMessage(M).Value;
    if not (FForm = Form) then exit;

    if (not (State in [TDockElement.none])) and (not FisInit) then begin
      FisInit := true;
      SendMessageDock;
    end;

    FForm.BorderStyle := FBorderStyle;
  end
  );
end;

procedure TCustomDockProvider.SubscribeFormCreated;
var
  MessageManager: TMessageManager;
begin
  MessageManager := TMessageManager.DefaultManager;

  MessageManager.SubscribeToMessage(TAfterCreateFormHandle, procedure(const Sender: TObject; const M: TMessage)
  var
    Form: TCommonCustomForm;
  begin
    Form := TAfterCreateFormHandle(M).Value;
    if not (FForm = Form) then exit;

    InitWinHookMessage;
  end
  );
end;

class function TCustomDockProvider.TryGetDockForm(Hwnd: HWND; var DockForm: TCustomDockProvider): boolean;
begin
  CS.Enter;
  try
    result := DockFormList.TryGetValue(Hwnd, DockForm);
  finally
    CS.Leave;
  end;
end;

initialization
  CS := TCriticalSection.Create;
  DockFormList := TDictionary<HWND, TCustomDockProvider>.Create;

finalization
  CS.Free;
  DockFormList.Free;

end.
