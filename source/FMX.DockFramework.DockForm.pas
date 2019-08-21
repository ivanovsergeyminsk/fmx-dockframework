unit FMX.DockFramework.DockForm;

interface

uses
  System.SysUtils, System.Classes, FMX.Forms,
  System.Generics.Collections, System.SyncObjs,
  Winapi.Windows,
  FMX.DockFramework.DockTypes,
  FMX.DockFramework.DockMessages
  ;

type
  TCustomDockForm = class(TComponent)
  private class var
    CS: TCriticalSection;
    DockFormList: TDictionary<HWND, TCustomDockForm>;
  private
    class constructor Create;
    class destructor Destroy;

    class function TryGetDockForm(Hwnd: HWND; var DockForm: TCustomDockForm): boolean;
    class procedure AddOrSetDockForm(Hwnd: HWND; DockForm: TCustomDockForm);
  private
    FState: TDockElement;
    FDocks: TDocks;
    FForm: TForm;

    FHwnd : HWND;
    FOldWndProc : LONG_PTR;
    procedure InitWinHookMessage;

    procedure SendMesssageMoving;
    procedure SendMessageMoved;
    procedure SendMessageDockForm;

    procedure SubscribeFormCreated;
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

  TDockForm = class(TCustomDockForm)
  published
    property State default TDockElement.none;
    property Docks default AllDocks;
  end;

procedure Register;

implementation

uses
  FMX.Platform.Win, Winapi.Messages, System.Messaging;

procedure Register;
begin
  RegisterComponents('Dock Framework', [TDockForm]);
end;

function DockFormWndProc(Hwnd : HWND; Msg : UINT; WParam : WPARAM; LParam : LPARAM) : LRESULT; stdcall;
var
  DockForm: TCustomDockForm;
begin
  Result := 0;
  if not TCustomDockForm.TryGetDockForm(Hwnd, DockForm) then exit;

  if (Msg = WM_MOVING) then
    DockForm.SendMesssageMoving;
  if (Msg = WM_CAPTURECHANGED) then
   DockForm.SendMessageMoved;

  Result := CallWindowProc(Ptr(DockForm.FOldWndProc), Hwnd, Msg, WParam, LParam);
end;

{ TDockForm }

class procedure TCustomDockForm.AddOrSetDockForm(Hwnd: HWND; DockForm: TCustomDockForm);
begin
  CS.Enter;
  try
    TCustomDockForm.DockFormList.AddOrSetValue(Hwnd, DockForm);
  finally
    CS.Leave;
  end;
end;

constructor TCustomDockForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TForm
    then FForm := AOwner as TForm
    else FForm := nil;

  if (not (csDesignInstance in ComponentState)) and (not (csDesigning in ComponentState)) then begin
    InitWinHookMessage;
    TCustomDockForm.AddOrSetDockForm(FHwnd, self);
    SubscribeFormCreated;
  end;
end;

class constructor TCustomDockForm.Create;
begin
  CS := TCriticalSection.Create;
  TCustomDockForm.DockFormList := TDictionary<HWND, TCustomDockForm>.Create;
end;

destructor TCustomDockForm.Destroy;
begin

  inherited Destroy;
end;

function TCustomDockForm.GetDocks: TDocks;
begin
  result := FDocks;
end;

function TCustomDockForm.GetState: TDockElement;
begin
  result := FState;
end;

class destructor TCustomDockForm.Destroy;
begin
  CS.Free;
  TCustomDockForm.DockFormList.Free;
end;

procedure TCustomDockForm.InitWinHookMessage;
begin
  if not assigned(FForm) then exit;

  FHwnd      := FmxHandleToHwnd(FForm.Handle);
  FOldWndProc := GetWindowLongPtr(FHwnd, GWL_WNDPROC);

  SetWindowLongPtr(FHwnd, GWL_WNDPROC, LONG_PTR(@DockFormWndProc));
end;

procedure TCustomDockForm.SendMessageDockForm;
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

procedure TCustomDockForm.SendMessageMoved;
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

procedure TCustomDockForm.SendMesssageMoving;
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

procedure TCustomDockForm.SetDocks(const Value: TDocks);
begin
  FDocks := Value;
end;

procedure TCustomDockForm.SetState(const Value: TDockElement);
begin
  FState := Value;
end;

procedure TCustomDockForm.SubscribeFormCreated;
var
  MessageManager: TMessageManager;
begin
  MessageManager := TMessageManager.DefaultManager;

  MessageManager.SubscribeToMessage(TFormsCreatedMessage, procedure(const Sender: TObject; const M: TMessage)
  begin
//    if not (self = Sender) then exit;

    if not (State in [TDockElement.none])
      then SendMessageDockForm;
  end
  );
end;

class function TCustomDockForm.TryGetDockForm(Hwnd: HWND; var DockForm: TCustomDockForm): boolean;
begin
  CS.Enter;
  try
    result := TCustomDockForm.DockFormList.TryGetValue(Hwnd, DockForm);
  finally
    CS.Leave;
  end;
end;

end.
