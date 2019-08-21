unit FMX.DockFramework.DockForm;

interface

uses
  System.SysUtils, System.Classes, FMX.Forms,
  System.Generics.Collections, System.SyncObjs,

  Winapi.Windows,
  FMX.DockFramework.DockTypes
  ;

type
  TDockForm = class(TComponent)
  private class var
    CS: TCriticalSection;
    DockFormList: TDictionary<HWND, TDockForm>;
  private
    class constructor Create;
    class destructor Destroy;

    class function TryGetDockForm(Hwnd: HWND; var DockForm: TDockForm): boolean;
    class procedure AddOrSetDockForm(Hwnd: HWND; DockForm: TDockForm);
  private
    { Private declarations }
    FState: TDockState;
    FDocks: TDocks;
    FForm: TForm;

    FHwnd : HWND;
    FOldWndProc : LONG_PTR;
    procedure InitWinHookMessage;

    procedure SendMesssageMoving;
    procedure SendMessageMoved;

    procedure SetState(const Value: TDockState);
    procedure SetDocks(const Value: TDocks);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property State: TDockState read FState write SetState;
    property Docks: TDocks read FDocks write SetDocks;
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
  DockForm: TDockForm;
begin
  if not TDockForm.DockFormList.TryGetValue(Hwnd, DockForm) then exit;

  Result := 0;
  if (Msg = WM_MOVING) then
    DockForm.SendMesssageMoving;
  if (Msg = WM_CAPTURECHANGED) then
   DockForm.SendMessageMoved;

  Result := CallWindowProc(Ptr(DockForm.FOldWndProc), Hwnd, Msg, WParam, LParam);
end;

{ TDockForm }

class procedure TDockForm.AddOrSetDockForm(Hwnd: HWND; DockForm: TDockForm);
begin
  CS.Enter;
  try
    TDockForm.DockFormList.AddOrSetValue(Hwnd, DockForm);
  finally
    CS.Leave;
  end;
end;

constructor TDockForm.Create(AOwner: TComponent);
begin
  FDocks := [];
  inherited Create(AOwner);
  if AOwner is TForm
    then FForm := AOwner as TForm
    else FForm := nil;

  if (csDesignInstance in ComponentState) or (csDesigning in ComponentState) then exit;

  InitWinHookMessage;
  TDockForm.AddOrSetDockForm(FHwnd, self);
end;

class constructor TDockForm.Create;
begin
  CS := TCriticalSection.Create;
  TDockForm.DockFormList := TDictionary<HWND, TDockForm>.Create;
end;

destructor TDockForm.Destroy;
begin

  inherited Destroy;
end;

class destructor TDockForm.Destroy;
begin
  CS.Free;
  TDockForm.DockFormList.Free;
end;

procedure TDockForm.InitWinHookMessage;
begin
  if not assigned(FForm) then exit;

  FHwnd      := FmxHandleToHwnd(FForm.Handle);
  FOldWndProc := GetWindowLongPtr(FHwnd, GWL_WNDPROC);

  SetWindowLongPtr(FHwnd, GWL_WNDPROC, LONG_PTR(@DockFormWndProc));
end;

procedure TDockForm.SendMessageMoved;
var
  MessageManager: TMessageManager;
  LMessage: TMessage<UnicodeString>;
  RDocks: TDocks;
  LDocks: integer absolute RDocks;
begin
  RDocks := FDocks;
  MessageManager := TMessageManager.DefaultManager;

  LMessage := TMessage<UnicodeString>.Create(Format(TDockMessages.MOVED, [Screen.MousePos.X, Screen.MousePos.Y, LDocks]));
  MessageManager.SendMessage(self, LMessage, true);
end;

procedure TDockForm.SendMesssageMoving;
var
  MessageManager: TMessageManager;
  LMessage: TMessage<UnicodeString>;
  RDocks: TDocks;
  LDocks: integer absolute RDocks;
begin
  RDocks := FDocks;
  MessageManager := TMessageManager.DefaultManager;

  LMessage := TMessage<UnicodeString>.Create(Format(TDockMessages.MOVING, [Screen.MousePos.X, Screen.MousePos.Y, LDocks]));
  MessageManager.SendMessage(self, LMessage, true);
end;

procedure TDockForm.SetDocks(const Value: TDocks);
begin
  FDocks := Value;
end;

procedure TDockForm.SetState(const Value: TDockState);
begin
  FState := Value;
end;

class function TDockForm.TryGetDockForm(Hwnd: HWND; var DockForm: TDockForm): boolean;
begin
  CS.Enter;
  try
    result := TDockForm.DockFormList.TryGetValue(Hwnd, DockForm);
  finally
    CS.Leave;
  end;
end;

end.
