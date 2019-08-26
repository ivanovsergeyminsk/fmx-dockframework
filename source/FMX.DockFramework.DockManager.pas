unit FMX.DockFramework.DockManager;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Layouts, FMX.StdCtrls,
  FMX.Objects, System.UITypes, System.Messaging, System.Types, FMX.Forms, FMX.TabControl,

  FMX.DockFramework.FDockToolForm,
  FMX.DockFramework.DockTypes,
  FMX.DockFramework.DockMessages,
  FMX.DockFramework.DockProvider, FMX.Styles
  ;

type
  TPictureDockTool = class(TGridPanelLayout)
    strict private
      FPicture: TRectangle;
      FSpace:   TRectangle;
      FColor:   TAlphaColor;
      FIsActive: boolean;

      function GetIsActive: boolean;
      function GetColor: TAlphaColor;
    strict protected
      procedure SetIsActive(const Value: boolean); virtual;
      procedure SetColor(const Value: TAlphaColor); virtual;
      property Picture: TRectangle read FPicture;
      property Space: TRectangle read FSpace;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      property IsActive: boolean read GetIsActive write SetIsActive;
      property Color: TAlphaColor read GetColor write SetColor;
    end;

  TPictureTop = class(TPictureDockTool)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPictureLeft = class(TPictureDockTool)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPictureBottom = class(TPictureDockTool)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPictureRight = class(TPictureDockTool)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPictureClient = class(TPictureDockTool)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TDockTool = class(TLayout)
  private const
    BACKGROUND =
    'M0,237 L15.194676399231,237.016204833984 L30.0032386779785,222.184951782227 '+
    'L30,207 L60,207 L60.0047988891602,222.195693969727 L74.7881927490234,236.991134643555 '+
    'L89.9999618530273,237.000137329102 L89.9999618530273,267 L74.7809371948242,266.997009277344 '+
    'L60.0064430236816,281.771484375 L59.999942779541,297.000244140625 L30,297.000244140625 '+
    'L30.0146007537842,281.794189453125 L15.2173891067505,266.996765136719 L0,267 Z ';
  private
    FActiveElement: TDockElement;
    FDocks: TDocks;
    FColor: TAlphaColor;

    FBackground: TPath;
    FGridPanel: TGridPanelLayout;

    FViewDockTop:     TPictureDockTool;
    FViewDockBottom:  TPictureDockTool;
    FViewDockLeft:    TPictureDockTool;
    FViewDockRight:   TPictureDockTool;
    FViewDockClient:  TPictureDockTool;
    FBackgroundColor: TAlphaColor;

    procedure CreateBackground;
    procedure CreateGridPanel;
    procedure CreatePictures;
    procedure SetPictures;

    procedure SetActiveElement(const Value: TDockElement);
    procedure SetColor(const Value: TAlphaColor);
    function GetBottomRect: TRectF;
    function GetClientRect: TRectF;
    function GetLeftRect: TRectF;
    function GetRightRect: TRectF;
    function GetTopRect: TRectF;

    function PictureDockToolToScreenRect(APictureDockTool: TPictureDockTool): TRectF;

    procedure SetBackgroundColor(const Value: TAlphaColor);  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ActiveElement: TDockElement read FActiveElement write SetActiveElement;
    property Color: TAlphaColor read FColor write SetColor;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;

    property TopRect: TRectF read GetTopRect;
    property LeftRect: TRectF read GetLeftRect;
    property BottomRect: TRectF read GetBottomRect;
    property RightRect: TRectF read GetRightRect;
    property ClientRect: TRectF read GetClientRect;

    property Docks: TDocks read FDocks write FDocks;
  end;


  TDockTabItemClose = class(TTabItem)
  private
    FItemStyle: TControl;
    FOnClose: TNotifyEvent;
    procedure DoOnCloseClick(Sender: TObject);
  protected
    function GetStyleObject: TFmxObject; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
  public
    property OnCloseClick: TNotifyEvent read FOnClose write FOnClose;
  end;

  TDockContent = class(TLayout)
  private
    FDockTab: TTabControl;
    FDockElement: TDockElement;

    FisTabItemDrag: boolean;
    FTabItemDragPosition: TPointF;
    FOnDock: TNotifyEvent;

    procedure TabItemMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure TabItemMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure TabItemMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);

    procedure ControlsToForm(Tabitem: TTabItem; Form: TForm);
    procedure ControlsToDock(TabItem: TTabitem; Form: TForm);
    function GetCount: integer;
    procedure SetOnDock(const Value: TNotifyEvent);

    procedure DeleteTabItem(TabItem: TTabItem);
    procedure DoCloseContent(Sender: TObject);
  protected
    procedure DoUnDockTab(TabItem: TTabItem);
    procedure DoDockTab(Form: TForm);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DockElement: TDockElement read FDockElement write FDockElement;
    property Count: integer read GetCount;
    property OnDock: TNotifyEvent read FOnDock write SetOnDock;
  end;

  TDockContents = class(TLayout)
  private
    FDockTop:     TDockContent;
    FDockLeft:    TDockContent;
    FDockBottom:  TDockContent;
    FDockRight:   TDockContent;
    FDockClient:  TDockContent;

    FSplitterTop:     TSplitter;
    FSplitterLeft:    TSplitter;
    FSplitterBottom:  TSplitter;
    FSplitterRight:   TSplitter;

    FPreview: TRectangle;
    FPreviewState: TDockElement;

    procedure DoOnDock(Sender: TObject);
    procedure SetPreviewState(const Value: TDockElement);
    function GetPreviewColor: TAlphaColor;
    procedure SetPreviewColor(const Value: TAlphaColor);
    function GetDockBottomSize: single;
    function GetDockLeftSize: single;
    function GetDockRightSize: single;
    function GetDockTopSize: single;
    procedure SetDockBottomSize(const Value: single);
    procedure SetDockLeftSize(const Value: single);
    procedure SetDockRightSize(const Value: single);
    procedure SetDockTopSize(const Value: single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddContent(const DockElement: TDockElement; Form: TForm);

    property Preview: TDockElement read FPreviewState write SetPreviewState;
    property PreviewColor: TAlphaColor read GetPreviewColor write SetPreviewColor;

    property DockTopSize: single read GetDockTopSize write SetDockTopSize;
    property DockLeftSize: single read GetDockLeftSize write SetDockLeftSize;
    property DockBottomSize: single read GetDockBottomSize write SetDockBottomSize;
    property DockRightSize: single read GetDockRightSize write SetDockRightSize;
  end;

  TCustomDockManager = class(TLayout)
  private
    { Private declarations }
    FContents: TDockContents;

    FDockToolForm: TFDockToolForm;
    FDockToolLayout: TLayout;
    FDockTool: TDockTool;

    function GetDockToolSize: single;
    procedure SetDockToolSize(const Value: single);

    function GetVisibleDockTool: boolean;
    procedure SetVisibleDockTool(const Value: boolean);

    function GetActiveElement: TDockElement;
    procedure SetActiveElement(const Value: TDockElement);

    function  GetDocToolColor: TAlphaColor;
    procedure SetDockToolColor(const Value: TAlphaColor);

    procedure Subscribes;
    procedure SubscribeDockMessageMoving;
    procedure SubscribeDockMessageMoved;
    procedure SubscribeDockMessageDock;


    procedure EventMoving(const X, Y: single; const Docks: TDocks);
    procedure EventMoved(const X, Y: single; const Docks: TDocks; Form: TForm);

    procedure ShowPreview(const X, Y: single);

    procedure DoDock(const SelectedDock: TDockElement; Form: TForm);
    procedure CreateDockTool;

    function GetDockToolBackgroundColor: TAlphaColor;
    procedure SetDockToolBackgroundColor(const Value: TAlphaColor);
    function GetDockBottomSize: single;
    function GetDockLeftSize: single;
    function GetDockRightSize: single;
    function GetDockTopSize: single;
    procedure SetDockBottomSize(const Value: single);
    procedure SetDockLeftSize(const Value: single);
    procedure SetDockRightSize(const Value: single);
    procedure SetDockTopSize(const Value: single);
    property DockToolVisible: boolean read GetVisibleDockTool write SetVisibleDockTool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DockToolSize: single read GetDockToolSize write SetDockToolSize;
    property DockToolColor: TAlphaColor read GetDocToolColor write SetDockToolColor;
    property DockToolBackgroundColor: TAlphaColor read GetDockToolBackgroundColor write SetDockToolBackgroundColor;
    property DockToolElement: TDockElement read GetActiveElement write SetActiveElement;

    property DockTopSize: single read GetDockTopSize write SetDockTopSize;
    property DockLeftSize: single read GetDockLeftSize write SetDockLeftSize;
    property DockBottomSize: single read GetDockBottomSize write SetDockBottomSize;
    property DockRightSize: single read GetDockRightSize write SetDockRightSize;
  end;

  TDockManager = class(TCustomDockManager)
  published
    property DockToolSize;
    property DockToolColor;// default TAlphaColorRec.Springgreen;
    property DockToolBackgroundColor;// default TAlphaColorRec.White;
    property DockToolElement;
    property DockTopSize;
    property DockLeftSize;
    property DockBottomSize;
    property DockRightSize;
  end;

procedure Register;

implementation

uses
  FMX.Graphics, FMX.Dialogs, System.Math;

  {$R *.win.res}

procedure Register;
begin
  RegisterComponents('Dock Framework', [TDockManager]);
end;

procedure RearrangeColumnsAndRows(GridPanelLayout: TGridPanelLayout); overload;
var
  WorkaroundIndex: Integer;
  ColumnIndex: Integer;
  ColumnItem: TGridPanelLayout.TColumnItem;

  RowIndex: integer;
  RowItem: TGridPanelLayout.TRowItem;
begin
  for WorkaroundIndex := 1 to 10 do begin
    for ColumnIndex := 0 to GridPanelLayout.ColumnCollection.Count - 1 do
    begin
      ColumnItem := TGridPanelLayout.TColumnItem(GridPanelLayout.ColumnCollection[ColumnIndex]);
      ColumnItem.SizeStyle := TGridPanelLayout.TSizeStyle.Percent;
      ColumnItem.Value     := 100/GridPanelLayout.ColumnCollection.Count;
    end;

    for RowIndex := 0 to GridPanelLayout.RowCollection.Count - 1 do
    begin
      RowItem := TGridPanelLayout.TRowItem(GridPanelLayout.RowCollection[RowIndex]);
      RowItem.SizeStyle := TGridPanelLayout.TSizeStyle.Percent;
      RowItem.Value     := 100/GridPanelLayout.RowCollection.Count;
    end;
  end;
end;

procedure Rearrange(GridPanelLayout: TGridPanelLayout; ColumntValues, RowValues: TArray<double>);
var
  WorkaroundIndex: Integer;
  ColumnIndex: Integer;
  ColumnItem: TGridPanelLayout.TColumnItem;

  RowIndex: integer;
  RowItem: TGridPanelLayout.TRowItem;
begin
  for WorkaroundIndex := 1 to 10 do begin
    for ColumnIndex := 0 to GridPanelLayout.ColumnCollection.Count - 1 do
    begin
      ColumnItem := TGridPanelLayout.TColumnItem(GridPanelLayout.ColumnCollection[ColumnIndex]);
      ColumnItem.SizeStyle := TGridPanelLayout.TSizeStyle.Percent;
      ColumnItem.Value     := ColumntValues[ColumnIndex];
    end;

    for RowIndex := 0 to GridPanelLayout.RowCollection.Count - 1 do
    begin
      RowItem := TGridPanelLayout.TRowItem(GridPanelLayout.RowCollection[RowIndex]);
      RowItem.SizeStyle := TGridPanelLayout.TSizeStyle.Percent;
      RowItem.Value     := RowValues[RowIndex];
    end;
  end;
end;

{$REGION 'TCustomDockManger'}

constructor TCustomDockManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  self.BeginUpdate;
  try
    FContents := TDockContents.Create(self);
    FContents.Stored := false;
    FContents.Align  := TAlignLayout.Client;
    FContents.Parent := self;

    CreateDockTool;

    Subscribes;

    if (csDesigning in ComponentState) or (csDesignInstance in ComponentState)
      then DockToolVisible := true
      else DockToolVisible := false;
  finally
    self.EndUpdate;
  end;
end;

procedure TCustomDockManager.CreateDockTool;
begin
  FDockToolLayout := TLayout.Create(self);
  FDockToolLayout.Align   := TAlignLayout.Contents;
  FDockToolLayout.Parent  := self;
  FDockToolLayout.HitTest := false;
  FDockToolLayout.Stored  := false;

  FDockTool := TDockTool.Create(self);
  FDockTool.Align   := TAlignLayout.Center;
  FDockTool.Parent  := FDockToolLayout;
  FDockTool.Stored  := false;
  SetDockToolSize(100.00);

  if (not (csDesigning in ComponentState)) and (not (csDesignInstance in ComponentState)) then begin
    Application.CreateForm(TFDockToolForm, FDockToolForm);
    FDockToolForm.Transparency := true;
    FDockToolForm.FormStyle    := TFormStyle.StayOnTop;
    FDockToolForm.Stored       := false;
    FDockToolForm.Visible      := true;
    FDockToolForm.Show;
    FDockTool.Parent           := self.FDockToolForm;
    FDockTool.Opacity          := 0.5;
  end;

end;

destructor TCustomDockManager.Destroy;
begin

  inherited Destroy;
end;

procedure TCustomDockManager.DoDock(const SelectedDock: TDockElement; Form: TForm);
begin
  self.BeginUpdate;
  try
    FContents.AddContent(SelectedDock, Form);
  finally
    self.EndUpdate;
  end;
end;

procedure TCustomDockManager.EventMoving(const X, Y: single; const Docks: TDocks);
var
  Point: TPointF;
begin
  if (not (csDesigning in ComponentState)) and (not (csDesignInstance in ComponentState)) then begin
    Point := self.LocalToScreen(TPointF.Create(0,0));
    self.FDockToolForm.Left     := Trunc(Point.X);
    self.FDockToolForm.Top      := Trunc(Point.Y);
    self.FDockToolForm.Width    := Trunc(self.Width);
    self.FDockToolForm.Height   := Trunc(self.Height);
  end;

  self.FDockTool.Docks := Docks;
  self.DockToolVisible := true;
  ShowPreview(X, Y);
end;

function TCustomDockManager.GetActiveElement: TDockElement;
begin
  result := FDockTool.ActiveElement;
end;

function TCustomDockManager.GetDockBottomSize: single;
begin
  result := FContents.DockBottomSize;
end;

function TCustomDockManager.GetDockLeftSize: single;
begin
  result := FContents.DockLeftSize;
end;

function TCustomDockManager.GetDockRightSize: single;
begin
  result := FContents.DockRightSize;
end;

function TCustomDockManager.GetDockToolBackgroundColor: TAlphaColor;
begin
  result := FDockTool.BackgroundColor;
end;

function TCustomDockManager.GetDockToolSize: single;
begin
  result := FDockTool.Width;
end;

function TCustomDockManager.GetDockTopSize: single;
begin
  result := FContents.DockTopSize;
end;

function TCustomDockManager.GetDocToolColor: TAlphaColor;
begin
  result := FDockTool.Color;
end;

function TCustomDockManager.GetVisibleDockTool: boolean;
begin
  result := FDockTool.Visible;
end;

procedure TCustomDockManager.SetActiveElement(const Value: TDockElement);
begin
  FDockTool.ActiveElement := Value;
  FContents.Preview       := Value;
end;

procedure TCustomDockManager.SetDockBottomSize(const Value: single);
begin
  FContents.DockBottomSize := Value;
end;

procedure TCustomDockManager.SetDockLeftSize(const Value: single);
begin
  FContents.DockLeftSize := Value;
end;

procedure TCustomDockManager.SetDockRightSize(const Value: single);
begin
  FContents.DockRightSize := Value;
end;

procedure TCustomDockManager.SetDockToolBackgroundColor(
  const Value: TAlphaColor);
begin
  FDockTool.BackgroundColor := Value;
end;

procedure TCustomDockManager.SetDockToolColor(const Value: TAlphaColor);
begin
  FDockTool.Color         := Value;
  FContents.PreviewColor  := Value;
end;

procedure TCustomDockManager.SetDockToolSize(const Value: single);
begin
  FDockTool.Width   := Value;
  FDockTool.Height  := FDockTool.Width;
end;

procedure TCustomDockManager.SetDockTopSize(const Value: single);
begin
  FContents.DockTopSize := Value;
end;

procedure TCustomDockManager.ShowPreview(const X, Y: single);
var
  MousePoint: TPointF;
begin
  MousePoint := TPointF.Create(X, Y);
  if FDockTool.GetBottomRect.Contains(MousePoint) and (TDockElement.Bottom in FDockTool.Docks) then begin
    DockToolElement := TDockElement.Bottom;
    exit;
  end;

  if FDockTool.GetClientRect.Contains(MousePoint) and (TDockElement.Client in FDockTool.Docks) then begin
    DockToolElement := TDockElement.Client;
    exit;
  end;

  if FDockTool.GetLeftRect.Contains(MousePoint) and (TDockElement.Left in FDockTool.Docks) then begin
    DockToolElement := TDockElement.Left;
    exit;
  end;

  if FDockTool.GetRightRect.Contains(MousePoint) and (TDockElement.Right in FDockTool.Docks) then begin
    DockToolElement := TDockElement.Right;
    exit;
  end;

  if FDockTool.GetTopRect.Contains(MousePoint) and (TDockElement.Top in FDockTool.Docks) then begin
    DockToolElement := TDockElement.Top;
    exit;
  end;

  DockToolElement := TDockElement.none;
end;

procedure TCustomDockManager.SetVisibleDockTool(const Value: boolean);
begin
  FDockTool.Visible := Value;
end;

procedure TCustomDockManager.SubscribeDockMessageDock;
var
  MessageManager: TMessageManager;
begin
  MessageManager := TMessageManager.DefaultManager;

  MessageManager.SubscribeToMessage(TDockMessageDock, procedure(const Sender: TObject; const M: TMessage)
  var
    Value: TDockDock;
    DockForm: TDockProvider;
  begin
    if not (Sender is TDockProvider) then exit;

    DockForm := Sender as TDockProvider;
    Value := (M as TDockMessageDock).Value;
    if Value.Dock in Value.AccessDocks then
      DoDock(Value.Dock, DockForm.Form);
  end
  );
end;

procedure TCustomDockManager.SubscribeDockMessageMoved;
var
  MessageManager: TMessageManager;
begin
  MessageManager := TMessageManager.DefaultManager;

  MessageManager.SubscribeToMessage(TDockMessageMoved, procedure(const Sender: TObject; const M: TMessage)
  var
    Value: TDockMove;
    DockForm: TDockProvider;
  begin
    if not (Sender is TDockProvider) then exit;

    DockForm := Sender as TDockProvider;
    Value := (M as TDockMessageMoved).Value;
    EventMoved(Value.MousePosition.X, Value.MousePosition.Y, Value.AccessDocks, DockForm.Form);
  end
  );
end;

procedure TCustomDockManager.SubscribeDockMessageMoving;
var
  MessageManager: TMessageManager;
begin
  MessageManager := TMessageManager.DefaultManager;

  MessageManager.SubscribeToMessage(TDockMessageMoving, procedure(const Sender: TObject; const M: TMessage)
  var
    Value: TDockMove;
  begin
    Value := (M as TDockMessageMoving).Value;
    EventMoving(Value.MousePosition.X, Value.MousePosition.Y, Value.AccessDocks);
  end
  );
end;

procedure TCustomDockManager.Subscribes;
begin
  SubscribeDockMessageMoving;
  SubscribeDockMessageMoved;
  SubscribeDockMessageDock;
end;

procedure TCustomDockManager.EventMoved(const X, Y: single; const Docks: TDocks; Form: TForm);
var
  MousePoint: TPointF;
  SelelectedDock: TDockElement;
begin
  self.FDockTool.Docks := Docks;
  self.DockToolVisible := false;

  MousePoint := TPointF.Create(X, Y);
  if FDockTool.GetBottomRect.Contains(MousePoint) and (TDockElement.Bottom in FDockTool.Docks) then begin
    SelelectedDock := TDockElement.Bottom;
  end else
  if FDockTool.GetClientRect.Contains(MousePoint) and (TDockElement.Client in FDockTool.Docks) then begin
    SelelectedDock := TDockElement.Client;
  end else
  if FDockTool.GetLeftRect.Contains(MousePoint) and (TDockElement.Left in FDockTool.Docks) then begin
    SelelectedDock := TDockElement.Left;
  end else
  if FDockTool.GetRightRect.Contains(MousePoint) and (TDockElement.Right in FDockTool.Docks) then begin
    SelelectedDock := TDockElement.Right;
  end else
  if FDockTool.GetTopRect.Contains(MousePoint) and (TDockElement.Top in FDockTool.Docks) then begin
    SelelectedDock := TDockElement.Top;
  end else
    SelelectedDock := TDockElement.none;

  DoDock(SelelectedDock, Form);
end;

{$ENDREGION}

{$REGION 'TDockContent'}

procedure TDockContent.TabItemMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
  if not FisTabItemDrag then exit;

  if (abs(FTabItemDragPosition.X-X) > 30)
  then
    DoUnDockTab(Sender as TTabItem);
end;

procedure TDockContent.ControlsToDock(TabItem: TTabitem; Form: TForm);
var
  FMXObject: TFMXObject;
begin
  Form.BeginUpdate;
  try
    for FmxObject in Form.Children.ToArray do
      FmxObject.Parent := TabItem;
  finally
    Form.EndUpdate;
  end;
end;

procedure TDockContent.ControlsToForm(Tabitem: TTabItem; Form: TForm);
var
  Content,
  FmxObjectForm: TFMXObject;
begin
  Form.BeginUpdate;
  try
    for Content in TabItem.Children.ToArray do begin
      if not (Content.Name = 'TabItemContent_') then
        continue;

      for FmxObjectForm in Content.Children.ToArray do begin
        Form.AddObject(FmxObjectForm);
      end;
      break;
    end;
  finally
    Form.EndUpdate;
  end;
end;

constructor TDockContent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDockTab := TTabControl.Create(self);
  FDockTab.Align       := TAlignLayout.Client;
  FDockTab.TabPosition := TTabPosition.Top;
  FDockTab.TabHeight   := 26;
  FDockTab.Parent      := self;
  FDockTab.Visible     := true;
  FDockTab.Stored      := false;
end;

procedure TDockContent.DeleteTabItem(TabItem: TTabItem);
var
  TabControl: TTabControl;
  Idx: integer;
begin
  TabControl := TabItem.TabControl;
  TabControl.ActiveTab := nil;

  idx := TabItem.Index;
  TabItem.Parent := nil;
  TabItem.Free;

  TabControl.ActiveTab := TabControl.Tabs[Min(idx, TabControl.TabCount-1)];

  if TabControl.TabCount = 0 then
    TabControl.Visible := false;
end;

destructor TDockContent.Destroy;
begin

  inherited Destroy;
end;

procedure TDockContent.DoCloseContent(Sender: TObject);
var
  TabItem: TTabItem;
  Form: TForm;
begin
  TabItem := Sender as TTabItem;
  Form := TForm(TabItem.Tag);
  if Form.Close = TCloseAction.caNone then exit;

  DeleteTabItem(TabItem);
end;

procedure TDockContent.DoDockTab(Form: TForm);
var
  TabItem: TDockTabItemClose;
begin
  FDockTab.Visible := true;

  TabItem := FDockTab.Add(TDockTabItemClose) as TDockTabItemClose;
  TabItem.BeginUpdate;
  try
    TabItem.Tag  := integer(Form);
    TabItem.Text := Form.Caption;
//    TabItem.AutoSize := false;
//    TabItem.Width         := 300;
//    TabItem.Height        := 26;
    TabItem.OnMouseDown   := TabItemMouseDown;
    TabItem.OnMouseUp     := TabItemMouseUp;
    TabItem.OnMouseMove   := TabItemMouseMove;
    TabItem.OnCloseClick  := DoCloseContent;

    ControlsToDock(TabItem, Form);
  finally
    TabItem.EndUpdate;
  end;

  FDockTab.ActiveTab := TabItem;

  if assigned(FOnDock) then
    FOnDock(self);

  Form.Visible := false;
end;

procedure TDockContent.DoUnDockTab(TabItem: TTabItem);
var
  Form: TForm;
begin
  Form := TForm(TabItem.Tag);
  self.BeginUpdate;
  try
    ControlsToForm(TabItem, Form);
    DeleteTabItem(TabItem);
    FisTabItemDrag := false;
  finally
    self.EndUpdate;
  end;

  if assigned(FOnDock) then
    FOnDock(self);

  Form.Left := Trunc(Screen.MousePos.X-150);
  Form.Top  := Trunc(Screen.MousePos.Y - 5);
  Form.Show;
  Form.StartWindowDrag;
end;

function TDockContent.GetCount: integer;
begin
  result := FDockTab.TabCount;
end;

procedure TDockContent.SetOnDock(const Value: TNotifyEvent);
begin
  FOnDock := Value;
end;

procedure TDockContent.TabItemMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then begin
    FisTabItemDrag := true;
    FTabItemDragPosition.X := X;
    FTabItemDragPosition.Y := Y;
  end else
    FisTabItemDrag := false;
end;

procedure TDockContent.TabItemMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FisTabItemDrag := false;
end;

{$ENDREGION}

{$REGION 'TDockTool'}

constructor TDockTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocks := [];
  CreateBackground;
  CreateGridPanel;
  CreatePictures;
  SetPictures;
end;

procedure TDockTool.CreateBackground;
begin
  FBackground := TPath.Create(self);
  FBackground.Parent      := self;
  FBackground.Align       := TAlignLayout.Contents;
  FBackground.Fill.Color  := TAlphaColorRec.White;
  FBackground.Stroke.Kind := TBrushKind.None;
  FBackground.Data.Data   := BACKGROUND;
end;

procedure TDockTool.CreateGridPanel;
var
 Row: TGridPanelLayout.TRowItem;
 Col: TGridPanelLayout.TColumnItem;
  I: Integer;
begin
  FGridPanel := TGridPanelLayout.Create(self);
  FGridPanel.Parent := self;
  FGridPanel.Align  := TAlignLayout.Client;
  FGridPanel.RowCollection.ClearAndResetID;
  FGridPanel.ColumnCollection.ClearAndResetID;

  for I := 0 to 2 do begin
    Row := FGridPanel.RowCollection.Add;
    Row.SizeStyle := TGridPanelLayout.TSizeStyle.Percent;
    Row.Value     := 33.333;
  end;

  for I := 0 to 2 do begin
    Col := FGridPanel.ColumnCollection.Add;
    Col.SizeStyle := TGridPanelLayout.TSizeStyle.Percent;
    Col.Value     := 33.333;
  end;

  RearrangeColumnsAndRows(FGridPanel);
end;

procedure TDockTool.CreatePictures;
begin
  FViewDockTop    := TPictureTop.Create(self);
  FViewDockBottom := TPictureBottom.Create(self);
  FViewDockLeft   := TPictureLeft.Create(self);
  FViewDockRight  := TPictureRight.Create(self);
  FViewDockClient := TPictureClient.Create(self);
end;

destructor TDockTool.Destroy;
begin

  inherited;
end;

function TDockTool.GetBottomRect: TRectF;
begin
  result := PictureDockToolToScreenRect(FViewDockBottom);
end;

function TDockTool.GetClientRect: TRectF;
begin
  result := PictureDockToolToScreenRect(FViewDockClient);
end;

function TDockTool.GetLeftRect: TRectF;
begin
  result := PictureDockToolToScreenRect(FViewDockLeft);
end;

function TDockTool.GetRightRect: TRectF;
begin
  result := PictureDockToolToScreenRect(FViewDockRight);
end;

function TDockTool.GetTopRect: TRectF;
begin
  result := PictureDockToolToScreenRect(FViewDockTop);
end;

function TDockTool.PictureDockToolToScreenRect(
  APictureDockTool: TPictureDockTool): TRectF;
begin
  result.Create(self.LocalToScreen(APictureDockTool.Position.Point));
  result.Size := TSizeF.Create(APictureDockTool.Width, APictureDockTool.Height);
end;

procedure TDockTool.SetActiveElement(const Value: TDockElement);
begin
  FActiveElement := Value;

  FViewDockTop.IsActive     := (ActiveElement in [TDockElement.Top])    and (ActiveElement in FDocks);
  FViewDockLeft.IsActive    := (ActiveElement in [TDockElement.Left])   and (ActiveElement in FDocks);
  FViewDockBottom.IsActive  := (ActiveElement in [TDockElement.Bottom]) and (ActiveElement in FDocks);
  FViewDockRight.IsActive   := (ActiveElement in [TDockElement.Right])  and (ActiveElement in FDocks);
  FViewDockClient.IsActive  := (ActiveElement in [TDockElement.Client]) and (ActiveElement in FDocks);
end;

procedure TDockTool.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
  FBackground.Fill.Color := FBackgroundColor;
end;

procedure TDockTool.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;

  FViewDockTop.Color    := Color;
  FViewDockBottom.Color := Color;
  FViewDockLeft.Color   := Color;
  FViewDockRight.Color  := Color;
  FViewDockClient.Color := Color;
end;

procedure TDockTool.SetPictures;
begin
  FGridPanel.ControlCollection.AddControl(FViewDockTop,    1, 0);
  FGridPanel.ControlCollection.AddControl(FViewDockLeft,   0, 1);
  FGridPanel.ControlCollection.AddControl(FViewDockBottom, 1, 2);
  FGridPanel.ControlCollection.AddControl(FViewDockRight,  2, 1);
  FGridPanel.ControlCollection.AddControl(FViewDockClient, 1, 1);

  FViewDockTop.Parent     := FGridPanel;
  FViewDockLeft.Parent    := FGridPanel;
  FViewDockBottom.Parent  := FGridPanel;
  FViewDockRight.Parent   := FGridPanel;
  FViewDockClient.Parent  := FGridPanel;
end;

{$ENDREGION}

{$REGION 'TPictureDockTool'}

constructor TPictureDockTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  self.RowCollection.ClearAndResetID;
  self.ColumnCollection.ClearAndResetID;
  self.Align := TAlignLayout.Client;

  self.Margins.Top     := 5;
  self.Margins.Left    := 5;
  self.Margins.Bottom  := 5;
  self.Margins.Right   := 5;

  FPicture := TRectangle.Create(self);
  FPicture.Align := TAlignLayout.Client;

  FSpace := TRectangle.Create(self);
  FSpace.Fill.Color := TAlphaColorRec.Alpha;
  FSpace.Align      := TAlignLayout.Client;

  IsActive := false;
end;

destructor TPictureDockTool.Destroy;
begin

  inherited Destroy;
end;

function TPictureDockTool.GetColor: TAlphaColor;
begin
  result := FColor;
end;

function TPictureDockTool.GetIsActive: boolean;
begin
  result := FIsActive;
end;

procedure TPictureDockTool.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
end;

procedure TPictureDockTool.SetIsActive(const Value: boolean);
begin
  FIsActive := Value;

  if IsActive
    then FPicture.Fill.Color := Color
    else FPicture.Fill.Color := TAlphaColorRec.Alpha;
end;

{$ENDREGION}

{$REGION 'TPictureTop'}

constructor TPictureTop.Create(AOwner: TComponent);
var
  Row: TRowItem;
begin
  inherited Create(AOwner);

  self.ColumnCollection.Add;

  Row := self.RowCollection.Add;
  Row.SizeStyle := TSizeStyle.Percent;
  Row.Value     := 30.00;

  Row := self.RowCollection.Add;
  Row.SizeStyle := TSizeStyle.Percent;
  Row.Value     := 70.00;

  Self.ControlCollection.AddControl(Picture, 0, 0);
  Self.ControlCollection.AddControl(Space, 0, 1);

  Picture.Parent := self;
  Space.Parent   := self;

  Rearrange(self, [100], [30, 70]);
end;

destructor TPictureTop.Destroy;
begin

  inherited Destroy;
end;

{$ENDREGION}

{$REGION 'TPictureLeft'}

constructor TPictureLeft.Create(AOwner: TComponent);
var
  Col: TColumnItem;
begin
  inherited Create(AOwner);

  self.RowCollection.Add;

  Col := self.ColumnCollection.Add;
  Col.SizeStyle := TSizeStyle.Percent;
  Col.Value     := 30.00;

  Col := self.ColumnCollection.Add;
  Col.SizeStyle := TSizeStyle.Percent;
  Col.Value     := 70.00;

  Self.ControlCollection.AddControl(Picture, 0, 0);
  Self.ControlCollection.AddControl(Space, 1, 0);

  Picture.Parent := self;
  Space.Parent   := self;

  Rearrange(self, [30, 70], [100]);
end;

destructor TPictureLeft.Destroy;
begin

  inherited Destroy;
end;

{$ENDREGION}

{$REGION 'TPictureButtom'}

constructor TPictureBottom.Create(AOwner: TComponent);
var
  Row: TRowItem;
begin
  inherited Create(AOwner);

  self.ColumnCollection.Add;

  Row := self.RowCollection.Add;
  Row.SizeStyle := TSizeStyle.Percent;
  Row.Value     := 70.00;

  Row := self.RowCollection.Add;
  Row.SizeStyle := TSizeStyle.Percent;
  Row.Value     := 30.00;

  Self.ControlCollection.AddControl(Space, 0, 0);
  Self.ControlCollection.AddControl(Picture, 0, 1);

  Picture.Parent := self;
  Space.Parent   := self;

  Rearrange(self, [100], [70, 30]);
end;

destructor TPictureBottom.Destroy;
begin

  inherited Destroy;
end;

{$ENDREGION}

{$REGION 'TPictureRight'}

constructor TPictureRight.Create(AOwner: TComponent);
var
  Col: TColumnItem;
begin
  inherited Create(AOwner);

  self.RowCollection.Add;

  Col := self.ColumnCollection.Add;
  Col.SizeStyle := TSizeStyle.Percent;
  Col.Value     := 70.00;

  Col := self.ColumnCollection.Add;
  Col.SizeStyle := TSizeStyle.Percent;
  Col.Value     := 30.00;

  Self.ControlCollection.AddControl(Space, 0, 0);
  Self.ControlCollection.AddControl(Picture, 1, 0);

  Picture.Parent := self;
  Space.Parent   := self;

  Rearrange(self, [70, 30], [100]);
end;

destructor TPictureRight.Destroy;
begin

  inherited Destroy;
end;

{$ENDREGION}

{$REGION 'TPictureClient'}

constructor TPictureClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  self.RowCollection.Add;
  self.ColumnCollection.Add;

  Self.ControlCollection.AddControl(Picture, 0, 0);

  Picture.Parent := self;
  Space.Parent   := self;

  Rearrange(self, [100], [100]);
end;

destructor TPictureClient.Destroy;
begin

  inherited Destroy;
end;

{$ENDREGION}

{$REGION 'TDockContents'}

procedure TDockContents.AddContent(const DockElement: TDockElement;
  Form: TForm);
begin
  case DockElement of
    TDockElement.none: ;
    TDockElement.Top:     FDockTop.DoDockTab(Form);
    TDockElement.Left:    FDockLeft.DoDockTab(Form);
    TDockElement.Bottom:  FDockBottom.DoDockTab(Form);
    TDockElement.Right:   FDockRight.DoDockTab(Form);
    TDockElement.Client:  FDockClient.DoDockTab(Form);
  end;
end;

constructor TDockContents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDockTop := TDockContent.Create(self);
  FDockTop.DockElement := TDockElement.Top;
  FDockTop.Stored  := false;
  FDockTop.Visible := false;
  FDockTop.Align   := TAlignLayout.MostTop;
  FDockTop.Parent  := self;
  FDockTop.OnDock  := DoOnDock;

  FSplitterTop := TSplitter.Create(self);
  FSplitterTop.Stored  := false;
  FSplitterTop.Visible := false;
  FSplitterTop.Align   := TAlignLayout.Top;
  FSplitterTop.Parent  := self;

  FDockLeft := TDockContent.Create(self);
  FDockLeft.DockElement := TDockElement.Left;
  FDockLeft.Stored  := false;
  FDockLeft.Visible := false;
  FDockLeft.Align   := TAlignLayout.Left;
  FDockLeft.Parent  := self;
  FDockLeft.OnDock  := DoOnDock;

  FSplitterLeft := TSplitter.Create(self);
  FSplitterLeft.Stored  := false;
  FSplitterLeft.Visible := false;
  FSplitterLeft.Align   := TAlignLayout.Left;
  FSplitterLeft.Parent  := self;

  FDockBottom := TDockContent.Create(self);
  FDockBottom.DockElement := TDockElement.Bottom;
  FDockBottom.Stored  := false;
  FDockBottom.Visible := false;
  FDockBottom.Align   := TAlignLayout.MostBottom;
  FDockBottom.Parent  := self;
  FDockBottom.OnDock  := DoOnDock;

  FSplitterBottom := TSplitter.Create(self);
  FSplitterBottom.Stored  := false;
  FSplitterBottom.Visible := false;
  FSplitterBottom.Align   := TAlignLayout.Bottom;
  FSplitterBottom.Parent  := self;

  FDockRight := TDockContent.Create(self);
  FDockRight.DockElement := TDockElement.Right;
  FDockRight.Stored  := false;
  FDockRight.Visible := false;
  FDockRight.Align   := TAlignLayout.Right;
  FDockRight.Parent  := self;
  FDockRight.OnDock  := DoOnDock;

  FSplitterRight := TSplitter.Create(self);
  FSplitterRight.Stored  := false;
  FSplitterRight.Visible := false;
  FSplitterRight.Align   := TAlignLayout.Right;
  FSplitterRight.Parent  := self;

  FDockClient := TDockContent.Create(self);
  FDockClient.DockElement := TDockElement.Client;
  FDockClient.Stored  := false;
  FDockClient.Visible := false;
  FDockClient.Align   := TAlignLayout.Client;
  FDockClient.Parent  := self;
  FDockClient.OnDock  := DoOnDock;

  FPreview := TRectangle.Create(self);
  FPreview.Align := TAlignLayout.Contents;
  FPreview.Margins.Top    := 5;
  FPreview.Margins.Left   := 5;
  FPreview.Margins.Bottom := 5;
  FPreview.Margins.Right  := 5;
  FPreview.Opacity        := 0.3;
  FPreview.Stored         := false;
end;

destructor TDockContents.Destroy;
begin

  inherited;
end;

procedure TDockContents.DoOnDock(Sender: TObject);
var
  DockContent: TDockContent;
begin
  if not (Sender is TDockContent) then exit;
  DockContent := Sender as TDockContent;

  if DockContent.Count = 0
    then DockContent.Visible := false
    else DockContent.Visible := true;

    case DockContent.DockElement of
      TDockElement.Top:    FSplitterTop.Visible    := DockContent.Visible;
      TDockElement.Left:   FSplitterLeft.Visible   := DockContent.Visible;
      TDockElement.Bottom: FSplitterBottom.Visible := DockContent.Visible;
      TDockElement.Right:  FSplitterRight.Visible  := DockContent.Visible;
    end;
end;

function TDockContents.GetDockBottomSize: single;
begin
  result := FDockBottom.Height;
end;

function TDockContents.GetDockLeftSize: single;
begin
  result := FDockLeft.Width;
end;

function TDockContents.GetDockRightSize: single;
begin
  result := FDockRight.Width;
end;

function TDockContents.GetDockTopSize: single;
begin
  result := FDockTop.Height;
end;

function TDockContents.GetPreviewColor: TAlphaColor;
begin
  result := FPreview.Fill.Color;
end;

procedure TDockContents.SetDockBottomSize(const Value: single);
begin
  FDockBottom.Height := Value;
end;

procedure TDockContents.SetDockLeftSize(const Value: single);
begin
  FDockLeft.Width := Value;
end;

procedure TDockContents.SetDockRightSize(const Value: single);
begin
  FDockRight.Width := Value;
end;

procedure TDockContents.SetDockTopSize(const Value: single);
begin
  FDockTop.Height := Value;
end;

procedure TDockContents.SetPreviewColor(const Value: TAlphaColor);
begin
  FPreview.Fill.Color := Value;
end;

procedure TDockContents.SetPreviewState(const Value: TDockElement);
begin
  FPreviewState := Value;
end;

{$ENDREGION}

{ TDockTabItem }

procedure TDockTabItemClose.ApplyStyle;
const
  ButtonClose = 'ButtonClose';
var
  Base: TFmxObject;
  B: TFmxObject;
begin
  inherited;
   B := ResourceLink.FindStyleResource('top');
   if B is TControl then begin
     FItemStyle := TControl(B);
     FItemStyle.Visible := True;

     Base := B.FindStyleResource(ButtonClose);
     TButton(Base).OnClick := DoOnCloseClick;

     B := ResourceLink.FindStyleResource('bottom');
     if B is TControl then
       TControl(B).Visible := False;
   end;
end;

procedure TDockTabItemClose.DoOnCloseClick(Sender: TObject);
begin
  if assigned(FOnClose) then
    FOnClose(self);
end;

procedure TDockTabItemClose.FreeStyle;
begin
  FItemStyle := nil;
  inherited;
end;

function TDockTabItemClose.GetStyleObject: TFmxObject;
const
  Style = 'TabItemCloseStyle';
var
  Base: TFmxObject;
begin
  if StyleLookup.IsEmpty then begin

    Base := TStyleStreaming.LoadFromResource(HInstance,
      Style, RT_RCDATA);

    if assigned(Base) then
      exit(Base);
  end;

  result := inherited GetStyleObject;
end;

end.
