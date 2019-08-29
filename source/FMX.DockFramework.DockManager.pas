unit FMX.DockFramework.DockManager;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Layouts, FMX.StdCtrls,
  FMX.Objects, System.UITypes, System.Messaging, System.Types, FMX.Forms, FMX.TabControl,
  FMX.ListBox, FMX.Effects,

  FMX.DockFramework.FDockToolForm,
  FMX.DockFramework.DockTypes,
  FMX.DockFramework.DockMessages,
  FMX.DockFramework.DockProvider, FMX.Styles, System.Generics.Collections
  ;

type
  TOnDockEvent = procedure(Sender: TObject; APosition: TDockPosition; AForm: TForm; isFormClose: boolean = false) of object;

  {$REGION 'Dock Tool'}

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
    FActiveElement: TDockPosition;
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

    procedure SetActiveElement(const Value: TDockPosition);
    procedure SetColor(const Value: TAlphaColor);
    function GetBottomRect: TRectF;
    function GetClientRect: TRectF;
    function GetLeftRect: TRectF;
    function GetRightRect: TRectF;
    function GetTopRect: TRectF;

    function PictureDockToolToScreenRect(APictureDockTool: TPictureDockTool): TRectF;

    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetDocks(const Value: TDocks);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ActiveElement: TDockPosition read FActiveElement write SetActiveElement;
    property Color: TAlphaColor read FColor write SetColor;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;

    property TopRect: TRectF read GetTopRect;
    property LeftRect: TRectF read GetLeftRect;
    property BottomRect: TRectF read GetBottomRect;
    property RightRect: TRectF read GetRightRect;
    property ClientRect: TRectF read GetClientRect;

    property Docks: TDocks read FDocks write SetDocks;
  end;

  {$ENDREGION}


  {$REGION 'TabControl'}

  TDockTabItemClose = class(TTabItem)
  private
    FItemStyle: TControl;
    FOnClose: TNotifyEvent;
    procedure DoOnCloseClick(Sender: TObject);
  protected
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
  public
    property OnCloseClick: TNotifyEvent read FOnClose write FOnClose;
  end;

  TInnerTabControl = class(TTabControl)
  private
    FOnUnpdateTabbarControls: TNotifyEvent;
  protected
    InnerTabBarControls: TTabControl.TArrayTabBarControls;
    procedure DoUpdateTabBarButtons(const TabBarButtons: TTabControl.TTabBarButtons; const TabPosition: TTabPosition;
      const Content: TContent; var TabBarControls: TTabControl.TArrayTabBarControls); override;
    property OnUpdateTabBar: TNotifyEvent read FOnUnpdateTabbarControls write FOnUnpdateTabbarControls;
  end;

  {$ENDREGION}

  {$REGION 'Dock Contents'}

  TDockContent = class(TLayout)
  const
    DragPoints = 30;
  private type
    TTabItemLink = record
      TabItem: TTabItem;
      Form: TForm;
      Container: TFMXObject;
      constructor New(ATabItem: TTabItem; AForm: TForm; AContainer: TFmxObject);
    end;
  private
    FDockTab: TInnerTabControl;
    FPoupUpTabs: TPopup;
    FListBoxTabs: TListBox;
    FDockElement: TDockPosition;
    FisTabItemDrag: boolean;
    FTabItemDragPosition: TPointF;
    FTabItemWidth: single;
    FTabItemHeight: single;
    FTabItemStyleLookup: string;
    FOnDock:   TOnDockEvent;
    FOnUnDock: TOnDockEvent;

    FTabItemLinks: TDictionary<TTabItem, TTabItemLink>;
    FListBoxItemLinks: TDictionary<TListBoxItem, TTabItem>;

    procedure TabItemMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure TabItemMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure TabItemMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure TabItemCloseClick(Sender: TObject);
    procedure TabBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ListBoxItemTabClick(Sender: TObject);
    procedure DockTabUpdateTabBar(Sender: TObject);

    procedure ControlsToForm(Tabitem: TTabItem; Form: TForm; Container: TFMXObject);
    procedure ControlsToDock(TabItem: TTabitem; Form: TForm; Container: TFMXObject);

    function AddTabItem(Form: TForm; Container: TFMXObject): TTabItem;
    procedure DeleteTabItem(TabItem: TTabItem);
    function GetCount: integer;
    procedure SetTabItemStyleLookup(const Value: string);
  protected
    procedure DockTab(Form: TForm; Container: TFMXObject);
    procedure UnDockTab(TabItem: TTabItem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DockElement: TDockPosition read FDockElement write FDockElement;
    property Count: integer read GetCount;

    property OnDock: TOnDockEvent read FOnDock write FOnDock;
    property OnUnDock: TOnDockEvent read FOnUnDock write FOnUnDock;

    property TabItemWidth: single read FTabItemWidth write FTabItemWidth;
    property TabItemHeight: single read FTabItemHeight write FTabItemHeight;
    property TabItemStyleLookup: string read FTabItemStyleLookup write SetTabItemStyleLookup;
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
    FPreview:      TRectangle;
    FPreviewState: TDockPosition;
    FTabItemWidth: single;
    FTabItemHeight: single;
    FTabItemStyleLookup: string;
    FOnDock:   TOnDockEvent;
    FOnUnDock: TOnDockEvent;
    procedure DoOnDock(Sender: TObject; APosition: TDockPosition; AForm: TForm; isFormClose: boolean = false);
    procedure DoOnUnDock(Sender: TObject; APosition: TDockPosition; AForm: TForm; isFormClose: boolean = false);

    function GetPreviewColor: TAlphaColor;
    function GetDockBottomSize: single;
    function GetDockLeftSize: single;
    function GetDockRightSize: single;
    function GetDockTopSize: single;

    procedure SetDockBottomSize(const Value: single);
    procedure SetDockLeftSize(const Value: single);
    procedure SetDockRightSize(const Value: single);
    procedure SetDockTopSize(const Value: single);
    procedure SetPreviewState(const Value: TDockPosition);
    procedure SetPreviewColor(const Value: TAlphaColor);

    procedure SetTabItemHeight(const Value: single);
    procedure SetTabItemWidth(const Value: single);
    function GetCount: integer;
    procedure SetTabItemStyleLookup(const Value: string);
  protected
    function DockContentByPosition(ADockPosition: TDockPosition): TDockContent;
    procedure UpdateVisibleContent(ADockContent: TDockContent);
    procedure AddContent(const DockElement: TDockPosition; Form: TForm; Container: TFmxObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Count: integer read GetCount;

    property Preview: TDockPosition read FPreviewState write SetPreviewState;
    property PreviewColor: TAlphaColor read GetPreviewColor write SetPreviewColor;
    property DockTopSize: single read GetDockTopSize write SetDockTopSize;
    property DockLeftSize: single read GetDockLeftSize write SetDockLeftSize;
    property DockBottomSize: single read GetDockBottomSize write SetDockBottomSize;
    property DockRightSize: single read GetDockRightSize write SetDockRightSize;

    property TabItemWidth: single read FTabItemWidth write SetTabItemWidth;
    property TabItemHeight: single read FTabItemHeight write SetTabItemHeight;
    property TabItemStyleLookup: string read FTabItemStyleLookup write SetTabItemStyleLookup;

    property OnDock: TOnDockEvent read FOnDock write FOnDock;
    property OnUnDock: TOnDockEvent read FOnUnDock write FOnUnDock;
  end;

  {$ENDREGION}

  TCustomDockManager = class(TLayout)
  private
    FContents: TDockContents;
    FDockToolForm:   TFDockToolForm;
    FDockToolLayout: TLayout;
    FDockTool: TDockTool;
    FOnDock: TOnDockEvent;
    FOnUnDock: TOnDockEvent;

    FActiveForm: TForm;
    FForm: TForm;
    FRealFocusChangedEvent: TNotifyEvent;
    FisInit: boolean;

    procedure FocusChanged(Sender: TObject);
    procedure UpdateActiveFormInfo;

    procedure ContentsDock(Sender: TObject; APosition: TDockPosition; AForm: TForm; isFormClose: boolean = false);
    procedure ContentsUnDock(Sender: TObject; APosition: TDockPosition; AForm: TForm; isFormClose: boolean = false);

    procedure SubscribeDockMessageMoving;
    procedure SubscribeDockMessageMoved;
    procedure SubscribeDockMessageDock;
    procedure SubscribeFormActivate;

    function GetDockToolSize: single;
    function GetVisibleDockTool: boolean;
    function GetActiveElement: TDockPosition;
    function GetDocToolColor: TAlphaColor;
    function GetDockToolBackgroundColor: TAlphaColor;
    function GetDockBottomSize: single;
    function GetDockLeftSize: single;
    function GetDockRightSize: single;
    function GetDockTopSize: single;
    function GetTabItemHeight: single;
    function GetTabItemWidth: single;
    function GetTabItemStyleLookuo: string;
    function GetActiveForm: TForm;

    procedure SetDockToolSize(const Value: single);
    procedure SetVisibleDockTool(const Value: boolean);
    procedure SetActiveElement(const Value: TDockPosition);
    procedure SetDockToolColor(const Value: TAlphaColor);
    procedure SetDockToolBackgroundColor(const Value: TAlphaColor);
    procedure SetDockBottomSize(const Value: single);
    procedure SetDockLeftSize(const Value: single);
    procedure SetDockRightSize(const Value: single);
    procedure SetDockTopSize(const Value: single);
    procedure SetTabItemHeight(const Value: single);
    procedure SetTabItemWidth(const Value: single);
    procedure SetTabItemStyleLookup(const Value: string);
  protected
    procedure Moving(const X, Y: single; const Docks: TDocks);
    procedure Moved(const X, Y: single; const Docks: TDocks; Form: TForm; Container: TFmxObject);
    procedure Dock(const SelectedDock: TDockPosition; Form: TForm; Container: TFmxObject);
    procedure ShowPreview(const X, Y: single);

    property DockToolVisible: boolean read GetVisibleDockTool write SetVisibleDockTool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ActiveForm: TForm read GetActiveForm;
  published
    property DockToolSize: single read GetDockToolSize write SetDockToolSize;
    property DockToolColor: TAlphaColor read GetDocToolColor write SetDockToolColor;
    property DockToolBackgroundColor: TAlphaColor read GetDockToolBackgroundColor write SetDockToolBackgroundColor;
    property DockToolElement: TDockPosition read GetActiveElement write SetActiveElement;
    property DockTopSize: single read GetDockTopSize write SetDockTopSize;
    property DockLeftSize: single read GetDockLeftSize write SetDockLeftSize;
    property DockBottomSize: single read GetDockBottomSize write SetDockBottomSize;
    property DockRightSize: single read GetDockRightSize write SetDockRightSize;
    property TabItemWidth: single read GetTabItemWidth write SetTabItemWidth;
    property TabItemHeight: single read GetTabItemHeight write SetTabItemHeight;
    property TabItemStyleLookup: string read GetTabItemStyleLookuo write SetTabItemStyleLookup;
    property OnDock: TOnDockEvent read FOnDock write FOnDock;
    property OnUnDock: TOnDockEvent read FOnUnDock write FOnUnDock;
  end;

  TDockManager = class(TCustomDockManager)
  published
    property DockToolSize;
    property DockToolColor;// default TAlphaColorRec.Springgreen;
    property DockToolBackgroundColor;// default TAlphaColorRec.Slategray;
    property DockToolElement;// default TDockPosition.none;
    property DockTopSize;
    property DockLeftSize;
    property DockBottomSize;
    property DockRightSize;
    property TabItemWidth;
    property TabItemHeight;
    property TabItemStyleLookup;
    property OnDock;
    property OnUnDock;
  end;

procedure Register;

implementation

uses
  FMX.Graphics, FMX.Dialogs, System.Math;

procedure Register;
begin
  RegisterComponents('Dock Framework', [TDockManager]);
end;

{$REGION 'Rearrange GridPanelLayout'}

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

{$ENDREGION}

{$REGION 'TCustomDockManger'}

procedure TCustomDockManager.ContentsDock(Sender: TObject; APosition: TDockPosition; AForm: TForm;isFormClose: boolean = false);
begin
  FActiveForm := AForm;

  if assigned(FOnDock) then
    FOnDock(Sender, APosition, AForm, isFormClose);
end;

procedure TCustomDockManager.ContentsUnDock(Sender: TObject; APosition: TDockPosition; AForm: TForm; isFormClose: boolean = false);
begin
  if FContents.Count = 0 then
    FActiveForm := nil;

  if assigned(FOnUnDock) then
    FOnUnDock(Sender, APosition, AForm, isFormClose);
end;

constructor TCustomDockManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FisInit := false;
  if AOwner is TForm
    then FForm := AOwner as TForm
    else FForm := nil;
  FActiveForm := nil;

  self.BeginUpdate;
  try
    FContents := TDockContents.Create(self);
    FContents.Stored    := false;
    FContents.Align     := TAlignLayout.Client;
    FContents.OnDock    := ContentsDock;
    FContents.OnUnDock  := ContentsUnDock;
    FContents.TabItemStyleLookup := string.Empty;
    FContents.Parent    := self;

    FDockToolLayout := TLayout.Create(self);
    FDockToolLayout.Stored  := false;
    FDockToolLayout.Align   := TAlignLayout.Contents;
    FDockToolLayout.HitTest := false;
    FDockToolLayout.Parent  := self;

    FDockTool := TDockTool.Create(self);
    FDockTool.Stored  := false;
    FDockTool.Align   := TAlignLayout.Center;
    FDockTool.Parent  := FDockToolLayout;
    DockToolSize := 100;

    if (not (csDesigning in ComponentState)) and (not (csDesignInstance in ComponentState)) then begin
      Application.CreateForm(TFDockToolForm, FDockToolForm);
      FDockToolForm.Stored       := false;
      FDockToolForm.Transparency := true;
      FDockToolForm.FormStyle    := TFormStyle.StayOnTop;
      FDockToolForm.Visible      := true;
      FDockToolForm.Show;
      FDockTool.Parent           := self.FDockToolForm;
      FDockTool.Opacity          := 0.5;
    end;

    SubscribeDockMessageMoving;
    SubscribeDockMessageMoved;
    SubscribeDockMessageDock;
    SubscribeFormActivate;

    if (csDesigning in ComponentState) or (csDesignInstance in ComponentState)
      then DockToolVisible := true
      else DockToolVisible := false;

  finally
    self.EndUpdate;
  end;
end;

destructor TCustomDockManager.Destroy;
begin

  inherited Destroy;
end;

procedure TCustomDockManager.Dock(const SelectedDock: TDockPosition; Form: TForm; Container: TFMXObject);
begin
  self.BeginUpdate;
  try
    FContents.AddContent(SelectedDock, Form, Container);
  finally
    self.EndUpdate;
  end;
end;

procedure TCustomDockManager.FocusChanged(Sender: TObject);
begin
  UpdateActiveFormInfo;

  if Assigned(FRealFocusChangedEvent) then
    FRealFocusChangedEvent(Sender);
end;

procedure TCustomDockManager.Moving(const X, Y: single; const Docks: TDocks);
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

function TCustomDockManager.GetActiveElement: TDockPosition;
begin
  result := FDockTool.ActiveElement;
end;

function TCustomDockManager.GetActiveForm: TForm;
begin
  result := FActiveForm;
end;

procedure TCustomDockManager.UpdateActiveFormInfo;

  procedure GetTabItem(AControl: TFmxObject; var ATabItem: TTabItem);
  begin
    if AControl = nil then exit;

    if AControl is TDockTabItemClose then begin
      ATabItem := AControl as TDockTabItemClose;
      exit;
    end else begin
      if AControl.Parent = nil then exit;
      GetTabItem(AControl.Parent, ATabItem);
    end;
  end;

var
  ActiveControl: TControl;
  ActiveTabItem: TTabItem;
  Link: TDockContent.TTabItemLink;
begin
  ActiveTabItem := nil;
  if FForm = nil then exit;

  ActiveControl := FForm.Focused as TControl;
  if ActiveControl = nil then exit;

  GetTabItem(ActiveControl, ActiveTabItem);
  if ActiveTabItem = nil then exit;

  if FContents.FDockClient.FTabItemLinks.TryGetValue(ActiveTabItem, Link) then begin
    FActiveForm := Link.Form;
    exit;
  end;

  if FContents.FDockTop.FTabItemLinks.TryGetValue(ActiveTabItem, Link) then begin
    FActiveForm := Link.Form;
    exit;
  end;
  if FContents.FDockRight.FTabItemLinks.TryGetValue(ActiveTabItem, Link) then begin
    FActiveForm := Link.Form;
    exit;
  end;
  if FContents.FDockBottom.FTabItemLinks.TryGetValue(ActiveTabItem, Link) then begin
    FActiveForm := Link.Form;
    exit;
  end;
  if FContents.FDockLeft.FTabItemLinks.TryGetValue(ActiveTabItem, Link) then begin
    FActiveForm := Link.Form;
    exit;
  end;
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

function TCustomDockManager.GetTabItemHeight: single;
begin
  result := FContents.TabitemHeight;
end;

function TCustomDockManager.GetTabItemStyleLookuo: string;
begin
  result := FContents.TabItemStyleLookup;
end;

function TCustomDockManager.GetTabItemWidth: single;
begin
  result := FContents.TabItemWidth;
end;

function TCustomDockManager.GetVisibleDockTool: boolean;
begin
  result := FDockTool.Visible;
end;

procedure TCustomDockManager.SetActiveElement(const Value: TDockPosition);
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

procedure TCustomDockManager.SetTabItemHeight(const Value: single);
begin
  FContents.TabItemHeight := Value;
end;

procedure TCustomDockManager.SetTabItemStyleLookup(const Value: string);
begin
  FContents.TabItemStyleLookup := Value;
end;

procedure TCustomDockManager.SetTabItemWidth(const Value: single);
begin
  FContents.TabItemWidth := Value;
end;

procedure TCustomDockManager.ShowPreview(const X, Y: single);
var
  MousePoint: TPointF;
begin
  MousePoint := TPointF.Create(X, Y);
  if FDockTool.GetBottomRect.Contains(MousePoint) and (TDockPosition.Bottom in FDockTool.Docks) then begin
    DockToolElement := TDockPosition.Bottom;
    exit;
  end;

  if FDockTool.GetClientRect.Contains(MousePoint) and (TDockPosition.Client in FDockTool.Docks) then begin
    DockToolElement := TDockPosition.Client;
    exit;
  end;

  if FDockTool.GetLeftRect.Contains(MousePoint) and (TDockPosition.Left in FDockTool.Docks) then begin
    DockToolElement := TDockPosition.Left;
    exit;
  end;

  if FDockTool.GetRightRect.Contains(MousePoint) and (TDockPosition.Right in FDockTool.Docks) then begin
    DockToolElement := TDockPosition.Right;
    exit;
  end;

  if FDockTool.GetTopRect.Contains(MousePoint) and (TDockPosition.Top in FDockTool.Docks) then begin
    DockToolElement := TDockPosition.Top;
    exit;
  end;

  DockToolElement := TDockPosition.none;
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
    Provider: TDockProvider;
  begin
    if not (Sender is TDockProvider) then exit;

    Provider := Sender as TDockProvider;
    Value := (M as TDockMessageDock).Value;
    if Value.Dock in Value.AccessDocks then
      Dock(Value.Dock, Provider.Form, Provider.Container);
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
    Provider: TDockProvider;
  begin
    if not (Sender is TDockProvider) then exit;

    Provider := Sender as TDockProvider;
    Value := (M as TDockMessageMoved).Value;
    Moved(Value.MousePosition.X, Value.MousePosition.Y, Value.AccessDocks, Provider.Form, Provider.Container);
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
    Moving(Value.MousePosition.X, Value.MousePosition.Y, Value.AccessDocks);
  end
  );
end;

procedure TCustomDockManager.SubscribeFormActivate;
var
  MessageManager: TMessageManager;
begin
  MessageManager := TMessageManager.DefaultManager;

  MessageManager.SubscribeToMessage(TFormActivateMessage, procedure(const Sender: TObject; const M: TMessage)
  var
    Form: TCommonCustomForm;
    EventReal, Event: TNotifyEvent;
  begin
    Form := TFormActivateMessage(M).Value;
    if not (FForm = Form) then exit;

    EventReal := Form.OnFocusChanged;
    Event     := FocusChanged;
    if @EventReal <> @Event then begin
      FRealFocusChangedEvent := Form.OnFocusChanged;
      Form.OnFocusChanged    := FocusChanged;
    end;

    FisInit := true;
  end
  );
end;

procedure TCustomDockManager.Moved(const X, Y: single; const Docks: TDocks; Form: TForm; Container: TFMXObject);
var
  MousePoint: TPointF;
  SelelectedDock: TDockPosition;
begin
  self.FDockTool.Docks := Docks;

  if self.DockToolVisible = false then exit;


  MousePoint := TPointF.Create(X, Y);
  if FDockTool.GetBottomRect.Contains(MousePoint) and (TDockPosition.Bottom in FDockTool.Docks) then begin
    SelelectedDock := TDockPosition.Bottom;
  end else
  if FDockTool.GetClientRect.Contains(MousePoint) and (TDockPosition.Client in FDockTool.Docks) then begin
    SelelectedDock := TDockPosition.Client;
  end else
  if FDockTool.GetLeftRect.Contains(MousePoint) and (TDockPosition.Left in FDockTool.Docks) then begin
    SelelectedDock := TDockPosition.Left;
  end else
  if FDockTool.GetRightRect.Contains(MousePoint) and (TDockPosition.Right in FDockTool.Docks) then begin
    SelelectedDock := TDockPosition.Right;
  end else
  if FDockTool.GetTopRect.Contains(MousePoint) and (TDockPosition.Top in FDockTool.Docks) then begin
    SelelectedDock := TDockPosition.Top;
  end else
    SelelectedDock := TDockPosition.none;

  DockToolElement := TDockPosition.none;
  Dock(SelelectedDock, Form, Container);
  self.DockToolVisible := false;
end;

{$ENDREGION}

{$REGION 'TDockContent'}

procedure TDockContent.TabItemMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
  if not FisTabItemDrag then exit;

  if (abs(FTabItemDragPosition.X-X) > DragPoints)
  then
    UnDockTab(Sender as TTabItem);
end;

function TDockContent.AddTabItem(Form: TForm; Container: TFMXObject): TTabItem;
var
  TabItem: TDockTabItemClose;
  ListItem: TListBoxItem;
begin
  TabItem := FDockTab.Add(TDockTabItemClose) as TDockTabItemClose;
  TabItem.BeginUpdate;
  try
    TabItem.Text          := Form.Caption;
    TabItem.AutoSize      := false;
    TabItem.StyleLookup   := FTabItemStyleLookup;
    TabItem.Width         := FTabItemWidth;
    TabItem.Height        := FTabItemHeight;
    TabItem.OnMouseDown   := TabItemMouseDown;
    TabItem.OnMouseUp     := TabItemMouseUp;
    TabItem.OnMouseMove   := TabItemMouseMove;
    TabItem.OnCloseClick  := TabItemCloseClick;

    FTabItemLinks.AddOrSetValue(TabItem, TTabItemLink.New(TabItem, Form, Container));

    FListBoxTabs.BeginUpdate;
    try
      ListItem := TListBoxItem.Create(FListBoxTabs);
      ListItem.Text := TabItem.Text;
      ListItem.OnClick := ListBoxItemTabClick;
      FListBoxTabs.AddObject(ListItem);

      FListBoxItemLinks.AddorSetValue(ListItem, TabItem);
    finally
      FListBoxTabs.EndUpdate;
    end;

    ControlsToDock(TabItem, Form, Container);
    result := TabItem;
  finally
    TabItem.EndUpdate;
  end;
end;

procedure TDockContent.ControlsToDock(TabItem: TTabitem; Form: TForm; Container: TFMXObject);
var
  FMXObject: TFMXObject;
begin
  Form.BeginUpdate;
  try
    if Container = nil then begin
      for FmxObject in Form.Children.ToArray do
        FmxObject.Parent := TabItem;
    end else begin
      for FmxObject in Container.Children.ToArray do
        FmxObject.Parent := TabItem;
    end;
  finally
    Form.EndUpdate;
  end;
end;

procedure TDockContent.ControlsToForm(Tabitem: TTabItem; Form: TForm; Container: TFMXObject);
var
  Content,
  FmxObjectForm: TFMXObject;
begin
  Form.BeginUpdate;
  try
    for Content in TabItem.Children.ToArray do begin
      if not (Content.Name = 'TabItemContent_') then
        continue;

      if Container = nil then begin
        for FmxObjectForm in Content.Children.ToArray do
          Form.AddObject(FmxObjectForm);
        end else begin
          for FmxObjectForm in Content.Children.ToArray do
            Container.AddObject(FmxObjectForm);
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

  FTabItemLinks     := TDictionary<TTabItem, TTabItemLink>.Create;
  FListBoxItemLinks := TDictionary<TListBoxItem, TTabItem>.Create;

  FDockTab := TInnerTabControl.Create(self);
  FDockTab.Stored      := false;
  FDockTab.Align       := TAlignLayout.Client;
  FDockTab.TabPosition := TTabPosition.Top;
  FDockTab.TabHeight   := 26;
  FDockTab.Visible     := true;
  FDockTab.OnUpdateTabBar := DockTabUpdateTabBar;
  FDockTab.Parent      := self;

  FPoupUpTabs := TPopup.Create(self);
  FPoupUpTabs.Stored         := false;
  FPoupUpTabs.DragWithParent := false;

  FListBoxTabs := TListBox.Create(self);
  FListBoxTabs.Stored := false;
  FListBoxTabs.Align  := TAlignLayout.Client;
  FListBoxTabs.Parent := FPoupUpTabs;
end;

procedure TDockContent.DeleteTabItem(TabItem: TTabItem);
var
  TabControl: TTabControl;
  Idx: integer;
begin
  TabControl := TabItem.TabControl;
  TabControl.ActiveTab := nil;

  {TODO: ”далить линк из FListBoxItemLinks}
  idx := TabItem.Index;
  TabItem.Parent := nil;
  TabItem.Free;

  TabControl.ActiveTab := TabControl.Tabs[Min(idx, TabControl.TabCount-1)];
  if TabControl.ActiveTab <> nil then
    TabControl.ActiveTab.SetFocus;

  FListBoxTabs.ListItems[idx].Free;
end;

destructor TDockContent.Destroy;
begin
  FTabItemLinks.Free;
  FListBoxItemLinks.Free;
  inherited Destroy;
end;

procedure TDockContent.TabItemCloseClick(Sender: TObject);
var
  TabItem: TTabItem;
  Link: TTabItemLink;
begin
  if not (Sender is TTabItem) then exit;
  TabItem := Sender as TTabItem;

  if not FTabItemLinks.ContainsKey(TabItem) then exit;
  Link := FTabItemLinks.ExtractPair(TabItem).Value;

  if Link.Form.Close = TCloseAction.caNone then exit;

  DeleteTabItem(TabItem);

  if assigned(FOnUnDock) then
    FOnUnDock(self, DockElement, Link.Form, true);
end;

procedure TDockContent.DockTab(Form: TForm; Container: TFMXObject);
var
  TabItem: TTabItem;
begin
  FDockTab.Visible := true;
  TabItem := AddTabItem(Form, Container);


  FDockTab.ActiveTab := TabItem;
  FDockTab.GoToActiveTab;
  TabItem.Visible := true;
  TabItem.SetFocus;

  if assigned(FOnDock) then
    FOnDock(self, DockElement, Form);

  Form.Visible := false;
end;

procedure TDockContent.DockTabUpdateTabBar(Sender: TObject);
var
  Control: TControl;
  I: integer;
begin
  for I := 0 to length(FDockTab.InnerTabBarControls) - 1 do begin
    Control := FDockTab.InnerTabBarControls[TTabControl.TTabBarButton(I)];
    if not assigned(Control) then continue;

    Control.OnMouseDown := TabBarMouseDown;
  end;
end;

procedure TDockContent.UnDockTab(TabItem: TTabItem);
var
  Link: TTabItemLink;
begin
  if not FTabItemLinks.ContainsKey(TabItem) then exit;
  self.BeginUpdate;
  try
    Link := FTabItemLinks.ExtractPair(TabItem).Value;
    ControlsToForm(TabItem, Link.Form, Link.Container);
    DeleteTabItem(TabItem);
    FisTabItemDrag := false;
  finally
    self.EndUpdate;
  end;

  if assigned(FOnUnDock) then
    FOnUnDock(self, DockElement, Link.Form);

  Link.Form.Show;
  Link.Form.StartWindowDrag;
end;

function TDockContent.GetCount: integer;
begin
  result := FDockTab.TabCount;
end;


procedure TDockContent.ListBoxItemTabClick(Sender: TObject);
var
  Item: TListBoxItem;
  TabItem: TTabitem;
begin
  if not (Sender is TListBoxItem) then exit;

  Item := Sender as TListBoxItem;

  if not FListBoxItemLinks.TryGetValue(Item, TabItem) then exit;
  FDockTab.ActiveTab := TabItem;
  FDockTab.GoToActiveTab;
  TabItem.SetFocus;
  FPoupUpTabs.IsOpen := false;
end;

procedure TDockContent.SetTabItemStyleLookup(const Value: string);
var
  I: integer;
begin
  FTabItemStyleLookup := Value;

  FDockTab.BeginUpdate;
  try
    for I := 0 to FDockTab.TabCount - 1 do
      FDockTab.Tabs[I].StyleLookup := FTabItemStyleLookup;
  finally
    FDockTab.EndUpdate;
  end;
end;

procedure TDockContent.TabBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if not (Button in [TMouseButton.mbRight]) then exit;

  FPoupUpTabs.Width     := 200;
  FPoupUpTabs.Height    := 300;

  FListBoxTabs.ListItems[FDockTab.ActiveTab.Index].IsSelected := true;
  FPoupUpTabs.IsOpen := true;
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

procedure TDockTool.SetActiveElement(const Value: TDockPosition);
begin
  FActiveElement := Value;

  FViewDockTop.IsActive     := (ActiveElement in [TDockPosition.Top])    and (ActiveElement in FDocks);
  FViewDockLeft.IsActive    := (ActiveElement in [TDockPosition.Left])   and (ActiveElement in FDocks);
  FViewDockBottom.IsActive  := (ActiveElement in [TDockPosition.Bottom]) and (ActiveElement in FDocks);
  FViewDockRight.IsActive   := (ActiveElement in [TDockPosition.Right])  and (ActiveElement in FDocks);
  FViewDockClient.IsActive  := (ActiveElement in [TDockPosition.Client]) and (ActiveElement in FDocks);
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

procedure TDockTool.SetDocks(const Value: TDocks);
begin
  FDocks := Value;

  FViewDockTop.Visible    := TDockPosition.Top in Value;
  FViewDockLeft.Visible   := TDockPosition.Left in Value;
  FViewDockBottom.Visible := TDockPosition.Bottom in Value;
  FViewDockRight.Visible  := TDockPosition.Right in Value;
  FViewDockClient.Visible := TDockPosition.Client in Value;
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

procedure TDockContents.AddContent(const DockElement: TDockPosition;
  Form: TForm; Container: TFmxObject);
var
  LDock: TDockContent;
begin
  LDock := nil;
  case DockElement of
    TDockPosition.none: ;
    TDockPosition.Top:     LDock := FDockTop;
    TDockPosition.Left:    LDock := FDockLeft;
    TDockPosition.Bottom:  LDock := FDockBottom;
    TDockPosition.Right:   LDock := FDockRight;
    TDockPosition.Client:  LDock := FDockClient;
  end;

  if assigned(LDock) then
    LDock.DockTab(Form, Container);
end;

constructor TDockContents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDockTop := TDockContent.Create(self);
  FDockTop.Stored      := false;
  FDockTop.DockElement := TDockPosition.Top;
  FDockTop.Visible     := false;
  FDockTop.Align       := TAlignLayout.MostTop;
  FDockTop.OnDock      := DoOnDock;
  FDockTop.OnUnDock    := DoOnUnDock;
  FDockTop.Parent      := self;

  FSplitterTop := TSplitter.Create(self);
  FSplitterTop.Stored  := false;
  FSplitterTop.Visible := false;
  FSplitterTop.Align   := TAlignLayout.Top;
  FSplitterTop.Parent  := self;

  FDockLeft := TDockContent.Create(self);
  FDockLeft.Stored      := false;
  FDockLeft.DockElement := TDockPosition.Left;
  FDockLeft.Visible     := false;
  FDockLeft.Align       := TAlignLayout.MostLeft;
  FDockLeft.OnDock      := DoOnDock;
  FDockLeft.OnUnDock    := DoOnUnDock;
  FDockLeft.Parent      := self;

  FSplitterLeft := TSplitter.Create(self);
  FSplitterLeft.Stored  := false;
  FSplitterLeft.Visible := false;
  FSplitterLeft.Align   := TAlignLayout.Left;
  FSplitterLeft.Parent  := self;

  FDockBottom := TDockContent.Create(self);
  FDockBottom.Stored      := false;
  FDockBottom.DockElement := TDockPosition.Bottom;
  FDockBottom.Visible     := false;
  FDockBottom.Align       := TAlignLayout.MostBottom;
  FDockBottom.OnDock      := DoOnDock;
  FDockBottom.OnUnDock    := DoOnUnDock;
  FDockBottom.Parent      := self;

  FSplitterBottom := TSplitter.Create(self);
  FSplitterBottom.Stored  := false;
  FSplitterBottom.Visible := false;
  FSplitterBottom.Align   := TAlignLayout.Bottom;
  FSplitterBottom.Parent  := self;

  FDockRight := TDockContent.Create(self);
  FDockRight.Stored       := false;
  FDockRight.DockElement  := TDockPosition.Right;
  FDockRight.Visible      := false;
  FDockRight.Align        := TAlignLayout.MostRight;
  FDockRight.OnDock       := DoOnDock;
  FDockRight.OnUnDock     := DoOnUnDock;
  FDockRight.Parent       := self;

  FSplitterRight := TSplitter.Create(self);
  FSplitterRight.Stored  := false;
  FSplitterRight.Visible := false;
  FSplitterRight.Align   := TAlignLayout.Right;
  FSplitterRight.Parent  := self;

  FDockClient := TDockContent.Create(self);
  FDockClient.Stored        := false;
  FDockClient.DockElement   := TDockPosition.Client;
  FDockClient.Visible       := false;
  FDockClient.Align         := TAlignLayout.Client;
  FDockClient.MinClipWidth  := 100;
  FDockClient.MinClipHeight := 100;
  FDockClient.OnDock        := DoOnDock;
  FDockClient.OnUnDock      := DoOnUnDock;
  FDockClient.Parent        := self;

  FPreview := TRectangle.Create(self);
  FPreview.Stored         := false;
  FPreview.Align          := TAlignLayout.None;
  FPreview.Visible        := false;
  FPreview.Parent         := self;
  FPreview.Padding.Top    := 5;
  FPreview.Padding.Left   := 5;
  FPreview.Padding.Bottom := 5;
  FPreview.Padding.Right  := 5;
  FPreview.Opacity        := 0.2;
end;

destructor TDockContents.Destroy;
begin

  inherited;
end;

procedure TDockContents.DoOnDock(Sender: TObject; APosition: TDockPosition; AForm: TForm; isFormClose: boolean = false);
begin
  if (Sender is TDockContent) then
    UpdateVisibleContent(TDockContent(Sender));

  if assigned(FOnDock) then
    FOnDock(Sender, APosition, AForm, isFormClose);
end;

procedure TDockContents.DoOnUnDock(Sender: TObject; APosition: TDockPosition; AForm: TForm; isFormClose: boolean = false);
begin
  if (Sender is TDockContent) then
    UpdateVisibleContent(TDockContent(Sender));

  if assigned(FOnUnDock) then
    FOnUnDock(Sender, APosition, AForm, isFormClose);
end;

function TDockContents.GetCount: integer;
begin
  result := 0;
  inc(result, FDockTop.Count);
  inc(result, FDockLeft.Count);
  inc(result, FDockBottom.Count);
  inc(result, FDockRight.Count);
  inc(result, FDockClient.Count);
end;

function TDockContents.GetDockBottomSize: single;
begin
  result := FDockBottom.Height;
end;

function TDockContents.DockContentByPosition(
  ADockPosition: TDockPosition): TDockContent;
begin
  case ADockPosition of
    TDockPosition.Top:    result := FDockTop;
    TDockPosition.Left:   result := FDockLeft;
    TDockPosition.Client: result := FDockclient;
    TDockPosition.Right:  result := FDockRight;
    TDockPosition.Bottom: result := FDockBottom;
    else                  result := nil;
  end;
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

procedure TDockContents.SetPreviewState(const Value: TDockPosition);
var
  PreviewPosition: TPointF;
  PreviewSize: TPointF;
begin
  FPreviewState := Value;

  if value in [TDockPosition.none] then begin
    FPreview.Visible := false;
    exit;
  end;

  case Value of
    TDockPosition.Top:     begin
                            PreviewPosition := TPointF.Create(0,0);
                            PreviewSize     := TPointF.Create(self.Width, DockTopSize);
                          end;
    TDockPosition.Left:    begin
                            PreviewPosition := TPointF.Create(0,0);
                            PreviewSize     := TPointF.Create(DockLeftSize, self.Height);

                            if FDockTop.Visible then begin
                              PreviewPosition.Y := PreviewPosition.Y + DockTopSize;
                              PreviewSize.Y := PreviewSize.Y - DockTopSize;
                            end;

                            if FDockBottom.Visible then begin
                              PreviewSize.Y := PreviewSize.Y - DockBottomSize;
                            end;
                          end;
    TDockPosition.Bottom:  begin
                            PreviewPosition := TPointF.Create(0,self.Height - DockBottomSize);
                            PreviewSize     := TPointF.Create(self.Width, DockBottomSize);
                          end;
    TDockPosition.Right:   begin
                            PreviewPosition := TPointF.Create(self.Width - DockRightSize,0);
                            PreviewSize     := TPointF.Create(DockRightSize, self.Height);

                            if FDockTop.Visible then begin
                              PreviewPosition.Y := PreviewPosition.Y + DockTopSize;
                              PreviewSize.Y := PreviewSize.Y - DockTopSize;
                            end;

                            if FDockBottom.Visible then begin
                              PreviewSize.Y := PreviewSize.Y - DockBottomSize;
                            end;
                          end;
    TDockPosition.Client:  begin
                            PreviewPosition := TPointF.Create(0,0);
                            PreviewSize     := TPointF.Create(self.Width, self.Height);

                            if FDockTop.Visible then begin
                              PreviewPosition.Y := PreviewPosition.Y + DockTopSize;
                              PreviewSize.Y := PreviewSize.Y - DockTopSize;
                            end;

                            if FDockBottom.Visible then begin
                              PreviewSize.Y := PreviewSize.Y - DockBottomSize;
                            end;

                            if FDockLeft.Visible then begin
                              PreviewPosition.X := PreviewPosition.X + DockLeftSize;
                              PreviewSize.X := PreviewSize.X - DockLeftSize;
                            end;

                            if FDockRight.Visible then begin
                              PreviewSize.X := PreviewSize.X - DockLeftSize;
                            end;
                          end;
  end;

  FPreview.Position.X := PreviewPosition.X + FPreview.Padding.Top;
  FPreview.Position.Y := PreviewPosition.Y + FPreview.Padding.Left;
  FPreview.Width      := PreviewSize.X - (FPreview.Padding.Right * 2);
  FPreview.Height     := PreviewSize.Y - (FPreview.Padding.Bottom * 2);
  FPreview.Visible    := true;
end;

procedure TDockContents.SetTabItemHeight(const Value: single);
begin
  FTabItemHeight := Value;

  FDockTop.TabItemHeight    := Value;
  FDockLeft.TabItemHeight   := Value;
  FDockBottom.TabItemHeight := Value;
  FDockRight.TabItemHeight  := Value;
  FDockClient.TabItemHeight := Value;
end;

procedure TDockContents.SetTabItemStyleLookup(const Value: string);
begin
  FTabItemStyleLookup := Value;

  if (not (csDesignInstance in ComponentState)) and (not (csDesigning in ComponentState)) then begin
    FDockTop.TabItemStyleLookup     := Value;
    FDockLeft.TabItemStyleLookup    := Value;
    FDockBottom.TabItemStyleLookup  := Value;
    FDockRight.TabItemStyleLookup   := Value;
    FDockClient.TabItemStyleLookup  := Value;
  end;
end;

procedure TDockContents.SetTabItemWidth(const Value: single);
begin
  FTabItemWidth := Value;

  FDockTop.TabItemWidth    := Value;
  FDockLeft.TabItemWidth   := Value;
  FDockBottom.TabItemWidth := Value;
  FDockRight.TabItemWidth  := Value;
  FDockClient.TabItemWidth := Value;
end;

procedure TDockContents.UpdateVisibleContent(ADockContent: TDockContent);
var
  I: TDockPosition;
  NextDockContent: TDockContent;
begin
  if ADockContent.Count = 0 then begin
    ADockContent.Visible := false;

    for I := TDockPosition.Top to TDockPosition.Bottom do begin
      NextDockContent := DockContentByPosition(I);
      if NextDockContent.Count > 0 then break;
    end;

    if NextDockContent <> nil then begin
      if NextDockContent.FDockTab.ActiveTab <> nil then
        NextDockContent.FDockTab.ActiveTab.SetFocus;
    end;

  end else
    ADockContent.Visible := true;

    case ADockContent.DockElement of
      TDockPosition.Top:    FSplitterTop.Visible    := ADockContent.Visible;
      TDockPosition.Left:   FSplitterLeft.Visible   := ADockContent.Visible;
      TDockPosition.Bottom: FSplitterBottom.Visible := ADockContent.Visible;
      TDockPosition.Right:  FSplitterRight.Visible  := ADockContent.Visible;
    end;
end;

{$ENDREGION}

{$REGION 'TDockTabItem' }

procedure TDockTabItemClose.ApplyStyle;
const
  ButtonCloseStyle = 'ButtonClose';
  TopStyle = 'top';
  BottomStyle = 'bottom';

var
  Base: TFmxObject;
  B: TFmxObject;
begin
  inherited;
   B := ResourceLink.FindStyleResource(TopStyle);
   if B is TControl then begin
     FItemStyle := TControl(B);
     FItemStyle.Visible := True;

     Base := B.FindStyleResource(ButtonCloseStyle);
     if assigned(Base) then
       TButton(Base).OnClick := DoOnCloseClick;

     B := ResourceLink.FindStyleResource(BottomStyle);
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

{$ENDREGION}

{$REGION 'TInnerTabControl '}

procedure TInnerTabControl.DoUpdateTabBarButtons(
  const TabBarButtons: TTabControl.TTabBarButtons; const TabPosition: TTabPosition;
  const Content: TContent; var TabBarControls: TTabControl.TArrayTabBarControls);
begin
  inherited DoUpdateTabBarButtons(TabBarButtons, TabPosition, Content, TabBarControls);
  InnerTabBarControls := TabBarControls;

  if Assigned(FOnUnpdateTabbarControls) then
    FOnUnpdateTabbarControls(self);
end;

{$ENDREGION}

{ TDockContent.TTabItemLink }


constructor TDockContent.TTabItemLink.New(ATabItem: TTabItem; AForm: TForm;
  AContainer: TFmxObject);
begin
  TabItem   := ATabItem;
  Form      := AForm;
  Container := AContainer;
end;

end.
