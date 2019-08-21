unit FMX.DockFramework.DockManger;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Layouts, FMX.StdCtrls,
  FMX.Objects, System.UITypes, System.Messaging, System.Types, FMX.Forms, FMX.TabControl,

  FMX.DockFramework.FDockToolForm,
  FMX.DockFramework.DockTypes,
  FMX.DockFramework.DockMessages,
  FMX.DockFramework.DockForm
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ActiveElement: TDockElement read FActiveElement write SetActiveElement;
    property Color: TAlphaColor read FColor write SetColor;

    property TopRect: TRectF read GetTopRect;
    property LeftRect: TRectF read GetLeftRect;
    property BottomRect: TRectF read GetBottomRect;
    property RightRect: TRectF read GetRightRect;
    property ClientRect: TRectF read GetClientRect;

    property Docks: TDocks read FDocks write FDocks;
  end;

  TCustomDockManger = class(TLayout)
  private
    { Private declarations }
    FDockToolForm: TFDockToolForm;

    FPreview: TRectangle;
    FDockLayoutTop:     TLayout;
    FDockLayoutLeft:    TLayout;
    FDockLayoutRight:   TLayout;
    FDockLayoutBottom:  TLayout;
    FDockLayoutClient:  TLayout;
    FSplitterTop: TSplitter;
    FSplitterLeft: TSplitter;
    FSplitterBottom: TSplitter;
    FSplitterRight: TSplitter;

    FDockTabTop: TTabControl;
    FDockTabLeft: TTabControl;
    FDockTabBottom: TTabControl;
    FDockTabRight: TTabControl;
    FDockTabClient: TTabControl;

    FDockToolLayout: TLayout;
    FDockTool: TDockTool;

    FTabItemDrag: boolean;
    FTabItemDragPosition: TPointF;
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

    procedure SetDockToolPicture(const X, Y: single);

    procedure DoDock(const SelectedDock: TDockElement; Form: TForm);
    procedure DoDockTab(TabControl: TTabControl; Form: TForm);
    procedure DoUnDockTab(TabItem: TTabItem);


    procedure CreateDocks;

    procedure CreateLeftDock;
    procedure CreateBottomDock;
    procedure CreateRightDock;
    procedure CreateTopDock;
    procedure CreateClientDock;

    procedure CreatePreview;
    procedure CreateDockTool;

    procedure TabItemMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure TabItemMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure TabItemMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
  protected
    { Protected declarations }
    property DockToolVisible: boolean read GetVisibleDockTool write SetVisibleDockTool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }

    property DockToolSize: single read GetDockToolSize write SetDockToolSize;
    property DockToolColor: TAlphaColor read GetDocToolColor write SetDockToolColor;
    property DockToolElement: TDockElement read GetActiveElement write SetActiveElement;
  end;

  TDockManger = class(TCustomDockManger)
  published
    { Published declarations }

    property DockToolSize;
    property DockToolColor;
    property DockToolElement;
  end;

procedure Register;

implementation

uses
  FMX.Graphics;

procedure Register;
begin
  RegisterComponents('Dock Framework', [TDockManger]);
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
      ColumnItem.Value := 100/GridPanelLayout.ColumnCollection.Count;
    end;

    for RowIndex := 0 to GridPanelLayout.RowCollection.Count - 1 do
    begin
      RowItem := TGridPanelLayout.TRowItem(GridPanelLayout.RowCollection[RowIndex]);
      RowItem.SizeStyle := TGridPanelLayout.TSizeStyle.Percent;
      RowItem.Value := 100/GridPanelLayout.RowCollection.Count;
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
      ColumnItem.Value := ColumntValues[ColumnIndex]//100/GridPanelLayout.ColumnCollection.Count;
    end;

    for RowIndex := 0 to GridPanelLayout.RowCollection.Count - 1 do
    begin
      RowItem := TGridPanelLayout.TRowItem(GridPanelLayout.RowCollection[RowIndex]);
      RowItem.SizeStyle := TGridPanelLayout.TSizeStyle.Percent;
      RowItem.Value := RowValues[RowIndex];//100/GridPanelLayout.RowCollection.Count;
    end;
  end;
end;

{ TDockManger }

constructor TCustomDockManger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  CreateDocks;

  CreatePreview;
  CreateDockTool;

  Subscribes;

  if (csDesigning in ComponentState) or (csDesignInstance in ComponentState)
    then DockToolVisible := true
    else DockToolVisible := false;
end;

procedure TCustomDockManger.CreateBottomDock;
begin
  FDockLayoutBottom := TLayout.Create(self);
  FDockLayoutBottom.Align  := TAlignLayout.MostBottom;
  FDockLayoutBottom.Parent := self;
  FDockLayoutBottom.Stored := false;

  FSplitterBottom := TSplitter.Create(self);
  FSplitterBottom.Align  := TAlignLayout.Bottom;
  FSplitterBottom.Parent := self;
  FSplitterBottom.Stored := false;

  FDockTabBottom := TTabControl.Create(self);
  FDockTabBottom.Align   := TAlignLayout.Client;
  FDockTabBottom.Parent  := FDockLayoutBottom;
  FDockTabBottom.Visible := false;
  FDockTabBottom.Stored  := false;
end;

procedure TCustomDockManger.CreateClientDock;
begin
  FDockLayoutClient := TLayout.Create(self);
  FDockLayoutClient.Align  := TAlignLayout.Client;
  FDockLayoutClient.Parent := self;
  FDockLayoutClient.Stored := false;

  FDockTabClient := TTabControl.Create(self);
  FDockTabClient.Parent  := FDockLayoutClient;
  FDockTabClient.Align   := TAlignLayout.Client;
  FDockTabClient.Visible := false;
  FDockTabClient.Stored  := false;
end;

procedure TCustomDockManger.CreateDocks;
begin
  CreateTopDock;
  CreateLeftDock;
  CreateRightDock;
  CreateBottomDock;
  CreateClientDock;
end;

procedure TCustomDockManger.CreateDockTool;
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

procedure TCustomDockManger.CreateLeftDock;
begin
  FDockLayoutLeft := TLayout.Create(self);
  FDockLayoutLeft.Align  := TAlignLayout.Left;
  FDockLayoutLeft.Parent := self;
  FDockLayoutLeft.Stored := false;

  FSplitterLeft := TSplitter.Create(self);
  FSplitterLeft.Align  := TAlignLayout.Left;
  FSplitterLeft.Parent := self;
  FSplitterLeft.Stored := false;

  FDockTabLeft := TTabControl.Create(self);
  FDockTabLeft.Align   := TAlignLayout.Client;
  FDockTabLeft.Parent  := FDockLayoutLeft;
  FDockTabLeft.Visible := false;
  FDockTabLeft.Stored  := false;
end;

procedure TCustomDockManger.CreatePreview;
begin
  FPreview := TRectangle.Create(self);
  FPreview.Align := TAlignLayout.Contents;
  FPreview.Margins.Top    := 5;
  FPreview.Margins.Left   := 5;
  FPreview.Margins.Bottom := 5;
  FPreview.Margins.Right  := 5;
  FPreview.Opacity        := 0.3;
  FPreview.Stored         := false;
end;

procedure TCustomDockManger.CreateRightDock;
begin
  FDockLayoutRight := TLayout.Create(self);
  FDockLayoutRight.Align  := TAlignLayout.Right;
  FDockLayoutRight.Parent := self;
  FDockLayoutRight.Stored := false;

  FSplitterRight := TSplitter.Create(self);
  FSplitterRight.Align  := TAlignLayout.Right;
  FSplitterRight.Parent := self;
  FSplitterRight.Stored := false;

  FDockTabRight := TTabControl.Create(self);
  FDockTabRight.Align   := TAlignLayout.Client;
  FDockTabRight.Parent  := FDockLayoutRight;
  FDockTabRight.Visible := false;
  FDockTabRight.Stored  := false;
end;

procedure TCustomDockManger.CreateTopDock;
begin
  FDockLayoutTop := TLayout.Create(self);
  FDockLayoutTop.Align  := TAlignLayout.Top;
  FDockLayoutTop.Parent := self;
  FDockLayoutTop.Stored := false;

  FSplitterTop := TSplitter.Create(self);
  FSplitterTop.Align  := TAlignLayout.Top;
  FSplitterTop.Parent := self;
  FSplitterTop.Stored := false;

  FDockTabTop := TTabControl.Create(self);
  FDockTabTop.Align   := TAlignLayout.Client;
  FDockTabTop.Parent  := FDockLayoutTop;
  FDockTabTop.Visible := false;
  FDockTabTop.Stored  := false;
end;

destructor TCustomDockManger.Destroy;
begin

  inherited Destroy;
end;

procedure TCustomDockManger.DoDock(const SelectedDock: TDockElement; Form: TForm);
begin
  self.BeginUpdate;
  try
    case SelectedDock of
      TDockElement.none:   ;
      TDockElement.Top:    DoDockTab(FDockTabTop, Form);
      TDockElement.Left:   DoDockTab(FDockTabLeft, Form);
      TDockElement.Bottom: DoDockTab(FDockTabBottom, Form);
      TDockElement.Right:  DoDockTab(FDockTabRight, Form);
      TDockElement.Client: DoDockTab(FDockTabClient, Form);
    end;
  finally
    self.EndUpdate;
  end;
end;

procedure TCustomDockManger.DoDockTab(TabControl: TTabControl; Form: TForm);
var
  TabItem: TTabItem;
  FmxObject: TFmxObject;
begin
  if not assigned(TabControl) then exit;
  
  TabControl.Visible := true;

  TabItem := TabControl.Add;
  TabItem.Tag := integer(Form);
  TabItem.Text := Form.Caption;
  TabItem.OnMouseDown := TabItemMouseDown;
  TabItem.OnMouseUp   := TabItemMouseUp;
  TabItem.OnMouseMove := TabItemMouseMove;

  for FmxObject in Form.Children.ToArray do
    FmxObject.Parent := TabItem;

  Form.Visible := false;
end;

procedure TCustomDockManger.DoUnDockTab(TabItem: TTabItem);
var
  Form: TForm;
  Content,
  FmxObjectForm: TFmxObject;
  TabControl: TTabControl;
begin
  Form := TForm(TabItem.Tag);
  TabControl := TabItem.TabControl;
  self.BeginUpdate;
  try
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

    if TabControl.TabCount - 1 > 0 then begin
      if TabItem.Index > 0
        then TabControl.ActiveTab := TabControl.Tabs[TabItem.Index-1]
        else TabControl.ActiveTab := TabControl.Tabs[TabItem.Index+1];
    end;

    TabItem.Parent := nil;
    TabItem.Free;

    if TabControl.TabCount = 0 then
      TabControl.Visible := false;

    FTabItemDrag := false;
  finally
    self.EndUpdate;
  end;

  Form.Left := Trunc(Screen.MousePos.X-150);
  Form.Top  := Trunc(Screen.MousePos.Y - 5);
  Form.Show;
  Form.StartWindowDrag;
end;

procedure TCustomDockManger.EventMoving(const X, Y: single; const Docks: TDocks);
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
  SetDockToolPicture(X, Y);
end;

function TCustomDockManger.GetActiveElement: TDockElement;
begin
  result := FDockTool.ActiveElement;
end;

function TCustomDockManger.GetDockToolSize: single;
begin
  result := FDockTool.Width;
end;

function TCustomDockManger.GetDocToolColor: TAlphaColor;
begin
  result := FDockTool.Color;
end;

function TCustomDockManger.GetVisibleDockTool: boolean;
begin
  result := FDockTool.Visible;
end;

procedure TCustomDockManger.SetActiveElement(const Value: TDockElement);
begin
  FDockTool.ActiveElement := Value;

  case Value of
    TDockElement.none:   FPreview.Parent := nil;
    TDockElement.Top:    FPreview.Parent := FDockLayoutTop;
    TDockElement.Left:   FPreview.Parent := FDockLayoutLeft;
    TDockElement.Bottom: FPreview.Parent := FDockLayoutBottom;
    TDockElement.Right:  FPreview.Parent := FDockLayoutRight;
    TDockElement.Client: FPreview.Parent := FDockLayoutClient;
  end;
end;

procedure TCustomDockManger.SetDockToolColor(const Value: TAlphaColor);
begin
  FDockTool.Color := Value;
  FPreview.Fill.Color := Value;
end;

procedure TCustomDockManger.SetDockToolSize(const Value: single);
begin
  FDockTool.Width   := Value;
  FDockTool.Height  := FDockTool.Width;
end;

procedure TCustomDockManger.SetDockToolPicture(const X, Y: single);
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

procedure TCustomDockManger.SetVisibleDockTool(const Value: boolean);
begin
  FDockTool.Visible := Value;
end;

procedure TCustomDockManger.SubscribeDockMessageDock;
var
  MessageManager: TMessageManager;
begin
  MessageManager := TMessageManager.DefaultManager;

  MessageManager.SubscribeToMessage(TDockMessageDock, procedure(const Sender: TObject; const M: TMessage)
  var
    Value: TDockDock;
    DockForm: TDockForm;
  begin
    if not (Sender is TDockForm) then exit;

    DockForm := Sender as TDockForm;
    Value := (M as TDockMessageDock).Value;
    if Value.Dock in Value.AccessDocks then
      DoDock(Value.Dock, DockForm.Form);
  end
  );
end;

procedure TCustomDockManger.SubscribeDockMessageMoved;
var
  MessageManager: TMessageManager;
begin
  MessageManager := TMessageManager.DefaultManager;

  MessageManager.SubscribeToMessage(TDockMessageMoved, procedure(const Sender: TObject; const M: TMessage)
  var
    Value: TDockMove;
    DockForm: TDockForm;
  begin
    if not (Sender is TDockForm) then exit;

    DockForm := Sender as TDockForm;
    Value := (M as TDockMessageMoved).Value;
    EventMoved(Value.MousePosition.X, Value.MousePosition.Y, Value.AccessDocks, DockForm.Form);
  end
  );
end;

procedure TCustomDockManger.SubscribeDockMessageMoving;
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

procedure TCustomDockManger.Subscribes;
begin
  SubscribeDockMessageMoving;
  SubscribeDockMessageMoved;
  SubscribeDockMessageDock;
end;

procedure TCustomDockManger.TabItemMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
  if not FTabItemDrag then exit;

  if (abs(FTabItemDragPosition.X-X) > 30)
  then
    DoUnDockTab(Sender as TTabItem);
end;

procedure TCustomDockManger.TabItemMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then begin
    FTabItemDrag := true;
    FTabItemDragPosition.X := X;
    FTabItemDragPosition.Y := Y;
  end else
    FTabItemDrag := false;
end;

procedure TCustomDockManger.TabItemMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FTabItemDrag := false;
end;

procedure TCustomDockManger.EventMoved(const X, Y: single; const Docks: TDocks; Form: TForm);
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

  FPreview.Parent := nil;
  DoDock(SelelectedDock, Form);
end;

{ TDockTool }

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

{ TDockTool.TPictureDockTool }

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

{ TDockTool.TPictureTop }

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

{ TDockTool.TPictureLeft }

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

{ TDockTool.TPictureButtom }

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

{ TDockTool.TPictureRight }

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

{ TDockTool.TPictureClient }

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

end.
