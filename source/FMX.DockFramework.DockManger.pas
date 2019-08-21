unit FMX.DockFramework.DockManger;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Layouts, FMX.StdCtrls,
  FMX.Objects, System.UITypes, System.Messaging, System.Types, FMX.Forms,
  FMX.DockFramework.FDockToolForm, FMX.DockFramework.DockTypes;

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
    FActiveElement: TDockToolElement;
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

    procedure SetActiveElement(const Value: TDockToolElement);
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
    property ActiveElement: TDockToolElement read FActiveElement write SetActiveElement;
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
    MessageManager: TMessageManager;

    FDockToolForm: TFDockToolForm;

    FPreview: TRectangle;
    FDockLayoutLeft:    TLayout;
    FDockLayoutRight:   TLayout;
    FDockLayoutBottom:  TLayout;
    FDockLayoutClient:  TLayout;
    FSplitterLeft: TSplitter;
    FSplitterBottom: TSplitter;
    FSplitterRight: TSplitter;

    FDockToolLayout: TLayout;
    FDockTool: TDockTool;
    function GetDockToolSize: single;
    procedure SetDockToolSize(const Value: single);

    function GetVisibleDockTool: boolean;
    procedure SetVisibleDockTool(const Value: boolean);

    function GetActiveElement: TDockToolElement;
    procedure SetActiveElement(const Value: TDockToolElement);

    function  GetDocToolColor: TAlphaColor;
    procedure SetDockToolColor(const Value: TAlphaColor);

    procedure SubscribeMessages;
    procedure EventMoving(const X, Y: single; const Docks: TDocks);
    procedure EventMoved(const X, Y: single; const Docks: TDocks);

    procedure SetPicture(const X, Y: single);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }

    property DockToolSize: single read GetDockToolSize write SetDockToolSize;
    property DockToolVisible: boolean read GetVisibleDockTool write SetVisibleDockTool;
    property DockToolColor: TAlphaColor read GetDocToolColor write SetDockToolColor;
    property DockToolElement: TDockToolElement read GetActiveElement write SetActiveElement;
  end;

  TDockManger = class(TCustomDockManger)
  published
    { Published declarations }

    property DockToolSize: single read GetDockToolSize write SetDockToolSize;
    property DockToolVisible: boolean read GetVisibleDockTool write SetVisibleDockTool;
    property DockToolColor: TAlphaColor read GetDocToolColor write SetDockToolColor;
    property DockToolElement: TDockToolElement read GetActiveElement write SetActiveElement;
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

  FDockLayoutLeft := TLayout.Create(self);
  FDockLayoutLeft.Align  := TAlignLayout.Left;
  FDockLayoutLeft.Parent := self;
  FDockLayoutLeft.Stored := false;

  FSplitterLeft := TSplitter.Create(self);
  FSplitterLeft.Align  := TAlignLayout.Left;
  FSplitterLeft.Parent := self;
  FSplitterLeft.Stored := false;

  FDockLayoutRight := TLayout.Create(self);
  FDockLayoutRight.Align  := TAlignLayout.Right;
  FDockLayoutRight.Parent := self;
  FDockLayoutRight.Stored := false;

  FSplitterRight := TSplitter.Create(self);
  FSplitterRight.Align  := TAlignLayout.Right;
  FSplitterRight.Parent := self;
  FSplitterRight.Stored := false;

  FDockLayoutBottom := TLayout.Create(self);
  FDockLayoutBottom.Align  := TAlignLayout.MostBottom;
  FDockLayoutBottom.Parent := self;
  FDockLayoutBottom.Stored  := false;

  FSplitterBottom := TSplitter.Create(self);
  FSplitterBottom.Align := TAlignLayout.Bottom;
  FSplitterBottom.Parent := self;
  FSplitterBottom.Stored := false;

  FDockLayoutClient := TLayout.Create(self);
  FDockLayoutClient.Align  := TAlignLayout.Client;
  FDockLayoutClient.Parent := self;
  FDockLayoutClient.Stored  := false;

  FPreview := TRectangle.Create(self);
  FPreview.Align := TAlignLayout.Contents;
  FPreview.Margins.Top    := 5;
  FPreview.Margins.Left   := 5;
  FPreview.Margins.Bottom := 5;
  FPreview.Margins.Right  := 5;
  FPreview.Opacity        := 0.3;
  FPreview.Stored         := false;

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

  SubscribeMessages;
end;

destructor TCustomDockManger.Destroy;
begin

  inherited Destroy;
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
  SetPicture(X, Y);
end;

function TCustomDockManger.GetActiveElement: TDockToolElement;
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

procedure TCustomDockManger.SetActiveElement(const Value: TDockToolElement);
begin
  FDockTool.ActiveElement := Value;

  case Value of
    TDockToolElement.none:   FPreview.Parent := nil;
    TDockToolElement.Top:    FPreview.Parent := FDockLayoutClient;
    TDockToolElement.Left:   FPreview.Parent := FDockLayoutLeft;
    TDockToolElement.Bottom: FPreview.Parent := FDockLayoutBottom;
    TDockToolElement.Right:  FPreview.Parent := FDockLayoutRight;
    TDockToolElement.Client: FPreview.Parent := FDockLayoutClient;
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

procedure TCustomDockManger.SetPicture(const X, Y: single);
var
  MousePoint: TPointF;
begin
  MousePoint := TPointF.Create(X, Y);
  if FDockTool.GetBottomRect.Contains(MousePoint) and (TDockToolElement.Bottom in FDockTool.Docks) then begin
    DockToolElement := TDockToolElement.Bottom;
    exit;
  end;

  if FDockTool.GetClientRect.Contains(MousePoint) and (TDockToolElement.Client in FDockTool.Docks) then begin
    DockToolElement := TDockToolElement.Client;
    exit;
  end;

  if FDockTool.GetLeftRect.Contains(MousePoint) and (TDockToolElement.Left in FDockTool.Docks) then begin
    DockToolElement := TDockToolElement.Left;
    exit;
  end;

  if FDockTool.GetRightRect.Contains(MousePoint) and (TDockToolElement.Right in FDockTool.Docks) then begin
    DockToolElement := TDockToolElement.Right;
    exit;
  end;

  if FDockTool.GetTopRect.Contains(MousePoint) and (TDockToolElement.Top in FDockTool.Docks) then begin
    DockToolElement := TDockToolElement.Top;
    exit;
  end;

  DockToolElement := TDockToolElement.none;
end;

procedure TCustomDockManger.SetVisibleDockTool(const Value: boolean);
begin
  FDockTool.Visible := Value;
end;

procedure TCustomDockManger.SubscribeMessages;
begin
  MessageManager := TMessageManager.DefaultManager;

  MessageManager.SubscribeToMessage(TMessage<UnicodeString>, procedure(const Sender: TObject; const M: TMessage)
  var
    Value: string;
    ValueM: TArray<string>;
    LDocks: integer;
    RDocks: TDocks absolute LDocks;
  begin
    Value := (M as TMessage<UnicodeString>).Value;
    ValueM := Value.Split(['|']);

    if not ValueM[0].Equals('DOCK_MANAGER') then exit;

    if ValueM[1].Equals('MOVING') then begin
      LDocks := ValueM[4].ToInteger;
      EventMoving(ValueM[2].ToSingle, ValueM[3].ToSingle, RDocks);
    end else if ValueM[1].Equals('MOVED') then begin
      LDocks := ValueM[4].ToInteger;
      EventMoved(ValueM[2].ToSingle, ValueM[3].ToSingle, RDocks);
    end;
  end
  );
end;

procedure TCustomDockManger.EventMoved(const X, Y: single; const Docks: TDocks);
begin
  self.FDockTool.Docks := Docks;
  self.DockToolVisible := false;
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

procedure TDockTool.SetActiveElement(const Value: TDockToolElement);
begin
  FActiveElement := Value;

  FViewDockTop.IsActive     := (ActiveElement in [TDockToolElement.Top])    and (ActiveElement in FDocks);
  FViewDockLeft.IsActive    := (ActiveElement in [TDockToolElement.Left])   and (ActiveElement in FDocks);
  FViewDockBottom.IsActive  := (ActiveElement in [TDockToolElement.Bottom]) and (ActiveElement in FDocks);
  FViewDockRight.IsActive   := (ActiveElement in [TDockToolElement.Right])  and (ActiveElement in FDocks);
  FViewDockClient.IsActive  := (ActiveElement in [TDockToolElement.Client]) and (ActiveElement in FDocks);
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
