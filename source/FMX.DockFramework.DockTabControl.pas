unit FMX.DockFramework.DockTabControl;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.TabControl,
  System.Types, FMX.Graphics, FMX.InertialMovement;

type
  TTabPosition = (Top, Bottom, Left, Right, None, Dots, PlatformDefault);

  TTabPositionHelper = record helper for FMX.DockFramework.DockTabControl.TTabPosition
    class function FromUsual(ATabPosition: FMX.TabControl.TTabPosition): TTabPosition; static;
  end;

  TDockTabItem = class(TTabItem)
  private
    FLeftOffset: single;
    FRightOffset: single;

    procedure UpdateLayoutControl;
  end;

  TDockTabControl = class(TTabControl)
  public type
    TTabBarButton = (Left, Ellipsis, Right);
    TTabBarButtons = set of TTabBarButton;
    TArrayTabBarControls = array [TTabBarButton] of TControl;
  strict private
    FContent: TContent;
  public const
    DefaultButtons = [TTabBarButton.Left, TTabBarButton.Ellipsis, TTabBarButton.Right];
    ///<summary> The styles names which used for the buttons in the header area </summary>
    ButtonStyleNames: array [TTabBarButton] of string = ('spinleftbutton', 'spinellipsisbutton', 'spinrightbutton');
  private
    FRealigningTabs: Boolean;
    FClientRect: TRectF;
    FScrollBackground: TBrushObject;
    FBackground: TControl;
    FDefaultFullSize: Boolean;
    FFixedTabHeight: Boolean;
    FTabHeight: single;
    FTabContentSize: TSizeF;
    FTabBarRect: TRectF;
    FNoItemsContent: TControl;
    FTabPosition: TTabPosition;

    procedure UpdateAnimation(const DotItems: Boolean; const ActiveTabLeft, ActiveTabRight: Single);
  protected
    procedure ApplyStyle; override;
    procedure RealignTabs; override;
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

uses
  System.Math;

type
  TOpenControl = class(TControl);
  TOpenTabItem = class(TTabItem);

procedure Register;
begin
  RegisterComponents('Dock Framework', [TDockTabControl]);
end;

{ TDockTabControl }

procedure TDockTabControl.ApplyStyle;
var
  B: TFmxObject;
begin
  inherited ApplyStyle;
  FindStyleResource<TBrushObject>('scrollbackground', FScrollBackground);
  FindStyleResource<TControl>('background', FBackground);
  if FindStyleResource<TFmxObject>('FullSize', B) then
    FDefaultFullSize := True;
  if FindStyleResource<TFmxObject>('TabHeight', B) then
  begin
    TabHeight := B.TagFloat;
    FFixedTabHeight := True;
  end;
end;

procedure TDockTabControl.RealignTabs;
const
  MinHeight = 5;
  InvisibleItemPos = $FFFF;
var
  TabPos: TTabPosition;
  DotItems: Boolean;
  CountVisibleTab, I, VisibleIndex, ColCount, RowCount, Surplus: Integer;
  CurX, CurY, LLeftOffset, LRightOffset, AutoWidth, MaxHeight, TotalWidth, ActiveTabLeft, ActiveTabRight: Single;
  ItemRect: TRectF;
  LItem: TTabItem;
  DesignTabWidth, DesignTabHeight, BorderDotItem: Integer;
begin
  if FRealigningTabs then
    Exit;

  inherited RealignTabs;

  if ([csLoading, csDestroying] * ComponentState = []) then
  begin
    FRealigningTabs := True;
    try
      { initialization }
      DesignTabWidth := TTabItem.DotSize;
      DesignTabHeight := TTabItem.DotSize;
      TabPos := TTabPosition.FromUsual(EffectiveTabPosition); //Из стандартного в кастомный
      DotItems := TabPos in [TTabPosition.None, TTabPosition.Dots];
      FClientRect := LocalRect;
      if FBackground <> nil then
        FClientRect := FBackground.Padding.PaddingRect(FClientRect);
      if ResourceControl <> nil then
        FClientRect := ResourceControl.Margins.PaddingRect(FClientRect);
      if DotItems then
        MaxHeight := DesignTabHeight
      else
        MaxHeight := MinHeight;
      TotalWidth := 0;
      CountVisibleTab := 0;
      ActiveTabRight := 0;
      ActiveTabLeft := 0;
      { Calculate count of visible tabs and sizes }
      for I := 0 to TabCount - 1 do
      begin
        LItem := Tabs[I];
        TOpenTabItem(LItem).FDesignSelectionMarks := not DotItems;
        if LItem.Visible then
        begin
          if not DotItems then
          begin
            if LItem = ActiveTab then
              ActiveTabLeft := TotalWidth + TDockTabItem(LItem).FLeftOffset;
            MaxHeight := Max(MaxHeight, Trunc(TOpenTabItem(LItem).Info.Size.cy + LItem.Margins.Top + LItem.Margins.Bottom));
            TotalWidth := TotalWidth + TOpenTabItem(LItem).Info.Size.cx + LItem.Margins.Left + LItem.Margins.Right;
            if LItem = ActiveTab then
              ActiveTabRight := TotalWidth - TDockTabItem(LItem).FRightOffset;
          end
          else if (CountVisibleTab = 0) and (TOpenTabItem(LItem).ResourceControl <> nil) then
          begin
            if TOpenControl(TOpenTabItem(LItem).ResourceControl).FixedSize.cx > 0 then
              DesignTabWidth := Max(DesignTabWidth, TOpenControl(TOpenTabItem(LItem).ResourceControl).FixedSize.cx);
            if TOpenControl(TOpenTabItem(LItem).ResourceControl).FixedSize.cy > 0 then
            begin
              DesignTabHeight := Max(DesignTabWidth, TOpenControl(TOpenTabItem(LItem).ResourceControl).FixedSize.cy);
              MaxHeight := Max(MaxHeight, DesignTabHeight);
            end;
          end;
          Inc(CountVisibleTab);
        end;
      end;
      BorderDotItem := Max(1, Max(DesignTabHeight, DesignTabWidth) div 10);
      Inc(DesignTabHeight, 2 * BorderDotItem);
      Inc(DesignTabWidth, 2 * BorderDotItem);
      { Initialization of left and right offsets }
      LLeftOffset := 0;
      LRightOffset := 0;
      VisibleIndex := 0;
      if not DotItems then
        for I := 0 to TabCount - 1 do
        begin
          LItem := Tabs[I];
          if LItem.Visible then
          begin
            if VisibleIndex = 0 then
              LLeftOffset := TDockTabItem(LItem).FLeftOffset;
            if VisibleIndex = CountVisibleTab - 1 then
              LRightOffset := TDockTabItem(LItem).FRightOffset;
            Inc(VisibleIndex);
          end;
        end;
      { Calculate rows and columns }
      ColCount := CountVisibleTab;
      RowCount := 1;
      if DotItems and (CountVisibleTab > 0) then
      begin
        if CountVisibleTab * DesignTabWidth > FClientRect.Width then
        begin
          ColCount := Max(1, Trunc(FClientRect.Width / DesignTabWidth));
          RowCount := (CountVisibleTab + ColCount - 1) div ColCount;
        end;
        TotalWidth := ColCount * DesignTabWidth - 2 * BorderDotItem;
        MaxHeight := Max(RowCount * DesignTabHeight, FTabHeight);
        AutoWidth := DesignTabWidth;
      end
      else
      begin
        AutoWidth := FClientRect.Width + LLeftOffset + LRightOffset;
        if CountVisibleTab = 0 then
          MaxHeight := 0
        else
        begin
          if FTabHeight > 0 then
            MaxHeight := FTabHeight;
          if EffectiveFullSize then
          begin
            AutoWidth := Trunc(Max(MinHeight, AutoWidth / CountVisibleTab));
            TotalWidth := AutoWidth * CountVisibleTab;
          end;
          TotalWidth := TotalWidth - LLeftOffset - LRightOffset;
        end;
      end;
      { Initialization of bounds }
      FTabContentSize := TSizeF.Create(TotalWidth, MaxHeight);
      FTabBarRect := TRectF.Create(TPointF.Zero, FClientRect.Width, MaxHeight);
      case TabPos of
        TTabPosition.Top:
          FTabBarRect.Offset(FClientRect.TopLeft);
        TTabPosition.Bottom:
          FTabBarRect.Offset(FClientRect.Left, FClientRect.Bottom - FTabBarRect.Height);
        TTabPosition.None, TTabPosition.Dots:
          FTabBarRect.Offset(FClientRect.Left, FClientRect.Bottom - FTabBarRect.Height - DesignTabHeight);
      end;
      FContent.BoundsRect := TabBarRect;
      FNoItemsContent.BoundsRect := TabBarRect;
      FNoItemsContent.Visible := not (FTabPosition in [TTabPosition.None, TTabPosition.Dots]);
      UpdateTabBarButtons;
      UpdateAnimation(DotItems, ActiveTabLeft, ActiveTabRight);
      CurX := TabContentPosition - LLeftOffset;
      if EffectiveFullSize then
        Surplus := Trunc(Max(FClientRect.Width - TotalWidth, 0))
      else
        Surplus := 0;
      if DotItems then
        if (not (csDesigning in ComponentState)) and (TabPos = TTabPosition.None) then
          CurY := InvisibleItemPos
        else
          CurY := Round(Max(0, MaxHeight - (DesignTabHeight * RowCount - 2 * BorderDotItem)) / 2)
      else
        CurY := 0;
      VisibleIndex := 0;
      { Update tab positions }
      for I := 0 to TabCount - 1 do
      begin
        LItem := Tabs[I];
        if not LItem.Visible then
          Continue;
        if DotItems then
        begin
          ItemRect := TRectF.Create(CurX, CurY, CurX + DesignTabWidth - 2 * BorderDotItem,
            CurY + DesignTabHeight - 2 * BorderDotItem);
          CurX := CurX + DesignTabWidth;
          if CurX >= TabContentPosition + TabContentSize.Width then
          begin
            CurX := TabContentPosition;
            CurY := CurY + DesignTabHeight;
          end;
        end
        else
        begin
          ItemRect := TRectF.Create(TPointF.Create(CurX, CurY), TOpenTabItem(LItem).Info.Size.cx, MaxHeight);
          ItemRect := LItem.Margins.PaddingRect(ItemRect);
          if EffectiveFullSize then
            ItemRect.Width := AutoWidth - LItem.Margins.Left - LItem.Margins.Right;
          if VisibleIndex = CountVisibleTab - 1 then
            ItemRect.Width := ItemRect.Width + Surplus;
          CurX := CurX + ItemRect.Width + LItem.Margins.Left + LItem.Margins.Right;
        end;
        LItem.BoundsRect := ItemRect;
        Inc(VisibleIndex);
      end;
      { aligning }
      for I := 0 to TabCount - 1 do
        if Tabs[I].Visible then
          TDockTabItem(Tabs[I]).UpdateLayoutControl;
      if not FDisableAlign then
        Realign;
    finally
      FRealigningTabs := False;
    end;
  end
  else
    FContent.Height := 0;
end;

procedure TDockTabControl.UpdateAnimation(const DotItems: Boolean;
  const ActiveTabLeft, ActiveTabRight: Single);
begin

end;

{ TTabPositionHelper }

class function TTabPositionHelper.FromUsual(
  ATabPosition: FMX.TabControl.TTabPosition): TTabPosition;
begin
  case ATabPosition of
    FMX.TabControl.TTabPosition.Top:             result := FMX.DockFramework.DockTabControl.TTabPosition.Top;
    FMX.TabControl.TTabPosition.Bottom:          result := FMX.DockFramework.DockTabControl.TTabPosition.Bottom;
    FMX.TabControl.TTabPosition.Dots:            result := FMX.DockFramework.DockTabControl.TTabPosition.Dots;
    FMX.TabControl.TTabPosition.PlatformDefault: result := FMX.DockFramework.DockTabControl.TTabPosition.PlatformDefault;
    else                                         result := FMX.DockFramework.DockTabControl.TTabPosition.None;
  end;
end;

{ TDockTabItem }

procedure TDockTabItem.UpdateLayoutControl;
begin

end;

end.
