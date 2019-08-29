unit FMX.DockFramework.DockTypes;

interface

uses
  System.Types;

type
  TDockPosition = (none, Top, Left, Client, Right, Bottom);

  TDocks = set of TDockPosition;

  TDockMove = packed record
    MousePosition: TPointF;
    AccessDocks: TDocks;
  end;

  TDockDock = packed record
    Dock: TDockPosition;
    AccessDocks: TDocks;
  end;

const
  AllDocks = [low(TDockPosition)..high(TDockPosition)];

implementation

end.
