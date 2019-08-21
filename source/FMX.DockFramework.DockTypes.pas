unit FMX.DockFramework.DockTypes;

interface

uses
  System.Types;

type
  TDockElement = (none, Top, Left, Bottom, Right, Client);

  TDocks = set of TDockElement;

  TDockMove = packed record
    MousePosition: TPointF;
    AccessDocks: TDocks;
  end;

  TDockDock = packed record
    Dock: TDockElement;
    AccessDocks: TDocks;
  end;

const
  AllDocks = [low(TDockElement)..high(TDockElement)];

implementation

end.
