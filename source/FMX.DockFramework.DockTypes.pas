unit FMX.DockFramework.DockTypes;

interface

type
  TDockToolElement = (none, Top, Left, Bottom, Right, Client);

  TDocks = set of TDockToolElement;

  TDockState = (Undocking,
                Docking);

  TDockMessages = class
  const
    MOVING      = 'DOCK_MANAGER|MOVING|%f|%f|%d';
    MOVED       = 'DOCK_MANAGER|MOVED|%f|%f|%d';
    DOCK_NONE   = 'DOCK_FROM|DOCK|NONE';
    DOCK_TOP    = 'DOCK_FROM|DOCK|TOP';
    DOCK_LEFT   = 'DOCK_FROM|DOCK|LEFT';
    DOCK_BOTTOM = 'DOCK_FROM|DOCK|BOTTOM';
    DOCK_RIGHT  = 'DOCK_FROM|DOCK|RIGHT';
    DOCK_CLIENT = 'DOCK_FROM|DOCK|CLIENT';

//    CREATED = '';
  end;

implementation

end.
