unit FMX.DockFramework.DockMessages;

interface

uses
  System.Messaging,
  FMX.Controls,
  FMX.DockFramework.DockTypes
  ;

type
  TMessageControlFocused = class(System.Messaging.TMessage<TControl>);

  TDockMessageMoving = class(System.Messaging.TMessage<TDockMove>);
  TDockMessageMoved  = class(System.Messaging.TMessage<TDockMove>);
  TDockMessageDock   = class(System.Messaging.TMessage<TDockDock>);


implementation

end.
