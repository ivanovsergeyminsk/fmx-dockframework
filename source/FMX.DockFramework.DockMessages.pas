unit FMX.DockFramework.DockMessages;

interface

uses
  System.Messaging,
  FMX.DockFramework.DockTypes
  ;

type
  TDockMessageMoving = class(System.Messaging.TMessage<TDockMove>);
  TDockMessageMoved  = class(System.Messaging.TMessage<TDockMove>);
  TDockMessageDock   = class(System.Messaging.TMessage<TDockDock>);

implementation

end.
