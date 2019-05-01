program ImageShop;

{$mode delphi}

uses
  {$ifdef unix}
  {$ifdef usecthreads}
  cthreads,
  {$endif}
  {$endif}
  Interfaces, Forms, Main;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TImageForm, ImageForm);
  Application.Run;
end.

