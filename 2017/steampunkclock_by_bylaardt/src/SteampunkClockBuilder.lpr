program SteampunkClockBuilder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, builder, pb_clock, propedit, builderoptions
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TBuilderform, Builderform);
  Application.CreateForm(TPropForm, PropForm);
  Application.CreateForm(TOptions, Options);
  Application.Run;
end.

