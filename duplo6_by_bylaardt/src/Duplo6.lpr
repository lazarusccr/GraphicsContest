program Duplo6;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cmem,
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tabuleiro,sobre,Controls,DefaultTranslator;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  form3.showmodal;
  if form3.ModalResult=mrok then
    Application.Run;
end.

