program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, principal, common, SysUtils, Audio, Sprites,
  Introduction, ChooseCountry, Language, variable_parameter_V2,
  Utilitaire_chaine_utf8, OpenAlSoundManager, OpenALSoundWavFileLoader;

{$R *.res}
var
  OldFormat : TFormatSettings ;

begin
  Application.Title:='EducRace';
  OldFormat := SysUtils.FormatSettings ;
  SysUtils.FormatSettings.DecimalSeparator := '.';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm_Principal, Form_Principal);
  Application.Run;
  SysUtils.FormatSettings := OldFormat ;
end.

