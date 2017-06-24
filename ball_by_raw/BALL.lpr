PROGRAM BALL;
 {$MODE OBJFPC}{$H+}

 USES
  {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
  {$ENDIF}{$ENDIF}
   Interfaces,
   Forms,
   uBALL;

  {$R *.RES}


BEGIN
 RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TwndGUI, wndGUI);
  Application.Run;
END.

