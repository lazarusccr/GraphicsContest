UNIT uBALL;
 {$MODE OBJFPC}{$H+}

Interface
 USES
  Classes,  SysUtils, Forms,
  Controls, Graphics, ExtCtrls;

 TYPE
  TwndGUI = Class(TForm)

   shpBall: TShape;

   Procedure shpBallMouseMove(Sender: TObject; Shift: TShiftState;
                              X, Y  : Integer);
   Procedure FormResize      (Sender: TObject);
  End;

 VAR
  wndGUI: TwndGUI;

Implementation
 {$R *.LFM}


Procedure TwndGUI.shpBallMouseMove(Sender: TObject; Shift: TShiftState;
                                   X, Y  : Integer);
 Begin
  shpBall.Top := Random(ClientHeight-shpBall.Height);
  shpBall.Left:= Random(ClientWidth -shpBall.Width);
 End;


Procedure TwndGUI.FormResize(Sender: TObject);
 Begin
  shpBall.Top := 0;
  shpBall.Left:= 0;
 End;

END.

