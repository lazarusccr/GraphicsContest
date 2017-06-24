unit HHunter;


{-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

 Images drawn in CorelDRAW! and exported as .PNG files are positioned
 on the [CaseControl] which ia a TBRAGraphicControl.

 There are also 5 TLabel components which, having no 'Colour', are
 invisible but they do have mouse events which can be interogated
 and therefore acted upon.

 They are used to [Open/Close] the case, [Release] the movement and
 [Display or Close] the movement (behind the dial) and, naturally,
 [Terminate] the program.

 Only the change in the appearance of the Mouse Cursor indicates that
 an 'action' is possible.

 As with the 'Real Thing', the action of pressing the winder opens the
 case and to release the Dial/Movement the winder needs to be pulled
 out and the 'Latch' pressed to enable the movement to be swung open.

 Closing the movement to again display the dial requires a mouse-click
 at the bottom of the movement and closing the case is another mouse-click
 on the inside case.

 The whole image can be moved about the screen with a Mouse-Down anywhere
 there is a Drag-Cursor.

 Termination is achieved with a mouse-click on the dial centre where the
 cursor becomes a 'Cross'.

-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Windows, DateUtils, LMessages, ActnList,
  BGRAGraphicControl, BGRABitmap, BGRABitmapTypes, BCTypes;

Const
  LWA_COLORKEY  = 1;
  LWA_ALPHA     = 2;
  LWA_BOTH      = 3;
  WS_EX_LAYERED = $80000;
  GWL_EXSTYLE   = -20;

  {Function SetLayeredWindowAttributes Lib "user32" (ByVal hWnd As Long, ByVal Color As Long, ByVal X As Byte, ByVal alpha As Long) As Boolean }
   function SetLayeredWindowAttributes (hWnd:Longint; Color:Longint; X:Byte; alpha:Longint):boolean
     stdcall; external 'USER32';

   {Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long }
   Function SetWindowLongA (hWnd:Longint; nIndex:longint; dwNewLong:longint):longint
     stdcall; external 'USER32';


   {Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long) As Long }
   Function GetWindowLongA ( hWnd:Longint; nIndex:longint):longint
     stdcall; external 'USER32';


type

  { TWatch }

  TWatch = class(TForm)
    Terminate: TLabel;
    WinderButton: TLabel;
    Latch: TLabel;
    CaseControl: TBGRAGraphicControl;
    TenthSec: TTimer;
    CaseCloseButton: TLabel;
    MovementClose: TLabel;
    procedure CaseCloseButtonClick(Sender: TObject);
    procedure CaseControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CaseControlRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LatchClick(Sender: TObject);
    procedure MovementCloseClick(Sender: TObject);
    procedure TenthSecTimer(Sender: TObject);
    procedure TerminateClick(Sender: TObject);
    procedure WinderButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Case1:    TBGRABitmap;
    MHand:    TBGRABitmap;
    SHand:    TBGRABitmap;
    HHand:    TBGRABitmap;
    Dial:     TBGRABitmap;
    DayWhl:   TBGRABitmap;
    DteWhl:   TBGRABitmap;
    Winder:   TBGRABitmap;
    WindIn:   TBGRABitmap;
    WindOut:  TBGRABitmap;
    Rim:      TBGRABitmap;
    Butt:     TBGRABitmap;
    Back:     TBGRABitmap;
    Case2:    TBGRABitmap;
    TopPlate: TBGRABitmap;
    BalWhl1:  TBGRABitmap;
    BalWhl2:  TBGRABitmap;
    BalPlate: TBGRABitmap;
    Wheel3:   TBGRABitmap;
    Wheel4:   TBGRABitmap;
    Wheel:    TBGRABitmap;
    Pallet1:  TBGRABitmap;
    Pallet2:  TBGRABitmap;
    Escape:   TBGRABitmap;
    DialBak:  TBGRABitmap;

    procedure LoadBitmap(var ABitmap: TBGRABitmap; const ResourceName: string);
    procedure LoadImages();

  end;

var
  Watch: TWatch;

implementation

{$R *.lfm}

{ TWatch }

Type
    date = record
            d,m : byte;
            y   : integer;
           End;
    time = record
             h  : shortint;
             m  : byte;
             s  : single;
           end;

Const
  DateAngle : single = 11.61290323;   // 360 / 31  =  angle to move per day
                                      // This takes care of all issues regarding
                                      // 28th/29th/30th/31st month-end
  DayAngle  : single = 51.42857143;   // 360 / 7

//  Following are the degrees each wheel turns in 1/10 sec
  EscapeTick: byte   = 12;
  FourthTick: byte   = 1;
  ThirdTick : single = 0.080645161;
  CentreTick: single = 0.010080645;

Var
  Alpha    : single;  // Hour Hand
  Beta     : single;  // Min Hand
  Gamma    : single;  // Sec Hand

  Delta    : single;  // Escape
  Epsilon  : single;  // Fourth
  Zeta     : single;  // Third
  Eta      : single;  // Centre

  Time_Now : time;
  Today    : date;
  Sys_Date : TDateTime;
  DayNum   : byte;

  Tick,
  WinderOut,
  MovementOpen,
  CaseClosed    : boolean;

  DateDegree,
  DayDegree     : single;


function  F_Mod(a: Double; b: Double): Double;
begin
  F_Mod := a - b * Trunc(a / b);      // Floating point MOD
end;

procedure TWatch.LoadImages;
begin
  LoadBitmap(Case1,   '01-CaseFront');
  LoadBitmap(MHand,   '02-MinHand');
  LoadBitmap(SHand,   '03-SecHand');
  LoadBitmap(HHand,   '04-HourHand');
  LoadBitmap(Dial,    '05-Dial');
  LoadBitmap(DayWhl,  '06-DayDial');
  LoadBitmap(DteWhl,  '07-DateWheel');
  LoadBitmap(Winder,  '08-Winder');
  LoadBitmap(WindIn,  '09-Winder');
  LoadBitmap(WindOut, '10-Winder');
  LoadBitmap(Rim,     '11-BackRim');
  LoadBitmap(Butt,    '12-Button');
  LoadBitmap(Back,    '13-Back');
  LoadBitmap(Case2,   '14-CaseFront');
  LoadBitmap(TopPlate,'15-TopPlates');  // 16 & 17 are not needed (although originally drawn)
  LoadBitmap(BalWhl1, '18-BalWheel');
  LoadBitmap(BalWhl2, '19-BalWheel');
  LoadBitmap(BalPlate,'20-BalPlate');
  LoadBitmap(Wheel3,  '21-Wheel-3');
  LoadBitmap(Wheel4,  '22-Wheel-4');
  LoadBitmap(Wheel,   '23-CentreWheel');
  LoadBitmap(Pallet1, '24-Pallet');
  LoadBitmap(Pallet2, '25-Pallet');
  LoadBitmap(Escape,  '26-EscapeWheel');
  LoadBitmap(DialBak, '27-DialBack');
end;

procedure TWatch.LoadBitmap(var ABitmap: TBGRABitmap; const ResourceName: string);
var
  res: TResourceStream;
begin
  res := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  if ABitmap <> nil then
    ABitmap.Free;
  ABitmap := TBGRABitmap.Create(res);
  res.Free;
end;

procedure TWatch.FormDestroy(Sender: TObject);
begin
  Case1.Free;
  MHand.Free;
  SHand.Free;
  HHand.Free;
  Dial.Free;
  DayWhl.Free;
  DteWhl.Free;
  Winder.Free;
  WindIn.Free;
  WindOut.Free;
  Rim.Free;
  Butt.Free;
  Back.Free;
  Case2.Free;
  TopPlate.Free;
  BalWhl1.Free;
  BalWhl2.Free;
  BalPlate.Free;
  Wheel3.Free;
  Wheel4.Free;
  Wheel.Free;
  Pallet1.Free;
  Pallet2.Free;
  Escape.Free;
  DialBak.Free;
end;

procedure TWatch.LatchClick(Sender: TObject);            // Latch is a TLabel
begin
  if WinderOut then               // only allow the movement to open if the winder has been released
    MovementOpen :=true;
  MovementClose.Enabled:=true;    // to enable cursor change
  Latch.Enabled:=false;           // to inhibit cursor change
  CaseCloseButton.Enabled:=false; //  - - - -  ditto  - - - - -
end;

procedure TWatch.MovementCloseClick(Sender: TObject);
begin
  MovementOpen := false;
  WinderOut    := false;
  MovementClose.Enabled:=false;   // to inhibit cursor change
  if WinderOut then
    Latch.Enabled:=true           // to enable cursor change
  else
    Latch.Enabled:=false;
  CaseCloseButton.Enabled:=true;  //  - - - -  ditto  - - - - -
end;

procedure TWatch.CaseCloseButtonClick(Sender: TObject);
begin
  if MovementOpen then           // prohibit closing the case
    begin                        // if the movement is open
    end
  else
    begin
      CaseClosed := true;
      WinderOut := false;
      CaseCloseButton.Enabled:=false;  // to inhibit cursor change
      Latch.Enabled:=false;            // to inhibit cursor change
    end;
end;

procedure TWatch.WinderButtonClick(Sender: TObject);
begin
  if WinderOut and (MovementOpen = false) then
    begin
      WinderOut := false;
      Latch.Enabled:=false;     // to inhibit cursor change
    end
  else
    if CaseClosed then          // on first click open the case
      begin                     // on second click pull winder out to release Movement
        CaseClosed := false;    // ie. Case is now Open
        WinderOut  := false;
        Latch.Enabled:=false;           // to inhibit cursor change
        CaseCloseButton.Enabled:=true;  // to enable cursor change
      end
    else
      begin
        WinderOut := true;
        Latch.Enabled:=true;            // to enable cursor change
        CaseCloseButton.Enabled:=true;  //  - -  ditto  - -
      end;
end;

procedure TWatch.TerminateClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure SetTranslucent(ThehWnd: Longint; Color: Longint; nTrans: Integer);
var
  attrib:longint;
begin
    {SetWindowLong and SetLayeredWindowAttributes are API functions, see MSDN for details }
    attrib := GetWindowLongA(ThehWnd, GWL_EXSTYLE);
    SetWindowLongA (ThehWnd, GWL_EXSTYLE, attrib Or WS_EX_LAYERED);

    {anything with color value color will completely disappear if flag = 1 or flag = 3  }
    SetLayeredWindowAttributes (ThehWnd, Color, nTrans,1);
end;

procedure ZeroWheelAngles;
begin
  Delta    := 0;
  Epsilon  := 0;
  Zeta     := 0;
  Eta      := 0;
end;

procedure CalcWheelAngles;
begin
  Delta    := F_MOD((Delta   + EscapeTick),360);
  Epsilon  := F_MOD((Epsilon + FourthTick),360);
  Zeta     := F_MOD((Zeta    + ThirdTick ),360);
  Eta      := F_MOD((Eta     + CentreTick),360);
end;

procedure CalcHandAngles;
begin
  Alpha := 30 * ((Time_Now.H Mod 12) + Time_Now.M/60 + Time_Now.S/3600);  // HourHand
  Beta  :=  6 * (Time_Now.M + Time_Now.S/60);                             // Min Hand
  Gamma :=  6 * (Time_Now.S);                                             // Sec Hand
end;

procedure Read_Date;
begin
  Sys_Date := Now;                   // Read system Date/Time to 'Freeze' it
  DayNum := DayOfWeek(Sys_Date);
  With Today do
    begin
      D := DayOf(Sys_Date);
      M := MonthOf(Sys_Date);
      Y := YearOf(Sys_Date);
    end;
  With Time_Now do
    begin
      H := HourOf(Sys_Date);
      M := MinuteOf(Sys_Date);
      S := SecondOf(Sys_Date) + MilliSecondOf(Sys_date)/1000;
    end;
  CalcHandAngles;
end;

procedure TWatch.CaseControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Case Button of
    mbLeft : begin
               ReleaseCapture;
               SendMessage(Watch.Handle, LM_SYSCOMMAND, 61458, 0);
             end;
  end;
end;

procedure TWatch.CaseControlRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.FillTransparent;                  // This clears all previous drawings

  DateDegree := (Today.d-1) * DateAngle;   //  angle to rotate the Day Number wheel

  DayDegree  := -(DayNum-1) * DayAngle;    //  angle to rotate the Day Name wheel
                                           //  takes Sunday as 0

  Bitmap.PutImage(274,   0, Back,      dmDrawWithTransparency); // Draw Case Back
  Bitmap.PutImage(559,  69, Butt,      dmDrawWithTransparency);
  if WinderOut then
    Bitmap.PutImage(580, 108, WindOut, dmDrawWithTransparency)
  else
    Bitmap.PutImage(570, 108, Winder,  dmDrawWithTransparency);

  Bitmap.PutImage(268,     0, Rim,     dmDrawWithTransparency);

// if the movement is visible, draw it - else - draw the dial and hands

  if MovementOpen then
    begin
      CalcWheelAngles;
      Bitmap.PutImage(0,   0, Case2,   dmDrawWithTransparency);
      Bitmap.PutImage(0,   0, DialBak, dmDrawWithTransparency);

      Bitmap.PutImageAngle(185, 209, Escape, Delta, 15, 15);          // clockwise (from back)

      if tick then                                                    // 'Tick' alternates between True and False
        Bitmap.PutImage(165, 168, Pallet1, dmDrawWithTransparency)    // every 1/10 second (on TTimer)
      else                                                            // thus the Pallet Fork changes
        Bitmap.PutImage(165, 168, Pallet2, dmDrawWithTransparency);

      Bitmap.PutImageAngle(150, 150, Wheel, -Zeta,   36, 36);         // anti-clock  (from back)  Minute hand
      Bitmap.PutImageAngle(150, 225, Wheel4, Epsilon,45, 45);
      Bitmap.PutImageAngle(132, 178, Wheel3,-Eta,    51, 51);         // anti-clock  (from back)  Seconds hand

      Bitmap.PutImage(0, 0, BalPlate, dmDrawWithTransparency);

      if tick then
        Bitmap.PutImage(170, 129, BalWhl1, dmDrawWithTransparency)
      else
        Bitmap.PutImage(170, 129, BalWhl2, dmDrawWithTransparency);

      Bitmap.PutImage(0, 0, TopPlate, dmDrawWithTransparency);
    end
  else
    begin
      Bitmap.PutImageAngle(453, 151, DteWhl, -DateDegree, 150, 150);
      Bitmap.PutImageAngle(504, 150, DayWhl,  DayDegree,   45,  45);
      Bitmap.PutImage(268, 0, Dial, dmDrawWithTransparency);

      Bitmap.PutImageAngle(450, 150, HHand, Alpha,150, 150);
      Bitmap.PutImageAngle(450, 150, MHand,  Beta,150, 150);
      Bitmap.PutImageAngle(450, 226, SHand, Gamma, 45,  45);

      if CaseClosed then
        Bitmap.PutImage(270, 0, Case1, dmDrawWithTransparency)
      else
        Bitmap.PutImage(0,   0, Case2, dmDrawWithTransparency);
    end;
end;

procedure TWatch.TenthSecTimer(Sender: TObject);
begin
  Read_Date;
  Tick := not Tick;              // change the direction of the Pallet Fork
  CaseControl.DiscardBitmap;
end;

procedure TWatch.FormCreate(Sender: TObject);
var
   transparency:longint;
begin
  {the color were going to make transparent -
   the White that the form background is set to}
  transparency:= clWhite;
  {call the function to do it}
  SetTranslucent(Watch.Handle, transparency, 0);

  Left := trunc((screen.Width-Watch.Width) / 2);
  Top  := 100;

  LoadImages;
  ZeroWheelAngles;
  CaseClosed   := true;
  MovementOpen := false;
  WinderOut    := false;
  Tick         := false;
  Read_Date;
end;

end.

