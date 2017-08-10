unit variable_parameter_V2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls,
  BGRABitmap, BGRABitmapTypes, BGRAPath;

{
-------------------------------------
   Variable parameter



   How it works ?

   1)  ComputeBezierCurve or ComputeOpenedSpline functions (BGRAPath unit of BGRABitmap) are called to have array of TPointF
       in range [0..100 , 0..100]. Why 100 ? Because more give too much segments and less give not enough segments :)

   2)  After, each point of this array is reduced from range [0..100] to [0..1] to do velocity calculation througt time
       in your application ( sprite's move, audio volume, etc...) 

}


const
  // available predefined curves ID
  idcLinear = 0;
  idcStartFastEndSlow = 1;
  idcStartSlowEndFast = 2;
  idcSinusoid = 3;
  idcSinusoid2 = 4;

function CurveIDToString( ACurveID: word ): string;
function StringToCurveID( ACurveName: string ): word;

const
  CONTROL_POINT_MAX_VALUE = 100;
  COEFF_ADJUST = 1 / CONTROL_POINT_MAX_VALUE;

type

  TCustomParam = class;

type

  PPointF = ^TPointF;

  { TDataCurve }

  TDataCurve = class
  private
    FID: word;
    FName: string;
    FImage: TBGRABitmap;
    function GetPointCount: integer;
    procedure CreateExtremities;
  public
    Points: array of TPointF;
    constructor Create;
    destructor Destroy; override;
    // tools for curve construct
    procedure Clear(aCreateExtremities: boolean = True);

    procedure DeletePoint(aIndex: integer);
    procedure CopyPointsFrom(const aSource: array of TPointf);
    function ValidIndex(aIndex: integer): boolean;
    // Render curve on given TImage. You have to free it yourself
    procedure DrawOn(aImage: TImage);
    // Render curve on TBGRABitmap. Don't free it, it's done in TDataCurve.Destroy
    function GetBGRABitmapImage(aImageWidth, aImageHeight: integer;
      DrawInvertedCurve: boolean = True): TBGRABitmap;

    property Name: string read FName write FName;
    property ID: word read FID;
    property PointCount: integer read GetPointCount;
  end;

  { TVelocityCurve }
  TVelocityCurve = class
  private
    FDataCurveToUse: TDataCurve;
    FFinished,
    FInvert   : boolean;
    FX, FDuration, FYOrigin, FYTarget, FDeltaY: single;
    FCurrentIndexPoint1: integer;
    a, x1, y1, x2, y2: single;
    procedure GetSegmentCoor;
  public
    constructor Create;
    // initiate calculation
    procedure InitParameters(aCurrentValue, aTargetValue, aSeconds: single;
      aCurveID: word = idcLinear);
    // Computes and return the new value according to elapsed time
    function Compute(const AElapsedSec: single): single;
    property Finished: boolean read FFinished write FFinished;
  end;



  { TDataCurveList }
  TDataCurveList = class
  private
    FList: TList;
    FNumID: integer;
    function GetDataCurveCount: integer;
    function NextID: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    // Add new curve to the list. Return ID of the created curve
    function AddCurve(const aName: string; const Pts: array of TPointF): word;
    procedure DeleteByIndex(aIndex: integer);
    procedure DeleteByID(aIDCurve: word);
    procedure DeleteByName(ACurveName: string);
    function GetDataCurveByID(aID: word): TDataCurve;
    function GetDataCurveByIndex(aIndex: integer): TDataCurve;
    function NameAlreadyExist(const aCurveName: string): boolean;

    property Count: integer read GetDataCurveCount;
  end;


type

  TParamState = (psNO_CHANGE, psADD_CONSTANT, psUSE_CURVE);

  TCustomParam = class
    procedure OnElapse(const AElapsedSec: single); virtual; abstract;
  end;


  TFParam = class(TCustomParam)
  private
    FValue, FConstPerSecond: single;
    FState: TParamState;
    FCurve: TVelocityCurve;
  protected
    function GetValue: single;virtual;
    procedure SetValue(AValue: single);virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnElapse(const AElapsedSec: single); override;

    // use curve
    procedure ChangeTo(aNewValue, aSecond: single; aCurveID: word = idcLinear); virtual;
    // add a constant per second
    procedure AddConstant(aConstPerSecond: single);
    // Current value of the parameter. Setting a value, stop an "ChangeTo" or "AddConstant" action.
    property Value: single read GetValue write SetValue;
    property State: TParamState read FState;
  end;

type
  { TBoundedFParam }
  // parameter with boundary

  TBoundedFParam = class(TFParam)
  private
    function GetpcValue: single;
    procedure SetpcValue(AValue: single);
  protected
    procedure SetValue(AValue: single);override;
    procedure ApplyBounds( var AValue: single );
  public
    MinValue, MaxValue: single;
    Loop : boolean;           // if TRUE, value can loop between bounds (usefull for i.e. [0..360] angle)
    procedure SetBoundary(aMin, aMax: single; aLoop: boolean=FALSE );
    procedure OnElapse(const AElapsedSec: single); override;
    procedure ChangeTo(aNewValue, aSecond: single; aCurveID: word = idcLinear); override;
    // percentage range is [0..100], 0% is MinValue and 100% is MaxValue
    function PercentToValue(aPercentage: single): single;
    function ValueToPercent(aValue: single): single;
    function pcRandomValueBetween(PercentageMin, PercentageMax: single): single;
    procedure pcChangeTo(aNewPercentValue, aSecond: single; aCurveID: word = idcLinear);
    property pcValue: single read GetpcValue write SetpcValue;
  end;

function CreateBoundedFParam(Min, Max: single; Loop: boolean=FALSE ): TBoundedFParam;


type

  { TBGRAParam }

  TBGRAParam = class(TCustomParam)
  private
    function GetBGRAPixel: TBGRAPixel;
    function GetState: TParamState;
    procedure SetBGRAPixel(aValue: TBGRAPixel);
  public
    Red, Green, Blue, Alpha: TBoundedFParam;
    constructor Create;
    destructor Destroy; override;
    procedure OnElapse(const AElapsedSec: single); override;
    procedure ChangeTo(aNewValue: TBGRAPixel; aSeconds: single; aCurveID: word = idcLinear);
    property Value: TBGRAPixel read GetBGRAPixel write SetBGRAPixel;
    property State: TParamState read GetState;
  end;

  { TPointFParam }

  TPointFParam = class(TCustomParam)
  private

    function GetPointF: TPointF;
    function GetState: TParamState;
    procedure SetPointF(AValue: TPointF);
  public
    x, y : TFParam;
    constructor Create;
    destructor Destroy; override;
    procedure OnElapse(const AElapsedSec: single); override;
    procedure ChangeTo(aNewValue: TPointF; aSeconds: single; aCurveID: word = idcLinear);
    property Value: TPointF read GetPointF write SetPointF;
    property State: TParamState read GetState;
  end;



implementation
var
  DataCurveList: TDataCurveList;

function CurveIDToString(ACurveID: word): string;
begin
 case ACurveID of
  idcStartFastEndSlow : Result := 'StartFastEndSlow';
  idcStartSlowEndFast : Result := 'StartSlowEndFast';
  idcSinusoid : Result := 'Sinusoid';
  idcSinusoid2 : Result := 'Sinusoid2'
  else Result := 'Linear';
 end;
end;

function StringToCurveID(ACurveName: string): word;
begin
 case LowerCase( ACurveName ) of
  'startfastendslow': Result := idcStartFastEndSlow;
  'startslowendfast': Result := idcStartSlowEndFast;
  'sinusoid': Result := idcSinusoid;
  'sinusoid2': Result := idcSinusoid2
  else Result := idcLinear;
 end;
end;

function CreateBoundedFParam(Min, Max: single; Loop: boolean): TBoundedFParam;
begin
  Result := TBoundedFParam.Create;
  Result.SetBoundary(Min, Max);
  Result.Loop := Loop;
end;


{ TPointFParam }

function TPointFParam.GetPointF: TPointF;
begin
 Result.x := x.Value;
 Result.y := y.Value;
end;

function TPointFParam.GetState: TParamState;
begin
  if (x.State = psNO_CHANGE) and (y.State = psNO_CHANGE) then
    Result := psNO_CHANGE
    else if (x.State = psADD_CONSTANT) and (y.State = psADD_CONSTANT) then
    Result := psADD_CONSTANT
    else Result := psUSE_CURVE;
end;

procedure TPointFParam.SetPointF(AValue: TPointF);
begin
 x.Value := AValue.x;
 y.Value := AValue.y;
end;

constructor TPointFParam.Create;
begin
 x := TFParam.Create;
 y := TFParam.Create;
end;

destructor TPointFParam.Destroy;
begin
 FreeAndNil( x );
 FreeAndNil( y );
 inherited Destroy;
end;

procedure TPointFParam.OnElapse(const AElapsedSec: single);
begin
 x.OnElapse( AElapsedSec );
 y.OnElapse( AElapsedSec );
end;

procedure TPointFParam.ChangeTo(aNewValue: TPointF; aSeconds: single; aCurveID: word);
begin
 x.ChangeTo(aNewValue.x, aSeconds, aCurveID);
 y.ChangeTo(aNewValue.y, aSeconds, aCurveID);
end;

{ TBGRAParam }

function TBGRAParam.GetBGRAPixel: TBGRAPixel;
begin
  Result.red := round(red.Value);
  Result.green := round(green.Value);
  Result.blue := round(blue.Value);
  Result.alpha := round(alpha.Value);
end;

function TBGRAParam.GetState: TParamState;
begin
 if (Red.State = psNO_CHANGE) and (Green.State = psNO_CHANGE) and
    (Blue.State = psNO_CHANGE) and (Alpha.State = psNO_CHANGE) then
   Result := psNO_CHANGE
   else if (Red.State = psADD_CONSTANT) and (Green.State = psADD_CONSTANT) and
    (Blue.State = psADD_CONSTANT) and (Alpha.State = psADD_CONSTANT) then
   Result := psADD_CONSTANT
   else Result := psUSE_CURVE;
end;

procedure TBGRAParam.SetBGRAPixel(aValue: TBGRAPixel);
begin
  red.Value := aValue.red;
  green.Value := aValue.green;
  blue.Value := aValue.blue;
  alpha.Value := aValue.alpha;
end;

constructor TBGRAParam.Create;
begin
  red := CreateBoundedFParam(0, 255);
  green := CreateBoundedFParam(0, 255);
  blue := CreateBoundedFParam(0, 255);
  alpha := CreateBoundedFParam(0, 255);
end;

destructor TBGRAParam.Destroy;
begin
 FreeAndNil( red );
 FreeAndNil( green );
 FreeAndNil( blue );
 FreeAndNil( alpha );
 inherited Destroy;
end;

procedure TBGRAParam.OnElapse(const AElapsedSec: single);
begin
  red.OnElapse(AElapsedSec);
  green.OnElapse(AElapsedSec);
  blue.OnElapse(AElapsedSec);
  alpha.OnElapse(AElapsedSec);
end;

procedure TBGRAParam.ChangeTo(aNewValue: TBGRAPixel; aSeconds: single; aCurveID: word);
begin
  red.ChangeTo(aNewValue.red, aSeconds, aCurveID);
  green.ChangeTo(aNewValue.green, aSeconds, aCurveID);
  blue.ChangeTo(aNewValue.blue, aSeconds, aCurveID);
  alpha.ChangeTo(aNewValue.alpha, aSeconds, aCurveID);
end;

{ TDataCurve }

function TDataCurve.GetPointCount: integer;
begin
  Result := Length(Points);
end;

procedure TDataCurve.CreateExtremities;
begin
  SetLength(Points, 2);
  Points[0] := PointF(0, 1);
  Points[1] := PointF(1, 0);
end;

constructor TDataCurve.Create;
begin
  inherited Create;
  CreateExtremities;
  FImage := nil;
end;

destructor TDataCurve.Destroy;
begin
  Clear(False);
  if FImage <> nil then
    FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TDataCurve.Clear(aCreateExtremities: boolean);
begin
  SetLength(Points, 0);
  if aCreateExtremities then
    CreateExtremities;
end;

procedure TDataCurve.DeletePoint(aIndex: integer);
var
  i: integer;
begin
  if (aIndex < 1) or (aIndex > Length(Points) - 2) then
    exit;
  for i := GetPointCount - 1 downto aIndex do
    Points[i - 1] := Points[i];
  SetLength(Points, Length(Points) - 1);
end;

procedure TDataCurve.CopyPointsFrom(const aSource: array of TPointf);
var
  i: integer;
begin
  Clear(False);
  SetLength(Points, Length(aSource));
  for i := 0 to Length(aSource) - 1 do
    Points[i] := aSource[i];
end;

function TDataCurve.ValidIndex(aIndex: integer): boolean;
begin
  Result := (aIndex >= 0) and (aIndex < GetPointCount);
end;

procedure TDataCurve.DrawOn(aImage: TImage);
var
  x1, y1, x2, y2, i: integer;
  cline, clineinvert: TColor;
begin
  with aImage.Canvas do
  begin
    // background
    Brush.Color := rgbToColor(50, 20, 20);
    FillRect(0, 0, Width, Height);
    cline := rgbToColor(255, 140, 0);
    clineinvert := rgbToColor(20, 80, 100);
    // inverted curve
    Pen.Color := clineinvert;
    for i := 1 to GetPointCount - 1 do
    begin
      with Points[i - 1] do
      begin
        x1 := System.round(x * Width);
        y1 := System.round(Height - y * Height);
      end;
      with Points[i] do
      begin
        x2 := System.round(x * Width);
        y2 := System.round(Height - y * Height);
      end;
      Line(x1, y1, x2, y2);
    end;
    // axis
    Pen.Color := rgbToColor(150, 100, 100);
    Line(0, Height - 1, Width, Height - 1);
    Line(0, Height - 2, Width, Height - 2);
    Line(0, 0, 0, Height);
    Line(1, 0, 1, Height);
    // normal curve
    Pen.Color := cline;
    for i := 1 to GetPointCount - 1 do
    begin
      with Points[i - 1] do
      begin
        x1 := System.round(x * Width);
        y1 := System.round(y * Height);
      end;
      with Points[i] do
      begin
        x2 := System.round(x * Width);
        y2 := System.round(y * Height);
      end;
      Line(x1, y1, x2, y2);
    end;
  end;
end;

function TDataCurve.GetBGRABitmapImage(aImageWidth, aImageHeight: integer;
  DrawInvertedCurve: boolean): TBGRABitmap;
var
  x1, y1, x2, y2, w: single;
  i: integer;
  cline, clineinvert, c: TBGRAPixel;
begin
  if FImage <> nil then
  begin
    if (FImage.Width = aImageWidth) and (FImage.Height = aImageHeight) then
    begin
      Result := FImage;
      exit;
    end
    else
      FreeAndNil(FImage);
  end;

  FImage := TBGRABitmap.Create(aImageWidth, aImageHeight, BGRA(50, 20, 20));
  cline := BGRA(255, 140, 0);
  clineinvert := BGRA(60, 24, 24); // BGRA(20, 80, 100);
  with FImage do
  begin
    if (Width > 20) and (Height > 20) then
      w := 1.5
    else
      w := 0.5;
    // inverted curve
    if DrawInvertedCurve then
    begin
      for i := 1 to GetPointCount - 1 do
      begin
        with Points[i - 1] do
        begin
          x1 := x * Width;
          y1 := Height * (1 - y);
        end;
        with Points[i] do
        begin
          x2 := x * Width;
          y2 := Height * (1 - y);
        end;
        DrawLineAntialias(x1, y1, x2, y2, clineinvert, 2);
      end;
    end;
    // axis
    c := BGRA(150, 100, 100);
    HorizLine(0, Height - 1, Width, c, dmSet);
    DrawLine(0, 0, 0, Height, c, True, dmSet);
    if (Width > 20) and (Height > 20) then
    begin
      HorizLine(0, Height - 2, Width, c, dmSet);
      DrawLine(1, 0, 1, Height, c, True, dmSet);
    end;
    // normal curve
    for i := 1 to GetPointCount - 1 do
    begin
      with Points[i - 1] do
      begin
        x1 := x * Width;
        y1 := y * Height;
      end;
      with Points[i] do
      begin
        x2 := x * Width;
        y2 := y * Height;
      end;
      DrawLineAntialias(x1, y1, x2, y2, cline, w);
    end;
  end;
  Result := FImage;
end;

{ TBoundedFParam }

function TBoundedFParam.GetpcValue: single;
begin
  Result := ValueToPercent(FValue);
end;

procedure TBoundedFParam.SetpcValue(AValue: single);
var v: single;
begin
  v := PercentToValue(AValue);
  SetValue(v);
end;

procedure TBoundedFParam.SetValue(AValue: single);
begin
 ApplyBounds( AValue );
 inherited SetValue( AValue );
end;

procedure TBoundedFParam.ApplyBounds(var AValue: single);
var delta: single;
begin
 if Loop
  then begin
    delta := MaxValue - MinValue;
    while AValue < MinValue do AValue += delta;
    while AValue > MaxValue do AValue -= delta;
  end else begin
    if AValue < MinValue
      then AValue := MinValue
      else if AValue > MaxValue
             then AValue := MaxValue;
  end;
end;

procedure TBoundedFParam.SetBoundary(aMin, aMax: single; aLoop: boolean);
begin
  if aMin > aMax then
  begin
    MinValue := aMax;
    MaxValue := aMin;
  end
  else
  begin
    MinValue := aMin;
    MaxValue := aMax;
  end;

  Loop := aLoop;
  ApplyBounds( FValue );
end;

function TBoundedFParam.PercentToValue(aPercentage: single): single;
var
  delta: single;
begin
  delta := MaxValue - MinValue;
  Result := delta * aPercentage * 0.01 + MinValue;
end;

function TBoundedFParam.ValueToPercent(aValue: single): single;
begin
  Result := (aValue - MinValue) / (MaxValue - MinValue) * 100.0;
end;

function TBoundedFParam.pcRandomValueBetween(PercentageMin, PercentageMax:
  single): single;
var
  p: single;
begin
  p := random(round((PercentageMax - PercentageMin) * 10000.0)) *
    0.00001 + PercentageMin;
  Result := PercentToValue(p);
end;

procedure TBoundedFParam.pcChangeTo(aNewPercentValue, aSecond: single; aCurveID: word);
var v: single;
begin
  v := PercentToValue(aNewPercentValue);
  ChangeTo(v, aSecond, aCurveID);
end;

procedure TBoundedFParam.ChangeTo(aNewValue, aSecond: single; aCurveID: word);
begin
 ApplyBounds( aNewValue );
 inherited ChangeTo( aNewValue, aSecond, aCurveID);
end;

procedure TBoundedFParam.OnElapse(const AElapsedSec: single);
begin
  case FState of
    psADD_CONSTANT:
    begin
      FValue += FConstPerSecond * AElapsedSec;
      if Loop
        then ApplyBounds( FValue )
        else begin
              if FValue <= MinValue then
                begin
                 FValue := MinValue;
                 FState := psNO_CHANGE;
                end
                else if FValue >= MaxValue then
                begin
                 FValue := MaxValue;
                 FState := psNO_CHANGE;
                end;
             end;
    end;
    psUSE_CURVE:
    begin
      FValue := FCurve.Compute(AElapsedSec);
      if FCurve.Finished then
        FState := psNO_CHANGE;
    end;
  end;
end;


{ TFParam }


function TFParam.GetValue: single;
begin
  Result := FValue;
end;

procedure TFParam.SetValue(AValue: single);
begin
  FValue := AValue;
  FState := psNO_CHANGE;
end;

constructor TFParam.Create;
begin
  FState := psNO_CHANGE;
  FValue := 0.0;
  FConstPerSecond := 0.0;
  FCurve := TVelocityCurve.Create;
end;

destructor TFParam.Destroy;
begin
  FreeAndNil(FCurve);
  inherited Destroy;
end;

procedure TFParam.ChangeTo(aNewValue, aSecond: single; aCurveID: word);
begin
  if aSecond <= 0 then
  begin
    SetValue(aNewValue);
    exit;
  end;

  if aNewValue <> FValue then
  begin
    FState := psUSE_CURVE;
    FCurve.InitParameters(FValue, aNewValue, aSecond, aCurveID);
  end
  else FState := psNO_CHANGE;
end;

procedure TFParam.AddConstant(aConstPerSecond: single);
begin
  if aConstPerSecond <> 0 then
  begin
    FState := psADD_CONSTANT;
    FConstPerSecond := aConstPerSecond;
  end
  else FState := psNO_CHANGE;
end;

procedure TFParam.OnElapse(const AElapsedSec: single);
begin
  case FState of
    psADD_CONSTANT: FValue += FConstPerSecond * AElapsedSec;
    psUSE_CURVE:
    begin
      FValue := FCurve.Compute(AElapsedSec);
      if FCurve.FFinished then
        FState := psNO_CHANGE;
    end;
  end;
end;

{ TVelocityCurve }

constructor TVelocityCurve.Create;
begin
  inherited Create;
  FFinished := True;
  FInvert := FALSE;
end;

procedure TVelocityCurve.GetSegmentCoor;
var
  fp, fp1: TPointF;
begin
  fp := FDataCurveToUse.Points[FCurrentIndexPoint1];
  fp1 := FDataCurveToUse.Points[FCurrentIndexPoint1 + 1];
  x1 := fp.x;
  x2 := fp1.x;

 if FInvert then
  begin
    y1 := fp.y;
    y2 := fp1.y;
  end
  else
  begin
    y1 := 1 - fp.y;
    y2 := 1 - fp1.y;
  end;

  a := (y2 - y1) / (x2 - x1);
end;

procedure TVelocityCurve.InitParameters(aCurrentValue, aTargetValue, aSeconds: single;
  aCurveID: word);
begin
  FX := 0;
  FYOrigin := aCurrentValue;
  FYTarget := aTargetValue;

  FInvert := aCurrentValue > aTargetValue;

  FDeltaY := FYTarget - FYOrigin;
  FDuration := aSeconds;
  FFinished := aSeconds = 0.0;
  FCurrentIndexPoint1 := 0;

  FDataCurveToUse := DataCurveList.GetDataCurveByID(aCurveID);
  if FDataCurveToUse = nil then
    raise Exception.Create('Velocities curves error ! no curve available for ID=' +
      IntToStr(aCurveID) + ')');

  GetSegmentCoor;
end;

function TVelocityCurve.Compute(const AElapsedSec: single): single;
var
  xcurve, ycurve: single;
begin
  FX += AElapsedSec;
  if (FX >= FDuration) or (FDuration <= 0) then
    FFinished := True;
  if FFinished then
  begin
    Result := FYTarget;
    exit;
  end
  else
  begin
    xcurve := FX / FDuration;
    while xcurve > x2 do
    begin
      Inc(FCurrentIndexPoint1);
      GetSegmentCoor;
    end;

   ycurve := a * (xcurve - x1) + y1;
   if FInvert then ycurve := 1 - ycurve;

   Result := FYOrigin + ycurve * FDeltaY;
  end;
end;




//------------------------------------------------------------------------------------

//                            TDataCurveList




{ TDataCurveList }

constructor TDataCurveList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FNumID := -1;
end;

destructor TDataCurveList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TDataCurveList.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    TDataCurve(FList.Items[i]).Free;
  FList.Clear;
  FNumID := -1;
end;

function TDataCurveList.GetDataCurveCount: integer;
begin
  Result := FList.Count;
end;

function TDataCurveList.NextID: integer;
begin
  Inc(FNumID);
  Result := FNumID;
end;


function TDataCurveList.AddCurve(const aName: string; const Pts: array of TPointF): word;
var
  o: TDataCurve;
  i: integer;
begin
  o := TDataCurve.Create;
  o.Name := aName;
  o.FID := NextID;
  o.CopyPointsFrom(Pts);
  // adjust values to range [0..1]
  for i := 0 to Length(Pts) - 1 do
    o.Points[i] := o.Points[i] * COEFF_ADJUST;
  FList.Add(o);
  Result := o.ID;
end;

procedure TDataCurveList.DeleteByIndex(aIndex: integer);
begin
  if (aIndex < 0) or (aIndex >= FList.Count) then
    exit;
  TDataCurve(FList.Items[aIndex]).Free;
  FList.Delete(aIndex);
end;

procedure TDataCurveList.DeleteByID(aIDCurve: word);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    if GetDataCurveByIndex(i).ID = aIDCurve then
    begin
      DeleteByIndex(i);
      exit;
    end;
end;

procedure TDataCurveList.DeleteByName(ACurveName: string);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    if GetDataCurveByIndex(i).Name = ACurveName then
    begin
      DeleteByIndex(i);
      exit;
    end;
end;

function TDataCurveList.GetDataCurveByID(aID: word): TDataCurve;
var
  i: integer;
  dc: TDataCurve;
begin
  Result := GetDataCurveByIndex(0); // Linear curve by default
  for i := 0 to FList.Count - 1 do
  begin
    dc := GetDataCurveByIndex(i);
    if dc.ID = aID then
    begin
      Result := dc;
      exit;
    end;
  end;
end;

function TDataCurveList.GetDataCurveByIndex(aIndex: integer): TDataCurve;
begin
  if (aIndex < 0) or (aIndex >= FList.Count) then
    Result := TDataCurve(FList.Items[0])  // Linear curve by default
  else
    Result := TDataCurve(FList.Items[aIndex]);
end;

function TDataCurveList.NameAlreadyExist(const aCurveName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FList.Count - 1 do
    if GetDataCurveByIndex(i).FName = aCurveName then
    begin
      Result := True;
      exit;
    end;
end;


initialization

  DataCurveList := TDataCurveList.Create;

  DataCurveList.AddCurve('Linear', [PointF(0, 100), PointF(100, 0)]);

  DataCurveList.AddCurve('StartFastEndSlow', ComputeBezierCurve(
    BezierCurve(PointF(0, 100), PointF(30.405405045, 0), PointF(100, 0))));

  DataCurveList.AddCurve('StartSlowEndFast', ComputeBezierCurve(
    BezierCurve(PointF(0, 100), PointF(80.405403137, 99.864868164), PointF(100, 0))));

  DataCurveList.AddCurve('Sinusoid', ComputeBezierCurve(
    BezierCurve(PointF(0, 100), PointF(36.486488342, 99.864868164),
    PointF(63.513511658, 0), PointF(100, 0))));




  DataCurveList.AddCurve('Sinusoid2', ComputeBezierCurve(
    BezierCurve(PointF(0, 100), PointF(53.378379822, 99.324325562),
    PointF(46.621620178, 0.67567569), PointF(100, 0))));

finalization
  FreeAndNil(DataCurveList);
end.
