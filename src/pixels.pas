unit Pixels;

{$mode delphi}

interface

{ The TPixel type }

type
  {$ifdef linux}
  TPixel = record R, G, B, A: Byte; end;
  {$else}
  TPixel = record B, G, R, A: Byte; end;
  {$endif}
  PPixel = ^TPixel;

{ Operation and blend registration functions }

  TPixelOperation = procedure(var Pixel: TPixel; X, Y: Integer; Level: Single);
  TPixelBlend = procedure(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);

  TAddOperation = procedure(const Name: string; Proc: TPixelOperation);
  TAddBlend = procedure(const Name: string; Proc: TPixelBlend);

{ Initialization callbacks }

procedure InitializeOperations(Add: TAddOperation);
procedure InitializeBlends(Add: TAddBlend);

{ Globally set width and height of the image being processed by operations or blends }

var
  ImageWidth, ImageHeight: Integer;

implementation

{ Helper functions }

const
  {$ifdef linux}
  White: TPixel = (R: $FF; G: $FF; B: $FF; A: $FF);
  Black: TPixel = (R: 0; G: 0; B: 0; A: $FF);
  {$else}
  White: TPixel = (B: $FF; G: $FF; R: $FF; A: $FF);
  Black: TPixel = (B: 0; G: 0; R: 0; A: $FF);
  {$endif}

function RoundByte(Value: Single): Byte; inline;
begin
  if Value > $FF then
    Result := $FF
  else if Value < 0 then
    Result := 0
  else
    Result := Round(Value);
end;

function Mix(A, B: TPixel; Percent: Single): TPixel; inline;
var
  Invert: Single;
begin
  if Percent < 0.001 then
    Result := A
  else if Percent > 0.999 then
    Result := B
  else
  begin
    Invert := 1 - Percent;
    Result.B := RoundByte(B.B * Percent + A.B * Invert);
    Result.G := RoundByte(B.G * Percent + A.G * Invert);
    Result.R := RoundByte(B.R * Percent + A.R * Invert);
    Result.A := RoundByte(B.A * Percent + A.A * Invert);
  end;
end;

function Hue(Value: Single): TPixel;
const
  Step = 1 / 6;
var
  R, G, B: Single;
begin
  R := 0;
  G := 0;
  B := 0;
  if Value < 0 then
    R := 1
  else if Value < 1 * Step then
  begin
    R := 1;
    G := Value / Step;
  end
  else if Value < 2 * Step then
  begin
    R := 1 - (Value - 1 * Step) / Step;
    G := 1;
  end
  else if Value < 3 * Step then
  begin
    G := 1;
    B := (Value - 2 * Step) / Step;
  end
  else if Value < 4 * Step then
  begin
    G := 1 - (Value - 3 * Step) / Step;
    B :=  1;
  end
  else if Value < 5 * Step then
  begin
    B :=  1;
    R := (Value - 4 * Step) / Step;
  end
  else if Value < 6 * Step then
  begin
    B := 1 - (Value - 5 * Step) / Step;
    R := 1;
  end
  else
    R := 1;
  Result.R := RoundByte(R * $FF);
  Result.G := RoundByte(G * $FF);
  Result.B := RoundByte(B * $FF);
  Result.A := $FF;
end;

{ Operation procedures }

procedure InvertOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
var
  P: TPixel;
begin
  P.B := not Pixel.B;
  P.G := not Pixel.G;
  P.R := not Pixel.R;
  P.A := Pixel.A;
  Pixel := Mix(Pixel, P, Level);
end;

procedure SaturationOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
var
  P: TPixel;
  D: Byte;
begin
  D := RoundByte(Pixel.B * 0.863 + Pixel.G * 0.275 + Pixel.R * 0.510);
  P.B := D;
  P.G := D;
  P.R := D;
  P.A := Pixel.A;
  Pixel := Mix(P, Pixel, Level);
end;

procedure HueOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
var
  P: TPixel;
  A: Byte;
begin
  P := Hue(Level);
  A := Pixel.A;
  Pixel := Mix(P, White, (Pixel.B * 0.863 + Pixel.G * 0.275 + Pixel.R * 0.510) / $FF);
  Pixel.A := A;
end;

procedure BlackOrWhiteOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
var
  A: Byte;
begin
  A := Pixel.A;
  if Pixel.R + Pixel.G + Pixel.B > Level * 3 * $FF then
    Pixel := White
  else
    Pixel := Black;
  Pixel.A := A;
end;

procedure BrightenOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel.B := RoundByte(Pixel.B + Level * $FF);
  Pixel.G := RoundByte(Pixel.G + Level * $FF);
  Pixel.R := RoundByte(Pixel.R + Level * $FF);
end;

procedure ContrastOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
var
  B, G, R: Single;
begin
  B := (Pixel.B / $FF - 0.5) * 4 * Level;
  G := (Pixel.G / $FF - 0.5) * 4 * Level;
  R := (Pixel.R / $FF - 0.5) * 4 * Level;
  Pixel.B := RoundByte(Pixel.B + B * $FF);
  Pixel.G := RoundByte(Pixel.G + G * $FF);
  Pixel.R := RoundByte(Pixel.R + R * $FF);
end;

procedure DarkenOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel.B := RoundByte(Pixel.B - Level * $FF);
  Pixel.G := RoundByte(Pixel.G - Level * $FF);
  Pixel.R := RoundByte(Pixel.R - Level * $FF);
end;

procedure RedOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel.R := RoundByte(Pixel.R + (Level - 0.5) * $FF * 2);
end;

procedure GreenOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel.G := RoundByte(Pixel.G + (Level - 0.5) * $FF * 2);
end;

procedure BlueOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel.B := RoundByte(Pixel.B + (Level - 0.5) * $FF * 2);
end;

procedure AlphaOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel.A := RoundByte(Pixel.A * Level);
end;

{ Blend procedures }

procedure OpacityBlend(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel := Mix(B, A, Level);
end;

var
  FastR1: Integer = 13;
  FastR2: Integer = 31;

function FastRandom(Range: Integer): Integer;
begin
  FastR1 := 18030 * (FastR1 and $FFFF) + (FastR1 shr 16);
  FastR2 := 30903 * (FastR2 and $FFFF) + (FastR2 shr 16);
  if Range < 2 then
    Result := 0
  else
    Result := (FastR1 shr 16 + (FastR2 and $FFFF)) mod Range;
end;

procedure FastRandomSeed(Seed: Integer);
begin
  FastR1 := Seed;
  FastR2 := 31;
  FastR2 := FastRandom(High(Word));
end;

procedure DisolveBlend(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  if Level < 0.001 then
    Pixel := B
  else if Level > 0.999 then
    Pixel := A
  else
  begin
    if (X = 0) and (Y = 0) then
      FastRandomSeed(ImageWidth * ImageHeight);
    if FastRandom(10000) < Level * 10000 then
      Pixel := A
    else
      Pixel := B;
  end;
end;

procedure BlockBlend(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
const
  BlockSize = 50;
var
  Fade: Single;
begin
  if Level < 0.001 then
    Pixel := B
  else if Level > 0.999 then
    Pixel := A
  else
  begin
    Inc(X, BlockSize);
    Inc(Y, BlockSize);
    FastRandomSeed((X div BlockSize) + (Y div BlockSize) * (X div BlockSize) * 73 +
      ImageWidth * 31 + ImageHeight * 57 * ImageWidth * 31);
    Fade := Level + 0.2 - FastRandom(10000) / 10000;
    if Level < 0.5 then
      Fade := Fade * (Level / 0.5);
    if Fade < 0.001 then
      Pixel := B
    else if Fade < 0.2 then
      Pixel := Mix(B, A, Fade / 0.2)
    else
      Pixel := A;
  end;
end;

procedure MultiplyBlend(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
var
  P: TPixel;
begin
  P.B := RoundByte(A.B * B.B / $FF);
  P.G := RoundByte(A.G * B.G / $FF);
  P.R := RoundByte(A.R * B.R / $FF);
  P.A := RoundByte(A.A * B.A / $FF);
  Pixel := Mix(B, P, Level);
end;

procedure AdditionBlend(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
var
  P: TPixel;
begin
  P.B := RoundByte(A.B + B.B);
  P.G := RoundByte(A.G + B.G);
  P.R := RoundByte(A.R + B.R);
  P.A := RoundByte(A.A + B.A);
  Pixel := Mix(B, P, Level);
end;

procedure SubtractionBlend(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
var
  P: TPixel;
begin
  P.B := RoundByte(B.B - A.B);
  P.G := RoundByte(B.G - A.G);
  P.R := RoundByte(B.R - A.R);
  P.A := RoundByte(B.A - A.A);
  Pixel := Mix(B, P, Level);
end;

procedure WipeBlend(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  if X < ImageWidth * Level then
    Pixel := A
  else
    Pixel := B;
end;

procedure CircleBlend(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
var
  D, W, H: Single;
begin
  D := ImageWidth;
  if ImageHeight > D then
    D := ImageHeight;
  D := D * 1.42 * Level / 2;
  W := X - ImageWidth / 2;
  H := Y - ImageHeight / 2;
  if Sqrt(W * W + H * H) < D then
    Pixel := A
  else
    Pixel := B;
end;

{ Initialization callbacks }

procedure InitializeOperations(Add: TAddOperation);
begin
  Add('Red Channel', RedOperation);
  Add('Green Channel', GreenOperation);
  Add('Blue Channel', BlueOperation);
  Add('Saturation', SaturationOperation);
  Add('Alpha Channel', AlphaOperation);
  Add('Black or White', BlackOrWhiteOperation);
  Add('Brighten', BrightenOperation);
  Add('Contrast', ContrastOperation);
  Add('Darken', DarkenOperation);
  Add('Invert', InvertOperation);
  Add('Hue', HueOperation);
end;

procedure InitializeBlends(Add: TAddBlend);
begin
  Add('Opacity', OpacityBlend);
  Add('Disolve', DisolveBlend);
  Add('Multiply', MultiplyBlend);
  Add('Addition', AdditionBlend);
  Add('Subtraction', SubtractionBlend);
  Add('Wipe', WipeBlend);
  Add('Circle', CircleBlend);
  Add('Blocks', BlockBlend);
end;

end.

