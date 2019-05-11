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

{ Blend procedures }

procedure OpacityBlend(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel := Mix(B, A, Level);
end;

{ Initialization callbacks }

procedure InitializeOperations(Add: TAddOperation);
begin
  Add('Invert', InvertOperation);
end;

procedure InitializeBlends(Add: TAddBlend);
begin
  Add('Opacity', OpacityBlend);
end;

end.

