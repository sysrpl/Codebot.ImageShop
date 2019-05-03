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

procedure WhiteOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel := Mix(Pixel, White, Level);
end;

procedure OpacityBlend(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel := Mix(B, A, Level);
end;

{ Initialization callbacks }

procedure InitializeOperations(Add: TAddOperation);
begin
  Add('White', WhiteOperation);
end;

procedure InitializeBlends(Add: TAddBlend);
begin
  Add('Opacity', OpacityBlend);
end;

end.

