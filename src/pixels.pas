unit Pixels;

{$mode delphi}

interface

{ The TPixel type }

type
  TPixel = record B, G, R, A: Byte; end;
	PPixel = ^TPixel;

{ Operation and blend registration functions }

  TPixelOperationProc = procedure(var Pixel: TPixel; X, Y: Integer; Level: Single);
  TPixelOperation = record
    Name: string;
    Proc: TPixelOperationProc;
  end;
	TPixelOperations = array of TPixelOperation;

  TPixelBlendProc = procedure(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
  TPixelBlend = record
    Name: string;
    Proc: TPixelBlendProc;
  end;
  TPixelBlends = array of TPixelBlend;

function PixelOperations(Index: Integer; out Operation: TPixelOperation): Boolean;
function PixelBlends(Index: Integer; out Blend: TPixelBlend): Boolean;

{ Globally set width and height of the image being processed by operations or blends }

var
  ImageWidth, ImageHeight: Integer;

implementation

{ Helper functions }

const
  White: TPixel = (B: $FF; G: $FF; R: $FF; A: $FF);
  Black: TPixel = (B: 0; G: 0; R: 0; A: $FF);

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
  P.A := $FF;
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

{procedure AlphaOperation(var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel.A := RoundByte(Pixel.A * Level);
end;}

{ Blend procedures }

procedure OpacityBlend(const A, B: TPixel; var Pixel: TPixel; X, Y: Integer; Level: Single);
begin
  Pixel := Mix(B, A, Level);
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

{ Operation and blend registration functions }

function PixelOperations(Index: Integer; out Operation: TPixelOperation): Boolean;
begin
  Operation.Name := '';
  Operation.Proc := nil;
	Result := True;
	case Index of
  	0:
      begin
				Operation.Name := 'Red Channel';
        Operation.Proc := RedOperation;
      end;
  	1:
      begin
				Operation.Name := 'Green Channel';
        Operation.Proc := GreenOperation;
      end;
  	2:
      begin
				Operation.Name := 'Blue Channel';
        Operation.Proc := BlueOperation;
      end;
  	3:
      begin
				Operation.Name := 'Saturation';
        Operation.Proc := SaturationOperation;
      end;
  	4:
      begin
				Operation.Name := 'Black or White';
        Operation.Proc := BlackOrWhiteOperation;
      end;
  	5:
      begin
				Operation.Name := 'Brighten';
        Operation.Proc := BrightenOperation;
      end;
  	6:
      begin
				Operation.Name := 'Contrast';
        Operation.Proc := ContrastOperation;
      end;
  	7:
      begin
				Operation.Name := 'Darken';
        Operation.Proc := DarkenOperation;
      end;
  	8:
      begin
				Operation.Name := 'Invert';
        Operation.Proc := InvertOperation;
      end;
    9:
      begin
				Operation.Name := 'Hue';
        Operation.Proc := HueOperation;
      end
  else
  	Result := False;
  end;
end;

function PixelBlends(Index: Integer; out Blend: TPixelBlend): Boolean;
begin
  Blend.Name := '';
  Blend.Proc := nil;
  Result := True;
  case Index of
	  0:
      begin
				Blend.Name := 'Opacity';
        Blend.Proc := OpacityBlend;
      end;
	  1:
      begin
				Blend.Name := 'Multiply';
        Blend.Proc := MultiplyBlend;
      end;
	  2:
      begin
				Blend.Name := 'Addition';
        Blend.Proc := AdditionBlend;
      end;
	  3:
      begin
				Blend.Name := 'Subtraction';
        Blend.Proc := SubtractionBlend;
      end;
	  4:
      begin
				Blend.Name := 'Wipe';
        Blend.Proc := WipeBlend;
      end;
	  5:
      begin
				Blend.Name := 'Circle';
        Blend.Proc := CircleBlend;
      end;
  else
	  Result := False;
  end;
end;

end.

