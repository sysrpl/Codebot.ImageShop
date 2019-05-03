unit ImageNodes;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, Pixels, ExtDlgs, Styles;

{ TBaseNode }

type
  TBaseNode = class
  protected
    procedure Changed; virtual; abstract;
    procedure Update; virtual; abstract;
    function GetConnected: Boolean; virtual; abstract;
    function GetImage: TGraphic; virtual; abstract;
  public
    procedure Align; virtual; abstract;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    function  MouseOver(X, Y: Integer): Boolean; virtual; abstract;
    procedure MouseDown(X, Y: Integer); virtual; abstract;
    procedure MouseDrag(X, Y: Integer); virtual; abstract;
    procedure MouseUp(X, Y: Integer); virtual; abstract;
    function Regenerate: Boolean; virtual; abstract;
    property Connected: Boolean read GetConnected;
    property Image: TGraphic read GetImage;
  end;

  TChildNode = class;
  TDisplayNode = class;

{ TNodeEnumerator }

  TNodeEnumerator = class
  private
    FList: TList;
    FPosition: Integer;
  public
    constructor Create(List: TList);
    function GetCurrent: TChildNode;
    function MoveNext: Boolean;
    property Current: TChildNode read GetCurrent;
  end;

{ TNodeList }

  TNodeList = class(TBaseNode)
  private
    FInsert: TPoint;
    FList: TList;
    FHotNode: TChildNode;
    FCaptureNode: TChildNode;
    FWidth: Integer;
    FHeight: Integer;
    FDisplay: TDisplayNode;
    FContainsNode: TChildNode;
    FContains: Boolean;
    FOnChange: TNotifyEvent;
    FOnUpdate: TNotifyEvent;
    function GetCount: Integer;
    function GetNode(Index: Integer): TChildNode;
  public
    function GetEnumerator: TNodeEnumerator;
  protected
    procedure Add(Node: TChildNode);
    procedure Changed; override;
    procedure Update; override;
    function GetConnected: Boolean; override;
    function GetImage: TGraphic; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Remove(Node: TChildNode);
    procedure Clear;
    procedure Align; override;
    procedure Draw(Canvas: TCanvas); override;
    procedure Resize(Width, Height: Integer);
    function  MouseOver(X, Y: Integer): Boolean; override;
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseDrag(X, Y: Integer); override;
    procedure MouseUp(X, Y: Integer); override;
    function Contains(Node: TChildNode): Boolean;
    function Regenerate: Boolean; override;
    property Display: TDisplayNode read FDisplay;
    property Count: Integer read GetCount;
    property Node[Index: Integer]: TChildNode read GetNode; default;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

{ TPinKind }

  TPinKind = (pkInput, pkOutput);

{ TNodePin }

  TNodePin = class
  private
    FNode: TChildNode;
    FConnect: TNodePin;
    FKind: TPinKind;
    FLocation: TPoint;
    procedure SetConnect(Value: TNodePin);
  public
    constructor Create(Node: TChildNode; Kind: TPinKind);
    destructor Destroy; override;
    function CanConnect(Pin: TNodePin): Boolean;
    property Node: TChildNode read FNode;
    property Connect: TNodePin read FConnect write SetConnect;
    property Kind: TPinKind read FKind;
  end;

{ TChildNode }

  TChildNode = class(TBaseNode)
  private
    FOwner: TNodeList;
    FDragPin: TNodePin;
    FDragPoint: TPoint;
    FReleased: Boolean;
    FRect: TRect;
    FCaptionHeight: Integer;
    FTitle: string;
    FCloseDown: Boolean;
    function CloseRect: TRect;
    procedure SetTitle(const Value: string);
  protected
    procedure Changed; override;
    procedure Update; override;
    function GetConnected: Boolean; override;
    function GetImage: TGraphic; override;
    function GetInfo: string; virtual;
    procedure Release; virtual;
    function GetInputPin(Index: Integer): TNodePin; virtual;
    function GetInputCount: Integer; virtual;
    function GetOutputPin(Index: Integer): TNodePin; virtual;
    function GetOutputCount: Integer; virtual;
    property Owner: TNodeList read FOwner;
    property InputPin[Index: Integer]: TNodePin read GetInputPin;
    property InputCount: Integer read GetInputCount;
    property OutputPin[Index: Integer]: TNodePin read GetOutputPin;
    property OutputCount: Integer read GetOutputCount;
  public
    constructor Create(Owner: TNodeList); virtual;
    destructor Destroy; override;
    procedure Align; override;
    procedure Draw(Canvas: TCanvas); override;
    function PinFromPoint(X, Y: Integer; Kind: TPinKind): TNodePin;
    function  MouseOver(X, Y: Integer): Boolean; override;
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseDrag(X, Y: Integer); override;
    procedure MouseUp(X, Y: Integer); override;
    procedure MoveTo(X, Y: Integer);
    function Regenerate: Boolean; override;
    property Info: string read GetInfo;
    property Title: string read FTitle write SetTitle;
  end;

{ TDisplayNode }

  TDisplayNode = class(TChildNode)
  private
    FInput: TNodePin;
  protected
    function GetInfo: string; override;
    function GetInputPin(Index: Integer): TNodePin; override;
    function GetInputCount: Integer; override;
  public
    constructor Create(Owner: TNodeList); override;
    procedure Align; override;
    property Input: TNodePin read FInput;
  end;

  { TControlNode }

  TControlNode = class(TChildNode)
  private
    FControl: TRect;
    FPressed: Boolean;
  public
    procedure Draw(Canvas: TCanvas); override;
    procedure MouseDown(X, Y: Integer); override;
    procedure MouseDrag(X, Y: Integer); override;
    procedure MouseUp(X, Y: Integer); override;
  end;

{ TImageNode }

  TImageNode = class(TControlNode)
  private
    FOutput: TNodePin;
    FImage: TPortableNetworkGraphic;
    FSurface: TPortableNetworkGraphic;
    FFileName: string;
  protected
    function GetImage: TGraphic; override;
    function GetInfo: string; override;
    function GetOutputPin(Index: Integer): TNodePin; override;
    function GetOutputCount: Integer; override;
  public
    constructor Create(Owner: TNodeList); override;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadImage(const FileName: string);
    function Regenerate: Boolean; override;
    procedure Draw(Canvas: TCanvas); override;
    procedure MouseUp(X, Y: Integer); override;
    property Output: TNodePin read FOutput;
  end;

{ TSliderNode }

  TSliderNode = class(TControlNode)
  private
    FPosition: Single;
    procedure SetPosition(Value: Single);
  public
    constructor Create(Owner: TNodeList); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure MouseDrag(X, Y: Integer); override;
    property Position: Single read FPosition write SetPosition;
  end;

{ TOperationNode }

  TOperationNode = class(TSliderNode)
  private
    FOperation: TPixelOperation;
    FInput: TNodePin;
    FOutput: TNodePin;
  protected
    function GetImage: TGraphic; override;
    function GetInfo: string; override;
    function GetInputPin(Index: Integer): TNodePin; override;
    function GetInputCount: Integer; override;
    function GetOutputPin(Index: Integer): TNodePin; override;
    function GetOutputCount: Integer; override;
  public
    constructor Create(Owner: TNodeList); override;
    function Regenerate: Boolean; override;
    property Operation: TPixelOperation read FOperation write FOperation;
    property Input: TNodePin read FInput;
    property Output: TNodePin read FOutput;
  end;

{ TBlendNode }

  TBlendNode = class(TSliderNode)
  private
    FBlend: TPixelBlend;
    FImage: TPortableNetworkGraphic;
    FInputA: TNodePin;
    FInputB: TNodePin;
    FOutput: TNodePin;
  protected
    function GetImage: TGraphic; override;
    function GetInfo: string; override;
    function GetInputPin(Index: Integer): TNodePin; override;
    function GetInputCount: Integer; override;
    function GetOutputPin(Index: Integer): TNodePin; override;
    function GetOutputCount: Integer; override;
  public
    constructor Create(Owner: TNodeList); override;
    destructor Destroy; override;
    function Regenerate: Boolean; override;
    property Blend: TPixelBlend read FBlend write FBlend;
    property InputA: TNodePin read FInputA;
    property InputB: TNodePin read FInputB;
    property Output: TNodePin read FOutput;
  end;

type
  TDirection = (dirLeft = DT_LEFT, dirCenter = DT_CENTER, dirRight = DT_RIGHT, dirWrap);

procedure DrawString(Canvas: TCanvas; S: string; Rect: TRect; Direction: TDirection);
function PointInRect(const Rect: TRect; X, Y: Integer): Boolean;

implementation

var
  SimpleWires: Boolean;

function RectHeight(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

procedure DrawString(Canvas: TCanvas; S: string; Rect: TRect; Direction: TDirection);
var
  F: Cardinal;
  R: TRect;
begin
  if S = '' then
    Exit;
  F := DT_WORDBREAK;
  if Direction = dirWrap then
    DrawText(Canvas.Handle, PChar(S), -1, Rect, DT_LEFT or F);
  F := F or Ord(Direction);
  R := Rect;
  DrawText(Canvas.Handle, PChar(S), -1, R, F or DT_CALCRECT);
  Rect.Top := Rect.Top + RectHeight(Rect) div 2 - RectHeight(R) div 2;
  DrawText(Canvas.Handle, PChar(S), -1, Rect, F or DT_NOCLIP);
end;

const
  GridSize = 10;
  NodeWidth = 160;
  NodeHeight = 60;

function PointInRect(const Rect: TRect; X, Y: Integer): Boolean;
begin
  Result := (X > Rect.Left) and (X < Rect.Right) and
    (Y > Rect.Top) and (Y < Rect.Bottom);
end;

function RectIsEmpty(const Rect: TRect): Boolean;
begin
  Result := (Rect.Left >= Rect.Right) or (Rect.Top >= Rect.Bottom);
end;

function InflateRect(X, Y: Integer; const Rect: TRect): TRect;
begin
  Result := Rect;
  Dec(Result.Left, X);
  Dec(Result.Top, Y);
  Inc(Result.Right, X);
  Inc(Result.Bottom, Y);
end;

{ TNodePin }

constructor TNodePin.Create(Node: TChildNode; Kind: TPinKind);
begin
  inherited Create;
  FNode := Node;
  FKind := Kind;
end;

destructor TNodePin.Destroy;
begin
  Connect := nil;
  inherited Destroy;
end;

procedure TNodePin.SetConnect(Value: TNodePin);
begin
  if Value = nil then
  begin
    if FConnect <> nil then
      FConnect.FConnect := nil;
    FConnect := nil;
    FNode.Changed;
  end
  else if CanConnect(Value) then
  begin

    if FConnect <> nil then
    begin
      if FConnect.FConnect <> nil then
        FConnect.FConnect.FConnect := nil;
      FConnect.FConnect := nil;
    end;
    if Value.FConnect <> nil then
      Value.FConnect.FConnect := nil;
    FConnect := Value;
    FConnect.FConnect := Self;
    FNode.Changed;
  end;
  if FNode.FOwner <> nil then
    FNode.FOwner.Update;
end;

function TNodePin.CanConnect(Pin: TNodePin): Boolean;
var
  Linked: Boolean;

  procedure CheckLinks(Node: TChildNode);
  var
    P: TNodePin;
    I: Integer;
  begin
    for I := 0 to Node.GetOutputCount - 1 do
    begin
      P := Node.GetOutputPin(I);
      if P.Connect <> nil then
        if P.Connect.FNode = FNode then
        begin
          Linked := True;
          Exit;
        end
        else
          CheckLinks(P.Connect.FNode);
    end;
  end;

begin
  if FKind = pkInput then
    Exit(False);
  Result := (Pin.FNode <> FNode) and (Pin.FKind = pkInput);
  if Result then
  begin
    Linked := False;
    CheckLinks(Pin.FNode);
    Result := not Linked;
  end;
end;

{ TNodeEnumerator }

constructor TNodeEnumerator.Create(List: TList);
begin
  inherited Create;
  FList := List;
  FPosition := -1;
end;

function TNodeEnumerator.GetCurrent: TChildNode;
begin
  Result := TChildNode(FList[FPosition]);
end;

function TNodeEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TNodeList }

function TNodeList.GetEnumerator: TNodeEnumerator;
begin
  Result := TNodeEnumerator.Create(FList);
end;

constructor TNodeList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FDisplay := TDisplayNode.Create(Self);
end;

destructor TNodeList.Destroy;
begin
  FDisplay := nil;
  while Count > 1 do
    Remove(Node[0]);
  FList.Free;
  inherited Destroy;
end;

function TNodeList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TNodeList.GetNode(Index: Integer): TChildNode;
begin
  Result := TChildNode(FList[Index]);
end;

procedure TNodeList.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNodeList.Update;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

procedure TNodeList.Add(Node: TChildNode);
begin
  FList.Add(Node);
  Inc(FInsert.X, GridSize * 2);
  if FInsert.X > 500 then
    FInsert.X := GridSize * 2;
  Inc(FInsert.Y, GridSize * 2);
  if FInsert.Y > 200 then
    FInsert.Y := GridSize * 2;
  Node.FRect.TopLeft := FInsert;
  Node.FRect.Right := Node.FRect.Left + NodeWidth;
  Node.FRect.Bottom := Node.FRect.Top + NodeHeight;
  Node.Align;
  Changed;
end;

procedure TNodeList.Remove(Node: TChildNode);
var
  WasConnected: Boolean;
begin
  if Node.Owner = Self then
  begin
    WasConnected := Contains(Node);
    FList.Remove(Node);
    Node.Release;
    Changed;
    if WasConnected then
      Update;
  end;
end;

procedure TNodeList.Clear;
begin
  FDisplay := nil;
  while Count > 0 do
    Remove(Node[0]);
  FDisplay := TDisplayNode.Create(Self);
  FInsert.X := GridSize;
  FInsert.Y := GridSize;
  Changed;
  Update;
end;

procedure TNodeList.Align;
var
  N: TChildNode;
begin
  for N in Self do N.Align;
end;

procedure TNodeList.Draw(Canvas: TCanvas);
var
  N: TChildNode;
  X, Y: Integer;
  R: TRect;
  S: string;
begin
  Canvas.Brush.Color := clStyleWindow;
  Canvas.FillRect(0, 0, FWidth, FHeight);
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Style := psDot;
  for X := 0 to FWidth div GridSize div 2 + 1 do
  begin
    Y := X * GridSize * 2;
    Canvas.MoveTo(Y, 0);
    Canvas.LineTo(Y, FHeight + 1);
  end;
  for Y := 0 to FHeight div GridSize div 2 + 1 do
  begin
    X := Y * GridSize * 2;
    Canvas.MoveTo(0, X);
    Canvas.LineTo(FWidth + 1, X);
  end;
  Canvas.Pen.Style := psSolid;
  for N in Self do
    N.Draw(Canvas);
  if FHotNode <> nil then
  begin
    S := FHotNode.Info;
    if S = '' then
      Exit;
    R.Right := FWidth + 1;
    R.Bottom := FHeight + 1;
    R.Top := R.Bottom - Canvas.TextHeight('Wg') - 8;
    R.Left := R.Right - Canvas.TextWidth(S) - 16;
    Canvas.Pen.Color := clStyleText;
    Canvas.Brush.Color := clStyleWindow;
    Canvas.Rectangle(R);
    Canvas.Font.Color := clStyleText;
    Inc(R.Left, 8);
    DrawString(Canvas, S, R, dirLeft);
  end;
end;

procedure TNodeList.Resize(Width, Height: Integer);
begin
  FWidth := Width;
  FHeight := Height;
  Align;
  Changed;
end;

function TNodeList.MouseOver(X, Y: Integer): Boolean;
var
  N: TChildNode;
  I: Integer;
begin
  Result := False;
  if FCaptureNode <> nil then
  begin
    MouseDrag(X, Y);
    Exit;
  end;
  for I := Count - 1 downto 0 do
  begin
    N := Node[I];
    if N.MouseOver(X, Y) then
    begin
      if FHotNode <> N then
        Changed;
      FHotNode := N;
      Exit;
    end;
  end;
  if FHotNode <> nil then
    Changed;
  FHotNode := nil;
end;

procedure TNodeList.MouseDown(X, Y: Integer);
var
  N: TChildNode;
  I: Integer;
begin
  if (FHotNode <> nil) or (FCaptureNode <> nil) then
    Changed;
  FCaptureNode := nil;
  for I := Count - 1 downto 0 do
  begin
    N := Node[I];
    if N.MouseOver(X, Y) then
    begin
      FHotNode := N;
      FCaptureNode := N;
      N.MouseDown(X, Y);
      N.MouseDrag(X, Y);
      Exit;
    end;
  end;
end;

procedure TNodeList.MouseDrag(X, Y: Integer);
begin
  if FCaptureNode = nil then
    Exit;
  FCaptureNode.MouseDrag(X, Y);
end;

procedure TNodeList.MouseUp(X, Y: Integer);
begin
  if FCaptureNode = nil then
    Exit;
  FCaptureNode.MouseUp(X, Y);
  FCaptureNode := nil;
  Changed;
end;

function TNodeList.Contains(Node: TChildNode): Boolean;
begin
  FContainsNode := Node;
  FContains := False;
  GetImage;
  Result := FContains;
end;

function TNodeList.Regenerate: Boolean;
begin
  Result := FDisplay.Regenerate;
end;

function TNodeList.GetConnected: Boolean;
begin
  Result := FDisplay.GetConnected;
end;

function TNodeList.GetImage;
begin
  if FDisplay <> nil then
    Result := FDisplay.GetImage
  else
    Result := nil;
end;

{ TChildNode }

constructor TChildNode.Create(Owner: TNodeList);
begin
  inherited Create;
  FOwner := Owner;
  FOwner.Add(Self);
end;

destructor TChildNode.Destroy;
var
  I: Integer;
begin
  FReleased := True;
  for I := 0 to OutputCount - 1 do
    OutputPin[I].Free;
  for I := 0 to InputCount - 1 do
    InputPin[I].Free;
  inherited Destroy;
end;

function TChildNode.CloseRect: TRect;
begin
  if Self is TDisplayNode then
  begin
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := 0;
    Result.Bottom := 0;
  end
  else
  begin
    Result := FRect;
    Result.Bottom := FRect.Top + FCaptionHeight;
    Result.Left := Result.Right - FCaptionHeight;
    Result := InflateRect(-3, -3, Result);
  end;
end;

procedure TChildNode.Release;
var
  PriorOwner: TNodeList;
  MustFree: Boolean;
begin
  PriorOwner := FOwner;
  if PriorOwner = nil then
    Exit;
  FOwner := nil;
  MustFree := not FReleased;
  FReleased := True;
  PriorOwner.Remove(Self);
  if MustFree then
    Free;
end;

function TChildNode.GetInputPin(Index: Integer): TNodePin;
begin
  Result := nil;
end;

function TChildNode.GetInputCount: Integer;
begin
  Result := 0;
end;

function TChildNode.GetOutputPin(Index: Integer): TNodePin;
begin
  Result := nil;
end;

function TChildNode.GetOutputCount: Integer;
begin
  Result := 0;
end;

procedure TChildNode.SetTitle(const Value: string);
begin
  if Value <> FTitle then
  begin
    FTitle := Value;
    Changed;
  end;
end;

procedure TChildNode.Changed;
begin
  if FOwner <> nil then
    FOwner.Changed;
end;

procedure TChildNode.Update;
begin
  if FOwner <> nil then
    if FOwner.Contains(Self) then
      FOwner.Update;
end;

function TChildNode.GetConnected;
begin
  Result := GetImage <> nil;
end;

function TChildNode.GetImage: TGraphic;
begin
  if FOwner.FContainsNode = Self then
    FOwner.FContains := True;
  Result := nil;
  if (InputCount > 0) and (InputPin[0].Connect <> nil) then
    Result := InputPin[0].Connect.Node.GetImage;
end;

function TChildNode.GetInfo: string;
begin
  Result := '';
end;

function TChildNode.Regenerate: Boolean;
begin
  Result := False;
  if (InputCount > 0) and (InputPin[0].Connect <> nil) then
    Result := InputPin[0].Connect.Node.Regenerate;
end;

procedure TChildNode.Align;
var
  R: TRect;
  P: TPoint;
  I: Integer;
begin
  R.Left := FRect.Left div GridSize * GridSize;
  if R.Left < 0 then
    R.Left := 0;
  R.Top := FRect.Top div GridSize * GridSize;
  if R.Top < 0 then
    R.Top := 0;
  R.Right := R.Left + NodeWidth;
  R.Bottom := R.Top + NodeHeight;
  for I := 0 to InputCount - 1 do
  begin
    P.X := R.Left - GridSize;
    P.Y := R.Top + NodeHeight div (InputCount + 1) * (I + 1);
    InputPin[I].FLocation := P;
  end;
  for I := 0 to OutputCount - 1 do
  begin
    P.X := R.Right + GridSize;
    P.Y := R.Top + NodeHeight div (OutputCount + 1) * (I + 1);
    OutputPin[I].FLocation := P;
  end;
  if FRect <> R then
  begin
    FRect := R;
    Changed;
  end;
end;

procedure TChildNode.Draw(Canvas: TCanvas);

  procedure DrawClose;
  const
    Offset = 4;
  var
    R: TRect;
    C: TColor;
  begin
    R := CloseRect;
    if RectIsEmpty(R) then
      Exit;
    C := Canvas.Pen.Color;
    if FCloseDown then
      Canvas.Pen.Color := clStyleHighlight;
    Canvas.Pen.Width := 3;
    Canvas.MoveTo(R.Left + Offset, R.Top + Offset);
    Canvas.LineTo(R.Right - Offset - 1, R.Bottom - Offset - 1);
    Canvas.MoveTo(R.Left + Offset, R.Bottom - Offset - 1);
    Canvas.LineTo(R.Right - Offset - 1, R.Top + Offset);
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := C;
  end;

  procedure DrawWire(A, B: TPoint);
  var
    X: Integer;
  begin
    Canvas.MoveTo(FRect.Right, A.Y);
    Canvas.LineTo(A.X, A.Y);
    if B.X < A.X + GridSize then
    begin
      if B.Y > A.Y then
      begin
        Canvas.LineTo(A.X, FRect.Bottom + GridSize * 2);
        Canvas.LineTo(B.X - GridSize div 2, FRect.Bottom + GridSize * 2);
        Canvas.LineTo(B.X - GridSize div 2, B.Y);
      end
      else
      begin
        Canvas.LineTo(A.X, FRect.Top - GridSize * 2);
        Canvas.LineTo(B.X - GridSize div 2, FRect.Top - GridSize * 2);
        Canvas.LineTo(B.X - GridSize div 2, B.Y);
      end;
    end
    else if (B.X - A.X > Abs(B.Y - A.Y)) then
    begin
      X := ((B.X - A.X) - Abs(B.Y - A.Y)) div 2;
      Canvas.LineTo(A.X + X, A.Y);
      Canvas.LineTo(B.X - X, B.Y);
      Canvas.LineTo(B.X, B.Y);
    end
    else
    begin
      X := (B.X - A.X - GridSize) div 2;
      Canvas.LineTo(A.X + X, A.Y);
      Canvas.LineTo(A.X + X, B.Y);
      Canvas.LineTo(B.X - GridSize, B.Y);
    end;
    B.X := B.X - GridSize;
    Canvas.Brush.Color := Canvas.Pen.Color;
    Canvas.Brush.Color := Canvas.Pen.Color;
    Canvas.Rectangle(B.X + 1, B.Y - GridSize div 2 + 1,
      B.X + GridSize - 1, B.Y + GridSize div 2 - 1);
  end;

var
  R: TRect;
  P: TPoint;
  I: Integer;
begin
  R := FRect;
  if Self = FOwner.FHotNode then
  begin
    Canvas.Pen.Color := clStyleText;
    Canvas.Font.Color := clStyleText;
  end
  else
  begin
    Canvas.Pen.Color := clStyleDull;
    Canvas.Font.Color := clStyleDull;
  end;
  Canvas.Brush.Color := clStyleWindow;
  Canvas.Rectangle(R);
  FCaptionHeight := Canvas.TextHeight('Wg') + 8;
  R.Bottom := R.Top + FCaptionHeight;
  Canvas.Rectangle(R);
  Canvas.TextOut(R.Left + 8, R.Top + 4, FTitle);
  DrawClose;
  if InputCount > 0 then
  begin
    for I := 0 to InputCount - 1 do
    begin
      P := InputPin[I].FLocation;
      Canvas.MoveTo(FRect.Left, P.Y);
      Canvas.LineTo(P.X, P.Y);
      Canvas.LineTo(P.X, P.Y - GridSize div 2);
      Canvas.LineTo(P.X - GridSize, P.Y - GridSize div 2);
      Canvas.MoveTo(P.X, P.Y);
      Canvas.LineTo(P.X, P.Y + GridSize div 2);
      Canvas.LineTo(P.X - GridSize, P.Y + GridSize div 2);
    end;
  end;
  if OutputCount > 0 then
  begin
    for I := 0 to OutputCount - 1 do
    begin
      if OutputPin[I] = FDragPin then
      begin
        if SimpleWires then
        begin
          P := OutputPin[I].FLocation;
          Canvas.MoveTo(FRect.Right, P.Y);
          Canvas.LineTo(P.X, P.Y);
          P := FDragPoint;
          Canvas.LineTo(P.X - GridSize, P.Y);
          P.X := P.X - GridSize;
          Canvas.Brush.Color := Canvas.Pen.Color;
          Canvas.Rectangle(P.X, P.Y * (I + 1) - GridSize div 2 + 2,
            P.X + GridSize - 2, P.Y * (I + 1) + GridSize div 2 - 1);
        end
        else
          DrawWire(OutputPin[I].FLocation, FDragPoint);
      end
      else if OutputPin[I].Connect <> nil then
      begin
        if SimpleWires then
        begin
          P := OutputPin[I].FLocation;
          Canvas.MoveTo(FRect.Right, P.Y);
          Canvas.LineTo(P.X, P.Y);
          P := OutputPin[I].Connect.FLocation;
          P.X := P.X - GridSize;
          Canvas.LineTo(P);
          Canvas.Brush.Color := Canvas.Pen.Color;
          Canvas.Rectangle(P.X, P.Y * (I + 1) - GridSize div 2 + 2,
            P.X + GridSize - 2, P.Y * (I + 1) + GridSize div 2 - 1);
        end
        else
          DrawWire(OutputPin[I].FLocation, OutputPin[I].Connect.FLocation);
      end
      else
      begin
        P := OutputPin[I].FLocation;
        Canvas.MoveTo(FRect.Right, P.Y);
        Canvas.LineTo(P.X, P.Y);
        Canvas.Brush.Color := Canvas.Pen.Color;
        Canvas.Rectangle(P.X, P.Y * (I + 1) - GridSize div 2 + 1,
          P.X + GridSize - 2, P.Y * (I + 1) + GridSize div 2 - 1);
      end;
    end;
  end;
end;

function Distance(X1, Y1, X2, Y2: Integer): Double;
begin
  Result :=  Sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2));
end;

function TChildNode.PinFromPoint(X, Y: Integer; Kind: TPinKind): TNodePin;
var
  C: TNodePin;
  P: TPoint;
  I: Integer;
begin
  Result := nil;
  if Kind = pkInput then
    for I := 0 to InputCount - 1 do
    begin
        P := InputPin[I].FLocation;
        if Distance(X, Y, P.X, P.Y) < GridSize then
          Exit(InputPin[I])
    end
  else
    for I := 0 to OutputCount - 1 do
    begin
      C := OutputPin[I].Connect;
      if C = nil then
      begin
        P := OutputPin[I].FLocation;
        if Distance(X, Y, P.X, P.Y) < GridSize then
          Exit(OutputPin[I]);
      end
      else
      begin
        P := C.FLocation;
        if Distance(X, Y, P.X, P.Y) < GridSize then
          Exit(OutputPin[I]);
      end;
    end;
end;

function TChildNode.MouseOver(X, Y: Integer): Boolean;
begin
  Result := PointInRect(FRect, X, Y) or (PinFromPoint(X, Y, pkOutput) <> nil);
end;

procedure TChildNode.MouseDown(X, Y: Integer);
begin
  FOwner.FList.Remove(Self);
  FOwner.FList.Add(Self);
  FCloseDown := PointInRect(CloseRect, X, Y);
  if FCloseDown then
  begin
    Changed;
    Exit;
  end;
  FDragPoint.X := X;
  FDragPoint.Y := Y;
  FDragPin := PinFromPoint(X, Y, pkOutput);
end;

procedure TChildNode.MouseDrag(X, Y: Integer);
begin
  if FCloseDown then
    Exit;
  if FDragPin <> nil then
  begin
    FDragPoint.X := X;
    FDragPoint.Y := Y;
    Changed;
  end
  else
    MoveTo(X, Y);
end;

procedure TChildNode.MouseUp(X, Y: Integer);
var
  N: TChildNode;
  P: TNodePin;
  I: Integer;
begin
  if FCloseDown then
  begin
    FCloseDown := False;
    Changed;
    if PointInRect(CloseRect, X, Y) then
      FOwner.Remove(Self);
    Exit;
  end;
  if FDragPin <> nil then
  begin
    FDragPin.Connect := nil;
    for N in FOwner do
    begin
      if N = Self then
        Continue;
      for I := 0 to N.InputCount - 1 do
      begin
        P := N.PinFromPoint(X, Y, pkInput);
        if (P <> nil) and FDragPin.CanConnect(P) then
        begin
          FDragPin.Connect := P;
          FDragPin := nil;
          Exit;
        end;
      end;
    end;
  end;
  FDragPin := nil;
end;

procedure TChildNode.MoveTo(X, Y: Integer);
var
  X1, Y1: Integer;
begin
  X1 := X - NodeWidth div 2;
  Y1 := Y - NodeHeight div 2;
  X1 := X1 div GridSize * GridSize;
  Y1 := Y1 div GridSize * GridSize;
  if (X1 <> FRect.Left) or (Y1 <> FRect.Top) then
  begin
    FRect.Left := X1;
    FRect.Top := Y - FCaptionHeight div 3;
    Align;
  end;
end;

{ TDisplayNode }

constructor TDisplayNode.Create(Owner: TNodeList);
begin
  FInput := TNodePin.Create(Self, pkInput);
  inherited Create(Owner);
  Title := 'Final output';
end;

function TDisplayNode.GetInfo: string;
var
  G: TGraphic;
begin
  G := GetImage;
  if G <> nil then
    Result := Format('Final output %d X %d', [G.Width, G.Height])
  else
    Result := 'Final output no image';
end;

function TDisplayNode.GetInputPin(Index: Integer): TNodePin;
begin
  case Index of
    0: Result := FInput;
  else
    Result := nil;
  end;
end;

function TDisplayNode.GetInputCount: Integer;
begin
  Result := 1;
end;

procedure TDisplayNode.Align;
begin
  FRect.Top := (Owner.Height - NodeHeight)  div 2;
  FRect.Left := Owner.Width - NodeWidth - GridSize * 2;
  FRect.Right := FRect.Left + NodeWidth;
  FRect.Bottom := FRect.Top + NodeHeight;
  inherited Align;
end;

{ TControlNode }

procedure TControlNode.Draw(Canvas: TCanvas);
begin
  inherited Draw(Canvas);
  FControl := FRect;
  FControl.Top := FControl.Top + FCaptionHeight;
  FControl := InflateRect(-12, -8, FControl);
  FControl.Top := FControl.Top - 1;
  Canvas.Brush.Color := clStyleWindow;
end;

procedure TControlNode.MouseDown(X, Y: Integer);
begin
  inherited MouseDown(X, Y);
  FPressed := PointInRect(FControl, X, Y);
  if FPressed then
    Changed;
end;

procedure TControlNode.MouseDrag(X, Y: Integer);
begin
  if not FPressed then
    inherited MouseDrag(X, Y);
end;

procedure TControlNode.MouseUp(X, Y: Integer);
begin
  inherited MouseUp(X, Y);
  if FPressed then
    Changed;
  FPressed := False;
end;

{ TImageNode }

constructor TImageNode.Create(Owner: TNodeList);
begin
  FOutput := TNodePin.Create(Self, pkOutput);
  FImage := TPortableNetworkGraphic.Create;
  FSurface := TPortableNetworkGraphic.Create;
  inherited Create(Owner);
  Title := 'No image';
end;

destructor TImageNode.Destroy;
begin
  FImage.Free;
  FSurface.Free;
  inherited Destroy;
end;

procedure TImageNode.Clear;
begin
  FImage.Width := 0;
  FImage.Height := 0;
  FSurface.Width := 0;
  FSurface.Height := 0;
end;

procedure TImageNode.LoadImage(const FileName: string);
var
  P: TPicture;
  A, B: PPixel;
  C: Byte;
  I: Integer;
begin
  P := TPicture.Create;
  try
    P.LoadFromFile(FileName);
    FImage.Width := P.Width;
    FImage.Height := P.Height;
    FImage.PixelFormat := pf32bit;
    if (P.Graphic is TPortableNetworkGraphic) and
      (TPortableNetworkGraphic(P.Graphic).PixelFormat = pf32bit) then
    begin
      A := TPortableNetworkGraphic(P.Graphic).ScanLine[0];
      B := FImage.ScanLine[0];
      for I := 1 to FImage.Width * FImage.Height do
      begin
        B^ := A^;
        {$ifdef linux}
        C := B.R;
        B.R := B.B;
        B.B := C;
        {$endif}
        Inc(A);
        Inc(B);
      end;
    end
    else
    begin
      FImage.Canvas.Draw(0, 0, P.Graphic);
      A := FImage.ScanLine[0];
      for I := 1 to FImage.Width * FImage.Height do
      begin
        A.A := $FF;
        Inc(A);
      end;
    end;
    Regenerate;
    FFileName:= FileName;
  finally
    P.Free;
  end;
  Changed;
  Update;
end;

function TImageNode.GetImage: TGraphic;
begin
  if FOwner.FContainsNode = Self then
    FOwner.FContains := True;
  if FSurface.Empty then
    Result := nil
  else
    Result := FSurface;
end;

function TImageNode.GetInfo: string;
begin
  if FSurface.Empty then
    Result := 'No image has been loaded'
  else
    Result := Format('Image %d X %d from %s', [FSurface.Width, FSurface.Height, FFileName]);
end;

function TImageNode.Regenerate: Boolean;
var
  A, B: PPixel;
begin
  Result := not FImage.Empty;
  if Result then
  begin
    FSurface.Width := 0;
    FSurface.Height := 0;
    FSurface.Width := FImage.Width;
    FSurface.Height := FImage.Height;
    FSurface.PixelFormat := pf32bit;
    A := FImage.ScanLine[0];
    B := FSurface.ScanLine[0];
    Move(A^, B^, FImage.Width * FImage.Height * SizeOf(TPixel));
  end;
end;

procedure TImageNode.Draw(Canvas: TCanvas);
var
  C: TColor;
begin
  inherited Draw(Canvas);
  if Self = FOwner.FHotNode then
    C := clStyleText
  else
    C := clStyleDull;
  if FPressed then
  begin
    Canvas.Pen.Color := clStyleHighlight;
    Canvas.Font.Color := clStyleHighlight;
  end
  else
  begin
    Canvas.Pen.Color := C;
    Canvas.Font.Color := C;
  end;
  Canvas.Rectangle(FControl);
  FControl.Bottom := FControl.Bottom - 1;
  DrawString(Canvas, 'Open Image', FControl, dirCenter);
  Canvas.Pen.Color := C;
  Canvas.Font.Color := C;
end;

procedure TImageNode.MouseUp(X, Y: Integer);
const
  Limit = 17;
var
  WasPressed: Boolean;
  D: TOpenPictureDialog;
  S: string;
begin
  WasPressed := FPressed;
  inherited MouseUp(X, Y);
  if WasPressed and PointInRect(FControl, X, Y) then
  begin
    D := TOpenPictureDialog.Create(nil);
    try
      if D.Execute then
      begin
        S := D.FileName;
        LoadImage(S);
        S := ExtractFileName(S);
        if Length(S) > Limit then
          SetLength(S, Limit);
        Title := S;
      end;
    finally
      D.Free;
    end;
  end;
end;

function TImageNode.GetOutputPin(Index: Integer): TNodePin;
begin
  case Index of
    0: Result := FOutput;
  else
    Result := nil;
  end;
end;

function TImageNode.GetOutputCount: Integer;
begin
  Result := 1;
end;

{ TSliderNode }

constructor TSliderNode.Create(Owner: TNodeList);
begin
  FPosition := 1;
  inherited Create(Owner);
end;

procedure TSliderNode.SetPosition(Value: Single);
begin
  if Value < 0 then
    Value := 0;
  if Value > 1 then
    Value := 1;
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed;
    Update;
  end;
end;

procedure TSliderNode.Draw(Canvas: TCanvas);
const
  Thumb = 4;
var
  C: TColor;
  R: TRect;
  X, Y: Integer;
begin
  inherited Draw(Canvas);
  if Self = FOwner.FHotNode then
    C := clStyleText
  else
    C := clStyleDull;
  if FPressed then
  begin
    Canvas.Pen.Color := clStyleHighlight;
    Canvas.Font.Color := clStyleHighlight;
  end
  else
  begin
    Canvas.Pen.Color := C;
    Canvas.Font.Color := C;
  end;
  R := FControl;
  R.Left := R.Left + Thumb;
  R.Right := R.Right - Thumb;
  X := R.Left + Round(FPosition * (R.Right - R.Left));
  Y := (R.Top + R.Bottom) div 2;
  Canvas.MoveTo(R.Left, Y);
  Canvas.LineTo(R.Right, Y);
  Canvas.Rectangle(X - Thumb, R.Top, X + Thumb, R.Bottom);
end;

procedure TSliderNode.MouseDrag(X, Y: Integer);
begin
  inherited MouseDrag(X, Y);
  if FPressed then
    Position := (X - FControl.Left) / (FControl.Right - FControl.Left);
end;

{ TOperationNode }

constructor TOperationNode.Create(Owner: TNodeList);
begin
  FInput := TNodePin.Create(Self, pkInput);
  FOutput := TNodePin.Create(Self, pkOutput);
  inherited Create(Owner);
end;

function TOperationNode.GetImage: TGraphic;
begin
  if FOwner.FContainsNode = Self then
    FOwner.FContains := True;
  if Assigned(FOperation) then
    Result := inherited GetImage
  else
    Result := nil;
end;

function TOperationNode.GetInfo: string;
var
  G: TGraphic;
begin
  if Input.Connect = nil then
    Result := Format(Title + ' operation no input at level %.3f', [Position])
  else
  begin
    G := GetImage;
    if G = nil then
      Result := Format(Title + ' operation connected no source image at level %.3f', [Position])
    else
      Result := Format(Title + ' operation connected %d X %d at level %.3f', [
      G.Width, G.Height, Position]);
  end;
end;

function TOperationNode.Regenerate: Boolean;
var
  Graphic: TGraphic;
  Bitmap: TPortableNetworkGraphic;
  Pixel: PPixel;
  X, Y: Integer;
begin
  Result := inherited Regenerate;
  if Result and Assigned(FOperation) then
  begin
    Graphic := GetImage;
    if Graphic <> nil then
    begin
      Bitmap := Graphic as TPortableNetworkGraphic;
      Pixel := PPixel(Bitmap.ScanLine[0]);
      ImageWidth := Bitmap.Width;
      ImageHeight := Bitmap.Height;
      for Y := 0 to ImageHeight - 1 do
        for X := 0 to ImageWidth - 1 do
        begin
          FOperation(Pixel^, X, Y, FPosition);
          Inc(Pixel);
        end;
      Result := True;
    end;
  end;
end;

function TOperationNode.GetInputPin(Index: Integer): TNodePin;
begin
  case Index of
    0: Result := FInput;
  else
    Result := nil;
  end;
end;

function TOperationNode.GetInputCount: Integer;
begin
  Result := 1;
end;

function TOperationNode.GetOutputPin(Index: Integer): TNodePin;
begin
  case Index of
    0: Result := FOutput;
  else
    Result := nil;
  end;
end;

function TOperationNode.GetOutputCount: Integer;
begin
  Result := 1;
end;

{ TBlendNode }

constructor TBlendNode.Create(Owner: TNodeList);
begin
  FInputA := TNodePin.Create(Self, pkInput);
  FInputB := TNodePin.Create(Self, pkInput);
  FOutput := TNodePin.Create(Self, pkOutput);
  FImage := TPortableNetworkGraphic.Create;
  inherited Create(Owner);
end;

destructor TBlendNode.Destroy;
begin
  FImage.Free;
  FImage := nil;
  inherited Destroy;
end;

function TBlendNode.GetImage: TGraphic;
var
  A, B: TGraphic;
  W, H: Integer;
begin
  Result := nil;
  if FOwner.FContainsNode = Self then
    FOwner.FContains := True;
  if Assigned(FBlend) and (FInputA.Connect <> nil) and
    (FInputB.Connect <> nil) then
  begin
    A := FInputA.Connect.Node.Image;
    B := FInputB.Connect.Node.Image;
    if (A = nil) or (B = nil) then
      Exit;
    if A.Empty or B.Empty then
      Exit;
    W := A.Width;
    if B.Width < W then
      W := B.Width;
    H := A.Height;
    if B.Height < H then
      H := B.Height;
    if (FImage.Width <> W) or (FImage.Height <> H) then
    begin
      FImage.Width := W;
      FImage.Height := H;
      FImage.PixelFormat := pf32bit;
    end;
    Result := FImage;
  end
end;

function TBlendNode.GetInfo: string;
var
  G: TGraphic;
begin
  if InputA.Connect = nil then
    Result := Format(Title + ' blend no A input at level %.3f', [Position])
  else if InputB.Connect = nil then
    Result := Format(Title + ' blend no B input at level %.3f', [Position])
  else
  begin
    G := GetImage;
    if G = nil then
      Result := Format(Title + ' blend connected no source A or B image at level %.3f', [Position])
    else
      Result := Format(Title + ' blend connected %d X %d at level %.3f', [
      G.Width, G.Height, Position]);
  end;
end;

function TBlendNode.Regenerate: Boolean;
var
  Graphic: TGraphic;
  BitmapA, BitmapB: TPortableNetworkGraphic;
  PixelA, PixelB, Pixel: PPixel;
  X, Y: Integer;
begin
  Result := False;
  Graphic := GetImage;
  if Graphic = nil then
    Exit;
  Result := InputA.Connect.Node.Regenerate and InputB.Connect.Node.Regenerate;
  if Result then
  begin
    Graphic.Width := 0;
    Graphic.Height := 0;
    Graphic := GetImage;
    if Graphic = nil then
      Exit;
    ImageWidth := FImage.Width;
    ImageHeight := FImage.Height;
    BitmapA := InputA.Connect.Node.Image as TPortableNetworkGraphic;
    BitmapB := InputB.Connect.Node.Image as TPortableNetworkGraphic;
    for Y := 0 to ImageHeight - 1 do
    begin
      PixelA := BitmapA.ScanLine[Y];
      PixelB := BitmapB.ScanLine[Y];
      Pixel := FImage.ScanLine[Y];
      for X := 0 to ImageWidth - 1 do
      begin
        FBlend(PixelA^, PixelB^, Pixel^, X, Y, Position);
        Inc(PixelA);
        Inc(PixelB);
        Inc(Pixel);
      end;
    end;
    Result := True;
  end;
end;

function TBlendNode.GetInputPin(Index: Integer): TNodePin;
begin
  case Index of
    0: Result := FInputA;
    1: Result := FInputB;
  else
    Result := nil;
  end;
end;

function TBlendNode.GetInputCount: Integer;
begin
  Result := 2;
end;

function TBlendNode.GetOutputPin(Index: Integer): TNodePin;
begin
  case Index of
    0: Result := FOutput;
  else
    Result := nil;
  end;
end;

function TBlendNode.GetOutputCount: Integer;
begin
  Result := 1;
end;

initialization
  SimpleWires := ParamStr(1) = '-simple';
end.

