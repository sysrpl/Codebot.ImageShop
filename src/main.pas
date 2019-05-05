unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ImageNodes, Pixels, Types, LCLType, Styles;

{ TImageForm }

type
  TImageForm = class(TForm)
    ImagePanel: TPanel;
    NodeBox: TListBox;
    NodePanel: TPanel;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    Splitter: TSplitter;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImagePanelPaint(Sender: TObject);
    procedure NodeBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure NodeBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NodeBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NodePanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NodePanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure NodePanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NodePanelPaint(Sender: TObject);
    procedure NodePanelResize(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FNodes: TNodeList;
    FNodeBoxDown: Boolean;
    procedure NodesChange(Sender: TObject);
    procedure NodesUpdate(Sender: TObject);
  end;

var
  ImageForm: TImageForm;

implementation

{$R *.lfm}

type
  TPixelOperationItem = record
    Name: string;
    Proc: TPixelOperation;
  end;
  TPixelOperations = array of TPixelOperationItem;

  TPixelBlendItem = record
    Name: string;
    Proc: TPixelBlend;
  end;
  TPixelBlends = array of TPixelBlendItem;

var
  Operations: TPixelOperations;
  Blends: TPixelBlends;

procedure AddOperation(const Name: string; Proc: TPixelOperation);
var
  I: Integer;
begin
  I := Length(Operations);
  SetLength(Operations, I + 1);
  Operations[I].Name := Name;
  Operations[I].Proc := Proc;
end;

procedure AddBlends(const Name: string; Proc: TPixelBlend);
var
  I: Integer;
begin
  I := Length(Blends);
  SetLength(Blends, I + 1);
  Blends[I].Name := Name;
  Blends[I].Proc := Proc;
end;

{ TImageForm }

procedure TImageForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FNodes := TNodeList.Create;
  FNodes.OnChange := NodesChange;
  FNodes.OnUpdate := NodesUpdate;
  NodeBox.Items.AddObject('Sources', nil);
  NodeBox.Items.AddObject('Image', TObject(1));
  NodeBox.Items.AddObject('Reset', TObject(1));
  NodeBox.Items.AddObject('Operator Nodes', nil);
  InitializeOperations(AddOperation);
  for I := Low(Operations) to High(Operations) do
    NodeBox.Items.AddObject(Operations[I].Name, TObject(2));
  NodeBox.Items.AddObject('Blend Nodes', nil);
  InitializeBlends(AddBlends);
  for I := Low(Blends) to High(Blends) do
    NodeBox.Items.AddObject(Blends[I].Name, TObject(3));
  NodeBox.ItemIndex := -1;
end;

procedure TImageForm.FormDestroy(Sender: TObject);
begin
  FNodes.OnUpdate := nil;
  FNodes.OnChange := nil;
  FNodes.Free;
end;

procedure TImageForm.FormShow(Sender: TObject);
begin
  NodeBox.ItemIndex := 3;
  NodeBox.Invalidate;
end;

procedure TImageForm.ImagePanelPaint(Sender: TObject);
var
  B: TBitmap;
  G: TGraphic;
  X, Y: Integer;
begin
  B := TBitmap.Create;
  try
    B.Width := 20;
    B.Height := 20;
    B.Canvas.Brush.Color := clWhite;
    B.Canvas.FillRect(0, 0, 20, 20);
    B.Canvas.Brush.Color := clSilver;
    B.Canvas.FillRect(0, 0, 10, 10);
    B.Canvas.FillRect(10, 10, 20, 20);
    ImagePanel.Canvas.Brush.Bitmap := B;
    ImagePanel.Canvas.FillRect(ImagePanel.ClientRect);
  finally
    B.Free;
  end;
  G := FNodes.Display.Image;
  if G <> nil then
  begin
    X := (ImagePanel.Width - G.Width) div 2;
    Y := (ImagePanel.Height - G.Height) div 2;
    ImagePanel.Canvas.Draw(X, Y, G);
  end;
end;

procedure TImageForm.NodeBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  S: string;
begin
  S := NodeBox.Items[Index];
  if NodeBox.Items.Objects[Index] = nil then
  begin
    NodeBox.Canvas.Pen.Color := clStyleDull;
    NodeBox.Canvas.Brush.Color := clStyleLight;
    NodeBox.Canvas.Font.Color := clBlack;
    NodeBox.Canvas.Rectangle(ARect);
    ARect.Left := ARect.Left + 3;
    DrawString(NodeBox.Canvas, S, ARect, ImageNodes.dirLeft);
  end
  else
  begin
    if Index = NodeBox.ItemIndex then
      NodeBox.Canvas.Brush.Color := clStyleHighlight
    else
      NodeBox.Canvas.Brush.Color := clStyleWindow;
    NodeBox.Canvas.FillRect(ARect);
    NodeBox.Canvas.Font.Color := clStyleText;
    ARect.Left := ARect.Left + 8;
    DrawString(NodeBox.Canvas, S, ARect, ImageNodes.dirLeft);
  end;
end;

procedure TImageForm.NodeBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then
    Exit;
  FNodeBoxDown := True;
  NodeBox.Invalidate;
end;

procedure TImageForm.NodeBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  S: string;
  I: Integer;
begin
  if Button <> mbLeft then
    Exit;
  I := NodeBox.ItemIndex;
  if I < 0 then
    Exit;
  S := NodeBox.Items[I];
  I := IntPtr(NodeBox.Items.Objects[I]);
  if (I = 1) and (S = 'Reset') then
  begin
    if NodeBox.GetIndexAtXY(X, Y) = NodeBox.ItemIndex then
      FNodes.Clear;
    NodeBox.ItemIndex := -1;
    NodeBox.Visible := False;
    NodeBox.Visible := True;
  end;
  FNodeBoxDown := False;
  NodeBox.Invalidate;
end;

procedure TImageForm.NodesChange(Sender: TObject);
begin
  NodePanel.Invalidate;
end;

procedure TImageForm.NodesUpdate(Sender: TObject);
begin
  UpdateTimer.Enabled := False;
  UpdateTimer.Enabled := True;
end;

procedure TImageForm.NodePanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FNodes.MouseOver(X, Y);
    FNodes.MouseDown(X, Y);
  end;
end;

procedure TImageForm.NodePanelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  FNodes.MouseOver(X, Y);
end;

procedure TImageForm.NodePanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  O: TOperationNode;
  B: TBlendNode;
  S: string;
  I: Integer;
begin
  if Button <> mbLeft then
    Exit;
  I := NodeBox.ItemIndex;
  if I < 0 then
  begin
    FNodes.MouseUp(X, Y);
    FNodes.MouseOver(X, Y);
    Exit;
  end;
  S := NodeBox.Items[I];
  I := IntPtr(NodeBox.Items.Objects[I]);
  case I of
    1:
      if S = 'Image' then
        TImageNode.Create(FNodes).MoveTo(X, Y)
      else if S = 'Reset' then
        FNodes.Clear;
    2:
      for I := Low(Operations) to High(Operations) do
        if S = Operations[I].Name then
        begin
          O := TOperationNode.Create(FNodes);
          O.Title := Operations[I].Name;
          O.Operation := Operations[I].Proc;
          O.MoveTo(X, Y);
        end;
    3:
      for I := Low(Blends) to High(Blends) do
        if S = Blends[I].Name then
        begin
          B := TBlendNode.Create(FNodes);
          B.Title := Blends[I].Name;
          B.Blend := Blends[I].Proc;
          B.MoveTo(X, Y);
        end;
  end;
  I := NodeBox.TopIndex;
  NodeBox.ItemIndex := -1;
  NodeBox.Visible := False;
  NodeBox.Visible := True;
  NodeBox.Invalidate;
  NodeBox.TopIndex := I;
end;

procedure TImageForm.NodePanelPaint(Sender: TObject);
begin
  FNodes.Draw(NodePanel.Canvas);
end;

procedure TImageForm.NodePanelResize(Sender: TObject);
begin
  FNodes.Resize(NodePanel.Width, NodePanel.Height);
end;

procedure TImageForm.UpdateTimerTimer(Sender: TObject);
begin
  UpdateTimer.Enabled := False;
  FNodes.Regenerate;
  ImagePanel.Invalidate;
end;

end.

