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
    FOperations: TPixelOperations;
    FBlends: TPixelBlends;
    FNodeBoxDown: Boolean;
    procedure NodesChange(Sender: TObject);
    procedure NodesUpdate(Sender: TObject);
  end;

var
  ImageForm: TImageForm;

implementation

{$R *.lfm}

{ TImageForm }

procedure TImageForm.FormCreate(Sender: TObject);
var
  O: TPixelOperation;
  B: TPixelBlend;
  I: Integer;
begin
  FNodes := TNodeList.Create;
  FNodes.OnChange := NodesChange;
  FNodes.OnUpdate := NodesUpdate;
  NodeBox.Items.AddObject('Sources', nil);
  NodeBox.Items.AddObject('Image', TObject(1));
  NodeBox.Items.AddObject('Reset', TObject(1));
  NodeBox.Items.AddObject('Operator Nodes', nil);
  SetLength(FOperations, 100);
  for I := Low(FOperations) to High(FOperations) do
    if PixelOperations(I, O) then
    begin
      FOperations[I] := O;
      NodeBox.Items.AddObject(O.Name, TObject(2));
    end
    else
    begin
      SetLength(FOperations, I + 1);
      Break;
    end;
  NodeBox.Items.AddObject('Blend Nodes', nil);
  SetLength(FBlends, 100);
  for I := Low(FBlends) to High(FBlends) do
    if PixelBlends(I, B) then
    begin
      FBlends[I] := B;
      NodeBox.Items.AddObject(B.Name, TObject(3));
    end
    else
    begin
      SetLength(FBlends, I + 1);
      Break;
    end;
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
  if G = nil then
  begin

  end
  else
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
  I, J: Integer;
begin
  if Button <> mbLeft then
    Exit;
  I := NodeBox.ItemIndex;
  J := NodeBox.TopIndex;
  if I < 0 then
    Exit;
  S := NodeBox.Items[I];
  I := IntPtr(NodeBox.Items.Objects[I]);
  if (I = 1) and (S = 'Reset') then
  begin
    if NodeBox.GetIndexAtXY(X, Y) = NodeBox.ItemIndex then
      FNodes.Clear;
    NodeBox.ItemIndex := -1;
  end;
  FNodeBoxDown := False;
  NodeBox.TopIndex := J;
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
  I, J: Integer;
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
  J := NodeBox.TopIndex;
  S := NodeBox.Items[I];
  I := IntPtr(NodeBox.Items.Objects[I]);
  case I of
    1:
      if S = 'Image' then
        TImageNode.Create(FNodes).MoveTo(X, Y)
      else if S = 'Reset' then
        FNodes.Clear;
    2:
      for I := Low(FOperations) to High(FOperations) do
        if S = FOperations[I].Name then
        begin
          O := TOperationNode.Create(FNodes);
          O.Title := FOperations[I].Name;
          O.Operation := FOperations[I].Proc;
          O.MoveTo(X, Y);
        end;
    3:
      for I := Low(FBlends) to High(FBlends) do
        if S = FBlends[I].Name then
        begin
          B := TBlendNode.Create(FNodes);
          B.Title := FBlends[I].Name;
          B.Blend := FBlends[I].Proc;
          B.MoveTo(X, Y);
        end;
  end;
  NodeBox.ItemIndex := -1;
  NodeBox.TopIndex := J;
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

