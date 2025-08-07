{===============================================================================
  SIMPLE RDP SERVER - INSTANT UPDATES WITH FLICKER PROTECTION - FIXED
===============================================================================}

unit Unit1;

interface

uses
  // Core Windows API
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Winapi.UxTheme, // For SetWindowTheme

  // VCL Components
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Vcl.Themes,

  // NetComms Framework
  ncSources, ncSocketList;

const
  CMD_SCREENSHOT_FULL  = 1;
  CMD_SCREENSHOT_DELTA = 2;
  CMD_MOUSE_EVENT      = 3;
  CMD_KEYBOARD_EVENT   = 4;
  CMD_MOUSE_TOGGLE     = 5;
  CMD_KEYBOARD_TOGGLE  = 6;

type
  // Custom paint panel that completely bypasses themes
  TThemeResistantPanel = class(TPanel)
  private
    FBitmap: TBitmap;
    FUpdateInProgress: Boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBitmap(ABitmap: TBitmap);
    procedure ForceRedraw;
    property UpdateInProgress: Boolean read FUpdateInProgress write FUpdateInProgress;
  end;

  TPixelDelta = packed record
    X, Y: Word;
    Color: Cardinal; // RGB packed into 32-bit for better compression
  end;

  // Enhanced delta header with compression info
  TDeltaHeader = packed record
    Width, Height: Word;
    ChangeCount: Cardinal;
    CompressionRatio: Single; // For statistics
    BlockChanges: Word;       // Number of 8x8 blocks changed
  end;

  TMouseData = packed record
    X, Y: Integer;
    Button: Byte;
    Action: Byte;
  end;

  TKeyData = packed record
    VKey: Word;
    Action: Byte;
  end;

  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..32767] of TRGBTriple;

  TForm1 = class(TForm)
    ncServerSource1: TncServerSource;
    pnlTop: TPanel;
    pnlCenter: TPanel;
    btnStartServer: TButton;
    btnStopServer: TButton;
    btnToggleMouse: TButton;
    btnToggleKeyboard: TButton;
    lblStatus: TLabel;
    lblTotalData: TLabel;
    lblCurrentDelta: TLabel;
    scrollBox: TScrollBox;
    // NOTE: imgDesktop will be replaced with TThemeResistantPanel at runtime

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartServerClick(Sender: TObject);
    procedure btnStopServerClick(Sender: TObject);
    procedure btnToggleMouseClick(Sender: TObject);
    procedure btnToggleKeyboardClick(Sender: TObject);
    procedure imgDesktopMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgDesktopMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgDesktopMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ncServerSource1Connected(Sender: TObject; aLine: TncLine);
    procedure ncServerSource1Disconnected(Sender: TObject; aLine: TncLine);
    function ncServerSource1HandleCommand(Sender: TObject; aLine: TncLine;
      aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
      const aSenderComponent, aReceiverComponent: string): TBytes;

  protected
    // Override Windows messages to prevent flicker - THEME-RESISTANT
    procedure CreateParams(var Params: TCreateParams); override;

  private
    FClientLine: TncLine;
    FCurrentScreenshot: TBitmap;
    FDisplayBitmap: TBitmap;
    FMouseControlEnabled: Boolean;
    FKeyboardControlEnabled: Boolean;
    FTotalBytesReceived: Int64;
    FCurrentDeltaSize: Integer;

    // Custom theme-resistant display
    FDisplayPanel: TThemeResistantPanel;

    // Remote cursor tracking
    FRemoteCursorX: Integer;
    FRemoteCursorY: Integer;
    FRemoteCursorVisible: Boolean;
    FCursorBitmap: TBitmap;

    // Memory management
    FLastUpdateTime: Cardinal;
    FUpdateInProgress: Boolean;

    procedure ProcessScreenshot(const AData: TBytes; AIsDelta: Boolean);
    procedure ApplyDelta(ABitmap: TBitmap; const ADeltaData: TBytes);
    procedure SendMouseEvent(X, Y: Integer; Button, Action: Byte);
    procedure SendKeyEvent(VKey: Word; Action: Byte);
    procedure UpdateDataLabels;
    function FormatBytes(ABytes: Int64): string;

    // Cursor management - THEME-RESISTANT DISPLAY
    procedure CreateCursorBitmap;
    procedure UpdateRemoteCursor(X, Y: Integer);
    procedure DrawCursorOverlay;
    procedure UpdateDisplayImmediate; // INSTANT update method with complete theme resistance
    procedure CleanupBitmaps;
    procedure ForceGarbageCollection;
    procedure CreateThemeResistantDisplay;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TThemeResistantPanel }

constructor TThemeResistantPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf24bit;
  FUpdateInProgress := False;

  // Complete theme resistance
  ControlStyle := ControlStyle + [csOpaque, csNoDesignVisible];
  DoubleBuffered := False; // We handle our own buffering
end;

destructor TThemeResistantPanel.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TThemeResistantPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // Completely bypass theme system
  Params.ExStyle := Params.ExStyle or WS_EX_COMPOSITED;
  Params.WindowClass.style := Params.WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TThemeResistantPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // NEVER erase background
  Message.Result := 1;
end;

procedure TForm1.CreateThemeResistantDisplay;
begin
 // Create custom display panel that completely resists themes
 FDisplayPanel := TThemeResistantPanel.Create(Self);
 FDisplayPanel.Parent := scrollBox;
 FDisplayPanel.Align := alClient;
 FDisplayPanel.Color := clBlack;

 // Set up mouse events
 FDisplayPanel.OnMouseDown := imgDesktopMouseDown;
 FDisplayPanel.OnMouseUp := imgDesktopMouseUp;
 FDisplayPanel.OnMouseMove := imgDesktopMouseMove;
end;

procedure TThemeResistantPanel.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
  DC: HDC;
begin
  // Custom paint - completely bypass VCL and themes
  DC := Message.DC;
  if DC = 0 then
    DC := BeginPaint(Handle, PS);
  try
    Paint;
  finally
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end;
  Message.Result := 0;
end;

procedure TThemeResistantPanel.Paint;
var
  R: TRect;
begin
  // Direct bitmap drawing - no theme interference
  if not FBitmap.Empty then
  begin
    R := ClientRect;
    Canvas.StretchDraw(R, FBitmap);
  end
  else
  begin
    Canvas.Brush.Color := clBlack;
    Canvas.FillRect(ClientRect);
  end;
end;

procedure TThemeResistantPanel.SetBitmap(ABitmap: TBitmap);
begin
  if Assigned(ABitmap) and not ABitmap.Empty then
  begin
    FBitmap.Assign(ABitmap);
    ForceRedraw;
  end;
end;

procedure TThemeResistantPanel.ForceRedraw;
begin
  if not FUpdateInProgress then
  begin
    FUpdateInProgress := True;
    try
      Invalidate;
      Update;
    finally
      FUpdateInProgress := False;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FClientLine := nil;
  FMouseControlEnabled := False;
  FKeyboardControlEnabled := False;
  FTotalBytesReceived := 0;
  FCurrentDeltaSize := 0;
  FLastUpdateTime := 0;
  FUpdateInProgress := False;

  // Create bitmaps with explicit initialization
  FCurrentScreenshot := TBitmap.Create;
  FCurrentScreenshot.PixelFormat := pf24bit;

  FDisplayBitmap := TBitmap.Create;
  FDisplayBitmap.PixelFormat := pf24bit;

  FCursorBitmap := TBitmap.Create;

  FRemoteCursorX := 0;
  FRemoteCursorY := 0;
  FRemoteCursorVisible := False;

  CreateCursorBitmap;
  CreateThemeResistantDisplay;

  btnStopServer.Enabled := False;
  btnToggleMouse.Enabled := False;
  btnToggleKeyboard.Enabled := False;
  lblStatus.Caption := 'Ready - Click Start Server';

  UpdateDataLabels;

  // Setup scrollbox for theme resistance
  scrollBox.VertScrollBar.Tracking := True;
  scrollBox.HorzScrollBar.Tracking := True;

  // Enable keyboard events
  KeyPreview := True;

  // Enable double buffering for form but not display panel
  DoubleBuffered := True;
  scrollBox.DoubleBuffered := True;

  // Disable theme drawing for scrollbox
  if StyleServices.Enabled then
  begin
    SetWindowTheme(scrollBox.Handle, nil, nil);
    SetWindowTheme(pnlCenter.Handle, nil, nil);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CleanupBitmaps;
end;

procedure TForm1.CleanupBitmaps;
begin
  try
    // Clean up custom display panel
    if Assigned(FDisplayPanel) then
    begin
      FDisplayPanel.Free;
      FDisplayPanel := nil;
    end;

    if Assigned(FCurrentScreenshot) then
    begin
      FCurrentScreenshot.FreeImage; // Release GDI handle
      FCurrentScreenshot.Free;
      FCurrentScreenshot := nil;
    end;

    if Assigned(FDisplayBitmap) then
    begin
      FDisplayBitmap.FreeImage; // Release GDI handle
      FDisplayBitmap.Free;
      FDisplayBitmap := nil;
    end;

    if Assigned(FCursorBitmap) then
    begin
      FCursorBitmap.FreeImage; // Release GDI handle
      FCursorBitmap.Free;
      FCursorBitmap := nil;
    end;

    ForceGarbageCollection;
  except
    // Ignore cleanup errors
  end;
end;

procedure TForm1.ForceGarbageCollection;
begin
  // Force memory cleanup using Windows API
  SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF);

  // Force handle cleanup
  Application.ProcessMessages;
end;

procedure TForm1.CreateCursorBitmap;
begin
  // Create minimal cursor bitmap
  FCursorBitmap.Width := 16;
  FCursorBitmap.Height := 16;
  FCursorBitmap.PixelFormat := pf24bit;
  FCursorBitmap.Transparent := True;
  FCursorBitmap.TransparentColor := clWhite;

  // Draw simple red arrow
  FCursorBitmap.Canvas.Brush.Color := clWhite;
  FCursorBitmap.Canvas.FillRect(Rect(0, 0, 16, 16));

  FCursorBitmap.Canvas.Pen.Color := clBlack;
  FCursorBitmap.Canvas.Pen.Width := 1;
  FCursorBitmap.Canvas.Brush.Color := clRed;

  // Simple arrow shape
  FCursorBitmap.Canvas.Polygon([
    Point(0, 0), Point(0, 12), Point(4, 8), Point(8, 12), Point(12, 8), Point(8, 4), Point(12, 0)
  ]);
end;

procedure TForm1.UpdateRemoteCursor(X, Y: Integer);
begin
  FRemoteCursorX := X;
  FRemoteCursorY := Y;
  FRemoteCursorVisible := True;

  // INSTANT cursor update with flicker protection
  UpdateDisplayImmediate;
end;

procedure TForm1.DrawCursorOverlay;
begin
  if FCurrentScreenshot.Empty then Exit;

  try
    // Fast bitmap sizing
    if (FDisplayBitmap.Width <> FCurrentScreenshot.Width) or
       (FDisplayBitmap.Height <> FCurrentScreenshot.Height) then
    begin
      FDisplayBitmap.SetSize(FCurrentScreenshot.Width, FCurrentScreenshot.Height);
    end;

    // Fast copy
    FDisplayBitmap.Assign(FCurrentScreenshot);

    // Fast cursor draw
    if FRemoteCursorVisible and
       (FRemoteCursorX >= 0) and (FRemoteCursorX < FDisplayBitmap.Width - 16) and
       (FRemoteCursorY >= 0) and (FRemoteCursorY < FDisplayBitmap.Height - 16) then
    begin
      FDisplayBitmap.Canvas.Draw(FRemoteCursorX, FRemoteCursorY, FCursorBitmap);
    end;
  except
    // Silent fail
  end;
end;

procedure TForm1.UpdateDisplayImmediate;
begin
  // COMPLETELY THEME-RESISTANT instant display update
  if not Assigned(FDisplayPanel) then Exit;

  try
    if not FCurrentScreenshot.Empty then
    begin
      DrawCursorOverlay;

      if not FDisplayBitmap.Empty then
      begin
        // Direct bitmap assignment to custom panel - completely bypasses themes
        FDisplayPanel.SetBitmap(FDisplayBitmap);
      end;
    end;
  except
    // Silent fail but continue
  end;
end;

procedure TForm1.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // Prevent theme interference with our custom drawing
  Params.ExStyle := Params.ExStyle or WS_EX_COMPOSITED;
end;

procedure TForm1.btnStartServerClick(Sender: TObject);
begin
  try
    ncServerSource1.Port := 3389;
    ncServerSource1.Active := True;
    btnStartServer.Enabled := False;
    btnStopServer.Enabled := True;
    lblStatus.Caption := 'Server running on port 3389 - INSTANT MODE';
  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;

procedure TForm1.btnStopServerClick(Sender: TObject);
begin
  ncServerSource1.Active := False;
  btnStartServer.Enabled := True;
  btnStopServer.Enabled := False;
  btnToggleMouse.Enabled := False;
  btnToggleKeyboard.Enabled := False;
  FClientLine := nil;
  FMouseControlEnabled := False;
  FKeyboardControlEnabled := False;
  FRemoteCursorVisible := False;
  btnToggleMouse.Caption := 'Enable Mouse';
  btnToggleKeyboard.Caption := 'Enable Keyboard';
  lblStatus.Caption := 'Server stopped';

  // Clean up display
  if Assigned(FDisplayPanel) then
    FDisplayPanel.SetBitmap(nil);
  CleanupBitmaps;

  // Recreate bitmaps and display
  FCurrentScreenshot := TBitmap.Create;
  FCurrentScreenshot.PixelFormat := pf24bit;
  FDisplayBitmap := TBitmap.Create;
  FDisplayBitmap.PixelFormat := pf24bit;
  FCursorBitmap := TBitmap.Create;
  CreateCursorBitmap;
  CreateThemeResistantDisplay;
end;

procedure TForm1.btnToggleMouseClick(Sender: TObject);
var
  Data: TBytes;
begin
  if FClientLine = nil then Exit;

  FMouseControlEnabled := not FMouseControlEnabled;
  SetLength(Data, 1);
  Data[0] := Byte(FMouseControlEnabled);

  ncServerSource1.ExecCommand(FClientLine, CMD_MOUSE_TOGGLE, Data, False);

  if FMouseControlEnabled then
  begin
    btnToggleMouse.Caption := 'Disable Mouse';
    lblStatus.Caption := 'Mouse control enabled - INSTANT MODE';
  end
  else
  begin
    btnToggleMouse.Caption := 'Enable Mouse';
    lblStatus.Caption := 'Mouse control disabled';
  end;
end;

procedure TForm1.btnToggleKeyboardClick(Sender: TObject);
var
  Data: TBytes;
begin
  if FClientLine = nil then Exit;

  FKeyboardControlEnabled := not FKeyboardControlEnabled;
  SetLength(Data, 1);
  Data[0] := Byte(FKeyboardControlEnabled);

  ncServerSource1.ExecCommand(FClientLine, CMD_KEYBOARD_TOGGLE, Data, False);

  if FKeyboardControlEnabled then
  begin
    btnToggleKeyboard.Caption := 'Disable Keyboard';
    lblStatus.Caption := 'Keyboard control enabled - INSTANT MODE';
  end
  else
  begin
    btnToggleKeyboard.Caption := 'Enable Keyboard';
    lblStatus.Caption := 'Keyboard control disabled';
  end;
end;

procedure TForm1.ncServerSource1Connected(Sender: TObject; aLine: TncLine);
begin
  FClientLine := aLine;
  btnToggleMouse.Enabled := True;
  btnToggleKeyboard.Enabled := True;
  lblStatus.Caption := 'Client connected: ' + aLine.PeerIP + ' - INSTANT MODE READY';
end;

procedure TForm1.ncServerSource1Disconnected(Sender: TObject; aLine: TncLine);
begin
  FClientLine := nil;
  btnToggleMouse.Enabled := False;
  btnToggleKeyboard.Enabled := False;
  FMouseControlEnabled := False;
  FKeyboardControlEnabled := False;
  FRemoteCursorVisible := False;
  btnToggleMouse.Caption := 'Enable Mouse';
  btnToggleKeyboard.Caption := 'Enable Keyboard';
  lblStatus.Caption := 'Client disconnected';

  // Cleanup on disconnect
  if Assigned(FDisplayPanel) then
    FDisplayPanel.SetBitmap(nil);
  ForceGarbageCollection;
end;

function TForm1.ncServerSource1HandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
var
  MouseData: TMouseData;
begin
  Result := nil;
  Inc(FTotalBytesReceived, Length(aData));

  case aCmd of
    CMD_SCREENSHOT_FULL:
      begin
        FCurrentDeltaSize := Length(aData);
        lblStatus.Caption := 'INSTANT: Full screenshot received (' + FormatBytes(Length(aData)) + ')';
        ProcessScreenshot(aData, False);
      end;
    CMD_SCREENSHOT_DELTA:
      begin
        FCurrentDeltaSize := Length(aData);
        lblStatus.Caption := 'INSTANT: Delta received (' + FormatBytes(Length(aData)) + ')';
        ProcessScreenshot(aData, True);
      end;
    CMD_MOUSE_EVENT:
      begin
        if Length(aData) >= SizeOf(TMouseData) then
        begin
          Move(aData[0], MouseData, SizeOf(TMouseData));
          UpdateRemoteCursor(MouseData.X, MouseData.Y);
        end;
      end;
  end;

  UpdateDataLabels;

  // Memory cleanup every 5MB
  if FTotalBytesReceived mod (5 * 1024 * 1024) = 0 then
    ForceGarbageCollection;
end;

procedure TForm1.ProcessScreenshot(const AData: TBytes; AIsDelta: Boolean);
var
  Stream: TMemoryStream;
  JPEG: TJPEGImage;
  TempBitmap: TBitmap;
begin
  if Length(AData) < 100 then
  begin
    lblStatus.Caption := 'INSTANT: Error - Invalid data';
    Exit;
  end;

  Stream := nil;
  JPEG := nil;
  TempBitmap := nil;

  try
    if AIsDelta then
    begin
      if not FCurrentScreenshot.Empty then
      begin
        ApplyDelta(FCurrentScreenshot, AData);
        UpdateDisplayImmediate; // INSTANT update with flicker protection
        lblStatus.Caption := 'INSTANT: Delta applied successfully';
      end
      else
      begin
        lblStatus.Caption := 'INSTANT: Error - No base image for delta';
      end;
    end
    else
    begin
      // JPEG processing
      Stream := TMemoryStream.Create;
      Stream.WriteBuffer(AData[0], Length(AData));
      Stream.Position := 0;

      JPEG := TJPEGImage.Create;
      JPEG.LoadFromStream(Stream);

      if (JPEG.Width > 0) and (JPEG.Height > 0) then
      begin
        // Convert JPEG to bitmap
        TempBitmap := TBitmap.Create;
        TempBitmap.PixelFormat := pf24bit;
        TempBitmap.Assign(JPEG);

        // Assign to current screenshot
        FCurrentScreenshot.Assign(TempBitmap);

        UpdateDisplayImmediate; // INSTANT update with flicker protection
        lblStatus.Caption := Format('INSTANT: Screenshot %dx%d loaded', [JPEG.Width, JPEG.Height]);
      end
      else
      begin
        lblStatus.Caption := 'INSTANT: Error - Invalid JPEG';
      end;
    end;
  except
    on E: Exception do
    begin
      lblStatus.Caption := 'INSTANT: Error - ' + E.Message;
    end;
  end;

  // Cleanup
  if Assigned(TempBitmap) then TempBitmap.Free;
  if Assigned(Stream) then Stream.Free;
  if Assigned(JPEG) then JPEG.Free;
end;

procedure TForm1.ApplyDelta(ABitmap: TBitmap; const ADeltaData: TBytes);
var
  Header: TDeltaHeader;
  Change: TPixelDelta;
  Offset: Integer;
  i: Integer;
  ScanLine: PRGBArray;
  RGB: TRGBTriple;
  CurrentY: Integer;
  PackedColor: Cardinal;
  IsOldFormat: Boolean;
begin
  if Length(ADeltaData) < SizeOf(TDeltaHeader) then Exit;

  Move(ADeltaData[0], Header, SizeOf(TDeltaHeader));
  if (ABitmap.Width <> Header.Width) or (ABitmap.Height <> Header.Height) then Exit;
  if Header.ChangeCount > 1000000 then Exit; // Sanity check

  // Detect format
  IsOldFormat := (Header.CompressionRatio = 0) and (Header.BlockChanges = 0);

  Offset := SizeOf(TDeltaHeader);
  CurrentY := -1;
  ScanLine := nil;

  for i := 0 to Header.ChangeCount - 1 do
  begin
    if Offset + SizeOf(TPixelDelta) <= Length(ADeltaData) then
    begin
      Move(ADeltaData[Offset], Change, SizeOf(TPixelDelta));
      Inc(Offset, SizeOf(TPixelDelta));

      if (Change.X < ABitmap.Width) and (Change.Y < ABitmap.Height) then
      begin
        if CurrentY <> Change.Y then
        begin
          CurrentY := Change.Y;
          ScanLine := PRGBArray(ABitmap.ScanLine[CurrentY]);
        end;

        if IsOldFormat then
        begin
          // Old format
          RGB.rgbtRed := GetRValue(TColor(Change.Color));
          RGB.rgbtGreen := GetGValue(TColor(Change.Color));
          RGB.rgbtBlue := GetBValue(TColor(Change.Color));
        end
        else
        begin
          // New format - unpack the 32-bit color
          PackedColor := Change.Color;
          RGB.rgbtRed := (PackedColor shr 16) and $FF;
          RGB.rgbtGreen := (PackedColor shr 8) and $FF;
          RGB.rgbtBlue := PackedColor and $FF;
        end;

        ScanLine[Change.X] := RGB;
      end;
    end;
  end;
end;

procedure TForm1.SendMouseEvent(X, Y: Integer; Button, Action: Byte);
var
  Data: TBytes;
  MouseData: TMouseData;
begin
  if (FClientLine = nil) or not FMouseControlEnabled then Exit;

  MouseData.X := X;
  MouseData.Y := Y;
  MouseData.Button := Button;
  MouseData.Action := Action;

  SetLength(Data, SizeOf(TMouseData));
  Move(MouseData, Data[0], SizeOf(TMouseData));

  ncServerSource1.ExecCommand(FClientLine, CMD_MOUSE_EVENT, Data, False);
  UpdateRemoteCursor(X, Y);
end;

procedure TForm1.SendKeyEvent(VKey: Word; Action: Byte);
var
  Data: TBytes;
  KeyData: TKeyData;
begin
  if (FClientLine = nil) or not FKeyboardControlEnabled then Exit;

  KeyData.VKey := VKey;
  KeyData.Action := Action;

  SetLength(Data, SizeOf(TKeyData));
  Move(KeyData, Data[0], SizeOf(TKeyData));

  ncServerSource1.ExecCommand(FClientLine, CMD_KEYBOARD_EVENT, Data, False);
end;

procedure TForm1.UpdateDataLabels;
begin
  lblTotalData.Caption := 'Total Received: ' + FormatBytes(FTotalBytesReceived);
  lblCurrentDelta.Caption := 'Current Delta: ' + FormatBytes(FCurrentDeltaSize);
end;

function TForm1.FormatBytes(ABytes: Int64): string;
begin
  if ABytes < 1024 then
    Result := IntToStr(ABytes) + ' B'
  else if ABytes < 1024 * 1024 then
    Result := Format('%.1f KB', [ABytes / 1024])
  else if ABytes < 1024 * 1024 * 1024 then
    Result := Format('%.1f MB', [ABytes / (1024 * 1024)])
  else
    Result := Format('%.1f GB', [ABytes / (1024 * 1024 * 1024)]);
end;

// Mouse and keyboard event handlers
procedure TForm1.imgDesktopMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  BtnCode: Byte;
  RemoteX, RemoteY: Integer;
begin
  case Button of
    mbLeft: BtnCode := 1;
    mbRight: BtnCode := 2;
    mbMiddle: BtnCode := 3;
    else BtnCode := 0;
  end;

  if not FCurrentScreenshot.Empty then
  begin
    RemoteX := Round((X / FDisplayPanel.Width) * FCurrentScreenshot.Width);
    RemoteY := Round((Y / FDisplayPanel.Height) * FCurrentScreenshot.Height);
  end
  else
  begin
    RemoteX := X;
    RemoteY := Y;
  end;

  SendMouseEvent(RemoteX, RemoteY, BtnCode, 1);
end;

procedure TForm1.imgDesktopMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  BtnCode: Byte;
  RemoteX, RemoteY: Integer;
begin
  case Button of
    mbLeft: BtnCode := 1;
    mbRight: BtnCode := 2;
    mbMiddle: BtnCode := 3;
    else BtnCode := 0;
  end;

  if not FCurrentScreenshot.Empty then
  begin
    RemoteX := Round((X / FDisplayPanel.Width) * FCurrentScreenshot.Width);
    RemoteY := Round((Y / FDisplayPanel.Height) * FCurrentScreenshot.Height);
  end
  else
  begin
    RemoteX := X;
    RemoteY := Y;
  end;

  SendMouseEvent(RemoteX, RemoteY, BtnCode, 2);
end;

procedure TForm1.imgDesktopMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  RemoteX, RemoteY: Integer;
begin
  if not FCurrentScreenshot.Empty then
  begin
    RemoteX := Round((X / FDisplayPanel.Width) * FCurrentScreenshot.Width);
    RemoteY := Round((Y / FDisplayPanel.Height) * FCurrentScreenshot.Height);
  end
  else
  begin
    RemoteX := X;
    RemoteY := Y;
  end;

  SendMouseEvent(RemoteX, RemoteY, 0, 0);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FKeyboardControlEnabled then
  begin
    SendKeyEvent(Key, 1);
    Key := 0;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FKeyboardControlEnabled then
  begin
    SendKeyEvent(Key, 2);
    Key := 0;
  end;
end;

end.
