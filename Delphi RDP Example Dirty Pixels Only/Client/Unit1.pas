{===============================================================================
  ULTRA-OPTIMIZED RDP CLIENT - BANDWIDTH CONSCIOUS - COMPLETE FIXED VERSION
===============================================================================}

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  ncSources,Math,JPEG;

const
  CMD_SCREENSHOT_FULL  = 1;
  CMD_SCREENSHOT_DELTA = 2;
  CMD_MOUSE_EVENT      = 3;
  CMD_KEYBOARD_EVENT   = 4;
  CMD_MOUSE_TOGGLE     = 5;
  CMD_KEYBOARD_TOGGLE  = 6;

  WH_MOUSE_LL = 14;
  WH_KEYBOARD_LL = 13;

  // BALANCED settings for real-world usage
  DELTA_THRESHOLD = 15;    // Higher threshold to ignore micro-changes
  MAX_DELTA_SIZE = 100000; // Allow larger deltas
  BLOCK_SIZE = 4;          // Smaller blocks for faster processing
  FULL_SCREENSHOT_INTERVAL = 30; // Send full screenshot every 30 frames

type
  // Optimized pixel delta - uses smaller data types
  TPixelDelta = packed record
    X, Y: Word;
    Color: Cardinal; // RGB packed into 32-bit
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

  // Fast RGB access
  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..32767] of TRGBTriple;

  // Hook structures
  PMSLLHOOKSTRUCT = ^MSLLHOOKSTRUCT;
  MSLLHOOKSTRUCT = record
    pt: TPoint;
    mouseData: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;

  PKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;
  KBDLLHOOKSTRUCT = record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;

  TForm1 = class;

  // Ultra-fast screen capture thread with bandwidth-conscious delta compression
  TScreenCaptureThread = class(TThread)
  private
    FForm: TForm1;
    FLastScreenshot: TBitmap;
    FCurrentScreenshot: TBitmap;
    FFrameCounter: Integer;
    FFrameInterval: Cardinal;
    FLastMousePos: TPoint;

  protected
    procedure Execute; override;
    procedure CaptureScreen;
    procedure SendFullScreenshot;
    procedure SendSmartDelta;
    function CreateUltraCompressedDelta(const AOld, ANew: TBitmap): TBytes;

  public
    constructor Create(AForm: TForm1);
    destructor Destroy; override;
    procedure SetFrameRate(AFPS: Integer);
  end;

  TForm1 = class(TForm)
    ncClientSource1: TncClientSource;
    pnlTop: TPanel;
    edtServerIP: TEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    lblStatus: TLabel;
    lblTotalData: TLabel;
    lblCurrentDelta: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure ncClientSource1Connected(Sender: TObject; aLine: TncLine);
    procedure ncClientSource1Disconnected(Sender: TObject; aLine: TncLine);
    function ncClientSource1HandleCommand(Sender: TObject; aLine: TncLine;
      aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
      const aSenderComponent, aReceiverComponent: string): TBytes;

  private
    FConnected: Boolean;
    FMouseControlEnabled: Boolean;
    FKeyboardControlEnabled: Boolean;
    FMouseHook: HHOOK;
    FKeyboardHook: HHOOK;
    FTotalBytesSent: Int64;
    FCurrentDeltaSize: Integer;
    FScreenCaptureThread: TScreenCaptureThread;

    // Performance tracking
    FLastBandwidthCheck: Cardinal;
    FBytesLastSecond: Integer;
    FCompressionRatio: Single;

    procedure InstallHooks;
    procedure RemoveHooks;
    procedure ProcessMouseCommand(const AData: TBytes);
    procedure ProcessKeyCommand(const AData: TBytes);
    procedure UpdateDataLabels;
    function FormatBytes(ABytes: Int64): string;
    procedure UpdateBandwidthStats(BytesSent: Integer);

  public
    procedure SendCommand(ACmd: Integer; const AData: TBytes);
    procedure UpdateStats(ADeltaSize: Integer; ACompressionRatio: Single = 0);
    function GetConnected: Boolean;
  end;

var
  Form1: TForm1;

function MouseProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function KeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

implementation

{$R *.dfm}

// Optimized mouse hook - send ALL movements for responsiveness
function MouseProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  MouseStruct: PMSLLHOOKSTRUCT;
  MouseData: TMouseData;
  Data: TBytes;
begin
  Result := CallNextHookEx(Form1.FMouseHook, nCode, wParam, lParam);

  if (nCode >= 0) and Form1.FMouseControlEnabled and Form1.GetConnected then
  begin
    MouseStruct := PMSLLHOOKSTRUCT(lParam);

    MouseData.X := MouseStruct^.pt.X;
    MouseData.Y := MouseStruct^.pt.Y;
    MouseData.Button := 0;
    MouseData.Action := 0;

    case wParam of
      WM_MOUSEMOVE:
        begin
          // Send ALL mouse moves for instant responsiveness
          MouseData.Action := 0;
          Form1.FScreenCaptureThread.FLastMousePos := Point(MouseData.X, MouseData.Y);
        end;
      WM_LBUTTONDOWN: begin MouseData.Button := 1; MouseData.Action := 1; end;
      WM_LBUTTONUP: begin MouseData.Button := 1; MouseData.Action := 2; end;
      WM_RBUTTONDOWN: begin MouseData.Button := 2; MouseData.Action := 1; end;
      WM_RBUTTONUP: begin MouseData.Button := 2; MouseData.Action := 2; end;
    end;

    SetLength(Data, SizeOf(TMouseData));
    Move(MouseData, Data[0], SizeOf(TMouseData));
    Form1.SendCommand(CMD_MOUSE_EVENT, Data);
  end;
end;

function KeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KeyStruct: PKBDLLHOOKSTRUCT;
  KeyData: TKeyData;
  Data: TBytes;
begin
  Result := CallNextHookEx(Form1.FKeyboardHook, nCode, wParam, lParam);

  if (nCode >= 0) and Form1.FKeyboardControlEnabled and Form1.GetConnected then
  begin
    KeyStruct := PKBDLLHOOKSTRUCT(lParam);

    KeyData.VKey := KeyStruct^.vkCode;
    case wParam of
      WM_KEYDOWN, WM_SYSKEYDOWN: KeyData.Action := 1;
      WM_KEYUP, WM_SYSKEYUP: KeyData.Action := 2;
      else KeyData.Action := 0;
    end;

    SetLength(Data, SizeOf(TKeyData));
    Move(KeyData, Data[0], SizeOf(TKeyData));
    Form1.SendCommand(CMD_KEYBOARD_EVENT, Data);
  end;
end;

{===============================================================================
  TScreenCaptureThread - BANDWIDTH CONSCIOUS Implementation
===============================================================================}

constructor TScreenCaptureThread.Create(AForm: TForm1);
begin
  inherited Create(False);
  FForm := AForm;
  FFrameCounter := 0;
  FFrameInterval := 16; // ~60 FPS for instant updates

  FLastScreenshot := TBitmap.Create;
  FCurrentScreenshot := TBitmap.Create;
  FLastScreenshot.PixelFormat := pf24bit;
  FCurrentScreenshot.PixelFormat := pf24bit;

  FLastMousePos := Point(-1, -1);
end;

destructor TScreenCaptureThread.Destroy;
begin
  FLastScreenshot.Free;
  FCurrentScreenshot.Free;
  inherited;
end;

procedure TScreenCaptureThread.SetFrameRate(AFPS: Integer);
begin
  if AFPS > 0 then
    FFrameInterval := 1000 div AFPS
  else
    FFrameInterval := 16; // 60 FPS default for instant updates
end;

procedure TScreenCaptureThread.Execute;
begin
  while not Terminated do
  begin
    if FForm.GetConnected then
    begin
      CaptureScreen;
      Inc(FFrameCounter);

      // Send full screenshot periodically for sync
      if (FFrameCounter >= FULL_SCREENSHOT_INTERVAL) or FLastScreenshot.Empty then
      begin
        SendFullScreenshot;
        FFrameCounter := 0;
      end
      else
        SendSmartDelta;
    end;

    Sleep(FFrameInterval);
  end;
end;

procedure TScreenCaptureThread.CaptureScreen;
var
  DC: HDC;
  ScreenRect: TRect;
begin
  ScreenRect := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));

  FCurrentScreenshot.SetSize(ScreenRect.Right, ScreenRect.Bottom);
  FCurrentScreenshot.PixelFormat := pf24bit;

  DC := GetDC(0);
  try
    BitBlt(FCurrentScreenshot.Canvas.Handle, 0, 0,
           FCurrentScreenshot.Width, FCurrentScreenshot.Height,
           DC, 0, 0, SRCCOPY);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TScreenCaptureThread.SendFullScreenshot;
var
  Stream: TMemoryStream;
  Data: TBytes;
  JPEG: TJPEGImage;
begin
  if not FForm.GetConnected then Exit;

  Stream := TMemoryStream.Create;
  JPEG := TJPEGImage.Create;
  try
    // Use higher quality for better image
    JPEG.Assign(FCurrentScreenshot);
    JPEG.CompressionQuality := 85; // Higher quality
    JPEG.SaveToStream(Stream);

    SetLength(Data, Stream.Size);
    Move(Stream.Memory^, Data[0], Stream.Size);

    FForm.SendCommand(CMD_SCREENSHOT_FULL, Data);
    FLastScreenshot.Assign(FCurrentScreenshot);

    FFrameCounter := 0;
    FForm.UpdateStats(Length(Data), Stream.Size / (FCurrentScreenshot.Width * FCurrentScreenshot.Height * 3));
  finally
    JPEG.Free;
    Stream.Free;
  end;
end;

procedure TScreenCaptureThread.SendSmartDelta;
var
  DeltaData: TBytes;
  Header: TDeltaHeader;
begin
  if not FForm.GetConnected then Exit;

  // CRITICAL: Only compare if we have a valid previous screenshot
  if FLastScreenshot.Empty or
     (FLastScreenshot.Width <> FCurrentScreenshot.Width) or
     (FLastScreenshot.Height <> FCurrentScreenshot.Height) then
  begin
    // Force full screenshot if dimensions don't match or no previous image
    SendFullScreenshot;
    Exit;
  end;

  DeltaData := CreateUltraCompressedDelta(FLastScreenshot, FCurrentScreenshot);

  // Only send if there are ACTUAL changes
  if Length(DeltaData) > 0 then
  begin
    // Double-check that we have actual pixel changes
    Move(DeltaData[0], Header, SizeOf(TDeltaHeader));

    if Header.ChangeCount > 0 then
    begin
      FForm.SendCommand(CMD_SCREENSHOT_DELTA, DeltaData);
      FLastScreenshot.Assign(FCurrentScreenshot);
      FForm.UpdateStats(Length(DeltaData));
    end;
    // If ChangeCount = 0, don't send anything (no bandwidth waste)
  end;
  // If Length(DeltaData) = 0, there are truly no changes - send nothing
end;

function TScreenCaptureThread.CreateUltraCompressedDelta(const AOld, ANew: TBitmap): TBytes;
var
  Header: TDeltaHeader;
  Changes: array of TPixelDelta;
  ChangeCount: Integer;
  x, y: Integer;
  OldRow, NewRow: PRGBArray;
  OldPixel, NewPixel: TRGBTriple;
  Offset: Integer;
  ColorDistance: Integer;
  NewColor: Cardinal;
begin
  // Initialize result as empty
  SetLength(Result, 0);

  // Validate inputs
  if AOld.Empty or ANew.Empty or
     (AOld.Width <> ANew.Width) or
     (AOld.Height <> ANew.Height) then
    Exit;

  Header.Width := ANew.Width;
  Header.Height := ANew.Height;
  Header.ChangeCount := 0;
  Header.BlockChanges := 0;

  // Pre-allocate change array
  SetLength(Changes, MAX_DELTA_SIZE);
  ChangeCount := 0;

  AOld.PixelFormat := pf24bit;
  ANew.PixelFormat := pf24bit;

  // Simple pixel-by-pixel comparison for speed
  for y := 0 to ANew.Height - 1 do
  begin
    if ChangeCount >= MAX_DELTA_SIZE then Break;

    OldRow := PRGBArray(AOld.ScanLine[y]);
    NewRow := PRGBArray(ANew.ScanLine[y]);

    for x := 0 to ANew.Width - 1 do
    begin
      if ChangeCount >= MAX_DELTA_SIZE then Break;

      OldPixel := OldRow[x];
      NewPixel := NewRow[x];

      // Check for EXACT pixel differences first
      if (OldPixel.rgbtRed <> NewPixel.rgbtRed) or
         (OldPixel.rgbtGreen <> NewPixel.rgbtGreen) or
         (OldPixel.rgbtBlue <> NewPixel.rgbtBlue) then
      begin
        // Calculate color distance for threshold check
        ColorDistance := Abs(OldPixel.rgbtRed - NewPixel.rgbtRed) +
                        Abs(OldPixel.rgbtGreen - NewPixel.rgbtGreen) +
                        Abs(OldPixel.rgbtBlue - NewPixel.rgbtBlue);

        if ColorDistance >= DELTA_THRESHOLD then
        begin
          Changes[ChangeCount].X := x;
          Changes[ChangeCount].Y := y;

          // Pack RGB into 32-bit
          NewColor := (NewPixel.rgbtRed shl 16) or
                     (NewPixel.rgbtGreen shl 8) or
                     NewPixel.rgbtBlue;
          Changes[ChangeCount].Color := NewColor;

          Inc(ChangeCount);
        end;
      end;
    end;
  end;

  Header.ChangeCount := ChangeCount;
  Header.CompressionRatio := ChangeCount / (ANew.Width * ANew.Height / 100.0);

  // ONLY create result if there are actual changes
  if ChangeCount > 0 then
  begin
    SetLength(Result, SizeOf(TDeltaHeader) + (ChangeCount * SizeOf(TPixelDelta)));
    Move(Header, Result[0], SizeOf(TDeltaHeader));
    Offset := SizeOf(TDeltaHeader);
    Move(Changes[0], Result[Offset], ChangeCount * SizeOf(TPixelDelta));
  end;
  // If ChangeCount = 0, Result remains empty (Length = 0)
end;

{===============================================================================
  TForm1 Implementation
===============================================================================}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FConnected := False;
  FMouseControlEnabled := False;
  FKeyboardControlEnabled := False;
  FMouseHook := 0;
  FKeyboardHook := 0;
  FTotalBytesSent := 0;
  FCurrentDeltaSize := 0;
  FScreenCaptureThread := nil;
  FLastBandwidthCheck := GetTickCount;
  FBytesLastSecond := 0;
  FCompressionRatio := 0;
  btnDisconnect.Enabled := False;
  UpdateDataLabels;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FScreenCaptureThread) then
  begin
    FScreenCaptureThread.Terminate;
    FScreenCaptureThread.WaitFor;
    FScreenCaptureThread.Free;
  end;
  RemoveHooks;
end;

function TForm1.GetConnected: Boolean;
begin
  Result := FConnected;
end;

procedure TForm1.SendCommand(ACmd: Integer; const AData: TBytes);
begin
  if FConnected then
  begin
    ncClientSource1.ExecCommand(ACmd, AData, False);
    UpdateBandwidthStats(Length(AData));
  end;
end;

procedure TForm1.UpdateBandwidthStats(BytesSent: Integer);
var
  CurrentTime: Cardinal;
begin
  Inc(FBytesLastSecond, BytesSent);
  CurrentTime := GetTickCount;

  // Update bandwidth stats every second
  if CurrentTime - FLastBandwidthCheck >= 1000 then
  begin
    lblCurrentDelta.Caption := Format('Bandwidth: %s/s (Compression: %.1f%%)',
      [FormatBytes(FBytesLastSecond), FCompressionRatio * 100]);
    FBytesLastSecond := 0;
    FLastBandwidthCheck := CurrentTime;
  end;
end;

procedure TForm1.UpdateStats(ADeltaSize: Integer; ACompressionRatio: Single = 0);
begin
  TThread.Synchronize(nil, procedure
  begin
    FCurrentDeltaSize := ADeltaSize;
    Inc(FTotalBytesSent, ADeltaSize);
    if ACompressionRatio > 0 then
      FCompressionRatio := ACompressionRatio;
    UpdateDataLabels;
  end);
end;

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  try
    ncClientSource1.Host := edtServerIP.Text;
    ncClientSource1.Port := 3389;
    ncClientSource1.Active := True;
  except
    on E: Exception do
      ShowMessage('Connection failed: ' + E.Message);
  end;
end;

procedure TForm1.btnDisconnectClick(Sender: TObject);
begin
  ncClientSource1.Active := False;
end;

procedure TForm1.ncClientSource1Connected(Sender: TObject; aLine: TncLine);
begin
  FConnected := True;
  btnConnect.Enabled := False;
  btnDisconnect.Enabled := True;
  edtServerIP.Enabled := False;
  lblStatus.Caption := 'Connected - Bandwidth Conscious Mode';

  // Start bandwidth-conscious capture thread
  if not Assigned(FScreenCaptureThread) then
  begin
    FScreenCaptureThread := TScreenCaptureThread.Create(Self);
    FScreenCaptureThread.SetFrameRate(60); // 60 FPS for instant updates
  end;
end;

procedure TForm1.ncClientSource1Disconnected(Sender: TObject; aLine: TncLine);
begin
  FConnected := False;
  FMouseControlEnabled := False;
  FKeyboardControlEnabled := False;
  RemoveHooks;

  if Assigned(FScreenCaptureThread) then
  begin
    FScreenCaptureThread.Terminate;
    FScreenCaptureThread.WaitFor;
    FreeAndNil(FScreenCaptureThread);
  end;

  btnConnect.Enabled := True;
  btnDisconnect.Enabled := False;
  edtServerIP.Enabled := True;
  lblStatus.Caption := 'Disconnected';
end;

function TForm1.ncClientSource1HandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
begin
  Result := nil;

  case aCmd of
    CMD_MOUSE_TOGGLE:
      begin
        if Length(aData) > 0 then
        begin
          FMouseControlEnabled := aData[0] = 1;
          if FMouseControlEnabled then
            InstallHooks
          else if not FKeyboardControlEnabled then
            RemoveHooks;
        end;
      end;
    CMD_KEYBOARD_TOGGLE:
      begin
        if Length(aData) > 0 then
        begin
          FKeyboardControlEnabled := aData[0] = 1;
          if FKeyboardControlEnabled then
            InstallHooks
          else if not FMouseControlEnabled then
            RemoveHooks;
        end;
      end;
    CMD_MOUSE_EVENT:
      ProcessMouseCommand(aData);
    CMD_KEYBOARD_EVENT:
      ProcessKeyCommand(aData);
  end;
end;

procedure TForm1.InstallHooks;
begin
  if FMouseControlEnabled and (FMouseHook = 0) then
    FMouseHook := SetWindowsHookEx(WH_MOUSE_LL, @MouseProc, HInstance, 0);

  if FKeyboardControlEnabled and (FKeyboardHook = 0) then
    FKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @KeyboardProc, HInstance, 0);
end;

procedure TForm1.RemoveHooks;
begin
  if FMouseHook <> 0 then
  begin
    UnhookWindowsHookEx(FMouseHook);
    FMouseHook := 0;
  end;
  if FKeyboardHook <> 0 then
  begin
    UnhookWindowsHookEx(FKeyboardHook);
    FKeyboardHook := 0;
  end;
end;

procedure TForm1.ProcessMouseCommand(const AData: TBytes);
var
  MouseData: TMouseData;
begin
  if Length(AData) < SizeOf(TMouseData) then Exit;
  Move(AData[0], MouseData, SizeOf(TMouseData));

  case MouseData.Action of
    0: SetCursorPos(MouseData.X, MouseData.Y);
    1: begin
         SetCursorPos(MouseData.X, MouseData.Y);
         case MouseData.Button of
           1: mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
           2: mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
         end;
       end;
    2: begin
         case MouseData.Button of
           1: mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
           2: mouse_event(MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0);
         end;
       end;
  end;
end;

procedure TForm1.ProcessKeyCommand(const AData: TBytes);
var
  KeyData: TKeyData;
begin
  if Length(AData) < SizeOf(TKeyData) then Exit;
  Move(AData[0], KeyData, SizeOf(TKeyData));

  case KeyData.Action of
    1: keybd_event(KeyData.VKey, 0, 0, 0);
    2: keybd_event(KeyData.VKey, 0, KEYEVENTF_KEYUP, 0);
  end;
end;

procedure TForm1.UpdateDataLabels;
begin
  lblTotalData.Caption := 'Total Sent: ' + FormatBytes(FTotalBytesSent);
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

end.
