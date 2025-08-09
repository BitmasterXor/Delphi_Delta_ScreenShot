unit RDPClient;

{
  RDPClient.pas
  -------------
  A lightweight remote screen capture + remote input client.

  Key responsibilities:
    - Connect to a remote RDP server via ncClientSource1 (networking handled by ncSources).
    - Capture the local screen, send a full screenshot once, then send delta updates.
    - Optionally enable remote mouse/keyboard control by setting low-level hooks and
      forwarding events to the server.
    - Compress captured screenshots with ZLib before sending.

  Notes:
    - This file intentionally keeps stream handling simple: three memory streams
      (base/current/delta) and a critical section to protect them.
    - Hooks and input injection are global (WH_MOUSE_LL and WH_KEYBOARD_LL).
    - Several operations swallow exceptions to avoid breaking hook chains / timers.
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  ncSources, RDPCommon, StreamManager, System.ZLib;

type
  TRDPClientForm = class(TForm)
    // Networking component (client side)
    ncClientSource1: TncClientSource;

    // Simple UI controls
    pnlTop: TPanel;
    edtServerIP: TEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    lblServerIP: TLabel;
    lblStatus: TLabel;   // status messages for user feedback
    lblStats: TLabel;    // stats: bytes sent / last payload size

    // Event handlers wired to the form and ncClientSource
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
    // Connection and control flags
    FConnected: Boolean;                 // whether client is currently connected
    FMouseControlEnabled: Boolean;       // whether remote mouse control is enabled
    FKeyboardControlEnabled: Boolean;    // whether remote keyboard control is enabled

    // Hooks for capturing local input (when sending input events to server)
    FMouseHook: HHOOK;
    FKeyboardHook: HHOOK;

    // Transmission statistics
    FTotalBytesSent: Int64;              // running total of bytes sent

    // Input processing guard
    FProcessingCommand: Boolean;         // true while processing incoming input commands
                                         // prevents re-entrancy / hook echo

    // Screen capture timer and mouse filtering
    FCaptureTimer: TTimer;               // timer used to trigger periodic capture
    FLastMousePos: TPoint;               // last mouse position recorded to avoid spam
    FMouseMoveThreshold: Integer;        // minimum movement (px) to send a move event

    // SIMPLE STREAMS - NO COMPLEX LOGIC
    // Protected by FStreamCS for thread safety
    FStreamCS: TRTLCriticalSection;
    FBaseStream: TMemoryStream;          // last full screenshot (baseline)
    FCurrentStream: TMemoryStream;       // newly captured screenshot
    FDeltaStream: TMemoryStream;         // delta between base and current

    FInitialSent: Boolean;               // indicates if initial full screenshot has been sent
    FCapturing: Boolean;                 // guard to prevent concurrent captures

    // Internal helpers
    procedure InstallHooks;
    procedure RemoveHooks;
    procedure ProcessMouseCommand(const AData: TBytes);
    procedure ProcessKeyCommand(const AData: TBytes);
    procedure OnCaptureTimer(Sender: TObject);
    procedure CaptureAndSendScreen;
    procedure SendInitialScreenshot;
    procedure SendDeltaScreenshot;

    // Compression and stream helpers
    function CompressStreamData(Stream: TMemoryStream): TBytes;
    procedure SafeClearAllStreams;

  public
    // Send a network command via ncClientSource1 (if connected)
    procedure SendCommand(ACmd: Integer; const AData: TBytes);
    // Update on-screen statistics (byte counters)
    procedure UpdateStats(ADataSize: Integer);
    property Connected: Boolean read FConnected;
  end;

var
  RDPClientForm: TRDPClientForm;

{
  Hook procedure signatures for low-level mouse/keyboard hooks.
  Implementations are below in the implementation section.
}
function MouseProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function KeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

implementation

{$R *.dfm}

const
  WH_MOUSE_LL = 14;
  WH_KEYBOARD_LL = 13;

  // flag in MSLLHOOKSTRUCT/KBDLLHOOKSTRUCT that marks injected events
  // used to avoid sending events that were injected by this program back to the server
  LLMHF_INJECTED = $00000001;

type
  // Local definitions for the hook structs (same layout as Windows)
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

{ ------------------------ Mouse hook callback ------------------------
  Global low-level mouse hook that captures user mouse events and forwards
  them to the server as CMD_MOUSE_EVENT when mouse-control is enabled.

  Important points:
    - We call CallNextHookEx first to preserve the hook chain behavior.
    - We early exit if the event should not be processed (nCode < 0, control disabled,
      already processing a command, or not connected).
    - We ignore injected events (LLMHF_INJECTED) to prevent feedback loops.
    - WM_MOUSEMOVE events are throttled by FMouseMoveThreshold to avoid flooding the network.
    - Mouse events are packaged into TMouseData and sent using SendCommand.
  ---------------------------------------------------------------------}
function MouseProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  MouseStruct: PMSLLHOOKSTRUCT;
  MouseData: TMouseData;
  Data: TBytes;
  CurrentPos: TPoint;
begin
  // Ensure the rest of the hook chain still runs
  Result := CallNextHookEx(RDPClientForm.FMouseHook, nCode, wParam, lParam);

  // Guard conditions - if any true, we do not process this event
  if (nCode < 0) or RDPClientForm.FProcessingCommand or
     not RDPClientForm.FMouseControlEnabled or not RDPClientForm.FConnected then
    Exit;

  try
    MouseStruct := PMSLLHOOKSTRUCT(lParam);
    // If pointer invalid or event was injected by software, ignore
    if not Assigned(MouseStruct) or ((MouseStruct^.flags and LLMHF_INJECTED) <> 0) then Exit;

    // Get current cursor position
    CurrentPos.X := MouseStruct^.pt.X;
    CurrentPos.Y := MouseStruct^.pt.Y;

    // Initialize TMouseData structure (defined in RDPCommon)
    MouseData.X := CurrentPos.X;
    MouseData.Y := CurrentPos.Y;
    MouseData.Button := 0;
    MouseData.Action := 0;

    case wParam of
      WM_MOUSEMOVE:
        begin
          // Only send move if beyond configured threshold to reduce network usage
          if (Abs(CurrentPos.X - RDPClientForm.FLastMousePos.X) > RDPClientForm.FMouseMoveThreshold) or
             (Abs(CurrentPos.Y - RDPClientForm.FLastMousePos.Y) > RDPClientForm.FMouseMoveThreshold) then
          begin
            MouseData.Action := 0; // 0 indicates move
            RDPClientForm.FLastMousePos := CurrentPos;
          end
          else
            Exit; // movement too small -> ignore
        end;

      // Button events map into Button and Action fields (1=left,2=right,3=middle)
      WM_LBUTTONDOWN: begin MouseData.Button := 1; MouseData.Action := 1; end;
      WM_LBUTTONUP:   begin MouseData.Button := 1; MouseData.Action := 2; end;
      WM_RBUTTONDOWN: begin MouseData.Button := 2; MouseData.Action := 1; end;
      WM_RBUTTONUP:   begin MouseData.Button := 2; MouseData.Action := 2; end;
      WM_MBUTTONDOWN: begin MouseData.Button := 3; MouseData.Action := 1; end;
      WM_MBUTTONUP:   begin MouseData.Button := 3; MouseData.Action := 2; end;
      WM_LBUTTONDBLCLK: begin MouseData.Button := 1; MouseData.Action := 3; end;
      else Exit; // ignore any other mouse messages
    end;

    // Serialize the TMouseData into a byte array and send
    SetLength(Data, SizeOf(TMouseData));
    Move(MouseData, Data[0], SizeOf(TMouseData));
    RDPClientForm.SendCommand(CMD_MOUSE_EVENT, Data);
  except
    // Swallow exceptions in the hook to avoid breaking system hooks
  end;
end;

{ ----------------------- Keyboard hook callback -----------------------
  Low-level keyboard hook: captures key presses/releases and forwards them
  as CMD_KEYBOARD_EVENT when keyboard-control is enabled.

  Important points:
    - CallNextHookEx is called first to keep the hook chain intact.
    - Injected key events are ignored to avoid echoing injected input.
    - We map WM_KEYDOWN/WM_SYSKEYDOWN to Action=1 (press) and
      WM_KEYUP/WM_SYSKEYUP to Action=2 (release).
    - The hook will not forward events if FProcessingCommand is true (guard).
  ---------------------------------------------------------------------}
function KeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KeyStruct: PKBDLLHOOKSTRUCT;
  KeyData: TKeyData;
  Data: TBytes;
begin
  Result := CallNextHookEx(RDPClientForm.FKeyboardHook, nCode, wParam, lParam);

  if (nCode >= 0) and RDPClientForm.FKeyboardControlEnabled and
     RDPClientForm.FConnected and not RDPClientForm.FProcessingCommand then
  begin
    try
      KeyStruct := PKBDLLHOOKSTRUCT(lParam);
      if not Assigned(KeyStruct) or ((KeyStruct^.flags and LLMHF_INJECTED) <> 0) then Exit;

      // Fill KeyData with the virtual key code
      KeyData.VKey := KeyStruct^.vkCode;

      case wParam of
        WM_KEYDOWN, WM_SYSKEYDOWN: KeyData.Action := 1; // key down
        WM_KEYUP, WM_SYSKEYUP:     KeyData.Action := 2; // key up
        else Exit;
      end;

      // Serialize and send the keyboard event
      SetLength(Data, SizeOf(TKeyData));
      Move(KeyData, Data[0], SizeOf(TKeyData));
      RDPClientForm.SendCommand(CMD_KEYBOARD_EVENT, Data);
    except
      // Swallow exceptions in hook
    end;
  end;
end;

{ --------------------------- Form lifecycle ---------------------------
  FormCreate: initialize state, create streams, timer, and critical section.
  FormDestroy: clean up timer, hooks, streams, and delete critical section.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.FormCreate(Sender: TObject);
begin
  // Initial state
  FConnected := False;
  FMouseControlEnabled := False;
  FKeyboardControlEnabled := False;
  FMouseHook := 0;
  FKeyboardHook := 0;
  FTotalBytesSent := 0;
  FProcessingCommand := False;
  FLastMousePos := Point(-1, -1); // sentinel so first move is always sent
  FMouseMoveThreshold := 2;       // default threshold in pixels
  FInitialSent := False;
  FCapturing := False;

  // UI defaults
  btnDisconnect.Enabled := False;
  lblStatus.Caption := 'Ready to connect';
  edtServerIP.Text := '127.0.0.1';

  // Initialize critical section protecting the three memory streams
  InitializeCriticalSection(FStreamCS);
  FBaseStream := TMemoryStream.Create;
  FCurrentStream := TMemoryStream.Create;
  FDeltaStream := TMemoryStream.Create;

  // Capture timer: roughly 30 FPS (33ms interval). Disabled until connected.
  FCaptureTimer := TTimer.Create(Self);
  FCaptureTimer.Interval := 33;
  FCaptureTimer.OnTimer := OnCaptureTimer;
  FCaptureTimer.Enabled := False;

  UpdateStats(0); // initialize stats display
end;

procedure TRDPClientForm.FormDestroy(Sender: TObject);
begin
  // Stop and free the capture timer
  if Assigned(FCaptureTimer) then
  begin
    FCaptureTimer.Enabled := False;
    FCaptureTimer.Free;
  end;

  // Unhook and clean up any global hooks
  RemoveHooks;

  // Free streams under critical section to ensure thread-safety
  EnterCriticalSection(FStreamCS);
  try
    if Assigned(FBaseStream) then
      FBaseStream.Free;
    if Assigned(FCurrentStream) then
      FCurrentStream.Free;
    if Assigned(FDeltaStream) then
      FDeltaStream.Free;
  finally
    LeaveCriticalSection(FStreamCS);
  end;

  DeleteCriticalSection(FStreamCS);
end;

{ --------------------------- Stream helpers ---------------------------
  SafeClearAllStreams: clears all streams if critical section available.
    - Uses TryEnterCriticalSection to avoid deadlock in edge cases.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.SafeClearAllStreams;
begin
  if TryEnterCriticalSection(FStreamCS) then
  try
    if Assigned(FBaseStream) then
      FBaseStream.Clear;
    if Assigned(FCurrentStream) then
      FCurrentStream.Clear;
    if Assigned(FDeltaStream) then
      FDeltaStream.Clear;
  finally
    LeaveCriticalSection(FStreamCS);
  end;
end;

{ --------------------- Compression helper (ZLib) ----------------------
  CompressStreamData: compresses the provided memory stream with zlib
  (TCompressionStream) and returns a byte array containing the compressed data.

  Notes:
    - The input stream's Position is reset to 0 before copying.
    - A temporary InStream and OutStream are used to avoid modifying the original.
    - If compression yields zero bytes, result is nil.
  ---------------------------------------------------------------------}
function TRDPClientForm.CompressStreamData(Stream: TMemoryStream): TBytes;
var
  InStream, OutStream: TMemoryStream;
  CompStream: TCompressionStream;
begin
  Result := nil;
  if not Assigned(Stream) or (Stream.Size = 0) then Exit;

  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  try
    Stream.Position := 0;
    InStream.CopyFrom(Stream, Stream.Size);
    InStream.Position := 0;

    // Compress to OutStream using default compression level
    CompStream := TCompressionStream.Create(clDefault, OutStream);
    try
      CompStream.CopyFrom(InStream, InStream.Size);
    finally
      CompStream.Free;
    end;

    // If compression produced output, return it as TBytes
    if OutStream.Size > 0 then
    begin
      SetLength(Result, OutStream.Size);
      OutStream.Position := 0;
      OutStream.ReadBuffer(Result[0], OutStream.Size);
    end;
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

{ --------------------------- Capture timer ---------------------------
  OnCaptureTimer: triggered by FCaptureTimer. Stops the timer briefly,
  runs CaptureAndSendScreen, then restarts the timer (if still connected).

  This prevents re-entrancy where the previous capture hasn't completed yet.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.OnCaptureTimer(Sender: TObject);
begin
  if FConnected and not FCapturing then
  begin
    // Temporarily stop the timer to avoid overlapping captures
    FCaptureTimer.Enabled := False;
    try
      CaptureAndSendScreen;
    finally
      // Restart timer if we are still connected
      if FConnected then
        FCaptureTimer.Enabled := True;
    end;
  end;
end;

{ ------------------- Capture & send orchestration ---------------------
  CaptureAndSendScreen:
    - Prevents concurrent captures via FCapturing flag.
    - If initial screenshot hasn't been sent, send full screenshot first.
    - Otherwise send delta screenshot.
    - Catches exceptions and resets the initial-sent flag on errors.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.CaptureAndSendScreen;
begin
  if FCapturing then Exit;

  FCapturing := True;
  try
    if not FConnected then Exit;

    if not FInitialSent then
      SendInitialScreenshot
    else
      SendDeltaScreenshot;

  except
    on E: Exception do
    begin
      lblStatus.Caption := 'Capture error: ' + E.Message;
      // Reset to force a full send next time
      FInitialSent := False;
      SafeClearAllStreams;
    end;
  end;

  FCapturing := False;
end;

{ ------------------------ Full screenshot send ------------------------
  SendInitialScreenshot:
    - Captures the entire screen into FBaseStream.
    - Compresses the captured BMP and sends CMD_SCREENSHOT_FULL to server.
    - Sets FInitialSent to true on success so subsequent captures send deltas.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.SendInitialScreenshot;
var
  CompressedData: TBytes;
begin
  EnterCriticalSection(FStreamCS);
  try
    FBaseStream.Clear;

    try
      // Capture the screen into FBaseStream.
      // GetScreenToBmp is expected to be provided by StreamManager or RDPCommon.
      GetScreenToBmp(True, FBaseStream);

      if FBaseStream.Size > 0 then
      begin
        CompressedData := CompressStreamData(FBaseStream);
        if Length(CompressedData) > 0 then
        begin
          // Send the full compressed screenshot to server
          SendCommand(CMD_SCREENSHOT_FULL, CompressedData);
          UpdateStats(Length(CompressedData));

          FInitialSent := True;
          lblStatus.Caption := 'Initial screenshot sent - Delta mode active';
        end
        else
        begin
          lblStatus.Caption := 'Failed to compress initial screenshot';
        end;
      end
      else
      begin
        lblStatus.Caption := 'Failed to capture initial screenshot';
      end;

    except
      on E: Exception do
      begin
        lblStatus.Caption := 'Initial capture error: ' + E.Message;
        FBaseStream.Clear;
      end;
    end;

  finally
    LeaveCriticalSection(FStreamCS);
  end;
end;

{ --------------------------- Delta send ------------------------------
  SendDeltaScreenshot:
    - Captures current screen into FCurrentStream.
    - If current and base sizes match, uses CompareStream (from StreamManager)
      to produce the difference into FDeltaStream.
    - If delta exists, compress and send CMD_SCREENSHOT_DELTA.
    - On a size mismatch (likely resolution change) it forces a full reset.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.SendDeltaScreenshot;
var
  CompressedData: TBytes;
begin
  EnterCriticalSection(FStreamCS);
  try
    // If there is no base, force initial resend
    if FBaseStream.Size = 0 then
    begin
      FInitialSent := False;
      Exit;
    end;

    FCurrentStream.Clear;

    try
      // Capture current screen
      GetScreenToBmp(True, FCurrentStream);

      // Only attempt a delta if sizes match (simple approach)
      if (FCurrentStream.Size > 0) and (FCurrentStream.Size = FBaseStream.Size) then
      begin
        // CompareStream handles producing a delta (implementation in StreamManager)
        CompareStream(FBaseStream, FCurrentStream, FDeltaStream);

        if FDeltaStream.Size > 0 then
        begin
          CompressedData := CompressStreamData(FDeltaStream);
          if Length(CompressedData) > 0 then
          begin
            SendCommand(CMD_SCREENSHOT_DELTA, CompressedData);
            UpdateStats(Length(CompressedData));
          end;
        end;
      end
      else
      begin
        // Screen size changed (resolution / multi-monitor layout), force full update
        lblStatus.Caption := 'Screen size changed - resetting';
        FInitialSent := False;
        SafeClearAllStreams;
      end;

    except
      on E: Exception do
      begin
        lblStatus.Caption := 'Delta capture error: ' + E.Message;
        FInitialSent := False;
        SafeClearAllStreams;
      end;
    end;

  finally
    LeaveCriticalSection(FStreamCS);
  end;
end;

{ --------------------------- UI actions ------------------------------
  btnConnectClick: configure ncClientSource1 with IP and RDP_PORT and activate it.
  btnDisconnectClick: deactivate the client source.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.btnConnectClick(Sender: TObject);
begin
  try
    // Reset capture state and streams prior to connecting
    FInitialSent := False;
    FCapturing := False;
    SafeClearAllStreams;

    // Configure network component and activate (connect)
    ncClientSource1.Host := edtServerIP.Text;
    ncClientSource1.Port := RDP_PORT;
    ncClientSource1.Active := True;
    lblStatus.Caption := 'Connecting...';
  except
    on E: Exception do
      ShowMessage('Connection failed: ' + E.Message);
  end;
end;

procedure TRDPClientForm.btnDisconnectClick(Sender: TObject);
begin
  try
    ncClientSource1.Active := False;
  except
    // ignore disconnect-time errors
  end;
end;

{ ---------------------- ncClientSource1 event handlers -----------------
  ncClientSource1Connected:
    - Called when network link is established.
    - Sets state, enables capture timer to start sending screenshots.
  ncClientSource1Disconnected:
    - Called when link is lost/closed.
    - Resets flags, removes hooks, stops capture timer, and clears streams.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.ncClientSource1Connected(Sender: TObject; aLine: TncLine);
begin
  FConnected := True;
  btnConnect.Enabled := False;
  btnDisconnect.Enabled := True;
  edtServerIP.Enabled := False;
  lblStatus.Caption := 'Connected - Preparing initial screenshot...';

  // Reset capture state and streams
  FInitialSent := False;
  FCapturing := False;
  SafeClearAllStreams;

  // Start capture loop
  FCaptureTimer.Enabled := True;
end;

procedure TRDPClientForm.ncClientSource1Disconnected(Sender: TObject; aLine: TncLine);
begin
  // Reset most runtime state
  FConnected := False;
  FMouseControlEnabled := False;
  FKeyboardControlEnabled := False;
  FInitialSent := False;
  FCapturing := False;

  // Remove any installed hooks and stop capture
  RemoveHooks;

  FCaptureTimer.Enabled := False;
  SafeClearAllStreams;

  // Restore UI state
  btnConnect.Enabled := True;
  btnDisconnect.Enabled := False;
  edtServerIP.Enabled := True;
  lblStatus.Caption := 'Disconnected';
end;

{ ---------------------- Command handling from server --------------------
  ncClientSource1HandleCommand:
    - Central dispatcher for commands coming from the server.
    - Handles toggling of mouse/keyboard control, update requests, rate changes,
      forced full updates, and forwarded mouse/keyboard events.
    - When toggling control, InstallHooks / RemoveHooks are called appropriately.
  ---------------------------------------------------------------------}
function TRDPClientForm.ncClientSource1HandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
var
  UpdateRate: Word;
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
            // only remove hooks if both mouse and keyboard control are off
            RemoveHooks;

          lblStatus.Caption := 'Mouse control: ' + BoolToStr(FMouseControlEnabled, True);
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

          lblStatus.Caption := 'Keyboard control: ' + BoolToStr(FKeyboardControlEnabled, True);
        end;
      end;

    CMD_REQUEST_UPDATE:
      begin
        // Server wants an immediate refresh; perform one if not already capturing
        if not FCapturing then
        begin
          lblStatus.Caption := 'Server requested update';
          CaptureAndSendScreen;
        end;
      end;

    CMD_SET_UPDATE_RATE:
      begin
        // Expecting at least a Word representing milliseconds
        if Length(aData) >= SizeOf(Word) then
        begin
          Move(aData[0], UpdateRate, SizeOf(Word));
          // Only accept sensible values (16ms -> ~60 FPS, 1000ms -> 1 FPS)
          if (UpdateRate >= 16) and (UpdateRate <= 1000) then
          begin
            FCaptureTimer.Interval := UpdateRate;
            lblStatus.Caption := Format('Update rate set to %dms', [UpdateRate]);
          end;
        end;
      end;

    CMD_FORCE_FULL_UPDATE:
      begin
        // Force a full screenshot on next capture
        FInitialSent := False;
        SafeClearAllStreams;
        lblStatus.Caption := 'Force full update requested';
      end;

    CMD_MOUSE_EVENT:
      // Incoming mouse action from server to be executed locally
      ProcessMouseCommand(aData);

    CMD_KEYBOARD_EVENT:
      // Incoming keyboard action from server to be executed locally
      ProcessKeyCommand(aData);
  end;
end;

{ ----------------------- Hook installation helpers ---------------------
  InstallHooks: sets low-level mouse and keyboard hooks when enabled.
  RemoveHooks: unhooks and zeroes hook handles.

  Note: SetWindowsHookEx with WH_*_LL requires that the module remains loaded.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.InstallHooks;
begin
  try
    if FMouseControlEnabled and (FMouseHook = 0) then
      FMouseHook := SetWindowsHookEx(WH_MOUSE_LL, @MouseProc, HInstance, 0);

    if FKeyboardControlEnabled and (FKeyboardHook = 0) then
      FKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @KeyboardProc, HInstance, 0);
  except
    on E: Exception do
      lblStatus.Caption := 'Hook error: ' + E.Message;
  end;
end;

procedure TRDPClientForm.RemoveHooks;
begin
  try
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
  except
    // ignore unhook errors
  end;
end;

{ ------------------- Process incoming mouse command --------------------
  ProcessMouseCommand:
    - Validates incoming data length and uses FProcessingCommand as a guard
      to avoid re-entrancy.
    - Moves the incoming TMouseData into MouseData and performs the action:
        Action=0 -> move cursor
        Action=1 -> press (down)
        Action=2 -> release (up)
        Action=3 -> double-click sequence (left button only)
    - Uses SetCursorPos and mouse_event to simulate input.
    - Note: We intentionally swallow exceptions here to avoid crashing the
      command processing path.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.ProcessMouseCommand(const AData: TBytes);
var
  MouseData: TMouseData;
begin
  if Length(AData) < SizeOf(TMouseData) then Exit;
  if FProcessingCommand then Exit;

  try
    FProcessingCommand := True;
    Move(AData[0], MouseData, SizeOf(TMouseData));

    case MouseData.Action of
      0: SetCursorPos(MouseData.X, MouseData.Y);
      1: begin
           // Mouse down at X,Y (then leave button pressed)
           SetCursorPos(MouseData.X, MouseData.Y);
           case MouseData.Button of
             1: mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
             2: mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
             3: mouse_event(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
           end;
         end;
      2: begin
           // Mouse up
           case MouseData.Button of
             1: mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
             2: mouse_event(MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0);
             3: mouse_event(MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0);
           end;
         end;
      3: begin
           // Double-click emulation (left button)
           SetCursorPos(MouseData.X, MouseData.Y);
           if MouseData.Button = 1 then
           begin
             mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
             mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
             Sleep(10);
             mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
             mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
           end;
         end;
    end;
  except
    // ignore any exceptions during input injection
  end;

  FProcessingCommand := False;
end;

{ ------------------- Process incoming key command ----------------------
  ProcessKeyCommand:
    - Expects a TKeyData payload.
    - Simulates key press / release using keybd_event.
    - No explicit re-entrancy guard here (hook guard used on capture side).
  ---------------------------------------------------------------------}
procedure TRDPClientForm.ProcessKeyCommand(const AData: TBytes);
var
  KeyData: TKeyData;
begin
  if Length(AData) < SizeOf(TKeyData) then Exit;

  try
    Move(AData[0], KeyData, SizeOf(TKeyData));

    case KeyData.Action of
      1: keybd_event(KeyData.VKey, 0, 0, 0);                // key down
      2: keybd_event(KeyData.VKey, 0, KEYEVENTF_KEYUP, 0); // key up
    end;
  except
    // swallow exceptions
  end;
end;

{ ------------------------ Network send helper ------------------------
  SendCommand: wrapper around ncClientSource1.ExecCommand that checks
  connection state and handles exceptions by updating lblStatus.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.SendCommand(ACmd: Integer; const AData: TBytes);
begin
  if FConnected and ncClientSource1.Active then
  begin
    try
      ncClientSource1.ExecCommand(ACmd, AData, False);
    except
      on E: Exception do
        lblStatus.Caption := 'Send error: ' + E.Message;
    end;
  end;
end;

{ --------------------------- Stats UI --------------------------------
  UpdateStats: increments total bytes sent and updates UI with a readable
  bytes representation. FormatBytes is assumed provided by RDPCommon or utility unit.
  ---------------------------------------------------------------------}
procedure TRDPClientForm.UpdateStats(ADataSize: Integer);
begin
  Inc(FTotalBytesSent, ADataSize);
  lblStats.Caption := Format('Sent: %s | Last: %s',
    [FormatBytes(FTotalBytesSent), FormatBytes(ADataSize)]);
end;

end.

