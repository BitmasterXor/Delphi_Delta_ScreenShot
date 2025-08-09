{
  Remote Desktop Protocol (RDP) Server Implementation

  This unit implements a server component for remote desktop functionality,
  allowing clients to connect and view/control the desktop remotely.

  Key Features:
  - Screenshot streaming with delta compression
  - Mouse and keyboard input forwarding
  - Real-time display updates
  - Connection management
}
unit RDPServer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  ncSources, RDPCommon, StreamManager, System.ZLib, Math;

type
  {
    Custom display panel for rendering remote desktop screenshots
    Handles thread-safe bitmap rendering with double buffering
  }
  TDisplayPanel = class(TPanel)
  private
    FBitmap: TBitmap;                    // Current screenshot bitmap
    FCriticalSection: TRTLCriticalSection; // Thread synchronization for bitmap access
  protected
    procedure Paint; override;           // Custom paint method for rendering bitmap
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBitmap(ABitmap: TBitmap);  // Thread-safe bitmap assignment
    procedure UpdateDisplay;               // Trigger display refresh
  end;

  {
    Main RDP Server Form
    Manages client connections, screenshot processing, and input forwarding
  }
  TRDPServerForm = class(TForm)
    // UI Components
    ncServerSource1: TncServerSource;    // Network server component
    pnlTop: TPanel;                      // Top control panel
    btnStartServer: TButton;             // Start server button
    btnStopServer: TButton;              // Stop server button
    btnToggleMouse: TButton;             // Toggle mouse control
    btnToggleKeyboard: TButton;          // Toggle keyboard control
    lblStatus: TLabel;                   // Status display label
    lblStats: TLabel;                    // Statistics display label
    scrollBox: TScrollBox;               // Container for display panel

    // Event Handlers - Form Management
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    // Event Handlers - Server Control
    procedure btnStartServerClick(Sender: TObject);
    procedure btnStopServerClick(Sender: TObject);
    procedure btnToggleMouseClick(Sender: TObject);
    procedure btnToggleKeyboardClick(Sender: TObject);

    // Event Handlers - Network Events
    procedure ncServerSource1Connected(Sender: TObject; aLine: TncLine);
    procedure ncServerSource1Disconnected(Sender: TObject; aLine: TncLine);
    function ncServerSource1HandleCommand(Sender: TObject; aLine: TncLine;
      aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
      const aSenderComponent, aReceiverComponent: string): TBytes;

    // Event Handlers - Mouse Input
    procedure DisplayPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DisplayPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DisplayPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DisplayPanelDblClick(Sender: TObject);

    // Event Handlers - Keyboard Input
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    // Connection Management
    FClientLine: TncLine;                // Active client connection

    // Display Management
    FCurrentScreenshot: TBitmap;         // Current screenshot bitmap
    FDisplayPanel: TDisplayPanel;        // Custom display panel component

    // Input Control Flags
    FMouseControlEnabled: Boolean;       // Mouse input forwarding enabled
    FKeyboardControlEnabled: Boolean;    // Keyboard input forwarding enabled

    // Statistics Tracking
    FTotalBytesReceived: Int64;         // Total bytes received from client
    FRemoteCursorX, FRemoteCursorY: Integer; // Remote cursor position
    FLastUpdateTime: Cardinal;          // Last update timestamp for FPS calculation
    FFrameCount: Integer;               // Frame counter for FPS calculation

    // Mouse State Tracking
    FLastClickTime: Cardinal;           // Last mouse click timestamp
    FLastClickButton: TMouseButton;     // Last clicked mouse button
    FLastClickX, FLastClickY: Integer;  // Last click coordinates
    FMouseButtonDown: Boolean;          // Mouse button currently pressed

    // Screenshot Streaming - Thread-Safe Implementation
    FStreamCS: TRTLCriticalSection;     // Critical section for stream access
    FBaseStream: TMemoryStream;         // Base screenshot for delta comparison
    FCurrentStream: TMemoryStream;      // Current processed screenshot
    FDeltaStream: TMemoryStream;        // Delta data stream
    FInitialReceived: Boolean;          // Flag indicating initial screenshot received

    // Screenshot Processing Methods
    procedure ProcessScreenshotFull(const AData: TBytes);   // Process initial full screenshot
    procedure ProcessScreenshotDelta(const AData: TBytes);  // Process delta updates

    // Input Forwarding Methods
    procedure SendMouseEvent(X, Y: Integer; Button, Action: Byte);  // Send mouse event to client
    procedure SendKeyEvent(VKey: Word; Action: Byte);              // Send keyboard event to client

    // UI Update Methods
    procedure UpdateStats(ADataSize: Integer);              // Update statistics display
    procedure CreateDisplayPanel;                           // Initialize display panel
    procedure UpdateRemoteCursor(X, Y: Integer);           // Update cursor position tracking
    procedure SafeUpdateDisplay;                           // Thread-safe display update

    // Stream Processing Utilities
    function DecompressStreamData(const Data: TBytes): TBytes;  // Decompress received data
    function StreamToBitmap(Stream: TMemoryStream): Boolean;    // Convert stream to bitmap
    procedure SafeClearAllStreams;                             // Thread-safe stream cleanup

  public
    // Public interface for sending commands to client
    procedure SendCommand(ACmd: Integer; const AData: TBytes);
  end;

var
  RDPServerForm: TRDPServerForm;  // Global form instance

implementation

{$R *.dfm}

{ TDisplayPanel Implementation }

{
  Constructor: Initialize display panel with bitmap and synchronization
}
constructor TDisplayPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initialize bitmap for screenshot display
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf32bit;           // 32-bit pixel format for better quality

  // Initialize thread synchronization
  InitializeCriticalSection(FCriticalSection);

  // Optimize painting performance
  ControlStyle := ControlStyle + [csOpaque];  // Don't erase background
  DoubleBuffered := True;                     // Enable double buffering
  Canvas.Brush.Style := bsSolid;              // Solid brush for background
end;

{
  Destructor: Clean up bitmap and synchronization objects
}
destructor TDisplayPanel.Destroy;
begin
  // Thread-safe cleanup of bitmap
  EnterCriticalSection(FCriticalSection);
  try
    FBitmap.Free;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  // Clean up synchronization object
  DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
end;

{
  Custom paint method: Render bitmap with high-quality stretching
}
procedure TDisplayPanel.Paint;
var
  R: TRect;
  OldMode: Integer;
begin
  // Try to enter critical section without blocking
  if TryEnterCriticalSection(FCriticalSection) then
  try
    // Check if bitmap is available and valid
    if Assigned(FBitmap) and not FBitmap.Empty then
    begin
      R := ClientRect;

      // Set high-quality stretch mode for better image quality
      OldMode := SetStretchBltMode(Canvas.Handle, HALFTONE);
      SetBrushOrgEx(Canvas.Handle, 0, 0, nil);

      try
        // Stretch and draw the bitmap to fit the panel
        Canvas.StretchDraw(R, FBitmap);
      finally
        // Restore original stretch mode
        SetStretchBltMode(Canvas.Handle, OldMode);
      end;
    end
    else
    begin
      // No bitmap available - show waiting message
      Canvas.Brush.Color := clBlack;
      Canvas.FillRect(ClientRect);
      Canvas.Font.Color := clWhite;
      Canvas.Font.Size := 12;
      Canvas.TextOut(10, 10, 'Waiting for initial screenshot...');
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

{
  Thread-safe bitmap assignment
}
procedure TDisplayPanel.SetBitmap(ABitmap: TBitmap);
begin
  if not Assigned(ABitmap) then Exit;

  // Thread-safe bitmap assignment
  EnterCriticalSection(FCriticalSection);
  try
    FBitmap.Assign(ABitmap);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

{
  Trigger display update without full repaint
}
procedure TDisplayPanel.UpdateDisplay;
begin
  if HandleAllocated then
  begin
    // Invalidate without erasing background for better performance
    InvalidateRect(Handle, nil, False);
    Update;  // Force immediate repaint
  end;
end;

{ TRDPServerForm Implementation }

{
  Form constructor: Initialize all components and state variables
}
procedure TRDPServerForm.FormCreate(Sender: TObject);
begin
  // Initialize connection state
  FClientLine := nil;

  // Initialize input control states
  FMouseControlEnabled := False;
  FKeyboardControlEnabled := False;

  // Initialize statistics tracking
  FTotalBytesReceived := 0;
  FRemoteCursorX := 0;
  FRemoteCursorY := 0;
  FLastUpdateTime := GetTickCount;
  FFrameCount := 0;

  // Initialize mouse state tracking
  FLastClickTime := 0;
  FLastClickButton := mbLeft;
  FLastClickX := -1;
  FLastClickY := -1;
  FMouseButtonDown := False;

  // Initialize screenshot processing state
  FInitialReceived := False;

  // Create bitmap for screenshot display
  FCurrentScreenshot := TBitmap.Create;
  FCurrentScreenshot.PixelFormat := pf32bit;

  // Initialize stream management (matches client implementation)
  InitializeCriticalSection(FStreamCS);
  FBaseStream := TMemoryStream.Create;      // Stores base screenshot
  FCurrentStream := TMemoryStream.Create;   // Working screenshot
  FDeltaStream := TMemoryStream.Create;     // Delta data

  // Create custom display panel
  CreateDisplayPanel;

  // Set initial UI state
  btnStopServer.Enabled := False;
  btnToggleMouse.Enabled := False;
  btnToggleKeyboard.Enabled := False;
  lblStatus.Caption := 'Ready to start server';

  // Enable form-level keyboard handling
  KeyPreview := True;
  DoubleBuffered := True;
end;

{
  Form destructor: Clean up all resources
}
procedure TRDPServerForm.FormDestroy(Sender: TObject);
begin
  // Clean up screenshot bitmap
  if Assigned(FCurrentScreenshot) then
    FCurrentScreenshot.Free;

  // Thread-safe cleanup of streams (matches client implementation)
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

  // Clean up synchronization object
  DeleteCriticalSection(FStreamCS);
end;

{
  Thread-safe method to clear all screenshot streams
}
procedure TRDPServerForm.SafeClearAllStreams;
begin
  // Try to enter critical section without blocking
  if TryEnterCriticalSection(FStreamCS) then
  try
    // Clear all streams to reset state
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

{
  Initialize the custom display panel component
}
procedure TRDPServerForm.CreateDisplayPanel;
begin
  // Create display panel with event handlers
  FDisplayPanel := TDisplayPanel.Create(Self);
  FDisplayPanel.Parent := scrollBox;
  FDisplayPanel.Align := alClient;          // Fill the scroll box
  FDisplayPanel.Color := clBlack;

  // Assign mouse event handlers
  FDisplayPanel.OnMouseDown := DisplayPanelMouseDown;
  FDisplayPanel.OnMouseUp := DisplayPanelMouseUp;
  FDisplayPanel.OnMouseMove := DisplayPanelMouseMove;
  FDisplayPanel.OnDblClick := DisplayPanelDblClick;

  // Enable double buffering for smooth rendering
  FDisplayPanel.DoubleBuffered := True;
end;

{
  Decompress received screenshot data using ZLib
}
function TRDPServerForm.DecompressStreamData(const Data: TBytes): TBytes;
var
  InStream, OutStream: TMemoryStream;
  DecompStream: TDecompressionStream;
begin
  Result := nil;
  if Length(Data) = 0 then Exit;

  // Create streams for decompression
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  try
    // Load compressed data
    InStream.WriteBuffer(Data[0], Length(Data));
    InStream.Position := 0;

    // Decompress using ZLib
    DecompStream := TDecompressionStream.Create(InStream);
    try
      OutStream.CopyFrom(DecompStream, 0);

      // Convert decompressed stream to byte array
      if OutStream.Size > 0 then
      begin
        SetLength(Result, OutStream.Size);
        OutStream.Position := 0;
        OutStream.ReadBuffer(Result[0], OutStream.Size);
      end;
    finally
      DecompStream.Free;
    end;
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

{
  Convert memory stream containing bitmap data to TBitmap object
}
function TRDPServerForm.StreamToBitmap(Stream: TMemoryStream): Boolean;
var
  TempBitmap: TBitmap;
begin
  Result := False;
  if not Assigned(Stream) or (Stream.Size = 0) then Exit;

  try
    TempBitmap := TBitmap.Create;
    try
      // Load bitmap from stream
      Stream.Position := 0;
      TempBitmap.LoadFromStream(Stream);

      // Validate bitmap dimensions
      if (TempBitmap.Width > 0) and (TempBitmap.Height > 0) then
      begin
        // Assign to current screenshot
        FCurrentScreenshot.Assign(TempBitmap);
        Result := True;
      end;
    finally
      TempBitmap.Free;
    end;
  except
    Result := False;
  end;
end;

{
  Process initial full screenshot from client
  This is called once when client first connects
}
procedure TRDPServerForm.ProcessScreenshotFull(const AData: TBytes);
var
  DecompressedData: TBytes;
begin
  if Length(AData) = 0 then Exit;

  try
    lblStatus.Caption := 'Processing initial screenshot...';

    // Decompress the screenshot data
    DecompressedData := DecompressStreamData(AData);
    if Length(DecompressedData) = 0 then
    begin
      lblStatus.Caption := 'Failed to decompress initial screenshot';
      Exit;
    end;

    // Thread-safe processing
    EnterCriticalSection(FStreamCS);
    try
      // Store decompressed data as base stream for future delta comparisons
      FBaseStream.Clear;
      FBaseStream.WriteBuffer(DecompressedData[0], Length(DecompressedData));

      // Convert stream to bitmap and display
      if StreamToBitmap(FBaseStream) then
      begin
        // Update display with initial screenshot
        SafeUpdateDisplay;
        FInitialReceived := True;

        lblStatus.Caption := Format('Initial screenshot received: %dx%d - Delta mode active',
          [FCurrentScreenshot.Width, FCurrentScreenshot.Height]);

        // Reset performance counters
        FFrameCount := 0;
        FLastUpdateTime := GetTickCount;
      end
      else
      begin
        lblStatus.Caption := 'Failed to convert initial screenshot to bitmap';
        FInitialReceived := False;
        FBaseStream.Clear;
      end;
    finally
      LeaveCriticalSection(FStreamCS);
    end;

  except
    on E: Exception do
    begin
      lblStatus.Caption := 'Error processing initial screenshot: ' + E.Message;
      FInitialReceived := False;
      SafeClearAllStreams;
    end;
  end;
end;

{
  Process delta screenshot updates from client
  This is called for all subsequent updates after initial screenshot
}
procedure TRDPServerForm.ProcessScreenshotDelta(const AData: TBytes);
var
  DecompressedData: TBytes;
begin
  // Ensure we have received initial screenshot before processing deltas
  if not FInitialReceived then
  begin
    lblStatus.Caption := 'Delta received but no initial - ignoring';
    Exit;
  end;

  if Length(AData) = 0 then Exit;

  try
    // Decompress delta data
    DecompressedData := DecompressStreamData(AData);
    if Length(DecompressedData) = 0 then Exit;

    // Thread-safe delta processing
    EnterCriticalSection(FStreamCS);
    try
      // Validate base stream exists
      if FBaseStream.Size = 0 then
      begin
        lblStatus.Caption := 'No base image - delta ignored';
        Exit;
      end;

      // Validate delta size matches base size
      if Length(DecompressedData) <> FBaseStream.Size then
      begin
        lblStatus.Caption := 'Delta size mismatch - ignoring';
        Exit;
      end;

      // Store delta data for processing
      FDeltaStream.Clear;
      FDeltaStream.WriteBuffer(DecompressedData[0], Length(DecompressedData));

      // Apply delta to base stream using ResumeStream function
      // This reconstructs the current image by combining base + delta
      ResumeStream(FBaseStream, FCurrentStream, FDeltaStream);

      // Convert reconstructed stream to bitmap
      if StreamToBitmap(FCurrentStream) then
      begin
        // Update display with new screenshot
        SafeUpdateDisplay;
        Inc(FFrameCount);

        // Update FPS display every second
        if (GetTickCount - FLastUpdateTime) >= 1000 then
        begin
          lblStatus.Caption := Format('Delta mode - %d FPS | Base: %s',
            [FFrameCount, FormatBytes(FBaseStream.Size)]);
          FLastUpdateTime := GetTickCount;
          FFrameCount := 0;
        end;

        // CRITICAL: Update base stream with new image for next delta comparison
        // This ensures the next delta will be calculated against the current state
        FBaseStream.Clear;
        FCurrentStream.Position := 0;
        FBaseStream.CopyFrom(FCurrentStream, FCurrentStream.Size);
      end;

    finally
      LeaveCriticalSection(FStreamCS);
    end;

  except
    on E: Exception do
    begin
      lblStatus.Caption := 'Delta processing error: ' + E.Message;
      // Don't reset streams - just skip this frame to maintain stability
    end;
  end;
end;

{
  Thread-safe display update method
}
procedure TRDPServerForm.SafeUpdateDisplay;
begin
  try
    if Assigned(FDisplayPanel) and not FCurrentScreenshot.Empty then
    begin
      // Update display panel with current screenshot
      FDisplayPanel.SetBitmap(FCurrentScreenshot);
      FDisplayPanel.UpdateDisplay;
    end;
  except
    on E: Exception do
      lblStatus.Caption := 'Display update error: ' + E.Message;
  end;
end;

{
  Send command to connected client
}
procedure TRDPServerForm.SendCommand(ACmd: Integer; const AData: TBytes);
begin
  // Ensure client is connected and server is active
  if Assigned(FClientLine) and ncServerSource1.Active then
  begin
    try
      // Execute command without expecting return value
      ncServerSource1.ExecCommand(FClientLine, ACmd, AData, False);
    except
      on E: Exception do
        lblStatus.Caption := 'Send error: ' + E.Message;
    end;
  end;
end;

{
  Update statistics display with received data information
}
procedure TRDPServerForm.UpdateStats(ADataSize: Integer);
begin
  // Track total bytes received
  Inc(FTotalBytesReceived, ADataSize);

  // Display statistics including total, last packet size, and frame count
  lblStats.Caption := Format('Received: %s | Last: %s | Frames: %d',
    [FormatBytes(FTotalBytesReceived), FormatBytes(ADataSize), FFrameCount]);
end;

{
  Send mouse event to client with coordinate translation
}
procedure TRDPServerForm.SendMouseEvent(X, Y: Integer; Button, Action: Byte);
var
  Data: TBytes;
  MouseData: TMouseData;
begin
  // Only send if client connected and mouse control enabled
  if (FClientLine = nil) or not FMouseControlEnabled then Exit;

  // Prepare mouse event data structure
  MouseData.X := X;
  MouseData.Y := Y;
  MouseData.Button := Button;    // 0=move, 1=left, 2=right, 3=middle
  MouseData.Action := Action;    // 0=move, 1=down, 2=up, 3=double-click

  // Convert structure to byte array for transmission
  SetLength(Data, SizeOf(TMouseData));
  Move(MouseData, Data[0], SizeOf(TMouseData));

  // Send to client
  SendCommand(CMD_MOUSE_EVENT, Data);
end;

{
  Send keyboard event to client
}
procedure TRDPServerForm.SendKeyEvent(VKey: Word; Action: Byte);
var
  Data: TBytes;
  KeyData: TKeyData;
begin
  // Only send if client connected and keyboard control enabled
  if (FClientLine = nil) or not FKeyboardControlEnabled then Exit;

  // Prepare keyboard event data structure
  KeyData.VKey := VKey;          // Virtual key code
  KeyData.Action := Action;      // 1=key down, 2=key up

  // Convert structure to byte array for transmission
  SetLength(Data, SizeOf(TKeyData));
  Move(KeyData, Data[0], SizeOf(TKeyData));

  // Send to client
  SendCommand(CMD_KEYBOARD_EVENT, Data);
end;

{
  Update remote cursor position tracking
}
procedure TRDPServerForm.UpdateRemoteCursor(X, Y: Integer);
begin
  FRemoteCursorX := X;
  FRemoteCursorY := Y;
end;

{
  Handle mouse button press on display panel
  Translates local coordinates to remote coordinates and handles double-clicks
}
procedure TRDPServerForm.DisplayPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  BtnCode: Byte;
  RemoteX, RemoteY: Integer;
  CurrentTime: Cardinal;
begin
  if not FMouseControlEnabled then Exit;

  // Convert mouse button to protocol code
  case Button of
    mbLeft: BtnCode := 1;
    mbRight: BtnCode := 2;
    mbMiddle: BtnCode := 3;
    else Exit;
  end;

  // Translate display panel coordinates to remote screen coordinates
  if not FCurrentScreenshot.Empty then
  begin
    RemoteX := Round((X / FDisplayPanel.Width) * FCurrentScreenshot.Width);
    RemoteY := Round((Y / FDisplayPanel.Height) * FCurrentScreenshot.Height);
  end
  else
  begin
    // No screenshot available - use direct coordinates
    RemoteX := X;
    RemoteY := Y;
  end;

  FMouseButtonDown := True;
  CurrentTime := GetTickCount;

  // Double-click detection
  if (Button = FLastClickButton) and
     (Abs(X - FLastClickX) < 5) and (Abs(Y - FLastClickY) < 5) and
     (CurrentTime - FLastClickTime < GetDoubleClickTime) then
  begin
    // Send double-click event
    SendMouseEvent(RemoteX, RemoteY, BtnCode, 3);
  end
  else
  begin
    // Send mouse down event
    SendMouseEvent(RemoteX, RemoteY, BtnCode, 1);
  end;

  // Update click tracking for double-click detection
  FLastClickTime := CurrentTime;
  FLastClickButton := Button;
  FLastClickX := X;
  FLastClickY := Y;
end;

{
  Handle mouse button release on display panel
}
procedure TRDPServerForm.DisplayPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  BtnCode: Byte;
  RemoteX, RemoteY: Integer;
begin
  if not FMouseControlEnabled then Exit;

  // Convert mouse button to protocol code
  case Button of
    mbLeft: BtnCode := 1;
    mbRight: BtnCode := 2;
    mbMiddle: BtnCode := 3;
    else Exit;
  end;

  // Translate coordinates to remote screen
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

  FMouseButtonDown := False;

  // Send mouse up event
  SendMouseEvent(RemoteX, RemoteY, BtnCode, 2);
end;

{
  Handle mouse movement on display panel
  Sends continuous mouse position updates to client
}
procedure TRDPServerForm.DisplayPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  RemoteX, RemoteY: Integer;
begin
  if not FMouseControlEnabled then Exit;

  // Translate coordinates to remote screen
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

  // Send mouse move event (button=0, action=0 for movement)
  SendMouseEvent(RemoteX, RemoteY, 0, 0);
end;

{
  Handle double-click on display panel
  Alternative double-click handling method
}
procedure TRDPServerForm.DisplayPanelDblClick(Sender: TObject);
var
  P: TPoint;
  RemoteX, RemoteY: Integer;
begin
  if not FMouseControlEnabled then Exit;

  // Get current mouse position relative to display panel
  P := FDisplayPanel.ScreenToClient(Mouse.CursorPos);

  // Translate to remote coordinates
  if not FCurrentScreenshot.Empty then
  begin
    RemoteX := Round((P.X / FDisplayPanel.Width) * FCurrentScreenshot.Width);
    RemoteY := Round((P.Y / FDisplayPanel.Height) * FCurrentScreenshot.Height);
  end
  else
  begin
    RemoteX := P.X;
    RemoteY := P.Y;
  end;

  // Send double-click event for left button
  SendMouseEvent(RemoteX, RemoteY, 1, 3);
end;

{
  Handle key press events
  Forwards keyboard input to remote client
}
procedure TRDPServerForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FKeyboardControlEnabled then
  begin
    // Send key down event to client
    SendKeyEvent(Key, 1);
    Key := 0;  // Prevent local handling
  end;
end;

{
  Handle key release events
  Forwards keyboard input to remote client
}
procedure TRDPServerForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FKeyboardControlEnabled then
  begin
    // Send key up event to client
    SendKeyEvent(Key, 2);
    Key := 0;  // Prevent local handling
  end;
end;

{
  Start the RDP server and begin listening for connections
}
procedure TRDPServerForm.btnStartServerClick(Sender: TObject);
begin
  try
    // Configure and start network server
    ncServerSource1.Port := RDP_PORT;
    ncServerSource1.Active := True;

    // Update UI state
    btnStartServer.Enabled := False;
    btnStopServer.Enabled := True;
    lblStatus.Caption := 'Server running on port ' + IntToStr(RDP_PORT);
  except
    on E: Exception do
      ShowMessage('Server start failed: ' + E.Message);
  end;
end;

{
  Stop the RDP server and reset all states
}
procedure TRDPServerForm.btnStopServerClick(Sender: TObject);
begin
  // Stop network server
  ncServerSource1.Active := False;

  // Reset UI state
  btnStartServer.Enabled := True;
  btnStopServer.Enabled := False;
  btnToggleMouse.Enabled := False;
  btnToggleKeyboard.Enabled := False;

  // Reset connection and control states
  FClientLine := nil;
  FMouseControlEnabled := False;
  FKeyboardControlEnabled := False;
  FInitialReceived := False;
  FFrameCount := 0;

  // Reset button captions
  btnToggleMouse.Caption := 'Enable Mouse';
  btnToggleKeyboard.Caption := 'Enable Keyboard';
  lblStatus.Caption := 'Server stopped';

  // Clear all screenshot streams
  SafeClearAllStreams;

  // Clear display panel
  if Assigned(FDisplayPanel) then
    FDisplayPanel.SetBitmap(nil);
end;

{
  Toggle mouse control on/off and notify client
}
procedure TRDPServerForm.btnToggleMouseClick(Sender: TObject);
var
  Data: TBytes;
begin
  if FClientLine = nil then Exit;

  // Toggle mouse control state
  FMouseControlEnabled := not FMouseControlEnabled;

  // Prepare command data with new state
  SetLength(Data, 1);
  Data[0] := Byte(FMouseControlEnabled);

  // Send toggle command to client
  SendCommand(CMD_MOUSE_TOGGLE, Data);

  // Update button caption to reflect current state
  if FMouseControlEnabled then
    btnToggleMouse.Caption := 'Disable Mouse'
  else
    btnToggleMouse.Caption := 'Enable Mouse';
end;

{
  Toggle keyboard control on/off and notify client
}
procedure TRDPServerForm.btnToggleKeyboardClick(Sender: TObject);
var
  Data: TBytes;
begin
  if FClientLine = nil then Exit;

  // Toggle keyboard control state
  FKeyboardControlEnabled := not FKeyboardControlEnabled;

  // Prepare command data with new state
  SetLength(Data, 1);
  Data[0] := Byte(FKeyboardControlEnabled);

  // Send toggle command to client
  SendCommand(CMD_KEYBOARD_TOGGLE, Data);

  // Update button caption to reflect current state
  if FKeyboardControlEnabled then
    btnToggleKeyboard.Caption := 'Disable Keyboard'
  else
    btnToggleKeyboard.Caption := 'Enable Keyboard';
end;

{
  Handle client connection event
  Initialize connection state and prepare for screenshot reception
}
procedure TRDPServerForm.ncServerSource1Connected(Sender: TObject; aLine: TncLine);
begin
  // Store client connection reference
  FClientLine := aLine;

  // Enable control buttons
  btnToggleMouse.Enabled := True;
  btnToggleKeyboard.Enabled := True;

  // Update status display with client information
  lblStatus.Caption := 'Client connected: ' + aLine.PeerIP + ' - Waiting for initial...';

  // Reset screenshot processing state for new connection
  FInitialReceived := False;
  FFrameCount := 0;
  FLastUpdateTime := GetTickCount;

  // Clear all streams for fresh start
  SafeClearAllStreams;

  // Clear display panel and force refresh
  if Assigned(FDisplayPanel) then
  begin
    FDisplayPanel.SetBitmap(nil);
    FDisplayPanel.Invalidate;
  end;
end;

{
  Handle client disconnection event
  Reset all states and disable controls
}
procedure TRDPServerForm.ncServerSource1Disconnected(Sender: TObject; aLine: TncLine);
begin
  // Clear connection reference
  FClientLine := nil;

  // Disable control buttons
  btnToggleMouse.Enabled := False;
  btnToggleKeyboard.Enabled := False;

  // Reset control states
  FMouseControlEnabled := False;
  FKeyboardControlEnabled := False;
  FInitialReceived := False;
  FFrameCount := 0;

  // Reset button captions
  btnToggleMouse.Caption := 'Enable Mouse';
  btnToggleKeyboard.Caption := 'Enable Keyboard';
  lblStatus.Caption := 'Client disconnected';

  // Clean up all screenshot data
  SafeClearAllStreams;

  // Clear display panel
  if Assigned(FDisplayPanel) then
    FDisplayPanel.SetBitmap(nil);
end;

{
  Handle incoming commands from client
  Main command processing hub for screenshot data and other client messages
}
function TRDPServerForm.ncServerSource1HandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
var
  MouseData: TMouseData;
begin
  Result := nil;

  // Process command based on command type
  case aCmd of
    CMD_SCREENSHOT_FULL:
      begin
        // Initial full screenshot - received once when client connects
        // Contains complete screenshot data for initial display
        ProcessScreenshotFull(aData);
      end;

    CMD_SCREENSHOT_DELTA:
      begin
        // Delta screenshot update - received for all subsequent updates
        // Contains only changes from previous screenshot for efficiency
        ProcessScreenshotDelta(aData);
      end;

    CMD_MOUSE_EVENT:
      begin
        // Mouse position update from client (for cursor tracking)
        if Length(aData) >= SizeOf(TMouseData) then
        begin
          Move(aData[0], MouseData, SizeOf(TMouseData));
          UpdateRemoteCursor(MouseData.X, MouseData.Y);
        end;
      end;
  end;

  // Update statistics with received data size
  UpdateStats(Length(aData));
end;

end.
