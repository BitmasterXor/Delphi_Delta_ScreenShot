{==============================================================================
  SCREENSHOT DIFFERENCE ANALYZER - SERVER (RECEIVER) - OPTIMIZED & CLEAN
  ==============================================================================

  Description : High-performance server application that receives screenshots
                and pixel delta updates from multiple clients using n/cSockets.

  Purpose     : Acts as a central receiver, processes and displays incoming
                screen images or diffs for efficient monitoring or comparison.

  Optimized   : All bitmap processing done using pf24bit + ScanLine access.

  Author      : BitmasterXor
  Date        : 08/05/2025
  Version     : 2.1 (Performance Optimized Clean Edition)
==============================================================================}

unit Unit1;

interface

uses
  // Core Windows API and VCL dependencies
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,

  // UI Components
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  // NetComms (n/cSockets) networking components
  ncSources, ncSocketList,

  // Additional VCL
  Vcl.ExtCtrls, Vcl.StdCtrls, System.Types;

const
  // Custom Command IDs sent between client and server
  CMD_REQUEST_SCREENSHOT     = 1; // Server asks client for full screenshot
  CMD_SEND_SCREENSHOT        = 2; // Client sends full screenshot
  CMD_SEND_DELTA_SCREENSHOT  = 3; // Client sends pixel delta (only changes)

type
  // Represents a single pixel color change (used in delta transmission)
  TPixelChange = packed record
    X, Y: Word;
    Color: TColor;
  end;

  // Metadata header describing the delta image
  TDeltaHeader = packed record
    Width, Height: Word;
    ChangeCount: Cardinal; // Number of TPixelChange records to follow
  end;

  // RGB structure used for fast pixel manipulation
  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;
  PRGBTriple = ^TRGBTriple;

  // For ScanLine[] casting
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..32767] of TRGBTriple;

  // Main Form Declaration
  TForm1 = class(TForm)
    // Network communication source (server)
    ncServerSource1: TncServerSource;

    // UI Components for status and image display
    ImageReceived: TImage;
    lblStatus: TLabel;
    lblClientInfo: TLabel;
    btnStartServer: TButton;
    btnRequestScreenshot: TButton;
    lblServerTitle: TLabel;
    memoLog: TMemo;
    lblStats: TLabel;
    lblPixelChanges: TLabel;

    // Event handlers
    procedure FormCreate(Sender: TObject);
    procedure btnStartServerClick(Sender: TObject);
    procedure btnRequestScreenshotClick(Sender: TObject);
    procedure ncServerSource1Connected(Sender: TObject; aLine: TncLine);
    procedure ncServerSource1Disconnected(Sender: TObject; aLine: TncLine);
    function  ncServerSource1HandleCommand(Sender: TObject; aLine: TncLine;
              aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
              const aSenderComponent, aReceiverComponent: string): TBytes;
    procedure FormDestroy(Sender: TObject);

  private
    // Internal performance + state tracking
    FConnectedClients: Integer;
    FTotalBytesReceived: Int64;
    FScreenshotCount: Integer;
    FLastScreenshot: TBitmap;
    FUpdatePending: Boolean;
    FLastDeltaSize: Int64;

    // Utility procedures
    procedure UpdateStatus(const AMessage: string);
    procedure LogActivity(const AMessage: string);
    procedure UpdateStats;
    function  FormatBytes(Bytes: Int64): string;

    // Core image handling
    procedure ApplyDeltaChangesFast(Bitmap: TBitmap; const DeltaData: TBytes);
    procedure DisplayFullScreenshotFast(const ImageData: TBytes);
    procedure RefreshDisplay;
  end;

// Global instance
var
  Form1: TForm1;

implementation

{$R *.dfm}

{==============================================================================
  FORM INITIALIZATION
==============================================================================}
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Init counters
  FConnectedClients := 0;
  FTotalBytesReceived := 0;
  FScreenshotCount := 0;
  FUpdatePending := False;
  FLastDeltaSize := 0;

  // Prepare bitmap memory with optimized pixel format
  FLastScreenshot := TBitmap.Create;
  FLastScreenshot.PixelFormat := pf24bit;

  // Same optimization for on-screen rendering
  ImageReceived.Picture.Bitmap := TBitmap.Create;
  ImageReceived.Picture.Bitmap.PixelFormat := pf24bit;

  // Notify user
  UpdateStatus('High-Performance Server ready - Click "Start Server"');
  LogActivity('Screenshot Analyzer Server v2.1 (Optimized) initialized');
end;

{==============================================================================
  FORM DESTRUCTION
==============================================================================}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Gracefully stop network server
  if ncServerSource1.Active then
    ncServerSource1.Active := False;

  // Free bitmap memory
  FLastScreenshot.Free;
end;

{==============================================================================
  START/STOP SERVER BUTTON HANDLER
==============================================================================}
procedure TForm1.btnStartServerClick(Sender: TObject);
begin
  try
    if not ncServerSource1.Active then
    begin
      // Activate server on default port 8080
      ncServerSource1.Active := True;
      btnStartServer.Caption := 'Stop Server';
      Caption := 'Screenshot Analyzer Server - High Performance Mode - Port 8080';
      UpdateStatus('High-performance server started on port 8080');
      LogActivity('Server started with optimizations enabled');
    end
    else
    begin
      // Shutdown server
      ncServerSource1.Active := False;
      btnStartServer.Caption := 'Start Server';
      btnRequestScreenshot.Enabled := False;
      Caption := 'Screenshot Analyzer Server - Stopped';
      UpdateStatus('Server stopped');
      LogActivity('Server stopped');
      FConnectedClients := 0;
      UpdateStats;
    end;
  except
    // Handle network errors gracefully
    on E: Exception do
    begin
      UpdateStatus('Error: ' + E.Message);
      LogActivity('Server error: ' + E.Message);
      ShowMessage('Server Error: ' + E.Message);
    end;
  end;
end;

{==============================================================================
  MANUALLY REQUEST SCREENSHOT FROM ALL CONNECTED CLIENTS
==============================================================================}
procedure TForm1.btnRequestScreenshotClick(Sender: TObject);
var
  Clients: TSocketList;
  i: Integer;
begin
  // Prevent sending if server is offline
  if not ncServerSource1.Active then
  begin
    UpdateStatus('Error: Server not running');
    Exit;
  end;

  // Broadcast screenshot request to all clients
  Clients := ncServerSource1.Lines.LockList;
  try
    if Clients.Count > 0 then
    begin
      UpdateStatus('Requesting screenshots from ' + IntToStr(Clients.Count) + ' client(s)');
      for i := 0 to Clients.Count - 1 do
        ncServerSource1.ExecCommand(Clients.Lines[i], CMD_REQUEST_SCREENSHOT, nil, False);
    end
    else
      UpdateStatus('No clients connected');
  finally
    ncServerSource1.Lines.UnlockList;
  end;
end;

{==============================================================================
  CLIENT CONNECTION EVENT HANDLERS
==============================================================================}
procedure TForm1.ncServerSource1Connected(Sender: TObject; aLine: TncLine);
begin
  // Track active clients
  Inc(FConnectedClients);
  btnRequestScreenshot.Enabled := True;
  UpdateStatus('Client connected from ' + aLine.PeerIP);
  LogActivity('New client connected: ' + aLine.PeerIP);
  UpdateStats;
end;

procedure TForm1.ncServerSource1Disconnected(Sender: TObject; aLine: TncLine);
begin
  // Remove from client count safely
  Dec(FConnectedClients);
  if FConnectedClients <= 0 then
  begin
    btnRequestScreenshot.Enabled := False;
    FConnectedClients := 0;
  end;

  UpdateStatus('Client disconnected: ' + aLine.PeerIP);
  LogActivity('Client disconnected: ' + aLine.PeerIP);
  UpdateStats;
end;

{==============================================================================
  COMMAND HANDLER - RECEIVES CLIENT MESSAGES
==============================================================================}
function TForm1.ncServerSource1HandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
var
  DataSize: Integer;
  StartTime: Cardinal;
begin
  Result := nil;
  DataSize := Length(aData);
  Inc(FTotalBytesReceived, DataSize);
  StartTime := GetTickCount;

  // Handle based on command ID
  case aCmd of
    CMD_SEND_SCREENSHOT:
    begin
      Inc(FScreenshotCount);
      try
        DisplayFullScreenshotFast(aData);
        UpdateStatus('Full screenshot processed in ' +
                     IntToStr(GetTickCount - StartTime) + 'ms (' +
                     FormatBytes(DataSize) + ')');
        LogActivity('Fast full screenshot #' + IntToStr(FScreenshotCount));
        UpdateStats;
      except
        on E: Exception do
        begin
          UpdateStatus('Error displaying screenshot: ' + E.Message);
          LogActivity('Display error: ' + E.Message);
        end;
      end;
    end;

    CMD_SEND_DELTA_SCREENSHOT:
    begin
      Inc(FScreenshotCount);
      if DataSize > SizeOf(TDeltaHeader) then
      begin
        try
          if not FLastScreenshot.Empty then
          begin
            ApplyDeltaChangesFast(FLastScreenshot, aData);
            RefreshDisplay;
            FLastDeltaSize := DataSize - SizeOf(TDeltaHeader);
            UpdateStatus('Delta processed in ' + IntToStr(GetTickCount - StartTime) + 'ms (' + FormatBytes(DataSize) + ')');
            LogActivity('Fast delta #' + IntToStr(FScreenshotCount));
          end
          else
          begin
            UpdateStatus('No base image for delta');
            LogActivity('Delta received but no base image available');
          end;
          UpdateStats;
        except
          on E: Exception do
          begin
            UpdateStatus('Error applying delta: ' + E.Message);
            LogActivity('Delta application error: ' + E.Message);
          end;
        end;
      end
      else
        UpdateStatus('No changes detected');
    end;

  else
    // Unknown/unsupported command
    UpdateStatus('Unknown command received: ' + IntToStr(aCmd));
    LogActivity('Unknown command ' + IntToStr(aCmd) + ' from ' + aLine.PeerIP);
  end;
end;

{==============================================================================
  DISPLAY FULL SCREENSHOT FROM STREAM (OPTIMIZED)
==============================================================================}
procedure TForm1.DisplayFullScreenshotFast(const ImageData: TBytes);
var
  Stream: TMemoryStream;
  TempBitmap: TBitmap;
begin
  if FUpdatePending then Exit;
  FUpdatePending := True;

  Stream := TMemoryStream.Create;
  TempBitmap := TBitmap.Create;
  try
    Stream.WriteBuffer(ImageData[0], Length(ImageData));
    Stream.Position := 0;

    TempBitmap.LoadFromStream(Stream);
    TempBitmap.PixelFormat := pf24bit;

    FLastScreenshot.Assign(TempBitmap);
    ImageReceived.Picture.Bitmap.Assign(TempBitmap);

    ImageReceived.Invalidate;
    Application.ProcessMessages;
  finally
    TempBitmap.Free;
    Stream.Free;
    FUpdatePending := False;
  end;
end;

{==============================================================================
  APPLY DELTA PIXEL CHANGES TO EXISTING BITMAP (USING SCANLINE)
==============================================================================}
procedure TForm1.ApplyDeltaChangesFast(Bitmap: TBitmap; const DeltaData: TBytes);
var
  Header: TDeltaHeader;
  Change: TPixelChange;
  Offset: Integer;
  i: Integer;
  ScanLine: PRGBArray;
  RGB: TRGBTriple;
  CurrentY: Integer;
  ChangesApplied: Integer;
begin
  if Length(DeltaData) < SizeOf(TDeltaHeader) then Exit;
  if FUpdatePending then Exit;
  FUpdatePending := True;

  try
    Move(DeltaData[0], Header, SizeOf(TDeltaHeader));

    if (Bitmap.Width <> Header.Width) or (Bitmap.Height <> Header.Height) then
    begin
      LogActivity('Delta size mismatch');
      Exit;
    end;

    Offset := SizeOf(TDeltaHeader);
    ChangesApplied := 0;
    CurrentY := -1;
    ScanLine := nil;

    // Loop through all pixel changes
    for i := 0 to Header.ChangeCount - 1 do
    begin
      if Offset + SizeOf(TPixelChange) <= Length(DeltaData) then
      begin
        Move(DeltaData[Offset], Change, SizeOf(TPixelChange));
        Inc(Offset, SizeOf(TPixelChange));

        if (Change.X < Bitmap.Width) and (Change.Y < Bitmap.Height) then
        begin
          if CurrentY <> Change.Y then
          begin
            CurrentY := Change.Y;
            ScanLine := PRGBArray(Bitmap.ScanLine[CurrentY]);
          end;

          RGB.rgbtRed := GetRValue(Change.Color);
          RGB.rgbtGreen := GetGValue(Change.Color);
          RGB.rgbtBlue := GetBValue(Change.Color);

          ScanLine[Change.X] := RGB;
          Inc(ChangesApplied);
        end;
      end;
    end;

    LogActivity('Applied ' + IntToStr(ChangesApplied) + ' pixel changes via scanline');
  finally
    FUpdatePending := False;
  end;
end;

{==============================================================================
  REFRESH IMAGE ON SCREEN FROM LAST BITMAP
==============================================================================}
procedure TForm1.RefreshDisplay;
begin
  if not FUpdatePending then
  begin
    ImageReceived.Picture.Bitmap.Assign(FLastScreenshot);
    ImageReceived.Invalidate;
    Application.ProcessMessages;
  end;
end;

{==============================================================================
  MISC UTILITY PROCEDURES
==============================================================================}
procedure TForm1.UpdateStatus(const AMessage: string);
begin
  lblStatus.Caption := FormatDateTime('hh:nn:ss', Now) + ' - ' + AMessage;
end;

procedure TForm1.LogActivity(const AMessage: string);
begin
  memoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + AMessage);
  if memoLog.Lines.Count > 500 then
    memoLog.Lines.Delete(0);

  // Auto-scroll occasionally
  if memoLog.Lines.Count mod 10 = 0 then
  begin
    memoLog.SelStart := Length(memoLog.Text);
    memoLog.SelLength := 0;
  end;
end;

procedure TForm1.UpdateStats;
begin
  lblClientInfo.Caption := 'Connected Clients: ' + IntToStr(FConnectedClients);
  lblStats.Caption := 'Total Received: ' + FormatBytes(FTotalBytesReceived) +
                     ' | Screenshots: ' + IntToStr(FScreenshotCount);
  lblPixelChanges.Caption := 'Last Delta Size: ' + FormatBytes(FLastDeltaSize);
end;

function TForm1.FormatBytes(Bytes: Int64): string;
begin
  if Bytes < 1024 then
    Result := IntToStr(Bytes) + ' B'
  else if Bytes < 1024 * 1024 then
    Result := Format('%.1f KB', [Bytes / 1024])
  else if Bytes < 1024 * 1024 * 1024 then
    Result := Format('%.1f MB', [Bytes / (1024 * 1024)])
  else
    Result := Format('%.1f GB', [Bytes / (1024 * 1024 * 1024)]);
end;

end.

