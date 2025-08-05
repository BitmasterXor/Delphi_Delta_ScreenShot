{==============================================================================
  SCREENSHOT DIFFERENCE ANALYZER - CLIENT (SENDER) - CLEAN & OPTIMIZED
  ==============================================================================

  Description: Client application that captures screenshots and sends them
               to remote server. No redundant UI setup bullshit.

  Author:      BitmasterXor
  Date:        08/05/2025
  Version:     2.1 - Clean & Fast
==============================================================================}

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, ncSources, System.Types, Vcl.Samples.Spin;

const
  CMD_REQUEST_SCREENSHOT = 1;
  CMD_SEND_SCREENSHOT = 2;
  CMD_SEND_DELTA_SCREENSHOT = 3;

type
  TPixelChange = packed record
    X, Y: Word;
    Color: TColor;
  end;

  TDeltaHeader = packed record
    Width, Height: Word;
    ChangeCount: Cardinal;
  end;

  TForm1 = class(TForm)
    ncClientSource1: TncClientSource;
    btnConnect: TButton;
    btnSendScreenshot: TButton;
    edtServerIP: TEdit;
    edtServerPort: TEdit;
    lblStatus: TLabel;
    lblStats: TLabel;
    chkAutoDelta: TCheckBox;
    memoLog: TMemo;
    lblConnectionInfo: TLabel;
    Timer1: TTimer;
    chkAutoCapture: TCheckBox;
    spnInterval: TSpinEdit;
    lblServerIP: TLabel;
    lblPort: TLabel;
    lblIntervalText: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendScreenshotClick(Sender: TObject);
    procedure ncClientSource1Connected(Sender: TObject; aLine: TncLine);
    procedure ncClientSource1Disconnected(Sender: TObject; aLine: TncLine);
    function ncClientSource1HandleCommand(Sender: TObject; aLine: TncLine;
      aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
      const aSenderComponent, aReceiverComponent: string): TBytes;
    procedure Timer1Timer(Sender: TObject);
    procedure chkAutoCaptureClick(Sender: TObject);

  private
    FLastScreenshot: TBitmap;
    FCurrentScreenshot: TBitmap;
    FConnected: Boolean;
    FTotalBytesSent: Int64;
    FScreenshotCount: Integer;

    procedure TakeScreenshot(ABitmap: TBitmap);
    procedure SendFullScreenshot;
    procedure SendDeltaScreenshot;
    function CreateDeltaDataFast(const AOld, ANew: TBitmap): TBytes;
    procedure UpdateStatus(const AMessage: string);
    procedure LogActivity(const AMessage: string);
    procedure UpdateStats;
    function FormatBytes(Bytes: Int64): string;
    procedure EnableControls(AEnabled: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{==============================================================================
  FORM INITIALIZATION - CLEAN
  ==============================================================================}
procedure TForm1.FormCreate(Sender: TObject);
begin
  FConnected := False;
  FTotalBytesSent := 0;
  FScreenshotCount := 0;

  FLastScreenshot := TBitmap.Create;
  FCurrentScreenshot := TBitmap.Create;

  // Pre-set pixel formats for performance
  FLastScreenshot.PixelFormat := pf24bit;
  FCurrentScreenshot.PixelFormat := pf24bit;

  UpdateStatus('Ready - Enter server details and click Connect');
  LogActivity('Screenshot Difference Analyzer Client v2.1 (Optimized) initialized');
end;

{==============================================================================
  FORM DESTRUCTION
  ==============================================================================}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  if ncClientSource1.Active then
    ncClientSource1.Active := False;

  FLastScreenshot.Free;
  FCurrentScreenshot.Free;
end;

{==============================================================================
  CONNECTION HANDLER
  ==============================================================================}
procedure TForm1.btnConnectClick(Sender: TObject);
begin
  try
    if not FConnected then
    begin
      ncClientSource1.Host := edtServerIP.Text;
      ncClientSource1.Port := StrToInt(edtServerPort.Text);
      ncClientSource1.Active := True;

      UpdateStatus('Connecting to ' + edtServerIP.Text + ':' + edtServerPort.Text);
      LogActivity('Attempting connection to ' + edtServerIP.Text + ':' + edtServerPort.Text);
    end
    else
    begin
      Timer1.Enabled := False;
      chkAutoCapture.Checked := False;
      ncClientSource1.Active := False;

      UpdateStatus('Disconnected');
      LogActivity('Disconnected from server');
    end;
  except
    on E: Exception do
    begin
      UpdateStatus('Connection error: ' + E.Message);
      LogActivity('Connection error: ' + E.Message);
      ShowMessage('Connection Error: ' + E.Message);
    end;
  end;
end;

{==============================================================================
  MANUAL SCREENSHOT SENDER
  ==============================================================================}
procedure TForm1.btnSendScreenshotClick(Sender: TObject);
begin
  if not FConnected then
  begin
    UpdateStatus('Not connected to server');
    Exit;
  end;

  try
    if chkAutoDelta.Checked and not FLastScreenshot.Empty then
      SendDeltaScreenshot
    else
      SendFullScreenshot;
  except
    on E: Exception do
    begin
      UpdateStatus('Error sending screenshot: ' + E.Message);
      LogActivity('Send error: ' + E.Message);
    end;
  end;
end;

{==============================================================================
  CONNECTION ESTABLISHED HANDLER
  ==============================================================================}
procedure TForm1.ncClientSource1Connected(Sender: TObject; aLine: TncLine);
begin
  FConnected := True;
  btnConnect.Caption := 'Disconnect';
  btnSendScreenshot.Enabled := True;

  lblConnectionInfo.Caption := 'Connected to ' + aLine.PeerIP + ':' + IntToStr(ncClientSource1.Port);
  lblConnectionInfo.Font.Color := clGreen;

  UpdateStatus('Connected successfully');
  LogActivity('Connected to server: ' + aLine.PeerIP + ':' + IntToStr(ncClientSource1.Port));

  EnableControls(False);
end;

{==============================================================================
  CONNECTION LOST HANDLER
  ==============================================================================}
procedure TForm1.ncClientSource1Disconnected(Sender: TObject; aLine: TncLine);
begin
  FConnected := False;
  btnConnect.Caption := 'Connect';
  btnSendScreenshot.Enabled := False;
  Timer1.Enabled := False;
  chkAutoCapture.Checked := False;

  lblConnectionInfo.Caption := 'Not connected';
  lblConnectionInfo.Font.Color := clRed;

  UpdateStatus('Disconnected from server');
  LogActivity('Disconnected from server');

  EnableControls(True);
end;

{==============================================================================
  NETWORK COMMAND HANDLER
  ==============================================================================}
function TForm1.ncClientSource1HandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
begin
  Result := nil;

  case aCmd of
    CMD_REQUEST_SCREENSHOT:
    begin
      UpdateStatus('Screenshot requested by server');
      LogActivity('Server requested screenshot');

      try
        if chkAutoDelta.Checked and not FLastScreenshot.Empty then
          SendDeltaScreenshot
        else
          SendFullScreenshot;
      except
        on E: Exception do
        begin
          UpdateStatus('Error processing server request: ' + E.Message);
          LogActivity('Server request error: ' + E.Message);
        end;
      end;
    end;

  else
    UpdateStatus('Unknown command received: ' + IntToStr(aCmd));
    LogActivity('Unknown command received: ' + IntToStr(aCmd));
  end;
end;

{==============================================================================
  AUTO-CAPTURE TIMER
  ==============================================================================}
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if FConnected then
  begin
    try
      if chkAutoDelta.Checked and not FLastScreenshot.Empty then
        SendDeltaScreenshot
      else
        SendFullScreenshot;
    except
      on E: Exception do
      begin
        UpdateStatus('Auto-capture error: ' + E.Message);
        LogActivity('Auto-capture error: ' + E.Message);
        Timer1.Enabled := False;
        chkAutoCapture.Checked := False;
      end;
    end;
  end
  else
  begin
    Timer1.Enabled := False;
    chkAutoCapture.Checked := False;
  end;
end;

{==============================================================================
  AUTO-CAPTURE TOGGLE
  ==============================================================================}
procedure TForm1.chkAutoCaptureClick(Sender: TObject);
begin
  if chkAutoCapture.Checked then
  begin
    if FConnected then
    begin
      Timer1.Interval := spnInterval.Value;  // Direct milliseconds
      Timer1.Enabled := True;
      UpdateStatus('Auto-capture enabled (' + IntToStr(spnInterval.Value) + 'ms interval)');
      LogActivity('Auto-capture started with ' + IntToStr(spnInterval.Value) + 'ms interval');
    end
    else
    begin
      chkAutoCapture.Checked := False;
      UpdateStatus('Cannot enable auto-capture: not connected');
    end;
  end
  else
  begin
    Timer1.Enabled := False;
    UpdateStatus('Auto-capture disabled');
    LogActivity('Auto-capture stopped');
  end;
end;

{==============================================================================
  DESKTOP SCREENSHOT CAPTURE
  ==============================================================================}
procedure TForm1.TakeScreenshot(ABitmap: TBitmap);
var
  DC: HDC;
  Rect: TRect;
begin
  GetWindowRect(GetDesktopWindow, Rect);

  ABitmap.SetSize(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
  ABitmap.PixelFormat := pf24bit;

  DC := GetDC(0);
  try
    BitBlt(ABitmap.Canvas.Handle, 0, 0, ABitmap.Width, ABitmap.Height,
      DC, Rect.Left, Rect.Top, SRCCOPY);
  finally
    ReleaseDC(0, DC);
  end;
end;

{==============================================================================
  FULL SCREENSHOT TRANSMISSION
  ==============================================================================}
procedure TForm1.SendFullScreenshot;
var
  Stream: TMemoryStream;
  Data: TBytes;
  StartTime: Cardinal;
begin
  StartTime := GetTickCount;
  UpdateStatus('Capturing full screenshot...');

  TakeScreenshot(FCurrentScreenshot);

  Stream := TMemoryStream.Create;
  try
    FCurrentScreenshot.SaveToStream(Stream);

    SetLength(Data, Stream.Size);
    Move(Stream.Memory^, Data[0], Stream.Size);

    UpdateStatus('Sending full screenshot...');
    ncClientSource1.ExecCommand(CMD_SEND_SCREENSHOT, Data, False);

    Inc(FScreenshotCount);
    Inc(FTotalBytesSent, Length(Data));

    FLastScreenshot.Assign(FCurrentScreenshot);

    UpdateStatus('Full screenshot sent in ' + IntToStr(GetTickCount - StartTime) + 'ms (' + FormatBytes(Length(Data)) + ')');
    LogActivity('Full screenshot #' + IntToStr(FScreenshotCount) + ' sent: ' + FormatBytes(Length(Data)) + ' in ' + IntToStr(GetTickCount - StartTime) + 'ms');
    UpdateStats;

  finally
    Stream.Free;
  end;
end;

{==============================================================================
  DELTA SCREENSHOT TRANSMISSION
  ==============================================================================}
procedure TForm1.SendDeltaScreenshot;
var
  DeltaData: TBytes;
  StartTime: Cardinal;
begin
  StartTime := GetTickCount;
  UpdateStatus('Capturing delta screenshot...');

  TakeScreenshot(FCurrentScreenshot);
  DeltaData := CreateDeltaDataFast(FLastScreenshot, FCurrentScreenshot);

  if Length(DeltaData) > SizeOf(TDeltaHeader) then
  begin
    UpdateStatus('Sending delta changes...');
    ncClientSource1.ExecCommand(CMD_SEND_DELTA_SCREENSHOT, DeltaData, False);

    Inc(FScreenshotCount);
    Inc(FTotalBytesSent, Length(DeltaData));

    UpdateStatus('Delta changes sent in ' + IntToStr(GetTickCount - StartTime) + 'ms (' + FormatBytes(Length(DeltaData)) + ')');
    LogActivity('Delta screenshot #' + IntToStr(FScreenshotCount) + ' sent: ' + FormatBytes(Length(DeltaData)) + ' in ' + IntToStr(GetTickCount - StartTime) + 'ms');
  end
  else
  begin
    UpdateStatus('No changes detected');
    LogActivity('No screen changes detected');
  end;

  FLastScreenshot.Assign(FCurrentScreenshot);
  UpdateStats;
end;

{==============================================================================
  OPTIMIZED DELTA DATA CREATION
  ==============================================================================}
function TForm1.CreateDeltaDataFast(const AOld, ANew: TBitmap): TBytes;
type
  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..32767] of TRGBTriple;

var
  Header: TDeltaHeader;
  Changes: array of TPixelChange;
  ChangeCount: Integer;
  y, x: Integer;
  OldRow, NewRow: PRGBArray;
  Offset: Integer;
  OldPixel, NewPixel: TRGBTriple;
begin
  Header.Width := ANew.Width;
  Header.Height := ANew.Height;
  Header.ChangeCount := 0;

  SetLength(Changes, ANew.Width * ANew.Height div 10);  // im assumming a 10% change rate here!
  ChangeCount := 0;

  AOld.PixelFormat := pf24bit;
  ANew.PixelFormat := pf24bit;

  // Fast scanline-based comparison
  for y := 0 to ANew.Height - 1 do
  begin
    OldRow := PRGBArray(AOld.ScanLine[y]);
    NewRow := PRGBArray(ANew.ScanLine[y]);

    for x := 0 to ANew.Width - 1 do
    begin
      OldPixel := OldRow[x];
      NewPixel := NewRow[x];

      if (OldPixel.rgbtRed <> NewPixel.rgbtRed) or
         (OldPixel.rgbtGreen <> NewPixel.rgbtGreen) or
         (OldPixel.rgbtBlue <> NewPixel.rgbtBlue) then
      begin
        if ChangeCount >= Length(Changes) then
          SetLength(Changes, Length(Changes) * 2);

        Changes[ChangeCount].X := x;
        Changes[ChangeCount].Y := y;
        Changes[ChangeCount].Color := RGB(NewPixel.rgbtRed, NewPixel.rgbtGreen, NewPixel.rgbtBlue);
        Inc(ChangeCount);
      end;
    end;
  end;

  Header.ChangeCount := ChangeCount;

  SetLength(Result, SizeOf(TDeltaHeader) + (ChangeCount * SizeOf(TPixelChange)));

  Move(Header, Result[0], SizeOf(TDeltaHeader));

  if ChangeCount > 0 then
  begin
    Offset := SizeOf(TDeltaHeader);
    Move(Changes[0], Result[Offset], ChangeCount * SizeOf(TPixelChange));
  end;
end;

{==============================================================================
  UTILITY METHODS
  ==============================================================================}
procedure TForm1.UpdateStatus(const AMessage: string);
begin
  lblStatus.Caption := FormatDateTime('hh:nn:ss', Now) + ' - ' + AMessage;
  Application.ProcessMessages;
end;

procedure TForm1.LogActivity(const AMessage: string);
begin
  memoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + AMessage);

  if memoLog.Lines.Count > 1000 then
    memoLog.Lines.Delete(0);
  memoLog.SelStart := Length(memoLog.Text);
  memoLog.SelLength := 0;
end;

procedure TForm1.UpdateStats;
begin
  lblStats.Caption := 'Sent: ' + FormatBytes(FTotalBytesSent) +
                     ' | Screenshots: ' + IntToStr(FScreenshotCount);
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

procedure TForm1.EnableControls(AEnabled: Boolean);
begin
  edtServerIP.Enabled := AEnabled;
  edtServerPort.Enabled := AEnabled;
end;

end.
