unit RDPScreenCapture;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  Vcl.Graphics, Vcl.Forms, Math, ZLibEx;

type
  TScreenCaptureCallback = procedure(const Data: TBytes; IsFullScreenshot: Boolean) of object;

  TScreenCapture = class(TThread)
  private
    FCallback: TScreenCaptureCallback;
    FCurrentBitmap: TBitmap;
    FPreviousBitmap: TBitmap;
    FIsFirstCapture: Boolean;
    FScreenWidth: Integer;
    FScreenHeight: Integer;
    FCaptureRate: Integer;
    FTerminating: Boolean;
    FCompressionLevel: TZCompressionLevel;

    // High-quality capture members
    FScreenDC: HDC;
    FMemDC: HDC;
    FBitmapInfo: TBitmapInfo;
    FDIBBits: Pointer;
    FDIBSection: HBITMAP;
    FOldBitmap: HBITMAP;

    // For synchronized callback
    FSendData: TBytes;
    FIsFullshotFlag: Boolean;

    procedure InitializeCapture;
    procedure CleanupCapture;
    procedure CaptureScreenHighQuality;
    function CompressBitmapHighQuality(Bitmap: TBitmap): TBytes;
    function CreateOptimizedDelta(Current, Previous: TBitmap): TBytes;
    procedure SendData(const Data: TBytes; IsFullshot: Boolean);
    procedure DoSendCallback;
    function BitmapToRawBytes(Bitmap: TBitmap): TBytes;

  protected
    procedure Execute; override;

  public
    constructor Create(ACallback: TScreenCaptureCallback);
    destructor Destroy; override;
    procedure ResetCapture;
    procedure Terminate;
    procedure SetCompressionLevel(Level: Integer); // 1-9, 9=best quality
  end;

implementation

constructor TScreenCapture.Create(ACallback: TScreenCaptureCallback);
begin
  inherited Create(False);
  FCallback := ACallback;
  FIsFirstCapture := True;
  FCaptureRate := 16; // 60 FPS for smooth updates
  FTerminating := False;
  FCompressionLevel := zcDefault; // Default compression

  // Get screen dimensions
  FScreenWidth := GetSystemMetrics(SM_CXSCREEN);
  FScreenHeight := GetSystemMetrics(SM_CYSCREEN);

  // Create high-quality bitmaps
  FCurrentBitmap := TBitmap.Create;
  FPreviousBitmap := TBitmap.Create;

  // Set to 32-bit for maximum quality
  FCurrentBitmap.PixelFormat := pf32bit;
  FPreviousBitmap.PixelFormat := pf32bit;

  FCurrentBitmap.Width := FScreenWidth;
  FCurrentBitmap.Height := FScreenHeight;
  FPreviousBitmap.Width := FScreenWidth;
  FPreviousBitmap.Height := FScreenHeight;

  InitializeCapture;
  Priority := tpTimeCritical;
end;

destructor TScreenCapture.Destroy;
begin
  FTerminating := True;
  Terminate;
  WaitFor;

  CleanupCapture;
  FCurrentBitmap.Free;
  FPreviousBitmap.Free;
  inherited Destroy;
end;

procedure TScreenCapture.InitializeCapture;
begin
  FScreenDC := GetDC(0);
  FMemDC := CreateCompatibleDC(FScreenDC);

  // Setup DIB section for direct pixel access
  ZeroMemory(@FBitmapInfo, SizeOf(TBitmapInfo));
  with FBitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := FScreenWidth;
    biHeight := -FScreenHeight; // Negative for top-down DIB
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biSizeImage := FScreenWidth * FScreenHeight * 4;
  end;

  FDIBSection := CreateDIBSection(FMemDC, FBitmapInfo, DIB_RGB_COLORS, FDIBBits, 0, 0);
  FOldBitmap := SelectObject(FMemDC, FDIBSection);
end;

procedure TScreenCapture.CleanupCapture;
begin
  if FMemDC <> 0 then
  begin
    if FOldBitmap <> 0 then
      SelectObject(FMemDC, FOldBitmap);
    DeleteDC(FMemDC);
    FMemDC := 0;
  end;

  if FDIBSection <> 0 then
  begin
    DeleteObject(FDIBSection);
    FDIBSection := 0;
  end;

  if FScreenDC <> 0 then
  begin
    ReleaseDC(0, FScreenDC);
    FScreenDC := 0;
  end;
end;

procedure TScreenCapture.Terminate;
begin
  FTerminating := True;
  inherited Terminate;
end;

procedure TScreenCapture.ResetCapture;
begin
  FIsFirstCapture := True;
end;

procedure TScreenCapture.SetCompressionLevel(Level: Integer);
begin
  // Convert integer level to TZCompressionLevel enum
  case Level of
    1: FCompressionLevel := zcFastest;
    2, 3: FCompressionLevel := zcFastest;
    4, 5: FCompressionLevel := zcDefault;
    6, 7: FCompressionLevel := zcDefault;
    8, 9: FCompressionLevel := zcMax;
    else FCompressionLevel := zcDefault;
  end;
end;

procedure TScreenCapture.Execute;
begin
  while not FTerminating do
  begin
    try
      CaptureScreenHighQuality;
      Sleep(FCaptureRate);
    except
      on E: Exception do
      begin
        Sleep(100);
      end;
    end;
  end;
end;

procedure TScreenCapture.CaptureScreenHighQuality;
var
  Data: TBytes;
  TempBitmap: TBitmap;
begin
  if FTerminating then Exit;

  try
    // Capture screen to DIB section for maximum quality
    if not BitBlt(FMemDC, 0, 0, FScreenWidth, FScreenHeight, FScreenDC, 0, 0, SRCCOPY) then
      Exit;

    // Create temporary bitmap from DIB data
    TempBitmap := TBitmap.Create;
    try
      TempBitmap.PixelFormat := pf32bit;
      TempBitmap.Width := FScreenWidth;
      TempBitmap.Height := FScreenHeight;

      // Copy DIB data to bitmap
      SetDIBits(FScreenDC, TempBitmap.Handle, 0, FScreenHeight, FDIBBits, FBitmapInfo, DIB_RGB_COLORS);

      if FIsFirstCapture then
      begin
        // Send full screenshot with high quality compression
        Data := CompressBitmapHighQuality(TempBitmap);
        if Length(Data) > 0 then
        begin
          SendData(Data, True);
          FCurrentBitmap.Canvas.Draw(0, 0, TempBitmap);
          FPreviousBitmap.Canvas.Draw(0, 0, TempBitmap);
          FIsFirstCapture := False;
        end;
      end
      else
      begin
        // Create optimized delta
        FCurrentBitmap.Canvas.Draw(0, 0, TempBitmap);
        Data := CreateOptimizedDelta(FCurrentBitmap, FPreviousBitmap);
        if Length(Data) > 0 then
        begin
          SendData(Data, False);
          FPreviousBitmap.Canvas.Draw(0, 0, FCurrentBitmap);
        end;
      end;

    finally
      TempBitmap.Free;
    end;

  except
    // Handle errors silently
  end;
end;

function TScreenCapture.BitmapToRawBytes(Bitmap: TBitmap): TBytes;
var
  BmpInfo: TBitmapInfo;
  DataSize: Integer;
  ScanLines: Pointer;
begin
  Result := nil;
  if Bitmap.Empty then Exit;

  ZeroMemory(@BmpInfo, SizeOf(TBitmapInfo));
  with BmpInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := Bitmap.Width;
    biHeight := -Bitmap.Height; // Negative for top-down
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
  end;

  DataSize := Bitmap.Width * Bitmap.Height * 4;
  GetMem(ScanLines, DataSize);
  try
    if GetDIBits(FScreenDC, Bitmap.Handle, 0, Bitmap.Height, ScanLines, BmpInfo, DIB_RGB_COLORS) > 0 then
    begin
      SetLength(Result, DataSize);
      Move(ScanLines^, Result[0], DataSize);
    end;
  finally
    FreeMem(ScanLines);
  end;
end;

function TScreenCapture.CompressBitmapHighQuality(Bitmap: TBitmap): TBytes;
var
  RawData: TBytes;
  InBuffer, OutBuffer: Pointer;
  InSize, OutSize: Integer;
begin
  Result := nil;
  if FTerminating or Bitmap.Empty then Exit;

  // Get raw bitmap data
  RawData := BitmapToRawBytes(Bitmap);
  if Length(RawData) = 0 then Exit;

  try
    InSize := Length(RawData);
    InBuffer := @RawData[0];

    // Use ZCompress with proper compression level
    ZCompress(InBuffer, InSize, OutBuffer, OutSize, FCompressionLevel);

    if (OutBuffer <> nil) and (OutSize > 0) then
    begin
      SetLength(Result, OutSize);
      Move(OutBuffer^, Result[0], OutSize);
      FreeMem(OutBuffer);
    end;
  except
    // Handle compression errors silently
  end;
end;

function TScreenCapture.CreateOptimizedDelta(Current, Previous: TBitmap): TBytes;
var
  CurrentData, PreviousData: TBytes;
  DeltaData: TBytes;
  I, PixelCount: Integer;
  ChangedPixels: Integer;
  InBuffer, OutBuffer: Pointer;
  InSize, OutSize: Integer;
  P1, P2, P3: PCardinal;
begin
  Result := nil;
  if FTerminating or Current.Empty or Previous.Empty then Exit;

  CurrentData := BitmapToRawBytes(Current);
  PreviousData := BitmapToRawBytes(Previous);

  if (Length(CurrentData) = 0) or (Length(PreviousData) = 0) or
     (Length(CurrentData) <> Length(PreviousData)) then Exit;

  PixelCount := Length(CurrentData) div 4;
  SetLength(DeltaData, Length(CurrentData));

  P1 := PCardinal(@CurrentData[0]);
  P2 := PCardinal(@PreviousData[0]);
  P3 := PCardinal(@DeltaData[0]);

  ChangedPixels := 0;

  // Compare pixels and create delta
  for I := 0 to PixelCount - 1 do
  begin
    if P1^ <> P2^ then
    begin
      P3^ := P1^; // Store changed pixel
      Inc(ChangedPixels);
    end
    else
    begin
      P3^ := $00000000; // Marker for unchanged pixel
    end;
    Inc(P1);
    Inc(P2);
    Inc(P3);
  end;

  // Only send if enough pixels changed (reduces unnecessary updates)
  if ChangedPixels < (PixelCount div 1000) then Exit; // Less than 0.1% changed for crystal clear

  try
    InSize := Length(DeltaData);
    InBuffer := @DeltaData[0];

    // Compress delta with high quality
    ZCompress(InBuffer, InSize, OutBuffer, OutSize, FCompressionLevel);

    if (OutBuffer <> nil) and (OutSize > 0) then
    begin
      SetLength(Result, OutSize);
      Move(OutBuffer^, Result[0], OutSize);
      FreeMem(OutBuffer);
    end;
  except
    // Handle compression errors silently
  end;
end;

procedure TScreenCapture.SendData(const Data: TBytes; IsFullshot: Boolean);
begin
  if FTerminating or not Assigned(FCallback) or (Length(Data) = 0) then Exit;

  try
    FSendData := Data;
    FIsFullshotFlag := IsFullshot;
    Synchronize(DoSendCallback);
  except
    // Handle callback errors silently
  end;
end;

procedure TScreenCapture.DoSendCallback;
begin
  if Assigned(FCallback) then
    FCallback(FSendData, FIsFullshotFlag);
end;

end.
