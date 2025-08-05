{==============================================================================
  SCREENSHOT DIFFERENCE ANALYZER
  ==============================================================================

  Description: Advanced screenshot comparison tool that captures desktop images
               and highlights only the changed pixels between consecutive screenshots.
               Optimized for high performance using scanline pixel access.

  Author:      [BitmasterXor]
  Date:        [08/05/2025]
  Version:     1.0

  Features:    - Real-time desktop screenshot capture
               - Pixel-perfect difference detection
               - Optimized scanline image processing
               - Memory-efficient bitmap handling
               - Accurate change size calculation
               - Progress indication during processing

  Performance: Uses direct scanline access instead of Canvas.Pixels for
               hundreds of times faster pixel processing on large images.
==============================================================================}

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  {============================================================================
    MAIN FORM CLASS
    ============================================================================
    TForm1: Primary application form containing screenshot comparison functionality
  ============================================================================}
  TForm1 = class(TForm)
    // UI Controls
    Button1: TButton;    // Screenshot capture trigger
    Label1: TLabel;      // Status and size information display
    Image1: TImage;      // Visual display of comparison results

    // Event Handlers
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    {==========================================================================
      PRIVATE MEMBERS
      ==========================================================================}
    FLastBitmap: TBitmap;     // Previous screenshot for comparison
    FCurrentBitmap: TBitmap;  // Current screenshot being processed
    FChangesSize: Int64;      // Size in bytes of changed pixels only

    {==========================================================================
      PRIVATE METHODS
      ==========================================================================}
    procedure TakeScreenshot(ABitmap: TBitmap);
    function CompareImages(const AOld, ANew: TBitmap): TBitmap;
    procedure UpdateDisplay(AImage: TBitmap);
    function GetFileSizeString(AStream: TMemoryStream): string;
    function GetBitmapFileSize(ABitmap: TBitmap): string;

  public
    {==========================================================================
      PUBLIC METHODS
      ==========================================================================}
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{==============================================================================
  DESTRUCTOR
  ==============================================================================
  Purpose: Clean up allocated bitmap resources to prevent memory leaks
  Called:  Automatically when form is destroyed
==============================================================================}
destructor TForm1.Destroy;
begin
  // Free bitmap resources
  FLastBitmap.Free;
  FCurrentBitmap.Free;
  inherited;
end;

{==============================================================================
  FORM INITIALIZATION
  ==============================================================================
  Purpose: Initialize form components, create bitmaps, and set up UI layout
  Called:  Automatically when form is first created
==============================================================================}
procedure TForm1.FormCreate(Sender: TObject);
begin
  //==========================================================================
  // BITMAP INITIALIZATION
  //==========================================================================
  // Create bitmap objects for screenshot storage
  FLastBitmap := TBitmap.Create;
  FCurrentBitmap := TBitmap.Create;

  // Set pixel format to 24-bit RGB for optimal scanline performance
  // This ensures consistent memory layout and fastest pixel access
  FLastBitmap.PixelFormat := pf24bit;
  FCurrentBitmap.PixelFormat := pf24bit;

  //==========================================================================
  // FORM LAYOUT CONFIGURATION
  //==========================================================================
  // Set initial form dimensions
  Width := 800;
  Height := 600;
end;

{==============================================================================
  MAIN SCREENSHOT PROCESSING HANDLER
  ==============================================================================
  Purpose: Orchestrates the screenshot capture and comparison process
  Called:  When user clicks the "Take Screenshot" button

  Process Flow:
  1. Disable UI during processing
  2. Capture current desktop screenshot
  3. If first screenshot: display and store for future comparison
  4. If subsequent: compare with previous, show differences, update reference
  5. Display appropriate size information
  6. Re-enable UI
==============================================================================}
procedure TForm1.Button1Click(Sender: TObject);
var
  ChangedBitmap: TBitmap;  // Result bitmap showing only changed areas
  ChangesSize: string;     // Formatted string of changes size
begin
  try
    //========================================================================
    // PROCESSING SETUP
    //========================================================================
    Button1.Enabled := False;  // Prevent multiple simultaneous operations
    Label1.Caption := 'Taking screenshot...';
    Application.ProcessMessages;  // Update UI immediately

    //========================================================================
    // SCREENSHOT CAPTURE
    //========================================================================
    TakeScreenshot(FCurrentBitmap);

    //========================================================================
    // FIRST SCREENSHOT HANDLING
    //========================================================================
    if FLastBitmap.Empty then
    begin
      // First run: establish baseline image
      UpdateDisplay(FCurrentBitmap);
      FLastBitmap.Assign(FCurrentBitmap);  // Store for next comparison
      Label1.Caption := 'Size: ' + GetBitmapFileSize(FCurrentBitmap);
    end
    else
    begin
      //======================================================================
      // SCREENSHOT COMPARISON PROCESSING
      //======================================================================
      Label1.Caption := 'Comparing images...';
      Application.ProcessMessages;  // Show progress to user

      // Perform pixel-by-pixel comparison
      ChangedBitmap := CompareImages(FLastBitmap, FCurrentBitmap);
      try
        // Display the difference visualization
        UpdateDisplay(ChangedBitmap);

        // Update reference image for next comparison
        FLastBitmap.Assign(FCurrentBitmap);

        //==================================================================
        // RESULTS DISPLAY
        //==================================================================
        if FChangesSize > 0 then
        begin
          // Format and display size of actual changes
          if FChangesSize < 1024 then
            ChangesSize := Format('%d bytes', [FChangesSize])
          else if FChangesSize < 1024 * 1024 then
            ChangesSize := Format('%.2f KB', [FChangesSize / 1024])
          else
            ChangesSize := Format('%.2f MB', [FChangesSize / (1024 * 1024)]);

          Label1.Caption := 'Changes size: ' + ChangesSize;
        end
        else
        begin
          Label1.Caption := 'No changes detected';
        end;
      finally
        ChangedBitmap.Free;  // Always clean up temporary bitmap
      end;
    end;
  except
    //========================================================================
    // ERROR HANDLING
    //========================================================================
    on E: Exception do
    begin
      ShowMessage('Error: ' + E.Message);
    end;
  end;

  //==========================================================================
  // CLEANUP
  //==========================================================================
  Button1.Enabled := True;  // Always re-enable UI, even after errors
end;

{==============================================================================
  DESKTOP SCREENSHOT CAPTURE
  ==============================================================================
  Purpose: Captures the entire desktop screen into a bitmap
  Params:  ABitmap - Target bitmap to store the screenshot

  Technical Details:
  - Uses Windows GDI BitBlt for fast, direct screen capture
  - Automatically detects full screen dimensions
  - Sets optimal pixel format for subsequent processing
==============================================================================}
procedure TForm1.TakeScreenshot(ABitmap: TBitmap);
var
  DC: HDC;      // Device context for screen access
  Rect: TRect;  // Screen dimensions
begin
  //==========================================================================
  // SCREEN DIMENSION DETECTION
  //==========================================================================
  GetWindowRect(GetDesktopWindow, Rect);

  //==========================================================================
  // BITMAP PREPARATION
  //==========================================================================
  // Size bitmap to match screen dimensions exactly
  ABitmap.SetSize(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
  ABitmap.PixelFormat := pf24bit;  // Ensure consistent format

  //==========================================================================
  // SCREEN CAPTURE OPERATION
  //==========================================================================
  DC := GetDC(0);  // Get screen device context
  try
    // Perform fast bitmap copy from screen to our bitmap
    BitBlt(ABitmap.Canvas.Handle, 0, 0, ABitmap.Width, ABitmap.Height,
      DC, Rect.Left, Rect.Top, SRCCOPY);
  finally
    ReleaseDC(0, DC);  // Always release system resources
  end;
end;

{==============================================================================
  HIGH-PERFORMANCE IMAGE COMPARISON
  ==============================================================================
  Purpose: Compares two bitmaps pixel-by-pixel and creates a difference image
  Params:  AOld - Previous screenshot bitmap
           ANew - Current screenshot bitmap
  Returns: Bitmap showing only changed pixels (unchanged areas are white)

  Performance Optimization:
  - Uses direct scanline access instead of Canvas.Pixels (100x+ faster)
  - Processes entire rows at once using pointer arithmetic
  - Only calculates size of actual changed pixels, not full image

  Algorithm:
  1. Create result bitmap with white background
  2. For each pixel position, compare RGB values
  3. If different: copy new pixel to result, increment change counter
  4. If same: leave as white background
  5. Calculate total size as: changed_pixels × 3_bytes_per_pixel
==============================================================================}
function TForm1.CompareImages(const AOld, ANew: TBitmap): TBitmap;
type
  // Packed RGB structure for direct memory access
  TRGBTriple = packed record
    rgbtBlue: Byte;   // Blue component (0-255)
    rgbtGreen: Byte;  // Green component (0-255)
    rgbtRed: Byte;    // Red component (0-255)
  end;
  PRGBTriple = ^TRGBTriple;           // Pointer to single RGB pixel
  PRGBArray = ^TRGBArray;             // Pointer to array of RGB pixels
  TRGBArray = array[0..32767] of TRGBTriple;  // Maximum array for scanline

var
  y: Integer;                                    // Row counter
  OldRow, NewRow, ResultRow: PRGBArray;         // Scanline pointers
  ChangedPixels: Integer;                       // Count of different pixels
  x: Integer;                                   // Column counter
begin
  Result := TBitmap.Create;
  try
    //========================================================================
    // RESULT BITMAP INITIALIZATION
    //========================================================================
    Result.SetSize(ANew.Width, ANew.Height);  // Match input dimensions
    Result.PixelFormat := pf24bit;             // Ensure scanline compatibility

    // Fill entire bitmap with white background (unchanged pixel indicator)
    Result.Canvas.Brush.Color := clWhite;
    Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));

    ChangedPixels := 0;  // Initialize change counter

    //========================================================================
    // PIXEL-BY-PIXEL COMPARISON LOOP
    //========================================================================
    for y := 0 to ANew.Height - 1 do
    begin
      // Get direct memory pointers to each row for maximum speed
      OldRow := PRGBArray(AOld.ScanLine[y]);
      NewRow := PRGBArray(ANew.ScanLine[y]);
      ResultRow := PRGBArray(Result.ScanLine[y]);

      // Process entire row pixel by pixel
      for x := 0 to ANew.Width - 1 do
      begin
        // Compare all three RGB components simultaneously
        if (OldRow[x].rgbtRed <> NewRow[x].rgbtRed) or
           (OldRow[x].rgbtGreen <> NewRow[x].rgbtGreen) or
           (OldRow[x].rgbtBlue <> NewRow[x].rgbtBlue) then
        begin
          // PIXEL CHANGED: Copy new pixel data to result
          ResultRow[x] := NewRow[x];
          Inc(ChangedPixels);
        end;
        // PIXEL UNCHANGED: Leave as white (already filled above)
      end;

      //======================================================================
      // PROGRESS REPORTING
      //======================================================================
      // Update progress display every 100 rows to avoid UI slowdown
      if (y mod 100) = 0 then
      begin
        Label1.Caption := Format('Comparing... %d%%', [(y * 100) div ANew.Height]);
        Application.ProcessMessages;  // Allow UI updates during processing
      end;
    end;

    //========================================================================
    // CHANGE SIZE CALCULATION
    //========================================================================
    // Calculate raw data size: 3 bytes per RGB pixel × number of changed pixels
    // This gives the actual size of change data, not the full image size
    FChangesSize := ChangedPixels * 3;

  except
    //========================================================================
    // ERROR CLEANUP
    //========================================================================
    Result.Free;  // Prevent memory leak on error
    raise;        // Re-raise exception for caller handling
  end;
end;

{==============================================================================
  DISPLAY UPDATE
  ==============================================================================
  Purpose: Updates the main image display with the provided bitmap
  Params:  AImage - Bitmap to display in the Image1 control
==============================================================================}
procedure TForm1.UpdateDisplay(AImage: TBitmap);
begin
  Image1.Picture.Bitmap := AImage;  // Auto-stretches due to Stretch := True
end;

{==============================================================================
  MEMORY STREAM SIZE FORMATTER
  ==============================================================================
  Purpose: Converts raw byte count to human-readable size string
  Params:  AStream - Memory stream containing data to measure
  Returns: Formatted string (e.g., "1.25 MB", "512.34 KB", "1024 bytes")
==============================================================================}
function TForm1.GetFileSizeString(AStream: TMemoryStream): string;
var
  Size: Int64;
begin
  Size := AStream.Size;

  // Format size with appropriate units
  if Size < 1024 then
    Result := Format('%d bytes', [Size])
  else if Size < 1024 * 1024 then
    Result := Format('%.2f KB', [Size / 1024])
  else
    Result := Format('%.2f MB', [Size / (1024 * 1024)]);
end;

{==============================================================================
  BITMAP FILE SIZE CALCULATOR
  ==============================================================================
  Purpose: Determines the file size a bitmap would have when saved
  Params:  ABitmap - Bitmap to measure
  Returns: Formatted size string

  Method: Temporarily saves bitmap to memory stream to get accurate file size
          including headers and compression
==============================================================================}
function TForm1.GetBitmapFileSize(ABitmap: TBitmap): string;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    ABitmap.SaveToStream(Stream);           // Serialize bitmap to get true size
    Result := GetFileSizeString(Stream);    // Format the resulting size
  finally
    Stream.Free;  // Clean up temporary stream
  end;
end;

{==============================================================================
  FORM RESIZE HANDLER
  ==============================================================================
  Purpose: Automatically adjusts image display area when form is resized
  Called:  Automatically when user resizes the form window

  Layout: Maintains 10-pixel margins on left/right and 70-pixel margin on bottom
          for button and label areas
==============================================================================}
procedure TForm1.FormResize(Sender: TObject);
begin
  // Dynamically resize image display to fit available form space
  if Assigned(Image1) then
  begin
    Image1.Width := Width - 20;   // 10px margin on each side
    Image1.Height := Height - 70; // Space for button and label at top
  end;
end;

end.
