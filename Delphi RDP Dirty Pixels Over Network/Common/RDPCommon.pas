unit RDPCommon;

interface

uses
  System.SysUtils;

const
  RDP_PORT = 8888;

  // Command constants
  CMD_SCREENSHOT_FULL = 1;
  CMD_SCREENSHOT_DELTA = 2;
  CMD_MOUSE_EVENT = 3;
  CMD_KEYBOARD_EVENT = 4;
  CMD_MOUSE_TOGGLE = 5;
  CMD_KEYBOARD_TOGGLE = 6;
  CMD_REQUEST_UPDATE = 7;
  CMD_SET_UPDATE_RATE = 8;
  CMD_FORCE_FULL_UPDATE = 9;
  CMD_SET_QUALITY_MODE = 10;     // NEW: Set quality mode
  CMD_SET_COMPRESSION = 11;      // NEW: Set compression level

  // Quality settings for crystal clear display
  QUALITY_MODE_FAST = 1;         // Fast updates, lower quality
  QUALITY_MODE_BALANCED = 2;     // Balanced quality/speed
  QUALITY_MODE_CRYSTAL = 3;      // Maximum quality, slower updates

  // Compression levels (1-9, where 9 is best quality)
  COMPRESSION_FAST = 1;          // Fast compression
  COMPRESSION_BALANCED = 6;      // Balanced
  COMPRESSION_BEST = 9;          // Best quality

  // Update rates for different quality modes
  UPDATE_RATE_CRYSTAL = 16;      // 60 FPS for crystal mode
  UPDATE_RATE_BALANCED = 33;     // 30 FPS for balanced
  UPDATE_RATE_FAST = 50;         // 20 FPS for fast mode

  // Delta threshold (percentage of pixels that must change)
  DELTA_THRESHOLD_CRYSTAL = 0.1; // 0.1% for crystal clear
  DELTA_THRESHOLD_BALANCED = 1.0; // 1% for balanced
  DELTA_THRESHOLD_FAST = 5.0;    // 5% for fast mode

type
  // Mouse data structure
  TMouseData = packed record
    X: Integer;
    Y: Integer;
    Button: Byte;      // 1=Left, 2=Right, 3=Middle
    Action: Byte;      // 0=Move, 1=Down, 2=Up, 3=DoubleClick
  end;

  // Keyboard data structure
  TKeyData = packed record
    VKey: Word;        // Virtual key code
    Action: Byte;      // 1=KeyDown, 2=KeyUp
  end;

  // Quality settings structure
  TQualitySettings = packed record
    Mode: Byte;              // Quality mode (1-3)
    CompressionLevel: Byte;  // Compression level (1-9)
    UpdateRate: Word;        // Update rate in milliseconds
    DeltaThreshold: Single;  // Delta threshold percentage
  end;

// Utility functions
function FormatBytes(Bytes: Int64): string;
function GetOptimalSettings(Mode: Byte): TQualitySettings;
function CalculateCompressionRatio(OriginalSize, CompressedSize: Integer): Single;

implementation

function FormatBytes(Bytes: Int64): string;
begin
  if Bytes < 1024 then
    Result := Format('%d B', [Bytes])
  else if Bytes < 1024 * 1024 then
    Result := Format('%.1f KB', [Bytes / 1024])
  else if Bytes < 1024 * 1024 * 1024 then
    Result := Format('%.1f MB', [Bytes / (1024 * 1024)])
  else
    Result := Format('%.1f GB', [Bytes / (1024 * 1024 * 1024)]);
end;

function GetOptimalSettings(Mode: Byte): TQualitySettings;
begin
  case Mode of
    QUALITY_MODE_FAST:
    begin
      Result.Mode := QUALITY_MODE_FAST;
      Result.CompressionLevel := COMPRESSION_FAST;
      Result.UpdateRate := UPDATE_RATE_FAST;
      Result.DeltaThreshold := DELTA_THRESHOLD_FAST;
    end;

    QUALITY_MODE_BALANCED:
    begin
      Result.Mode := QUALITY_MODE_BALANCED;
      Result.CompressionLevel := COMPRESSION_BALANCED;
      Result.UpdateRate := UPDATE_RATE_BALANCED;
      Result.DeltaThreshold := DELTA_THRESHOLD_BALANCED;
    end;

    QUALITY_MODE_CRYSTAL:
    begin
      Result.Mode := QUALITY_MODE_CRYSTAL;
      Result.CompressionLevel := COMPRESSION_BEST;
      Result.UpdateRate := UPDATE_RATE_CRYSTAL;
      Result.DeltaThreshold := DELTA_THRESHOLD_CRYSTAL;
    end;

    else
    begin
      // Default to crystal clear mode
      Result := GetOptimalSettings(QUALITY_MODE_CRYSTAL);
    end;
  end;
end;

function CalculateCompressionRatio(OriginalSize, CompressedSize: Integer): Single;
begin
  if OriginalSize > 0 then
    Result := (1 - (CompressedSize / OriginalSize)) * 100
  else
    Result := 0;
end;

end.
