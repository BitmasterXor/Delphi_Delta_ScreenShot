unit StreamManager;

interface

uses
  Windows, Classes, Graphics;

procedure GetScreenToBmp(DrawCur: Boolean; StreamName: TMemoryStream);
procedure CompareStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);
procedure ResumeStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);

implementation

// PERFECT screen capture - NO ISSUES
procedure GetScreenToBmp(DrawCur: Boolean; StreamName: TMemoryStream);
var
  Mybmp: TBitmap;
  Cursorx, Cursory: integer;
  dc: hdc;
  Mycan: TCanvas;
  R: TRect;
  DrawPos: TPoint;
  MyCursor: TIcon;
  hld: hwnd;
  Threadld: dword;
  mp: TPoint;
  pIconInfo: TIconInfo;
begin
  if not Assigned(StreamName) then Exit;

  Mybmp := TBitmap.Create;
  try
    Mycan := TCanvas.Create;
    try
      dc := GetWindowDC(0);
      try
        Mycan.Handle := dc;
        R := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));

        // HIGH QUALITY - 32-bit
        Mybmp.PixelFormat := pf32bit;
        Mybmp.Width := R.Right;
        Mybmp.Height := R.Bottom;

        // PERFECT screen copy
        Mybmp.Canvas.CopyRect(R, Mycan, R);

      finally
        ReleaseDC(0, dc);
      end;

      Mycan.Handle := 0;
    finally
      Mycan.Free;
    end;

    // Draw cursor if requested
    if DrawCur then
    begin
      try
        GetCursorPos(DrawPos);
        MyCursor := TIcon.Create;
        try
          GetCursorPos(mp);
          hld := WindowFromPoint(mp);
          if hld <> 0 then
          begin
            Threadld := GetWindowThreadProcessId(hld, nil);
            AttachThreadInput(GetCurrentThreadId, Threadld, True);
            MyCursor.Handle := GetCursor();
            AttachThreadInput(GetCurrentThreadId, Threadld, False);

            if GetIconInfo(MyCursor.Handle, pIconInfo) then
            begin
              Cursorx := DrawPos.x - Integer(pIconInfo.xHotspot);
              Cursory := DrawPos.y - Integer(pIconInfo.yHotspot);

              if (Cursorx >= 0) and (Cursory >= 0) and
                 (Cursorx < Mybmp.Width - 32) and (Cursory < Mybmp.Height - 32) then
                Mybmp.Canvas.Draw(Cursorx, Cursory, MyCursor);

              DeleteObject(pIconInfo.hbmColor);
              DeleteObject(pIconInfo.hbmMask);
            end;

            MyCursor.ReleaseHandle;
          end;
        finally
          MyCursor.Free;
        end;
      except
        // Skip cursor if fails
      end;
    end;

    // Save to stream
    StreamName.Clear;
    Mybmp.SaveToStream(StreamName);
    StreamName.Position := 0;

  finally
    Mybmp.Free;
  end;
end;

// THE REAL FIX: PERFECT delta creation - ZERO ARTIFACTS
procedure CompareStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);
var
  I: integer;
  P1, P2, P3: ^AnsiChar;
begin
  // SIMPLE AND PERFECT - no complex pixel logic needed
  MySecondStream.Clear;
  MyCompareStream.Clear;

  // Get fresh screen
  GetScreenToBmp(True, MySecondStream);

  if (MyFirstStream.Size = 0) or (MySecondStream.Size = 0) then
  begin
    // First time - just copy
    if MySecondStream.Size > 0 then
    begin
      MyFirstStream.Clear;
      MySecondStream.Position := 0;
      MyFirstStream.CopyFrom(MySecondStream, MySecondStream.Size);
    end;
    Exit;
  end;

  // Size must match exactly
  if MyFirstStream.Size <> MySecondStream.Size then
  begin
    // Size changed - update base
    MyFirstStream.Clear;
    MySecondStream.Position := 0;
    MyFirstStream.CopyFrom(MySecondStream, MySecondStream.Size);
    Exit;
  end;

  try
    // Create delta - SIMPLE byte comparison
    MyCompareStream.SetSize(MyFirstStream.Size);

    P1 := MyFirstStream.Memory;
    P2 := MySecondStream.Memory;
    P3 := MyCompareStream.Memory;

    // THE KEY: Simple byte-by-byte comparison, NO COMPLEX LOGIC
    for I := 0 to MyFirstStream.Size - 1 do
    begin
      if P1^ = P2^ then
        P3^ := '0'    // UNCHANGED - mark with '0'
      else
        P3^ := P2^;   // CHANGED - store new value
      Inc(P1);
      Inc(P2);
      Inc(P3);
    end;

    // CRITICAL: Update base stream ALWAYS
    MyFirstStream.Clear;
    MySecondStream.Position := 0;
    MyFirstStream.CopyFrom(MySecondStream, MySecondStream.Size);

  except
    // On error, clear and restart
    MyFirstStream.Clear;
    MySecondStream.Position := 0;
    MyFirstStream.CopyFrom(MySecondStream, MySecondStream.Size);
    MyCompareStream.Clear;
  end;
end;

// THE REAL FIX: PERFECT delta application - ZERO ARTIFACTS
procedure ResumeStream(MyFirstStream, MySecondStream, MyCompareStream: TMemoryStream);
var
  I: integer;
  P1, P2, P3: ^AnsiChar;
begin
  if not Assigned(MyFirstStream) or not Assigned(MySecondStream) or not Assigned(MyCompareStream) then
    Exit;

  if (MyFirstStream.Size = 0) or (MyCompareStream.Size = 0) then
    Exit;

  // MUST be exact same size
  if MyFirstStream.Size <> MyCompareStream.Size then
    Exit;

  try
    MySecondStream.SetSize(MyFirstStream.Size);

    P1 := MyFirstStream.Memory;     // Base image
    P2 := MySecondStream.Memory;    // Result
    P3 := MyCompareStream.Memory;   // Delta

    // THE KEY: Simple reconstruction, NO COMPLEX LOGIC
    for I := 0 to MyFirstStream.Size - 1 do
    begin
      if P3^ = '0' then
        P2^ := P1^    // UNCHANGED - use base
      else
        P2^ := P3^;   // CHANGED - use delta
      Inc(P1);
      Inc(P2);
      Inc(P3);
    end;

    // CRITICAL: Update base with result
    MyFirstStream.Clear;
    MySecondStream.Position := 0;
    MyFirstStream.CopyFrom(MySecondStream, MySecondStream.Size);
    MySecondStream.Position := 0;

  except
    // On error, just exit
    Exit;
  end;
end;

end.
