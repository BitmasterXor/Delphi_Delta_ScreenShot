# 🖥️ Delphi_Delta_ScreenShot
**Advanced Desktop Screenshot Capture, Analysis & Network Transmission System**

<div align="center">

![Version](https://img.shields.io/badge/Version-2.1-blue?style=for-the-badge)
![Delphi](https://img.shields.io/badge/Delphi-XE2%2B-red?style=for-the-badge)
![Platform](https://img.shields.io/badge/Platform-Windows-green?style=for-the-badge)
![License](https://img.shields.io/badge/License-MIT-orange?style=for-the-badge)

*High-performance screenshot monitoring with real-time pixel difference detection and network streaming capabilities*

</div>

---

## 🚀 Overview

The Delphi_Delta_ScreenShot Suite is a comprehensive desktop monitoring solution built in Delphi, featuring three powerful applications that work together to provide advanced screenshot analysis, comparison, and network transmission capabilities. Perfect for remote monitoring, screen sharing, change detection, and educational demonstrations.

### 🎯 What's Included

- **📡 Server Application** - Centralized screenshot receiver with real-time display
- **📱 Client Application** - Intelligent screenshot sender with delta compression
- **🔍 Standalone Comparator** - Local screenshot difference analyzer
![Server Preview](ServerPreview.png)
![Client Preview](ClientPreview.png)
---

## 🏗️ Architecture

```mermaid
graph TD
    A[Desktop Screen] --> B[Client Captures Screenshot]
    B --> C{Delta Mode?}
    C -->|Yes| D[Compare with Previous]
    C -->|No| E[Send Full Screenshot]
    D --> F[Generate Pixel Changes Only]
    F --> G[Transmit via NetCom7 Sockets]
    E --> G
    G --> H[Server Receives Data]
    H --> I[Process & Display]
    I --> J[Live Monitor Display]
```

---

## ⭐ Key Features

### 🖥️ **Server (Receiver)**
- **Real-time Display** - Live screenshot viewing from multiple clients
- **Delta Processing** - Efficient pixel-change reconstruction
- **Multi-client Support** - Handle connections from multiple sources simultaneously using NetCom7
- **Performance Optimized** - ScanLine-based bitmap processing for maximum speed
- **Network Statistics** - Track bandwidth usage, client connections, and data flow
- **Request/Response** - Server can request screenshots from clients on-demand

### 📱 **Client (Sender)**
- **Intelligent Capture** - Full desktop screenshot acquisition
- **Delta Compression** - Send only changed pixels (up to 95% bandwidth reduction)
- **Auto-capture Mode** - Configurable interval-based automatic transmission
- **Network Resilience** - Automatic reconnection and error handling
- **Flexible Modes** - Switch between full screenshots and delta updates
- **Performance Metrics** - Real-time transmission statistics and timing

### 🔍 **Standalone Comparator**
- **Visual Diff Display** - Highlight exactly what changed between screenshots
- **Pixel-Perfect Analysis** - Detect even single-pixel modifications
- **Size Calculation** - Precise measurement of actual changes (bytes)
- **High Performance** - Optimized scanline processing for large images
- **Memory Efficient** - Smart bitmap management for minimal resource usage

---

## 🛠️ Technical Specifications

### Performance Optimizations
- **24-bit Pixel Format** - Consistent memory layout for fastest access
- **ScanLine Processing** - Direct memory manipulation (100x+ faster than Canvas.Pixels)
- **Delta Compression Algorithm** - Transmit only changed pixels with coordinate mapping
- **Memory Streaming** - Efficient bitmap serialization and network transmission
- **Multi-threaded Architecture** - Non-blocking UI during intensive operations

### Network Protocol (NetCom7)
```pascal
// Command Structure used with NetCom7
CMD_REQUEST_SCREENSHOT     = 1  // Server requests client screenshot
CMD_SEND_SCREENSHOT        = 2  // Client sends full screenshot data
CMD_SEND_DELTA_SCREENSHOT  = 3  // Client sends pixel changes only

// Delta Data Format
TDeltaHeader = packed record
  Width, Height: Word;      // Image dimensions
  ChangeCount: Cardinal;    // Number of pixel changes
end;

TPixelChange = packed record
  X, Y: Word;              // Pixel coordinates
  Color: TColor;           // New pixel color value
end;
```

---

## 📦 Installation

### Prerequisites
- **Delphi XE2 or later**
- **Windows 7+**
- **NetCom7 components** - [Available on GitHub](https://github.com/spawn451/NetCom7)

### Quick Setup
1. **Clone Repository**
   ```bash
   git clone https://github.com/BitmasterXor/Delphi_Delta_ScreenShot.git
   cd Delphi_Delta_ScreenShot
   ```

2. **Install NetCom7**
   - Download NetCom7 from https://github.com/spawn451/NetCom7
   - Install package in Delphi IDE
   - Ensure components are available in Tool Palette

3. **Build Applications**
   ```
   Server: Open Server\ScreenshotServer.dpr → Build
   Client: Open Client\ScreenshotClient.dpr → Build  
   Comparator: Open Comparator\ScreenshotComparator.dpr → Build
   ```

4. **Deploy & Run**
   - Run Server first, click "Start Server"
   - Run Client(s), enter server IP, click "Connect"
   - Use Comparator standalone for local analysis

---

## 🚀 Usage Examples

### Basic Remote Monitoring
```pascal
// Server Side - Start listening for connections
procedure StartMonitoring;
begin
  ncServerSource1.Port := 8080;
  ncServerSource1.Active := True;
  // Server now receives and displays screenshots automatically
end;
```

### Automated Screenshot Streaming
```pascal
// Client Side - Auto-capture every 500ms with delta compression
procedure EnableAutoCapture;
begin
  chkAutoDelta.Checked := True;      // Enable delta mode
  chkAutoCapture.Checked := True;    // Enable auto-capture
  spnInterval.Value := 500;          // 500ms intervals
  // Client now streams changes automatically
end;
```

### Local Screenshot Analysis
```pascal
// Comparator - Detect changes between two captures
function AnalyzeChanges: string;
var
  ChangesBitmap: TBitmap;
begin
  TakeScreenshot(FCurrentBitmap);
  ChangesBitmap := CompareImages(FLastBitmap, FCurrentBitmap);
  Result := Format('Changes: %.2f KB', [FChangesSize / 1024]);
end;
```

---

## 📈 Performance Benchmarks (Estimation)
*Performance differs with screen sizes, hardware specifications, and network conditions*

| Operation | Time (avg) | Memory Usage | Notes |
|-----------|------------|--------------|-------|
| **Full Screenshot (1920x1080)** | ~50ms | 6.2MB | BitBlt + Stream serialization |
| **Delta Generation** | ~25ms | 2.1MB | ScanLine pixel comparison |
| **Network Transmission** | ~15ms | 1.2MB | Compressed delta data |
| **Server Display Update** | ~10ms | 0.8MB | Direct bitmap assignment |

*Tested on: Intel i7, 16GB RAM, 1Gbps network*

---

## 🎛️ Configuration Options

### Server Settings
- **Port Configuration** - Default 8080, customizable
- **Encryption Key** - Secure client-server communication
- **Client Limits** - Maximum concurrent connections
- **Display Options** - Scaling, refresh rates, statistics

### Client Settings
- **Server Connection** - IP address and port configuration
- **Capture Modes** - Full screenshot vs. delta-only transmission
- **Auto-capture** - Interval timing (50ms - 60000ms)
- **Quality Settings** - Bitmap compression and pixel format options

### Comparator Settings
- **Sensitivity** - Pixel difference thresholds
- **Display Modes** - Highlight changes, side-by-side, overlay
- **Export Options** - Save difference maps and statistics

---

## 🎯 Use Cases

### 🏢 **Enterprise & Business**
- **Remote Support** - IT technicians monitor user screens in real-time
- **Quality Assurance** - Automated detection of UI changes during testing
- **Training & Education** - Live demonstration of software usage
- **Security Monitoring** - Track desktop activity and unauthorized changes

### 🎮 **Development & Testing**
- **Regression Testing** - Automated detection of visual changes in applications
- **Performance Analysis** - Monitor application behavior over time
- **Live Coding Sessions** - Stream desktop to multiple viewers efficiently
- **Version Comparison** - Visual diff between software releases

### 🎓 **Educational**
- **Screen Recording** - Efficient capture of desktop tutorials
- **Student Monitoring** - Teachers observe student progress remotely
- **Presentation Tools** - Share desktop with minimal bandwidth usage
- **Research Applications** - Analyze user interaction patterns

---

## 🔧 Advanced Features

### Network Optimization
- **Adaptive Compression** - Automatically adjust quality based on network conditions  
- **Connection Pooling** - Efficiently manage multiple client connections
- **Bandwidth Throttling** - Limit transmission rates to prevent network congestion
- **Reconnection Logic** - Automatic recovery from network interruptions

### Image Processing
- **Smart Delta Detection** - Ignore insignificant pixel changes (cursors, animations)
- **Region of Interest** - Focus monitoring on specific screen areas
- **Color Space Optimization** - Efficient pixel format conversions
- **Memory Pool Management** - Reuse bitmap objects to minimize garbage collection

---

## 🧪 Testing & Quality Assurance

### Automated Tests
- **Unit Tests** - Core bitmap processing functions
- **Integration Tests** - Client-server communication protocols  
- **Performance Tests** - Memory usage and processing speed benchmarks
- **Stress Tests** - Multiple concurrent connections and high-frequency updates

### Manual Testing Checklist
- [ ] Server starts/stops cleanly
- [ ] Multiple clients connect simultaneously
- [ ] Delta compression reduces bandwidth usage
- [ ] Screenshot quality remains high
- [ ] Network interruptions handled gracefully
- [ ] Memory usage stays within acceptable limits

---

## 🤝 Contributing

We welcome contributions! Here's how to get started:

1. **Fork** the repository
2. **Create** a feature branch (`git checkout -b feature/amazing-feature`)
3. **Commit** your changes (`git commit -m 'Add amazing feature'`)
4. **Push** to branch (`git push origin feature/amazing-feature`)
5. **Open** a Pull Request

### Development Guidelines
- Follow Delphi coding conventions
- Add unit tests for new functionality
- Update documentation for API changes
- Optimize for performance where possible
- Test with multiple Delphi versions

---

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## 👨‍💻 Author

**BitmasterXor**
- GitHub: [@BitmasterXor](https://github.com/BitmasterXor)
- Discord: BitmasterXor

---

## 🙏 Acknowledgments

- **NetCom7 Team** - Excellent networking components for Delphi ([GitHub](https://github.com/spawn451/NetCom7))
- **Delphi Community** - Continuous inspiration and support
- **Beta Testers** - Valuable feedback during development
- **Open Source Contributors** - Thanks for your improvements and bug reports

---

## 📊 Project Stats

![GitHub stars](https://img.shields.io/github/stars/BitmasterXor/Delphi_Delta_ScreenShot)
![GitHub forks](https://img.shields.io/github/forks/BitmasterXor/Delphi_Delta_ScreenShot)
![GitHub issues](https://img.shields.io/github/issues/BitmasterXor/Delphi_Delta_ScreenShot)

---

<div align="center">

**⭐ Star this repository if you find it useful!**

**Made with ❤️ by BitmasterXor for the Delphi Community**

</div>
