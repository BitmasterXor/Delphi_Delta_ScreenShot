program RDPClientApp;

uses
  Vcl.Forms,
  RDPCommon in '..\Common\RDPCommon.pas',
  RDPScreenCapture in 'RDPScreenCapture.pas',
  RDPClient in 'RDPClient.pas' {RDPClientForm},
  StreamManager in '..\Common\StreamManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'RDP Client';
  Application.CreateForm(TRDPClientForm, RDPClientForm);
  Application.Run;
end.
