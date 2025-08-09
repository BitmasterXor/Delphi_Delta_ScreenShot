program RDPServerApp;

uses
  Vcl.Forms,
  RDPCommon in '..\Common\RDPCommon.pas',
  RDPServer in 'RDPServer.pas' {RDPServerForm},
  StreamManager in '..\Common\StreamManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'RDP Server';
  Application.CreateForm(TRDPServerForm, RDPServerForm);
  Application.Run;
end.