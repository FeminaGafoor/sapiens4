[Setup]
AppId={#PackageId}
AppName={#PackageName}
AppVersion={#AppVersion}
VersionInfoVersion={#FileVersion}
DefaultDirName={localappdata}\Programs\{#PackageName}
DefaultGroupName={#PackageName}
PrivilegesRequired=lowest
ArchitecturesAllowed={#PackageArchitecture}
ArchitecturesInstallIn64BitMode={#PackageArchitecture}
MinVersion=10.0
DisableDirPage=yes
DisableProgramGroupPage=yes
#ifdef StagePayload
UsePreviousAppDir=no
Uninstallable=no
CreateUninstallRegKey=no
#else
UsePreviousAppDir=yes
#endif
OutputDir={#OutputDir}
OutputBaseFilename={#ArtifactBaseName}
Compression=lzma2/fast
SolidCompression=yes
CloseApplications=no
RestartApplications=no
RestartIfNeededByRun=no
UninstallDisplayIcon={app}\Sapiens4.exe
#ifdef PublisherSign
SignTool=sapiens
SignedUninstaller=yes
#else
SignedUninstaller=no
#endif

[Files]
#ifdef StagePayload
Source: "{#SourceDir}\*"; DestDir: "{app}"; Excludes: "sapiens-installation.id,sapiens-starter.exe"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#SourceDir}\sapiens-installation.id"; DestDir: "{app}"; Flags: ignoreversion
#else
Source: "{#SourceDir}\*"; DestDir: "{app}\versions\{#InitialVersion}"; Excludes: "sapiens-starter.exe"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "{#SourceDir}\sapiens-starter.exe"; DestDir: "{app}"; DestName: "Sapiens4.exe"; Flags: ignoreversion
Source: "{#SourceDir}\sapiens-installation.id"; DestDir: "{app}"; Flags: ignoreversion
#endif

#ifndef StagePayload
[Icons]
Name: "{userprograms}\{#PackageName}"; Filename: "{app}\Sapiens4.exe"

[Run]
Filename: "{app}\Sapiens4.exe"; Description: "Launch {#PackageName}"; Flags: nowait postinstall skipifsilent unchecked

[UninstallDelete]
Type: files; Name: "{app}\installation.lock"
Type: dirifempty; Name: "{app}\backups"
Type: dirifempty; Name: "{app}\versions"

#endif

[Code]
#ifndef StagePayload
const
  Identity = '{#PackageId}';
  ImageWriteAccess = $40000000;
  ImageDeleteAccess = $00010000;

function OwnedDirectory: Boolean;
var Marker: AnsiString; Installed: String;
begin
  Result := LoadStringFromFile(ExpandConstant('{app}\sapiens-installation.id'), Marker);
  Result := Result and (Trim(String(Marker)) = Identity);
  Result := Result and RegQueryStringValue(HKCU,
    'Software\Microsoft\Windows\CurrentVersion\Uninstall\' + Identity + '_is1', 'InstallLocation', Installed);
  Result := Result and (CompareText(AddBackslash(Installed), AddBackslash(ExpandConstant('{app}'))) = 0);
end;
#endif

function FileAttributes(FileName: String): Cardinal;
  external 'GetFileAttributesW@kernel32.dll stdcall';

function SafeDestination: Boolean;
var Target, StateRoot, Parent, NextParent: String; Item: TFindRec; Attributes: Cardinal;
begin
  Target := AddBackslash(ExpandFileName(ExpandConstant('{app}')));
  Result := True;
  Parent := RemoveBackslash(Target);
  while Parent <> '' do begin
    Attributes := FileAttributes(Parent);
    if (Attributes <> $FFFFFFFF) and ((Attributes and $400) <> 0) then begin Result := False; Exit; end;
    NextParent := ExtractFileDir(Parent);
    if CompareText(Parent, NextParent) = 0 then Break;
    Parent := NextParent;
  end;
  StateRoot := AddBackslash(ExpandConstant('{localappdata}\Sapiens4'));
  if CompareText(Copy(Target, 1, Length(StateRoot)), StateRoot) = 0 then begin Result := False; Exit; end;
  StateRoot := AddBackslash(ExpandConstant('{localappdata}\Sapiens4 Development'));
  if CompareText(Copy(Target, 1, Length(StateRoot)), StateRoot) = 0 then begin Result := False; Exit; end;
  if GetEnv('SAPIENS_DATA_DIR') <> '' then begin
    StateRoot := AddBackslash(ExpandFileName(GetEnv('SAPIENS_DATA_DIR')));
    if CompareText(Copy(Target, 1, Length(StateRoot)), StateRoot) = 0 then begin Result := False; Exit; end;
  end;
  if GetEnv('SAPIENS_CONFIG_DIR') <> '' then begin
    StateRoot := AddBackslash(ExpandFileName(GetEnv('SAPIENS_CONFIG_DIR')));
    if CompareText(Copy(Target, 1, Length(StateRoot)), StateRoot) = 0 then begin Result := False; Exit; end;
  end;
#ifndef StagePayload
  if FileExists(ExpandConstant('{app}\update-state.json')) then begin Result := False; Exit; end;
  if OwnedDirectory then Exit;
#endif
  if FindFirst(Target + '*', Item) then begin
    try
      repeat
        if (Item.Name <> '.') and (Item.Name <> '..') then begin
          Result := False;
          Exit;
        end;
      until not FindNext(Item);
    finally
      FindClose(Item);
    end;
  end;
end;

#ifndef StagePayload
function OpenInstalledImage(FileName: String; Access, Sharing: Cardinal;
  Security: THandle; Creation, Attributes: Cardinal; Template: THandle): THandle;
  external 'CreateFileW@kernel32.dll stdcall';
function CloseInstalledImage(Handle: THandle): BOOL;
  external 'CloseHandle@kernel32.dll stdcall';

type
  TImageFindData = array[0..147] of Cardinal;
function FindImageFile(Pattern: String; var Data: TImageFindData): THandle;
  external 'FindFirstFileW@kernel32.dll stdcall';
function CloseImageSearch(Handle: THandle): BOOL;
  external 'FindClose@kernel32.dll stdcall';

function InstallationImagesWritable(Directory, Exempt: String; Access: Cardinal): Boolean;
var Item: TFindRec; Path, Extension: String; Handle: THandle; Attributes: Cardinal;
    FindData: TImageFindData;
begin
  Attributes := FileAttributes(Directory);
  if Attributes = $FFFFFFFF then begin
    Result := (DLLGetLastError = 2) or (DLLGetLastError = 3);
    Exit;
  end;
  Result := False;
  if (Attributes and $400) <> 0 then Exit;
  if not FindFirst(AddBackslash(Directory) + '*', Item) then begin
    Handle := FindImageFile(AddBackslash(Directory) + '*', FindData);
    if Handle = THandle(-1) then Result := DLLGetLastError = 2
    else CloseImageSearch(Handle);
    Exit;
  end;
  Result := True;
  try
    repeat
      if (Item.Name = '.') or (Item.Name = '..') then Continue;
      Path := AddBackslash(Directory) + Item.Name;
      if (Item.Attributes and $400) <> 0 then begin Result := False; Exit; end;
      if (Item.Attributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
        Result := InstallationImagesWritable(Path, Exempt, Access)
      else begin
        Extension := ExtractFileExt(Path);
        if ((CompareText(Extension, '.exe') = 0) or (CompareText(Extension, '.dll') = 0)) and
           (CompareText(Path, Exempt) <> 0) then begin
          Handle := OpenInstalledImage(Path, Access, 7, 0, 3, 0, 0);
          Result := Handle <> THandle(-1);
          if Result then Result := CloseInstalledImage(Handle);
        end;
      end;
      if not Result then begin Log('Installation image unavailable: ' + Path); Exit; end;
    until not FindNext(Item);
  finally
    FindClose(Item);
  end;
end;
#endif

function PrepareToInstall(var NeedsRestart: Boolean): String;
begin
  Result := '';
#ifdef StagePayload
  if ExpandConstant('{param:DIR|}') = '' then begin
    Result := 'Payload staging requires an explicit new /DIR destination.';
    Exit;
  end;
#endif
  if not SafeDestination then begin
    Result := 'Choose a new installation directory. Existing data or unowned files cannot be replaced.';
    Exit;
  end;
#ifndef StagePayload
  if not InstallationImagesWritable(ExpandConstant('{app}'), '', ImageWriteAccess) then
    Result := 'Quit {#PackageName} and its backend, then retry. No running process will be stopped by Setup.';
#endif
end;

#ifndef StagePayload
function InitializeUninstall: Boolean;
begin
  Result := OwnedDirectory;
  if not Result then begin
    Log('Uninstall refused: the installation directory could not be verified.');
    if not UninstallSilent then
      MsgBox('The {#PackageName} installation directory could not be verified.', mbError, MB_OK);
  end;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var Code: Integer;
begin
  if CurUninstallStep <> usUninstall then Exit;
  if InstallationImagesWritable(ExpandConstant('{app}'), ExpandConstant('{uninstallexe}'), ImageWriteAccess or ImageDeleteAccess) then
    if Exec(ExpandConstant('{app}\Sapiens4.exe'), '--uninstall', '', SW_HIDE, ewWaitUntilTerminated, Code) and (Code = 0) then Exit;
  Log('Uninstall refused: files are in use, recovery is pending, or cleanup failed.');
  if not UninstallSilent then
    MsgBox('Quit {#PackageName} and its backend, then retry. Recover any interrupted update first.', mbError, MB_OK);
  Abort;
end;

procedure CurStepChanged(CurStep: TSetupStep);
var Code: Integer;
begin
  if CurStep = ssPostInstall then begin
    if not Exec(ExpandConstant('{app}\Sapiens4.exe'), '--initialize {#InitialVersion}', '', SW_HIDE, ewWaitUntilTerminated, Code) or (Code <> 0) then
      RaiseException('Sapiens installation could not publish its initial version.');
  end;
end;
#endif
