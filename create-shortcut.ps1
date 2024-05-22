
$Exe = Get-Item -Path .\ProcessGroup\bin\Debug\ProcessGroup.exe
$ExeFullName = $Exe.FullName
$ExeDirectory = $Exe.DirectoryName


$UserProfile = $env:USERPROFILE
$AppData = "$UserProfile\AppData"
$LinkFullPath = "$($AppData)\Roaming\Microsoft\Windows\Start Menu\Programs\VRM\ProcessGroup.lnk"

$LinkDirectory = Split-Path -Path $LinkFullPath -Parent

if (-not (Test-Path -Path $LinkDirectory)) {
  New-Item -Path $LinkDirectory -ItemType Directory -Force | Out-Null
}



$WshShell = New-Object -comObject WScript.Shell

$Shortcut = $WshShell.CreateShortcut($LinkFullPath)
$Shortcut.TargetPath = "gsudo.exe"
$Shortcut.Arguments = $ExeFullName
$Shortcut.Description = "Run ProcessGroup as Administrator"
$Shortcut.WorkingDirectory = $ExeDirectory
$Shortcut.Save()

Write-Host "Created shortcut to ProcessGroup"
Write-Host "Location: $LinkFullPath"
