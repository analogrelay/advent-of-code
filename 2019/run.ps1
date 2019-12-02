param([Parameter(Mandatory = $false, Position = 0)][string]$SourceDir)

if (!$SourceDir) {
    $SourceDir = Get-Location
}
$SourceDir = Convert-Path $SourceDir

& "$PSScriptRoot\build.ps1" -SourceDir $SourceDir

Write-Host -ForegroundColor Green "Running..."

Push-Location $SourceDir
try {
    java -jar $OutputJar @args
} finally {
    Pop-Location
}