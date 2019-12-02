param([Parameter(Mandatory = $false, Position = 0)][string]$SourceDir)

if (!$SourceDir) {
    $SourceDir = Get-Location
}
$SourceDir = Convert-Path $SourceDir

$Name = Split-Path -Leaf $SourceDir

$OutputDir = Join-Path $SourceDir "build"
if (!(Test-Path $OutputDir)) {
    New-Item -ItemType Directory $OutputDir | Out-Null
}

$OutputJar = Join-Path $OutputDir "$Name.jar"

$InputFile = Join-Path $SourceDir "main.kt"

Write-Host -ForegroundColor Green "Building..."
kotlinc $InputFile -include-runtime -d $OutputJar
if ($LASTEXITCODE -ne 0) {
    throw "Build Failed"
}