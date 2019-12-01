param([Parameter(Mandatory = $false, Position = 0)][int]$DayNumber = $null)

if (!$DayNumber) {
  $DayNumber = Get-ChildItem $PSScriptRoot | 
  Where-Object { $_ -match "day(\d\d)" } |
  ForEach-Object { [int]::Parse($matches[1]) } |
  Sort-Object -desc |
  Select-Object -first 1

  if (!$DayNumber) {
    $DayNumber = 1
  }
}

Write-Host "Generating project for day $DayNumber"

$DayDirName = "day$($DayNumber.ToString("00"))"
$DayDir = Join-Path $PSScriptRoot $DayDirName
if (!(Test-Path $DayDir)) {
  New-Item -ItemType Directory $DayDir
  $InputFilePath = Join-Path $DayDir "input.txt"
  $TestFilePath = Join-Path $DayDir "test.txt"
  "" | Out-File $InputFilePath
  "" | Out-File $TestFilePath
}
else {
  Write-Warning "Day directory already exists: $DayDir"
}