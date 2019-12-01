$Source = Convert-Path (Get-Location)

$Name = Split-Path -Leaf $Source

$OutputDir = Join-Path $Source "build"
if (!(Test-Path $OutputDir)) {
    New-Item -ItemType Directory $OutputDir | Out-Null
}

$OutputJar = Join-Path $OutputDir "$Name.jar"

$InputFile = Join-Path $Source "main.kt"

kotlinc $InputFile -include-runtime -d $OutputJar
java -jar $OutputJar @args