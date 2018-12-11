param([Parameter(Mandatory = $true, Position = 0)][int]$DayNumber)

Push-Location $PSScriptRoot
try {
    $name = "day$($DayNumber.ToString("00"))"
    dotnet new console --language "F#" --name $name
    dotnet sln add $name
    dotnet add $name reference "./VibrantCode.AdventOfCode"

    "" | Add-Content -Path "./$name/input.txt"
    "" | Add-Content -Path "./$name/test.txt"

    $LaunchSettingsContent = @"
{
  "profiles": {
    "$name": {
      "commandName": "Project",
      "commandLineArgs": "`$(MSBuildProjectDirectory)/input.txt"
    },
    "$name - test": {
      "commandName": "Project",
      "commandLineArgs": "`$(MSBuildProjectDirectory)/test.txt"
    }
  }
}
"@
    mkdir "./$name/Properties" | Out-Null

    $ProgramFsContent = @"
open System
open VibrantCode.AdventOfCode.AdventHelpers

let run data = 
    raise (new NotImplementedException())

[<EntryPoint>]
let main argv =
    argv.[0]
    |> loadLines
    |> Seq.map int
    |> run

    0
"@
    $LaunchSettingsContent | Set-Content -Path "./$name/Properties/launchSettings.json" -Encoding UTF8
    $ProgramFsContent | Set-Content -Path "./$name/Program.fs" -Encoding UTF8
}
finally {
    Pop-Location
}