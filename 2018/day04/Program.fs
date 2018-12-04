open System

open System
open VibrantCode.AdventOfCode
open System.Text.RegularExpressions
open System.ComponentModel.DataAnnotations
open System.Linq

let parseGuardId (input: string) =
    let hashIdx = input.IndexOf '#'
    let spaceIdx = input.IndexOf(' ', hashIdx)
    input.Substring((hashIdx + 1), (spaceIdx - hashIdx - 1)) |> int

type LogRow(timestamp: DateTime, message: string) =
    member _this.TimeStamp = timestamp
    member _this.Message = message
    static member parse (input: string) =
        let endBracket = input.IndexOf ']'
        let timestamp = input.Substring(1, (endBracket - 1)) |> DateTime.Parse
        let message = (input.Substring (endBracket + 1)).Trim()
        LogRow(timestamp, message)

type LogAction =
    | StartShift
    | FallAsleep
    | WakeUp

type LogEntry = {
    TimeStamp: DateTime;
    Action: LogAction;
    Guard: int;
    } with
    static member parse (previousEntry: LogEntry option) (row: LogRow) =
        let (guard, action) =
            match (previousEntry, row.Message) with
            | (Some(e), "falls asleep") -> (e.Guard, FallAsleep)
            | (Some(e), "wakes up") -> (e.Guard, WakeUp)
            | (None, "falls asleep") -> raise (new Exception("No current guard!"))
            | (None, "wakes up") -> raise (new Exception("No current guard!"))
            | (_, x) -> (parseGuardId x, StartShift)
        { TimeStamp = row.TimeStamp; Action = action; Guard = guard }

type TimelineDay(day: DateTime, guard: int, entries: LogEntry seq) =
    member _this.Day = day;
    member _this.Guard = guard;
    member _this.Entries = entries;
    member _this.EachMinute =
        seq {
            let mutable prevMinute = 0
            let mutable prevAction = WakeUp
            for entry in entries do
                let count = entry.TimeStamp.Minute - prevMinute
                yield! (Seq.replicate count prevAction)
                prevMinute <- entry.TimeStamp.Minute
                prevAction <- entry.Action
            let remaining = 60 - prevMinute
            if remaining > 0 then
                for x in 0..remaining do
                    yield prevAction
        }
        
    member this.AsleepAt (minute: int) = (this.EachMinute |> Seq.item minute) = FallAsleep
    member this.MinutesAsleep = this.EachMinute |> Seq.filter (fun x -> x = FallAsleep) |> Seq.length

type Timeline(list: TimelineDay list) =
    member _this.Days = list
    static member parse (lines: string seq) =
        lines
        |> Seq.map LogRow.parse
        |> Seq.sortBy (fun x -> x.TimeStamp)
        |> Seq.scan (fun state line -> Some(LogEntry.parse state line)) None
        |> Seq.skipWhile (fun x -> x.IsNone)
        |> Seq.map (fun x -> x.Value)
        |> Seq.filter (fun x -> x.Action <> StartShift)
        |> Timeline.create
        
    static member create (log: LogEntry seq) =
        log
        |> Seq.groupBy (fun { TimeStamp = t; Guard = g } -> (t.Date, g))
        |> Seq.map (fun ((day, guard), entries) -> TimelineDay(day, guard, entries))
        |> List.ofSeq
        |> Timeline
        

let actionChar (action: LogAction) =
    match action with
    | StartShift -> raise (new Exception("Unexpected state"))
    | WakeUp -> '.'
    | FallAsleep -> '#'

let displayTimeline (timeline: Timeline) =
    // Print a timeline, to prove we got it right
    printfn "Date   ID     Minute"
    printfn "              000000000011111111112222222222333333333344444444445555555555"
    printfn "              012345678901234567890123456789012345678901234567890123456789"

    timeline.Days
    |> Seq.iter (fun day ->
        let timelineStr =
            day.EachMinute
            |> Seq.map (fun action ->
                match action with
                | WakeUp -> "."
                | FallAsleep -> "#"
                | _ -> "")
            |> String.concat ""
        printfn "%02d-%02d  #%04d  %s" day.Day.Month day.Day.Day day.Guard timelineStr
    )

let part1 (timeline: Timeline) =
    let byGuard =
        timeline.Days
        |> Seq.groupBy (fun x -> x.Guard)

    let sleepiestGuard =
        byGuard
        |> Seq.maxBy (fun (guard, entries) -> entries |> Seq.sumBy (fun entry -> entry.MinutesAsleep))

    // Determine which minute they sleep most in
    let sleepiestMinute =
        seq { 0..59 }
        |> Seq.maxBy (fun minute ->
            sleepiestGuard
            |> snd
            |> Seq.filter (fun day -> day.AsleepAt minute)
            |> Seq.length
        )

    let result = (fst sleepiestGuard) * sleepiestMinute

    printfn "Part 1: %d * %d = %d" (fst sleepiestGuard) sleepiestMinute result

let part2 (timeline: Timeline) =
    let byGuard =
        timeline.Days
        |> Seq.groupBy (fun x -> x.Guard)

    // Generate sleep counts by minute by guard
    let (minute, guard, count) =
        seq { 0..59 }
        |> Seq.map (fun minute ->
            let (guard, count) =
                byGuard
                |> Seq.map (fun (guard, days) ->
                    let sleepCount =
                        days
                        |> Seq.filter (fun x -> x.AsleepAt minute)
                        |> Seq.length
                    (guard, sleepCount))
                |> Seq.maxBy snd
            (minute, guard, count))
        |> Seq.maxBy (fun (minute, guard, count) -> count)

    let result = guard * minute

    printfn "Part 2: %d * %d = %d" guard minute result

let run (timeline: Timeline) =
    timeline |> displayTimeline
    timeline |> part1
    timeline |> part2

[<EntryPoint>]
let main argv =
    argv.[0]
    |> AdventHelpers.loadLines
    |> Timeline.parse
    |> run

    0
