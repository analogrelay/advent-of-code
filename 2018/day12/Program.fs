open System
open VibrantCode.AdventOfCode.AdventHelpers

let parseStateSequence (s: string) =
    s
    |> Seq.map (fun c ->
        match c with
        | '.' -> false
        | '#' -> true
        | _ -> raise (new Exception("Invalid Input")))
    |> List.ofSeq

let flagToString flag = if flag then "#" else "."

type Rule(template: bool list, result: bool) =
    member _x.Template = template
    member _x.Result = result

    member _x.Match (sequence: bool list) =
        // Match the devirtualized state against the template
        if template = sequence then
            // If there's a match, return a result
            Some(result)
        else
            None

    static member parse (rule: string) =
        let (template, result) =
            match rule with
            | Regex "([.#]*) => ([.#])" [ template; result ] ->
                (parseStateSequence template, parseStateSequence result |> List.exactlyOne)
            | _ -> raise (new Exception("Invalid Input"))
        Rule(template, result)

type Machine(tape: bool array, startId: int, rules: Rule list) =
    member _x.Tape = tape
    member _x.Rules = rules
    member _x.StartId = startId

    static member parse (initialState: string) (rules: string list) =
        let tape = 
            match initialState with
            | Regex "initial state: (.*)" [ state ] -> parseStateSequence state
            | _ -> raise (new Exception("Invalid Input"))

        let parsedRules = rules |> List.map Rule.parse

        Machine(tape, 0, parsedRules)

let renderTape (tape: bool list) =
    tape
    |> Seq.map flagToString
    |> String.concat ""
    |> printfn "Tape %s"

let renderRule (rule: Rule) =
    let template = rule.Template |> Seq.map flagToString |> String.concat ""
    let result = rule.Result |> flagToString
    printfn "%s => %s" template result

let step (m: Machine) =
    // Materialize the virtual pots on either end (representing the infinite sequence of pots)
    let newTape = [ false; false; false; false; ] @@ m.Tape @@ [ false; false; false; false; ]

let run (m: Machine) =
    m.Tape |> renderTape
    m.Rules |> List.iter renderRule

[<EntryPoint>]
let main argv =
    let lines =
        argv.[0]
        |> loadLines
        |> List.ofSeq

    match lines with
    | initialState :: _ :: rules -> Machine.parse initialState rules |> run
    | _ -> raise (new Exception("Invalid Input"))

    0

