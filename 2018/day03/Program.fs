open System
open VibrantCode.AdventOfCode.AdventHelpers
open System.Diagnostics
open System.Data

let splatBy (sep: char) (str: string) =
    let splat = str.Split(sep)
    if splat.Length = 2 then
        Some((splat.[0] |> int, splat.[1] |> int))
    else
        None


// Active patterns for string parsing
let (|ClaimId|_|) (str: string) =
    if str.StartsWith("#") then
        Some(str.Substring 1 |> int)
    else
        None

let (|ClaimCoords|_|) (str: string) = str.TrimEnd(':') |> splatBy ','
let (|ClaimSize|_|) (str: string) = str |> splatBy 'x'

type Claim = {
    Id: int;
    Coords: int * int;
    Size: int * int; } with
    /// <summary>Attempts to parse a line as a claim</summary>
    static member tryParse (line: string) =
        match line.Split ' ' with
        | [| ClaimId id; _; ClaimCoords coords; ClaimSize size |] -> Some({ Id = id; Coords = coords; Size = size; })
        | _ -> None // Skip invalid lines

    /// <summary>Enumerates the cells covered by this claim</summary>
    member this.enumerateRegions =
        let x_start = this.Coords |> fst
        let x_end = x_start + (this.Size |> fst) - 1
        let y_start = this.Coords |> snd
        let y_end = y_start + (this.Size |> snd) - 1
        seq {
            for row in x_start..x_end do
                for col in y_start..y_end do
                    yield (row, col) }

[<DebuggerDisplay("{debugDisplay}")>]
type Fabric = {
    Space: int[][] } with
    static member create (size: int * int) =
        { Space = Array.init (fst size) (fun _ -> Array.zeroCreate (snd size)) }

    /// <summary>Applies the provided claim to the fabric. Returns the Fabric for use in fold operations</summary>
    member this.applyClaim (claim: Claim) =
        claim.enumerateRegions
        |> Seq.iter (fun (row, col) ->
            this.Space.[row].[col] <- this.Space.[row].[col] + 1)

        this

    member this.countIntersections (min: int) =
        this.Space
        |> Seq.map (fun row ->
            row
            |> Seq.filter (fun cell -> cell >= min)
            |> Seq.length)
        |> Seq.sum

    member this.maxIntersections (claim: Claim) =
        claim.enumerateRegions
        |> Seq.map (fun (row, col) -> this.Space.[row].[col])
        |> Seq.max

    member this.debugDisplay =
        this.Space
        |> Seq.map (fun row ->
            row
            |> Seq.map string
            |> String.concat "")
        |> String.concat (Environment.NewLine)

let run (data: Claim seq) =
    let max_x =
        data
        |> Seq.map (fun claim -> (fst claim.Coords) + (fst claim.Size))
        |> Seq.max
    
    let max_y =
        data
        |> Seq.map (fun claim -> (snd claim.Coords) + (snd claim.Size))
        |> Seq.max
    

    let fabric =
        data
        |> Seq.fold (fun (fabric: Fabric) claim -> fabric.applyClaim claim) (Fabric.create (max_x, max_y))

    fabric.countIntersections 2
    |> printfn "Part 1: %d"

    // Iterate back over the claims looking for ones that have no intersections
    data
    |> Seq.filter (fun claim -> (fabric.maxIntersections claim) = 1)
    |> Seq.map (fun { Id = i } -> i)
    |> List.ofSeq
    |> List.exactlyOne
    |> printfn "Part 2: %d"

[<EntryPoint>]
let main argv =
    argv.[0]
    |> loadLines
    |> Seq.choose Claim.tryParse
    |> run

    0
