open System
open VibrantCode.AdventOfCode.AdventHelpers
open System.Globalization

let alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
let coordId idx = alpha.Chars idx
let manhattenDistance (x, y) (xt, yt) = (abs (xt - x)) + (abs (yt - y))

type GridState =
    | Unknown
    | Equidistant
    | PartOf of char
    | Center of char

type Side =
    | North
    | East
    | South
    | West

type Coord(id: char, x: int, y: int) =
    member _this.Id = id
    member _this.X = x
    member _this.Y = y
    static member create index coords =
        let (x, y) = pairwiseApply int coords
        Coord(coordId index, x, y)

let sideOf (side: Side) arr =
    let xend = ((Array2D.length2 arr) - 1)
    let yend = ((Array2D.length1 arr) - 1)
    match side with
    | North -> seq { for x in 0 .. xend do yield arr.[x, 0] }
    | East -> seq { for y in 0 .. yend do yield arr.[xend, y] }
    | South -> seq { for x in 0 .. xend do yield arr.[x, yend] }
    | West -> seq { for y in 0 .. yend do yield arr.[0, yend] }

let getGridState (data: Coord list) x y = 
    let folder (state: GridState, dist: int option) (coord: Coord) =
        let mydist = manhattenDistance (coord.X, coord.Y) (x, y)
        match dist with
        | _ when mydist = 0 -> (Center coord.Id, Some(0))
        | None -> (PartOf coord.Id, Some(mydist))
        | Some(d) when d = mydist -> (Equidistant, Some(d))
        | Some(d) when d > mydist -> (PartOf coord.Id, Some(mydist))
        | d -> (state, d)

    data
    |> Seq.fold folder (Unknown, None)
    |> fst

let getGridStr state =
    match state with
    | Unknown -> "(?)"
    | Equidistant -> "(.)"
    | PartOf c -> sprintf "[%c]" c
    | Center c -> sprintf "(%c)" c

let computeSize (input: int seq) = (input |> Seq.max) + 1

let buildGrid (data: Coord list) =
    let x_size = data |> Seq.map (fun c -> c.X) |> computeSize
    let y_size = data |> Seq.map (fun c -> c.Y) |> computeSize

    // Compute the grid
    Array2D.init x_size y_size (getGridState data)

let getCoordId state =
    match state with
    | Unknown -> None
    | Equidistant -> None
    | PartOf c -> Some(c)
    | Center c -> Some(c)

let getEdges (grid: GridState[,]) =
    let xstart = grid |> Array2D.base1
    let ystart = grid |> Array2D.base2
    let xend = (grid |> Array2D.length1) - 1
    let yend = (grid |> Array2D.length2) - 1
    seq {
        for x in xstart..xend do
            yield getCoordId grid.[x, 0]
            yield getCoordId grid.[x, yend]
        for y in ystart..yend do
            yield getCoordId grid.[0, y]
            yield getCoordId grid.[xend, y]
    } |> Seq.choose id

let printGrid (grid: GridState[,]) =
    let xstart = grid |> Array2D.base1
    let ystart = grid |> Array2D.base2
    let xend = (grid |> Array2D.length1) - 1
    let yend = (grid |> Array2D.length2) - 1
    for y in ystart..yend do
        for x in xstart..xend do
            grid.[x,y] |> getGridStr |> printf "%s"
        printfn ""

let computeTotalDistance (data: Coord list) (x, y) =
    data
    |> Seq.map (fun coord -> manhattenDistance (coord.X, coord.Y) (x, y))
    |> Seq.sum

let run (threshold: int) (data: Coord list) = 
    data |> List.length |> printfn "Coords: %d"
    let grid = buildGrid data

    let ylen = grid |> Array2D.length2
    let xlen = grid |> Array2D.length1
    printfn "Grid size: %d x %d" xlen ylen

    // Print the grid
    //grid |> printGrid

    // Identify the areas the hit the edges
    let edges = grid |> getEdges |> Set.ofSeq
    edges
    |> Seq.map string
    |> String.concat ", "
    |> printfn "Edges: %s"

    // Compute the area for each coord
    grid
    |> seqOfArray2D
    |> Seq.map snd
    |> Seq.groupBy getCoordId
    |> Seq.choose (fun (coord, cells) ->
        match coord with
        | Some(c) when edges.Contains c -> None
        | Some(c) -> Some((c, cells |> Seq.length))
        | None -> None
    )
    |> Seq.maxBy snd
    ||> printfn "Part 1: Point %c (Size: %d)"

    // Compute Part 2 solution
    grid
    |> seqOfArray2D
    |> Seq.map fst
    |> Seq.map (computeTotalDistance data)
    |> Seq.filter (fun x -> x < threshold)
    |> Seq.length
    |> printfn "Part 2: %d"

[<EntryPoint>]
let main argv =
    argv.[0]
    |> loadLines
    |> Seq.map (split2 ',')
    |> Seq.mapi Coord.create
    |> List.ofSeq
    |> run (int argv.[1])

    0
