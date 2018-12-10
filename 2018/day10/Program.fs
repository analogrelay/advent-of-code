open System
open VibrantCode.AdventOfCode
open System.Text.RegularExpressions
open System.Threading

// We use the following heuristic to detect when the lights align to form a "message"
// 1. At each tick, compute the bounding box for all points
// 2. Compute the area of that box
// 3. If the area is smaller than the previous tick, keep going
// 4. If the area is _larger_ than the previous tick, the previous tick contains the message
//
// This is based on the idea that a message will be a text string ("HI") and will be a very
// compact set of points. The tick immediately following the one in which the message appears
// will cause the lights to drift apart and the area will start increasing again.

// Active pattern for matching using a regular expression.
// Produces a list of groups yielded by the regex, skipping the first one
let (|Regex|_|) (pattern: string) (input: string) =
    let matches = Regex.Match(input, pattern)
    if matches.Success then
        Some(matches.Groups |> Seq.map (fun g -> g.Value) |> Seq.skip 1 |> List.ofSeq)
    else
        None

[<StructuredFormatDisplay("{X},{Y}")>]
type Point(x: int, y: int) =
    member _x.X = x
    member _x.Y = y

[<StructuredFormatDisplay("{TopLeft} -> {BottomRight} ({Size})")>]
type Rect(topLeft: Point, bottomRight: Point) =
    member _x.TopLeft = topLeft
    member _x.BottomRight = bottomRight

    member _x.Size = Point(abs (bottomRight.X - topLeft.X), abs (bottomRight.Y - topLeft.Y))
    member this.Area = 
        let (x, y) = (int64 this.Size.X, int64 this.Size.Y)
        x * y

    member _x.Include (point: Point) =
        let topLeft = Point(min point.X topLeft.X, min point.Y topLeft.Y)
        let bottomRight = Point(max point.X bottomRight.X, max point.Y bottomRight.Y)
        Rect(topLeft, bottomRight)

    static member empty = Rect(Point(0, 0), Point(0, 0))
    static member fromPoint (point: Point) = Rect(point, point)

[<StructuredFormatDisplay("position=<{Position}> velocity=<{Velocity}>")>]
type Light(position: Point, velocity: Point) =
    member _x.Position = position
    member _x.Velocity = velocity

    member _x.Tick =
        // Create a new light representing a single second of motion
        let newPos = Point(position.X + velocity.X, position.Y + velocity.Y)
        Light(newPos, velocity)

    static member tryParse (line: string) =
        match line with
        | Regex "position=<([- \d]+),([- \d]+)> velocity=<([- \d]+),([- \d]+)>" [ pos_x; pos_y; vel_x; vel_y ] ->
            Some(Light(Point(int pos_x, int pos_y), Point(int vel_x, int vel_y)))
        | _ -> None

let computeBounds (lights: Light list) =
    if List.isEmpty lights then
        Rect.empty
    else
        Seq.fold (fun (rect: Rect option) (light: Light) ->
            match rect with
            | Some(r) -> Some(r.Include light.Position)
            | None -> Some(Rect.fromPoint light.Position)
        ) None lights |> Option.get

type Field(lights: Light list, bounds: Rect) =
    member _x.Lights = lights
    member _x.Bounds = bounds

    member _x.Tick =
        let folder (rect: Rect option) (light: Light) =
            let newLight = light.Tick
            match rect with
            | Some(r) -> (newLight, Some(r.Include newLight.Position))
            | None -> (newLight, Some(Rect.fromPoint newLight.Position))

        let (lights, bounds) =
            lights
            |> List.mapFold folder None

        Field(lights, bounds |> Option.get)

    member _x.LightAt x y =
        match lights |> List.tryFind (fun l -> l.Position.X = x && l.Position.Y = y) with
        | Some(l) -> true
        | None -> false

    static member create (lights: Light list) =
        Field(lights, computeBounds lights)

let shrinkField (field: Field) =
    let rec shrinkFieldWorker (second: int) (field: Field) =
        let newField = field.Tick

        if newField.Bounds.Area > field.Bounds.Area then
            // We're growing again, stop and return the previous field
            (second, field)
        else
            // The area is still shrinking, keep ticking
            shrinkFieldWorker (second + 1) newField
    shrinkFieldWorker 0 field

let run (data: Light list) = 
    let (totalSeconds, resultField) =
        Field.create data
        |> shrinkField
    printfn "Final Bounds: %A (Area: %d)" resultField.Bounds resultField.Bounds.Area

    for y in resultField.Bounds.TopLeft.Y .. resultField.Bounds.BottomRight.Y do
        for x in resultField.Bounds.TopLeft.X .. resultField.Bounds.BottomRight.X do
            if resultField.LightAt x y then
                printf "#"
            else
                printf "."
        printfn ""

    printfn "Appears at %d seconds" totalSeconds

[<EntryPoint>]
let main argv =
    argv.[0]
    |> AdventHelpers.loadLines
    |> Seq.choose Light.tryParse
    |> List.ofSeq
    |> run

    0
