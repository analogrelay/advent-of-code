open System
open VibrantCode.AdventOfCode.AdventHelpers

type RingNode<'T> = { mutable prev: RingNode<'T> option; mutable next: RingNode<'T> option; data: 'T } with
    member this.Prev = Option.get this.prev
    member this.Next = Option.get this.next
    member this.Value = this.data
    member this.Enumerate =
        seq {
            yield this.Value
            let mutable nod = this.Next
            while not (LanguagePrimitives.PhysicalEquality nod this) do
                yield nod.Value
                nod <- nod.Next
        }

    member this.Seek index =
        match index with
        | 0 -> this
        | x when x > 0 -> this.Next.Seek (index - 1)

        // We know that the only remaining case is x<0 but F# doesn't so we don't put a when clause in
        | _ -> this.Prev.Seek (index + 1)

    member this.Insert item =
        let newNode = { prev = Some(this); next = this.next; data = item }
        newNode.Next.prev <- Some(newNode)
        this.next <- Some(newNode)
        newNode
    member this.Detach =
        this.Prev.next <- this.next
        this.Next.prev <- this.prev

    static member create (item: 'T) =
        let newNode = { prev = None; next = None; data = item }
        newNode.prev <- Some(newNode)
        newNode.next <- Some(newNode)
        newNode

type Board(head: RingNode<int>, currentMarble: RingNode<int>) =
    member _x.PlaceMarble (marble: int) =
        // Check if this is the next scoring marble
        if (marble % 23) = 0 then
            // Find the marble 7 spaces counter-clockwise
            let toRemove = currentMarble.Seek -7
            let next = toRemove.Next

            // Detach it and score it
            toRemove.Detach
            let result = int64(marble) + int64(toRemove.Value)
            (result, Board(head, next))
        else
            // Seek forward 1 and then insert the marble
            let dest = currentMarble.Seek 1
            let newCurrent = dest.Insert marble
            (int64(0), Board(head, newCurrent))

    member _x.Render (player: int option) =
        match player with
        | None -> printf "[-] "
        | Some(p) -> printf "[%d] " p

        for marble in head.Enumerate do
            if marble = currentMarble.Value then
                printf "(%2d)" marble
            else
                printf " %2d " marble
        printfn ""   

    member this.PlayRound (player: int) (scores: int64 array) (marble: int) (totalMarbles: int) =
        if marble > totalMarbles then
            ()
        else
            // Place the marble
            let (score, board) = this.PlaceMarble marble

            // Update score
            //if score > 0 then
            //    printfn "[Marble: %05d/%05d] Player %d scores: %d" marble totalMarbles player score
            let newScore = scores.[player] + score
            scores.[player] <- newScore

            // Render the board
            //board.Render (Some(player))

            // Play the next marble
            board.PlayRound ((player + 1) % scores.Length) scores (marble + 1) totalMarbles

    static member create =
        let node = RingNode.create 0
        Board(node, node)

let run (part: int) (players: int) (marbles: int) = 
    let board = Board.create

    let scores = Array.zeroCreate players

    //board.Render None
    board.PlayRound 0 scores 1 marbles

    printfn "Part %d: %d" part (Array.max scores)

[<EntryPoint>]
let main argv =
    let players = argv.[1] |> int
    let marbles = argv.[2] |> int

    run 1 players marbles
    run 2 players (marbles * 100)

    0
