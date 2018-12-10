open System
open VibrantCode.AdventOfCode.AdventHelpers

type RingNode<'T> = { mutable prev: RingNode<'T> option; mutable next: RingNode<'T> option; data: 'T } with
    member this.Prev = Option.get this.prev
    member this.Next = Option.get this.next
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

    static member create item =
        let newNode = { prev = None; next = None; data = item }
        newNode.prev <- Some(newNode)
        newNode.next <- Some(newNode)
        newNode

let insert (index: int) item list =
    if index = (List.length list) then
        list @ [item]
    else
        // Split the list at the index
        let (left, right) = List.splitAt index list

        // Stich the list back together
        left @ (item :: right)

let remove (index: int) list =
    match List.splitAt index list with
    | (left, removed :: right) -> (removed, left @ right)
    | (left, []) -> raise (new Exception("Attempted to remove item that does not exist!"))

type Board(marbles: int list, currentIndex: int) =
    member _x.Marbles = marbles
    member _x.CurrentIndex = currentIndex

    member _x.PlaceMarble (marble: int) =
        // Check if this is the next scoring marble
        if (marble % 23) = 0 then
            // Remove the marble 7 places counter-clockwise
            let removeOffset = (currentIndex - 7) % marbles.Length |> abs
            let (removed, marbles) = marbles |> remove removeOffset
            (removed + marble, Board(marbles, removeOffset))
        else
            // Figure out where to place the marble
            match marbles with
            | [ one ] -> (0, Board([ one; marble ], 1))
            | x ->
                // Compute the offset for the new marble
                let offset =
                    match (currentIndex + 2) % marbles.Length with
                    // If the destination is offset '0', actually insert it at the end
                    | 0 -> marbles.Length
                    | x -> x

                // Insert the new marble at that offset and return a new board
                (0, Board(marbles |> insert offset marble, offset))

    member _x.Render (player: int option) =
        match player with
        | None -> printf "[-] "
        | Some(p) -> printf "[%d] " p

        for (idx, marble) in (List.indexed marbles) do
            if idx = currentIndex then
                printf "(%2d)" marble
            else
                printf " %2d " marble
        printfn ""   

    member this.PlayRound (player: int) (scores: int array) (marble: int) (totalMarbles: int) =
        if marble > totalMarbles then
            ()
        else
            // Place the marble
            let (score, board) = this.PlaceMarble marble

            // Update score
            if score > 0 then
                printfn "[Marble: %05d/%05d] Player %d scores: %d" marble totalMarbles player score
            let newScore = scores.[player] + score
            scores.[player] <- newScore

            // Render the board
            //board.Render (Some(player))

            // Play the next marble
            board.PlayRound ((player + 1) % scores.Length) scores (marble + 1) totalMarbles

    static member create = Board([0], 0)

let run (players: int) (marbles: int) = 
    let board = Board.create

    let scores = Array.zeroCreate players

    //board.Render None
    board.PlayRound 0 scores 1 marbles

    scores |> Array.max |> printfn "Part 1: %d"

[<EntryPoint>]
let main argv =
    let players = argv.[1] |> int
    let marbles = argv.[2] |> int

    run players marbles

    0
