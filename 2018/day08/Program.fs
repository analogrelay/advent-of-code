open System
open VibrantCode.AdventOfCode.AdventHelpers

let generateName index = index |> string

type TreeNode(name: string, children: TreeNode list, metadata: int list) =
    member _x.Name = name
    member _x.Children = children
    member _x.Metadata = metadata

    member _x.TotalMetadata =
        let myMetadata = metadata |> List.sum
        let childMetdata = children |> List.sumBy (fun x -> x.TotalMetadata)
        myMetadata + childMetdata

    member _x.Score =
        if List.isEmpty children then
            metadata |> List.sum
        else
            metadata
            |> List.filter ((<>) 0)
            |> List.map (fun idx -> idx - 1)
            |> List.choose (fun idx -> children |> List.tryItem idx)
            |> List.sumBy (fun child -> child.Score)


let rec buildTree (counter: int) (values: int list) =
    match values with
    | numChildren :: numMetadata :: rest -> 
        let name = generateName counter

        // Read the children
        let (children, (values, counter)) =
            seq { 1..numChildren }
            |> Seq.mapFold (fun (vals, counter) _ -> buildTree (counter + 1) vals) (rest, counter)

        // Read metadata values
        let (metadata, values) =
            values
            |> List.splitAt numMetadata

        // Return the resulting node and the remainder of the list
        let newNode = TreeNode(name, List.ofSeq children, metadata)
        (newNode, (values, counter))
    | _ -> raise (new Exception("Invalid tree!"))

let rec formatTree (depth: int) (node: TreeNode) =
    let indent = (new String(' ', depth * 2))
    let metadata = node.Metadata |> Seq.map string |> String.concat ","
    printfn "%s(%s) [Metadata: %s]" indent node.Name metadata

    node.Children
    |> Seq.iter (formatTree (depth + 1))

let run (root: TreeNode) = 
    // Uncomment to dump the tree
    // root |> formatTree 0

    root.TotalMetadata
    |> printfn "Part 1: %d"

    root.Score
    |> printfn "Part 2: %d"

[<EntryPoint>]
let main argv =
    let (tree, (rest, _)) =
        argv.[0]
        |> loadSpaceDelimited
        |> Seq.map int
        |> List.ofSeq
        |> buildTree 0

    if rest |> List.isEmpty |> not then
        raise (new Exception("Invalid tree!"))

    tree
    |> run

    0
