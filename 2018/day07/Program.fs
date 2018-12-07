open VibrantCode.AdventOfCode
open System.Text.RegularExpressions
open System
open System.Threading

let matcher = new Regex("Step (?<dependency>[A-Z]) must be finished before step (?<dependent>[A-Z]) can begin.")

type LogLine(dependency: char, dependent: char) =
    member _x.Dependency = dependency
    member _x.Dependent = dependent
    static member tryParse (inp: string) =
        let result = matcher.Match inp
        if result.Success then
            Some(LogLine(result.Groups.Item("dependency").Value.Chars(0), result.Groups.Item("dependent").Value.Chars(0)))
        else
            None

type GraphNode(name: char, workRemaining: int, dependencies: Set<char>) =
    member _x.Name = name
    member _x.Dependencies = dependencies
    member _x.WorkRemaining = workRemaining
    member _x.Complete = workRemaining = 0

    member _x.workOn =
        GraphNode(name, workRemaining - 1, dependencies)
    member _x.withDependency (newDependency: char) =
        GraphNode(name, workRemaining, dependencies |> Set.add newDependency)
    member _x.withoutDependency (removedDependency: char) =
        GraphNode(name, workRemaining, dependencies |> Set.remove removedDependency)
    static member create baseDuration name =
        GraphNode(name, baseDuration + ((int name) - (int 'A')) + 1, Set.empty)

let getOrCreate baseDuration (name: char) (nodes: Map<char, GraphNode>) =
    match (nodes |> Map.tryFind name) with
    | Some(n) -> n
    | None -> GraphNode.create baseDuration name

type Graph(nodes: Map<char, GraphNode>) =
    member _x.Nodes = nodes |> Map.toSeq |> Seq.map snd
    member _x.withEdge baseDuration (dependent: char) (dependency: char) =
        let dependent = nodes |> getOrCreate baseDuration dependent |> (fun g -> g.withDependency dependency)
        let dependency = nodes |> getOrCreate baseDuration dependency

        Graph(nodes |> Map.add dependent.Name dependent |> Map.add dependency.Name dependency)

    /// <summary>Removes the next available task from the graph</summary>
    member x.nextTask =
        let nextTaskName =
            nodes
            |> Map.filter (fun _ n -> Seq.isEmpty n.Dependencies)
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.sortBy id
            |> Seq.tryHead

        match nextTaskName with
        | Some(k) -> (Some(nodes |> Map.find k), Graph(nodes |> Map.remove k))
        | None -> (None, x)

    member _x.completeTask (name: char) =
        let newNodes =
            nodes
            |> Map.map (fun _ node -> node.withoutDependency name)
        Graph(newNodes)
            
    static member empty = Graph(Map.empty)
    static member build baseDuration (logs: LogLine seq) =
        logs
        |> Seq.fold (fun (graph: Graph) line -> (graph.withEdge baseDuration line.Dependent line.Dependency)) (Graph.empty)

type Worker(id: int, task: GraphNode option) =
    member _x.Id = id
    member _x.Task = task
    member _x.WorkRemaining =
        task
        |> Option.map (fun x -> x.WorkRemaining)
        |> Option.defaultValue 0
    member _x.assign task = Worker(id, Some(task))
    member _x.idle = Worker(id, None)
    member _x.doWork =
        match task with
        | Some(t) -> Worker(id, Some(t.workOn))
        | None -> Worker(id, None)

    static member create id = Worker(id, None)

let executeGraph (graph: Graph) =
    let rec worker (revOrder: char list) (graph: Graph) =
        match graph.nextTask with
        | (Some(t), g) -> worker (t.Name :: revOrder) (g.completeTask t.Name)
        | (None, _) -> revOrder |> List.rev
    worker [] graph

type State(second: int, workers: Worker list, completed: char list, graph: Graph) =
    member _x.Second = second
    member _x.Workers = workers
    member _x.Completed = completed
    member _x.Graph = graph

    member _x.Finished =
        (workers |> List.forall (fun w -> w.WorkRemaining = 0))
        && (graph.Nodes |> Seq.isEmpty)

    member _x.doWork (worker: Worker) =
        // Check for completed tasks or idle workers
        let (newWorker, newCompleted, newGraph) =
            match worker.Task with
            // Not assigned, or complete? Get a new task.
            | Some(completedTask) when completedTask.WorkRemaining = 0 ->
                // Remove the completed task from the graph
                let newGraph = graph.completeTask completedTask.Name

                // Try to get a new task
                match newGraph.nextTask with
                | (Some(next), g) -> (worker.assign next, completedTask.Name :: completed, g)
                | (None, g) -> (worker.idle, completedTask.Name :: completed, g)
            | None ->
                // Try to get a new task
                match graph.nextTask with
                | (Some(next), g) -> (worker.assign next, completed, g)
                | (None, g) -> (worker.idle, completed, g)
            // If we're assigned, nothing to update
            | Some(t) -> (worker, completed, graph)

        match newWorker.Task with
        | Some(task) ->
            // If the worker is assigned, do some work on the task
            State(second, workers @ [newWorker.doWork], newCompleted, newGraph)
        | None ->
            // The worker is just idle for this tick
            State(second, workers @ [newWorker], newCompleted, newGraph)

    member x.tick =
        let rec tickWorkers (state: State) (workers: Worker list) =
            match workers with
            | w :: ws -> tickWorkers (state.doWork w) ws
            | [] -> state

        let initialState = State(second + 1, [], completed, graph)
        tickWorkers initialState workers

    static member create workers graph =
        let workers =
            seq { 1..workers }
            |> Seq.map Worker.create
            |> List.ofSeq

        State(-1, workers, [], graph)

let renderState (state: State) =
    printf " % 4d " state.Second

    for worker in state.Workers do
        match worker.Task with
        | Some(t) -> printf " %c(%02d) " t.Name t.WorkRemaining
        | None -> printf " ..... "

    state.Completed
    |> Seq.rev
    |> Seq.map string
    |> String.concat ""
    |> printfn " %s" 

let executeParallel workers (graph: Graph) =
    // Create the initial state
    let initialState = State.create workers graph

    let rec worker (revStates: State list) =
        match revStates with
        | [] -> raise (new Exception("State list should never be empty!"))
        | state :: rest when state.Finished -> revStates
        | state :: rest ->
            let nextState = state.tick

            // Comment this out for faster run time!
            renderState nextState

            // Uncomment this to sloowww things down a bit
            //Thread.Sleep 50
            worker (nextState :: state :: rest)

    worker [initialState]

let run workers (data: Graph) = 
    // Render the graph
    data.Nodes
    |> Seq.iter (fun node ->
        let deps = (node.Dependencies |> Set.toSeq |> Seq.map string |> String.concat ",")
        printfn "%c (%d) -> %s" node.Name node.WorkRemaining deps)

    // Execute the graph in sequence and get the sequence order
    data
    |> executeGraph
    |> Seq.map string
    |> String.concat ""
    |> printfn "Part 1: %s"

    printf "    T "
    for x in 1..workers do
        printf " %d     " x
    printfn " Completed"

    let finalState =
        data
        |> executeParallel workers
        |> List.head

    printfn "Part 2: %d" finalState.Second

[<EntryPoint>]
let main argv =
    let baseDuration = int argv.[1]
    let workers = int argv.[2]

    argv.[0]
    |> AdventHelpers.loadLines
    |> Seq.choose LogLine.tryParse
    |> Graph.build baseDuration
    |> run workers

    0
