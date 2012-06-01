type 'a Node = 'a * 'a list
type 'a Graph = 'a Node list
let g = [('a', ['b'; 'd']); ('b', ['a'; 'c'; 'd']); ('c', ['b']); ('d', ['a'; 'b']); ('e', ['f']); ('f', ['e'])]

let findConnectedGraph (map : Map<'a, 'a list>) (firstVal : 'a) =
  let rec walk (seen : 'a list) (tosearch : 'a list) =
    let isSeen n =
      seen |> List.exists (fun i -> i=n)
    let notSeenOnly (nodes : 'a list) =
      nodes |> List.filter (fun i -> not <| isSeen i)

    let notSeenNeighbors = tosearch |> List.collect (fun i -> map.[i] |> notSeenOnly)
    match tosearch with
      | [] -> seen
      | _ -> walk (seen @ notSeenNeighbors) notSeenNeighbors

  walk [firstVal] [firstVal]

let findAllConnectedGraphs (g : 'a Graph) =
  let map = Map.ofList g
  let rec findAllConnectedGraphsWorker (availableNodes : 'a list) = [
    let graph = findConnectedGraph map (availableNodes |> List.head)
    yield graph
    let remainingNodes = (Set.ofList availableNodes) - (Set.ofList graph)
    if not (Set.isEmpty remainingNodes) then
      yield! findAllConnectedGraphsWorker (Set.toList remainingNodes)
  ]
    
  findAllConnectedGraphsWorker (g |> List.map (fun i -> fst i))

let result = findAllConnectedGraphs g
printfn "RESULT: %A" result
