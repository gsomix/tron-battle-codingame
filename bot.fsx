namespace TronBattle

open System.Collections.Generic

module Map =
    let choose (chooser : 'T -> 'b -> 'c option) (source : Map<'T,'b>) =
        (Map.empty, source) ||> Map.fold (fun state key value ->
            let result = chooser key value
            if Option.isSome result
            then Map.add key result.Value state
            else state
        )

module Bot =

    type Position = int*int
    let Width = 30
    let Height = 20
    let EmptyTile = -1
    let MaxDistance = Width * Height + 1
    
    let dist (x0, y0) (x1, y1) = abs(x0 - x1) + abs(y0 - y1)
    
    let inbounds (x, y) = x >= 0 && x < Width && y >= 0 && y < Height
        
    let applyMove (tiles: int[,]) (playersPositions: Position[]) botInd (dx, dy) =
        let x, y = playersPositions.[botInd]
        let x', y' = x + dx, y + dy

        if inbounds(x', y') && tiles.[x', y'] = EmptyTile
        then
            let grid' = Array2D.copy tiles
            let playersPositions' = Array.copy playersPositions
            grid'.[x', y'] <- botInd
            playersPositions'.[botInd] <- x', y'
            Some (grid', playersPositions')
        else None
        
    let distanceMap (tiles: int[,]) (startPosition: Position)  =
        let queue = Queue<_>(600)
        let map = Array2D.create Width Height MaxDistance
        let visited = Array2D.create Width Height false

        if not (inbounds startPosition)
        then map
        else        
            let expand (x, y) =
                [ for (dx, dy) in [ (1, 0); (0, 1); (-1, 0); (0, -1) ] do
                    let x', y' = x + dx, y + dy
                    if inbounds(x', y') && tiles.[x', y'] = EmptyTile && not (visited.[x', y'])
                    then
                        visited.[x', y'] <- true
                        yield (x', y')
                ]
            
            queue.Enqueue(startPosition, 0)
            while queue.Count > 0 do
                let (x, y), dist = queue.Dequeue()
                map.[x, y] <- dist               
                for nextPos in expand(x, y) do
                    queue.Enqueue(nextPos, dist + 1)
            map
    
    let voronoiMap (tiles: int[,]) (playersPositions: Position[]) =
        let map = Array2D.create Width Height EmptyTile
        let distanceMaps = playersPositions |> Array.map (distanceMap tiles)
        
        for y = 0 to Height - 1 do
            for x = 0 to Width - 1 do
                let closestPlayerInd, distance =
                    distanceMaps
                    |> Array.indexed
                    |> Array.minBy (fun (_, distanceMap) -> distanceMap.[x, y])
                    |> fun (ind, map) -> ind, map.[x, y] 
                if distance < MaxDistance
                then map.[x, y] <- closestPlayerInd
        map
        
    let score (tiles: int[,]) (playersPositions: Position[]) botInd =
        let botPosition = playersPositions.[botInd]
        let distanceMap = distanceMap tiles botPosition
        let voronoiMap = voronoiMap tiles playersPositions 

        let mutable score = 0
        for y = 0 to Height - 1 do
            for x = 0 to Width - 1 do
                if voronoiMap.[x, y] = botInd
                then score <- score + distanceMap.[x, y]
        score

    let run() = 
        let readLine() = stdin.ReadLine()
        let readIntegers() = readLine().Split() |> Array.map int

        let tiles = Array2D.create Width Height EmptyTile
        let moves = Map.ofList [ "RIGHT", (1, 0); "DOWN", (0, 1); "LEFT", (-1, 0); "UP", (0, -1) ]
        
        while true do
            let [| playersCount; botInd |] = readIntegers()
            let playersPositions = Array.init playersCount (fun ind ->
                match readIntegers() with
                | [| -1; -1; x; y |] ->
                    tiles |> Array2D.iteri(fun x y tile -> if tile = ind then tiles.[x, y] <- EmptyTile)
                    x, y
                | [| x0; y0; x; y |] ->
                    tiles.[x0, y0] <- ind
                    tiles.[x, y] <- ind
                    x, y
                | _ -> failwith "Impossible"
            )
            
            let scores =
                moves
                |> Map.choose (fun _ move -> applyMove tiles playersPositions botInd move)
                |> Map.map (fun _ (futureTiles, playersPositions) -> score futureTiles playersPositions botInd)
                
            let command =
                scores
                |> Map.toList
                |> List.maxBy snd
                |> fst
            
            printfn "%s" command
            ()


module EntryPoint = do Bot.run()
