module Types
open System
open System.IO

type Board<'T> = 'T[,]

type Game = { Id: string }// Unified unique ID of the game
type Position = { Y: int; X: int }
type EnemyPosition = { Y: int; X: int;Neutral:bool }

module EnemyPosition =
    let position enemy = { Y = enemy.Y; X = enemy.X }


module Distance = 
    let diff (p1:Position) (p2:Position) = p2.X - p1.X , p2.Y - p1.Y

    let isInSameLine (p1:Position) (p2:Position) = p1.X = p2.X || p1.Y = p2.Y 

    let manhattan (p1:Position) (p2:Position) = p2.X - p1.X + p2.Y - p1.Y

    let euclidien (p1:Position) (p2:Position) = sqrt ((float p2.X - float p1.X)**2. + (float p2.Y - float p1.Y)**2.)

type Area = { Y1: int ;X1: int;Y2: int;X2: int}
type Player = { // Object - player data
        Id: string // Unified unique ID of the player
        Name: string // The name of the player
        Position: Position
        Previous: Position
        Area: Area
        Fire: bool // If the player can fire this round
}

type Size = {Height: int; Width: int}
type  Board= { // Object - board data
    Size:  Size
    Walls: Position list // Array - visible walls
    Seen: Position list option
}


type Cell = 
    | Wall
    | Me
    | Other
    | Enemy of neutral:bool
    | WasSeen
    | Unknown

let shoot = exn
let visibleArea = exn
let gameLength = 0


type State = {
        Game: Game // Object - game data
        Player: Player
        Board: Board
        Players: Position list // Array - other players positions
        Enemies: EnemyPosition list // Array - enemies positions
}


type Move = 
 | Up
 | Down
 | Left
 | Right
 | ``Fire-up``
 | ``Fire-down``
 | ``Fire-right``
 | ``Fire-left``

module Move = 
    let random() =
      printfn "Do random !"
      [ Up ; Down ; Left ; Right] |> List.minBy (fun _ -> Guid.NewGuid())



module Matrix =

    let createMatrix width height = 
        Array2D.init height width (fun _ __ -> Cell.Unknown)

    // return unit since mutable
    let setMe (position:Position) (board:Board<Cell>) = 
        board.[position.Y, position.X] <- Cell.Me

    let setWalls (positions:Position list) (board:Board<Cell>) = 
        for position in positions do
            board.[position.Y, position.X] <- Cell.Wall 

    let setEnemies (enemies:EnemyPosition list) (board:Board<Cell>) = 
        for enemy in enemies do
            board.[enemy.Y, enemy.X] <- Enemy(enemy.Neutral)

    let setOther (players:Position list) (board:Board<Cell>) = 
        for position  in players do
            board.[position.Y, position.X] <- Other

    let setView (area:Area) (board:Board<Cell>) = 
        for x in [area.X1 .. area.X2 ] do
            for y in [area.Y1 .. area.Y2 ] do
              match board.[y, x] with
              | Unknown -> board.[y, x] <- WasSeen
              | _ -> ignore()

    let printCell cell = 
        let s = match cell with
                | Wall -> "🧱"
                | Me -> "🦇"
                | Other -> "👹"
                | Enemy neutral when neutral -> "🍈"
                | Enemy _ -> "🎃"
                | WasSeen _ -> "⬛"
                | Unknown -> "⬜"
        printf "%s" s

    let print (board:Board<Cell>) =
        for y = 0 to Array2D.length1 board - 1 do
            printfn ""
            for x = 0 to Array2D.length2 board - 1 do
                board.[y,x] |> printCell

        printfn ""

module State = 
    
    let createMatrix state = 

        let matrix = Matrix.createMatrix state.Board.Size.Width state.Board.Size.Height
        matrix |> Matrix.setView state.Player.Area
        matrix |> Matrix.setMe state.Player.Position
        matrix |> Matrix.setWalls state.Board.Walls
        matrix |> Matrix.setEnemies state.Enemies
        matrix |> Matrix.setOther state.Players
        matrix |> Matrix.setView state.Player.Area
        matrix

    let printMatrix state = 
        state |> createMatrix |> Matrix.print
    
    let mergeWith prev current =
        { prev with 
            Game = current.Game
            Player = current.Player
            Board = { prev.Board with Walls = ((prev.Board.Walls @ current.Board.Walls) |> List.distinct)}
            Players = current.Players
            Enemies = current.Enemies
            }

    let neutrals state = state.Enemies |> List.filter (fun x->x.Neutral)
    let enemiesTokill state = state.Enemies |> List.filter (fun x-> not x.Neutral)

    let save getJson state = 
        let json = state |> getJson
        File.WriteAllText ((sprintf "./%s.json" state.Game.Id),json)

    let load readJson id =
        try
            let json = File.ReadAllText (sprintf "./%s.json" id)
            let state = json |> readJson
            Some state
        with ex -> 
            None
 
module Agent = 

    let getShoot p e = 
        let (x, y ) = Distance.diff p e

        if x > 0 then Some Move.``Fire-right``
        elif x < 0 then Some Move.``Fire-left``
        elif y > 0 then Some Move.``Fire-down``
        elif y < 0 then Some Move.``Fire-up``
        else None

    let stepTo p e = 
        let (x, y ) = Distance.diff p e

        if x > 0 then Some Move.Right
        elif x < 0 then Some Move.Left
        elif y > 0 then Some Move.Down
        elif y < 0 then Some Move.Up
        else None

    let neighbours state (p:Position) =
        let found = 
             [
                 {p with X = p.X + 1} ; {p with X = p.X - 1};
                 {p with Y = p.Y + 1} ; {p with Y = p.Y - 1}]

        found |> Seq.filter (fun p -> 
            p.X > 0 && p.Y > 0 &&
            p.X < state.Board.Size.Width && p.Y < state.Board.Size.Height &&
            not <| List.contains p state.Board.Walls ) 

    let gScore _ _ = 1.

    let fScore (p1) (p2) = Distance.euclidien p1 p2
       
    let findPath state (destinations:Position list) = 

        let aStarConf:AStar.Config<Position> =  { neighbours = (neighbours state ) ; gCost = gScore; fCost = fScore; maxIterations = None }

        let paths = destinations
                    |> Array.ofList
                    |> Array.map (fun dest -> 
                        AStar.search state.Player.Position dest aStarConf )
                    |> Array.choose id
                    |> Array.map (Seq.toArray >> Array.rev) 
        paths
        
    let findNearestNeutral state = 

        let neutralsPositions = state |> State.neutrals |> List.map (EnemyPosition.position)

        let paths = findPath state neutralsPositions
                    
        if paths.Length = 0 
        then None
        else 
            let bestPath = paths |>  Array.minBy (fun p -> p |> Array.length) 
            printfn "GOTO NEUTRAL ! %A" bestPath.[1]
            let move = stepTo state.Player.Position bestPath.[1]
            move
            
    let findNearestEnemy (state:State) =  
        
        //let enemiesPositions = state |> State.enemiesTokill |> List.map (EnemyPosition.position)
        let enemiesPositions = state.Enemies |> List.map (EnemyPosition.position)

        // shot if same line
        let shouldShot = enemiesPositions 
                            |> List.filter (fun e -> Distance.isInSameLine state.Player.Position e  )
                            |> List.sortBy (fun e -> Distance.manhattan state.Player.Position e)
                            

        match shouldShot with
        |  h :: t -> 
            printfn "Shoot enemy %A" h
            getShoot state.Player.Position h
        | _ -> 
            let paths = findPath state enemiesPositions
            if paths.Length = 0 
            then 
                printfn "NO ENEMY TO Shoot"
                None
            else
                printfn "NO ENEMY TO Shoot"
                let best = paths |> Array.minBy (fun p -> p |> Array.length) |> fun x->x.[1]
                printfn "Go to enemy %A" best
                let move = stepTo state.Player.Position best
                
                move



    let tryFireEnemy (state:State) = 
        if state.Player.Fire
        then findNearestEnemy state
        else None
    
    
    
    let visitMaze (state:State) = 
        printfn "VISIT MAZE"

        let goto =    [ for x in 0..state.Board.Size.Width -> {X = x; Y = 0} ] @
                      [ for x in 0..state.Board.Size.Width -> {X = x; Y = state.Board.Size.Height - 1} ] @
                      [ for y in 0..state.Board.Size.Height -> {X = 0; Y = y} ] @
                      [ for y in 0..state.Board.Size.Height -> {X = state.Board.Size.Width-1; Y = y} ]
                      |> List.tryFind (fun p -> state.Board.Walls |> List.contains p |> not)

        let aStarConf:AStar.Config<Position> =  { neighbours = (neighbours state ) ; gCost = gScore; fCost = fScore; maxIterations = None }

        match goto with 
        | None -> None
        | Some x -> 
            let paths = findPath state [x] 
            if paths.Length = 0 
            then 
                None
            else
                let best = paths |> Array.minBy (fun p -> p |> Array.length) |> fun x->x.[1]
                printfn "Go to unknown %A" best
                let move = stepTo state.Player.Position best
                
                move
        
        // let neighbours = neighbours state state.Player.Position
        // // dummy 
        // neighbours |> Seq.tryHead
        // |> Option.bind (fun x -> stepTo state.Player.Position x)



    let getMove state =
        // todo use optionnal comp expression
        tryFireEnemy state
        |> Option.orElseWith (fun () ->findNearestNeutral state)
        |> Option.orElseWith (fun () -> visitMaze state)
        |> Option.defaultWith (Move.random)
        



// flow
// |> Move
// |> Shot
// |> MoveEnemy
