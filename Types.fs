module CodeChallenge.Types
open System
open System.IO
open Serilog
open System.Text


type Game = { Id: string }// Unified unique ID of the game
type Position = { Y: int; X: int }
type EnemyPosition = { Y: int; X: int;Neutral:bool }

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
}


type State = {
        Game: Game // Object - game data
        Player: Player
        Board: Board
        Players: Position list // Array - other players positions
        Enemies: EnemyPosition list // Array - enemies positions
}



type Cell = 
    | Wall
    | Me
    | Other
    | Enemy of neutral:bool
    | WasSeen
    | Unknown

type Matrix = Cell[,]


type Move = 
 | Up
 | Down
 | Left
 | Right
 | ``Fire-up``
 | ``Fire-down``
 | ``Fire-right``
 | ``Fire-left``
 | Idle

type Line = | Row | Column

module EnemyPosition =
    let position enemy = { Y = enemy.Y; X = enemy.X }


module Distance = 
    let diff (p1:Position) (p2:Position) = abs(p2.X - p1.X) , abs(p2.Y - p1.Y)

    let isInSameLine (p1:Position) (p2:Position) = 
        if p1.X = p2.X  then Some Column
        elif p1.Y = p2.Y then Some Row
        else None

    let manhattan (p1:Position) (p2:Position) = abs(p2.X - p1.X) + abs(p2.Y - p1.Y)

    let euclidien (p1:Position) (p2:Position) = sqrt ((float p2.X - float p1.X)**2. + (float p2.Y - float p1.Y)**2.)


/// bind
let inline (>>=) x f = Option.bind f x

/// orElseWith
let inline (<|>) x f = Option.orElseWith f x



[<AutoOpen>]
module Utils = 
    
    let getShoot (p:Position) (e:Position) = 
        
        if p.X < e.X then Some Move.``Fire-right``
        elif p.X > e.X then Some Move.``Fire-left``
        elif p.Y < e.Y then Some Move.``Fire-down``
        elif p.Y > e.Y then Some Move.``Fire-up``
        else None

    let stepTopDown (p:Position) (e:Position) = 
        if p.Y < e.Y then Some Move.Down
        elif p.Y > e.Y then Some Move.Up
        else None

    let stepLeftRight (p:Position) (e:Position) = 
        if p.X < e.X then Some Move.Right
        elif p.X > e.X then Some Move.Left
        else None


    let stepTo (p:Position) (e:Position) = stepLeftRight p e |> Option.orElseWith (fun() -> stepTopDown p e)


[<AutoOpen>]
module Matrix =

    let createMatrix width height : Matrix = 
        Array2D.init height width (fun _ __ -> Cell.Unknown)

    // return unit since mutable
    let setMe (position:Position) (matrix:Matrix) = 
        matrix.[position.Y, position.X] <- Cell.Me

    let setWalls (positions:Position list) (matrix:Matrix) = 
        for position in positions do
            matrix.[position.Y, position.X] <- Cell.Wall 

    let setEnemies (enemies:EnemyPosition list) (matrix:Matrix) = 
        for enemy in enemies do
            matrix.[enemy.Y, enemy.X] <- Enemy(enemy.Neutral)

    let setOther (players:Position list) (matrix:Matrix) = 
        for position  in players do
            matrix.[position.Y, position.X] <- Other

    let setView (area:Area) (matrix:Matrix) = 
        for x in [area.X1 .. area.X2 ] do
            for y in [area.Y1 .. area.Y2 ] do
              match matrix.[y, x] with
              | Unknown -> matrix.[y, x] <- WasSeen
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
        sprintf "%s" s  

    open System.Collections.Generic
    open System.Runtime.CompilerServices
    [<Extension>]
    type MatrixExtension =
            [<Extension>]
            static member inline Print(matrix:Matrix) =
                let builder = StringBuilder() 
                let append format = Printf.bprintf builder format
                for y = 0 to Array2D.length1 matrix - 1 do
                    append  "%s" System.Environment.NewLine
                    for x = 0 to Array2D.length2 matrix - 1 do
                        append "%s" (matrix.[y,x] |> printCell)

                append  "%s" System.Environment.NewLine
                builder.ToString()
    

[<AutoOpen>]
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
    
    let mergeWith prev current =
        { prev with 
            Game = current.Game
            Player = current.Player
            Board = { prev.Board with Walls = ((prev.Board.Walls @ current.Board.Walls) |> List.distinct)}
            Players = current.Players
            Enemies = current.Enemies
            }
    
    let save  getJson state = 
        let json = state |> getJson
        File.WriteAllText ((sprintf "./%s.json" state.Game.Id),json)

    let load readJson id =
        try
            let json = File.ReadAllText (sprintf "./%s.json" id)
            let state = json |> readJson
            Some state
        with ex -> 
            None
    
    
    let isValidPosition state (p:Position) = 
        p.X >= 0 && p.Y >= 0 &&
        p.X < state.Board.Size.Width && p.Y < state.Board.Size.Height &&
        not <| List.contains p state.Board.Walls 

   

    let hasWallInLines state p1 p2 = 
        let wallInLine = 
            match Distance.isInSameLine p1 p2 with
            | Some Row -> state.Board.Walls |> List.exists (fun x-> x.Y = p1.Y && x.X > (min p1.X p2.X)  && x.X < (max p1.X p2.X) )
            | Some Column -> state.Board.Walls |> List.exists (fun x-> x.X = p1.X && x.Y > (min p1.Y p2.Y)  && x.Y < (max p1.Y p2.Y))
            | None -> false

        wallInLine

    let viewSize state ={ Width =  (state.Player.Area.X2 - state.Player.Area.X1) ; Height = (state.Player.Area.Y2 - state.Player.Area.Y1) }


    let getCross state (p:Position) = 
        let crossSize = viewSize state |> fun a-> {a with Height = a.Height / 2 ; Width = a.Width / 2}

        // dont take if wall
        if hasWallInLines state p { p with X = p.X - crossSize.Width + 1} |> not
        then [ for i in 2 .. crossSize.Width -> { p with X = p.X - i}  ] //left
        else []
        @
        if hasWallInLines state p { p with X = p.X + crossSize.Width - 1} |> not
        then [ for i in 2 .. crossSize.Width -> { p with X = p.X + i}  ] // right
        else []
        @
        if hasWallInLines state p { p with Y = p.Y + crossSize.Height - 1} |> not
        then [ for i in 2 .. crossSize.Height -> { p with Y = p.Y + i}  ]
        else []
        @
        if hasWallInLines state p { p with Y = p.Y - crossSize.Height + 1} |> not
        then [ for i in 2 .. crossSize.Height -> { p with Y = p.Y - i}  ]
        else []
        |> List.filter (isValidPosition state )
    
    let gScore _ _ = 1.

    let fScore (p1) (p2) = Distance.euclidien p1 p2
    
    let left (p:Position) = { p with X = p.X - 1}
    let right (p:Position) = { p with X = p.X + 1}
    let top (p:Position) = { p with Y = p.Y - 1}
    let bottom (p:Position) = { p with Y = p.Y + 1}
    
    let neighbours state (p:Position) =
            [ top p ; right p; bottom p; left p ]
            |> Seq.filter (isValidPosition state ) 



    type IState = 
        abstract CanFire : bool
        abstract Matrix :Matrix
        abstract NeutralEnemies : Position list
        abstract AggressiveEnemies : Position list
        abstract Players : Position list
        abstract AllEnemies : Position list
        abstract Walls : Position list
        abstract Position : Position 
        abstract IsValidPosition : Position -> bool
        abstract IsSafePosition : Position -> bool
        abstract IsValidAndSafePosition : Position -> bool
        abstract HasWallInLines : Position -> Position -> bool
        abstract GetCross : Position -> Position list
        abstract GetNeighbours : Position -> seq<Position>
        abstract FindPath: Position -> Position[] option
        abstract StepTo: Position -> Move option
        abstract StepLeftRight: Position -> Move option
        abstract StepTopDown: Position -> Move option
        abstract BoardSize: Size
        abstract DistanceFrom : Position -> int
        abstract IsOnLeft : Position -> bool
        abstract IsOnRight : Position -> bool
        abstract IsOnTop : Position -> bool
        abstract IsOnBottom : Position -> bool

        abstract TryGoLeft : Move option
        abstract TryGoRight : Move option
        abstract TryGoUp : Move option
        abstract TryGoDown : Move option

        abstract Debug: (string *  obj[]) -> unit
        abstract Info: (string *  obj[]) -> unit

        abstract IsPlayer : Position -> bool
        abstract IsAggressiveEnemy : Position -> bool
        abstract IsNeutral : Position -> bool



    let fn state (logger:ILogger) = 
        let neutral, aggressive = state.Enemies |> List.partition (fun x -> x.Neutral)
        { new IState with 
            member this.CanFire = state.Player.Fire
            member this.Matrix = createMatrix state
            member this.NeutralEnemies = neutral |> List.map EnemyPosition.position
            member this.AggressiveEnemies = aggressive |> List.map EnemyPosition.position
            member this.Players = state.Players
            member this.AllEnemies = this.Players @ this.NeutralEnemies @ this.AggressiveEnemies
            member this.Walls = state.Board.Walls
            member this.Position = state.Player.Position
            member this.IsValidPosition position = isValidPosition state position

            member this.IsSafePosition (p:Position) = 
                    let distance = (Distance.manhattan p)
                    let isInSameLine = (Distance.isInSameLine p)
//                    if this.CanFire
//                    then 
//                        this.AggressiveEnemies |> List.map distance |> List.forall (fun x-> x >= 2)
//                        &&  this.Players |> List.map distance |> List.forall (fun x-> x >= 2)
//                    else 
                    this.AggressiveEnemies |> List.map distance |> List.forall (fun x-> x >= 2)
                    &&  this.Players |> List.map distance |> List.forall (fun x-> x >= 2)
                    && this.Players |> List.map isInSameLine |> List.forall (fun x -> x.IsNone)

            member this.IsValidAndSafePosition position = this.IsValidPosition position && this.IsSafePosition position

            member this.HasWallInLines p1 p2 = hasWallInLines state p1 p2
            // member this.ViewSize = viewSize state
            member this.GetCross position = getCross state position
            member this.GetNeighbours postion = [ top postion ; right postion; bottom postion; left postion ] |> Seq.filter this.IsValidAndSafePosition 
            member this.FindPath destination =  
                let config:AStar.Config<Position> = { Neighbours = this.GetNeighbours ; GCost = gScore; FCost = fScore; MaxIterations = None }
                AStar.search this.Position destination config 
                |> Option.map (Array.ofSeq >> Array.rev )


            // member this.TryLeftOrRight = tryLeftOrRight state
            // member this.TryTopOrBottom = tryTopOrBottom state
            member this.StepTo p = stepTo this.Position p
            member this.StepLeftRight p = stepLeftRight this.Position p
            member this.StepTopDown p = stepTopDown this.Position p
            member this.BoardSize = state.Board.Size
            member this.DistanceFrom p = Distance.manhattan this.Position p
            member this.IsOnLeft p = this.Position.X < p.X
            member this.IsOnRight p = this.Position.X > p.X
            member this.IsOnTop p = this.Position.Y < p.Y
            member this.IsOnBottom p = this.Position.Y > p.Y

            member this.TryGoLeft = if this.Position |> left |> this.IsValidPosition then Some Left else None
            member this.TryGoRight = if this.Position |> right |> this.IsValidPosition then Some Right else None
            member this.TryGoUp = if this.Position |> top |> this.IsValidPosition then Some Up else None
            member this.TryGoDown = if this.Position |> bottom |> this.IsValidPosition then Some Down else None
            
            member this.Debug arg = logger.Debug(arg |> fst ,arg |> snd)
            member this.Info arg = logger.Information(arg |> fst ,arg |> snd)

            member this.IsPlayer p = this.Players |> List.exists (fun x -> x.X = p.X && x.Y = p.Y )
            member this.IsAggressiveEnemy p = this.AggressiveEnemies |> List.exists (fun x -> x.X = p.X && x.Y = p.Y )
            member this.IsNeutral p = this.NeutralEnemies |> List.exists (fun x -> x.X = p.X && x.Y = p.Y )
         }

module Agent = 

    let tryLeftOrRight (state:IState) =
        
        [ left state.Position ; right state.Position ]
            |> List.filter state.IsValidPosition 
            |> function
            | h :: t -> state.StepLeftRight h
            | _ -> None

    let tryTopOrBottom (state:IState) = 
        [ top state.Position ; bottom state.Position ]
            |> List.filter state.IsValidPosition
            |> function
            | h :: t -> state.StepTopDown h
            | _ -> None

    let getAwayFromPlayers (state:IState) = 

        let from = state.Players |> List.filter (fun x -> state.DistanceFrom x <= 2) |> List.sortBy state.DistanceFrom
        if from.Length = 0
        then None
        else
            let moves = state.GetNeighbours state.Position |> Seq.map state.StepTo |> Seq.toList
            match moves with
            | h::_ -> 
                state.Debug ("GETTING AWAY !!!! ", Array.empty)
                h
    //            if state.IsOnLeft h
    //            then state.TryGoLeft
    //            else None
    //            <|> fun () -> if state.IsOnRight h
    //                            then state.TryGoRight
    //                            else None
    //            <|> fun () -> if state.IsOnTop h
    //                          then state.TryGoUp
    //                          else None
    //            <|> fun () -> if state.IsOnBottom h
    //                          then state.TryGoDown
    //                          else None
            | _ -> None
    let getAway (state:IState) = 

         
        let from' = state.AggressiveEnemies @ state.Players |> List.filter (fun x -> state.DistanceFrom x <= 2) |> List.sortBy state.DistanceFrom
        if from'.Length = 0
        then None
        else
            
            let moves = state.GetNeighbours state.Position |> Seq.map state.StepTo |> Seq.toList
            match moves with
            | h::_ -> 
                state.Debug ("GETTING AWAY !!!! ", Array.empty)
                h
    //            if state.IsOnLeft h
    //            then state.TryGoLeft
    //            else None
    //            <|> fun () -> if state.IsOnRight h
    //                            then state.TryGoRight
    //                            else None
    //            <|> fun () -> if state.IsOnTop h
    //                          then state.TryGoUp
    //                          else None
    //            <|> fun () -> if state.IsOnBottom h
    //                          then state.TryGoDown
    //                          else None
            | _ -> None

    let findNearestNeutral (state:IState) = 

        let neutralsPositions = state.NeutralEnemies

        let paths = neutralsPositions |> List.map state.FindPath |> List.choose id

        if paths.Length = 0 
        then None
        else 
            let bestPath = paths |>  List.minBy (fun p -> p |> Array.length) 
            if bestPath.Length = 1
            then 
                state.Debug ("GOTO NEUTRAL ! {@Position}", [|bestPath.[0]|])
                Some Move.Idle
            else
                state.Debug ("GOTO NEUTRAL ! {@Position}", [|bestPath.[1]|])
                state.StepTo bestPath.[1]

    let tryTouchNeutral (state:IState) = 
        state.Debug ("\t▶ TRY TO TOUCH NEUTRAL", [||])
        state.NeutralEnemies 
                |> List.filter (fun x -> state.DistanceFrom x = 1)
                |> List.filter state.IsValidPosition
                |> List.map state.StepTo
                |> function
                | [] -> None
                | h::t -> h
                
    let tryGoToNeutral (state:IState) = 
        state.Debug ("\t▶ TRY GO TO NEUTRAL", [||])
        state.NeutralEnemies 
                |> List.map state.FindPath 
                |> List.choose id
                |> List.filter (fun p -> p |> Array.length = 1)
                |> List.sortBy (fun p -> p |> Array.length)
                |> function
                | [] -> None
                | h::t ->
                    state.Debug ("🏃‍♀️ GO TO ENEMY {@Position}}", [|h.[1]|])
                    let move = state.StepTo h.[1]
                    move

    let tryShoot (state:IState) = 
        if state.CanFire |> not
        then None
        else
            let shouldShot = state.AllEnemies  
                            |> List.filter (fun e -> (Distance.isInSameLine state.Position e).IsSome)
                            |> List.filter (fun e -> state.HasWallInLines state.Position e |> not  )
                            |> List.sortByDescending(fun x -> 
                                            if state.IsPlayer x then 100 , Distance.manhattan state.Position x
                                            elif state.IsAggressiveEnemy x then 50,Distance.manhattan state.Position x
                                            else 25, Distance.manhattan state.Position x
                                         )
            if shouldShot.Length = 0
            then 
                state.Debug ("NO PATH TO ANY ENEMY", [||])
                None
            else
                state.Debug ("🔫 SHOOT ENEMY {@Position}}", [|shouldShot.Head|])
                getShoot state.Position shouldShot.Head


    let tryToGetInline (state:IState) = 
        let paths = 
            state.AllEnemies 
            |> List.collect state.GetCross 
            |> List.map state.FindPath 
            |> List.choose id
        
        if paths.Length = 0 
        then 
            state.Debug ("😭 NO PATH TO ANY ENEMY", [||])
            None
        else
            let validPath = paths |>  List.filter (fun p -> p |> Array.length > 1)
            match validPath with 
            | [] -> 
                state.Debug ("😭 NO PATH TO ANY ENEMY", [||])
                None
            | _ -> 
                // TODO : maybe not the best 
                let best = validPath |> List.minBy (fun p -> p |> Array.length) |> fun x -> x.[1]
                state.Debug ("🏃‍♀️ GO TO ENEMY {@Position}}", [|best|])
                let move = state.StepTo best
                move

    let tryWaitPlayer (state:IState) =
        if state.CanFire
        then
            state.Players
            |> List.filter (fun x -> x.X = state.Position.X + 1  || x.X = state.Position.X - 1 || 
                                     x.Y = state.Position.Y + 1 || x.Y = state.Position.Y - 1  )
            |> function
               | [] -> None
               | h::_ -> Some Idle
        else
            None

    let visitMaze (state:IState) =
        state.Debug ("ℹ VISIT MAZE", [||]) 

        let borderToSee =   [ for x in 0..state.BoardSize.Width -> {X = x; Y = 0} ] @
                            [ for x in 0..state.BoardSize.Width -> {X = x; Y = state.BoardSize.Height - 1} ] @
                            [ for y in 0..state.BoardSize.Height -> {X = 0; Y = y} ] @
                            [ for y in 0..state.BoardSize.Height -> {X = state.BoardSize.Width-1; Y = y} ]
                            |> List.tryFind (fun p -> state.Walls |> List.contains p |> not)

        state.Debug ("👀 try to see {@Position}", [|borderToSee|])
        
        borderToSee 
        >>=  state.FindPath
        |> function
            | None -> 
                let topLeft = {X = 0; Y = 0}
                let topRight = {X= state.BoardSize.Width - 1; Y = 0}
                let bottomLeft = {X= 0; Y = state.BoardSize.Height - 1}
                let bottomRight = {X= state.BoardSize.Width - 1; Y = state.BoardSize.Height - 1}

                let cornerToGo = [topLeft; topRight; bottomLeft; bottomRight] |> List.maxBy state.DistanceFrom
                state.Debug ("⛔ Go to corner {@Position} ", [|cornerToGo|])
                cornerToGo |> state.FindPath |> Option.map (fun p -> p.[1]) |> Option.bind state.StepTo

            | Some p -> p.[1] |> state.StepTo

    let randomMove(state:IState) =
      state.Info ("🙏 DO RANDOM", [||])
      [ Up ; Down ; Left ; Right] |> List.minBy (fun _ -> Guid.NewGuid())

        

    let getMove (state:State) (logger:ILogger) =

        let state = State.fn state logger

        state.Debug("{@Matrix}", [|state.Matrix.Print()|]) 

        getAwayFromPlayers(state)
        <|> fun () ->  tryTouchNeutral(state)
        <|> fun () ->  tryGoToNeutral(state)
        <|> fun () ->  tryShoot(state)
        <|> fun () ->  tryToGetInline(state)
        <|> fun () -> tryWaitPlayer state
        <|> fun () -> getAway state
        <|> fun () -> findNearestNeutral state
        <|> fun () -> visitMaze state
        |> Option.defaultWith (fun () -> randomMove state)
        
