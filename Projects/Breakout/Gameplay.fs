namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quitting
    | Quit

// the user-controlled paddle.
type Paddle =
    { Position : Vector3
      Size : Vector3 }

    static member initial =
        { Position = v3 0.0f -160.0f 0.0f
          Size = v3 64.0f 16.0f 0.0f }

// the block-breaking ball.
type Ball =
    { Position : Vector3
      Size : Vector3
      Speed : single
      Direction : Vector3 }

    member this.Velocity =
        this.Speed * this.Direction

    member this.PositionNext =
        this.Position + this.Velocity

    static member initial =
        { Position = v3 0.0f 48.0f 0.0f
          Size = v3 8.0f 8.0f 0.0f
          Speed = 3.0f
          Direction = (v3 (0.5f - Gen.randomf) -1.0f 0.0f).Normalized }

// the bricks to break out of.
type Brick =
    { Position : Vector3
      Size : Vector3
      Color : Color }

    static member make position =
        { Position = position
          Size = v3 64.0f 16.0f 0.0f
          Color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f }

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      Paddle : Paddle
      Ball : Ball
      Bricks : Map<string, Brick>
      Score : int
      Lives : int }

    // this represents the gameplay model in a vacant state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = Quitting
          Paddle = Paddle.initial
          Ball = Ball.initial
          Bricks = Map.empty
          Score = 0
          Lives = 0 }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        let bricks =
            Map.ofList
                [for i in 0 .. dec 5 do
                    for j in 0 .. dec 6 do
                        (Gen.name, Brick.make (v3 (single i * 64.0f - 128.0f) (single j * 16.0f + 64.0f) 0.0f))]
        { Gameplay.empty with
            GameplayState = Playing
            Bricks = bricks
            Lives = 5 }

    // this updates the gameplay model every frame that gameplay is active.
    static member update gameplay world =
        match gameplay.GameplayState with
        | Playing when gameplay.Lives > 0 && gameplay.Bricks.Count > 0 ->

            // update paddle
            let gameplay =
                let paddle = gameplay.Paddle
                let paddle =
                    if World.isKeyboardKeyDown KeyboardKey.Left world then
                        { paddle with Position = paddle.Position.MapX (fun x -> max -128.0f (x - 4.0f)) }
                    elif World.isKeyboardKeyDown KeyboardKey.Right world then
                        { paddle with Position = paddle.Position.MapX (fun x -> min 128.0f (x + 4.0f)) }
                    else paddle
                { gameplay with Paddle = paddle }

            // update ball motion
            let gameplay =
                let ball = gameplay.Ball
                let ball = { ball with Position = ball.PositionNext }
                { gameplay with Ball = ball }

            // update ball interaction with walls
            let gameplay =
                let ball = gameplay.Ball
                let ball =
                    if  ball.PositionNext.X <= -160.0f ||
                        ball.PositionNext.X >= 160.0f then 
                        World.playSound 1.0f Assets.Default.Sound world
                        { ball with Direction = ball.Direction.MapX negate }
                    else ball
                let ball =
                    if ball.PositionNext.Y >= 172.0f then
                        World.playSound 1.0f Assets.Default.Sound world
                        { ball with Direction = ball.Direction.MapY negate }
                    else ball
                { gameplay with Ball = ball }

            // update ball interaction with paddle
            let gameplay =
                let paddle = gameplay.Paddle
                let ball = gameplay.Ball
                let ball =
                    let perimeter = box3 (paddle.Position - paddle.Size * 0.5f) paddle.Size
                    if perimeter.Intersects ball.PositionNext then
                        World.playSound 1.0f Assets.Default.Sound world
                        { ball with Direction = (ball.Position - paddle.Position).Normalized }
                    else ball
                { gameplay with Ball = ball }

            // update ball interaction with bricks
            let gameplay =
                let ball = gameplay.Ball
                let bricksIntersected =
                    Map.filter (fun _ (brick : Brick) ->
                        let perimeter = box3 (brick.Position - brick.Size * 0.5f) brick.Size
                        perimeter.Intersects ball.PositionNext)
                        gameplay.Bricks
                let ball =
                    if Map.notEmpty bricksIntersected then
                        World.playSound 1.0f Assets.Default.Sound world
                        let brick = Seq.head bricksIntersected.Values
                        { ball with Direction = (ball.Position - brick.Position).Normalized }
                    else ball
                let scoring = Map.count bricksIntersected * 100
                let bricks = Map.removeMany bricksIntersected.Keys gameplay.Bricks
                let gameplay = { gameplay with Ball = ball; Bricks = bricks; Score = gameplay.Score + scoring }
                gameplay

            // update ball death
            let gameplay =
                if gameplay.Ball.PositionNext.Y < -180.0f then
                    let gameplay = { gameplay with Lives = dec gameplay.Lives }
                    let gameplay = if gameplay.Lives > 0 then { gameplay with Ball = Ball.initial } else gameplay
                    gameplay
                else gameplay

            // fin
            gameplay

        | _ -> gameplay

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay> (Gameplay.empty)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the behavior of our gameplay
    override this.Run (gameplay, screen, world) =

        // update gameplay when advancing
        let gameplay =
            if world.Advancing
            then Gameplay.update gameplay world
            else gameplay

        // declare scene group
        let world =
            if screen.GetSelected world
            then World.beginGroupFromFile "Scene" "Assets/Gameplay/Scene.nugroup" [] world
            else World.beginGroup "Scene" [] world

        // background model
        let rotation = Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, gameplay.GameplayTime % 360L |> single |> Math.DegreesToRadians)
        let world = World.doStaticModel "StaticModel" [Entity.Position .= v3 0.0f 0.0f -2.0f; Entity.Rotation @= rotation] world

        // paddle
        let world =
            World.doStaticSprite "Paddle"
                [Entity.Position @= gameplay.Paddle.Position
                 Entity.Size .= gameplay.Paddle.Size
                 Entity.StaticImage .= Assets.Default.Paddle] world

        // ball
        let world =
            World.doStaticSprite "Ball"
                [Entity.Position @= gameplay.Ball.Position
                 Entity.Size .= gameplay.Ball.Size
                 Entity.StaticImage .= Assets.Default.Ball] world

        // walls
        let world =
            World.doStaticSprite "LeftWall"
                [Entity.Position .= v3 -164.0f 0.0f 0.0f
                 Entity.Size .= v3 8.0f 360.0f 0.0f
                 Entity.StaticImage .= Assets.Default.Black] world
        let world =
            World.doStaticSprite "RightWall"
                [Entity.Position .= v3 164.0f 0.0f 0.0f
                 Entity.Size .= v3 8.0f 360.0f 0.0f
                 Entity.StaticImage .= Assets.Default.Black] world
        let world =
            World.doStaticSprite "TopWall"
                [Entity.Position .= v3 0.0f 176.0f 0.0f
                 Entity.Size .= v3 320.0f 8.0f 0.0f
                 Entity.StaticImage .= Assets.Default.Black] world

        // bricks
        let world =
            gameplay.Bricks.Pairs |> Seq.fold (fun world (brickName, brick) ->
                World.doStaticSprite brickName
                    [Entity.Position .= brick.Position
                     Entity.Size .= brick.Size
                     Entity.Color @= brick.Color
                     Entity.StaticImage .= Assets.Default.Brick] world) world

        // end scene declaration
        let world = World.endGroup world

        // declare gui group
        let world = World.beginGroup "Gui" [] world

        // declare score
        let world = World.doText "Score" [Entity.Position .= v3 248.0f 136.0f 0.0f; Entity.Text @= "Score: " + string gameplay.Score] world

        // declare lives
        let world = World.doText "Lives" [Entity.Position .= v3 -240.0f 0.0f 0.0f; Entity.Text .= "Lives"] world
        let world =
            [0 .. dec gameplay.Lives] |> List.fold (fun world i ->
                World.doStaticSprite ("Life+" + string i)
                    [Entity.Position .= v3 -240.0f (single (inc i) * -16.0f) 0.0f
                     Entity.Size .= v3 32.0f 8.0f 0.0f
                     Entity.StaticImage .= Assets.Default.Paddle] world) world

        // declare message
        let messageText = if gameplay.Lives <= 0 then "Game over!" elif gameplay.Bricks.Count = 0 then "You win!" else ""
        let world = World.doText "Message" [Entity.Text @= messageText] world

        // declare quit button
        let (gameplay, world) =
            match World.doButton "Quit" [Entity.Text .= "Quit"; Entity.Position .= v3 232.0f -144.0f 0.0f] world with
            | (true, world) -> ({ gameplay with GameplayState = Quitting }, world)
            | (false, world) -> (gameplay, world)
        
        // end group declaration
        let world = World.endGroup world

        // advance gameplay time
        let gameDelta = world.GameDelta
        let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
        (gameplay, world)