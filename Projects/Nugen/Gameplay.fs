namespace Nugen
open System
open System.Numerics
open Prime
open Nu

type FightMessage =
    | Nil
    interface Message
    
type FightCommand =
    | Nop
    interface Command

type PlayerController =
    | LocalPlayer
    | AIPlayer

type Player =
    { Controller: PlayerController
      Fighter: Fighter
      Score1: int
      Score2: int
    }

    static member empty =
        { Controller = LocalPlayer
          Fighter = Fighter.empty
          Score1 = 0
          Score2 = 0
        }
    
    static member make player fighter =
        { Player.empty with
            Controller = player
            Fighter = fighter
        }

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit
    
// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
// if you wish to use clock time instead (https://github.com/bryanedds/Nu/wiki/GameTime-and-its-Polymorphic-Nature),
// you could use `GameplayTime : single` instead. If you're going to use Split MMCC instead of Pure MMCC, you won't
// need this field at all and should remove it, using world.UpdateTime or world.ClockTime instead (see
// https://github.com/bryanedds/Nu/wiki/Pure-MMCC-vs.-Split-MMCC)
type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      Player1: Player
      Player2: Player
      CameraPosition: Vector2i
      RoundStartTime : int64 }
    member game.Players = [ game.Player1; game.Player2 ]

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = Quit
          Player1 = Player.empty
          Player2 = Player.empty
          CameraPosition = v2iZero
          RoundStartTime = 0L }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        { Gameplay.empty with
            GameplayState = Playing
            Player1 = Player.make LocalPlayer (Fighter.make Rightward (v2i -100 0))
            Player2 = Player.make AIPlayer (Fighter.make Leftward (v2i 100 0))
            CameraPosition = v2i 0 0
            RoundStartTime = 0 }
        
    static member private translateKeyInput facing world =
        let dpadH =
            if World.isKeyboardKeyDown KeyboardKey.D world then
                if facing = Rightward then DPadH.Forward else DPadH.Backward
            elif World.isKeyboardKeyDown KeyboardKey.A world then
                if facing = Rightward then DPadH.Backward else DPadH.Forward
            else DPadH.Center
        let dpadV =
            if World.isKeyboardKeyDown KeyboardKey.W world then
                DPadV.Up
            elif World.isKeyboardKeyDown KeyboardKey.S world then
                DPadV.Down
            else DPadV.Center
        let button =
            if World.isKeyboardKeyDown KeyboardKey.J world then
                Some FighterInputButton.LowPunch
            elif World.isKeyboardKeyDown KeyboardKey.K world then
                Some FighterInputButton.MediumPunch
            elif World.isKeyboardKeyDown KeyboardKey.L world then
                Some FighterInputButton.HighPunch
            elif World.isKeyboardKeyDown KeyboardKey.N world then
                Some FighterInputButton.LowKick
            elif World.isKeyboardKeyDown KeyboardKey.M world then
                Some FighterInputButton.MediumKick
            elif World.isKeyboardKeyDown KeyboardKey.Comma world then
                Some FighterInputButton.HighKick
            else None
        dpadH, dpadV, button
        
    static member update gameplay world =
        match gameplay.GameplayState with
        | Playing ->
            let dpadH, dpadV, button = Gameplay.translateKeyInput gameplay.Player1.Fighter.Facing world
            let gameplay =
                let loopedBack, _ = Fighter.currentActionFrame gameplay.Player1.Fighter gameplay.GameplayTime
                let fighter =
                    gameplay.Player1.Fighter.updateInput gameplay.GameplayTime loopedBack (dpadH, dpadV, button)
                { gameplay with Player1.Fighter = fighter }
            { gameplay with
                Player1.Fighter = gameplay.Player1.Fighter.update gameplay.GameplayTime gameplay.Player2.Fighter
                Player2.Fighter = gameplay.Player2.Fighter.update gameplay.GameplayTime gameplay.Player1.Fighter
            }
        | Quit -> gameplay

// this is our gameplay MMCC message type.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | Update
    | TimeUpdate
    interface Message

// this is our gameplay MMCC command type.
type GameplayCommand =
    | StartQuitting
    interface Command

// this extends the Screen API to expose the Gameplay model as well as the gameplay quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.UpdateEvent => Update
         Screen.TimeUpdateEvent => TimeUpdate
         ]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =
        
        match message with
        | StartPlaying ->
            let gameplay = Gameplay.initial
            just gameplay

        | FinishQuitting ->
            let gameplay = Gameplay.empty
            just gameplay

        | Update ->
            let gameplay = Gameplay.update gameplay world
            just gameplay

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
            just gameplay

    // here we handle the above commands
    override this.Command (_, command, screen, world) =

        match command with
        | StartQuitting ->
            let world = World.publish () screen.QuitEvent screen world
            just world
    override this.Edit(model, op, screen, world) =
        let boxPerimeter (box: Box2i) =
            [ struct (box.Min.V2, box.TopLeft.V2)
              struct (box.TopLeft.V2, box.TopRight.V2)
              struct (box.TopRight.V2, box.BottomRight.V2)
              struct (box.BottomRight.V2, box.BottomLeft.V2)
            ]
        let drawBoxes collisions fighter frame otherFighter otherFighterFrame =
            let hurts =
                Fighter.getCollisions fighter frame.HitBoxes
                                      otherFighter otherFighterFrame.HurtBoxes
                |> Set.map fst
            for box in frame.HitBoxes do
                let color =
                    if Set.contains box.Key hurts
                    then Color.Red
                    elif Set.contains box.Key collisions
                    then Color.Orange
                    else Color.Green
                World.imGuiSegments2d
                    true
                    (box.Value |> Fighter.transformBox fighter |> boxPerimeter)
                    1f color world
            for box in frame.HurtBoxes do
                World.imGuiSegments2d
                    true
                    (box.Value |> Fighter.transformBox fighter |> boxPerimeter)
                    1f Color.Purple world
        let _, fighter1Frame = Fighter.currentActionFrame model.Player1.Fighter model.GameplayTime
        let _, fighter2Frame = Fighter.currentActionFrame model.Player2.Fighter model.GameplayTime
        let collisions =
            Fighter.getCollisions model.Player1.Fighter fighter1Frame.HitBoxes
                                  model.Player2.Fighter fighter2Frame.HitBoxes
        drawBoxes (Set.map fst collisions) model.Player1.Fighter fighter1Frame model.Player2.Fighter fighter2Frame
        drawBoxes (Set.map snd collisions) model.Player2.Fighter fighter2Frame model.Player1.Fighter fighter1Frame
        
        for player in model.Players do
            World.imGuiSegment2d true (player.Fighter.Position.V2 + v2 -5f -5f, player.Fighter.Position.V2 + v2 5f 5f) 1f Color.Green world
            World.imGuiSegment2d true (player.Fighter.Position.V2 + v2 -5f 5f, player.Fighter.Position.V2 + v2 5f -5f) 1f Color.Green world
        
        just model
        
    // here we describe the content of the game including the hud, the scene, and the player
    override this.Content (gameplay, _) =

        [// the gui group
         Content.group Simulants.GameplayGui.Name []

            [
             // quit
             Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -144.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]]

         // the scene group while playing
         match gameplay.GameplayState with
         | Playing ->
            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" [] [
                // Content.text Simulants.GameplayTime.Name
                //    [Entity.Position == v3 0.0f 150.0f 0.0f
                //     Entity.Elevation == 10.0f
                //     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                //     Entity.Text := $"Axis: {currentFrame.CenteredAxis}"]
                   
                let playerSprite name player =
                    let _, currentFrame = Fighter.currentActionFrame player.Fighter gameplay.GameplayTime
                    Content.staticSprite name
                       [ Entity.Position := (player.Fighter.Position.V2 + (Fighter.transformAxis player.Fighter currentFrame)).V3
                         // Entity.Scale := v3 3.0f 3.0f 0.0f
                         Entity.Flip :=
                             match player.Fighter.Facing with
                             | Rightward ->
                                 Flip.FlipNone
                             | Leftward ->
                                 Flip.FlipH
                         Entity.Size := v3 (float32 currentFrame.Width) (float32 currentFrame.Height) 0f
                         Entity.StaticImage := asset<Image> "TenShinHan" currentFrame.AssetName
                       ]
                playerSprite Simulants.GameplayPlayer1.Name gameplay.Player1
                playerSprite Simulants.GameplayPlayer2.Name gameplay.Player2
            ]

         // no scene group otherwise
         | Quit -> ()]