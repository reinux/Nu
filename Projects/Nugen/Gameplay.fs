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
      fighter: Fighter
      Score1: int
      Score2: int
    }

    static member empty =
        { Controller = LocalPlayer
          fighter = Fighter.empty
          Score1 = 0
          Score2 = 0
        }
    
    static member make player fighter =
        { Player.empty with
            Controller = player
            fighter = fighter
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
            Player1 = Player.make LocalPlayer (Fighter.make Fighter.tempFighterAirFile Rightward (v2i -100 0))
            Player2 = Player.make AIPlayer (Fighter.make Fighter.tempFighterAirFile Leftward (v2i 100 0))
            CameraPosition = v2i 0 0
            RoundStartTime = 0 }

    static member update gameplay world =
        match gameplay.GameplayState with
        | Playing ->
            let gameplay = 
                match gameplay.Player1.fighter.Action, World.isKeyboardKeyDown KeyboardKey.A world with
                | ActionState.WalkingBack, false ->
                    { gameplay with
                        Player1.fighter.ActionStartTime = gameplay.GameplayTime
                        Player1.fighter.Action = Standing
                    }
                | ActionState.WalkingBack, true ->
                    gameplay
                | _, true ->
                    { gameplay with
                        Player1.fighter.Action = WalkingBack
                        Player1.fighter.ActionStartTime = gameplay.GameplayTime
                    }
                | _, false ->
                    gameplay
            if gameplay.Player1.fighter.Action = WalkingBack then
                { gameplay with Player1.fighter.Position = gameplay.Player1.fighter.Position + v2i -1 0 }
            else gameplay
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
    | UpdateSize
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
         Simulants.GameplayPlayer1.StaticImage.ChangeEvent => UpdateSize
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

        | UpdateSize ->
            let staticImage = Simulants.GameplayPlayer1.GetStaticImage world
            match Metadata.tryGetTextureSize staticImage with
            | Some textureSize ->
                let world = Simulants.GameplayPlayer1.SetSize textureSize.V3 world
                just world
            | None -> just world

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
                let currentFrame = Fighter.currentActionElement gameplay.Player1.fighter gameplay.GameplayTime
                Content.text Simulants.GameplayTime.Name
                   [Entity.Position == v3 0.0f 150.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Text := $"Offset: {currentFrame.Offset}"]
                
                Content.staticSprite Simulants.GameplayPlayer1.Name
                   [ Entity.Position := v3 (float32 gameplay.Player1.fighter.Position.X) (float32 gameplay.Player1.fighter.Position.Y) 0f
                     // Entity.Elevation == 10.0f
                     // Entity.Scale := v3 3.0f 3.0f 0.0f
                     Entity.StaticImage := Fighter.fighterSpriteAsset (ActionId currentFrame.GroupNum, currentFrame.ImageNum)
                   ]
             ]

         // no scene group otherwise
         | Quit -> ()]