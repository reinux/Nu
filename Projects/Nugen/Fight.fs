namespace Nugen
open Nu

type FightMessage =
    | Nop
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

type Fight =
    { Player1: Player
      Player2: Player
      CameraPosition: Vector2i
      RoundStartTime : int64 }

    static member empty =
        { Player1 = Player.empty
          Player2 = Player.empty
          CameraPosition = v2iZero
          RoundStartTime = 0L }

    // static member empty = {}
    //
    static member initial fighter1AirFile fighter2AirFile =
        { Player1 = Player.make LocalPlayer (Fighter.make fighter1AirFile Rightward (v2i -100 0))
          Player2 = Player.make AIPlayer (Fighter.make fighter2AirFile Leftward (v2i 100 0))
          CameraPosition = v2i 0 0
          RoundStartTime = 0
        }
        
    static member update ticks (world: World) fight =
        let fight =
            match fight.Player1.fighter.Action, World.isKeyboardKeyDown KeyboardKey.A world with
            | ActionState.WalkingBack, false ->
                { fight with
                    Player1.fighter.ActionStartTime = ticks
                    Player1.fighter.Action = Standing
                }
            | ActionState.WalkingBack, true ->
                fight
            | _, true ->
                { fight with
                    Player1.fighter.Action = WalkingBack
                    Player1.fighter.ActionStartTime = ticks
                }
            | _, false ->
                fight
        let fight =
            if fight.Player1.fighter.Action = WalkingBack then
                { fight with Player1.fighter.Position = fight.Player1.fighter.Position + v2i -1 0 }
            else fight
        fight
    //
    // static member make _ _ _ = {}
