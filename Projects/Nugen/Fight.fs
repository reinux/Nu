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
    
    static member make player fighter =
        { Controller = player
          fighter = fighter
          Score1 = 0
          Score2 = 0
        }

type Fight =
    { Player1: Player
      Player2: Player
      CameraPosition: Vector2i
      RoundStartTime : int64 }

    // static member empty = {}
    //
    static member initial fighter1AirFile fighter2AirFile =
        { Player1 = Player.make LocalPlayer (Fighter.initial fighter1AirFile)
          Player2 = Player.make AIPlayer (Fighter.initial fighter2AirFile)
          CameraPosition = v2i 0 0
          RoundStartTime = 0
        }
    //
    // static member make _ _ _ = {}
