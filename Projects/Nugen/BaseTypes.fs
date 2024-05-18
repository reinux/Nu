namespace Nugen

type ActionId =
  ActionId of int
    member x.Value = let (ActionId i) = x in i

type CollisionBox =
  { L: int; R: int; T: int; B: int }
  static member make l t r b = { L = l; R = r; T = t; B = b }

type Flip =
  | NoFlip
  | FlipHorizontal
  | FlipVertical
  | FlipBoth

type FrameInfo =
  { Axis: int * int
    Width: int
    Height: int
    Offset: int * int
    Duration: int
    Flip: Flip option
    BlendSource: int
    BlendDest: int
    HitBoxes: Map<int, CollisionBox>
    HurtBoxes: Map<int, CollisionBox>
    AssetName: string
  }
  
type ActionInfo =
  { PreLoopFrames: FrameInfo list
    LoopFrames: FrameInfo list
  }
  member info.PreLoopDuration = int64 (List.sumBy (_.Duration) info.PreLoopFrames)
  member info.LoopDuration = int64 (List.sumBy (_.Duration) info.LoopFrames)
  
  
type CharacterInfo =
  { Actions: Map<ActionId, ActionInfo>
    AssetName: string
  }

