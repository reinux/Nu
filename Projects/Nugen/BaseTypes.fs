namespace Nugen

open Nu

type ActionId =
  ActionId of int
    member x.Value = let (ActionId i) = x in i

type Flip =
  | NoFlip
  | FlipHorizontal
  | FlipVertical
  | FlipBoth

type FrameInfo =
  { Axis: Vector2i
    Width: int
    Height: int
    Offset: Vector2i
    Duration: int
    Flip: Flip option
    BlendSource: int
    BlendDest: int
    HitBoxes: Map<int, Box2i>
    HurtBoxes: Map<int, Box2i>
    AssetName: string
  }
  member info.CenteredAxis =
    v2 (float32 info.Width / 2f - float32 info.Axis.X)
      -(float32 info.Height / 2f - float32 info.Axis.Y)

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

