namespace Nugen

type ActionId =
  ActionId of int
    member x.Value = let (ActionId i) = x in i

type CollisionBox =
  { L: int; R: int; T: int; B: int }
  static member make l t r b = { L = l; R = r; T = t; B = b }
  member box1.collidesWith (x1, y1) box2 (x2, y2) =
    x1 + box1.L <= x2 + box2.R
    && x1 + box1.R >= x2 + box2.L
    && y1 + box1.T >= y2 + box2.B
    && y1 + box1.B <= y2 + box2.T

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
  member info.CenteredAxis =
    float32 info.Width / 2f - float32 (fst info.Axis),
    -(float32 info.Height / 2f - float32 (snd info.Axis))

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

