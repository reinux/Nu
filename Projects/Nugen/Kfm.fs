namespace Nugen
open Nu

type InteractionBox =
  | HurtBox
  | HitBox

type Collider = Collider of Bounds: Box2i * Kind: InteractionBox

type Facing = Leftward | Rightward

type FrameDescriptor = unit

type AnimationFrame =
  { FrameDescriptor: FrameDescriptor
    Duration: int
    Colliders: Collider list
    Offset: Vector2i }

type Animation =
    { AnimationFrame : Animation array }

type AnimationType =
    | JumpAnimation

type Animations =
    { Animations : Map<AnimationType, Animation> }

type AnimationEndAction =
  | Repeat
  | TransitionTo of ActionState

and ActionState =
  | Standing
  | Crouching
  | StandingToCrouching
  | CrouchingToStanding
  | Walking
  | Running
  | Falling
  | Fallen
  | Punching
  | Kicking
  | TakingDamage
  | Defeated
  member state.frames =
    match state with
    | Standing -> []
    | Crouching -> []
    | StandingToCrouching -> []
    | CrouchingToStanding -> []
    | Walking -> []
    | Running -> []
    | Falling -> []
    | Fallen -> []
    | Punching -> []
    | Kicking -> []
    | TakingDamage -> []
    | Defeated -> []
  member state.atAnimationEnd =
    match state with
    | Standing
    | Crouching
    | Walking
    | Running
    | Fallen
    | Defeated -> Repeat
    | CrouchingToStanding
    | Punching
    | Kicking
    | TakingDamage -> TransitionTo Standing
    | StandingToCrouching -> TransitionTo Crouching
    | Falling -> TransitionTo Fallen
  member state.acceptsInput =
    match state with
    | Standing
    | Crouching
    | Walking
    | Running
    | CrouchingToStanding
    | StandingToCrouching -> true
    | Fallen
    | Defeated
    | Punching
    | Kicking
    | Falling
    | TakingDamage -> false

type Kfm =
  { Health: int
    State: ActionState
    TimeInFrame: int64
    Facing: Facing
    Position: Vector2i }
  static member initial =
    { Health = 100
      State = Standing
      TimeInFrame = 0
      Facing = Rightward
      Position = v2i -100 0 }