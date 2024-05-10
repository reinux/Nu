namespace Nugen
open Nu

type FighterInput =
  | Left
  | Right
  | Jump
  | Crouch
  | Punch
  | Kick

type InteractionBox =
  | HurtBox
  | HitBox

type Collider = Collider of Bounds: Box2i * Kind: InteractionBox

type Facing = Rightward | Leftward

type DamageDescriptor =
  { points: int }

type FighterMessage =
  | UpdateInput of FighterInput
  | TakeDamage of DamageDescriptor
  
type FighterCommand =
  | Nop

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
  | WalkingBack
  | Running
  | Falling
  | Fallen
  | Punching
  | Kicking
  | TakingDamage
  | Defeated
  member state.actionId =
    match state with
    | Standing -> 0
    | Crouching -> 11
    | StandingToCrouching -> 12
    | CrouchingToStanding -> 10
    | Walking -> 20
    | WalkingBack -> 21
    | Running -> 100
    | Falling -> 5050
    | Fallen -> 5110
    | Punching -> 200
    | Kicking -> 240
    | TakingDamage -> 120
    | Defeated -> 5110
    |> ActionId
  member state.atAnimationEnd =
    match state with
    | Standing
    | Crouching
    | Walking
    | WalkingBack
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
    | WalkingBack
    | Running
    | CrouchingToStanding
    | StandingToCrouching -> true
    | Fallen
    | Defeated
    | Punching
    | Kicking
    | Falling
    | TakingDamage -> false

type Fighter =
  { Health: int
    Action: ActionState
    ActionStartTime: int64
    AirFile: AirFile
    Facing: Facing
    Position: Vector2i }

  static member empty =
    { Health = 0
      Action = Standing
      ActionStartTime = 0
      AirFile = AirFile.Default
      Facing = Rightward
      Position = v2iZero }

  static member make airFile facing position =
    { Fighter.empty with
        Health = 100
        AirFile = airFile
        Facing = facing
        Position = position }
    
module Fighter =
    let fighterSpriteAsset (ActionId group, frame) =
      asset<Image> "TenShinHan" $"TenShinHan_%d{group}-%d{frame}"
    let tempFighterAirFile =
        System.IO.File.ReadAllLines("Assets/TenShinHan/TenShinHan.air")
        |> AirFile.parse
    let rec eatActionFrames (elements: AnimationElement list) (frame: int) =
      match elements with
      | { Duration = -1 } as element::_ -> element
      | element::elements ->
        if element.Duration < frame then
          eatActionFrames elements (frame - element.Duration)
        else element
      | [] -> failwith "No frames for animation"
    let currentActionElement fighter time =
      let currentFrame = time - fighter.ActionStartTime
      let action = fighter.AirFile.Actions[fighter.Action.actionId]
      let preLoop, loop =
        List.splitAt action.LoopStartIndex action.Elements
      let preLoopDuration = preLoop |> List.sumBy (fun element -> if element.Duration < 0 then 0 else element.Duration)
      let loopDuration =
          match List.tryLast action.Elements with
          | Some { Duration = -1 }
          | None ->
            System.Int64.MaxValue
          | _ ->
            loop |> List.sumBy (fun element -> if element.Duration < 0 then 0 else element.Duration)
      if currentFrame < preLoopDuration then
        eatActionFrames preLoop (int currentFrame)
      else
        let timeInLoop = (currentFrame - (int64 preLoopDuration)) % loopDuration
        eatActionFrames loop (int timeInLoop)