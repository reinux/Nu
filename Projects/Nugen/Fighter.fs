namespace Nugen
open Nu

[<RequireQualifiedAccess>]
type DPadH =
  | Center
  | Forward
  | Backward
[<RequireQualifiedAccess>]
type DPadV =
  | Center
  | Up
  | Down

type FighterInputButton =
  | LowPunch
  | MediumPunch
  | HighPunch
  | LowKick
  | MediumKick
  | HighKick
  
type InteractionBox =
  | HurtBox
  | HitBox

type Collider = Collider of Bounds: Box2i * Kind: InteractionBox

type Facing = Rightward | Leftward

type DamageDescriptor =
  { points: int }

type FighterMessage =
  | UpdateInput of DPadH * DPadV * FighterInputButton
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
  | WalkingForward
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
    | StandingToCrouching -> 10
    | CrouchingToStanding -> 12
    | WalkingForward -> 20
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
    | WalkingForward
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
    | WalkingForward
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
    Facing: Facing
    Position: Vector2i }
  member fighter.withAction startTime action =
    { fighter with ActionStartTime = startTime; Action = action }
  
  /// Respond to player input. In a full implementation, the commands inputs would first be
  /// parsed by the Player type before being issued to Fighter.
  member fighter.parseInput time loopedBack (dpadh, dpadv, button) =
    let fighter =
      let transitionTo toAction =
        { fighter with Action = toAction; ActionStartTime = time }
      match fighter.Action, loopedBack, dpadh, dpadv, button  with
      | Standing, _, DPadH.Forward, DPadV.Center, None ->
        transitionTo WalkingForward
      | Standing, _, DPadH.Backward, DPadV.Center, None ->
        transitionTo WalkingBack
      | Standing, _, DPadH.Center, DPadV.Down, None ->
        transitionTo StandingToCrouching
      | StandingToCrouching, true, _, _, _ ->
        transitionTo Crouching
      | Crouching, _, DPadH.Center, DPadV.Center, None ->
        transitionTo CrouchingToStanding
      | CrouchingToStanding, true, _, _, _
      | WalkingForward, _, DPadH.Center, DPadV.Center, None
      | WalkingBack, _, DPadH.Center, DPadV.Center, None ->
        transitionTo Standing
      | Standing, _, DPadH.Center, DPadV.Center, Some LowPunch ->
        transitionTo Punching
      | Standing, _, DPadH.Center, DPadV.Center, Some LowKick ->
        transitionTo Kicking
      | Punching, true, DPadH.Center, DPadV.Center, _
      | Kicking, true, DPadH.Center, DPadV.Center, _ ->
        transitionTo Standing
      | _ -> fighter
    fighter

  static member empty =
    { Health = 0
      Action = Standing
      ActionStartTime = 0
      Facing = Rightward
      Position = v2iZero }

  static member make facing position =
    { Fighter.empty with
        Health = 100
        Facing = facing
        Position = position }
    
module Fighter =
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
      let action = FighterAssets.tenShinHan.Value.AirFile.Actions[fighter.Action.actionId]
      let preLoop, loop =
        List.splitAt action.LoopStartIndex action.Elements
      let preLoopDuration =
        preLoop
        |> List.sumBy (fun element -> if element.Duration < 0 then 0 else element.Duration)
        |> int64
      let loopDuration =
          match List.tryLast action.Elements with
          | Some { Duration = -1 }
          | None ->
            System.Int64.MaxValue
          | _ ->
            loop |> List.sumBy (fun element -> if element.Duration < 0 then 0 else element.Duration)
      if currentFrame < preLoopDuration then
        false, eatActionFrames preLoop (int currentFrame)
      else
        let timeInLoop = (currentFrame - preLoopDuration) % loopDuration
        let loopedBack = timeInLoop = 0 && currentFrame <> preLoopDuration + 1L
        loopedBack, eatActionFrames loop (int timeInLoop)