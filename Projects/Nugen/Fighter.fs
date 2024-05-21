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
  
type Facing = Rightward | Leftward

type DamageDescriptor =
  { points: int }

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
  member fighter.updateInput time loopedBack (dpadh, dpadv, button) =
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
    
  member fighter.update time opponent =
    let fighter =
      match fighter.Action, fighter.Facing with
      | WalkingForward, Leftward
      | WalkingBack, Rightward ->
          { fighter with Position = fighter.Position + v2i -1 0 }
      | WalkingForward, Rightward
      | WalkingBack, Leftward ->
          { fighter with Position = fighter.Position + v2i +1 0 }
      | _ -> fighter
    let fighter =
      { fighter with
          Facing =
            if opponent.Position.X < fighter.Position.X
            then Leftward
            else Rightward
      }
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
    let rec eatActionFrames (frames: FrameInfo list) (numFrames: int) =
      match frames with
      | { Duration = -1 } as element::_ -> element
      | frame::frames ->
        if frame.Duration < numFrames then
          eatActionFrames frames (numFrames - frame.Duration)
        else frame
      | [] -> failwith "No frames for animation"
      
    let currentActionFrame fighter time =
      let currentFrame = time - fighter.ActionStartTime
      let action = Characters.tenShinHan.Actions[fighter.Action.actionId]
      if currentFrame < action.PreLoopDuration then
        false, eatActionFrames action.PreLoopFrames (int currentFrame)
      else
        let timeInLoop = (currentFrame - action.PreLoopDuration) % action.LoopDuration
        let loopedBack = timeInLoop = 0 && currentFrame <> action.PreLoopDuration + 1L
        loopedBack, eatActionFrames action.LoopFrames (int timeInLoop)
        
    let transformAxis fighter (frame: FrameInfo) =
      if fighter.Facing = Leftward
      then frame.CenteredAxis.MapX ((*) -1f)
      else frame.CenteredAxis
        
    let transformBox fighter (box: Box2i) =
      let box =
        if fighter.Facing = Leftward
        then Box2i(-(box.Right.X), box.Bottom.Y, box.Width, box.Height)
        else box
      box.Translate(fighter.Position)
        
    let getCollisions fighter1 (boxes1: Map<int, Box2i>) fighter2 (boxes2: Map<int, Box2i>) =
      seq {
        for kvp1 in boxes1 do
        for kvp2 in boxes2 do
          let b1 = transformBox fighter1 kvp1.Value
          let b2 = transformBox fighter2 kvp2.Value
          if b1.Contains b2 <> ContainmentType.Disjoint then
            kvp1.Key, kvp2.Key
      } |> Set
      