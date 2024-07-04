namespace Nugen.Data
open Nu
open Nugen

// https://www.elecbyte.com/mugendocs/air.html

[<AutoOpen>]
module NugenPatterns =

  let (|ParseRegex|_|) regex str =
     let m = System.Text.RegularExpressions.Regex(regex).Match(str)
     if m.Success
     then Some (List.tail [ for x in m.Groups -> x.Value ])
     else None
     
  let (|Int|_|) (str: string) =
    match System.Int32.TryParse str with
    | false, _ -> None
    | true, i -> Some i
    
type AnimationElement =
  { GroupNum: int
    ImageNum: int
    Offset: int * int
    Duration: int
    Flip: Flip option
    BlendSource: int
    BlendDest: int
    CollisionBoxes: Map<int, Box2i>
    AttackCollisionBoxes: Map<int, Box2i>
  }

type Action =
  { Elements: AnimationElement list
    LoopStartIndex: int
  }
  static member Default = { Elements = []; LoopStartIndex = 0 }

[<ReferenceEquality>]
type AirFile =
  { Actions: Map<ActionId, Action>
  }
  static member Default = { Actions = Map.empty }
  static member Make actions =
    let actions =
      actions
      |> List.map (fun (aid, action) -> aid, { action with Elements = List.rev action.Elements })
    { Actions = Map actions }
  
type State =
  { Actions: (ActionId * Action) list
    collisionBoxes: Map<int, Box2i>
    attackCollisionBoxes: Map<int, Box2i>
  }
  static member Default =
    { Actions = []
      collisionBoxes = Map.empty
      attackCollisionBoxes = Map.empty
    }

module AirFile =
  let preprocessLine (line: string) =
    match line.IndexOf(';') with
    | ix when ix < 0 -> line.Trim()
    | ix -> line.Substring(0, ix).Trim()

  let parseLine (state: State) (line: string) =
    let generalParse line =
      match line with
      | ParseRegex @"^\[Begin Action\s+(\d+)\]$" [ Int actionNum ] ->
        { state with Actions = (ActionId actionNum, Action.Default)::state.Actions }
        | "" -> state
        | x -> printfn $"Failed to parse line: {x}"; state
    match state.Actions with
    | [] -> generalParse line
    | (aid, action)::actions ->
      match line with
      | ParseRegex @"^Clsn(1|2)\[(\d+)\]\s*=\s*(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)$"
                   [ Int clsnKind; Int clsnIx; Int l; Int t; Int r; Int b ] ->
        let box = Box2i(l, -b, r - l, b - t)
        match clsnKind with
        | 1 -> { state with attackCollisionBoxes =  state.attackCollisionBoxes.Add(clsnIx, box) }
        | 2 -> { state with collisionBoxes = state.collisionBoxes.Add(clsnIx, box) }
        | x -> failwith $"Invalid collision kind: {x}"
      | ParseRegex @"^(-?\d+)\s*,\s*(\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)((\s*,\s*(VH|HV|V|H|))(\s*,\s*(A|A1|(AS(\d{0,3}))?(D(\d{0,3}))?))?)?\s*$"
                   (Int groupNum :: Int imageNum :: Int offsetX :: Int offsetY :: Int duration :: _optional) ->
        // TODO: do blend and alpha
        let element =
          { GroupNum = groupNum
            ImageNum = imageNum
            Offset = offsetX, offsetY
            Duration = duration
            Flip = None
            BlendSource = 255
            BlendDest = 0
            CollisionBoxes = state.collisionBoxes
            AttackCollisionBoxes = state.attackCollisionBoxes
          }
        { state with
            Actions = (aid, { action with Elements = element::action.Elements })::actions }
      | ParseRegex @"^Clsn(1|2)(Default)?\s*\:\s*(\d+)$" [ Int _clsnKind; _default'; Int _numClsns ] ->
        // { state with collisionBoxes = Map.empty; attackCollisionBoxes = Map.empty }
        state
      | "Loopstart" | "LoopStart" ->
        { state with Actions = (aid, { action with LoopStartIndex = action.Elements.Length })::actions }
      | x -> generalParse x
   
  let parse lines =
    lines
    |> Seq.map preprocessLine
    |> Seq.fold parseLine State.Default
    |> (_.Actions >> AirFile.Make)  