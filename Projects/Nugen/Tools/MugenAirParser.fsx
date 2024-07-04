#r "nuget: FSharpPlus"
#r "nuget: Prime"
open FSharpPlus

// https://www.elecbyte.com/mugendocs/air.html

let (|ParseRegex|_|) regex str =
   let m = System.Text.RegularExpressions.Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None
   
let (|Int|_|) (str: string) =
  match System.Int32.TryParse str with
  | false, _ -> None
  | true, i -> Some i

type ActionId = ActionId of int

type CollisionBox =
  { l: int; r: int; t: int; b: int }
  static member Make l r t b = { l = l; r = r; t = t; b = b }

type Flip =
  | NoFlip
  | FlipHorizontal
  | FlipVertical
  | FlipBoth

type AnimationElement =
  { groupNum: int
    imageNum: int
    offset: int * int
    duration: int
    flip: Flip option
    blendSource: int
    blendDest: int
    collisionBoxes: Map<int, CollisionBox>
    attackCollisionBoxes: Map<int, CollisionBox>
  }

type Action =
  { elements: AnimationElement list
    loopStartIndex: int
  }
  static member Default = { elements = []; loopStartIndex = 0 }

type AirFile =
  { actions: Map<ActionId, Action>
  }
  static member Default = { actions = Map.empty }
  static member Make actions = { actions = Map actions }
  
type State =
  { actions: (ActionId * Action) list
    collisionBoxes: Map<int, CollisionBox>
    attackCollisionBoxes: Map<int, CollisionBox>
  }
  static member Default =
    { actions = []
      collisionBoxes = Map.empty
      attackCollisionBoxes = Map.empty
    }

let preprocessLine line =
  match String.tryFindIndex ((=) ';') line with
  | None -> line.Trim()
  | Some ix -> line.Substring(0, ix).Trim()

let parseLine (state: State) (line: string) =
  let generalParse line =
    match line with
    | ParseRegex @"^\[Begin Action\s+(\d+)\]$" [ Int actionNum ] ->
      { state with actions = (ActionId actionNum, Action.Default)::state.actions }
      | "" -> state
      | x -> printfn $"Failed to parse line: {x}"; state
  match state.actions with
  | [] -> generalParse line
  | (aid, action)::actions ->
    match line with
    | ParseRegex @"^Clsn(1|2)\[(\d+)\]\s*=\s*(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)$"
                 [ Int clsnKind; Int clsnIx; Int a; Int b; Int c; Int d ] ->
      let box = CollisionBox.Make a b c d
      match clsnKind with
      | 1 -> { state with collisionBoxes = state.collisionBoxes.Add(clsnIx, box) }
      | 2 -> { state with attackCollisionBoxes =  state.attackCollisionBoxes.Add(clsnIx, box) }
      | x -> failwith $"Invalid collision kind: {x}"
    | ParseRegex @"^(-?\d+)\s*,\s*(\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)((\s*,\s*(VH|HV|V|H|))(\s*,\s*(A(\d{0,3}))?(S(\d{0,3}))?)?)?\s*$"
                 (Int groupNum :: Int imageNum :: Int offsetX :: Int offsetY :: Int duration :: optional) ->
      // TODO: do blend and alpha
      let element =
        { groupNum = groupNum
          imageNum = imageNum
          offset = offsetX, offsetY
          duration = duration
          flip = None
          blendSource = 255
          blendDest = 0
          collisionBoxes = state.collisionBoxes
          attackCollisionBoxes = state.attackCollisionBoxes
        }
      { state with actions = (aid, { action with elements = element::action.elements })::actions }
    | ParseRegex @"^Clsn(1|2)(Default)?\s*\:\s*(\d+)$" [ Int clsnKind; default'; Int numClsns ] ->
      state
    | "Loopstart" ->
      { state with actions = (aid, { action with loopStartIndex = action.elements.Length })::actions }
    | x -> generalParse x
 
"""
; Standing Animation
[Begin Action 000]
Clsn2Default: 2
 Clsn2[0] = -10,  0, 10,-79
 Clsn2[1] =  -4,-92,  6,-79
0,1, 0,0, 7
0,2, 0,0, 7
Loopstart
0,3, 0,0, 7
0,4, 0,0, 50
0,5, 0,0, 7
0,6, 0,0, 7
0,7, 0,0, 7
0,8, 0,0, 60
"""
|> String.split ["\r\n"; "\r"; "\n"]

let parse lines =
  lines
  |> Seq.map preprocessLine
  |> Seq.fold parseLine State.Default
  |> (_.actions >> AirFile.Make)
  
System.IO.File.ReadAllLines(@"/mnt/nu/Projects/Nugen/Scrap/kfm/kfm.air")
|> parse