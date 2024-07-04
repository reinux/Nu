module Nugen.Characters
open Nu
open Prime
open Nugen.Data

let load name =
  let airFile =
    System.IO.File.ReadAllLines($"Assets/%s{name}/%s{name}.air")
    |> AirFile.parse
  let trimmedFrameInfo =
    System.IO.File.ReadAllText($"Assets/%s{name}/%s{name}.frames")
    // |> SymbolicOperators.scvalue<(string * TrimmedFrameInfo) list>
    |> SymbolicOperators.scvalue<Map<string, TrimmedFrameInfo>>
  // let frameInfo =
  //   [ for assetName, info in frameInfo do
  //       match assetName with
  //       | ParseRegex @"_(\d+)-(\d+)$" [ Int actionId; Int frameNum ] ->
  //         (ActionId actionId, frameNum), (assetName, info)
  //       | path -> failwith $"Cannot parse path in fighter info: {path}"
  //   ]
  let actions =
    airFile.Actions
    |> Map.map (fun actionId action ->
        let frames =
          action.Elements
          |> List.map (fun element ->
              let assetName = $"%s{name}_%d{element.GroupNum}-{element.ImageNum}"
              let trimmedFrameInfo =
                trimmedFrameInfo.TryFind assetName
                |> Option.defaultValue TrimmedFrameInfo.Empty
              { Axis = Vector2i(trimmedFrameInfo.AxisX, trimmedFrameInfo.AxisY)
                Width = trimmedFrameInfo.Width
                Height = trimmedFrameInfo.Height
                Offset = Vector2i(fst element.Offset, snd element.Offset)
                Duration =
                  if element.Duration < 0 then 1
                  else element.Duration
                Flip = element.Flip
                BlendSource = element.BlendSource
                BlendDest = element.BlendDest
                HitBoxes = element.CollisionBoxes
                HurtBoxes = element.AttackCollisionBoxes
                AssetName = assetName
              })
        let preLoop, loop =
          match List.tryLast airFile.Actions[actionId].Elements with
          | None -> [], []
          | Some element ->
            // If the last element is -1, then it's terminating.
            if element.Duration = -1 then
              List.splitAt (frames.Length - 1) frames
            else
              List.splitAt airFile.Actions[actionId].LoopStartIndex frames
        { PreLoopFrames = preLoop
          LoopFrames = loop 
        })
  { Actions = actions; AssetName = name }
  
let tenShinHan =
  load "TenShinHan"
    
