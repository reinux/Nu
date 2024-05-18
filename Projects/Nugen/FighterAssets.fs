namespace Nugen

open Prime
open Nu
open NugenPatterns

// TODO: find this a home
type TrimmedFrameInfo =
  { Width: int
    Height: int
    AxisX: int
    AxisY: int
  }

type FighterInfo =
  { AirFile: AirFile
    FrameInfo: Map<ActionId, TrimmedFrameInfo list>
  }

module FighterAssets =
  let tenShinHan =
    let airFile =
      System.IO.File.ReadAllLines("Assets/TenShinHan/TenShinHan.air")
      |> AirFile.parse
    let info =
      System.IO.File.ReadAllText("Assets/TenShinHan/TenShinHan.frames")
      |> SymbolicOperators.scvalue<(string * TrimmedFrameInfo) list>
    let info =
      [ for path, info in info do
          match path with
          | ParseRegex @"_(\d+)-(\d+).png$" [ Int actionId; Int frameNum ] ->
            (ActionId actionId, frameNum), info
          | path -> failwith $"Cannot parse path in fighter info: {path}"
      ]
    let info =
      List.groupBy (fst >> fst) info
      |> List.map (fun (actionId, infos) ->
          let infos =
            infos
            |> List.sortBy fst
            |> List.map snd
          actionId, infos)
      |> Map
    lazy { AirFile = airFile; FrameInfo = info }
    
  // TODO: find this a home
  let tenshinHanSpriteAsset (ActionId group, frame) =
    asset<Image> "TenShinHan" $"TenShinHan_%d{group}-%d{frame}"
