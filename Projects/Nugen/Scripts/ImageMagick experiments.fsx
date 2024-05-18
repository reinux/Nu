#r "nuget: Prime"
#r "nuget: Magick.NET-Q8-AnyCPU"
#r "nuget: Magick.NET.Core"
#loasssssssssz "../FighterData.fs"
#load "../AirFile.fs"

#time "on"

open Prime
open System.IO
open ImageMagick

let inputDir = "Projects/Nugen/Assets/Unprocessed/TenShinHan"
let outputDir = "Projects/Nugen/Assets/TenShinHan"
let axis =
  [ for line in File.ReadAllLines(Path.Combine(inputDir, "axis.txt")) do
      System.Text.RegularExpressions.Regex.Match(line, @"(X|Y) axis. (\d+)").Groups[2].Value
  ]
  |> function [ x; y ] -> int x, int y
              | _ -> failwith "axis.txt format incorrect"
  
let files = Directory.GetFiles(inputDir, "*.png")

let infos =
  files
  |> Array.Parallel.map (fun path ->
      use img = new MagickImage(path)
      let destPath = Path.Combine(outputDir, Path.GetFileName path)
      let info =
        { Width = img.BoundingBox.Width
          Height = img.BoundingBox.Height
          AxisX = (fst axis) - img.BoundingBox.X
          AxisY = (snd axis) - img.BoundingBox.Y
        }
      img.Trim()
      img.Write(destPath, MagickFormat.Png)
      info)

Array.zip (files |> Array.map Path.GetFileNameWithoutExtension) infos
|> SymbolicOperators.scstring
|> fun s -> File.WriteAllText(Path.Combine(outputDir, "fighter_frame_info"), s)

