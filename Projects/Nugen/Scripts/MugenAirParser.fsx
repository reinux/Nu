#r "nuget: Prime"

#load "../AirFile.fs"

open Nugen
open Nugen.AirFile
 
// """
// ; Standing Animation
// [Begin Action 000]
// Clsn2Default: 2
//  Clsn2[0] = -10,  0, 10,-79
//  Clsn2[1] =  -4,-92,  6,-79
// 0,1, 0,0, 7
// 0,2, 0,0, 7
// Loopstart
// 0,3, 0,0, 7
// 0,4, 0,0, 50
// 0,5, 0,0, 7
// 0,6, 0,0, 7
// 0,7, 0,0, 7
// 0,8, 0,0, 60
// """
// |> fun s -> s.Split [|"\r\n"; "\r"; "\n"|]

let parse lines =
  lines
  |> Seq.map preprocessLine
  |> Seq.fold parseLine State.Default
  |> (_.Actions >> AirFile.Make)

System.IO.File.ReadAllLines(@"Projects/Nugen/Assets/TenShinHan/TenShinHan.air")
|> parse