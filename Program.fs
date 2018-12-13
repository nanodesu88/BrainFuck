open BF
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text
open Microsoft.FSharpLu

let measure name fn = 
  let sw = Stopwatch()
  sw.Start()
  let res = fn ()
  sw.Stop()
  printfn "%s %f" name sw.Elapsed.TotalMilliseconds
  
  res

[<EntryPoint>]
let main argv =
  let program = File.ReadAllText("programs/pi.bf")
  
  printfn "programLength = %d" (program |> String.length)
  
  let prepared1 = measure "prepare1" (fun _ -> BF1.prepare program)
  let prepared2 = measure "prepare2" (fun _ -> BF2.prepare program)
  
  printfn "prepared1Length = %d" (prepared1 |> Array.length)
  printfn "prepared2Length = %d" (prepared2 |> Array.length)
  
  let response1 = measure "response1" (fun _ -> BF1.go prepared1)
  let response2 = measure "response2" (fun _ -> BF2.go prepared2)
  
  let hex1 = response1 |> Array.map(fun b -> b.ToString("x2")) |> fun hexStrings -> String.Join(",", hexStrings)
  let hex2 = response2 |> Array.map(fun b -> b.ToString("x2")) |> fun hexStrings -> String.Join(",", hexStrings)

  printfn "%s" hex1
  printfn "%s" hex2

  // printfn "%d" ( cmds |> Array.length )
  0 // return an integer exit code
  
  // Вторая версия была с заменой Dicriminated Unions на char
