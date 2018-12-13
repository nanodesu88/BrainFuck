module BF.BF1
open System.Collections
open System.Collections.Generic
open System.Text

type BFCommand = 
  | MoveForward
  | MoveBackward
  | Increment
  | Decrement
  | Print
  | Read
  | LoopBegin
  | LoopEnd
  
let commandMap = 
  Map.empty 
    |> Map.add '>' MoveForward
    |> Map.add '<' MoveBackward
    |> Map.add '+' Increment
    |> Map.add '-' Decrement
    |> Map.add '.' Print
    |> Map.add ',' Read
    |> Map.add '[' LoopBegin
    |> Map.add ']' LoopEnd

let prepare (code: string): BFCommand[] = 
  Encoding.UTF8.GetBytes code
    |> Array.map (fun b -> char b)
    |> Array.filter(fun c -> commandMap |> Map.containsKey c)
    |> Array.map(fun c -> commandMap.[c])

let go (commands: BFCommand[]): uint16 array = 
  let commandCount = commands |> Seq.length

  let memory = Array.zeroCreate 30000
  let mutable memoryPointer = 0
  let mutable commandPointer = 0
  let mutable response = Array.empty
  let stack = Stack<int>()
  let mutable until = 0u
  
  while commandCount > commandPointer do
    match commands.[commandPointer] with
      | LoopBegin -> 
        if memory.[memoryPointer] = 0us then
          until <- until + 1u
        else
          stack.Push commandPointer
      | LoopEnd -> 
        if until = 0u then
          if memory.[memoryPointer] = 0us then
            stack.Pop () |> ignore
          else
            commandPointer <- stack.Peek ()
        else
          until <- until - 1u
      | cmd -> 
        if until = 0u then
          match cmd with
            | Increment -> memory.[memoryPointer] <- (memory.[memoryPointer] + 1us)
            | Decrement -> memory.[memoryPointer] <- (memory.[memoryPointer] - 1us)
            | MoveForward -> memoryPointer <- memoryPointer + 1
            | MoveBackward -> memoryPointer <- memoryPointer - 1
            | Print -> response <- Array.append response [| memory.[memoryPointer] |]
            | _ -> ()
    
    commandPointer <- commandPointer + 1
  
  response