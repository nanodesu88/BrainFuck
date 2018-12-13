module BF.BF2

open BF
open System.Collections.Generic
open System.Text

type BFCommand = 
  | Move of int
  | Crease of int
  | Decrement
  | Print
  | Read
  | LoopBegin
  | LoopEnd

let commandMap = 
  Map.empty 
    |> Map.add '>' (Move 1)
    |> Map.add '<' (Move -1)
    |> Map.add '+' (Crease 1)
    |> Map.add '-' (Crease -1)
    // |> Map.add '+' Increment
    // |> Map.add '-' Decrement
    |> Map.add '.' Print
    |> Map.add ',' Read
    |> Map.add '[' LoopBegin
    |> Map.add ']' LoopEnd
    
let replace (arr: 'a array) b e (v: 'a array) = 
  Array.concat [ (Array.take b arr); v; (Array.skip e arr) ]

let optimize (commands: BFCommand[]): BFCommand[] =
  let mutable pointer = 0
  let mutable commands = commands
  
  let next () = 
    pointer <- pointer + 1
  
  while commands |> Array.length > pointer do
    match commands.[pointer] with
      | Move (cnt) ->
        match commands.[pointer + 1] with
          | Move (cnt2) ->
            commands <- replace commands pointer (pointer + 2) [| Move (cnt + cnt2) |]
            // commands <- slice commands pointer (pointer + 2)
            // commands <- insert commands pointer (Move (cnt + cnt2))
          | _ -> next ()
      | Crease (cnt) ->
        match commands.[pointer + 1] with
          | Crease (cnt2) ->
            commands <- replace commands pointer (pointer + 2) [| Crease (cnt + cnt2) |]
            // commands <- slice commands pointer (pointer + 2)
            // commands <- insert commands pointer (Crease (cnt + cnt2))
          | _ -> next ()          
      | _ -> next ()
  
  commands

let prepare (code: string): BFCommand[] = 
  Encoding.UTF8.GetBytes code
    |> Array.map (fun b -> char b)
    |> Array.filter(fun c -> commandMap |> Map.containsKey c)
    |> Array.map(fun c -> commandMap.[c])
    |> optimize

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
            | Crease x -> memory.[memoryPointer] <- (memory.[memoryPointer] + (uint16 x))
            // | Increment -> memory.[memoryPointer] <- (memory.[memoryPointer] + 1us)
            // | Decrement -> memory.[memoryPointer] <- (memory.[memoryPointer] - 1us)
            | Move cnt -> memoryPointer <- memoryPointer + cnt
            // | MoveBackward cnt -> memoryPointer <- memoryPointer - cnt
            | Print -> response <- Array.append response [| memory.[memoryPointer] |]
            | _ -> ()
    
    commandPointer <- commandPointer + 1
  
  response