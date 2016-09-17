module CommandInterpriter

open System
open System.IO
open System.Runtime.Serialization.Formatters.Binary

type A<'a> =
    static member deserialize stream =
        //use fileStream = new FileStream(path, FileMode.Open)
        let bf = new BinaryFormatter()
        let result = bf.Deserialize stream
        (result :?> 'a)

    static member serialize thing =
        let serializeThing thing =
            let bf = new BinaryFormatter()
            use mstream = new MemoryStream()
        
            bf.Serialize(mstream, thing)
            //mstream.ToArray()
            //mstream.ReadByte()
            mstream.ToArray()

        let bytes = (serializeThing thing)
        let ms = new MemoryStream(bytes)
        ms

(*
let parser (str:string) = 
    let xs = str.Split('&') |> Array.map (fun x -> x.Split('='))
    if xs |> Array.exists (function [|x;y|] -> false | _ -> true) then None
    else xs |> Array.map (function [|x;y|] -> x, y | _ -> failwith "parse error") |> Some *)
type Rank = Rank of int
type Suit = Suit of int
type PlayingCard = { Rank: Rank; Suit: Suit }

module ServerAnswer = 
    type Ask =
        | IsRank of Rank
        | IsCount of Rank * int
        | IsSuit of Rank * Suit list

    type InfoMsg =
        | AskXOnYou  of Ask * string // противник спрашивает у тебя
        | AskXOnY    of Ask * string * string
        | MoveYouOnX of string
        | MoveXOnY   of string * string // кто-то на кого-то начинает ход
        | MoveXOnYou of string // на тебя ходит
        | Success    of bool // успешность какой-нибудь операции
        | CardAdd    of PlayingCard
        | CardRemove of PlayingCard

    type cmd = 
        | UnknownCmd
        /// <summary>порядок, имена игроков, а так же кол-во карт у каждого<summary>
        | Players of (string * int) list
        | Write of string
        | GetRank
        | GetSuit of int
        | Read
        | WaitServer
        | WaitPlayer of string
        
        | WaitConnectPlayers of int
        | Success of bool
        
        | StartGame
        | YourName of string
        | LoginReq
        /// <summary> Свободных мест нет (NoVacanties) </summary>
        | SlotBeAbsent
        | Info of InfoMsg
        | Cards of (int * int) list
        
    let unpars (x:cmd) = A<cmd>.serialize x
    let pars x = A<cmd>.deserialize x
    (*
    let unparse = function
        | UnknownCmd -> "act=unknown"
        | RemoveCard(rank, suit) -> String.Format("act=removeCard&rank={0}&suit={1}", rank, suit)
        | AddCard (rank, suit) -> String.Format("act=addCard&rank={0}&suit={1}", rank, suit)
        | Write str -> String.Format("act=write&arg={1}", str)
        | Read -> "act=read"
    //parser "act=write&arg={1}"
    let parse str =
        match parser str with
        | None -> None
        | Some [|("act", "unknown")|] -> Some(UnknownCmd)
        | Some [|("act", "removeCard"); ("rank", rank); ("suit", suit)|] -> Some(RemoveCard(rank, suit))
        | Some [|("act", "addCard"); ("rank", rank); ("suit", suit)|] -> Some(AddCard(rank, suit))
        | Some [|("act", "write"); ("arg", str)|] -> Some(Write str)
        | Some [|("act", "read")|] -> Some(Read)
        | _ -> None
    *)
module ClientReq =
    type cmd = 
        | Rank of int
        | Suits of int list
        //| Success
        | EnterBy of string
        | Write of string
        | Getstate
        | GetMyName
        
        | GetCards
        /// <summary>Запрос на порядок, имена игроков, а так же кол-во карт у каждого.</summary>
        | GetPlayers

    let unpars (x:cmd) = A<cmd>.serialize x
    let pars x = A<cmd>.deserialize x
    (*
    let unparse = function
        | Success -> "act=success"
        | Write str -> String.Format("act=write&arg={1}", str)
        | Getstate -> "act=getstate"

    let parse str =
        match parser str with
        | None -> None
        | Some [| "act", "success" |] -> Some(Success)
        | Some [| "act", "write"; "arg", arg |] -> Some(Write arg)
        | Some [| "act", "getstate" |] -> Some(Getstate)
        | _ -> None
        *)
/// <summary>
/// Copies the contents of input to output. Doesn't close either stream.
/// </summary>
let StreamToStream (input:Stream) (output:Stream) = 
    let buffer = Array.create (8 * 1024) (byte 0)
    let rec f () = 
        let len = input.Read(buffer, 0, buffer.Length)
        if len > 0 then
            output.Write(buffer, 0, len)
            f()
    f()