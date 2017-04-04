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

type PlayerId = string

type Rank = int
type Suit = int
type PlayingCard = { Rank:Rank; Suit:Suit }

module GameAnswer =
    type Ask =
        | IsRank of Rank
        | IsCount of Rank * int
        | IsSuit of Set<Suit>
    type Asking = 
        | AskXOnYou  of Ask * PlayerId
        | AskXOnY    of Ask * PlayerId * PlayerId
    type Info =
        | AddCardFromDeck of PlayingCard
        //| RemoveCard of PlayingCard
        | TakeCards of PlayerId * PlayingCard Set
        | GiveCards of PlayerId * PlayingCard Set

        | CompileChest of PlayerId * Rank
        | Asking of Asking
        | AskingResult of Asking * bool

        | MoveYouOnX of PlayerId
        | MoveXOnY   of PlayerId * PlayerId
        | MoveXOnYou of PlayerId

        | Success of bool

    type PlayerInfo = { Id:PlayerId; CardsCount:int; Chests:Set<Rank> }

    type Answ =
        | Wait of PlayerId
        | EndGame

        | GetRank
        | GetCount of Rank
        | GetSuit of Rank * int

        | Info of Info
        | FailAnsw
        //| Nil

        /// <summary>На случай если один из игроков выйдет из игры и снова войдет, таких данных достаточно
        /// чтобы продолжить игру.</summary>
        | GameInfo of PlayerInfo list * Set<PlayingCard>

module ServerAnswer = 
    type Answ = 
        | Success of bool
        | NameEmpty
        | NameBusy
        | SlotsFull
        
        | WaitPlayers of int
//        | GameStart of bool

        | Game of GameAnswer.Answ
        | ReqLogin
        | LoginSuccess
        | FailReq
    let unpars (x:Answ) = A<Answ>.serialize x
    let pars x = A<Answ>.deserialize x

module GameReq =
    type Inputs = 
        | RankInput of Rank
        | CountInput of int
        | SuitInput of Set<Suit>

    type Req = 
        | GetState
        | Input of Inputs
        /// <summary>Запрос на порядок, имена игроков, а так же кол-во карт у каждого.</summary>
        | GetGameInfo

module ClientReq =
    type Req =
        | Login of string
        | GameReq of GameReq.Req
        | GetServerState
    let unpars (x:Req) = A<Req>.serialize x
    let pars x = A<Req>.deserialize x

/// <summary>
/// Copies the contents of input to output. Doesn't close either stream.
/// </summary>
let streamToStream (input:Stream) (output:Stream) = 
    let buffer = Array.create (8 * 1024) (byte 0)
//    let f () = input.Read(buffer, 0, buffer.Length)
//    until ((=) 0) (fun len -> output.Write(buffer, 0, len); f()) <| f()
//    |> ignore

    let rec f () = 
        let len = input.Read(buffer, 0, buffer.Length)
        if len > 0 then
            output.Write(buffer, 0, len)
            f()
    f()