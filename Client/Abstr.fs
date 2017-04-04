module Abstr
open CommandInterpriter



//let infoAnsw update fin info =
//    match info with
//    | GameAnswer.TakeCards(p, cards') ->
//        print info
//        let k = List.ofSeq cards'
//        cards.AddRange k
//        showcards ()
//        update None
//    | GameAnswer.GiveCards(p, cards') ->
//        Set.iter (cards.Remove >> ignore) cards'
//        showcards ()
//        update None
//    | GameAnswer.AddCardFromDeck c ->
//        cards.Add c
//        showcards ()
//        update None
//    //| GameAnswer.GameInfo(_) -> failwith ""
//    | x -> fin <| ServerAnswer.Game(GameAnswer.Info x)

type T =
    | Print of string * (unit -> T)
    | Read of (string -> T)
    | WriteRead of (GameReq.Req * (ServerAnswer.Answ -> T))
    | Until of ((GameReq.Req * (ServerAnswer.Answ -> bool )) * (ServerAnswer.Answ -> T))
    | SuitSelect of int * (Suit Set -> T)
    | DisplayCards of (PlayingCard Set * (unit -> T))
    | Next of (ServerAnswer.Answ -> T)
    | NextUntil of ServerAnswer.Answ * (ServerAnswer.Answ -> T)
    | Login of (ServerAnswer.Answ -> T)
    //| GetServerState of (ServerAnswer.Answ -> T)
    | GameAnswerInfo of (GameAnswer.Info * (unit -> T))
    | SlotsFull
    | EndGame
let rec interp = function
    | ServerAnswer.Game gansw ->
        match gansw with
        | GameAnswer.GetRank -> 
            Print("input rank [2..10; jack; queen; king; ace]", fun () ->
            Read(fun x ->
            let rec f s = 
                match CardVisual.rankNameToId s with
                | None -> Print("not identify. Try again", fun () -> Read f)
                | Some s -> 
                    WriteRead(GameReq.Input <| GameReq.RankInput s, interp)
            f x))
        | GameAnswer.GetCount rank as x->
            Print(sprintf "%A" x, fun () ->
            Read(fun x ->
            let rec f s =
                match System.Int32.TryParse s with
                | false, _ -> Print("input int type value!", fun () -> Read f)
                | _, n ->
                    WriteRead(GameReq.Input <| GameReq.CountInput n, interp)
            f x))
        | GameAnswer.GetSuit(rank, count) as x ->
            Print(sprintf "%A" x, fun () ->
            SuitSelect(count, fun suits ->
            WriteRead(GameReq.Input <| GameReq.SuitInput suits, interp)))
        | GameAnswer.Info info ->  
            GameAnswerInfo(info, fun () -> Next interp)
        | GameAnswer.EndGame -> EndGame
        | GameAnswer.GameInfo(_, c) -> 
            DisplayCards(c, fun () ->
            WriteRead(GameReq.GetState, interp))
        | GameAnswer.FailAnsw as x-> //failwith "Not implemented yet"
            Print(sprintf "%A" x, fun () -> Next interp)
        | GameAnswer.Wait id as x -> 
            Print(sprintf "wait player %s" id, fun () -> NextUntil(ServerAnswer.Game x, interp))
    | ServerAnswer.Success(_) as x ->
        Print(sprintf "%A" x, fun () -> Next interp)
    | ServerAnswer.NameEmpty ->
        Print("Name empty. Please enter name!", fun () -> Login interp)
    | ServerAnswer.NameBusy -> 
        Print("Name busy. Select other name.", fun () -> Login interp)
    | ServerAnswer.ReqLogin ->
        Print("req login", fun () -> Login interp)
    | ServerAnswer.SlotsFull -> SlotsFull
    | ServerAnswer.WaitPlayers n as x -> 
        Print(sprintf "Wait players %i" n, fun () -> NextUntil(x, interp))
    | ServerAnswer.LoginSuccess ->
        Print(sprintf "login success", fun () ->
            Until((GameReq.GetGameInfo, function ServerAnswer.Game(GameAnswer.GameInfo(_)) -> true | _ -> false), interp))
            //WriteRead(GameReq.GetGameInfo, interp))
    | ServerAnswer.FailReq as x ->
        Print(sprintf "%A" x, fun () -> Next interp)


