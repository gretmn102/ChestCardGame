module Remade

#if INTERACTIVE
#r @"e:\Project\ChestCardGame\CommandInterpriter\bin\Debug\CommandInterpriter.dll"
#endif
open CommandInterpriter
open CommandInterpriter.GameAnswer
open CommandInterpriter.GameReq

module List =
    let next l =
        //List.permute(fun x -> (x + 1) % List.length l) l // [3;1;2] -> [2;3;1]
        [ yield! List.tail l; yield List.head l]
    (* test
    next [1;2;3] // [2;3;1]
    next [2;3;1] // [3;1;2]
    next [3;1;2] // [1;2;3]
    next ([]:int list) // fail
    next [1] // [1] *)

    let next2 curr rest =
        //List.permute(fun x -> (x + 1) % List.length l) l // [3;1;2] -> [2;3;1]
         [ yield! rest; yield curr]
    let others current l =
        if not <| List.exists ((=) current) l then failwith "такой элемент в списке отсутствует"
        let to' = Seq.takeWhile ((<>) current) l
        let from = Seq.skipWhile ((<>) current) l
        [ yield! Seq.skip 1 from; yield! to']
    (*
    // test
    others 1 [1..5] // [2;3;4;5]
    others 3 [1..5] // [4;5;1;2]
    others 5 [1..5] // [1;2;3;4]
    others 6 [1..5] // fail
    others 1 []     // fail
    others 1 [1]    // [] *)

    let nextWhile func xs =
        let rec f func acc = function
            | [] -> None
            | h::t as pls -> 
                if func h then
                    pls @ (List.rev acc) |> Some
                else
                    f func (h::acc) t
        f func [] xs
    assert
        List.forall id [
            nextWhile ((=) 10) [0..20] = Some[10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9];
            nextWhile ((=) -1) [0..20] = None
            nextWhile ((=) -1) [] = None;
            nextWhile (fun _ -> true) ([]:int list) = None;
        ]

module Intuitive =
    module List = 
        let next l = [ yield! List.tail l; yield List.head l]
    type Player =
        { Name:string; ReadLine: unit -> string; WriteLine: string -> unit}
    let rec gameCircle = function
        | h::t as xs -> 
            let msg = h.ReadLine()
            t |> List.iter (fun x -> x.WriteLine(sprintf "player %s send: %s." h.Name msg ))
            List.next xs |> gameCircle
        | _ -> ()
    type Cmd = ReadLine | WriteLine of string
    type Ty(users:Set<_>) =
        let mutable m_suspend = false
        let mutable m_input = ""
        let mutable m_state = "", ReadLine
        let suspend () =
            if m_suspend then failwith "already suspend"
            m_suspend <- true
            printfn "%A" m_state
            while m_suspend do System.Threading.Thread.Sleep 100
        do
            let getInput () = let input = m_input in m_input <- ""; input
            let read p () = 
                m_state <- p, ReadLine
                suspend()
                getInput ()
            let write p s =
                m_state <- p, WriteLine s
                suspend()
            users |> List.ofSeq
            |> List.map (fun name -> {Name = name; ReadLine = read name; WriteLine = write name} )
            |> (fun x -> async {gameCircle x} |> Async.Start)
        member __.State = m_state
        member __.Input s =
            if m_suspend |> not then failwith "not suspend for input"
            m_input <- s
            m_suspend <- false
        member __.Continue () =
            if m_suspend |> not then failwith "not suspend for continue"
            m_suspend <- false

///<summary> реализация через продолжения</summary>
module Continues =
    module Sample = 
        type T =
            | Read of (string -> T)
            | Write of (string * string) * (unit -> T)
            
        let next l = [ yield! List.tail l; yield List.head l]
        let rec f xs =
            let writers msg = 
                (fun () -> f (List.tail xs))
                |> List.foldBack (fun x state -> (fun () -> Write((x, msg), state))) xs
            Read(fun msg -> writers msg ())
    
    
    type PlayersCircleT = 
        | MoveCircle of PlayerId list * (unit -> PlayersCircleT)
        | DeckIsEmpty of (bool -> PlayersCircleT)
        | PlayerHaveCards of PlayerId * (bool -> PlayersCircleT)
        | PlayerTakeCardFromDeck of PlayerId * (unit -> PlayersCircleT)
        | End
        | Fail of string
    let rec playersCircle pls =
        DeckIsEmpty(function
            | false ->
                match pls with
                | [] -> Fail "deck not empty, player empty"
                | [h] -> PlayerTakeCardFromDeck(h, fun () -> playersCircle [h] )
                | (h::_ as pls) ->
                    MoveCircle(pls, fun () ->
                    PlayerTakeCardFromDeck(h, fun () ->
                    List.next pls |> playersCircle))
            | true ->
                let rec f = function
                    | [] -> End
                    | [h] -> 
                        PlayerHaveCards(h, function
                            | true -> Fail "deck is empty, one player not over"
                            | false -> End)
                    | h::t as pls ->
                        MoveCircle(pls, fun () ->
                        PlayerHaveCards(h, fun res ->
                        let pl = if res then List.next pls else t
                        f pl))
                f pls)
                
    type MoveCircleT =
        | EndMoveCircle
        | GetMove of (PlayerId * PlayerId * PlayerId list) * (bool -> MoveCircleT)
        | PlayerHaveCards of PlayerId * (bool -> MoveCircleT)
    let moveCircle = function
        | [] -> failwith "pls is empty"
        | [_] -> failwith "pls only one"
        | curr::t -> 
            let rec f xs =
                let nextWhile cont xs =
                    let rec f = function
                        | [] -> EndMoveCircle
                        | h::t as pls -> 
                            PlayerHaveCards(h, function true -> cont pls | false -> f t)
                    f xs
                let f' = function
                    | h::t as xs -> 
                        GetMove((curr, h, t), function true -> f (List.next xs) | false -> EndMoveCircle)
                    | [] -> failwith "nextWhile has return Some([])"
                xs |> nextWhile f'
            f t

module Deck =
    type Deck = Deck of PlayingCard list
    let suitVarCount = 4
    let rankVarCount = 13
    let take = function
        | Deck [] -> failwith "card is no more in deck"
        | Deck (h::t) -> h, Deck t
    let init = 
        [ for suit in 0..suitVarCount-1 do
            for rank in 0..rankVarCount-1 -> { Rank = rank; Suit = suit } ]
        |> Deck
    let initShuffle () =
        let (Deck xs) = init
        FsharpMyExtension.List.shuffle xs |> Deck
    let isEmpty = function
        | Deck [] -> true
        | _ -> false

module Player = 
    type Player = { Cards:Set<PlayingCard>; Chests:Set<Rank> }
    let takeFromDeck ({Cards = cards} as p) d =
        let (card, d') = Deck.take d
        {p with Cards = cards.Add card }, d'
    let addCard p card =
        let cards = p.Cards |> Set.filter (fun x -> x.Rank = card.Rank)
        if Set.count cards = Deck.suitVarCount - 1 then 
            {p with Cards = p.Cards - cards}, Some card.Rank
        else
            {p with Cards = Set.add card p.Cards}, None
    let takeFromDeck2 p d =
        let (card, d') = Deck.take d
        addCard p card, d', card
    let haveCards { Cards = cards } = Set.isEmpty cards |> not
    let haveCard card { Cards = cards } = Set.contains card cards

    let haveRank r { Cards = cards } =
        cards |> Set.exists (fun { Rank = x} -> x = r )
    let filterRank r { Cards = cards } =
        cards |> Set.filter (fun { Rank = x } -> x = r )
    let haveCount (r, count) { Cards = cards } =
        cards |> Set.filter (fun { Rank = x } -> x = r ) |> Set.count = count

    let give p1 p2 card =
        let p1 = {p1 with Cards = Set.remove card p1.Cards}
        let p2 = addCard p2 card
        p1, p2

open Continues
type Msg =
    | Post of PlayerId * Req * AsyncReplyChannel<Answ>

type DataState = { Deck: Deck.Deck; Players: Map<PlayerId, Player.Player> }
type PCircleState = { PCircle: PlayersCircleT; Data:DataState }
type MoveCircleState = { MoveCircle: MoveCircleT; Data:DataState }

let replyChannel (r:AsyncReplyChannel<_>) x = r.Reply x
let receive (mailboxProc:MailboxProcessor<_>) = mailboxProc.Receive()

let requester inbox {Players = pls} interp =
    let rec loop () =
        async {
            let! msg = receive inbox
            match msg with
            | Post(pId, GetGameInfo, r) ->
                let plsInfo = 
                    Map.toList pls |> List.map (fun (id, p) -> 
                        { PlayerInfo.Id = id;
                          CardsCount = p.Cards |> Set.count
                          PlayerInfo.Chests = p.Chests })
                let answ = GameInfo(plsInfo, pls.[pId].Cards)
                replyChannel r answ
                return! loop()
            | Post(pId, req, r) -> 
                let (answ, ret) = interp pId req
                replyChannel r answ
                match ret with
                | Some x -> return x
                | None -> return! loop()
        }
    loop()
let inputer inbox dataState pId getStateAnsw inputState =
    let interp pIdcurr = function
        | GetState -> 
            if pIdcurr = pId then getStateAnsw, None
            else Answ.Wait pId, None
        | Input(inputs) ->
            if pIdcurr = pId then inputState inputs
            else FailAnsw, None
        | _ -> FailAnsw, None
    requester inbox dataState interp

let informer inbox dataState pId msg =
    let f pId' = function
        | GetState -> 
            if pId' = pId then msg, Some ()
            else Answ.Wait pId, None
        | _ -> FailAnsw, None
    requester inbox dataState f

let loopMoveCircle inbox st =
    let getp pId (pls:Map<_,_>) = pls.[pId]
    let getMove (p1, p2, xs) ({ Players = pls} as st) =
        async {
            let informSingle = informer inbox st
                
            let informAll xs answ = 
                async{
                    for x in xs do
                        do! informSingle x answ
                }

            let informs answOnYou answXonY =
                async {
                    do! informer inbox st p2 (Info answOnYou)
                    for x in xs do
                        do! informer inbox st x (Info answXonY)
                }
            do! informer inbox st p1 (Info (MoveYouOnX p2))
            do! informs (MoveXOnYou p1) (MoveXOnY(p1, p2))
            
            let informAllSucc res =
                async {
                    let answ = Success res
                    do! informer inbox st p1 (Info answ)
                    do! informs answ answ
                }
            let maybe (x, f) = 
                match x |> Async.RunSynchronously with
                | None -> None
                | Some x -> f x
            let rankInputer () = 
                let f = function
                    | RankInput x -> GameAnswer.Info <| Success true, Some x
                    | _ -> FailAnsw, None
                inputer inbox st p1 GetRank f
            let countInputer rank () =
                let f = function
                    | CountInput x -> GameAnswer.Info <| Success true, Some x
                    | _ -> FailAnsw, None
                inputer inbox st p1 <| GetCount rank <| f
            let suitInputer rank count () =
                let f = function
                    | SuitInput x -> GameAnswer.Info <| Success true, Some x
                    | _ -> FailAnsw, None
                inputer inbox st p1 <| GetSuit(rank,count) <| f

            let func inputFunc askType f =
                async {
                    let! input = inputFunc()
                    let ask = askType input
                    do! informs (Asking <| AskXOnYou(ask, p1)) (Asking <| AskXOnY(ask, p1, p2))
                    match f input with
                    | None -> 
                        do! informs <| AskingResult(AskXOnYou(ask, p1), false) <| AskingResult(AskXOnY(ask, p1, p2), false)
                        //do! informAllSucc false
                        return None
                    | Some x -> 
                        do! informs <| AskingResult(AskXOnYou(ask, p1), true) <| AskingResult(AskXOnY(ask, p1, p2), true)
                        //do! informAllSucc true
                        return Some x
                }
            let result = 
                let re = func rankInputer IsRank (fun rank -> 
                    let cards = Player.filterRank rank (getp p2 pls)
                    if Set.isEmpty cards then None else Some(rank, cards))
                maybe(re, fun (rank, cards) ->
                    let r = 
                        func (countInputer rank) (fun k -> IsCount(rank, k)) (fun count ->
                            if Set.count cards = count then Some (count, cards) else None)
                    maybe(r, fun (count, cards) ->
                        let f suits = 
                            if suits |> Set.forall (fun s -> cards |> Set.exists(fun c -> c.Suit = s)) then
                                Some cards
                            else None
                        func (suitInputer rank count) IsSuit f
                        |> Async.RunSynchronously)
                )
            match result with
            | None -> return false, st
            | Some cards ->
                let give c ({Players = pls} as st) = 
                    async {
                        //do! informer inbox st p1 (Info.AddCard c |> Info)
                        //do! informer inbox st p2 (Info.RemoveCard c |> Info)
                        let (pd1, pd2) = Map.find p1 pls, Map.find p2 pls
                        let (pd2, (pd1, chest)) = Player.give pd2 pd1 c
                        match chest with 
                        | None -> ()
                        | Some rank -> 
                            for p in p1::p2::xs do
                                do! informer inbox st p (CompileChest(p1, rank) |> Info)
                        return {st with Players = Map.add p1 pd1 pls |> Map.add p2 pd2}
                    }
                do! informer inbox st p1 (Info <| Info.TakeCards(p2, cards) )
                do! informer inbox st p2 (Info <| Info.GiveCards(p1, cards) )
                let st = 
                    cards |> Set.fold (fun state x -> give x state |> Async.RunSynchronously) st
                return true, st
        }
    let rec loopMoveCircle ({ MoveCircle = mc; Data = {Players = pls} as dataState } as st) =
        async {
            match mc with
            | MoveCircleT.EndMoveCircle -> return st
            | MoveCircleT.GetMove(x, fcont) -> 
                let! (res, data) = getMove x dataState
                return! loopMoveCircle {MoveCircle = fcont res; Data = data}
            | MoveCircleT.PlayerHaveCards(pId, f) ->
                return! loopMoveCircle {st with MoveCircle = Player.haveCards pls.[pId] |> f}        
        }

    loopMoveCircle st
let loopCircle inbox plsId st = 
    let rec loop ({ PCircle = pcl; Data = {Deck = d; Players = pls} as dataState} as st) =
        let pUpdate id v = Map.add id v pls
        match pcl with
        | PlayersCircleT.DeckIsEmpty f -> 
            loop {st with PCircle = Deck.isEmpty d |> f}
        | PlayersCircleT.End -> 
            requester inbox dataState (fun _ _ -> Answ.EndGame, None)
        | PlayersCircleT.MoveCircle(pl, f) ->
            async {
                let! r = 
                    loopMoveCircle inbox { MoveCircle = moveCircle pl; Data = {Deck = d; Players = pls}}
                let st = { PCircle = f(); Data = r.Data }
                return! loop st
            }
        | PlayersCircleT.PlayerHaveCards(pl, f) ->
            loop {st with PCircle = Player.haveCards pls.[pl] |> f}
        | PlayersCircleT.PlayerTakeCardFromDeck(pId, f) ->
            async {
                let ((p, chest), d, card) = Player.takeFromDeck2 pls.[pId] d
                do! informer inbox dataState pId (Info(AddCardFromDeck card))
                match chest with
                | None -> do ()
                | Some rank ->
                    for p in plsId do
                        do! informer inbox dataState p (Info(CompileChest(pId, rank)))
                return! loop {PCircle = f(); Data = {Deck = d; Players = pUpdate pId p} }
            }
        | PlayersCircleT.Fail str -> failwith str
    loop st
let mail plsId =
    let init =
        let pCircleSt = playersCircle (List.ofSeq plsId)
//        assert
//            let pls = List.init 2 (fun _ -> { Player.Cards = Set.empty; Player.Chests = Set.empty})
//            true
        let pls = List.init (Set.count plsId) (fun _ -> { Player.Cards = Set.empty; Player.Chests = Set.empty})
        let d = Deck.initShuffle()
        let (d, pls) = 
            let f (d,pls) p =
                let (p, d) = Player.takeFromDeck p d
                (d, p::pls)
//            assert
//                List.fold f (d, []) pls
//                |> fun (d, pls) -> List.fold f (d, []) pls
//                |> fst
//                true
            List.fold f (d, []) pls
            |> fun (d, pls) -> List.fold f (d, []) pls
            |> fun (d, pls) -> List.fold f (d, []) pls
            |> fun (d, pls) -> List.fold f (d, []) pls

        let pls = List.zip (List.ofSeq plsId) pls |> Map.ofList
        { PCircle = pCircleSt; Data = {Deck = d; Players = pls}}
    MailboxProcessor.Start (fun inbox -> loopCircle inbox plsId init)

assert
    let m = mail <| Set["first"; "second"]
    let first msg = m.PostAndReply(fun x -> Post("first", msg,x))
    let second msg = m.PostAndReply(fun x -> Post("second", msg,x))
//    first GameReq.GetState
//    second GameReq.GetState
//    first <| (Input <| GameReq.RankInput 10)

    true