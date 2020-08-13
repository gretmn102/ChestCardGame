module RemadeOld
open CommandInterpriter
open GameReq
open ServerAnswer
open GameAnswer

open Remade
open Continues
type Msg =
    | Post of PlayerId * Req * AsyncReplyChannel<Answ>

type DataState = { Deck: Deck; Players: Map<PlayerId, Player> }
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
            else Wait pId, None
        | Input(inputs) ->
            if pIdcurr = pId then inputState inputs
            else FailAnsw, None
        | _ -> FailAnsw, None
    requester inbox dataState interp

let informer inbox dataState pId msg =
    let f pId' = function
        | GetState ->
            if pId' = pId then msg, Some ()
            else Wait pId, None
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
                    | RankInput x -> Info <| Success true, Some x
                    | _ -> FailAnsw, None
                inputer inbox st p1 GetRank f
            let countInputer rank () =
                let f = function
                    | CountInput x -> Info <| Success true, Some x
                    | _ -> FailAnsw, None
                inputer inbox st p1 <| GetCount rank <| f
            let suitInputer rank count () =
                let f = function
                    | SuitInput x -> Info <| Success true, Some x
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
                do! informer inbox st p1 (Info <| TakeCards(p2, cards) )
                do! informer inbox st p2 (Info <| GiveCards(p1, cards) )
                let st =
                    cards |> Set.fold (fun state x -> give x state |> Async.RunSynchronously) st
                return true, st
        }
    let rec loopMoveCircle ({ MoveCircle = mc; Data = {Players = pls} as dataState } as st) =
        async {
            match mc with
            | EndMoveCircle -> return st
            | GetMove(x, fcont) ->
                let! (res, data) = getMove x dataState
                return! loopMoveCircle {MoveCircle = fcont res; Data = data}
            | PlayerHaveCards(pId, f) ->
                return! loopMoveCircle {st with MoveCircle = Player.haveCards pls.[pId] |> f}
        }

    loopMoveCircle st
let loopCircle inbox plsId st =
    let rec loop ({ PCircle = pcl; Data = {Deck = d; Players = pls} as dataState} as st) =
        let pUpdate id v = Map.add id v pls
        match pcl with
        | DeckIsEmpty f ->
            loop {st with PCircle = Deck.isEmpty d |> f}
        | End ->
            requester inbox dataState (fun _ _ -> EndGame, None)
        | MoveCircle(pl, f) ->
            async {
                let! r =
                    loopMoveCircle inbox { MoveCircle = moveCircle pl; Data = {Deck = d; Players = pls}}
                let st = { PCircle = f(); Data = r.Data }
                return! loop st
            }
        | PlayersCircleT.PlayerHaveCards(pl, f) ->
            loop {st with PCircle = Player.haveCards pls.[pl] |> f}
        | PlayerTakeCardFromDeck(pId, f) ->
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
        | Fail str -> failwith str
    loop st
let mail plsId =
    let init =
        let pCircleSt = playersCircle (List.ofSeq plsId)

        let pls = List.init (Set.count plsId) (fun _ -> { Player.Cards = Set.empty; Player.Chests = Set.empty})
        let d = Deck.initShuffle()
        let (d, pls) =
            let f (d,pls) p =
                let ((p,_), d, _) = Player.takeFromDeck2 p d
                (d, p::pls)
            List.fold f (d, []) pls
            |> fun (d, pls) -> List.fold f (d, []) pls
            |> fun (d, pls) -> List.fold f (d, []) pls
            |> fun (d, pls) -> List.fold f (d, []) pls

        let pls = List.zip (List.ofSeq plsId) pls |> Map.ofList

        { PCircle = pCircleSt; Data = {Deck = d; Players = pls}}
    MailboxProcessor.Start (fun inbox -> loopCircle inbox plsId init)

