module Remade

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
        List.forall id 
            [nextWhile ((=) -1) [0..20] = Some[10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9];
            nextWhile ((=) -1) [] = None;
            nextWhile (fun _ -> true) ([]:int list) = None;
            ]

type playingCard = { Rank:int; Suit:int }
type deck = Deck of playingCard list
module Deck =
    let take = function
        | Deck [] -> failwith "card is no more in deck"
        | Deck (h::t) -> h, Deck t
    let init = 
        [ for suit in [ 0..3 ] do
            for rank in 0..13 -> { Rank = rank; Suit = suit } ]
    let isEmpty = function
        | Deck [] -> true
        | _ -> false

type player = { Id:int; Cards:Set<playingCard> }
module Player = 
    let takeFromDeck ({Cards = cards} as p) d =
        let (card, dRest) = Deck.take d
        {p with Cards = cards.Add card }, dRest
    let haveCards { Cards = cards } = Set.count cards <> 0
    let haveCard card { Cards = cards } = Set.contains card cards
    let give p1 p2 card = //({ Cards = c1 } as p1) { Cards = c2 } = 
        let p1 = {p1 with Cards = Set.remove card p1.Cards}
        let p2 = {p2 with Cards = Set.add card p2.Cards}
        p1, p2

module simple = 
    (*
    let playerCircle move = function
        | Deck [], [p] -> failwith "deck is empty, one player not over"
        | (d, []) -> None
        | (d, pls) ->
                match move pls with
                | p::t as players ->
                    if Deck.isEmpty d then
                        if Player.haveCards p then List.next players
                        else List.next t
                        |> (fun x -> Some(d, x))
                    else
                        let (pl, dRest) = Player.takeFromDeck p d
                        Some(dRest, List.next (pl::t))
                | [] -> failwith ""
                *)
    module moveCircle = 
        (*
        type server = 
            | GetMove of (player * player) * (player list * player list)
            | Stop of player * player list
        type client = 
            | Start of player * player list
            | MoveSuccess of (player * player) * (player list * player list)
            | MoveFail of (player * player) * (player list * player list)

        let moveCircle = 
            let rec moo acc curr = function
                | [] -> Stop(curr, acc)
                | h::t as pls ->
                    if Player.haveCards h then GetMove((curr, h), (acc, t))
                        (*
                        match getMove(curr, h) with
                        | Some(curr, h) -> moo [ yield! acc; yield h] curr t
                        | None -> curr, pls@acc *)
                    else moo [ yield! acc; yield h] curr t
            (function
                | Start (curr, rest) ->  moo [] curr rest
                | MoveSuccess((curr, h), (acc, rest)) -> moo [ yield! acc; yield h ] curr rest
                | MoveFail((curr, h), (acc, rest)) -> Stop(curr, (h::rest)@acc))
        assert
            let plEmpty id = {Id = id; Cards = set[ ] }
            let plHave id = {Id = id; Cards = set[ {Rank = 0; Suit = 1} ] }
            let rec next l = function 0 -> l | n -> next (List.next l) (n-1)

            let pls = [plEmpty 0; plEmpty 1; plHave 2 ]
            let x1 = moveCircle(Start(List.head pls, List.tail pls))
            let x2 = 
                match x1 with
                | GetMove(({Id = p1}, {Id = p2}), rest) -> MoveSuccess((plHave p1, plEmpty p2), rest) |> moveCircle
            x2 = Stop(plHave 0, [plEmpty 1; plEmpty 2]) *)
        type server = 
            | GetMove of (player * player) * player list
            | Stop of player * player list
        type client = 
            | Start of player * player list
            | MoveSuccess of (player * player) * player list
            | MoveFail of (player * player) * player list
        let moveCircle = 
            let moo curr rest = 
                match List.nextWhile Player.haveCards rest with
                | None -> Stop(curr, rest)
                | Some(h::t as pls) ->
                    GetMove((curr, h), t)
                | Some [] -> failwith "nextWhile has return Some([])"
                    //raise(failwith "nextWhile has return Some([])")
                    //if Player.haveCards h then GetMove((curr, h), (acc, t))
                        (*
                        match getMove(curr, h) with
                        | Some(curr, h) -> moo [ yield! acc; yield h] curr t
                        | None -> curr, pls@acc *)
                    //else moo [ yield! acc; yield h] curr t
            (function
                | Start (curr, rest) ->  moo curr rest
                | MoveSuccess((curr, h), rest) -> moo curr [ yield! rest; yield h ]
                | MoveFail((curr, h), rest) -> Stop(curr, h::rest))
        assert
            let plEmpty id = {Id = id; Cards = set[ ] }
            let plHave id = {Id = id; Cards = set[ {Rank = 0; Suit = 1} ] }
            let rec next l = function 0 -> l | n -> next (List.next l) (n-1)

            let pls = [plEmpty 0; plEmpty 1; plHave 2; plHave 3 ]
            let x1 = moveCircle(Start(List.head pls, List.tail pls))
            let x2 = 
                match x1 with
                | GetMove(({Id = p1}, {Id = p2}), rest) -> MoveSuccess((plHave p1, plEmpty p2), rest) |> moveCircle
                | _ -> failwith""
            let x3 = 
                match x2 with
                | GetMove(({Id = p1}, {Id = p2}), rest) -> MoveSuccess((plHave p1, plEmpty p2), rest) |> moveCircle
                | _ -> failwith""
            x3 = Stop(plHave 0, [plEmpty 1; plEmpty 2; plEmpty 3])
           
    module moveCircle2 = 
        type server = 
            | GetMove of (player * player) * (player list * player list)
            | Stop of player * player list
        type client = 
            | Start of player * player list
            | MoveSuccess of (player * player) * (player list * player list)
            | MoveFail of (player * player) * (player list * player list)
        type server2 =
            | GetMove of player * player
            | Stop of player * player list

        type client2 =
            | Fetch of AsyncReplyChannel<server2>
            | MoveSuccess of player * player
            | MoveFail of player * player

        let mail curr rest =
            MailboxProcessor.Start(fun inbox ->
                let rec moo acc curr = function
                    | [] -> None, Stop(curr, acc)
                    | h::t as pls ->
                        if Player.haveCards h then 
                            Some(acc, t), GetMove(curr, h)
                            (*
                            match getMove(curr, h) with
                            | Some(curr, h) -> moo [ yield! acc; yield h] curr t
                            | None -> curr, pls@acc *)
                        else moo [ yield! acc; yield h] curr t
                let rec f value = 
                    match value with
                    | Some(acc, rest), cmd -> 
                        async {
                            let! d = inbox.Receive()
                            match d with
                            | Fetch fetch -> fetch.Reply cmd; return! f value
                            | MoveSuccess (p1, p2) -> 
                                return! moo [ yield! acc; yield p2 ] p1 rest |> f
                            | MoveFail(p1, p2) -> return! f(None, Stop(curr, (p2::rest)@acc))
                            }
                    | None, cmd ->
                        async {
                            let! d = inbox.Receive()
                            match d with
                            | Fetch fetch -> fetch.Reply cmd; return! f value
                            | _ -> failwith""
                            }
                moo [] curr rest |> f
                (*function
                    | Start (curr, rest) ->  moo [] curr rest
                    | MoveSuccess((curr, h), (acc, rest)) -> moo [ yield! acc; yield h ] curr rest
                    | MoveFail((curr, h), (acc, rest)) -> Stop(curr, (h::rest)@acc)) *)
            )
        do 
            let plEmpty id = {Id = id; Cards = set[ ] }
            let plHave id = {Id = id; Cards = set[ {Rank = 0; Suit = 1} ] }
            let rec next l = function 0 -> l | n -> next (List.next l) (n-1)
            let pls = [ plEmpty 0; plEmpty 1; plHave 2 ]
            let m = mail (List.head pls) (List.tail pls)
            //m.PostAndReply(fun x -> Fetch x) |> ignore
            
            //let (k:unit) = m.PostAndReply(fun x -> MoveSuccess(plEmpty 0, plHave 2))
            ()
    module moveCircle3 = 
        //type t =| Move of 'a * (player * player)
        ()
        (*
        let rec f = function
            | [] -> None, []
            | h::t -> 
                Some f, t *)
        (*
        let rec moo acc curr = function
        | [] -> None, None, Some(curr, acc)
        | h::t as pls ->
            let getMove acc t curr pls = function
                | Some(curr, h) -> moo [ yield! acc; yield h] curr t
                | None -> None, None, Some(curr, pls@acc)
            //if Player.haveCards h then //GetMove((curr, h), (acc, t))
            let f x = getMove acc t curr pls x
            Some(f), Some (curr, h), None
            //else moo [ yield! acc; yield h] curr t *)
    module mainCircle = 
        (*
        let playerCircleOrig move = function
            | Deck [], [p] -> failwith "deck is empty, one player not over"
//            | MMove(Deck [], []) -> [EndGame]
            | (d, []) -> failwith "deck is not empty"
            | (d, p::({Id = id}::_ as rest)) ->
                    let rec f cmd = 
                        match moveCircle.moveCircle cmd with
                        | moveCircle.GetMove _ as x -> move x |> f
                        | moveCircle.Stop(p, pls) ->
                            let pls = 
                                match List.nextWhile (fun {Id = idcurr} -> idcurr = id) pls with
                                | Some xs -> xs
                                | None -> failwithf "%d not found in %A" id pls
                            if Deck.isEmpty d then
                                match List.nextWhile Player.haveCards (List.next (p::pls)) with
                                | None -> None
                                | Some xs -> Some(d, xs)
                            else
                                let (pl, dRest) = Player.takeFromDeck p d
                                Some(dRest, List.next (p::pls))
                    f (moveCircle.Start(p, rest))
            | _ -> failwith "not impl" *)
        type server = 
            | EndGame
            | GetMove of moveCircle.server * (int * player list * deck * player)
            //| GetNext of deck * player list
        type client =
            | Start of deck * player list
            | Move of moveCircle.client * (int * player list * deck * player)
            //| Next of deck * player list

        let rec playerCircle cmd = 
            let endMove id pls d p =
                let pls = 
                    match List.nextWhile (fun {Id = idcurr} -> idcurr = id) pls with
                    | Some xs -> xs
                    | None -> failwithf "%d not found in %A" id pls
                if Deck.isEmpty d then
                    match List.nextWhile Player.haveCards (List.next (p::pls)) with
                    | None -> EndGame
                    | Some xs -> 
                        //GetNext(d, xs)
                        Start(d, xs) |> playerCircle
                else
                    let (pl, dRest) = Player.takeFromDeck p d
                    //GetNext(dRest, List.next (p::pls))
                    Start(dRest, List.next (p::pls)) |> playerCircle
            let startMove (cmd, (id, pls, d, p)) = 
                match moveCircle.moveCircle cmd with
                | moveCircle.GetMove _ as x -> GetMove(x, (id, pls, d, p)) //move x |> f
                | moveCircle.Stop(p, pls) -> endMove id pls d p

            let start = function
                | Deck [], [p] -> failwith "deck is empty, one player not over"
                | (Deck [], []) -> EndGame
                | (d, []) -> failwith "deck is not empty"
                | (d, p::({Id = id}::_ as rest)) -> startMove (moveCircle.Start(p, rest), (id, rest, d, p))
                | _ -> failwith "not impl"
            match cmd with
                | Start(d, pls) -> start(d, pls)
                | Move(x, y) -> startMove(x, y)
                //| Next(d, p) -> start(d, p)
        
        assert
            let plEmpty id = {Id = id; Cards = set[ ] }
            let plHave id = {Id = id; Cards = set[ {Rank = 0; Suit = 1} ] }
            let rec next l = function 0 -> l | n -> next (List.next l) (n-1)

            //let deck = Deck[{ Rank=0; Suit= 0 }]
            let deck = Deck []
            let pls = [plEmpty 0; plEmpty 1; plHave 2; plHave 3 ]
            let x1 = Start(deck, pls) |> playerCircle

            false

    type ClientCmd =
        | GetState
        | Ask of int * int
    type ServerCmd =
        | MoveFail
        | MoveSuccess
        | Wait
        | YourTurn
        | Error of string
    type msg = Post of int * ClientCmd * AsyncReplyChannel<ServerCmd>
    let mail deck pls = MailboxProcessor.Start(fun inbox -> 
        let rec loop = function
            | mainCircle.GetMove(x, stateMain) as stateCurr ->
                async { 
                    match x with
                    | moveCircle.GetMove(({Id = id} as p1, p2), state) ->
                        let! Post(curr, cmd, reply) = inbox.Receive()
                        let answer =
                            if curr = id then
                                match cmd with
                                | GetState -> None, YourTurn
                                | Ask(r, s) -> 
                                    let card = { Rank = r; Suit = s }
                                    if Player.haveCard card p2 then
                                        let (p2, p1) = Player.give p2 p1 card
                                        //let cmd = mainCircle.Move(moveCircle.MoveSuccess(pls, state), stateMain)
                                        let cmd = moveCircle.MoveSuccess((p1, p2), state)
                                        Some(cmd), MoveSuccess
                                    else
                                        let cmd = moveCircle.MoveFail((p1, p2), state)
                                        Some cmd, MoveFail
                            else
                                (*
                                match cmd with
                                | GetState -> None, Wait *) 
                                None, Wait
                        match answer with
                        | Some cmd, answer -> 
                            reply.Reply answer;
                            try
                                return! mainCircle.playerCircle(mainCircle.Move(cmd, stateMain)) |> loop
                            with
                                | e ->
                                    let err = sprintf "serverFail %s" e.Message
                                    let! Post(curr, cmd, reply) = inbox.Receive()
                                    reply.Reply(Error err)
                                    return! loop stateCurr
                        | None, answer ->
                            reply.Reply answer
                            return! loop stateCurr
                    | x ->  
                        let err = sprintf "serverFail %A" x
                        let! Post(curr, cmd, reply) = inbox.Receive()
                        reply.Reply(Error err)
                        return! loop stateCurr
                    }
            | mainCircle.EndGame as x->
                async {
                        let err = sprintf "serverFail %A" x
                        let! Post(curr, cmd, reply) = inbox.Receive()
                        reply.Reply(Error err)
                        return! loop x }
        mainCircle.playerCircle(mainCircle.Start(deck, pls)) |> loop
        )

    assert
        let plEmpty id = {Id = id; Cards = set[ ] }
        let plHave id = {Id = id; Cards = set[ {Rank = 0; Suit = 1} ] }
        let rec next l = function 0 -> l | n -> next (List.next l) (n-1)

        
        //let deck = Deck[{ Rank=0; Suit= 0 }]
        let deck = Deck []
        let pls = [plEmpty 0; plEmpty 1; plHave 2; plHave 3 ]
        let game = mail deck pls
        let post id cmd = game.PostAndReply(fun x -> Post(id, cmd, x))
        post 0 (Ask(0, 1))
        post 0 GetState
        false

open simple
assert
    let plEmpty id = {Id = id; Cards = set[ ] }
    let plHave id = {Id = id; Cards = set[ {Rank = 0; Suit = 1} ] }
    let rec next l = function 0 -> l | n -> next (List.next l) (n-1)

        
    //let deck = Deck[{ Rank=0; Suit= 0 }]
    let deck = Deck []
    let pls = [plEmpty 0; plEmpty 1; plHave 2; plHave 3 ]
    let game = mail deck pls
    let post id cmd = game.PostAndReply(fun x -> Post(id, cmd, x))
    post 0 GetState
    post 0 (Ask(0, 1))
    post 0 GetState
    false