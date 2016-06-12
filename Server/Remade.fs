﻿module Remade

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
        
        (*
        if h.TakeCardFromPack() = false && h.HaveCards() = false then t
        else players
        |> List.next |> playerCircle *)
type reqBig = 
    | MMove of deck * player list
type respBig = 
    | MoveSuccess of player * player list


type client = 
    | Next of player * player list
    //| Move of ('a * 'a list) * bool
    | MMove of deck * player list
    | MoveSuccess of player * player list
type server = 
    | Stop of player * player list
    | GetMove of player * player list
    | GetMMove of deck * player list
    | Inform of string
    | EndGame
let moves havecard move curr rest = 
    let rec moves curr = function
        | [] -> curr, []
        | h::t as pls ->
            if havecard h then
                match move curr h with
                | Some(curr, h) -> moves curr (List.next (h::t))
                | None -> curr, pls
            else moves curr t
    moves curr rest

let maincircle x = 
    let moo informs curr rest = 
        let rec moo informs acc curr = function
            | [] -> Stop(curr, acc)::informs
            | h::t as pls ->
                if Player.haveCards h then GetMove(curr, pls@acc)::Inform (sprintf "%A have cards" h)::informs
                else moo informs [ yield! acc; yield h] curr t
        moo informs [] curr rest
    match x with
    //| Start(d, pls) -> Next(
    | MMove(Deck [], [p]) -> failwith "deck is empty, one player not over"
    | MMove(Deck [], []) -> [EndGame]
    | MMove(d, []) -> failwith "deck is not empty"
    | MMove(Deck [] as d, (p::t as players)) ->
                if Player.haveCards p then List.next players
                else List.next t
                |> (fun x -> [GetMMove(d, x)])
    | MMove(d, p::t) ->
                let (p, d) = Player.takeFromDeck p d
                let informs = [Inform "pl take card"]
                GetMMove(d, List.next (p::t))::informs
    | Next(curr, rest) -> moo [] curr rest
    | MoveSuccess(curr, pls) -> moo [] curr (List.next pls)
    //| Move((curr, []), true) -> failwith "pls empty"
    //| Move((curr, pls), true) -> moo [] curr (List.next pls)
    //| Move((curr, pls), false) -> [Stop(curr, pls)]

assert 
    let test = 
        let plEmpty id = {Id = id; Cards = set[ ] }
        let plHave id = {Id = id; Cards = set[ {Rank = 0; Suit = 1} ] }
        let rec next l = function 0 -> l | n -> next (List.next l) (n-1)
        
        let pls = [ plEmpty 1; plEmpty 2; plEmpty 3; plEmpty 4 ]
        let asse = maincircle(Next(plEmpty 10, pls)) |> List.head
        let guess = server.Stop(plEmpty 10, pls)
        if asse <> guess then failwith ""
        
        let pls = [ plEmpty 1; plEmpty 2; plEmpty 3; plHave 4 ]
        let ass = maincircle(Next(plEmpty 10, pls)) |> List.head
        let guess = server.GetMove(plEmpty 10, next pls 3)
        if ass <> guess then failwith ""

        let pls = [ plHave 1; plEmpty 2; plHave 3; plHave 4 ]
        let ass = maincircle(MoveSuccess(plEmpty 10, pls)) |> List.head
        let guess = server.GetMove(plEmpty 10, next pls 2)
        if ass <> guess then failwith ""
        let pls = [ plHave 1; plEmpty 2; plEmpty 3; plEmpty 4 ]
        let ass = maincircle(MoveSuccess(plEmpty 10, pls)) |> List.head
        let guess = server.GetMove(plEmpty 10, pls)
        if ass <> guess then failwith ""
    true

module main =
    type add = 
        | GetMove of player * player list
        | Stop of player * player list
    
    type client = 
        | Start of deck * player list
        | Next of player * player list
        | MMove of deck * player list
        | MoveSuccess of player * player list * int * deck
        | MoveFail of player * player list * int * deck
    type server = 
        | Get of player * player list * int * deck
        | GetMMove of deck * player list
        | Inform of string
        | EndGame 
    let rec maincircle x = 
        let moo curr rest = 
            let rec moo acc curr = function
                | [] -> Stop(curr, acc)
                | h::t as pls ->
                    if Player.haveCards h then GetMove(curr, pls@acc)
                    else moo [ yield! acc; yield h] curr t
            moo [] curr rest
        let rec jump reqId = function
            | {Id = x}::rest as l -> if x = reqId then l else jump reqId (List.next l)
            | [] -> failwith "list is empty"
        let rec endmove id dp =
            //let f d p t = 

            let f' = function
                | Deck [], [] -> None
                | Deck [], [p] -> failwith "deck is empty, one player not over"
                | d, [] -> failwith "deck is not empty"
                | Deck [] as d, (p::t as pls) ->
                    let pls = 
                        if Player.haveCards p then List.next pls
                        else List.next t
                    Some(d, List.head pls, List.tail pls)
                | d, p::t ->
                    let (p, d) = Player.takeFromDeck p d
                    Some(d, p, t)
            match f' dp with
            | Some(d, p, rest) ->
                match moo p rest with
                | GetMove(p, otherPls) -> Get(p, otherPls, id, d)
                | Stop(p, rest) -> endmove id (d, p :: (jump id rest)) //|> Start |> maincircle
            | None -> EndGame
                    //f d p t
                //let informs = [Inform "pl take card"]
                //GetMMove(d, List.next (p::t))::informs

            
        match x with
        | Start(Deck [], []) -> EndGame
        | Start(Deck [], [p]) -> failwith "deck is empty, one player not over"
        | Start(d, []) -> failwith "deck is not empty"
        | Start(d, (p::({Id = id}::_ as t) as pls)) ->
            match moo p t with
            | GetMove(p, otherPls) -> Get(p, otherPls, id, d)
            | Stop(p, rest) -> (d, p :: (jump id rest)) |> Start |> maincircle
        | MoveSuccess(curr, pls, id, d) ->
            match moo curr pls with
            | GetMove(p, otherPls) -> Get(p, otherPls, id, d)
            | Stop(p, rest) -> (d, p :: (jump id rest)) |> Start |> maincircle
        | MoveFail(curr, pls, id, d) -> (d, curr :: (jump id pls)) |> Start |> maincircle        
 (*
            circle (d, pls)
        //| Start(d, pls) -> Next(
        | MMove(Deck [], [p]) -> failwith "deck is empty, one player not over"
        | MMove(Deck [], []) -> [EndGame]
        | MMove(d, []) -> failwith "deck is not empty"
        | MMove(Deck [], (p::t as players)) ->
                    if Player.haveCards p then List.next players
                    else List.next t
                    |> (fun x -> [GetMMove(d, x)])
        | MMove(d, p::t) ->
                    let (p, d) = Player.takeFromDeck p d
                    let informs = [Inform "pl take card"]
                    GetMMove(d, List.next (p::t))::informs
        | Next(curr, rest) -> moo [] curr rest
        | MoveSuccess(curr, pls) -> moo [] curr (List.next pls) *)

module mod1 = 
    (*
    type req =
        | Start of deck * player list
    type resp =
        | GetMove of player * player * (deck *  
        | State of deck * player list *)

    let secCircle deck pls =
        let aswer reqId = function
            | server.Stop(curr, pls) ->
                let rec f reqId = function
                    | {Id = x}::rest as l -> if x = reqId then l else f reqId (List.next l)
                    | [] -> failwith""
                f reqId pls
            | _ -> failwith ""
        
        Next(List.head pls, List.tail pls) |> maincircle

type ClientCmd =
    | GetState
type ServerCmd =
    | Wait
    | YourTurn
    | Error of string
type msg = Post of int * ClientCmd * AsyncReplyChannel<ServerCmd>
let mail deck pls = MailboxProcessor.Start(fun inbox -> 
    let rec loop deck msgs = async {
        let! Post(curr, cmd, reply) = inbox.Receive()
        let rep = 
            match cmd with
            | GetState -> 
                match msgs with
                | [] -> Wait, None
                | server.GetMove({Id = id; Cards = cards}, pls)::_ -> if id = curr then YourTurn, None else Wait, None
                | x -> Error (sprintf "%A" x), None
        printfn "%A" rep
        match rep with
        | cmd, None -> reply.Reply cmd; return! loop deck msgs
        | _ -> reply.Reply (Error ""); return! loop deck msgs
        }
    Next(List.head pls, List.tail pls) |> maincircle |> loop deck//(maincircle (Next(List.head pls, List.tail pls)))
    )
let start() = 
//assert
    let pls = 
        let plEmpty id = {Id = id; Cards = set[ ] }
        let plHave id = {Id = id; Cards = set[ {Rank = 0; Suit = 1} ] }
        Seq.init 3 (((+)1) >> plHave) |> List.ofSeq
    let deck = Deck[{ Rank = 0; Suit = 0 }]
        
    let t pls = (maincircle (Next(List.head pls, List.tail pls)))
    t pls

    let m = mail deck pls
    m.PostAndReply(fun x -> Post(1, GetState, x) )
    true
    
(*
let move moves = function
    | p::(otherFirst::t as others)->
        let rec f l = if List.head l = otherFirst then l else f (List.next l)
        let (p::others) = moves p others
        p :: f others
    | _ -> failwith ""
    //| [] -> ()

move (fun x xs -> x :: List.next xs) [1..4]
   
   *)