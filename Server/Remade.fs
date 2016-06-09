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
    let haveCards { Cards = cards } = Set.isEmpty cards
        
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
type client<'a, 'b> = 
    | Next of 'a * 'a list
    | Move of ('a * 'a list) * bool
    | MMove of 'b * 'a list
type server<'a, 'b> = 
    | Stop of 'a * 'a list
    | GetMove of 'a * 'a list
    | GetMMove of 'b * 'a list
    
    | Inform of string

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
    let rec moo informs curr = function
        | [] -> Stop(curr, [])::informs
        | h::t as pls ->
            if Player.haveCards h then GetMove(curr, pls)::Inform (sprintf "%A have cards" h)::informs
            else moo informs curr t

    match x with
    | MMove(Deck [], [p]) -> failwith "deck is empty, one player not over"
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
    | Move((curr, []), true) -> failwith "pls empty"
    | Move((curr, pls), true) -> moo [] curr (List.next pls)
    | Move((curr, pls), false) -> [Stop(curr, pls)]
    
let test2 = 
    
    let d = Deck [ {Rank = 0; Suit = 0} ]
    let pls = [ {Id = 1; Cards = set[ ] }; ] //{Id = 2; Cards = set[ ] } ]
    maincircle (MMove(d, pls))
    |> (function GetMMove(d, pls)::_ -> maincircle (MMove(d, pls)))
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