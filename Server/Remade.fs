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
        List.forall id [
            nextWhile ((=) 10) [0..20] = Some[10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9];
            nextWhile ((=) -1) [0..20] = None
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
        let (card, d') = Deck.take d
        {p with Cards = cards.Add card }, d'
    let haveCards { Cards = cards } = Set.isEmpty cards |> not
    let haveCard card { Cards = cards } = Set.contains card cards
    let give p1 p2 card = //({ Cards = c1 } as p1) { Cards = c2 } = 
        let p1 = {p1 with Cards = Set.remove card p1.Cards}
        let p2 = {p2 with Cards = Set.add card p2.Cards}
        p1, p2


module Intuitive =
    open System
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
            (*
            let read = Read (List.head xs)
            let write m = WriteAll(m, List.tail xs), f (next xs)
            Continue(read, write) *)
            //List.foldBack (fun x state -> ) xs
            (*
            let msg = "some msg"
            let writers = 
                (fun () -> Write(("first", msg), 
                    (fun () -> Write(("second", msg),
                        (fun () -> Write(("three", msg),
                            (fun () -> f xs))))))) *)
            let writers msg = 
                (fun () -> f (List.tail xs))
                |> List.foldBack (fun x state -> (fun () -> Write((x, msg), state))) xs
            Read(fun msg -> writers msg ())
        
    type T = 
        | MoveCircle of int list * (unit -> T)
        | DeckIsEmpty of (bool -> T)
        | PlayerHaveCards of int * (bool -> T)
        | PlayerTakeCardFromDeck of int * (unit -> T)
        | End
        | Fail of string
    let rec plCircle pls =
        DeckIsEmpty(fun res ->
            match res, pls with
            | false, [h] -> PlayerTakeCardFromDeck(h, fun () -> plCircle [h] )
            | false, [] -> Fail "deck not empty, player empty"
            | false, (h::_ as pls) ->
                MoveCircle(pls, fun () ->
                    PlayerTakeCardFromDeck(h, fun () ->
                            List.next pls |> plCircle))
            | true, xs->
                let rec f = function
                    | [] -> End
                    | [h] -> 
                        PlayerHaveCards(h, function
                            | true -> Fail "deck is empty, one player not over"
                            | false -> End)
                    | h::t ->
                        MoveCircle(pls, fun () ->
                            PlayerHaveCards(h, fun res ->
                                let pl = if res then List.next pls else t
                                f pl))
                f xs
                )
    type T2 =
        | EndMoveCircle
        | GetMove of (int * int) * (bool -> T2)
        //| PlayersHaveCards of int list * (int list option -> Ty2 )
        | PlayerHaveCards of int * (bool -> T2)
    let moveCircle = function
        | [] -> failwith "pls is empty"
        | [_] -> failwith "pls only one"
        | curr::t -> 
            let rec f xs =
                (*
                let nextWhile cont xs =
                    let rec f acc = function
                        | [] -> EndMoveCircle
                        | h::t as pls -> 
                            PlayerHaveCards(h, function
                                | true -> pls @ (List.rev acc) |> cont
                                | false -> f (h::acc) t)
                    f [] xs *)
                let nextWhile cont xs =
                    let rec f = function
                        | [] -> EndMoveCircle
                        | h::t as pls -> 
                            PlayerHaveCards(h, function
                                | true -> cont pls
                                | false -> f t)
                    f xs
                let f'' = function
                    | h::_ as xs -> 
                        GetMove((curr, h), function 
                            | true -> f (List.next xs)
                            | false -> EndMoveCircle)
                    | [] -> failwith "nextWhile has return Some([])"
                xs |> nextWhile f''
            f t
