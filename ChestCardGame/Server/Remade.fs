module Remade

#if INTERACTIVE
#r @"E:\Project\ChestCardGame\ChestCardGame\CommandInterpriter\bin\Debug\net45\CommandInterpriter.dll"
#endif
open CommandInterpriter

module List =
    let next l =
        //List.permute(fun x -> (x + 1) % List.length l) l // [3;1;2] -> [2;3;1]
        [ yield! List.tail l; yield List.head l]

    // let others current l =
    //     if not <| List.exists ((=) current) l then failwith "такой элемент в списке отсутствует"
    //     let to' = Seq.takeWhile ((<>) current) l
    //     let from = Seq.skipWhile ((<>) current) l
    //     [ yield! Seq.skip 1 from; yield! to']
    (*
    // test
    others 1 [1..5] // [2;3;4;5]
    others 3 [1..5] // [4;5;1;2]
    others 5 [1..5] // [1;2;3;4]
    others 6 [1..5] // fail
    others 1 []     // fail
    others 1 [1]    // [] *)

    // let nextWhile func xs =
    //     let rec f func acc = function
    //         | [] -> None
    //         | h::t as pls ->
    //             if func h then
    //                 pls @ (List.rev acc) |> Some
    //             else
    //                 f func (h::acc) t
    //     f func [] xs
    // assert
    //     List.forall id [
    //         nextWhile ((=) 10) [0..20] = Some[10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9];
    //         nextWhile ((=) -1) [0..20] = None
    //         nextWhile ((=) -1) [] = None;
    //         nextWhile (fun _ -> true) ([]:int list) = None;
    //     ]

// module Intuitive =
//     module List =
//         let next l = [ yield! List.tail l; yield List.head l]
//     type Player =
//         { Name:string; ReadLine: unit -> string; WriteLine: string -> unit}
//     let rec gameCircle = function
//         | h::t as xs ->
//             let msg = h.ReadLine()
//             t |> List.iter (fun x -> x.WriteLine(sprintf "player %s send: %s." h.Name msg ))
//             List.next xs |> gameCircle
//         | _ -> ()
//     type Cmd = ReadLine | WriteLine of string
//     type Ty(users:Set<_>) =
//         let mutable m_suspend = false
//         let mutable m_input = ""
//         let mutable m_state = "", ReadLine
//         let suspend () =
//             if m_suspend then failwith "already suspend"
//             m_suspend <- true
//             printfn "%A" m_state
//             while m_suspend do System.Threading.Thread.Sleep 100
//         do
//             let getInput () = let input = m_input in m_input <- ""; input
//             let read p () =
//                 m_state <- p, ReadLine
//                 suspend()
//                 getInput ()
//             let write p s =
//                 m_state <- p, WriteLine s
//                 suspend()
//             users |> List.ofSeq
//             |> List.map (fun name -> {Name = name; ReadLine = read name; WriteLine = write name} )
//             |> (fun x -> async {gameCircle x} |> Async.Start)
//         member __.State = m_state
//         member __.Input s =
//             if m_suspend |> not then failwith "not suspend for input"
//             m_input <- s
//             m_suspend <- false
//         member __.Continue () =
//             if m_suspend |> not then failwith "not suspend for continue"
//             m_suspend <- false

module Continues =
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
                            | false -> End
                            | true -> Fail "deck is empty, one player not over")
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
            let rec f = function
                | h::t as pls ->
                    PlayerHaveCards(h, function
                        | false -> f t
                        | true  ->
                            GetMove((curr, h, t), function
                                | true  -> f (List.next pls)
                                | false -> EndMoveCircle))
                | [] -> EndMoveCircle
            f t

module Prog =
    type PlayersCircleT =
        | DeckIsEmpty of (bool -> PlayersCircleT)
        | PlayerHaveCards of PlayerId * (bool -> PlayersCircleT)
        | PlayerTakeCardFromDeck of PlayerId * (unit -> PlayersCircleT)
        | End
        | Fail of string
        | GetMove of (PlayerId * PlayerId) * (bool -> PlayersCircleT)

    let moveCircle curr pls next =
        // match pls with
        // | [] -> failwith "pls only one"
        // | t ->
        let rec f = function
            | h::t as pls ->
                PlayerHaveCards(h, function
                    | false -> f t
                    | true  ->
                        GetMove((curr, h), function
                            | true  -> f (List.next pls)
                            | false -> next))
            | [] -> next
        f pls
    let rec afterDeckEnd moveCircle = function
        | [h] ->
            PlayerHaveCards(h, function
                | false -> End
                | true -> Fail "deck is empty, one player have cards")
        | h::t as pls ->
            moveCircle h t (
                PlayerHaveCards(h, fun res ->
                let pl = if res then List.next pls else t
                afterDeckEnd moveCircle pl))
        | [] -> End
    let playersCircle moveCircle afterDeckEnd =
        let rec f pls = 
            DeckIsEmpty(function
                | false ->
                    match pls with
                    | [h] -> PlayerTakeCardFromDeck(h, fun () -> f [h] )
                    | h::t as pls ->
                        moveCircle h t (
                            PlayerTakeCardFromDeck(h, fun () ->
                            List.next pls |> f))
                    | [] -> Fail "deck not empty, player empty"
                | true -> afterDeckEnd pls)
        f
    let playersCircleFin =
        playersCircle moveCircle (afterDeckEnd moveCircle)

type Deck = Deck of PlayingCard list
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Deck =
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
        FsharpMyExtension.List.List.shuffle xs |> Deck
    let isEmpty = function
        | Deck [] -> true
        | _ -> false
type Player = { Cards:Set<PlayingCard>; Chests:Set<Rank> }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Player =
    // let takeFromDeck ({ Cards = cards} as p) d =
    //     let (card, d') = Deck.take d
    //     {p with Cards = cards.Add card }, d'
    let addCard p card =
        let cards = p.Cards |> Set.filter (fun x -> x.Rank = card.Rank)
        if Set.count cards = Deck.suitVarCount - 1 then
            {
                Cards = p.Cards - cards
                Chests = Set.add card.Rank p.Chests
            }, Some card.Rank
        else
            { p with Cards = Set.add card p.Cards }, None
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
        let p1 = { p1 with Cards = Set.remove card p1.Cards }
        let p2 = addCard p2 card
        p1, p2

