module Sandbox
open Remade

// #if INTERACTIVE
// #r @"E:\Project\ChestCardGame\ChestCardGame\CommandInterpriter\bin\Debug\net45\CommandInterpriter.dll"
// #endif

open CommandInterpriter
open GameAnswer
open GameReq
// open Remade.Prog

type DataState = { Deck: Deck; Players: Map<PlayerId, Player>; PlayersIds:PlayerId Set }

type T =
    | Move of (PlayerId * PlayerId * PlayerId list) * (unit -> T)
    | GetRank of (Rank -> bool * (unit -> T))
    | GetCount of (int -> bool * (unit -> T))
    | GetSuit of (Suit list -> bool * (unit -> T))
    | Gives of PlayingCard list * (unit -> T)

    | CompiledChest of (PlayerId * PlayerId list * Rank) * (unit -> T)

    /// Сообщить остальным игрокам, что игрок X берет карту
    | TakeFromDeck of (PlayerId * PlayerId list * PlayingCard) * (unit -> T)
    // | XTakeFromDeck of (PlayerId list * PlayerId) * (unit -> T)
    // | PlayerTakeCardFromDeck of PlayerId * (unit -> T)

    | EndGame of DataState
    | Fail of string

let move x y prog st =
    let gives p1 p2 cards =
        let g (p1, p2, chests) card =
            let p1, (p2, chest) = Player.give p1 p2 card
            let chests =
                match chest with
                | Some r -> r::chests
                | None -> chests
            p1, p2, chests
        cards |> List.fold g (p1, p2, [])

    let (>>=) cond next =
        if cond then next
        else (fun () -> prog st false)
        |> fun x -> cond, x

    let p1 = Map.find x st.Players
    let p2 = Map.find y st.Players
    let other = Set.remove x st.PlayersIds |> Set.remove y |> Set.toList
    let gives cards =
        let chests, st =
            let p2,p1,chests = gives p2 p1 cards
            chests, { st with Players = Map.add x p1 st.Players |> Map.add y p2 }
        let st =
            chests
            |> List.fold (fun st r ->
                    fun () -> CompiledChest((x, y::other, r), st))
                (fun () -> prog st true)
        fun () -> Gives(cards, st)
    Move((x, y, other), fun () ->
    GetRank(fun rank ->
        let cards = Player.filterRank rank p2
        not (Set.isEmpty cards) >>= fun () -> GetCount(fun count ->
        Set.count cards = count >>= fun () -> GetSuit(fun suits ->
            let cards' = suits |> List.map (fun suit -> { Rank = rank; Suit = suit } )
            cards = (cards' |> Set.ofList) >>= gives cards'
        ))
    ))

let rec prog st = function
    | Prog.DeckIsEmpty f ->
        f (Deck.isEmpty st.Deck)
        |> prog st
    | Prog.GetMove((x,y), f) ->
        move x y (fun st res -> prog st (f res)) st
    | Prog.PlayerHaveCards(x, f) ->
        Map.find x st.Players
        |> Player.haveCards
        |> f |> prog st
    | Prog.PlayerTakeCardFromDeck(x, f) ->
        let (p, chest), d, card =
            let p = Map.find x st.Players
            Player.takeFromDeck2 p st.Deck
        let st = { st with Deck = d; Players = Map.add x p st.Players }
        let other = Set.remove x st.PlayersIds |> List.ofSeq
        let g next =
            match chest with
            | Some r ->
                fun () ->
                    CompiledChest((x, other, r), next)
            | None -> next
        TakeFromDeck((x, other, card), g (f >> prog st))
    | Prog.End -> EndGame st
    | Prog.Fail x -> Fail x

type State = { PCircle: Prog.PlayersCircleT; Data:DataState; Msgs:Map<PlayerId, Answ list> }

// let start xs d =
//     { PCircle = Prog.playersCircleFin xs; Data = d; Msgs = Map.empty }


// let game (st:State) = function
//     | GetGameInfo ->


//         ()

type Post = (PlayerId * Req) * AsyncReplyChannel<Answ>


// let m =
//     MailboxProcessor.Start (fun inbox ->
//         let rec loop () =
//             async {
//                 let! msg = inbox.Receive()
//                 let x =
//                     match msg with
//                     | 0 -> 1
//                 inbox.Post x
//                 return! loop()
//             }
//         loop()
//         )
