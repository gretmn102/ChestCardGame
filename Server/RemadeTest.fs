﻿module RemadeTest

#if INTERACTIVE
#load "remade.fs"
#endif

open Remade.Continues

type 'a Pack = Node of 'a * Pack<'a> list
let visualize inputTree = 
    let prefMid = seq { yield "├─"; while true do yield "│ " }
    let prefEnd = seq { yield "└─"; while true do yield "  " }
    let prefNone = Seq.initInfinite (fun _ -> "")
 
    let inline c2 x y = Seq.map2 (fun u v -> String.concat "" [string u; string v]) x y

    let rec visualize (Node(label:'a, children:Pack<'a> list)) pre =
        seq {
            yield (Seq.head pre) + (sprintf "%A" label)
            if not <| List.isEmpty children then
                let preRest = Seq.skip 1 pre
                let last = Seq.last children
                for e in children do
                    let r = if e = last then prefEnd else prefMid
                    yield! c2 preRest r |> visualize e
        }
    
    let res = 
        visualize inputTree prefNone
        |> Seq.map (sprintf "%s")
    System.String.Join("\n", res)

let fbool test f x = [f true; f false] |> List.map (test x)
let funit test f x = [f() |> test x]
let accf acc f v = 
    let xs = 
        if Set.contains v acc then []
        else Set.add v acc |> f
    Node(v, xs)

open CommandInterpriter
open CommandInterpriter.GameAnswer
open CommandInterpriter.GameReq

module MoveCircle =
    type Test =
        | GetMoveT of PlayerId * PlayerId * PlayerId list
        | PlayerHaveCardsT of PlayerId
        | EndMoveCircleT
    let rec test acc x = 
        match x with
        | EndMoveCircle -> Node(EndMoveCircleT, [])
        | GetMove((p1, p2, other), f) -> 
            let v = GetMoveT(p1,p2, other)
            accf acc (fbool test f) v
        | PlayerHaveCards(p, f) ->
            let v = PlayerHaveCardsT p
            accf acc (fbool test f) v
    [1..3] |> List.map (string) |> moveCircle |> test Set.empty |> visualize |> printfn "%s"
module PlCircle =
    type Test = 
        | EndT
        | DeckIsEmptyT
        | MoveCircleT of PlayerId list
        | PlayerHaveCardsT of PlayerId
        | PlayerTakeCardFromDeckT of PlayerId
        | FailT of string
    let rec test acc x =
        match x with
        | End -> Node(EndT, [])
        | DeckIsEmpty f -> 
            Node(DeckIsEmptyT, fbool test f acc)
        | MoveCircle(pls, f) -> 
            let v = MoveCircleT pls
            accf acc (funit test f) v
        | PlayersCircleT.PlayerHaveCards(pl, f) ->
            let v = PlayerHaveCardsT pl
            accf acc (fbool test f) v
        | PlayerTakeCardFromDeck(pl, f) ->
            let v = PlayerTakeCardFromDeckT pl
            accf acc (funit test f) v
        | Fail str -> Node(FailT str, [])
    let k = [1..3] |> List.map (string) |> playersCircle |> test Set.empty |> visualize
    let file = System.IO.File.CreateText @"e:\file.txt"
    fprintfn file "%s" k
    file.Close()
module Basic =
    #if INTERACTIVE
    #load "remade.fs"
    #endif
    open Remade
    
    let m = Remade.mail (set([1..3] |> List.map (string)))
    let get p = m.PostAndReply ((fun r -> Post(p, GetState, r)), 500) |> printfn "%A"
    let input p input = m.PostAndReply ((fun r -> Post(p, Input(input), r)), 500) |> printfn "%A"
    get "1"
    get "2"
    get "3"
    input "1" (RankInput 2)
    input "1" (CountInput 1)
    input "1" (SuitInput (Set[0]))
module SandBox =
    let maybe (x, f) = Option.bind f x
    


