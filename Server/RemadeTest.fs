module RemadeTest

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
module MoveCircle =
    type Test =
        | GetMoveT of int * int
        | PlayerHaveCardsT of int
        | EndMoveCircleT
    let rec f acc x = 
        let fbool f' x = [f' true; f' false] |> List.map (f x)
        let fboolt f' v = 
            if Set.contains v acc then []
            else Set.add v acc |> fbool f'
        match x with
        | EndMoveCircle -> Node(EndMoveCircleT, [])
        | GetMove((p1, p2), f') -> 
            let v = GetMoveT(p1,p2)
            Node(v, fboolt f' v)
        | PlayerHaveCards(p, f') ->
            let v = PlayerHaveCardsT p
            Node(v, fboolt f' v)
    moveCircle [1..3] |> f Set.empty |> visualize |> printfn "%s"
module PlCircle =
    type Test = 
        | EndT
        | DeckIsEmptyT
        | MoveCircleT of int list
        | PlayerHaveCardsT of int
        | PlayerTakeCardFromDeckT of int
        | FailT of string
    let rec test acc x =
        let fbool f x = [f true; f false] |> List.map (test x)
        let fboolt f v = 
            if Set.contains v acc then []
            else Set.add v acc |> fbool f
        let funit f v =
            if Set.contains v acc then Node(v, [])
            else Node(v, [f () |> test (Set.add v acc)])
        match x with
        | End -> Node(EndT, [])
        | DeckIsEmpty f -> 
            Node(DeckIsEmptyT, fbool f acc)
        | MoveCircle(pls, f) -> 
            let v = MoveCircleT pls
            funit f v
        | T.PlayerHaveCards(pl, f) ->
            let v = PlayerHaveCardsT pl
            Node(v, fboolt f v)
        | PlayerTakeCardFromDeck(pl, f) ->
            funit f (PlayerTakeCardFromDeckT pl)
        | Fail str -> Node(FailT str, [])
    let k = plCircle [1..2] |> test Set.empty |> visualize
    let file = System.IO.File.CreateText @"e:\file.txt"
    fprintfn file "%s" k
    file.Close()