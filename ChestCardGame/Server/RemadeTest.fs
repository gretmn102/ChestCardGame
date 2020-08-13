module RemadeTest
open FsharpMyExtension.Tree
open Remade.Continues
let k = Sandbox.Fail ""
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
open FsharpMyExtension.Show

module MoveCircle =
    open FsharpMyExtension.FSharpExt
    type 'a Graph = (int * int) Set * Map<int, 'a>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Graph = 
        let toTgf = 
            let f ((leafs, nodes):_ Graph) =
                let leafs = leafs |> Set.map (curry (sprintf "%d %d")) |> String.concat "\n"
                let nodes = nodes |> Map.toSeq |> Seq.map (curry (sprintf "%d %A")) |> String.concat "\n"
                sprintf "%s\n#\n%s" nodes leafs
            f

    type Test =
        | Start
        | GetMoveT of PlayerId * PlayerId * PlayerId list
        | PlayerHaveCardsT of PlayerId
        | EndMoveCircleT
    let rec test (lastI, i) (lasts:Map<Test,int>) (acc:_ Graph) x = 
        let ff v f =
            match Map.tryFind v lasts with
            | Some i' ->
                let acc : _ Graph =
                    acc |> mapFst (Set.add (lastI, i'))
                i, acc
            | None ->
                let acc : _ Graph =
                    acc |> mapPair
                            (Set.add (lastI, i))
                            (Map.add i v)
                let lasts = lasts |> Map.add v i
                let i', acc = f true |> test (i, i+1) lasts acc
                f false |> test (i, i') lasts acc
        match x with
        | EndMoveCircle ->
            // let lastI' = lastI + 1
            let acc : _ Graph =
                acc |> mapPair
                        (Set.add (lastI, i))
                        (Map.add i EndMoveCircleT)
            i + 1, acc
            // Node(EndMoveCircleT, [])
        | GetMove((p1, p2, other), f) -> 
            let v = GetMoveT(p1,p2, other)
            ff v f
        | PlayerHaveCards(p, f) ->
            let v = PlayerHaveCardsT p
            ff v f
            // accf acc (fbool test f) v
    let res =
        [1..3]
        |> List.map string
        |> moveCircle
        |> test (0, 1) Map.empty (Set.empty, Map[0, Start])
        |> snd |> Graph.toTgf
        |> fun s -> System.IO.File.WriteAllText(@"e:\file.tgf", s)
     //|> Tree.visualize (sprintf "%A") |> printfn "%s"
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
    
    let k = [1..3] |> List.map (string) |> playersCircle |> test Set.empty //|> Tree.visualize (sprintf "%A")
    Tree.group2.group k
    |> Tree.group2.toTgf
    |> fun s -> System.IO.File.WriteAllText(@"e:\file.tgf", s)
    

    // let file = System.IO.File.CreateText @"e:\file.txt"
    // fprintfn file "%s" k
    // file.Close()

module SandBox =
    let maybe (x, f) = Option.bind f x
    


