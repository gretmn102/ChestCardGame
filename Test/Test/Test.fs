#if INTERACTIVE
#r @"E:\Project\ChestCardGame\ChestCardGame\CommandInterpriter\bin\Debug\net45\CommandInterpriter.dll"
#r @"E:\Project\ChestCardGame\ChestCardGame\Server\bin\Debug\net45\Server.exe"
#endif

open Fuchu


open FsharpMyExtension
open FsharpMyExtension.FSharpExt

open Remade.Prog

module RemadeTest =
    [<Tests>]
    let MoveCircleTest =
        testList "afterDeckEndTest" [
            testCase "base case" <| fun () ->
                let act = 
                    match afterDeckEnd (fun _ _ _ -> failwith "moveCircle") [] with
                    | End -> true
                    | _ -> false
                Assert.Equal("", true, act)
            testCase "deck empty, only one player in game" <| fun () ->
                let act = 
                    match afterDeckEnd (fun _ _ _ -> failwith "moveCircle") ["x"] with
                    | PlayerHaveCards("x", f) ->
                        match f true with
                        | Fail _ -> 
                            match f false with
                            | End -> true
                            | x -> failwithf "3 %A" x
                        | x -> failwithf "2 %A" x
                    | x -> failwithf "1 %A" x
                Assert.Equal("", true, act)
            testCase "base case3" <| fun () ->
                let act = 
                    match afterDeckEnd (fun x xs next -> next) ["x"; "y"; "z"] with
                    | PlayerHaveCards("x", f1) ->
                        match f1 true with
                        | PlayerHaveCards("y", f2) ->
                            match f2 true with
                            | PlayerHaveCards("z", f3) ->
                                match f3 true with
                                | PlayerHaveCards("x", _) -> true
                                | x -> failwithf "1 %A" x
                            | x -> failwithf "2 %A" x
                        | x -> failwithf "3 %A" x
                    | x -> failwithf "4 %A" x
                Assert.Equal("", true, act)
            testCase "base case4" <| fun () ->
                let act = 
                    match afterDeckEnd (fun x xs next -> next) ["x"; "y"; "z"] with
                    | PlayerHaveCards("x", f1) ->
                        match f1 false with
                        | PlayerHaveCards("y", f2) ->
                            match f2 true with
                            | PlayerHaveCards("z", f3) ->
                                match f3 true with
                                | PlayerHaveCards("y", _) -> true
                                | x -> failwithf "1 %A" x
                            | x -> failwithf "2 %A" x
                        | x -> failwithf "3 %A" x
                    | x -> failwithf "4 %A" x
                Assert.Equal("", true, act)
       ]
    ()
    // let f =
    //     match Remade.Continues2.playersCircle ["first"; "second"; "third"] with
    //     | DeckIsEmpty f ->
    //         match f true with
    //         | PlayerHaveCards("second", f) -> 
    //             match f true with
    //             | GetMove (("first", "second", ["third"]), f) ->
    //                 match f true with
    //                 | PlayerHaveCards("third", f) ->
    //                     match f true with
    //                     | GetMove (("first", "third", ["second"]), f) ->
    //                         f false
    //                         // match f false with
    //                         // | PlayerHaveCards("third", f) ->
    //                         //     f true
    //         f false
    //     | x -> failwith ""

module MoveTest =
    open Remade
    open Sandbox
    // let y = 

    let getRank tru f =
        match f with
        | GetRank f ->
            let tru = 
                match f tru with
                | true, f -> f()
                | x -> failwithf "getRank expected true, but %A" x
            let fal =
                match f -1 with
                | false, f -> f()
                | x -> failwithf "getRank expected false, but %A" x
            tru, fal
        | x -> failwithf "getRank return %A" x
    let getCount tru f =
        match f with
        | GetCount f ->
            let tru = 
                match f tru with
                | true, f -> f()
                | x -> failwithf "getCount expected true, but %A" x
            let fal =
                match f 0 with
                | false, f -> f()
                | x -> failwithf "getCount expected false, but %A" x
            tru, fal
        | x -> failwithf "getCount return %A" x
    let getSuit tru f =
        match f with
        | GetSuit f ->
            let tru = 
                match f tru with
                | true, f -> f()
                | x -> failwithf "getSuit expected true, but %A" x
            let fal =
                match f [] with
                | false, f -> f()
                | x -> failwithf "getSuit expected false, but %A" x
            tru, fal
        | x -> failwithf "getSuit return %A" x
    
    let initCard r s = { CommandInterpriter.Rank = r; CommandInterpriter.Suit = s }
    let cards = [ initCard 0 1; initCard 0 2; initCard 0 3 ]
    let x =
        let pempty = { Cards = Set.empty; Chests = Set.empty }
        let p2 = { pempty with Cards = Set.ofList cards };

        let st = {
            Deck = Deck.init
            Players = Map[ "1", { pempty with Cards = Set[initCard 0 0]}; "2", p2; "3", pempty ]
            PlayersIds = Set ["1"; "2"; "3"]
        }
        move "1" "2" (fun st _ -> EndGame st) st
    let y =
        match x with
        | Move(("1", "2", ["3"]), f) ->
            getRank 0 (f())
            |> fst
            |> getCount 3
            |> fst
            |> getSuit [1; 2; 3]
            |> function
                Gives(c, f), _ when c = cards ->
                    match f() with
                    | CompiledChest (("1", ["2"; "3"], 0), f) ->
                        f()

[<EntryPoint>]
let main arg =
    defaultMainThisAssembly arg
