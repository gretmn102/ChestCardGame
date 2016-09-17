module ChestGame

module List =
    let next l =
        //List.permute(fun x -> (x + 1) % List.length l) l // [3;1;2] -> [2;3;1]
        [ yield! List.tail l; yield List.head l]
    let private testfail f = try f () |> ignore; false with _ -> true
    assert 
        next [1;2;3] = [2;3;1]
    assert
        next [2;3;1] = [3;1;2]
    assert
        next [3;1;2] = [1;2;3]
    assert
        (fun () -> next ([]:int list)) |> testfail
    assert
        next [1] = [1]
    let others current l =
        if not <| List.exists ((=) current) l then failwith "такой элемент в списке отсутствует"
        let to' = Seq.takeWhile ((<>) current) l
        let from = Seq.skipWhile ((<>) current) l
        [ yield! Seq.skip 1 from; yield! to']

    assert
        others 1 [1..5] = [2;3;4;5]
    assert
        others 3 [1..5] = [4;5;1;2]
    assert
        others 5 [1..5] = [1;2;3;4]
    assert
        (fun () -> others 6 [1..5]) |> testfail
    assert
        (fun () -> others 1 []) |> testfail
    assert
        others 1 [1] |> List.isEmpty

open CommandInterpriter
open CommandInterpriter.ServerAnswer

type Pack() =
    // TODO: перемешать колоду
    let deckOfCard =
        (seq {
            for suit in [ 0..3 ] do
                for rank in 0..12 -> { Rank = Rank rank; Suit = Suit suit }
        }).GetEnumerator()
    let m_pack = deckOfCard

    member __.Give() = if m_pack.MoveNext() then Some(m_pack.Current) else None
    //member this.Empty() = m_pack.

let pack = Pack()

type IOFunc = 
    { AskYouOnX:  string  -> Ask; // спросить у противника что-нибудь
      Inform:     InfoMsg -> unit; }

type PlayerData (name: string, iofunc:IOFunc) = 
    let mutable m_cards = Set.empty<PlayingCard>
    /// <summary> имя игрока </summary>
    member __.Name = name
    member __.CardCount = m_cards.Count
    member __.Card = m_cards
    /// <summary> взять из колоды карту </summary>
    member __.TakeCardFromPack() = 
        match pack.Give() with
        | Some(card) ->
            iofunc.Inform (CardAdd card)
            m_cards <- m_cards.Add card; true
        | None -> false
    /// <summary>Есть ли у данного игрока карты?</summary>
    member __.HaveCards() = m_cards.Count <> 0
    /// <summary>Взять карты</summary>
    member __.Take cards = 
        Set.iter (CardAdd >> iofunc.Inform) cards
        m_cards <- cards |> Set.fold (fun state c -> state.Add(c)) m_cards
    /// <summary>Отдать карты</summary>
    member __.Give = function
        | IsSuit(r, suits) ->
            let cards = set [ for suit in suits -> { Rank = r; Suit = suit } ]
            Set.iter (CardRemove >> iofunc.Inform) cards
            m_cards <- cards |> Set.difference m_cards
            cards
        | _ -> failwith "Give принимает только объединение IsSuit"
    member __.Check = function
        | IsRank(r) ->
            m_cards |> Set.exists (function { Rank = current; } -> current = r)
        | IsCount(r, count) ->
            (m_cards 
            |> Set.filter (function { Rank = current; } -> current = r) 
            |> Set.count) = count
        | IsSuit(r, suits) ->
            set[ for suit in suits -> { Rank = r; Suit = suit}]
            |> Set.intersect m_cards
            |> Set.count
            |> (=) suits.Length
            
    member __.Inform = iofunc.Inform

    member __.Request name = iofunc.AskYouOnX name


//open ListCircle
open System

let mainCircle (playersCreated: PlayerData list) =
    let r = new Random()
    
    playersCreated
    |> List.iter (fun x -> 
        for i = 1 to 5 do 
            if x.TakeCardFromPack() |> not then 
                failwith "карт не хватило на первую раздачу")

    let shufflePlayers playersCreated = 
        let ran = r.Next(0, 10)
        let rec f state i = 
            if i = ran then state
            else f (List.next state) (i + 1)
        f playersCreated 0

    let move pl players =
        let rec playerCircle func haveCards = function
            | [] -> ()
            | h::t as players->
                if h |> haveCards then
                    if func h then players |> List.next |> playerCircle func haveCards else ()
                else 
                    if List.isEmpty t then ()
                    else 
                        t |> List.next |> playerCircle func haveCards
        //playerCircle (fun _ -> true) (fun _ -> false) [1..2]
        let haveCards (p:PlayerData) = p.HaveCards()
        let f (current:PlayerData) (moveOn:PlayerData) =
            current.Inform(InfoMsg.MoveYouOnX(moveOn.Name))
            moveOn.Inform(InfoMsg.MoveXOnYou(current.Name))

            let infoOther = 
                let others = 
                    playersCreated
                    |> List.filter (fun x -> [current; moveOn] |> List.exists ((=) x) |> not)
                (fun f -> others |> List.iter f)
            //others |> List.iter (fun x -> x.Inform(MoveXOnY(pl.Name, c.Name)) )
            //['b'..'h'] |> List.filter (fun x -> ['d'; 'c'] |> List.exists ((=) x) |> not)
            infoOther(fun x -> x.Inform(MoveXOnY(current.Name, moveOn.Name)))
            let response = current.Request(moveOn.Name)
            moveOn.Inform(AskXOnYou(response, current.Name))
            infoOther(fun x -> x.Inform(AskXOnY(response, current.Name, moveOn.Name)))
            if moveOn.Check(response) then
                playersCreated |> List.iter (fun x -> x.Inform(InfoMsg.Success true))
                current.Take(moveOn.Give(response))
                true
            else
                playersCreated |> List.iter (fun x -> x.Inform(InfoMsg.Success false)) 
                false
        List.others pl players
        |> playerCircle (f pl) haveCards

    let start () =  
        let rec playerCircle = function
            | [] -> ()
            | h::t as players ->
                move h players
                if h.TakeCardFromPack() |> not && h.HaveCards() |> not then t
                else players
                |> List.next |> playerCircle
        shufflePlayers playersCreated
        |> playerCircle
    start ()

let playersCreated l = 
    let pCreate (name, write, read, rank, info) =
        let plDel = (fun name ->
                            write "спросить ранг:"
                            //let r = Int32.Parse(read())
                            let r = Int32.Parse(rank())
                            write "указать масть"
                            let s = Int32.Parse(read())
                            IsSuit(Rank r, [Suit s]))
        let p = new PlayerData(name, {AskYouOnX = plDel; Inform = info}) //(fun x -> sprintf "%A" x |> write)})
        p
    List.map pCreate l
    //[pCreate 1; pCreate 2;]
//mainCircle playersCreated
