module Server

open System
open System.Threading
open System.IO;
open System.Net;
open System.Net.Sockets;
open CommandInterpriter

type cmd = Write of string | Read | InfoMsg of CommandInterpriter.ServerAnswer.InfoMsg | GetRank
type gameState = 
    { PlName:string; Cmd:cmd; }
    member this.ToServer () =
        match this with
        | { Cmd = Read } -> ServerAnswer.Read
        | { Cmd = Write s } -> ServerAnswer.Write s
        | { Cmd = GetRank } -> ServerAnswer.cmd.GetRank
        | { Cmd = InfoMsg msg } -> ServerAnswer.Info msg

type GameState (plsName) =
    let isSuspend = ref false

    let mutable m_input = ""
    let mutable m_state = { PlName = ""; Cmd = Read }
    let suspend () =
        isSuspend := true
        printfn "%A" m_state
        while(!isSuspend) do Thread.Sleep 100

    let pls = 
        let write plName str = 
            m_state <- { PlName = plName; Cmd = Write str };
            suspend()
        let info plName msg = 
            m_state <- { PlName = plName; Cmd = InfoMsg msg }
            suspend()
        let read plName () = 
            m_state <- { PlName = plName; Cmd = Read }; suspend(); 
            let input = m_input in m_input <- ""; input
        let rank plName () =
            m_state <- { PlName = plName; Cmd = GetRank }; suspend(); 
            let input = m_input in m_input <- ""; input            

        plsName 
        |> List.map (fun x -> x, write x, read x, rank x, info x)
        |> ChestGame.playersCreated

    do
        async {
            pls |> ChestGame.mainCircle 
        } |> Async.Start
    member __.Players = pls |> List.map (fun x -> x.Name, x.CardCount)
        
    member __.Input s =
        m_input <- s
        isSuspend := false
    member __.GetCards name = 
        (pls |> List.find (fun p -> p.Name = name)).Card |> List.ofSeq 
        |> List.map (function { Rank = ServerAnswer.Rank r; Suit = ServerAnswer.Suit s } -> r, s)
    member __.State = m_state
    member __.IsSuspend = !isSuspend
    member __.Continue () = isSuspend := false

module nameReg = 
    type msg =
        | Name of string * AsyncReplyChannel<bool>
        | Fetch of AsyncReplyChannel<string list>
    type NameRegister () =
            let counter =
                MailboxProcessor.Start(fun inbox ->
                    let rec loop names =
                        async { let! msg = inbox.Receive()
                                match msg with
                                | Fetch reply ->
                                    reply.Reply names
                                    //return! loop(names)
                                    return ()
                                | Name (name, (replyChannel:AsyncReplyChannel<bool>)) ->
                                    let res = 
                                        if List.exists ((=) name) names then
                                            replyChannel.Reply(false); names
                                        else
                                            replyChannel.Reply(true); name::names
                                    return! loop res }
                    loop ([]:string list))
            member this.InputName name = counter.PostAndReply(fun x -> Name(name, x))
            /// <summary> return names and dispose </summary>
            member this.Fetch () = counter.PostAndReply(fun x -> Fetch x)

let readWrite stream f = 
    let req = ClientReq.pars stream
    let answer = f req
    let ms = ServerAnswer.unpars answer
    StreamToStream ms stream

let tryRead streams = 
    try ClientReq.pars streams |> Some with _ -> None

let tryReadWrite stream f =
    match tryRead stream with
    | Some req -> 
        let answer = f req
        let ms = ServerAnswer.unpars answer
        StreamToStream ms stream
        true
    | None -> false

let write stream thing = 
    let ms = ServerAnswer.unpars thing
    StreamToStream ms stream

let start playerCount =
    let waitClientsNum = ref playerCount
    
    let listener = new TcpListener(5000);
    listener.Start();

    let nameRegister = nameReg.NameRegister()
    
    let nameReg clientStream =
        let rec login () =
            let answer = write clientStream

            match tryRead clientStream with
            | Some(ClientReq.EnterBy name) ->
                if nameRegister.InputName name then
                    ServerAnswer.cmd.Success true |> answer
                    Some name
                else
                    ServerAnswer.cmd.Success false |> answer
                    login()
            | Some _ ->
                ServerAnswer.LoginReq |> answer
                login()
            | None -> None
        login()
    let waitForOther clientStream =
        let wait () =
            tryReadWrite clientStream (function _ -> ServerAnswer.WaitConnectPlayers !waitClientsNum)
        let rec f () =
            if !waitClientsNum <> 0 then 
                if wait() then f()
                else false
            else 
                tryReadWrite clientStream (function _ -> ServerAnswer.StartGame)
                //true
        f()

    let client () =
            let client = listener.AcceptTcpClient()
            decr waitClientsNum
            let stream = client.GetStream()
            
            let res = 
                match nameReg stream with
                | None -> incr waitClientsNum; None
                | Some name -> Some(name, stream)
            res
    
    printfn "wait %d players" playerCount
    let firstess = 
        seq{ for i = 1 to playerCount do
                yield async { 
                        let rec f () = match client () with None -> f() | Some x -> x
                        return f()}}
        |> Async.Parallel
        |> Async.RunSynchronously
    printfn "всех игроков набрали"
    let plsNames = nameRegister.Fetch()
    let gameState = GameState plsNames
    
    let clientCircle (name,stream) = 
        let interp name = function
            | ClientReq.Getstate ->
                if !waitClientsNum <> 0 then ServerAnswer.WaitConnectPlayers !waitClientsNum
                else
                    let state = gameState.State
                    if state.PlName = name then
                        let res = state.ToServer()
                        match state.Cmd with 
                        | Write _ -> gameState.Continue()
                        | InfoMsg _ -> gameState.Continue()
                        | _ -> ()
                        res
                    else ServerAnswer.WaitPlayer state.PlName
            | ClientReq.GetCards -> gameState.GetCards name |> ServerAnswer.cmd.Cards
            | ClientReq.Write s -> 
                let state = gameState.State
                if state.PlName = name then
                    gameState.Input s
                    ServerAnswer.cmd.Success true
                else
                    ServerAnswer.cmd.Success false
            | ClientReq.Rank n ->
                let state = gameState.State
                if state.PlName = name then
                    gameState.Input (n.ToString())
                    ServerAnswer.cmd.Success true
                else
                    ServerAnswer.cmd.Success false                
            | ClientReq.GetMyName -> ServerAnswer.YourName name
            | ClientReq.EnterBy _ -> ServerAnswer.cmd.UnknownCmd
            | ClientReq.GetPlayers -> gameState.Players |> ServerAnswer.cmd.Players
        while tryReadWrite stream (interp name) do ()
        incr waitClientsNum
    
    firstess |> Array.iter (fun x -> async { clientCircle x } |> Async.Start)

    let reconnect streamClient =
        if !waitClientsNum = 0 then
                tryReadWrite streamClient (function ClientReq.Getstate -> ServerAnswer.SlotBeAbsent | _ -> ServerAnswer.SlotBeAbsent)
                |> ignore
                None
        else
            decr waitClientsNum
            let rec login () = 
                match tryRead streamClient with
                | Some(ClientReq.EnterBy name) ->
                    if plsNames |> List.exists ((=)name) then
                        ServerAnswer.cmd.Success true |> write streamClient; Some name
                    else ServerAnswer.cmd.Success false |> write streamClient; login()
                | Some _ -> ServerAnswer.LoginReq |> write streamClient; login()
                | None -> None
            login()
    
    while true do
        let client = listener.AcceptTcpClient()
        async { 
            let stream = client.GetStream()
            match reconnect stream  with
            | None -> incr waitClientsNum
            | Some name -> clientCircle (name, stream) } |> Async.Start


module SandBox =
    open System
    open System.Threading
    open System.IO;
    open System.Net;
    open System.Net.Sockets;

    type Client () =
        let isOpen = ref true
        let write = ref ""
        let isQuit = ref false
        let connect () =
            let connect () =
                let client = new TcpClient()
                client.Connect(IPAddress.Parse("127.0.0.1"), 5000)
                let w = new StreamWriter( client.GetStream() )
                w.AutoFlush <- true
                while !isOpen do
                    if !write <> "" then w.WriteLine(!write); write := ""
                    Thread.Sleep 100
                isQuit := true
            let thread = new Thread(connect)
            thread.Start()
            thread
        let mutable thread = connect()

        member this.Abort () = thread.Abort()
        member this.Close () = isOpen := false
        member this.ReConnect () = 
            if !isOpen then failwith "connect is open" else isOpen := true; thread <- connect()
        member this.Write s = write := s

    let read (r:StreamReader) = 
        try r.ReadLine() |> Some with _ -> None

    let sandbox () = 
        (*
        let f x func = 
            x, func ()
        let func () = printf "input:"; Console.ReadLine()
        let x = 10
        [for i = 1 to 3 do yield f x func]
        |> printfn "output:\n%A" *)

        //let localIPs = Dns.GetHostAddresses(Dns.GetHostName());
        let localIp = Dns.GetHostAddresses("localhost").[0];
        let listener = TcpListener(localIp, 5000)
        listener.Start()
        let status = ref ""
        let rec acceptClient n = 
            let print s = 
                status := s
                printf "%d: " n
                printfn "%s" s
            async {
                    let rec connect () =
                        print "wait connect"
                        let client = listener.AcceptTcpClient()
                        print "accept"
                        let streams = client.GetStream()
                        let reader = new StreamReader(streams)
                        print "read..."
                        match read reader with Some _ -> () | None -> print "disconect. Reconnect..."; client.Close(); connect()
                        print "done!"
                    connect()
                    print "exit from thread"
                }
            |> Async.Start
        for i = 1 to 2 do
            acceptClient i
        (*
        listener.Stop()
        listener.Server.Close()
        listener.Pending()
        !status
        let client = new Client()
        client.Close()
        client.Abort()
        client.ReConnect()
        //client.Abort()

    
    *)

        //client

        //listener.Con
        //counter.Post(Incr 10)
        (*
        let f name =
            async { let! res = counter.PostAndAsyncReply(fun x -> Name(name, x))
                    return sprintf "%s = %b" name res }
        
        [ "Bob"; "Clay"; "Bob"; "Alpha"; "Bravo"; "Grey" ]
        |> Seq.map f
        |> Async.Parallel
        |> Async.RunSynchronously
        
        counter.PostAndReply(fun x -> Fetch x)
        counter.CurrentQueueLength
        *)
        


//SandBox.sandbox()
start 2

printfn "Done!"
Console.ReadKey() |> ignore
