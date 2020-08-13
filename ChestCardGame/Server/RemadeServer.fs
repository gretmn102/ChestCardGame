module RemadeServer

open CommandInterpriter
open CommandInterpriter.ServerAnswer

module MailboxProcessor =
    let replyChannel (r:AsyncReplyChannel<_>) x = r.Reply x
    let receive (mailboxProc:MailboxProcessor<_>) = mailboxProc.Receive()
    let postAndReply (m:MailboxProcessor<_>) = m.PostAndReply

module nameReg = 
    type Log =
        | Login
        | Logout
    type Req =
        | Log of PlayerId * Log
        | GameReq of PlayerId * GameReq.Req
    type Msg =
        | Post of Req * AsyncReplyChannel<Answ>

    type State = { Names:Map<PlayerId, bool>; EmptySlots:int }

    let logProc (name:PlayerId) names slots = function
        | Logout ->
            match Map.tryFind name names with
            | None -> Success false, None
            | _ -> 
                let state = {
                        Names = Map.add name false names
                        EmptySlots = slots + 1
                    }
                Success true, Some state
        | Login ->
            if slots = 0 then SlotsFull, None
            elif name = "" then NameEmpty, None
            else
                match Map.tryFind name names with
                | None | Some false ->
                    let state = {Names = Map.add name true names; EmptySlots = slots - 1 }
                    LoginSuccess, Some state
                | Some true -> NameBusy, None

    let gameMail inbox m st =
        let gamePost name req =
            MailboxProcessor.postAndReply m (fun r -> RemadeOld.Msg.Post(name, req, r))
        let interp ({Names = names; EmptySlots = slots}) = function
            | Log(name, x) ->  logProc name names slots x
            | GameReq(name, req) ->
                if slots = 0 then Game <| gamePost name req, None
                else WaitPlayers slots, None
        let rec loop st =
            async {
                let! msg = MailboxProcessor.receive inbox
                match msg with
                | Post(req, r) -> 
                    let (answ, state) = interp st req 
                    r.Reply answ
                    match state with
                    | None -> return! loop st
                    | Some st -> return! loop st
            }
        loop st

    let nameRegister gameMailbox emptySlots =
        let interp ({Names = names; EmptySlots = slots}) = function
            | Log(name, x) -> logProc name names slots x
            | _ -> WaitPlayers slots, None
        let rec loop st inbox =
            async {
                let! msg = MailboxProcessor.receive inbox
                match msg with
                | Post(req, r) -> 
                    let (answ, state) = interp st req 
                    r.Reply answ
                    match state with
                    | None -> return! loop st inbox
                    | Some st -> 
                        if st.EmptySlots = 0 then
                            let m =
                                st.Names 
                                |> Map.fold (fun state key _ -> Set.add key state) Set.empty
                                |> gameMailbox
                            return! gameMail inbox m st
                        else
                            return! loop st inbox
            }
        MailboxProcessor.Start <| loop {Names = Map.empty; EmptySlots = emptySlots }
    let post req nameRegister = 
        MailboxProcessor.postAndReply nameRegister (fun x -> Post(req, x))

let tryRead streams = try ClientReq.pars streams |> Some with _ -> None

let write stream thing = 
    let ms = ServerAnswer.unpars thing
    streamToStream ms stream

let tryReadWrite stream f =
    match tryRead stream with
    | Some req -> 
        let answer = f req
        write stream answer
        Some answer
    | None -> None

let init gameMailbox countPlayer = 
    let listener = System.Net.Sockets.TcpListener(System.Net.IPAddress.Loopback, 5000)
    listener.Start()
    let post = 
        let m = nameReg.nameRegister gameMailbox countPlayer
        fun req -> nameReg.post req m
//        fun req last ->
//        if req = last then
//            nameReg.post req m
//        else
//            printfn "Req: %A" req;
//            nameReg.post req m
//            |> fun x -> printfn "Answ: %A" x; x
    let print =
        let logFile = System.IO.File.CreateText "e:\\output.log"
        logFile.AutoFlush <- true
        let m = MailboxProcessor.Start(fun inbox ->
            let rec loop i = 
                async {
                    let! r = MailboxProcessor.receive inbox
                    printfn "%s" r
                    fprintfn logFile "%s" r
                    //if i > 4 then logFile.Flush()
                    return! loop (i + 1)
                }
            loop 0)
        m.Post //(fun r -> r, x)
        
    let logReq curr last answ = (*if last <> curr then*) print <| sprintf "Req: %A" curr; print <| sprintf "Answ: %A" answ
   
    while true do
        let client = listener.AcceptTcpClient()
        async {
            print <| sprintf "Enter unknown" //(client.Client.ToString())
            let stream = client.GetStream()
            let rec logCircle lastReq =
                match tryRead stream with
                | None -> ()
                | Some x -> 
                    //logReq lastReq x
                    match x with
                    | ClientReq.Login name -> 
                        let curr = nameReg.Req.Log(name, nameReg.Login)
                        let answ = post curr //lastReq
                        logReq (name, x) lastReq answ
                        match answ with
                        | Answ.LoginSuccess -> 
                            write stream answ
                            let rec f name lastReq = 
                                let interp = function
                                    | ClientReq.GameReq gr -> post <| nameReg.Req.GameReq(name, gr)
                                    | _ -> FailReq
                                match tryRead stream with
                                | Some req -> 
                                    let answer = interp req
                                    write stream answer
                                    logReq (name, req) lastReq answer
                                    f name (name, req)
                                | None -> 
                                    print <| sprintf "Logout: %s" name
                                    post <| nameReg.Req.Log(name, nameReg.Logout) |> ignore
//                                match tryReadWrite stream interp with
//                                | Some answ -> f name x
//                                | None -> post <| nameReg.Req.Log(name, nameReg.Logout) |> ignore
                            f name (name, x)
                        | Answ.SlotsFull -> write stream answ
                        | answ -> 
                            write stream answ
                            logReq (name,x) lastReq answ
                            logCircle (name,x)
                    | x -> 
                        write stream Answ.ReqLogin
                        let curr = "unknown", x
                        logReq curr lastReq Answ.ReqLogin
                        logCircle curr
            logCircle("unknown", ClientReq.GetServerState) // nameReg.Req.Log("", nameReg.Logout)
        } |> Async.Start

init RemadeOld.mail 2