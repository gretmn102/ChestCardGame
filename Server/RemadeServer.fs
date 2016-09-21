module RemadeServer

module nameReg = 
    type UserId = int
    type Answ = 
        | Success of bool
        | YourId of UserId
        | SlotsFull

    type Req =
        | NewUser
        | DeleteUser of UserId
        | RegisterName of UserId * string
        
    type Msg =
        | Post of Req * AsyncReplyChannel<Answ>
        //| Fetch of AsyncReplyChannel<Set<string>>
    type State = { Names:Map<string, bool>; Data:Map<int, string>; EmptySlots:int }
    type NameRegister(emptySlots) =
        let counter =
            let add m =
                let rec add i = 
                    if Map.containsKey i m then add (i+1) else i, Map.add i "" m
                add 0
            let interp ({Data = data; EmptySlots = slots} as st) = function
                | NewUser -> 
                    if slots = 0 then SlotsFull, None
                    else
                        let (id, data) = add data
                        YourId id, {st with Data = data; EmptySlots = slots - 1} |> Some
                | DeleteUser id ->
                    let rmName names = 
                        let name = data.[id]
                        if name = "" then names
                        else
                            match Map.tryFind name names with
                            | None -> names
                            | _ -> Map.add name false names
                    let state = {Names = rmName st.Names; Data = Map.remove id data; EmptySlots = slots + 1}
                    Success true, Some state
                | RegisterName(id, name) ->
                    let addName name names =
                        let containt = 
                            match Map.tryFind name names with
                            | None -> false | Some x -> x
                        let set =
                            if containt then names
                            else Map.add name true names
                        not containt, set
                    if name = "" then Success false, None
                    else
                        let (r, names) = addName name st.Names
                        let state =
                            if r then 
                                {st with Names = names; Data = Map.add id name data} |> Some
                            else None
                        Success r, state
            MailboxProcessor.Start(fun inbox -> 
                let rec loop st =
                    async {
                        let! msg = inbox.Receive()
                        match msg with
                        //| Fetch r -> r.Reply names; return ()
                        | Post(req, r) -> 
                            let (answ, state) = interp st req 
                            r.Reply answ
                            match state with
                            | None -> return! loop st
                            | Some st -> return! loop st
                    }
                loop {Names = Map.empty; Data = Map.empty; EmptySlots = emptySlots })
        member __.Post name = counter.PostAndReply(fun x -> Post(name, x))
        /// <summary> return names and dispose </summary>
        //member __.Fetch () = counter.PostAndReply(Fetch)

module nameReg2 = 
    type UserName = string
    type Answ = 
        | Success of bool
        | NameEmpty
        | NameBusy
        | SlotsFull
        
        | WaitPlayers of int
        | GameStart of bool

        | Game of Remade.Answ
    type Log =
        | Login
        | Logout
    let logProc (name:UserName) names slots = function
        | Logout ->
            match Map.tryFind name names with
            | None -> Success false, None
            | _ -> 
                let state = Map.add name false names, slots + 1
                Success true, Some state
        | Login ->
            if slots = 0 then SlotsFull, None
            elif name = "" then NameEmpty, None
            else
                match Map.tryFind name names with
                | None | Some false ->
                    let state = Map.add name true names, slots - 1
                    Success true, Some state
                | Some true -> NameBusy, None
    module nameReg2 =
        type Req =
            | Log of Log
            | GetState
            | Input of Remade.Inputs
        type Msg =
            | Post of UserName * Req * AsyncReplyChannel<Answ>
            //| Fetch of AsyncReplyChannel<Set<string>>

        let replyChannel (r:AsyncReplyChannel<_>) x = r.Reply x
        let receive (mailboxProc:MailboxProcessor<_>) = mailboxProc.Receive()
        type State = { Names:Map<UserName, bool>; EmptySlots:int }

        module Continues =
            let start inbox (m:MailboxProcessor<Remade.Msg>) st =
                let interp name ({Names = names; EmptySlots = slots}) = function
                    | Log x -> 
                        let (answ, x) = logProc name names slots x
                        let state =
                            match x with
                            | None -> None
                            | Some(names, slots) -> Some {Names = names; EmptySlots = slots}
                        answ, state
                    | GetState ->
                        if slots = 0 then
                            let r = m.PostAndReply(fun r -> Remade.Msg.Post(Remade.Req.GetState name, r))
                            Game r, None
                        else WaitPlayers slots, None
                    | _ -> failwith "not impl"
                let rec loop st =
                    async {
                        let! msg = receive inbox
                        match msg with
                        //| Fetch r -> r.Reply names; return ()
                        | Post(name, req, r) -> 
                            let (answ, state) = interp name st req 
                            r.Reply answ
                            match state with
                            | None -> return! loop st
                            | Some st -> return! loop st
                    }
                loop st

        type NameRegister(emptySlots) =
            let counter =
                let interp name ({Names = names; EmptySlots = slots}) = function
                    | Log x -> 
                        let (answ, x) = logProc name names slots x
                        let state =
                            match x with
                            | None -> None
                            | Some(names, slots) -> Some {Names = names; EmptySlots = slots}
                        answ, state
                    | GetState ->
                        //if slots = 0 then GameStart true, None
                        //else WaitPlayers slots, None
                        WaitPlayers slots, None
                    | _ -> GameStart false, None
                MailboxProcessor.Start(fun inbox -> 
                    let rec loop st =
                        async {
                            let! msg = inbox.Receive()
                            match msg with
                            //| Fetch r -> r.Reply names; return ()
                            | Post(name, req, r) -> 
                                let (answ, state) = interp name st req 
                                r.Reply answ
                                match state with
                                | None -> return! loop st
                                | Some st -> return! loop st
                        }
                    loop {Names = Map.empty; EmptySlots = emptySlots })
            member __.Post(name, req) = counter.PostAndReply(fun x -> Post(name, req, x))
        /// <summary> return names and dispose </summary>
        //member __.Fetch () = counter.PostAndReply(Fetch)

