module Proxy

module Alternative = 
    type T<'readData, 'writeData> = //{ Reader: 'readData; Writer:'writeData}
        | Read of ('readData -> T<'readData, 'writeData>)
        | Write of 'writeData * (unit -> T<'readData, 'writeData>)
        | End

    let pars x =
        let fp (f, s) next =
            match f x with
            | true, _ -> sprintf "This string are %s type." s
            | false, _ -> next()
        fp (System.Int32.TryParse, "int32") (fun () ->
        fp (System.Boolean.TryParse, "bool") (fun () -> 
        fp (System.Double.TryParse, "double") (fun () -> "unknown")))

    let rec f () =
        Write("This sample text. After here need input some text.", fun () -> 
        Read(fun x -> 
        let res = pars x
        Write(res, fun () ->
        Write("но этого ведь не достаточно", fun () ->
        Write("еще чем-нибудь забьем принимающий поток", fun () ->
        Write("send me something", fun () ->
        Read(fun x -> 
        Write(sprintf "What this: %s" x, fun () ->
        Write("Я нуждаюсь в большем кол-ве информации", fun () -> f())))))))))
    
    //    write "this is ..."
    //    let x = read()
    //    write (pars x)
    //    write "но этого ведь не достаточно"
    //    write "еще чем-нибудь забьем в принимающий поток"
    //    write "send me something"
    //    let x = read()
    //    write (sprintf "What this: %s" x)
    //    write "Я нуждаюсь в больше кол-ве информации"
    //    f()


type Msg = string
type Resp = 
    | ReadThat of Msg
    | EmptyMsgBox
    | Done
type Req =
    | Read
    | Write of Msg
type Client = One | Two
type Ty =
    | Req of AsyncReplyChannel<Resp> * Client * Req
    //| Wrt of Msg
type State = { PostForOne: Msg list; PostForTwo: Msg list }
let mail = MailboxProcessor.Start(fun inbox ->
    let rec loop st =
        async {
            let! Req(reply, client, req) = inbox.Receive()
            match req with
            | Write msg -> 
                reply.Reply Done
                let st =
                    match client with
                    | One -> {st with PostForTwo = msg :: st.PostForTwo}
                    | Two -> {st with PostForOne = msg :: st.PostForOne}
                return! loop st
            | Read -> 
                let r post = 
                    match post with
                    | [] -> EmptyMsgBox, []
                    | h::t -> ReadThat h, t
                let m, st = 
                    match client with
                    | One -> 
                        let m, msgBox = r st.PostForOne
                        m, { st with PostForOne = msgBox }
                    | Two -> 
                        let m, msgBox = r st.PostForTwo
                        m, { st with PostForTwo = msgBox }         
                reply.Reply m
                return! loop st
            }
    loop { PostForOne = []; PostForTwo = [] })

let oneRead () = mail.PostAndReply(fun x -> Req(x, One, Read))
let oneWrite s = mail.PostAndReply(fun x -> Req(x, One, Write s))
let twoRead () = mail.PostAndReply(fun x -> Req(x, Two, Read))

