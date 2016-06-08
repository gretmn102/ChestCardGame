module ListCircle

// можно попробовать реализовать Closure через System.Collections.Generic.Queue
type 'a Clos = { Data:'a; mutable Next:Clos<'a> option }
type Closure<'a when 'a : equality> (inputList:'a list) = 
    let mutable last = None
    let mutable curr = None

    let createFromL l =
            let E1 = { Data = List.head l; Next = None }
            let rec f = function
                    | [] -> E1
                    | [c] -> 
                        let current = { Data = c; Next = Some(E1) }
                        last <- Some current
                        current
                    | h::t -> { Data = h; Next = Some(f t) }
            E1.Next <- Some(f (List.tail l))
            curr <- Some E1
    
    let mutable count = inputList |> List.length
    let mutable newClos = true
    
    do
        if count <> 0 then
            createFromL inputList

   // member this.Cur() = curr.Value.Data
   // member this.Last() = last.Value.Data

    member this.Current() = if newClos then failwith "новый clos" else curr.Value.Data
    member this.Count() = count
    member this.Next() =
            if count < 1 then
                false
            else
                if newClos then
                    //curr <- last
                    newClos <- false
                else
                    last <- curr
                    curr <- curr.Value.Next
                true
                
    member this.Remove() = 
            if newClos then
                failwith "новый clos"
            newClos <- true
            last.Value.Next <- curr.Value.Next
            curr <- curr.Value.Next
            count <- count - 1

    member this.Find (elem:'a) =
                        if this.Next() = false then failwith "список пуст"
                        if inputList |> List.exists ((=) elem) |> not then failwithf "список %A не содержит %A" inputList elem
                        //while last.IsNone do if this.Next() = false then failwith "пуст"
                        while curr.Value.Data <> elem do this.Next() |> ignore

(* //test
let c = new Closure<_>([1..41])

let next () = for i in 0..2 do c.Next()
while (c.Count() <> 2) do
    next ()
    c.Current() |> printfn "%d"
    c.Remove()
c.Next()
c.Current()

let testPrimary l =
    let moves (c:Closure<_>) steps =
        let res = ref true
        for _ in 1..steps do
            res := c.Next() //|> ignore
        if !res then
            Some <| c.Current()
        else
            None

    let c = new Closure<_> ([1..3])

    let k c steps =
        match moves c steps with
        | Some(curr) ->
            c.Remove()
            Some curr
        | None -> None

    let rec f =
        function
        | [] -> []
        | h::t ->
            k c h :: f t
    in f l


// тест Closure
// работает следующим образом
// задается программа в виде списка l
// элементы которого обозначают сколько Next'ов нужно вызвать. После вызова Next'ов, вызывается Remove
// c.Next(steps)
// let curr = c.Current
// c.Remove()
// curr :: c
let testPseudo l =
    let rec f iCurr l =
        function
        | [] -> []
        | count::t ->
            match l with
            | [] -> [ None]
            | [a] -> (Some a) :: f 0 [] t
            | _ ->
                let lstLength = List.length l
                let next indexCurr steps = (indexCurr + steps) % (lstLength)
                (*
                let back i s =
                    let f n = 
                        (lstLength - 1) - (n % lstLength)
                    f ((lstLength - 1) + s - i)
                    *)
                let iC = next iCurr count
                let cu = l.[iC]

                let valCurr = l.[next iC 1]
                let lAfterRem = l |> List.filter ((<>) l.[iC])
        
                let iLast = lAfterRem |> List.findIndex ((=) valCurr)

                //(Some cu, Some lAfterRem.[iLast]) :: f iLast lAfterRem t
                (Some cu):: f iLast lAfterRem t
    in f 0 [1..3] (l |> List.map (fun x -> x - 1))
(*
let p = Comb.Pow [[1..3]; [1..2]; [1]; [1];]
let t = p
        |> List.map (fun x ->
                        let prim = testPrimary x
                        let pseudo = testPseudo x
                        if prim = pseudo then
                            x, prim, pseudo, true
                        else
                            failwith "fail")
t
*)*)