module Synthesis

let abelar a =
    (a > 12) && (a < 3097) && (a%12 =0)

let area abase height =
            match abase < 0.0 || height < 0.0 with
            |true -> failwith "Input values are negative"
            |false-> abase/ 2.0 * height

let zollo z =
    match z < 0 with
    | true -> z * -1
    | false -> z * 2

let min a b =
    match a < b with
    |true -> a
    |false -> b

let max a b =
    match a > b with
    |true -> a
    |false -> b
   

let ofTime h m s =
    (h * 3600) + (m * 60) + s

let toTime b =
    match b < 0 with
    | true -> (0,0,0)
    | false ->
    let h = b/3600
    let m = (b % 3600)/60
    let s =( b % 3600) % 60
    (h,m,s)


let digits s=
   let rec count i =
       match i < 10 && i > -10 with
       |true -> 1 
       | false -> count(i/10) + 1
   count s

let minmax m=
    let a,b,c,d = m
    (min a b |> min c |> min d , max a b |> max c |> max d)

let isLeap yr = match yr >= 1582 with
    |false -> failwith "Invalid Year "
    | true -> yr % 4= 0 && yr % 100 <> 0 || yr % 400 =0
    |false -> false | true -> true

let month m = match m < 1 || m > 12 with
    | true -> failwith "Invalid month"
    | false -> 
              match m with
              |1 -> ("January", 31) | 2 -> ("February", 28) | 3 ->  ("March", 31) | 4 -> ("April", 30)
              |5 -> ("May", 31) | 6->  ("June", 30) | 7 ->  ("July", 31) | 8 ->("August", 31)
              |9-> ("September", 30) |10 -> ("October", 31) | 11-> ("November", 30) | 12 -> ("December", 31)
     

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay d y =
       match d < 366 & d >0 || y <1582 with
       |false  ->  failwith "Invalid Input"
       //|true -> 

let sqrt n=
    let rec calculate guess i=
       match i with 
       | 10 -> guess
       | _ ->
         let g =(guess + n/guess)/ 2.0
         calculate g (i + 1)
    match n <=0.0 with
       | true -> failwith "Impossibru!"
       |_ ->
        calculate(n/2.0) 0

let coord _ =
    failwith "Not implemented"