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

let toTime _ =
    failwith "Not implemented"

let digits _ =
    failwith "Not implemented"

let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"