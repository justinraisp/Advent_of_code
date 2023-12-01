let rec najdi_prvo_stevilo (str : char list) = match str with
    | [] -> None 
    | x :: xs -> if x > '0' && x <= '9' then Some(int_of_char x - int_of_char '0')
    else
      najdi_prvo_stevilo xs


let rec obrni_list acc = function 
    | [] -> acc
    | x :: xs -> obrni_list (x :: acc) xs    

let rec najdi_zadnje_stevilo (str : char list) = 
    let obrnjen = obrni_list [] str in match obrnjen with
        | [] -> None
        | x :: xs ->
            if x >= '0' && x <= '9' then
              Some(int_of_char x)
            else
              najdi_zadnje_stevilo xs

let rec string_to_char_list str =
    str |> String.to_seq |> List.of_seq

let naloga1 (vsebina_datoteke : string) =
    let vsebina_datoteke_to_char_list = string_to_char_list vsebina_datoteke in
    match najdi_prvo_stevilo vsebina_datoteke_to_char_list with
        | Some num -> string_of_int num
        | None -> "None"
let naloga2 vsebina_datoteke = 
    "10"

    let extract_first_last_numbers (str : string) =
        let rec find_first_last_numbers = function
          | [] -> ""
          | x :: xs ->
              if x >= '0' && x <= '9' then
                let num = int_of_char x - int_of_char '0' in
                match List.rev xs with
                | [] -> string_of_int num
                | y :: ys when y >= '0' && y <= '9' -> string_of_int (10 * num + (int_of_char y - int_of_char '0'))
                | _ -> find_first_last_numbers xs
              else
                find_first_last_numbers xs
        in
        let char_list = str |> String.to_seq |> List.of_seq in
        find_first_last_numbers char_list
      

let _ =
  let preberi_datoteko ime_datoteke =
      let chan = open_in ime_datoteke in
      let vsebina = really_input_string chan (in_channel_length chan) in
      close_in chan;
      vsebina
  and izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = preberi_datoteko "input/day1_0.in" in
  let odgovor1 = extract_first_last_numbers "two65eightbkgqcsn91qxkfvg"
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "output/day_1_1.out" odgovor1;
  izpisi_datoteko "output/day_1_2.out" odgovor2