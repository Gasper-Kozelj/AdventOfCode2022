let string_to_char_list niz =
  List.init (String.length niz) (String.get niz)

let poisci_prvo_cetverico sez =
  let rec aux (x :: xs) prvi drugi tretji acc =
    if (x = prvi ||
        x = drugi ||
        x = tretji ||
        prvi = drugi ||
        prvi = tretji ||
        drugi = tretji ||
        prvi = '-')
    then aux xs drugi tretji x (acc + 1)
    else acc
  in 
  aux sez '-' '-' '-' 1

let naloga1 vsebina_datoteke =
  vsebina_datoteke
  |> string_to_char_list
  |> poisci_prvo_cetverico
  |> string_of_int

let rec char_in_list sez a =
  match sez with
  | [] -> false
  | x :: xs -> (if x = a then true else char_in_list xs a)

let prvih_n sez n =
  let rec aux list indeks acc =
    match list with
    | [] -> failwith "Prekratek seznam."
    | x :: xs -> if indeks = 0 then acc else aux xs (indeks - 1) (x :: acc)
  in 
  aux sez n []

let rec vsi_razlicni sez =
  match sez with
  | [] -> true
  | x :: xs -> (if char_in_list xs x then false else vsi_razlicni xs)

let prvih_n_razlicnih sez n =
  prvih_n sez n
  |> vsi_razlicni

let prvo_sporocilo_dolzine_n n sez =
  let rec aux list acc =
    match list with
    | x :: xs -> if prvih_n_razlicnih list n then acc else aux xs (acc + 1)
    | _ -> failwith "Ni sporocila."
  in
  (aux sez 1) + n - 1

let naloga2 vsebina_datoteke =
  vsebina_datoteke
  |> string_to_char_list
  |> prvo_sporocilo_dolzine_n 14
  |> string_of_int

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
  let vsebina_datoteke = preberi_datoteko "inputs/day_6.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "outputs/day_6_1.out" odgovor1;
  izpisi_datoteko "outputs/day_6_2.out" odgovor2