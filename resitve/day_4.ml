let eden_vsebuje_drugega [[a; b]; [c; d]] =
  (a <= c && b >= d) || (a >= c && b <= d)

let prestej_true sez =
  let rec aux list acc =
    match list with
    | [] -> acc
    | true :: xs -> aux xs (acc + 1)
    | false :: ys -> aux ys acc
  in
  aux sez 0

let naloga1 vsebina_datoteke =
  vsebina_datoteke
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char ',')
  |> List.map (List.map (String.split_on_char '-'))
  |> List.map (List.map (List.map int_of_string))
  |> List.map eden_vsebuje_drugega
  |> prestej_true
  |> string_of_int

let se_prekrivata [[a; b]; [c; d]] =
  (a <= d && b >= c) || (b >= c && a <= d)

let naloga2 vsebina_datoteke =
  vsebina_datoteke
  |> String.split_on_char '\n'
  |> List.map (String.split_on_char ',')
  |> List.map (List.map (String.split_on_char '-'))
  |> List.map (List.map (List.map int_of_string))
  |> List.map se_prekrivata
  |> prestej_true
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
  let vsebina_datoteke = preberi_datoteko "inputs/day_4.in" in

  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "outputs/day_4_1.out" odgovor1;
  izpisi_datoteko "outputs/day_4_2.out" odgovor2