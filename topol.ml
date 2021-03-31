(*autor: Jerzy Denisiewicz, recenzent: Błażej Wilkoławski*)

open PMap

(*wierzchołek grafu*)
type 'a node = 'a * 'a list

exception Cykliczne

(* funkcja budująca mapę z listy;
   map to mapa przechowująca liczbę wierzchołków,
   które powinny wystąpić przed danym elementem,
   maplist to po prostu lista tych wierzchołków
   złożoność liniowa względem wielkości wprowadzanych danych*)
let build lista =
  let mapa = ref empty in
  let maplist = ref empty in
  let rec inp = function
  | [] -> ((!mapa), (!maplist))
  | (el, lis)::t ->
    let rec inside_inp = function
      | [] -> ()
      | hed::til ->
      begin
        try
          (find hed (!mapa)) := !(find hed (!mapa)) + 1
        with Not_found ->
          mapa := add hed (ref 1) (!mapa)
      end;
      inside_inp til
    in
    if not (mem el (!mapa)) then
      mapa := add el (ref 0) (!mapa);
    if not (mem el (!maplist)) then
      maplist := add el lis (!maplist)
    else
      maplist := add el (lis@(find el (!maplist))) (remove el (!maplist));
    inside_inp lis;
    inp t
  in
  inp lista

(* funckja wykonująca właściwe sortowanie,
   złożoność liniowa względem wielkości danych*)
let topol lista =
  let (mapa, maplist) = build lista in
  let que = Queue.create () in
  let iledoodw = ref 0 in
  let rec calculate ile =
    if ile = 0 then []
    else
      if Queue.is_empty que then raise Cykliczne
      else
        let act = Queue.take que in
        begin try
          List.iter (fun x ->
            (find x mapa) := !(find x mapa) - 1;
            if !(find x mapa) = 0 then
              Queue.add x que
          ) (find act maplist)
        with Not_found -> ()
        end;
        iledoodw := !iledoodw - 1;
        act :: (calculate (ile - 1))
  in
  iter (fun key elem ->
    iledoodw := !iledoodw + 1;
    if !elem = 0 then
    Queue.add key que) mapa;
  calculate (!iledoodw)
