(*
@author Daniel López López
*)

#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;
open List;;

let epsilon = Terminal "";;
let zeta = No_terminal "";;
let gic = gic_of_file "../data/ejemplo01.gic";;
let gicApuntes = gic_of_file "../data/ejemploApuntes.gic";;
let apApuntes = ap_of_file "../data/ejemploApuntes.ap";;

let rec arcos_terminales = function
  [] -> conjunto_vacio
  | terminal::t -> agregar (Arco_ap(Estado "1",Estado "1",terminal, terminal, [])) (arcos_terminales t)
;;

let ap_of_gic (Gic( no_terminales, terminales, reglas, s)) =
  let rec aux = function
    [] -> conjunto_vacio
    | Regla_gic(simbolo, nuevos_simbolos)::t -> agregar (Arco_ap(Estado "1",Estado "1",epsilon, simbolo, nuevos_simbolos)) (aux t)
  in Ap(Conjunto [Estado "0";Estado "1"; Estado "2"], terminales, agregar zeta (union terminales no_terminales), Estado "0",
   union (union (aux (list_of_conjunto reglas)) (arcos_terminales (list_of_conjunto terminales))) (Conjunto [Arco_ap(Estado "0", Estado "1", epsilon, zeta, [s;zeta] );Arco_ap(Estado "1", Estado "2", epsilon, zeta, [zeta] )]),
    zeta, Conjunto [Estado "2"])
;;

let traza_ap cadena (Ap (_, _, _, inicial, Conjunto delta, zeta, finales)) =
   let rec aux = function
        ([], [], _, pasos) -> (false,pasos)
      | ([], l, _, pasos) -> aux (l, [], delta, pasos)
      | (_::cfs, l, [], pasos) -> aux (cfs, l, delta, pasos)
      | (cf::cfs, l, a::arcos, pasos) ->
           try
              let
                 ncf = encaja cf a
              in
                 match (es_conf_final finales ncf) with
                 true -> (true, pasos @ [a])
                 | false -> (aux (cf::cfs, ncf::l, arcos, pasos @ [a]))
           with
              No_encaja -> aux (cf::cfs, l, arcos, pasos)
   in
      aux ([(inicial, cadena, [zeta])], [], delta, [])
   ;;
