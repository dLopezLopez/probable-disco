(*
@author Daniel López López
*)

#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

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
