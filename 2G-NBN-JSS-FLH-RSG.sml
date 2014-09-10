(* Soerg for filerne er i samme mappe *)
use "uge2_distance.sml";
use "uge2_cphmarathon2014.sml";
use "uge2_dhl2014.sml";
(* 2 Gruppe aflevering af
*  Rose
*  Frederik
*  Jeppe
*  Nikolaj
*)
(* 2G1
*  fn: (real * real) list -> real
*  Vi kalder funktionen med 3 basis tilfaelde, hvor det sidste er en rekursion
*  som adder distancen funktion til totalDistance.
*)

fun totalDistance [] = 0.0
  | totalDistance ([x]) = 0.0
  | totalDistance (x1::x2::xs) =  distance(x1,x2) + totalDistance(x2 :: xs);

val marathon_2014 = totalDistance(cphMarathon2014);

(* 2G2
*
*  tid = fn : int * int -> real
*  speeds = fn : ((real * real) * int) list -> real list
*
*  Funktionen tid regner tiden mellem to elementer og omregner
*  fra milisekunder til timer.
*
*  Funktionen speeds laver en liste hvor hvert element
*  er hastigheden fra segment til segment i ruten.
*)
fun fart (t, t2) = ((Real.fromInt t2) - (Real.fromInt t)) / 3600000.0;

fun speeds ([]) = raise Empty
  | speeds ([x]) = []
  | speeds (((x,t) :: (x2, t2) :: xs )) =
    (distance(x,x2) / fart(t, t2)) :: speeds((x2,t2) :: xs);

val storebaelt = [((55.336145, 10.990714), 0),
                  ((55.349420, 11.095690), 1500000),
                  ((55.336145, 10.990714), 3600000)];

val storebaelt_test = speeds(storebaelt);
val dhl2014_test = speeds(dhl2014);

(* 2G3
* funktionen laengde udregner laengden af en liste.
* funktionen averageAll udregner gennemsnittet af tallene i listen.
*)

fun laengde [] = 0.0
  | laengde (x ::xs) = 1.0 + laengde(xs);

fun averageAll [] = raise Empty
  | averageAll [x] = x
  | averageAll (x :: xs) = (x + averageAll(xs)) / laengde(x :: xs);

(* 2G4
*
*  averageSpeeds = fn: ((real * real) * int) list -> real
*
*  Funktionen averageSpeeds beregner gennemsnits hastigheden for ruten,
*  km/t per element delt med antal elementer.
*)
fun averageSpeeds [] = raise Empty
  | averageSpeeds [((x,x2),x3)] = raise Domain
  | averageSpeeds xs = averageAll(speeds(xs));

val storebaelt_testa = averageSpeeds(storebaelt);
val dhl2014_testa = averageSpeeds(dhl2014);

val ingenElement = averageSpeeds([]);
val etElement = averageSpeeds([((1.0,1.0),1)]);
(* ingenElement og etElement stopper funktionen helt *)

(* 2G5
*  Vi syntes ikke at funktionen er stillet paent op. Det hele staar taet, og
*  kommentarerne er skrevet mellem funktionerne, hvilket goer den svaer at laese
*  og forstaa. Der mangler en definition af, hvad det er for en type funktion,
*  men det ser dog ud til, at funktionen goer, det der bliver bedt om.
*
*  For at have vaeret helt sikker paa at funktionen virkede korrekt,
*  kunne man have lavet en testvaerdi, hvor n var laengere end w.
*
*  Set bort fra det, fungerer kommentarene godt. De er korte, men beskriver,
*  hvad funktionen goer.
*  Dog syntes vi ikke, at hver linje behoever sin egen kommentarboks.
*  Det goer det en smule forvirrende at laese.
*)
