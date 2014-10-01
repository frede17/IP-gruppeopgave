(*lavet af: Nikolaj Bang Nielsen; Frederik Leed Henriksen;
  Rose Sofie Greve; Jeppe Schönemann Skov*)
(* Datatyper *)
datatype pitchclass = C | Cis | D | Dis | E | F | Fis
                    | G | Gis | A | Ais | H | r
type octave = int
type pitch = pitchclass * octave
type duration = int
datatype music = Note of duration * pitch
                |Rest of duration
type melody = music list

(* 4G1 *)
local
(* udregner oktaven *)
fun udregnOktav 0 = ""
  | udregnOktav oc = if oc > 0
                     then "'" ^ udregnOktav(oc - 1)
                     else "," ^ udregnOktav(oc + 1)
(* converterer Pitchclass til string *)
fun pcToString  C = "c"
  | pcToString Cis = "cis"
  | pcToString D = "d"
  | pcToString Dis = "dis"
  | pcToString E = "e"
  | pcToString F = "f"
  | pcToString Fis = "fis"
  | pcToString G = "g"
  | pcToString Gis = "gis"
  | pcToString A = "a"
  | pcToString Ais = "ais"
  | pcToString H = "h"

in
(* musicToString : music -> string
*  musicToString konventerer en tone til en læsbar string.
*  funktionen tager også højde for pauser.
*)
fun musicToString (Rest (d)) = "r" ^ Int.toString d
     | musicToString (Note (d, (p, oc))) = pcToString p ^
                                           udregnOktav oc ^
                                           Int.toString d;
end;

(* Testcases for funktionen: musicToString *)
val test_musicTS_0 = musicToString (Note (4, (Fis, 2))) = "fis''4";
val test_musicTS_1 = musicToString (Note (16, (G, ~4))) = "g,,,,16";
val test_musicTS_2 = musicToString (Rest (8)) = "r8";
val test_musicTS_3 = musicToString (Note (8, (Ais, 0))) = "ais8";


(* 4G2
*  melodyToString : melody -> string
*  Funktionen tager en liste af toner og pauser og printer den ud som en string.
*)
fun melodyToString [] = ""
  | melodyToString [x] = musicToString x
  | melodyToString (x :: xs) = musicToString x ^ " " ^ melodyToString xs;

val test_melodyTS_0 = melodyToString [(Note (4, (Fis, 2))),
                                      (Note (16, (G, ~4))),
                                      (Rest (8)),
                                      (Note (8, (Ais, 0)))]
                                    = "fis''4 g,,,,16 r8 ais8";

(* Testcases for melodyToString *)
val test_melodyTS_1 = melodyToString [] = "";
val test_melodyTS_2 = melodyToString [(Note (4, (Fis, 2)))] = "fis''4";


(* 4T3 *)
type abspitch = int
local
(* converterer pitchclass til heltal*)
fun pcToInt C = 0
  | pcToInt Cis = 1
  | pcToInt D = 2
  | pcToInt Dis = 3
  | pcToInt E = 4
  | pcToInt F = 5
  | pcToInt Fis = 6
  | pcToInt G = 7
  | pcToInt Gis = 8
  | pcToInt A = 9
  | pcToInt Ais = 10
  | pcToInt H = 11;

(* absolutePitch : pitch * abspitch
*  Funktionen kombinerer pitchclass og oktave i et enkelt tal
*  den følger en 5-oktavers klaver.
*)
in
fun absolutePitch (p, oc) = pcToInt p + (oc *12);
end;
(* Testcases for absolutePitch *)
val test_absP_0 = absolutePitch (A, ~1) = ~3;
val test_absP_1 = absolutePitch (G, 1) = 19;
val test_absP_2 = absolutePitch (Fis, 0) = 6;


(* 4G4 *)
local
(* converterer heltil til pitchs *)
fun intToP 0 = C
  | intToP 1 = Cis
  | intToP 2 = D
  | intToP 3 = Dis
  | intToP 4 = E
  | intToP 5 = F
  | intToP 6 = Fis
  | intToP 7 = G
  | intToP 8 = Gis
  | intToP 9 = A
  | intToP 10 = Ais
  | intToP 11 = H;
in
(* pitch : abspitch * pitch
*  Funktionen foretager den modsatte konventering end absolutePitch
*  denne konventerer fra et heltal til et par bestående af tone og oktav.
*)
fun pitch a = (intToP(a mod 12), a div 12);
end;
(* Testcases for pitch *)
val test_pitch_0 = pitch ~3 = (A, ~1);
val test_pitch_1 = pitch 19 = (G, 1);
val test_pitch_2 = pitch 6 = (Fis, 0);
val test_pitch_3 = pitch ~11 = (Cis, ~1);


(* 4G5
*  transpose : int -> melody -> melody
*  transpose har til formål at flytte alle toner
*  det samme antal skridt i samme retning
*)
fun transpose n []                        = []
  | transpose n ((Rest (d)) :: xs)        = (Rest (d)) :: transpose n xs
  | transpose n (Note (d, (p, oc)) :: xs) =
    if n < 0
    then (Note (d, pitch((absolutePitch(p, oc)) - n)))
         :: transpose n xs
    else (Note (d, pitch((absolutePitch(p, oc)) + n)))
         :: transpose n xs;



val test_trans_0 = transpose 3 [(Note (4, (Fis, 2))),
                                (Note (16, (G, ~4))),
                                (Note (8, (Ais, 0)))] =
 [Note(4, (A, 2)), Note(16, (Ais, ~4)), Note(8, (Cis, 1))];
val test_trans_1 = transpose 3  [(Rest 8),
                                (Note (4, (Fis, 2))),
                                (Note (16, (G, ~4))),
                                (Note (8, (Ais, 0)))] =
 [Rest 8, Note(4, (A, 2)), Note(16, (Ais, ~4)), Note(8, (Cis, 1))];


(* 4G6 *)

local
(* sammenligner pitches og beslutter hvilken der er stoerst*)
fun comparePitch ((Note (d, x)),(Note (d1, x1))) =
               if (absolutePitch x) <= (absolutePitch x1)
               then (Note (d, x1))
               else (Note (d, x))
  | comparePitch ((Note (d, x)), (Rest d1)) = (Note (d, x))
  | comparePitch ((Rest d1), (Note (d, x))) = (Note (d, x))
  | comparePitch ((Rest d), (Rest (d1)))    = (Rest (d)) ;
in
(* funktionen maxPitch skal returnere den pitch i en melody
 * music som har den hoejeste tone *)
fun maxPitch []             = raise Domain
  | maxPitch [Rest d]       = raise Domain
  | maxPitch [a]            = a
  | maxPitch (a :: b :: xs) = maxPitch(comparePitch((a,b)):: xs)
end;
(*tests*)
val test_6_1 = maxPitch[(Note (4, (Fis, 2))),(Note (16, (G, ~4))),
                      (Rest (8)),(Note (8, (Ais, 0)))]=(Note (4, (Fis, 2)));
val test_7_2 = maxPitch[(Note (4, (Fis, 2)))]= (Note (4, (Fis, 2)));
val test_maxP_Tom = (maxPitch [];false) handle Domain => true
                                            | _ => false;
val test_maxP_Rests = (maxPitch [Rest 3, Rest 4, Rest 4];false)
                      handle Domain => true | _ => false;
(* 4G7 *)
(*funktonen skal lave en melody (music list) om til et rationelt tal der er
laengden af hele melodien*)
datatype rational = Ratio of int * int;
local
(* Fra duration til rational vaerdi *)
fun durationToRatio (Note (d, x)) = Ratio (1, d)
  | durationToRatio (Rest d)      = Ratio (1, d) ;
(* indsaet de rationale vaerdier i en liste *)
fun ratioToList xs = map durationToRatio xs;
(* Funktion til at laegge de rationale vaerdier sammen *)
fun plusListFun (Ratio (a, b)) (Ratio (c, d)) =
               (Ratio((a * d + c * b), (b * d)));
(* funktionen der udfører plusListFun paa listen *)(*
fun sumList ((Ratio(x))::xs) = foldl plusListFun Ratio(x) xs;*)
in(*
fun duration []        = raise Fail "Ingen musik"
  | duration [x]       = (1, tth2(x))
  | duration (xs)      = (1, xs);*)
end;
(*tests*)(* Alle tests er sat i (**) for koden skal kunne koeres
val test_7_1 = (duration[];false) handle Fail x => true | _ => false;
val test_7_2 = duration[(Note (4, (Fis, 2)))] ;
val test_7_3 = duration[(Note (4, (Fis, 2))),(Note (16, (G, ~4))),
                      (Rest (8)),(Note (8, (Ais, 0)))];
 * der er forsoegt at faa funktionen til at spille, men dette har ikke
 * vaeret helt muligt men vi har forsoegt os.
 * evt. hvordan kan man koble ratioToList med sumList?
*)
