use "InstagraML.sml";
structure I = InstagraML;
val magenta = (255, 0, 255);
val torben = I.readBMP("torben.bmp");

(* 3G1
Vi vil foerst og fremmest starte med at finde hoejdeforskellen
paa de to billeder. Det lavere billede skal saettes sammen med
to ensfarvede boxe. En over og en under billedet, saa billedet paa
den maade centreres. Naar vi ved hvor mange pixels det lave billede
mangler, kan vi bruge div og mod til at finde ud af hvor hoeje
de to boxe skal vaere. (Bredden paa boxene er samme som paa billedet).
Hvis hoejdeforskellen er ulige, vaelger vi at den oeverste box skal
vaere en pixel hoejere end den nederste box.
Vi vil ved hjaelp af I.beside funktionen saette de to boxe og det
laveste billede sammen, og derefter vende det vha. clockwise.
Til sidste vil vi saette det hoejeste billede og billede/boxe-
sammensaetningen sammen. *)

(* 3G2
a) Vi vil afproeve funktionen der laver et ensfarvet billede,
ved at lave to testtilfaelde med forskellige
stoerrelser.

b) Vi vil teste funktionen der saetter billedet sammen med to ensfarvede
billeder, ved baade at lave et testtilfaelde hvor de to boxe har samme
hoejde, og et hvor de to boxe har forskellige hoejder.

c) Vi vil afproeve den faerdige funktion med et tilfaelde hvor
hoejdeforskellen er lige, et tilfaelde hvor hoejdeforskellen
er ulige, og et tilfaelde hvor de to billeder er ens.
*)


(* 3G3
a) solid : colour -> int * int -> image
   solid laver et ensfarvet billede, i den oenskede stoerrelse.

b) padVertical : int -> image -> image
   padVertical saetter tre billeder sammen, og vender dem lodret.
*)


(* 3G4
solid : colour -> int * int -> image
solid laver et ensfarvet billede, i den oenskede stoerrelse. *)
fun solid c h b = I.scale (Real.fromInt(h)) (Real.fromInt(b)) (I.pixel c);

(* billederne mangenta0 og magenta1 har de forventede stoerrelser *)
val test_solid0 = I.writeBMP ("magenta0.bmp", solid magenta 200 300);
val test_solid1 = I.writeBMP ("magenta1.bmp", solid magenta 250 250);

(* 3G5
padVertical : int -> image -> image
padVertical saetter tre billeder sammen, og vender dem lodret. *)
fun imageConcat [] = raise Empty
  | imageConcat [x] = x
  | imageConcat (x :: xs) = I.beside(x, imageConcat(xs));
fun counterClockwise img = I.clockwise(I.clockwise(I.clockwise(img)));

fun padVertical b img =  I.clockwise (imageConcat
                         ([solid magenta (I.width(img)) b,
                         counterClockwise(img),
                         solid magenta (I.width(img)) b]));

val test_pad0 = padVertical 200 torben;
I.writeBMP ("torben3.bmp", test_pad0);
