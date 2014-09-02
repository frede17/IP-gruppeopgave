(* Uge 1 gruppe aflevering for IP
Rose
Jeppe
Frederik
Nikolaj
*)
(* 1G1
I denne opgave startede vi med at skrive funktionen for den ene loesning af
andengradsligningen. Da vi sikrede os at funktionen gav os en rigtig loesning,
lavede vi en tubbel med ogsaa funktionen for den anden loesning. Tubbelen
udregnede dermed begge vaerdier.
*)
fun solve2 (a, b, c) =
           ((~b + Math.sqrt((b*b)-(4.0 * a * c))) / (2.0 * a),
           (~b - Math.sqrt((b*b)-(4.0 * a * c))) / (2.0 * a));
(* 1G2
Her udregnede vi paa forhaand resultatet i haanden for at kunne vide om vores
funktion gav det korrekte resultat. Kaldet giver ~0.5 og ~1.0. Resultatet er
korrekt.
*)
val ig2 = solve2 (2.0, 3.0, 1.0);

(* 1G3
Kaldet giver: Uncaght exception: Domain.
Vi faar ingen loesning fordi at diskriminaten (b*b)-4*a*c er negativ (~23)
Derved ingen loesning.
*)
(* val ig3 = solve2 (2.0, 3.0, 4.0); Fejl meddelelse *)

(* 1G4
Hvis der kommer et ulige tal ind i funktionen, benytter powerNew den gamle
power funktion, hvorimod når der kommer et lige tal ind i funktionen, vil
powerNew funktionen bruge den nye funktionsmaade. Den nye funktionsmaade udnytter
det omskrevne (a, (n div 2)) dette optimere potensfunktionen til at beregne
færre gange.
Bacistilfaelde (a, 2) må man aendre praemissen da der ellers ville forekomme
et infinite loop.
*)
fun powerNew (a, 0) = 1
  | powerNew (a, 2) = a * a
  | powerNew (a, n) = if (n mod 2)=1
                      then a * powerNew (a, n-1)
                      else powerNew( powerNew(a, (n div 2)), 2);
val pn_21 = powerNew(2, 21);










