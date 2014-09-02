
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
val ig3 = solve2 (2.0, 3.0, 4.0);

(* 1G4 *)
fun power 










