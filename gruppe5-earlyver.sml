use "InstagraML.sml";
structure I = InstagraML;
type point = real * real
datatype 'col figure = Circle of 'col * point * real
                     | Rectangle of 'col * point * point
                     | Over of 'col figure * 'col figure;
datatype primaryColours = Black | Red | Green | Blue
                        | White | Cyan | Magenta | Yellow
val redCircleOverBlueSquare = Over (Circle (Red, (0.0, 0.0), 1.8),
                                    Rectangle (Blue , (~1.5, ~1.5), (1.5, 1.5)));

(* 5G1
*  toRGB : primaryColours -> int * int * int
*  toRGB oversaetter farver af typen primaryColors til RGB. *)

fun toRGB Black = (0, 0, 0)
  | toRGB Red = (255, 0, 0)
  | toRGB Green = (0, 255, 0)
  | toRGB Blue = (0, 0, 255)
  | toRGB White = (255, 255, 255)
  | toRGB Cyan = (0, 255, 255)
  | toRGB Magenta = (255, 0, 255)
  | toRGB Yellow = (255, 255, 0);

val test_toRGB = toRGB Cyan = (0, 255, 255);

(* 5G2
*  reColour : 'a figure -> ('a -> 'b) -> 'b figure
*  reColour aendre farve-navnet c til noget andet. *)

fun reColour (Circle (c, p, r)) f      = (Circle (f c, p, r))
  | reColour (Rectangle (c, bl, tr)) f = (Rectangle (f c, bl, tr))
  | reColour (Over (fig1, fig2)) f     = (Over (reColour fig1 f,
                                                reColour fig2 f));

val test_reColour0 = reColour redCircleOverBlueSquare toRGB =
                     Over (Circle ((255,0,0), (0.0, 0.0), 1.8),
                           Rectangle ((0,0,255), (~1.5, ~1.5), (1.5, 1.5)));
val test_reColour1 = reColour (Circle (Red, (0.0, 0.0), 1.8)) toRGB
                            = (Circle ((255,0,0), (0.0, 0.0), 1.8));
val test_reColour2 = reColour (Rectangle (Blue, (~1.5, ~1.5), (1.5, 1.5))) toRGB
                            = (Rectangle ((0,0,255), (~1.5, ~1.5), (1.5, 1.5)));


(* 5G3
*  colourOf : 'a figure -> point -> 'a option
*  colourOf fortaeller om et punkt (x, y) ligger i billedet
*  og hvilken farve billedet har i dette punkt. *)

fun colourOf (Circle (c, (p0, p1), r)) (x, y)               =
              if (x-p0)*(x-p0) + (y-p1)*(y-p1) <= r*r
              then SOME c
              else NONE
  | colourOf (Rectangle (c, (bl0, bl1), (tr0, tr1))) (x, y) =
              if  bl0 <= x andalso x <= tr0
              andalso bl1 <= y andalso y <= tr1
              then SOME c
              else NONE
  | colourOf (Over (fig1, fig2)) (x, y)                     =
              case colourOf fig1 (x, y)
                   of  SOME c => SOME c
                     | NONE   => colourOf fig2 (x, y);

val test_colourOf0 = colourOf redCircleOverBlueSquare (0.0, 0.0) = SOME Red;
val test_colourOf1 = colourOf redCircleOverBlueSquare (3.0, 0.0) = NONE;
val test_colourOf2 = colourOf (Circle (Red, (0.0, 0.0), 1.8)) (2.0, 2.0) = NONE;
val test_colourOf3 = colourOf (Rectangle (Blue, (~1.5, ~1.5), (1.5, 1.5)))
                              (1.0, 1.0) = SOME Blue;


(* 5G4
*  hasAColour : 'a figure -> point -> bool
*  hasAColour fortaeller om billedet har en farve i punket p. *)

fun hasAColour bil p  = if colourOf bil p = NONE
                        then false
                        else true;

val test_hasAC0lour0 = hasAColour redCircleOverBlueSquare (0.0, 0.0) = true;
val test_hasAColour1 = hasAColour redCircleOverBlueSquare (3.0, 0.0) = false;
val test_hasAColour2 = hasAColour (Circle (Red, (0.0, 0.0), 1.8)) (2.0, 2.0)
                                   = false;
val test_hasAColour3 = hasAColour (Rectangle (Blue, (~1.5, ~1.5), (1.5, 1.5)))
                                  (1.0, 1.0) = true;
