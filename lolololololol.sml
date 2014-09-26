(*individuel aflevering 4 Frederik Leed Henriksen*)
(*opgave 1*)
datatype ('a, 'b) either = Left of 'a | Right of 'b;

datatype 'a option = NONE | SOME of 'a;

val test = [Right 0, Left 2, Right 7, Left 1, Left 10];

fun sort_right [] = []
  | sort_right((Right a):: xs)= [a] @ sort_right(xs)
  | sort_right((Left a):: xs) = [] @  sort_right(xs);
fun sort_left [] = []
  | sort_left((Right a):: xs)= []@sort_left(xs)
  | sort_left((Left a):: xs) = [a]@sort_left(xs);

fun partEither (xs)= (sort_left(xs), sort_right(xs));
