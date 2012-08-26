structure DNA :> DNASIG = 
struct

open Common

type moth_dna = real array

type index = int

val SIZE = 8

val WINGSPAN = 0
val HEIGHT = 1
val DENSITY = 2
val FORCE = 3
val RED = 4
val GREEN = 5
val BLUE = 6
val PLANPROB = 7

val dna_mins = Array.fromList [0.1,
                               0.1,
                               4.0,
                               2.0,
                               0.3,
                               0.3,
                               0.3,
                               0.0]

val dna_maxs = Array.fromList [0.6,
                               0.5,
                               12.0,
                               7.0,
                               1.0,
                               1.0,
                               1.0,
                               0.1] 

fun get dna idx =
    let val mn = Array.sub (dna_mins, idx)
        val mx = Array.sub (dna_maxs, idx)
        val v = Array.sub (dna, idx)
    in
        v * (mx - mn) + mn
    end

fun random () = 
    Array.tabulate (SIZE, fn _ => random_real())

val moth1 = Array.fromList [0.2, 0.3, 0.5, 1.0, 0.9, 0.8, 0.1, 1.0]
val moth2 = Array.fromList [0.9, 0.4, 0.4, 0.9, 0.0, 0.9, 0.99, 0.01]

end
