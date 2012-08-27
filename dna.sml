structure DNA :> DNASIG = 
struct

open Common

type moth_dna = real array

type index = int

val SIZE = 12

val WINGSPAN = 0
val HEIGHT = 1
val DENSITY = 2
val FORCE = 3
val RED = 4
val GREEN = 5
val BLUE = 6
val PLANPROB = 7
val MOTHWEIGHT = 8
val LIGHTBULBWEIGHT = 9
val BLOCKWEIGHT = 10
val BALLWEIGHT = 11

val dna_mins = Array.fromList [0.1,
                               0.1,
                               4.0,
                               1.0, (* force *)
                               0.3,
                               0.3,
                               0.3,
                               0.0,

                               0.0,
                               0.0,
                               0.0,
                               0.0]

val dna_maxs = Array.fromList [0.5,
                               0.4,
                               12.0,
                               5.0,   (* force *)
                               1.0,
                               1.0,
                               1.0,
                               0.005, (* planprob *)
                              
                               0.5,  (* mothweight *)
                               1000.0, (* lightbulbweight *)
                               100.0,  (* blockweight *)
                               100.0]  (* ballweight *)

fun get dna idx =
    let val mn = Array.sub (dna_mins, idx)
        val mx = Array.sub (dna_maxs, idx)
        val v = Array.sub (dna, idx)
    in
        v * (mx - mn) + mn
    end

fun fudge x =
    let val r = 0.4 * (random_real ())  -  0.2
        val x1 = x + r
        val x2 = if x1 > 1.0
                 then 1.0
                 else if x1 < 0.0
                 then 0.0
                 else x1
    in x2 end


fun randomize dna =
    let 
        val dna1 = Array.tabulate
                       (SIZE,
                        (fn ii => fudge (Array.sub (dna, ii))))
    in dna1 end

fun random dna_array = 
    let val n = Array.length dna_array
        val ii = random_nat (n + 1)
    in
        if ii = n
        then Array.tabulate (SIZE, fn _ => random_real ())
        else randomize (Array.sub (dna_array, ii))
    end

end
