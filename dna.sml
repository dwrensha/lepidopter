structure DNA :> DNASIG = 
struct

type moth_dna = real array

type index = int

val SIZE = 7

val WINGSPAN = 0
val HEIGHT = 1
val DENSITY = 2
val FORCE = 3
val RED = 4
val GREEN = 5
val BLUE = 6

val dna_mins = Array.fromList [0.1, 0.1, 0.1, 0.1, 0.3, 0.3, 0.3]
val dna_maxs = Array.fromList [0.7, 0.6, 0.9, 2.0, 1.0, 1.0, 1.0]

fun get_dna_val dna idx =
    let val mn = Array.sub (dna_mins, idx)
        val mx = Array.sub (dna_maxs, idx)
        val v = Array.sub (dna, idx)
    in
        v * (mx - mn) + mn
    end


val moth1 = Array.fromList [0.2, 0.3, 0.5, 1.0, 0.9, 0.8, 0.4]
val moth2 = Array.fromList [0.1, 0.3, 0.4, 0.9, 0.3, 0.9, 0.99]

end