signature DNASIG = 
sig

type moth_dna

type index

val WINGSPAN : index
val HEIGHT : index
val DENSITY : index
val FORCE : index
val RED : index
val GREEN : index
val BLUE : index
val PLANPROB : index

val get : moth_dna -> index -> real

val random : unit -> moth_dna

val moth1 : moth_dna
val moth2 : moth_dna

end
