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

val get_dna_val : moth_dna -> index -> real

val moth1 : moth_dna
val moth2 : moth_dna

end
