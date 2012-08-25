signature DNASIG = 
sig

type moth_dna

type index

val DNA_WINGSPAN : index
val DNA_HEIGHT : index
val DNA_DENSITY : index
val DNA_FORCE : index
val DNA_RED : index
val DNA_GREEN : index
val DNA_BLUE : index

val get_dna_val : moth_dna -> index -> real

val moth1 : moth_dna
val moth2 : moth_dna

end
