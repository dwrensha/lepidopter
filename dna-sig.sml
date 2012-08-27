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
val MOTHWEIGHT : index
val LIGHTBULBWEIGHT : index
val BLOCKWEIGHT : index
val BALLWEIGHT : index

val get : moth_dna -> index -> real

val random : (moth_dna Array.array) -> moth_dna

end
