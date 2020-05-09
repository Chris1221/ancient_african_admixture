.libPaths( c( .libPaths(), "~/R/lib64/R/library/") )

library(dplyr)
library(admixr)
library(stringr)
library(data.table)

snps <- eigenstrat("~/well/reich-eigenstrat/v37.2.1240K_HumanOrigins")
snps_subset <- snps %>% filter_bed("~/repos/ancient_migration/analyses/general/output/sgpd/segments/1791095846/S_Han-1.S_Yoruba-1.bed")

# Analysis 1: Are Yorubans closer to each other?
w = c("S_Khomani_San-1.DG",
      "S_Mbuti-1.DG",
      "S_Papuan-1.DG",
      "S_French-1.DG",
      "S_Han-1.DG")

a1a = d(
	W = w,
	X = "S_Yoruba-1.DG",
	Y = "S_Yoruba-2.DG",
	Z = "Chimp.REF",
	data = snps_subset)

a1b = d(
	W = w,
	X = "S_Yoruba-2.DG",
	Y = "S_Yoruba-1.DG",
	Z = "Chimp.REF",
	data = snps_subset)

# Analysis 2:

w = c("S_Khomani_San-1.DG",
      "S_Mbuti-1.DG")
x = c("S_Yoruba-1.DG",
      "S_Yoruba-2.DG")
y = c("S_French-1.DG",
      "S_Han-1.DG",
      "S_Papuan-1.DG")

a2 = d(
       W = w,
       X = x,
       Y = y,
       Z = "Chimp.REF",
       data = snps_subset)

# Analysis 3:
y = c("S_French-1.DG",
      "S_Han-1.DG",
      "S_Papuan-1.DG")

a3 = d(
       W = "S_Yoruba-2.DG",
       X = "S_Yoruba-1.DG",
       Y = y,
       Z = "Chimp.REF",
       data= snps_subset)

# Analysis 4: Difference to other Yoruban in Neanderthal ancestry
a4 = d(
   W = "S_Yoruba-2.DG",
   X = "S_Yoruba-1.DG",
   Y = "Vindija.DG",
   Z = "Chimp.REF",
   data = snps_subset
)

# 5: Mbuti baseline
x = c("S_Yoruba-1.DG",
      "S_Yoruba-2.DG")

a5 = d(
       W = "S_Mbuti-1.DG",
       X = x,
       Y = "Vindija.DG",
       Z = "Chimp.REF",
       data = snps_subset)

# 6: Closer to Vindija?

y = c("S_Yoruba-1.DG",
      "S_Yoruba-2.DG",
      "S_Mbuti-1.DG",
      "S_Khomani_San-1.DG")

a6 = d(
       W = "Vindija.DG",
       X = "Altai_published.DG",
       Y = y,
       Z = "Chimp.REF",
       data = snps_subset)

df <- rbind(a1a, a1b, a2, a3, a4, a5, a6)


write.table(df, file = "~/repos/ancient_migration/analyses/general/output/selected_d_stats_ndth.txt", sep = " ", append = F, quote = FALSE, row.names = FALSE, col.names = T)
