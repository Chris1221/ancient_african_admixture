library(admixr)
library(dplyr)

cs = c("S_Yoruba-1.DG")

b = c(	"Vindija.DG",
	"S_French-1.DG",
      	"S_Han-1.DG",
	"S_Papuan-1.DG")

a = c("S_Yoruba-2.DG")



snps <- eigenstrat("~/well/reich-eigenstrat/v37.2.1240K_HumanOrigins") 

fre <- filter_bed(snps, "~/repos/ancient_migration/analyses/general/output/sgpd/segments/1791095846/S_French-1.S_Yoruba-1.bed")
han <-  filter_bed(snps, "~/repos/ancient_migration/analyses/general/output/sgpd/segments/1791095846/S_Han-1.S_Yoruba-1.bed")
pap <-  filter_bed(snps, "~/repos/ancient_migration/analyses/general/output/sgpd/segments/1791095846/S_Papuan-1.S_Yoruba-1.bed")


stats <- f3(A = a,
	   B = b,
	   C = cs,
	   data = snps) %>% mutate(Source = "All")


fre_stats <- f3(A = a,
	   B = b,
	   C = cs,
	   data = fre) %>% mutate(Source = "French")


han_stats <- f3(A = a,
	   B = b,
	   C = cs, 
	   data = han) %>% mutate(Source = "Han")


pap_stats <- f3(A = a,
	   B = b,
	   C = cs,
	   data = pap) %>% mutate(Source = "Papuan")

df <- rbind(stats, fre_stats, han_stats, pap_stats)


write.table(df, 
	    file = "~/repos/ancient_migration/analyses/general/output/a_couple_more_f3.txt", sep = " ", append = F, quote = FALSE, row.names = FALSE, col.names = T)


