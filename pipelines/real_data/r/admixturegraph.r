library(admixr)

s = c("S_Khomani_San-1.DG",
      "S_Yoruba-1.DG",
      "S_Mbuti-1.DG",
      "S_Biaka-1.DG")

a = c(	"Vindija.DG",
	"S_French-1.DG",
      	"S_Han-1.DG")

b <- c(s,a)


snps <- eigenstrat("~/well/reich-eigenstrat/v37.2.1240K_HumanOrigins")

stats <- d(W = b,
	   X = b,
	   Y = b,
	   Z = b,
	   data = snps)


write.table(stats, 
	    file = "~/repos/ancient_migration/analyses/general/output/graph2.txt", sep = " ", append = F, quote = FALSE, row.names = FALSE, col.names = T)


