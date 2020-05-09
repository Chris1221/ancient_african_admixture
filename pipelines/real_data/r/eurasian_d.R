.libPaths( c( .libPaths(), "~/R/lib64/R/library/") )

library(dplyr)
library(admixr)
library(stringr)
library(data.table)

pop_string = snakemake@wildcards[["sj"]]
n = as.numeric(snakemake@wildcards[["n"]])
modif_snps <- eigenstrat(tools::file_path_sans_ext(snakemake@input[["ind"]]))
modif_segs <- modif_snps %>% 
	filter_bed(snakemake@input[["bed"]])

if ( grepl( x = pop_string, pattern = "-1" ) ) {
	co = gsub(pop_string, pattern="-1", replacement="-2")
} else if ( grepl( x = pop_string, pattern = "-2" ) ) {
	co = gsub(pop_string, pattern="-2", replacement="-1")
} else { 
	stop("Cant find a good partner")
}

pop_string <- paste0(pop_string, ".DG")
co <- paste0(co, ".DG")

df <- read.table(snakemake@input[["ind"]], head = F, stringsAsFactors = F)
groups <- unique(df$V3)
groups <- groups[!grepl("Ignore", groups)]
groups <- groups[groups != pop_string]
groups <- groups[groups != co]

#save.image("image.rdb")
whole = d(
   W =  groups[n],
   X = "Chimp.REF",
   Y = pop_string,
   Z = co,
   data = modif_snps
)
seg = d(
   W = groups[n],
   X = "Chimp.REF",
   Y = pop_string,
   Z = co,
   data = modif_segs
)

name = snakemake@wildcards[["name"]]
seeds = snakemake@wildcards[["seeds"]]
path_all <- paste0("output/", name, "/segments/", seeds, "/d_all.txt")
path_segs <- paste0("output/", name, "/segments/", seeds, "/d_segs.txt")

write.table(whole, file = path_all, sep = " ", append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(seg, file = path_segs, sep = " ", append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)

#save.image("R.rdb")
