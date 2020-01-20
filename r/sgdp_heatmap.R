# Migration heatmap.

library(dplyr)
library(smcsmcTools)
library(rjson)
library(heatmaply)

config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/whole_sgdp.json")
source = names_from_config(config$source) # from tools
source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
sink = names_from_config(config$sink) # from tools
sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
seed = "1791095846"
smc2_path = "~/repos/dirmig/data/sgdp/"
msmc_path = "~/repos/dirmig/data/sgdp/"

n_source = length(source)
n_sink = length(sink)

mat = matrix(, nrow = n_source, ncol = n_sink)

for (so in c(1:n_source)){
  for (si in c(1:n_sink)){
    smcsmc_file = new("smcsmc", file = paste0(smc2_path, seed, ".", source[so], ".", sink[si], ".out"))
    mat[so, si] = avg_migr(smcsmc_file@file, ancient = 100000, modern = 0, g = 29)$integrated[2]
  }}

colnames(mat) <- sink_strings
rownames(mat) <- source_strings

heatmaply(mat, dendrogram = "column", limits = c(0, 1))


# Because of bug with heatmaply, have to save it through Rstudio then edit in illustrator.