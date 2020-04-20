# Supplemental figure

library(smcsmcTools)
library(rjson)
library(GGally)
library(dplyr)


config = fromJSON(file = "~/repos/dirmig/analyses/hgdp_physically_phased.json")
source = names_from_config(config$source) # from tools
source_strings = vapply(source, id_to_name, character(1), source = 'hgdp') %>% as.vector
sink = names_from_config(config$sink) # from tools
sink_strings = vapply(sink, ids_to_names, character(1), source = "hgdp") %>% as.vector
seed = "1791095846"
smc2_path = "~/repos/dirmig/data/hgdp_low_mig/"
msmc_path = "~/repos/dirmig/data/hgdp_low_mig/"

plots = list()

i = 1
for (si in sink){
  for (so in source){
    smcsmc_file = smcsmc(paste0(smc2_path, seed, ".", so, ".", si, ".out"))
    msmc_file = msmc(paste0(msmc_path, so, ".", si, ".final.txt"))
    
    plots[[i]] = plot_both_msmc_and_smcsmc(smcsmc = smcsmc_file, msmc = msmc_file, type = "migration", ylim = c(0, 3e-4))
    
    if(i == 1){
      leg = grab_legend(plots[[i]])
    }
    i = i + 1
  }
}

hgdp <- ggmatrix(plots, nrow = length(source), ncol = length(sink), byrow = F,xlab = "Thousands of Years Before Present", ylab = "Estimated Effective Population Size", yAxisLabels = source_strings, xAxisLabels = sink_strings) + theme(legend.position = "bottom")
ggsave(hgdp, file= "~/repos/dirmig/plot/hgdp_mig.pdf", height = 10, width = 8)
