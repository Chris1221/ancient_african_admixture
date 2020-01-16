# Supplemental figure

library(smcsmcTools)
library(rjson)
library(GGally)
library(dplyr)


config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/sgdp_replication.json")
source = names_from_config(config$source) # from tools
source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
sink = names_from_config(config$sink) # from tools
sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
seed = "1791095846"
smc2_path = "~/repos/eurasian-backmigration/v2/data/sgdp_subset/"
msmc_path = "~/repos/eurasian-backmigration/v2/data/sgdp_subset/"

plots = list()

i = 1
for (si in sink){
  for (so in source){
    smcsmc_file = new("smcsmc", file = paste0(smc2_path, seed, ".", so, ".", si, ".out"))
    msmc_file = new("msmc", file = paste0(msmc_path, so, ".", si, ".final.txt"))
    
    plots[[i]] = plot_both_msmc_and_smcsmc(smcsmc = smcsmc_file, msmc = msmc_file)
    
    if(i == 1){
      leg = grab_legend(plots[[i]])
    }
    i = i + 1
  }
}

ggmatrix(plots, nrow = length(source), ncol = length(sink), byrow = FALSE,xlab = "Years Before Present", ylab = "Estimated Effective Population Size", yAxisLabels = source_strings, xAxisLabels = sink_strings, legend = leg) + theme(legend.position = "bottom")
ggsave("~/repos/eurasian-backmigration/v2/plot/sgdp_subet_ne.eps", dpi = 300, height = 10, width = 8, unit = "in")
