# Supplemental figure

library(smcsmcTools)
library(rjson)
library(GGally)
library(dplyr)


config = fromJSON(file = "~/repos/dirmig/analyses/sgdp_replication.json")
source = names_from_config(config$source) # from tools
source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
sink = names_from_config(config$sink) # from tools
sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
seed = "1791095846"
smc2_path = "~/repos/dirmig/data/sgdp_subset/"
msmc_path = "~/repos/dirmig/data/sgdp_subset/"

plots = list()

i = 1
for (si in sink){
  for (so in source){
    smcsmc_file = smcsmc(paste0(smc2_path, seed, ".", so, ".", si, ".out"))
    msmc_file = msmc(paste0(msmc_path, so, ".", si, ".final.txt"))
    
    plots[[i]] = plot_both_msmc_and_smcsmc(smcsmc = smcsmc_file, msmc = msmc_file, ylim = c(1e3, 1e5))
    
    if(i == 1){
      leg = grab_legend(plots[[i]])
    }
    i = i + 1
  }
}

ggmatrix(plots, nrow = length(source), ncol = length(sink), byrow = FALSE,xlab = "Years Before Present", ylab = "Estimated Effective Population Size", yAxisLabels = source_strings, xAxisLabels = sink_strings, legend = leg) + theme(legend.position = "bottom")
#ggsave("~/repos/dirmig/plot/sgdp_subet_ne.pdf", height = 10, width = 8, unit = "in")

han <- plots[[18]]

two <- plot(smcsmc("~/repos/ancient_migration/data/yri4-vb.out"), type = "ne", ylim = c(1e3, 1e5), return_df = T)

han + geom_step(data = two, aes(x = Start * 29, y = Ne), col = "green") + scale_y_log10(limits = c(2e3, 5e4))

# Need to have some confidence interval for this probably
ggsave("~/repos/dirmig/plot/ne/figure.pdf")
