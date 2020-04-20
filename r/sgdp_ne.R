# Supplemental figure

library(smcsmcTools)
library(rjson)
library(GGally)
library(dplyr)
library(cowplot)
library(grid)
library(gridExtra)
library(ggplot2)


config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/whole_sgdp.json")
j = 1
matrices = list()


orientation = c("long", "wide", "long", "long")
orientation = c("long", "long", "long", "long")

for(language in list_language_families()){
  source = names_from_config(config$source) # from tools
  source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
  sink = names_from_config(config$sink) # from tools
  #sink = sink[0:10]
  sink = sink[sink %in% list_samples_from_lang(language)]
  sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
  seed = "1791095846"
  smc2_path = "~/repos/dirmig/data/sgdp/"
  msmc_path = "~/repos/dirmig/data/sgdp/"
  
  plots = list()
  
  i = 1
  
  # This is weird, but I want some of them to be long and some to be wide
  # No legend for the long ones for now...
  if( orientation[j] == "long") {
    for (so in source){
      for (si in sink){
        smcsmc_file = smcsmc(paste0(smc2_path, seed, ".", so, ".", si, ".out"))
        msmc_file = msmc(paste0(msmc_path, so, ".", si, ".final.txt"))
        plots[[i]] = plot_both_msmc_and_smcsmc(smcsmc = smcsmc_file, msmc = msmc_file, type = "ne", ylim = c(2.5e3, 0.5e5), xlim = c(1e4, 1e6))
        #leg = grab_legend(plots[[i]])
        i = i + 1 }}
    matrices[[j]] <- ggmatrix(plots, nrow = length(sink), ncol = length(source), byrow = FALSE, yAxisLabels = sink_strings, xAxisLabels = source_strings)
    j = j+ 1
  } else if (orientation[j] == "wide"){
    for (si in sink){
      for (so in source){
        smcsmc_file = smcsmc(paste0(smc2_path, seed, ".", so, ".", si, ".out"))
        msmc_file = msmc(paste0(msmc_path, so, ".", si, ".final.txt"))
        plots[[i]] = plot_both_msmc_and_smcsmc(smcsmc = smcsmc_file, msmc = msmc_file, type = "ne", ylim = c(2.5e3, 0.5e5), xlim = c(1e4, 1e6))
        leg = grab_legend(plots[[i]])
        i = i + 1 }}
    matrices[[j]] <- ggmatrix(plots, nrow = length(source), ncol = length(sink), byrow = FALSE,xlab = "Years Before Present", ylab = "Estimated Effective Population Size", yAxisLabels = source_strings, xAxisLabels = sink_strings, legend = leg) + theme(legend.position = "bottom")
    j = j+ 1
  }
}

# Add in an extra blank element for alignment

layout = rbind( c(3,3,3,1,1,1,4,4,4),
                c(3,3,3,1,1,1,4,4,4),
                c(5,5,5,1,1,1,5,5,5),
                c(2,2,2,2,2,2,2,2,2),
                c(2,2,2,2,2,2,2,2,2),
                c(2,2,2,2,2,2,2,2,2))
new_layout = rbind(c(3, 2),
                   c(3, 2),
                   c(5, 2),
                   c(1, 2),
                   c(1, 2), 
                   c(1, 2), 
                   c(5, 2), 
                   c(4, 2),
                   c(4, 2))

# Just directly save it
# Alternatively could do a newpage then draw it.
ggsave( arrange_ggmatrix(matrices, new_layout), file = "~/repos/dirmig/plot/sgdp_ne.pdf", dpi = 300, width = 13.6, height = 12.1, unit = "in")

#grid.newpage()
#grid.draw(arranged_matrix)


#ggsave(paste0("~/repos/eurasian-backmigration/v2/plot/sgdp_ne_", language, ".eps"), plot = p, dpi = 300, height = 10, width = 8, unit = "in")