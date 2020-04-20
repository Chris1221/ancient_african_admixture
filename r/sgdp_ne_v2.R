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

i = 1
plots = list()
for(language in list_language_families()){
  source = names_from_config(config$source) # from tools
  source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
  sink = names_from_config(config$sink) # from tools
  #sink = sink[0:10]
  sink = sink[sink %in% list_samples_from_lang(language)]
  sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
  seeds = c("1791095846", "946286477", "2135392492")
  smc2_path = "~/repos/dirmig/data/sgdp/"
  msmc_path = "~/repos/dirmig/data/msmc2/sgpd/"
  seeds = c("1791095846", "946286477", "2135392492")
  
  smcsmc_files <- NULL
  msmc_files <- NULL
  
  for (so in source){
    for (si in sink){
      
      for (seed in seeds){
        if(is.null(smcsmc_files)){
          smcsmc_files = plot(smcsmc(paste0(smc2_path, seed, ".", so, ".", si, ".out")), type = "ne", return_df = T) %>%
            mutate(so = ids_to_names(so), si = ids_to_names(si), seed = seed)
          msmc_files = msmc(paste0(msmc_path, seed, ".", so, ".", si, ".combined.final.txt"))@data %>%
            mutate(so = ids_to_names(so), si = ids_to_names(si), seed = seed)
        } else {
          smcsmc_files = rbind(smcsmc_files, plot(smcsmc(paste0(smc2_path, seed, ".", so, ".", si, ".out")), type = "ne", return_df = T) %>%
                                 mutate(so = ids_to_names(so), si = ids_to_names(si), seed = seed))
          msmc_files = rbind(msmc_files, msmc(paste0(msmc_path, seed, ".", so, ".", si, ".combined.final.txt"))@data %>%
                               mutate(so = ids_to_names(so), si = ids_to_names(si), seed = seed))
        }}}
  }
  
  smcsmc_files %>% group_by(Start, From, si, so) %>% summarise(mean = mean(Ne), sd = sd(Ne)) ->
    new_smcsmc_files
  
  msmc_g <- msmc_files %>%
    mutate(left_time_boundary = (left_time_boundary / mu) * g) %>%
    mutate(right_time_boundary = (right_time_boundary / mu) * g) %>%
    mutate(lambda_00 = (1 / lambda_00) / (2*mu)) %>% 
    mutate(lambda_11 = (1 / lambda_11) / (2*mu)) %>% 
    reshape2::melt(id.vars =c("time_index", "left_time_boundary", "right_time_boundary", "so", "si", "seed"), factorsAsStrings = F) %>%
    filter(variable != "lambda_01") %>% 
    group_by(time_index, variable, si, so) %>%
    summarise(time = mean(left_time_boundary), mean = mean(value), sd = sd(value)) 
  
  p <- ggplot() +
    geom_step(
      data = new_smcsmc_files,
      aes(x = Start*29, 
          y = mean, 
          group = From, 
          col = From,
          linetype = From)) + 
    geom_ribbon(data = new_smcsmc_files,
                stat = "stepribbon",
                aes(x = Start*29, 
                    ymin = mean -sd,
                    ymax = mean + sd,
                    fill = From),
                alpha = 0.3) + 
    geom_step(
      data = msmc_g,
      aes(x = time,
          y = mean,
          group = variable,
          col = variable,
          linetype = variable)) + 
    geom_ribbon(data = msmc_g,
                stat = "stepribbon",
                aes(x = time,
                    ymin = mean - sd,
                    ymax = mean + sd,
                    fill= variable),
                alpha = 0.3) +
    scale_x_log10(limits=c(1e4, 1e6), labels = label_comma(scale = 0.001)) + 
    scale_y_log10(limits = c(1e3, 1e5), labels = label_comma()) +
    xlab("Thousands of Years before Present") + 
    ylab("Effective Population Size") + 
    scale_fill_manual(values = c("blue", "red", "blue", "red"), labels = c("SMCSMC Eurasian", "SMCSMC African", "MSMC Eurasian", "MSMC African")) +
    scale_color_manual(values = c('blue', 'red', 'blue', 'red'), labels = c("SMCSMC Eurasian", "SMCSMC African", "MSMC Eurasian", "MSMC African")) + 
    scale_linetype_manual(values = c(1,1,2, 2), labels = c("SMCSMC Eurasian", "SMCSMC African", "MSMC Eurasian", "MSMC African")) + 
    theme_bw() + 
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          aspect.ratio = 9/16) +
    facet_grid(si ~ so) + 
    annotation_logticks()
  
  
  plots[[i]] <- p
  i = i+1
}

left <- ggarrange(plots[[1]], plots[[3]], plots[[4]], ncol = 1, legend = "none", labels = "auto")
both <- ggarrange(left, plots[[2]], nrow = 1, common.legend = T, labels = c("", "d"), legend = "bottom")
ggsave(both, file = "~/repos/dirmig/plot/sgdp_ne_new.pdf", height = 10, width = 10)
