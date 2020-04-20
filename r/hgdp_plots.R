
plot_hgdp_ne = function(){
  config = fromJSON(file = "~/repos/dirmig/analyses/hgdp_physically_phased.json")
  i = 1
  plots = list()
  source = names_from_config(config$source) # from tools
  source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
  sink = names_from_config(config$sink) # from tools
  #sink = sink[0:10]
  sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
  seeds = c("1791095846", "946286477", "2135392492")
  smc2_path = "~/repos/dirmig/data/hgdp_low_mig/"
  msmc_path = "~/repos/dirmig/data/hgdp_low_mig//"
  seeds = c("1791095846", "946286477", "2135392492")
  
  smcsmc_files <- NULL
  msmc_files <- NULL
  
  for (seed in seeds){
    for (so in source){
      for (si in sink){
        
        for (seed in seeds){
          if(is.null(smcsmc_files)){
            smcsmc_files = plot(smcsmc(paste0(smc2_path, seed, ".", so, ".", si, ".out")), type = "ne", return_df = T) %>%
              mutate(so = ids_to_names(so, "hgdp"), si = ids_to_names(si, "hgdp"), seed = seed)
            msmc_files = msmc(paste0(msmc_path, so, ".", si, ".final.txt"))@data %>%
              mutate(so = ids_to_names(so, "hgdp"), si = ids_to_names(si, "hgdp"), seed = seed)
          } else {
            smcsmc_files = rbind(smcsmc_files, plot(smcsmc(paste0(smc2_path, seed, ".", so, ".", si, ".out")), type = "ne", return_df = T) %>%
                                   mutate(so = ids_to_names(so, "hgdp"), si = ids_to_names(si, "hgdp"), seed = seed))
            msmc_files = rbind(msmc_files, msmc(paste0(msmc_path, so, ".", si, ".final.txt"))@data %>%
                                 mutate(so = ids_to_names(so, "hgdp"), si = ids_to_names(si, "hgdp"), seed = seed))
          }}}
    }}
  
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
  
  return(p)
}

plot_hgdp_mig = function(){
  config = fromJSON(file = "~/repos/dirmig/analyses/hgdp_physically_phased.json")
  i = 1
  plots = list()
  source = names_from_config(config$source) # from tools
  source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
  sink = names_from_config(config$sink) # from tools
  #sink = sink[0:10]
  sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
  seeds = c("1791095846", "946286477", "2135392492")
  smc2_path = "~/repos/dirmig/data/hgdp_low_mig/"
  msmc_path = "~/repos/dirmig/data/hgdp_low_mig//"
  seeds = c("1791095846", "946286477", "2135392492")
  
  smcsmc_files <- NULL
  msmc_files <- NULL
  
  for (seed in seeds){
    for (so in source){
      for (si in sink){
        
        for (seed in seeds){
          if(is.null(smcsmc_files)){
            smcsmc_files = plot(smcsmc(paste0(smc2_path, seed, ".", so, ".", si, ".out")), type = "ne", return_df = T) %>%
              mutate(so = ids_to_names(so, "hgdp"), si = ids_to_names(si, "hgdp"), seed = seed)
            msmc_files = msmc(paste0(msmc_path, so, ".", si, ".final.txt"))@data %>%
              mutate(so = ids_to_names(so, "hgdp"), si = ids_to_names(si, "hgdp"), seed = seed)
          } else {
            smcsmc_files = rbind(smcsmc_files, plot(smcsmc(paste0(smc2_path, seed, ".", so, ".", si, ".out")), type = "ne", return_df = T) %>%
                                   mutate(so = ids_to_names(so, "hgdp"), si = ids_to_names(si, "hgdp"), seed = seed))
            msmc_files = rbind(msmc_files, msmc(paste0(msmc_path, so, ".", si, ".final.txt"))@data %>%
                                 mutate(so = ids_to_names(so, "hgdp"), si = ids_to_names(si, "hgdp"), seed = seed))
          }}}
    }}
  smcsmc_files %>% group_by(Start, From, si, so) %>% summarise(mean = mean(Rate), sd = sd(Rate)) ->
    new_smcsmc_files
  
  smcsmc_files %>% filter(Start > 10000/29, Start < 300000/29) %>% 
    group_by(so, si, seed, From) %>% 
    filter(Rate == max(Rate))  %>% 
    mutate(Start = Start * 29) %>% 
    mutate(lang = language) -> peaks[[i]]
  
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  
  msmc_g <- msmc_files %>%
    mutate(left_time_boundary = (left_time_boundary / mu) * g) %>%
    mutate(right_time_boundary = (right_time_boundary / mu) * g) %>%
    mutate(lambda_01 = 2*lambda_01 / (lambda_00 + lambda_11)) %>%
    reshape2::melt(id.vars =c("time_index", "left_time_boundary", "right_time_boundary", "so", "si", "seed"), factorsAsStrings = F) %>%
    filter(variable == "lambda_01") %>% 
    mutate(value = scales::rescale(value, ylim)) %>%
    group_by(time_index, variable, si, so) %>%
    summarise(time = mean(left_time_boundary), mean = mean(value), sd = sd(value)) %>%
    mutate(cs = cumsum(mean))
  
  p <- ggplot() + 
    geom_step(
      data = new_smcsmc_files,
      aes(x = Start*29,
          y = mean,
          group = From,
          col = From,
          linetype = From)
    ) + 
    geom_ribbon(
      stat = "stepribbon",
      data = new_smcsmc_files,
      aes(x = Start*29,
          ymin = mean -sd,
          ymax = mean+sd,
          fill = From),
      alpha = 0.3
    ) + 
    geom_step(
      data = msmc_g,
      aes(x = time,
          y = rescale(mean, ylim), 
          col = variable, 
          linetype = variable)) +
    geom_ribbon(
      stat = "stepribbon",
      data=msmc_g,
      aes(x = time,
          ymin = rescale(mean, ylim) - sd,
          ymax = rescale(mean, ylim) + sd,
          fill = variable),
      alpha = 0.3
    ) +
    scale_x_log10(limits=xlim, labels = label_comma(scale = 0.001)) + 
    scale_y_continuous(limits = ylim, labels = label_comma(), breaks = c(0, ylim[2]/2, ylim[2])) +
    xlab("Thousands of Years before Present") + 
    ylab("Effective Population Size") + 
    scale_fill_manual(values = c('blue', 'red', 'purple', 'red'), labels = c("SMCSMC Eurasian", "SMCSMC African", "MSMC Relative X-Coal", "MSMC African")) +
    scale_color_manual(values = c('blue', 'red', 'purple', 'red'), labels = c("SMCSMC Eurasian", "SMCSMC African", "MSMC Relative X-Coal", "MSMC African")) + 
    scale_linetype_manual(values = c(1,1,5), labels = c("SMCSMC Eurasian", "SMCSMC African", "MSMC Relative X-Coal", "MSMC African")) + 
    theme_bw() + 
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          aspect.ratio = 9/16,
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    facet_grid(so ~ si) +
    annotation_logticks()
  
  return(p)
  #ggsave(both, file = "~/repos/dirmig/plot/sgdp_mig_new.pdf", height = 10, width = 10)
}

hgdp_plot = function(sgdp, hgdp){
  all = sgdp / hgdp + plot_annotation(tag_levels = "a") & 
    theme(plot.tag.position = c(0, 1),
          plot.tag = element_text(face = "bold", hjust = 0, vjust = 0))
  return(all)
}


