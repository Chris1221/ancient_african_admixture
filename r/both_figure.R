# Makes a boxplot of average Ne estimated by MSMC and SMC2 at particular times
library(smcsmcTools)
library(ggplot2)
library(dplyr)
library(smcsmcTools)
library(scales)
#library(dict)

sgdp_hgdp_mig_plot = function(){
  times <- c(300e3, 200e3, 250e3, 150e3, 120e3, 100e3, 75e3)
  path <- "~/repos/dirmig/data/sgdp_subset/"
  hgdp_path <- "~/repos/dirmig/data/hgdp_low_mig/"
  seed <- "1791095846"
  
  samples = c("Biaka", "Mbuti", "Yoruba", "San")
  hsamples = c("HGDP00930", "HGDP01029", "HGDP00450", "HGDP00460")
  
  conv <- data.frame(ID = hsamples, NAME = ids_to_names(hsamples, "hgdp"))
  
  plots <- list()
  i = 1
  
  smc2 <- NA
  msmc_df <- NA
  for(samp in samples){
    msmc_files <- list.files(path, pattern = "final.txt", full.names = T) %>% grep(pattern= samp, value = T)
    smc2_files <- list.files(path, pattern = "out", full.names = T) %>% grep(pattern = seed, value = T) %>%grep(pattern = samp, value = T)
    
    for(s in lapply(smc2_files, smcsmc)){
      if(is.na(smc2)){
        smc2 <- s@data %>% mutate(samp = samp, source = "SGDP") %>% filter(Iter == 25)
      } else {
        smc2 <- rbind(smc2, s@data %>% mutate(samp = samp, source = "SGDP") %>% filter(Iter == 25))
      }
    }
    for(s in lapply(msmc_files, msmc)){
      if(is.na(msmc_df)){
        msmc_df <- s@data %>% mutate(samp = samp, source = "SGDP")
      } else {
        msmc_df <- rbind(msmc_df, s@data %>% mutate(samp = samp, source = "SGDP"))
      }
    }}
  
  for(samp in hsamples){
    msmc_files <- list.files(hgdp_path, pattern = "final.txt", full.names = T) %>% grep(pattern= samp, value = T)
    smc2_files <- list.files(hgdp_path, pattern = "out", full.names = T) %>% grep(pattern = seed, value = T) %>%grep(pattern = samp, value = T)
    
    for(s in lapply(smc2_files, smcsmc)){
      if(is.na(smc2)){
        smc2 <- s@data %>% mutate(samp = conv$NAME[conv$ID == samp], source = "HGDP") %>% filter(Iter == 10)
      } else {
        smc2 <- rbind(smc2, s@data %>% mutate(samp = conv$NAME[conv$ID == samp], source = "HGDP") %>% filter(Iter == 10))
      }
    }
    for(s in lapply(msmc_files, msmc)){
      if(is.na(msmc_df)){
        msmc_df <- s@data %>% mutate(samp = conv$NAME[conv$ID == samp], source = "HGDP") 
      } else {
        msmc_df <- rbind(msmc_df, s@data %>% mutate(samp = conv$NAME[conv$ID == samp], source = "HGDP") )
      }
    }}
  
  mu = 1.25e-8
  g = 29
  ylim = c(0, 4.5e-4)
  smc2_g <- smc2 %>% filter(Type == "Migr") %>% group_by(From, Start, samp, source) %>% summarise(mean = mean(Rate), sd = sd(Rate))
  msmc_g <- msmc_df %>%
    mutate(left_time_boundary = (left_time_boundary / mu) * g) %>%
    mutate(right_time_boundary = (right_time_boundary / mu) * g) %>%
    mutate(lambda_01 = 2 * lambda_01 / (lambda_00 + lambda_11)) %>%
    mutate(lambda_01 = ((lambda_01 - min(lambda_01))/(max(lambda_01)-min(lambda_01))) * (ylim[2] - ylim[1]) + ylim[1]) %>%
    reshape2::melt(id.vars =c("time_index", "left_time_boundary", "right_time_boundary", "samp", "source"), factorsAsStrings = F) %>%
    filter(variable == "lambda_01") %>% 
    group_by(time_index, variable, samp, source) %>%
    summarise(time = mean(left_time_boundary), mean = mean(value), sd = sd(value)) 
  
  plot <- ggplot(smc2_g, aes(x = Start*g, y = mean, ymin = mean-sd, max = mean+sd, fill = factor(From))) + 
    geom_step(data = smc2_g, aes(col = factor(From), linetype = factor(From))) + 
    geom_ribbon(stat = "stepribbon", alpha = 0.3) + 
    geom_step(data = msmc_g, inherit.aes = F, aes(x = time, y = mean, col = factor(variable), linetype = factor(variable)) ) + 
    geom_ribbon(stat = "stepribbon", alpha = 0.3, data = msmc_g, inherit.aes = F, aes(x = time, ymin = mean-sd, ymax = mean+sd, fill = factor(variable))) +
    scale_x_log10(limits = c(1e4, 1e6), labels = label_comma(scale = 0.001)) + 
    scale_y_continuous(limits = ylim, labels = label_comma()) + 
    scale_color_manual(values = c("blue", "red", "purple", "red"), labels = c("SMC2 Eurasian", "SMC2 African", "MSMC X-Coal", "MSMC African")) +
    scale_fill_manual(values = c("blue", "red", "purple", "red"), labels = c("SMC2 Eurasian", "SMC2 African", "MSMC X-Coal", "MSMC African")) +
    scale_linetype_manual(values = c(1, 2, 1, 2), labels = c("SMC2 Eurasian", "SMC2 African", "MSMC X-Coal", "MSMC African")) +
    ylab("Migration") + 
    xlab("Thousands of Years before Present") + 
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()) +
    facet_grid(source~samp) + 
    annotation_logticks()
  
  return(plot)
  
  #ggsave(plot, file = "~/repos/dirmig/plot/both_average_mig.pdf", height = 4, width = 8)
}

sgdp_hgdp_ne_plot = function(){
  times <- c(300e3, 200e3, 250e3, 150e3, 120e3, 100e3, 75e3)
  path <- "~/repos/dirmig/data/sgdp_subset/"
  hgdp_path <- "~/repos/dirmig/data/hgdp_low_mig/"
  seed <- "1791095846"
  
  samples = c("Biaka", "Mbuti", "Yoruba", "San")
  hsamples = c("HGDP00930", "HGDP01029", "HGDP00450", "HGDP00460")
  
  conv <- data.frame(ID = hsamples, NAME = ids_to_names(hsamples, "hgdp"))
  
  plots <- list()
  i = 1
  
  smc2 <- NA
  msmc_df <- NA
  for(samp in samples){
    msmc_files <- list.files(path, pattern = "final.txt", full.names = T) %>% grep(pattern= samp, value = T)
    smc2_files <- list.files(path, pattern = "out", full.names = T) %>% grep(pattern = seed, value = T) %>%grep(pattern = samp, value = T)
    
    for(s in lapply(smc2_files, smcsmc)){
      if(is.na(smc2)){
        smc2 <- s@data %>% mutate(samp = samp, source = "SGDP") %>% filter(Iter == 25)
      } else {
        smc2 <- rbind(smc2, s@data %>% mutate(samp = samp, source = "SGDP") %>% filter(Iter == 25))
      }
    }
    for(s in lapply(msmc_files, msmc)){
      if(is.na(msmc_df)){
        msmc_df <- s@data %>% mutate(samp = samp, source = "SGDP")
      } else {
        msmc_df <- rbind(msmc_df, s@data %>% mutate(samp = samp, source = "SGDP"))
      }
    }}
  
  for(samp in hsamples){
    msmc_files <- list.files(hgdp_path, pattern = "final.txt", full.names = T) %>% grep(pattern= samp, value = T)
    smc2_files <- list.files(hgdp_path, pattern = "out", full.names = T) %>% grep(pattern = seed, value = T) %>%grep(pattern = samp, value = T)
    
    for(s in lapply(smc2_files, smcsmc)){
      if(is.na(smc2)){
        smc2 <- s@data %>% mutate(samp = conv$NAME[conv$ID == samp], source = "HGDP") %>% filter(Iter == 10)
      } else {
        smc2 <- rbind(smc2, s@data %>% mutate(samp = conv$NAME[conv$ID == samp], source = "HGDP") %>% filter(Iter == 10))
      }
    }
    for(s in lapply(msmc_files, msmc)){
      if(is.na(msmc_df)){
        msmc_df <- s@data %>% mutate(samp = conv$NAME[conv$ID == samp], source = "HGDP") 
      } else {
        msmc_df <- rbind(msmc_df, s@data %>% mutate(samp = conv$NAME[conv$ID == samp], source = "HGDP") )
      }
    }}
  
  mu = 1.25e-8
  g = 29
  smc2_g <- smc2 %>% filter(Type == "Coal") %>% group_by(From, Start, samp, source) %>% summarise(mean = mean(Ne), sd = sd(Ne))
  msmc_g <- msmc_df %>%
    mutate(left_time_boundary = (left_time_boundary / mu) * g) %>%
    mutate(right_time_boundary = (right_time_boundary / mu) * g) %>%
    mutate(lambda_00 = (1 / lambda_00) / (2*mu)) %>% 
    mutate(lambda_11 = (1 / lambda_11) / (2*mu)) %>% 
    reshape2::melt(id.vars =c("time_index", "left_time_boundary", "right_time_boundary", "samp", "source"), factorsAsStrings = F) %>%
    filter(variable != "lambda_01") %>% 
    group_by(time_index, variable, samp, source) %>%
    summarise(time = mean(left_time_boundary), mean = mean(value), sd = sd(value)) 
  
  plot_ne <- ggplot(smc2_g, aes(x = Start*g, y = mean, ymin = mean-sd, max = mean+sd, fill = factor(From))) + 
    geom_step(data = smc2_g, aes(col = factor(From), linetype = factor(From))) + 
    geom_ribbon(stat = "stepribbon", alpha = 0.3) + 
    geom_step(data = msmc_g, inherit.aes = F, aes(x = time, y = mean, col = factor(variable), linetype = factor(variable)) ) + 
    geom_ribbon(stat = "stepribbon", alpha = 0.3, data = msmc_g, inherit.aes = F, aes(x = time, ymin = mean-sd, ymax = mean+sd, fill = factor(variable))) +
    scale_x_log10(limits = c(1e4, 1e6), labels = label_comma(scale = 0.001)) + 
    scale_y_log10(limits = c(1e3, 1e5), labels = label_comma()) + 
    scale_color_manual(values = c("blue", "blue", "red", "red"), labels = c("SMC2 Eurasian", "SMC2 African", "MSMC Eurasian", "MSMC African")) +
    scale_fill_manual(values = c("blue", "blue", "red", "red"), labels = c("SMC2 Eurasian", "SMC2 African", "MSMC Eurasian", "MSMC African")) +
    scale_linetype_manual(values = c(1, 2, 1, 2), labels = c("SMC2 Eurasian", "SMC2 African", "MSMC Eurasian", "MSMC African")) +
    ylab("Effective Population Size") + 
    xlab("Thousands of Years before Present") + 
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()) +
    facet_grid(source~samp) +
    annotation_logticks()
  
  return(plot)
  
  #ggsave(plot, file = "~/repos/dirmig/plot/both_average_ne.pdf", height = 4, width = 8)
}

sgdp_hgdp_imf_heatmap = function(){
  # SGDP
  config = fromJSON(file = "~/repos/dirmig/analyses/sgdp_replication.json")
  source = names_from_config(config$source) # from tools
  sink = names_from_config(config$sink) # from tools
  seed = "1791095846"
  smc2_path = "~/repos/dirmig/data/sgdp_subset/"
  
  SGDP_African <- matrix(, nrow = length(source), ncol = length(sink))
  SGDP_Eurasian <- matrix(, nrow = length(source), ncol = length(sink))
  
  i = 1
  for(so in source){ 
    j = 1
    for(si in sink){
      SGDP_African[i, j] = avg_migr(paste0(smc2_path, seed, ".", so, ".", si, ".out"), ancient = 70000, modern = 30000, g = 29)$integrated[1]
      SGDP_Eurasian[i, j] =  avg_migr(paste0(smc2_path, seed, ".", so, ".", si, ".out"), ancient = 70000, modern = 30000, g = 29)$integrated[2]
      j = j + 1}
    i = i + 1
  }
  
  colnames(SGDP_African) <- ids_to_names(sink)
  rownames(SGDP_African) <- ids_to_names(source)
  colnames(SGDP_Eurasian) <- ids_to_names(sink)
  rownames(SGDP_Eurasian) <- ids_to_names(source)
  sgdp <- reshape2::melt(SGDP_African) %>% mutate(ds = "SGDP") %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2), pop = "Forwards")
  sgdp$Var2[sgdp$Var2 == "Khomani San"] = "San"
  
  sgdp_e <- reshape2::melt(SGDP_Eurasian) %>% mutate(ds = "SGDP") %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2), pop = "Backwards")
  sgdp_e$Var2[sgdp_e$Var2 == "Khomani San"] = "San"
  
  
  # HGDP
  
  config = fromJSON(file = "~/repos/dirmig/analyses/hgdp_physically_phased.json")
  source = names_from_config(config$source) # from tools
  sink = names_from_config(config$sink) # from tools
  seed = "1791095846"
  smc2_path = "~/repos/dirmig/data/hgdp_low_mig/"
  
  HGDP_African <- matrix(, nrow = length(source), ncol = length(sink))
  HGDP_Eurasian <- matrix(, nrow = length(source), ncol = length(sink))
  
  i = 1
  for(so in source){ 
    j = 1
    for(si in sink){
      HGDP_African[i, j] = avg_migr(paste0(smc2_path, seed, ".", so, ".", si, ".out"), ancient = 70000, modern = 30000, g = 29)$integrated[1]
      HGDP_Eurasian[i, j] =  avg_migr(paste0(smc2_path, seed, ".", so, ".", si, ".out"), ancient = 70000, modern = 30000, g = 29)$integrated[2]
      j = j + 1}
    i = i + 1
  }
  
  colnames(HGDP_African) <- ids_to_names(sink, "hgdp")
  rownames(HGDP_African) <- ids_to_names(source, "hgdp")
  colnames(HGDP_Eurasian) <- ids_to_names(sink, "hgdp")
  rownames(HGDP_Eurasian) <- ids_to_names(source, "hgdp")
  hgdp <- reshape2::melt(HGDP_African) %>% mutate(ds = "HGDP") %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2), pop = "Forwards") %>%
    filter(Var1 != "PapuanSepik")
  hgdp_e <- reshape2::melt(HGDP_Eurasian) %>% mutate(ds = "HGDP") %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2), pop = "Backwards") %>%
    filter(Var1 != "PapuanSepik")
  
  hgdp$Var1[hgdp$Var1 == "PapuanHighlands"] = "Papuan"
  hgdp_e$Var1[hgdp_e$Var1 == "PapuanHighlands"] = "Papuan"
  ## Combined
  
  
  both <- rbind(sgdp, sgdp_e, hgdp, hgdp_e)
  
  
  comparison <- ggplot(both, aes(x = fct_reorder(Var1, value), y = fct_reorder(Var2, value), fill = value, label = value)) + 
    geom_tile() + 
    geom_text(aes(label = round(value, 3)), col = "white", size = 3) +
    ylab("") +
    xlab("") +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.position = "none",
          legend.title = element_blank()) +
    facet_grid(pop ~ ds)
  
  
  #ggsave(comparison, file = "~/repos/dirmig/plot/hgdp_sgdp_heatmap.pdf", height = 4, width = 8)
  
  
  ## ALL
  
  all <- ggarrange(plot_ne, comparison, plot, nrow = 3, labels = "auto")
  return(all)
  #ggsave(all, file = "~/repos/dirmig/plot/both_figure.pdf", height = 10, width = 8)
}

plot_dendrograms = function(){
  # SGDP
  config = fromJSON(file = "~/repos/dirmig/analyses/sgdp_replication.json")
  source = names_from_config(config$source) # from tools
  sink = names_from_config(config$sink) # from tools
  seed = "1791095846"
  smc2_path = "~/repos/dirmig/data/sgdp_subset/"
  
  SGDP_African <- matrix(, nrow = length(source), ncol = length(sink))
  SGDP_Eurasian <- matrix(, nrow = length(source), ncol = length(sink))
  
  i = 1
  for(so in source){ 
    j = 1
    for(si in sink){
      SGDP_African[i, j] = avg_migr(paste0(smc2_path, seed, ".", so, ".", si, ".out"), ancient = 70000, modern = 30000, g = 29)$integrated[1]
      SGDP_Eurasian[i, j] =  avg_migr(paste0(smc2_path, seed, ".", so, ".", si, ".out"), ancient = 70000, modern = 30000, g = 29)$integrated[2]
      j = j + 1}
    i = i + 1
  }
  
  colnames(SGDP_African) <- ids_to_names(sink)
  rownames(SGDP_African) <- ids_to_names(source)
  colnames(SGDP_Eurasian) <- ids_to_names(sink)
  rownames(SGDP_Eurasian) <- ids_to_names(source)
  sgdp <- reshape2::melt(SGDP_African) %>% mutate(ds = "SGDP") %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2), pop = "Forwards")
  sgdp$Var2[sgdp$Var2 == "Khomani San"] = "San"
  
  sgdp_e <- reshape2::melt(SGDP_Eurasian) %>% mutate(ds = "SGDP") %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2), pop = "Backwards")
  sgdp_e$Var2[sgdp_e$Var2 == "Khomani San"] = "San"
  
  
  # HGDP
  
  config = fromJSON(file = "~/repos/dirmig/analyses/hgdp_physically_phased.json")
  source = names_from_config(config$source) # from tools
  sink = names_from_config(config$sink) # from tools
  seed = "1791095846"
  smc2_path = "~/repos/dirmig/data/hgdp_low_mig/"
  
  HGDP_African <- matrix(, nrow = length(source), ncol = length(sink))
  HGDP_Eurasian <- matrix(, nrow = length(source), ncol = length(sink))
  
  i = 1
  for(so in source){ 
    j = 1
    for(si in sink){
      HGDP_African[i, j] = avg_migr(paste0(smc2_path, seed, ".", so, ".", si, ".out"), ancient = 70000, modern = 30000, g = 29)$integrated[1]
      HGDP_Eurasian[i, j] =  avg_migr(paste0(smc2_path, seed, ".", so, ".", si, ".out"), ancient = 70000, modern = 30000, g = 29)$integrated[2]
      j = j + 1}
    i = i + 1
  }
  
  colnames(HGDP_African) <- ids_to_names(sink, "hgdp")
  rownames(HGDP_African) <- ids_to_names(source, "hgdp")
  colnames(HGDP_Eurasian) <- ids_to_names(sink, "hgdp")
  rownames(HGDP_Eurasian) <- ids_to_names(source, "hgdp")
  
  cbind(SGDP_Eurasian, SGDP_African) %>% dist %>% hclust %>% ggdendrogram(rotate = T)  -> sp
  cbind(HGDP_Eurasian, HGDP_African) %>% dist %>% hclust %>% ggdendrogram(rotate = T)  -> hp
  
  both = sp + hp + plot_annotation(tag_levels = "a")
  return(both)
}

