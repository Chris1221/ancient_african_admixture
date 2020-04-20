#' @description SMC2 versus MSMC2 for YRI/Han
#' @return ggplot
yri_han_ne = function(){
  
  times <- c(300e3, 200e3, 250e3, 150e3, 120e3, 100e3, 75e3)
  path <- "~/repos/dirmig/data/sgdp"
  msmc2_path <- "~/repos/dirmig/data/msmc2/sgpd/"
  
  plots <- list()
  i = 1
  
  smc2 <- NA
  msmc_df <- NA
  msmc_files <- list.files(msmc2_path, pattern = "S_Han-1.S_Yoruba-1.combined.final.txt", full.names = T)
  smc2_files <- list.files(path, pattern = "S_Han-1.S_Yoruba-1.out", full.names = T)
      
  for(s in lapply(smc2_files, smcsmc)){
    if(is.na(smc2)){
      smc2 <- s@data %>% mutate(source = "SGDP") %>% filter(Iter == 10)
    } else {
      smc2 <- rbind(smc2, s@data %>% mutate(source = "SGDP") %>% filter(Iter == 10))
    }
  }
  for(s in lapply(msmc_files, msmc)){
    if(is.na(msmc_df)){
      msmc_df <- s@data %>% mutate(samp = samp, source = "SGDP")
    } else {
      msmc_df <- rbind(msmc_df, s@data %>% mutate(samp = samp, source = "SGDP"))
    }
  }
  
  mu = 1.25e-8
  g = 29
  smc2_g <- smc2 %>% filter(Type == "Coal") %>% group_by(From, Start, source) %>% summarise(mean = mean(Ne), sd = sd(Ne))
  msmc_g <- msmc_df %>%
    mutate(left_time_boundary = (left_time_boundary / mu) * g) %>%
    mutate(right_time_boundary = (right_time_boundary / mu) * g) %>%
    mutate(lambda_00 = (1 / lambda_00) / (2*mu)) %>% 
    mutate(lambda_11 = (1 / lambda_11) / (2*mu)) %>% 
    reshape2::melt(id.vars =c("time_index", "left_time_boundary", "right_time_boundary", "samp", "source"), factorsAsStrings = F) %>%
    filter(variable != "lambda_01") %>% 
    group_by(time_index, variable, source) %>%
    summarise(time = mean(left_time_boundary), mean = mean(value), sd = sd(value)) 
  
  # This is a dummy value becuase I don't have
  # replicates of the MSMC yet.
  #msmc_g$sd <- 1
  
  msmc_g$variable <- as.character(msmc_g$variable)
  msmc_g$variable2 <- msmc_g$variable
  msmc_g$variable2[msmc_g$variable == "lambda_00"] = 0
  msmc_g$variable2[msmc_g$variable == "lambda_11"] = 1
  msmc_g$variable2 = factor(msmc_g$variable2)
  msmc_g$variable <- factor(msmc_g$variable2)
  
  comp <- ggplot(smc2_g, aes(x = Start*g, y = mean, ymin = mean-sd, max = mean+sd, fill = factor(From))) + 
    geom_step(data = smc2_g, aes(col = factor(From), linetype = factor(1))) + 
    geom_ribbon(stat = "stepribbon", alpha = 0.3) + 
    geom_step(data = msmc_g, inherit.aes = F, aes(x = time, y = mean, col = factor(variable), linetype = factor(2)) ) + 
    geom_ribbon(stat = "stepribbon", alpha = 0.3, data = msmc_g, inherit.aes = F, aes(x = time, ymin = mean-sd, ymax = mean+sd, fill = factor(variable))) +
    scale_x_log10(limits = c(1e4, 1e6), labels = label_comma(scale = 0.001, suffix = "k")) + 
    scale_y_log10(limits = c(2e3, 5e4), labels = label_comma()) + 
    scale_color_manual(values = c("red", "blue", "red", "blue"), labels = c("Han Chinese", "Nigerian Yoruban")) +
    scale_fill_manual(values = c("red", "blue", "red", "blue"), labels = c("Han Chinese", "Nigerian Yoruban")) +
    scale_linetype_manual(values = c(1, 2), labels = c("SMCSMC", "MSMC")) +
    ylab("Population size") + 
    xlab("Years ago") + 
    theme_bw() +
    theme(legend.position = "top", 
          legend.title = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 9/16) + 
    annotation_logticks() +
    guides(colour = guide_legend(order = 2), 
           linetype = guide_legend(order = 1),
           fill = guide_legend(order = 2))
  
  return(comp)
  
}

#' @description  SMC2 Joint versus Single
#' @return ggplot
single_versus_joint_inference = function(){
  #single <- plot(smcsmc("~/repos/ancient_migration/data/yri4-vb.out"), type = "ne", ylim = c(1e3, 1e5), return_df = T)
  #single_eurasian <- plot(smcsmc("~/repos/ancient_migration/data/ceu4-vb.out"), type = "ne", ylim = c(1e3, 1e5), return_df = T)
  
  han_files = list.files("~/repos/dirmig/data/sgdp/", pattern = "\\d+.S_Han-1.out", full.names = T)
  yri_files = list.files("~/repos/dirmig/data/sgdp/", pattern = "(1791095846|2135392492|946286477).S_Yoruba-1.out", full.names = T)
  
  files = list.files("~/repos/dirmig/data/sgdp/", pattern = "S_Han-1.S_Yoruba-1.out", full.names = T)
  
  smc2 <- NA
  for(s in lapply(files, smcsmc)){
    if(is.na(smc2)){
      smc2 <- s@data %>% filter(Iter == 10)
    } else {
      smc2 <- rbind(smc2, s@data  %>% filter(Iter == 10))
    }
  }
  
  yri = NA
  for(s in lapply(yri_files, smcsmc)){
    if(is.na(yri)){
      yri <- s@data %>% filter(Iter == 10)
    } else {
      yri <- rbind(yri, s@data  %>% filter(Iter == 10))
    }
  }
  
  han = NA
  for(s in lapply(han_files, smcsmc)){
    if(is.na(han)){
      han <- s@data %>% filter(Iter == 10)
    } else {
      han <- rbind(han, s@data  %>% filter(Iter == 10))
    }
  }
  
  han <- han %>% mutate(From = 0) %>% mutate(a = "Individual")
  yri <- yri %>% mutate(From = 1) %>% mutate(a = "Individual")
  
  smc2_g <- rbind(smc2 %>% mutate(a = "Joint"), han, yri) %>%
    filter(Type == "Coal") %>% 
    group_by(From,a, Start) %>% 
    summarise(mean = mean(Ne), sd = sd(Ne))
  smc2_g$sd[is.na(smc2_g$sd)] <- 1
  
  comp_single <- ggplot(smc2_g, aes(x = Start*g, y = mean, ymin = mean-sd, max = mean+sd, fill = factor(From), linetype = factor(a))) + 
    geom_step( aes(col = factor(From))) + 
    geom_ribbon(stat = "stepribbon", alpha = 0.3) + 
    scale_x_log10(limits = c(1e4, 1e6), labels = label_comma(scale = 0.001, suffix = "k")) + 
    scale_y_log10(limits = c(2e3, 5e4), labels = label_comma()) + 
    scale_color_manual(values = c("red", "blue"), labels = c("Han Chinese", "Nigerian Yoruban")) +
    scale_fill_manual(values = c("red", "blue"), labels = c("Han Chinese", "Nigerian Yoruban")) +
    scale_linetype_manual(values = c(3, 1), labels = c("Individual", "Joint")) +
    #ylab("Effective Population Size") + 
    ylab("") +
    xlab("Years ago") + 
    theme_bw() +
    theme(legend.position = "top", 
          legend.title = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 9/16) + 
    annotation_logticks() + 
    guides(colour = guide_legend(order = 2), 
           linetype = guide_legend(order = 1, override.aes = list(fill = NA)),
           fill = guide_legend(order = 2)) +
    theme(legend.key = element_rect(fill = NA, color = NA))
  
  return(comp_single)
}

#' @description All Eurasian Ne from SGDP
#' @return ggplot
all_eur_histories = function(){
  config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/sgdp_replication.json")
  source = names_from_config(config$source)
  source_strings = ids_to_names(source)
  sink = names_from_config(config$sink)
  sink_strings = ids_to_names(sink)
  path = "~/repos/dirmig/data/sgdp_subset/"
  
  smc2 <- NA
  
  for(so in source){
    
    files <- list.files(path, pattern = paste0(so), full.names = T) %>% grep(pattern = ".out", value = T)
    
    for(s in lapply(files, smcsmc)){
      if(is.na(smc2)){
        smc2 <- s@data %>% mutate(source = so) %>% filter(Iter == 25)
      } else {
        smc2 <- rbind(smc2, s@data %>% mutate(source = so) %>% filter(Iter == 25))
      }
    }
    
  }
  
  smc2_g <- smc2 %>% filter(Type == "Coal") %>% mutate(source = ids_to_names(source)) %>% filter(From == 0) %>% group_by(Start, source) %>% summarise(mean = mean(Ne), sd = sd(Ne)/sqrt(12))
  
  comp_eur <- ggplot(smc2_g, aes(x = Start*g, y = mean, ymin = mean-sd, max = mean+sd, fill = factor(source))) + 
    geom_step(data = smc2_g, aes(col = factor(source))) + 
    geom_ribbon(stat = "stepribbon", alpha = 0.3) + 
    scale_x_log10(limits = c(1e4, 1e6), labels = label_comma(scale = 0.001, suffix = "k")) + 
    scale_y_log10(limits = c(1e3, 1e5), labels = label_comma()) + 
    #scale_color_manual(values = c("red", "blue", "green", "orange"), labels = labels) +
    #scale_fill_manual(values = c("red", "blue", "green", "orange"), labels = labels) +
    #scale_linetype_manual(values = c(1, 1,2, 2), labels = labels) +
    ylab("Effective Population Size") + 
    xlab("Years ago") + 
    theme_bw() +
    theme(legend.position = "right", 
          legend.title = element_blank(),
          panel.grid = element_blank()) + 
    annotation_logticks()
  
  return(comp_eur)
}

#' @description  All African Ne from SGDP
#' @return ggplot
all_afr_histories = function(){
  config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/whole_sgdp.json")
  source = names_from_config(config$source)
  source_strings = ids_to_names(source)
  sink = names_from_config(config$sink)
  sink_strings = ids_to_names(sink)
  path = "~/repos/dirmig/data/sgdp/"
  
  smc2 <- NA
  
  for(so in sink){
    
    files <- list.files(path, pattern = paste0(so), full.names = T) %>% grep(pattern = ".out", value = T)
    
    for(s in lapply(files, smcsmc)){
      if(is.na(smc2)){
        smc2 <- s@data %>% mutate(source = so) %>% filter(Iter == 10)
      } else {
        smc2 <- rbind(smc2, s@data %>% mutate(source = so) %>% filter(Iter == 10))
      }
    }
    
  }
  
  smc2_g <- smc2 %>% filter(Type == "Coal") %>% filter(From == 1) %>% mutate(source = ids_to_names(source))%>% group_by(Start, source) %>% summarise(mean = mean(Ne), sd = sd(Ne)/sqrt(9))
  
  comp_afr <- ggplot(smc2_g, aes(x = Start*g, y = mean, ymin = mean-sd, max = mean+sd, fill = factor(source))) + 
    geom_step(data = smc2_g, aes(col = factor(source))) + 
    geom_ribbon(stat = "stepribbon", alpha = 0.3) + 
    scale_x_log10(limits = c(1e4, 1e6), labels = label_comma(scale = 0.001, suffix = "k")) + 
    scale_y_log10(limits = c(1e3, 1e5), labels = label_comma()) + 
    #scale_color_manual(values = c("red", "blue", "green", "orange"), labels = labels) +
    #scale_fill_manual(values = c("red", "blue", "green", "orange"), labels = labels) +
    #scale_linetype_manual(values = c(1, 1,2, 2), labels = labels) +
    ylab("Effective Population Size") + 
    xlab("Years ago") + 
    theme_bw() +
    theme(legend.position = "right", 
          legend.title = element_blank(),
          panel.grid = element_blank()) + 
    annotation_logticks()
  
  return(comp_afr)
}

#' @description Construct the joint figure for comparative histories.
#' @return ggplot
make_all_histories_figure = function(comp_eur, comp_afr){
  comp_both <- comp_eur / comp_afr + plot_annotation(tag_levels = "A")
  return(comp_both)
}

#' @description Three simulation plots for Figure 2
#' @return ggplot
plot_ne_simulations = function(){
  
  truth = read.table("~/repos/dirmig/data/spvaryingmig/truth.csv", sep = ",", h = T)
  s1 <- smcsmc(paste0("~/repos/dirmig/data/spvaryingmig_confident/backward_60000_10000_0.5_2.out"))
  single_genome_1 = smcsmc(paste0("~/repos/dirmig/data/spvaryingmig/sp.backward_60000_10000_0.5_9.out")) 
  sg_1 <- single_genome_1@data %>% filter(Type == "Coal") %>% filter(Iter == max(Iter)) %>% mutate(From = 3, run = "Backwards")
  
  
  s2 <- smcsmc(paste0("~/repos/dirmig/data/spvaryingmig_confident/bidirectional_60000_10000_0.5_2.out"))
  single_genome_2 = smcsmc(paste0("~/repos/dirmig/data/spvaryingmig/sp.bidirectional_60000_10000_0.5_9.out"))
  sg_2 <- single_genome_2@data %>% filter(Type == "Coal") %>% filter(Iter == max(Iter)) %>% mutate(From = 3, run = "Bidirectional")
  
  
  s3 <- smcsmc(paste0("~/repos/dirmig/data/spvaryingmig_confident/forward_60000_10000_0.5_2.out"))
  single_genome_3 = smcsmc(paste0("~/repos/dirmig/data/spvaryingmig/sp.forward_60000_10000_0.5_9.out"))
  sg_3 <- single_genome_3@data %>% filter(Type == "Coal") %>% filter(Iter == max(Iter)) %>% mutate(From = 3, run = "Forwards")
  
  
  df1 <- plot(s1, return_df = T, type = "ne", ylim = c(3e3, 5e4), xlim = c(1e4, 1e6)) %>% mutate(run = "Backwards", From = as.numeric(From))
  df2 <- plot(s2, return_df = T, type = "ne", ylim = c(3e3, 5e4), xlim = c(1e4, 1e6)) %>% mutate(run = "Bidirectional", From = as.numeric(From))
  df3 <- plot(s3, return_df = T, type = "ne", ylim = c(3e3, 5e4), xlim = c(1e4, 1e6)) %>% mutate(run = "Forwards", From = as.numeric(From))
  
  df1 <- rbind(df1, sg_1)
  df2 <- rbind(df2, sg_2)
  df3 <- rbind(df3, sg_3)
  
  truth %<>% mutate(From = 9)
  
  
  mid = 60000
  sim1 = ggplot(df1, aes(x = Start*29, y = Ne, col = as.factor(From), lty = as.factor(From)))  +
    geom_step(data = truth, aes(x = time*29, y = yri_ne*14312, col = as.factor(From))) + 
    geom_step(data = truth, aes(x = time*29, y = ceu_ne*14312, col = as.factor(From))) +
    geom_vline(xintercept = c(as.numeric(mid) + 5000, as.numeric(mid) - 5000), col = "red", linetype = 2) +
    geom_step() + 
    scale_x_log10(limits = c(1e4, 1e6), labels = label_comma(scale = 0.001, suffix = "k")) +
    scale_y_log10(limits = c(2e3, 5e4), labels = label_comma()) +
    scale_color_manual(values = c("red", "blue", "blue", "black"), label = c("Inferred Joint Eurasian", "Inferred Joint African", "Inferred Single African", "Simulated Truth Ne")) + 
    scale_linetype_manual(values = c(1,1,3,1), label = c("Inferred Joint Eurasian", "Inferred Joint African", "Inferred Single African", "Simulated Truth Ne")) +
    #facet_wrap(~run, ncol = 1, strip.position = "right") + theme_bw() + 
    theme_bw() +
    theme( legend.position = "bottom",
           legend.title = element_blank(),
           panel.grid = element_blank(),
           aspect.ratio = 9/16) +
    xlab("Years ago") + 
    ylab("Population size") + 
    annotation_logticks()
  
    sim2 = ggplot(df2, aes(x = Start*29, y = Ne, col = as.factor(From), lty = as.factor(From)))  +
      geom_step(data = truth, aes(x = time*29, y = yri_ne*14312, col = as.factor(From))) + 
      geom_step(data = truth, aes(x = time*29, y = ceu_ne*14312, col = as.factor(From))) +
      geom_vline(xintercept = c(as.numeric(mid) + 5000, as.numeric(mid) - 5000), col = "red", linetype = 2) +
      geom_step() + 
      scale_x_log10(limits = c(1e4, 1e6), labels = label_comma(scale = 0.001, suffix = "k")) +
      scale_y_log10(limits = c(2e3, 5e4), labels = label_comma()) +
      scale_color_manual(values = c("red", "blue", "blue", "black"), label = c("Inferred Joint Eurasian", "Inferred Joint African", "Inferred Single African", "Simulated Truth Ne")) + 
      scale_linetype_manual(values = c(1,1,3,1), label = c("Inferred Joint Eurasian", "Inferred Joint African", "Inferred Single African", "Simulated Truth Ne")) + 
    #facet_wrap(~run, ncol = 1, strip.position = "right") + theme_bw() + 
    theme_bw() +
      theme( legend.position = "bottom",
             legend.title = element_blank(),
             panel.grid = element_blank(),
             aspect.ratio = 9/16) +
      xlab("Years ago") + 
      ylab("") +
      annotation_logticks()
    
    sim3 = ggplot(df3, aes(x = Start*29, y = Ne, col = as.factor(From), lty = as.factor(From)))  +
      geom_step(data = truth, aes(x = time*29, y = yri_ne*14312, col = as.factor(From))) + 
      geom_step(data = truth, aes(x = time*29, y = ceu_ne*14312, col = as.factor(From))) +
      geom_vline(xintercept = c(as.numeric(mid) + 5000, as.numeric(mid) - 5000), col = "red", linetype = 2) +
      geom_step() + 
      scale_x_log10(limits = c(1e4, 1e6), labels = label_comma(scale = 0.001, suffix = "k")) +
      scale_y_log10(limits = c(2e3, 5e4), labels = label_comma()) +
      scale_color_manual(values = c("red", "blue", "blue", "black"), label = c("Inferred Joint Eurasian", "Inferred Joint African", "Inferred Single African", "Simulated Truth Ne")) + 
      scale_linetype_manual(values = c(1,1,3,1), label = c("Inferred Joint Eurasian", "Inferred Joint African", "Inferred Single African", "Simulated Truth Ne")) +
    #facet_wrap(~run, ncol = 1, strip.position = "right") + theme_bw() + 
    theme_bw() +
      theme( legend.position = "bottom",
             legend.title = element_blank(),
             panel.grid = element_blank(),
             aspect.ratio = 9/16) +
      xlab("Years ago") + 
      ylab("") +
      annotation_logticks()
  
  sims = sim1 + sim2 + sim3  + plot_layout(guides = 'collect') & theme(legend.position = "bottom")
  return(sims)
}


#' @description Compose Ne figure.
#' @return ggplot2
figure_2 = function(comp, comp_single, sims){
  all <- (comp + comp_single) / (sims) + 
    plot_layout(heights = c(2,1)) + 
    plot_annotation(tag_levels = "a") & 
    theme(plot.tag.position = c(0, 1),
          plot.tag = element_text(face = "bold", hjust = 0, vjust = 0))
  return(all)
  
}