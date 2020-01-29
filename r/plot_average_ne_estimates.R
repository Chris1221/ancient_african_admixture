# Makes a boxplot of average Ne estimated by MSMC and SMC2 at particular times
library(smcsmcTools)
library(ggplot2)
library(dplyr)
library(smcsmcTools)
library(scales)

times <- c(300e3, 200e3, 250e3, 150e3, 120e3, 100e3, 75e3)
path <- "~/repos/dirmig/data/sgdp_subset/"
seed <- "1791095846"

samples = c("Biaka", "Mbuti", "Yoruba", "Khomani_San")

plots <- list()
i = 1

for(samp in samples){
  msmc_files <- list.files(path, pattern = "final.txt", full.names = T) %>% grep(pattern= samp, value = T)
  smc2_files <- list.files(path, pattern = "out", full.names = T) %>% grep(pattern = seed, value = T) %>%grep(pattern = samp, value = T)
  
  smc2 <- NA 
  msmc_df <- NA
  for(s in lapply(smc2_files, smcsmc)){
    if(is.na(smc2)){
      smc2 <- s@data
    } else {
      smc2 <- rbind(smc2, s@data)
    }
  }
  for(s in lapply(msmc_files, msmc)){
    if(is.na(msmc_df)){
      msmc_df <- s@data
    } else {
      msmc_df <- rbind(msmc_df, s@data)
    }
  }
  
  mu = 1.25e-8
  g = 29
  smc2_g <- smc2 %>% filter(Iter == max(Iter), Type == "Coal") %>% group_by(From, Start) %>% summarise(mean = mean(Ne), sd = sd(Ne))
  msmc_g <- msmc_df %>%
    mutate(left_time_boundary = (left_time_boundary / mu) * g) %>%
    mutate(right_time_boundary = (right_time_boundary / mu) * g) %>%
    mutate(lambda_00 = (1 / lambda_00) / (2*mu)) %>% 
    mutate(lambda_11 = (1 / lambda_11) / (2*mu)) %>% 
    reshape2::melt(id.vars =c("time_index", "left_time_boundary", "right_time_boundary"), factorsAsStrings = F) %>%
    filter(variable != "lambda_01") %>% 
    group_by(time_index, variable) %>%
    summarise(time = mean(left_time_boundary), mean = mean(value), sd = sd(value)) 
  
  plots[[i]] <- ggplot(smc2_g, aes(x = Start*g, y = mean, ymin = mean-sd, max = mean+sd, fill = factor(From))) + 
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
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          legend.position = "top")
  
  leg <- grab_legend(plots[[1]])
  
  i = i + 1
}


ggmatrix(plots, ncol = 4, nrow = 1, ylab = "Effective Population Size", xlab = "Thousands of Years before Present", xAxisLabels = samples, legend = leg) + theme(legend.position = "top") 
ggsave("~/repos/dirmig/plot/ne/average_ne_subset.pdf", height = 5.87, width = 16.5, unit = "in")
