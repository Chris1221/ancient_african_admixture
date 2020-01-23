# Makes a boxplot of average Ne estimated by MSMC and SMC2 at particular times
library(smcsmcTools)
library(ggplot2)
library(dplyr)
library(smcsmcTools)
library(scales)

times <- c(300e3, 200e3, 250e3, 150e3, 120e3, 100e3, 75e3)
path <- "~/repos/dirmig/data/sgdp_subset/"
seed <- "1791095846"

msmc_files <- list.files(path, pattern = "final.txt", full.names = T)
smc2_files <- list.files(path, pattern = "out", full.names = T) %>% grep(pattern = seed, value = T)

pid = 1

df <- data.frame(
  time = numeric(),
  est = numeric(),
  pop = integer(),
  method = character()
)

for(time in times){
  smc2 = lapply(smc2_files, smcsmc) %>% lapply(ne_at, time) %>% unlist %>% matrix(ncol = 2, byrow = T)
  msmc = lapply(msmc_files, msmc) %>% lapply(ne_at, time) %>% unlist %>% matrix(ncol = 2, byrow = T)
  
  df <- rbind(df, data.frame(time = time, est = smc2[,1], pop = "Eurasian", method = "smcsmc"))
  df <- rbind(df, data.frame(time = time, est = smc2[,2], pop = "African", method = "smcsmc"))
  df <- rbind(df, data.frame(time = time, est = msmc[,1], pop = "Eurasian", method = "MSMC"))
  df <- rbind(df, data.frame(time = time, est = msmc[,2], pop = "African", method = "MSMC"))
  # Slow but its a small loop
  #df <- rbind(df, data.frame(time = time, smc_1 = smc2[,1], smc_2 = smc2[,2], msmc_1 = msmc[,1], msmc_2=msmc[,2]))
}

s <- df %>% group_by(time, pop, method) %>% summarise(mean = mean(est), sd = sd(est))  %>% mutate(upper = mean + sd) %>% mutate(lower = mean - sd)

ggplot(s, aes(x = time, y =mean, col = method)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  facet_wrap(~pop) + 
  ylab("Average Ne \u00B1 Standard Deviation") +
  xlab("Thousands of Years before Present") +
  scale_x_continuous(labels = label_comma(scale = 0.001)) +
  theme_bw() + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "bottom") + theme(panel.grid = element_blank())

ggsave("~/repos/dirmig/plot/ne/average_ne_by_method.pdf")
