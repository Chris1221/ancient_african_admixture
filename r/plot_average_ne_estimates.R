# Makes a boxplot of average Ne estimated by MSMC and SMC2 at particular times
library(smcsmcTools)
library(ggplot2)
library(dplyr)
library(smcsmcTools)
library(scales)
library(dict)


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

plot <- ggplot(smc2_g, aes(x = Start*g, y = mean, ymin = mean-sd, max = mean+sd, fill = factor(From))) + 
  geom_vline(xintercept = seq(10000,100000, by = 10000), col = "grey", alpha = 0.3) + 
  geom_vline(xintercept = seq(1e5,1e6,by=1e5), col = "grey", alpha = 0.3) + 
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
        panel.grid.minor = element_blank()) +
  facet_grid(source~samp)

ggsave(plot, file = "~/repos/dirmig/plot/both_average_ne.pdf", height = 4, width = 8)
