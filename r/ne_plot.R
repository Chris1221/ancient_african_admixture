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
ggsave("~/repos/dirmig/plot/sgdp_subet_ne.pdf", height = 10, width = 8, unit = "in")

han <- plots[[18]]

two <- plot(smcsmc("~/repos/ancient_migration/data/yri4-vb.out"), type = "ne", ylim = c(1e3, 1e5), return_df = T)
label = c("MSMC African", "SMCSMC Eurasian", "SMCSMC African", "MSMC Eurasian", "SMCSMC Single Genome")
real_data = han + geom_step(data = two, aes(x = Start * 29, y = Ne, col = factor(To), lty = factor(To))) + 
  scale_color_manual(values = c("red", "blue", "red", "blue", "green"), labels = label) + 
  scale_linetype_manual(values = c(2, 1, 1, 2, 1), labels = label) + 
  scale_y_log10(limits = c(2e3, 5e4))
real_data


# Simualtions
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

df <- bind_rows(df1, df2, df3, sg_1, sg_2, sg_3)

mid = 60000
sim = ggplot(df, aes(x = Start*29, y = Ne, col = as.factor(From)))  +
  geom_vline(xintercept = seq(10000,100000, by = 10000), col = "grey", alpha = 0.3) + 
  geom_vline(xintercept = seq(1e5,1e6,by=1e5), col = "grey", alpha = 0.3) + 
  geom_step(data = truth, aes(x = time*29, y = yri_ne*14312), col = "black") + 
  geom_step(data = truth, aes(x = time*29, y = ceu_ne*14312), col = "black") +
  geom_vline(xintercept = c(as.numeric(mid) + 5000, as.numeric(mid) - 5000), col = "red", linetype = 2) +
  geom_step() + 
  scale_x_log10(limits = c(1e4, 1e6), labels = label_comma(scale = 0.001)) +
  scale_y_log10(limits = c(2e3, 5e4), labels = label_comma()) +
  scale_color_manual(values = c("blue", "red", "green")) + 
  facet_wrap(~run, ncol = 1, strip.position = "right") + theme_bw() + 
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Thousands of Years before Present") + 
  ylab("")



ggarrange(real_data, sim, widths= c(3,2), common.legend = T, legend = "top", labels = "auto")
ggsave("~/repos/dirmig/plot/ne_figure.pdf", height = 8, width = 12, units = "in")
