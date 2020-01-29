library(smcsmcTools)
library(rjson)
library(GGally)
library(dplyr)
library(scales)

config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/sgdp_replication.json")
source = names_from_config(config$source) # from tools
source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
sink = names_from_config(config$sink) # from tools
sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
seed = "1791095846"
smc2_path = "~/repos/eurasian-backmigration/v2/data/sgdp_subset/"
msmc_path = "~/repos/eurasian-backmigration/v2/data/sgdp_subset/"


list_of_msmc <- 0
for (si in sink){
  for (so in source){
    msmc_file = msmc(file = paste0(msmc_path, so, ".", si, ".final.txt"))
    
    if(!is.data.frame(list_of_msmc)){
      list_of_msmc <- plot(msmc_file, return_df = T, lines = c("lambda_01"), ylim = c(0, 1)) %>%
                              mutate(sink = si) %>%
                              mutate(source = so)
    } else {
      list_of_msmc <- rbind(list_of_msmc, plot(msmc_file, return_df = T, lines = c("lambda_01"), ylim = c(0, 1)) %>% 
                              mutate(sink = si) %>%
                              mutate(source = so))
    }}}

ggplot(data = list_of_msmc %>% filter(sink == "S_Yoruba-1"), aes(x = left_time_boundary, y = value, col = source)) + 
  geom_vline(xintercept = seq(1e4,1e5,1e4), col = "grey", alpha = 0.3) +
  geom_vline(xintercept = seq(1e5,1e6,1e5), col = "grey", alpha = 0.3) +
  geom_step() + 
  scale_x_log10(labels = label_comma(scale = 0.001)) + 
  geom_hline(yintercept = 0.5, alpha = 0.2)+ 
  theme_bw() + 
  xlab("Thousands of Years before Present") + 
  ylab("Relative X-Coal Rate") + 
  theme(legend.position = "right",
        legend.title = element_blank(),
        panel.grid = element_blank()) + 
  facet_wrap(~sink, nrow = 1)

ggsave("~/repos/dirmig/plot/mig/yoruba-xcoal.pdf", height = 6.49, width = 7.64, units = "in")

ggplot(data = list_of_msmc %>% filter(sink == "S_Khomani_San-1"), aes(x = left_time_boundary, y = value, col = source)) + 
  geom_vline(xintercept = seq(1e4,1e5,1e4), col = "grey", alpha = 0.3) +
  geom_vline(xintercept = seq(1e5,1e6,1e5), col = "grey", alpha = 0.3) +
  geom_step() + 
  scale_x_log10(labels = label_comma(scale = 0.001)) + 
  geom_hline(yintercept = 0.5, alpha = 0.2)+ 
  theme_bw() + 
  xlab("Thousands of Years before Present") + 
  ylab("Relative X-Coal Rate") + 
  theme(legend.position = "right",
        legend.title = element_blank(),
        panel.grid = element_blank()) + 
  facet_wrap(~sink, nrow = 1)

ggsave("~/repos/dirmig/plot/mig/san-xcoal.pdf", height = 6.49, width = 7.64, units = "in")

#ggmatrix(plots %>% filter, nrow = length(source), ncol = length(sink), byrow = FALSE,xlab = "Years Before Present", ylab = "Estimated Effective Population Size", yAxisLabels = source_strings, xAxisLabels = sink_strings, legend = leg) + theme(legend.position = "bottom")
#ggsave("~/repos/dirmig/plot/sgdp_subet_mig.pdf", dpi = 300, height = 10, width = 8, unit = "in")
