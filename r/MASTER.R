library(smcsmcTools)
library(ggplot2)
library(patchwork)
library(xtable)
library(dplyr)
library(rjson)
library(GGally)
library(stringi)
library(scales)
library(ggalt)
library(ggpubr)
library(tidyr)
library(latex2exp)
library(forcats)
library(rstatix)
library(dict)
library(reshape2)

# Figure 1: migration
source("r/migration_plot.R")
figure_1 = migration_plot(
  sgdp = sgdp_three_pop_curve(),
  hgdp = hgdp_three_pop_curve(),
  sgdp_msmc = sgdp_msmc(),
  hgdp_msmc = hgdp_msmc(),
  boxplot = sgdp_boxplot(),
  sims1 = figure_1_sim_plots()[[1]], 
  sims2 = figure_1_sim_plots()[[2]],
  sims3 = figure_1_sim_plots()[[3]]) %>% 
    ggsave(file = "~/repos/dirmig/plot/new_mig_plot.pdf", width = 12, height = 6)

# Figure 2: Ne
source("r/new_ne_plot.R")
figure_2(
  yri_han_ne(),
  single_versus_joint_inference(),
  plot_ne_simulations()
) %>%
  ggsave(file = "~/repos/dirmig/plot/new_ne_figure.pdf", height = 8, width = 12)

# All histories from the SGDP subset.
source("r/new_ne_plot.R")
make_all_histories_figure(
  all_eur_histories(),
  all_afr_histories()
) %>% 
  ggsave(file="~/repos/dirmig/plot/individual_populations_averaged.pdf")

# Descriptive statistics and plots for IMF in the SGDP.
source("r/heatmaps.R")
imf_heatmap() %>% ggsave(file = "plot/mig/integrated_sgdp.pdf", height = 5.18, width = 14.9)
imf_heatmap("hgdp") %>% ggsave(file = "plot/mig/imf_hgdp.pdf", height = 5, width = 10)
simple_imf_boxplot() %>% ggsave(file = "~/repos/dirmig/plot/mig/sgdp_averages.pdf", height = 5.61, width = 12.5, units = "in")
sgdp_imf_t_test() # T test numbers on line 113 
sgdp_imf_t_test_table() # TeX for the T test table
papuan_linear_model()[["hgdp"]]
sgdp_imf_averages_table() # TeX for the averages table.

# Ne and Mig plot for SGDP subset
source("r/sgdp_subset_plots.r")
sgdp_subset_plot(
  plot_subset_ne(),
  plot_subset_mig()
) %>% ggsave(file = "plot/subset_ne_mig.pdf", height = 9, width = 8)

# Ne and Mig plot for HGDP
source("r/hgdp_plots.R")
hgdp_plot(
  plot_hgdp_ne(),
  plot_subset_mig()
) %>% ggsave(file = "plot/hgdp_ne_mig.pdf", height = 9, width = 8)

# Comparison of HGDP and subset of SGDP
source("r/both_figure.R")
sgdp_hgdp_mig_plot() %>% ggsave(file = "plot/both_average_mig.pdf", height = 4, width = 8)
sgdp_hgdp_ne_plot() %>% ggsave(file = "plot/both_average_ne.pdf", height = 4, width = 8)
sgdp_hgdp_imf_heatmap() %>%ggsave(file = "plot/both_figure.pdf", height = 10, width = 8)
plot_dendrograms() %>% ggsave(file = "plot/dendrograms.pdf", height = 8, width = 8)

# Length distribution plot
source("r/segments_table.R")
length_distribution_table = length_plot()[["table"]]
length_plot()[["plot"]] %>% ggsave(file = "plot/both_length.pdf", height = 8, width = 8)

# Simulation line plot
source("r/sim_line_plot.R")
simulation_line_plot() %>% ggsave(file = "~/repos/dirmig/plot/sim_line_plot.pdf", h = 6, w = 10)

source("r/new_dstats_figure.R")
plot_reich_figure(
  dotchart = reich_all_dotchart(),
  ancientheatmap = reich_ancientheatmap(),
  modern_bar_plot = reich_modern_density(),
  archaic_bar_plot = reich_archaic_density()
) %>% ggsave(file = "~/repos/dirmig/plot/new_dstats.pdf", height = 8, width = 12)
