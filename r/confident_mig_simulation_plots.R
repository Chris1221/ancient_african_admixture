# Just plotting 5 examples that we have now for the various models. 
library(smcsmcTools)
library(data.table)
library(dplyr)
library(ggplot2)
library(GGally)

scenarios = c("backward", "forward", "bidirectional")#, "backward")#, "bidirectional", "realistic")
mplots <- list()
neplots <- list()
i <- 1

migs = c("0.0", "0.1", "0.5", "0.9")
situations = c(2,4)
midpoints <- c("60000")

ylim = c(0, 5e-4)
g = 29
matrices <- list()

m <- list()
ne = list()
j = 1


for (s in scenarios){
  j = 1
  for(mid in midpoints){
    i = 1
    for(mig in migs){
      for(sit in situations){
        s2 <- smcsmc(paste0("~/repos/dirmig/data/spvaryingmig_confident/", s, "_", mid, "_10000_", mig, "_", sit, ".out"))
        p <- plot(s2, ylim = c(0, 5e-4), xlim = c(1e4, 1e6)) + 
          geom_vline(xintercept = c(as.numeric(mid) + 5000, as.numeric(mid) - 5000), col = "red", linetype = 2)
        mplots[[i]] <- p
        
        truth = read.table("~/repos/dirmig/data/spvaryingmig/truth.csv", sep = ",", h = T)
        single_genome = smcsmc(paste0("~/repos/dirmig/data/spvaryingmig/sp.", s, "_", mid, "_10000_", mig, "_9.out"))
        single_genome_df <- single_genome@data %>% filter(Type == "Coal") %>% filter(Iter == max(Iter))
        p2 <- plot(s2, type = "ne", ylim = c(3e3, 1e5), xlim = c(1e4, 1e6), iter = 1) + 
          geom_step(data = single_genome_df, aes(x = Start*29, y = Ne), col = "green") + 
          geom_vline(xintercept = c(as.numeric(mid) + 5000, as.numeric(mid) - 5000), col = "red", linetype = 2) +
          geom_step(data = truth, aes(x = time*29, y = yri_ne*14312), col = "grey") + 
          geom_step(data = truth, aes(x = time*29, y = ceu_ne*14312), col = "grey")
        neplots[[i]] <- p2
        i = i+1
      }
    }
    labels = c("1", "5")
    title = paste0("Midpoint: ", mid)
    m[[j]] = ggmatrix(mplots, nrow = length(situations), title = title, ncol = length(migs), byrow = F, xAxisLabels = migs, yAxisLabels = labels, xlab = "Thousands of Years before Present", ylab = "Proportion Replaced per Generation")
    ne[[j]] = ggmatrix(neplots, nrow = length(situations), title = title, ncol = length(migs), byrow = F, xAxisLabels = migs, yAxisLabels = labels, xlab = "Thousands of Years before Present", ylab = "Effective Population Size")
    j = j +1
  }
  lay = rbind( c(1,5), c(2, 6), c(3, 7), c(4, 8))
  plots <- arrange_ggmatrix(c(m, ne), lay = lay)
  ggsave(plots, file=paste0("~/repos/dirmig/plot/sims/", s, "_confident_different_starts.pdf"), height = 12, width = 20, units = "in")
}
