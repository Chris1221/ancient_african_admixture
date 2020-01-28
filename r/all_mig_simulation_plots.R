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

migs = c("0.0", "0.1", "0.3", "0.5", "0.7", "0.9")
situations = c(0,2,4)
midpoints <- c("40000", "50000", "60000", "70000")

ylim = c(0, 5e-4)
g = 30
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
          s2 <- smcsmc(paste0("~/repos/dirmig/data/spvaryingmig/", s, "_", mid, "_10000_", mig, "_", sit, ".out"))
          p <- plot(s2, ylim = c(0, 5e-4))
          mplots[[i]] <- p
        
          p2 <- plot(s2, type = "ne", ylim = c(3e3, 1e5))
          neplots[[i]] <- p2
          i = i+1
          }
    }
    labels = c("0", "1", "5")
    title = paste0("Midpoint: ", mid)
    m[[j]] = ggmatrix(mplots, nrow = length(situations), title = title, ncol = length(migs), byrow = F, xAxisLabels = migs, yAxisLabels = labels, xlab = "Thousands of Years before Present", ylab = "Proportion Replaced per Generation")
    ne[[j]] = ggmatrix(neplots, nrow = length(situations), title = title, ncol = length(migs), byrow = F, xAxisLabels = migs, yAxisLabels = labels, xlab = "Thousands of Years before Present", ylab = "Effective Population Size")
    j = j +1
  }
  lay = rbind( c(1,5), c(2, 6), c(3, 7), c(4, 8))
  plots <- arrange_ggmatrix(c(m, ne), lay = lay)
  ggsave(plots, file=paste0("~/repos/dirmig/plot/sims/", s, "_different_starts.pdf"), height = 12, width = 20, units = "in")
}
