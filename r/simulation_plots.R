# Just plotting 5 examples that we have now for the various models. 
library(smcsmcTools)
library(data.table)
library(dplyr)
library(ggplot2)
library(GGally)

avg_migr <- function(
  file,
  ancient,
  modern,
  g
){
  # Checks and imports 
  `%>%` <- dplyr::`%>%`
  
  assertthat::assert_that(  
    is.character(file) &&
      is.numeric(g) && 
      is.numeric(ancient) && 
      is.numeric(modern))
  
  # Read and set parameters. 
  df <- data.table::fread(file, h = T) %>%
    dplyr::filter(Type == "Migr") %>% 
    dplyr::filter(Iter == max(Iter)) %>% 
    dplyr::filter(Start < (ancient / g)) %>%
    dplyr::filter(End > (modern / g)) %>%
    dplyr::mutate(Diff = End - Start) %>%
    dplyr::mutate(totalepoch = Rate * Diff) %>%
    dplyr::group_by(From) %>%
    dplyr::summarise(sum = sum(totalepoch), integrated = 1-exp(-sum(totalepoch)))
  
  return(df)
  
}

scenarios = c("backward", "forward", "bidirectional")#, "backward")#, "bidirectional", "realistic")
plots <- list()
i <- 1

#migs =c("S_Khomani_San-1", "S_Han-1", "S_French-1", "S_Karitiana-1", "S_Yoruba-1" )
migs = c("0.0", "0.5", "1.0", "2.0", "4.0", "5.0", "10.0")
migs = c("0.0", "0.1", "0.3", "0.5", "0.7", "0.9")
#migs_yri = c("S_Papuan-1", "S_Papuan-8", "S_Papuan-11", "S_Mixe-2")
time = c("60000")

type = c("Coal", "Migr")
ylim = c(0, 5e-4)
g = 30

plots = list()
i = 1
for (s in scenarios){
  for(m in migs){
    for(t in time){
      plots[[i]] <- smcsmc(paste0("~/repos/eurasian-backmigration/data/simulations_spvaryingmig/", s, "_", t, "_10000_", m, "_1.out")) %>% plot(ylim = ylim)
      i = i + 1
    }}

  }

  ggmatrix(plots, nrow = length(migs), ncol = 3, byrow = T, yAxisLabels = migs_yri, ylab = "Effective Population Size", xlab = "Thousands of Years before Present", xAxisLabels = migs)
  