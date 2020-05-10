#' @importFrom dplyr %>% filter mutate
#' @importFrom data.table fread
#' @importFrom reshape2 melt
#' @export
plot.msmc <- function(input, 
                      mu = 1.25e-8, 
                      g = 29, 
                      xlim = c(1e4, 1e6), 
                      ylim = c(1e3, 1e6), 
                      lines = c("lambda_00", "lambda_11", "lambda_01"),
                      return_df = F){
  # For transforming relative xcoal rate.
  rmin = 0
  rmax = 1
  
  df = fread(input@file)
  df %<>% 
    mutate(left_time_boundary = (left_time_boundary / mu) * g) %>%
    mutate(right_time_boundary = (right_time_boundary / mu) * g) %>%
    mutate(lambda_01 = 2 * lambda_01 / (lambda_00 + lambda_11)) %>%
    mutate(lambda_01 = ((lambda_01 - min(lambda_01))/(max(lambda_01)-min(lambda_01))) * (ylim[2] - ylim[1]) + ylim[1]) %>%
    mutate(lambda_00 = (1 / lambda_00) / (2*mu)) %>% 
    mutate(lambda_11 = (1 / lambda_11) / (2*mu)) %>%
    melt(id.vars = c("time_index", "left_time_boundary", "right_time_boundary")) %>% 
    filter(variable == lines)
  
  if(return_df) return(df)
  
  p = ggplot(df, aes(x = left_time_boundary)) + 
       geom_step(aes(y = value, col = variable)) +
       scale_y_log10(limits = ylim) + 
       scale_x_log10(limits = xlim) + 
       ylab("Population Size") + 
       xlab("Time") + 
       theme_bw() +   
       theme(legend.position = "bottom", legend.title = element_blank()) 
  return(p)
}

#' @importFrom scales label_comma
#' @export
plot.smcsmc <- function(input, 
                        type = "migration", 
                        ylim = c(0, 2.5e-4), 
                        xlim = c(1e3, 1e6), 
                        g = 29,
                        N0 = 14312, 
                        title = "",
                        iter = "max",
                        return_df = F,
                        pops = NULL,
                        times = c(0,0)){
  
  if(iter == "max"){
    df <- input@data %>% filter(Iter == max(Iter))
  } else if (iter == "all"){
    df <- input@data
  } else if (is.numeric(iter)){
    df <- input@data %>% filter(Iter == iter)
  } else {
    stop("Bad value for iter.")
  }
  
  if (!all(times ==0)){
    df1 <- df[df$From == 0 & !(df$Start < times[1] / g),]
    df2 <- df[df$From == 1 & !(df$Start < times[2] / g),]
    df <- rbind(df1, df2)
  }
  if (type == "migration"){
    p <- .plot_migration(df, ylim,  xlim,  g, N0,  title, return_df, pops)
  } else if (type == "ne"){
    p <- .plot_ne(df, ylim,  xlim,  g, N0,  title, return_df, pops)
  } else {
    stop("Bad type arguement.")
  }
  
  return(p)
}

.plot_ne <- function(input, ylim,  xlim,  g, N0,  title, return_df, pops){
  df <- input %>% 
    filter(Type == "Coal") %>% 
    filter(Clump== -1)
  
  df$From <- as.factor(df$From)
  
  if(is.null(pops)){
    pops = c(levels(df$From))
  }
  
  if(return_df) return(df)
  
  p <- ggplot() + geom_step(
    data = df,
    aes(x = Start*g, 
        y = Ne, 
        group = From, 
        col = From)) + 
    scale_x_log10(limits=xlim, labels = label_comma(scale = 0.001)) + 
    scale_y_log10(limits = ylim, labels = label_comma()) +
    xlab("Time (thousands of years)") + 
    ylab("Effective Population Size") + 
    ggtitle(title) +
    scale_color_manual(values = c('blue', 'red', 'black'), labels = pops) + 
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank()) 
  
  return(p)
}
.plot_migration <- function(input, ylim,  xlim,  g, N0,  title, return_df, pops){
  df <- input %>%  
    dplyr::filter(Type == "Migr") %>% 
    dplyr::filter(Clump == -1) %>% 
    dplyr::select(Start, From, Rate)
  
  df$From <- as.factor(df$From)
  if(return_df) return(df)
 
  if(is.null(pops)){
    labels = levels(df$From) 
  } else {
    labels = c( paste0( pops[2], " to ", pops[1]),
                paste0( pops[1], " to ", pops[2]))
  }
  
  p <- ggplot(data = df) + 
    geom_vline(xintercept = seq(10000,100000, by = 10000), col = "grey", alpha = 0.3) + 
    geom_vline(xintercept = seq(1e5,1e6,by=1e5), col = "grey", alpha = 0.3) +
    geom_step(
      aes(x = Start*g, 
          y = Rate, 
          group = From, 
          col = From)) + 
    scale_x_log10(limits=xlim, labels = label_comma(scale = 0.001)) + 
    scale_y_continuous(limits = ylim, labels = label_comma()) +
    xlab("Time (thousands of years)") + 
    ylab("Proportion Replaced per Generation") + 
    ggtitle(title) +
    scale_color_manual(values = c('blue', 'red'), labels = labels) + 
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank()) 
  
  return (p)
} 
  
  
#' @import ggplot2
#' @importFrom scales label_comma
#' @export
plot_both_msmc_and_smcsmc <- function(smcsmc, msmc, type = "ne", ylim = c(1e3, 1e6), xlim = c(1e4, 1e6), g=29){
  if (type == "ne"){
    smc_df = plot(smcsmc, ylim = ylim, xlim = xlim, return_df = T, type = "ne")
    msmc_df = plot(msmc, ylim = ylim, xlim = xlim, return_df = T, lines = c("lambda_00", "lambda_11"), g = g)
    p <- ggplot() +
      geom_vline(xintercept = seq(10000,100000, by = 10000), col = "grey", alpha = 0.3) + 
      geom_vline(xintercept = seq(1e5,1e6,by=1e5), col = "grey", alpha = 0.3) + 
      geom_step(
        data = smc_df,
        aes(x = Start*g, 
            y = Ne, 
            group = From, 
            col = From,
            linetype = From)) + 
      geom_step(
        data = msmc_df,
        aes(x = left_time_boundary,
            y = value,
            group = variable,
            col = variable,
            linetype = variable)) + 
      scale_x_log10(limits=xlim, labels = label_comma(scale = 0.001)) + 
      scale_y_log10(limits =ylim, labels = label_comma()) +
      xlab("Thousands of Years before Present") + 
      ylab("Effective Population Size") + 
      scale_color_manual(values = c('blue', 'red', 'blue', 'red'), labels = c("SMCSMC Eurasian", "SMCSMC African", "MSMC Eurasian", "MSMC African")) + 
      scale_linetype_manual(values = c(1,1,2, 2), labels = c("SMCSMC Eurasian", "SMCSMC African", "MSMC Eurasian", "MSMC African")) + 
      theme_bw() + 
      theme(legend.position = "bottom", 
            legend.title = element_blank(),
            panel.grid.minor = element_blank()) 
      #legend(lty = c("solid", "solid", "dashed", "dashed"))
  } else if (type == "migration"){
    smc_df = plot(smcsmc, ylim = ylim, xlim = xlim, return_df = T)
    msmc_df = plot(msmc, ylim = ylim, xlim = xlim, return_df = T, lines = c("lambda_01"), g = g)
    p <- ggplot() + 
      geom_vline(xintercept = seq(10000,100000, by = 10000), col = "grey", alpha = 0.3) + 
      geom_vline(xintercept = seq(1e5,1e6,by=1e5), col = "grey", alpha = 0.3) + 
      geom_step(
        data = smc_df,
        aes(x = Start*g,
            y = Rate,
            group = From,
            col = From,
            linetype = From)
      ) + geom_step(
        data = msmc_df,
        aes(x = left_time_boundary,
            y = value, col = variable, linetype = variable)) +
      scale_x_log10(limits=xlim, labels = label_comma(scale = 0.001)) + 
      scale_y_continuous(limits =ylim, labels = label_comma()) +
      xlab("Thousands of Years before Present") + 
      ylab("Effective Population Size") + 
      scale_color_manual(values = c('blue', 'red', 'purple', 'red'), labels = c("SMCSMC Eurasian", "SMCSMC African", "MSMC Relative X-Coal", "MSMC African")) + 
      scale_linetype_manual(values = c(1,1,5), labels = c("SMCSMC Eurasian", "SMCSMC African", "MSMC Relative X-Coal", "MSMC African")) + 
      theme_bw() + 
      theme(legend.position = "bottom", 
            legend.title = element_blank(),
            panel.grid.minor = element_blank()) 
  }
  
  return(p)
}

#plot_psmc2(smcsmc, psmc2, ylim = c(1e3, 1e6), xlim = c(1e4, 1e6), g=29){
##  smc_df = plot(smcsmc, ylim = ylim, xlim = xlim, return_df = T, type = "ne")
#  psmc_df = plot(psmc2, ylim = ylim, xlim = xlim, return_df = T, type = "ne")
#}
  
#' @import ggalt
#' @importFrom scales label_comma
#' @importFrom dplyr group_by
#' @importFrom stringi stri_extract_all_regex
#' @export
plot_smcsmc_replicates <- function(list_of_smcsmc, ylim = c(0, 3e-4), xlim = c(1e4, 1e6), g=29){
  dfs = 0
  for(smc in list_of_smcsmc){
    seed = stri_extract_all_regex(smc@file, "[0-9]{3,}") %>% unlist
    if(!is.data.frame(dfs)){
      dfs = plot(smc, return_df = T) %>% mutate(seed = seed)
    } else {
      dfs = rbind(dfs, plot(smc, return_df = T) %>% mutate(seed = seed)) 
    }
  }
  
  summary <- dfs %>%
    group_by(Start, From) %>%
    summarise(mean = mean(Rate),
              sd = sd(Rate))
  
  ggplot(data = summary, aes(x = Start*g,  fill = From, y = mean, ymin=mean-sd, ymax=mean+sd)) + 
    geom_step(aes(col = From)) +
    geom_ribbon(stat="stepribbon",
                alpha = 0.3) + 
    scale_y_continuous(limits = ylim, 
                       labels = label_comma()) +
    scale_x_log10(limits = xlim,
                  labels = label_comma(scale = 0.001)) + 
    scale_color_manual(values = c('blue', 'red', 'black'), labels = c("Afr to Eur", "Eur to Afr")) +
    scale_fill_manual(values = c('blue', 'red', 'black'), labels = c("Afr to Eur", "Eur to Afr")) +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank(), 
          panel.grid = element_blank())
}
