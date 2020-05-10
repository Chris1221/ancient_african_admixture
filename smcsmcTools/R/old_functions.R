make_gif <- function(file, outfile, ylim = c(0, 3.5e-4)){
  
  df <- data.table::fread(file, h = T) %>% 
    dplyr::filter(Type == "Migr") %>% 
    dplyr::filter(Iter > 0) %>%
    dplyr::filter(Start > 0)
  
  system("mkdir -p tmp")
  
  for (i in 1:max(df$Iter)) {
    ggplot(data = df %>% filter(Iter == i), 
           aes(x = Start*29, 
               y = Rate,
               col = From,
               group = factor(From))) + 
      geom_step() + 
      scale_x_log10() +
      ylim(ylim) +
      annotate("label", label = paste0("Iteration ", i), x = 1e6, y = 3e-4)
    ggsave(paste0("tmp/tmp", i, ".png"))
  }
  
  system(paste0("convert -delay 10 ", paste0("tmp/tmp", c(1:50), ".png", collapse = ' ' ), " ", outfile))
  system("rm -rf tmp")
}

#' Plot migration between two groups
#'
#' Alternatively, plot migration between two sets of groups, 
#' and average over both. This gives a geom_smooth with an 
#' error measurement. This is useful for larger comparissons. 
#' 
#' @param out Either i. A character path to an out file or ii. A character vector of paths.
#' @param pop0 An optional label for Pop0. Defaults to 'Pop0'.
#' @param pop1 An optional label for Pop1. Defaults to 'Pop1'
#' @export
plot_migration_gif <- function(
  out, 
  pop0 = "Pop1", 
  pop1 = "Pop2",
  main = "", 
  plot_diff = F,
  plotly = F,
  ylim = c(0,2.5e-4),
  xlim = c(1e3, 1e4, 1e5, 1e6),
  labels = c("1", "10", "100","1000"),
  t0 = 500,
  t1 = 1500000) {
  
  if(length(out) == 1){ 
    
    df <- data.table::fread(out, h = T) %>% 
      dplyr::filter(Type == "Migr") %>% 
      dplyr::filter(Iter > 0) %>%
      dplyr::filter(Start > 0)
    
    zero_to_one <- df %>% 
      dplyr::filter(From == 0) %>% 
      dplyr::select(Epoch,Iter, Start,End, Count, Rate, From)
    one_to_zero <- df %>% 
      dplyr::filter(From == 1) %>% 
      dplyr::select(Epoch,Iter, Start, End, Count, Rate, From) 
    
    if(plot_diff){
      
      new <- zero_to_one %>% filter(Epoch < 18)
      new$Rate <- abs(zero_to_one$Rate[zero_to_one$Epoch < 18]  - one_to_zero$Rate)
      
      ggplot() +
        geom_step(data = new,
                  aes(
                    x = Start * 29,
                    y = Rate
                  ), 
                  colour = "#002147")+
        scale_x_log10(limits=c(10000,2e5),
                      breaks=c(10000,30000, 50000 , 70000, 100000, 200000), 
                      labels=c("10","30", "50", "70", "100", "200")
        ) +
        ggtitle(main) + 
        xlab("Time (kya)") + 
        ylab("Migration Events per year")
      
      
    } else {
      df$From <- as.numeric(df$From)
      
      
      new <- data.frame(
        x.1 = one_to_zero$Start * 29,
        y.1 = one_to_zero$Rate,
        x.2 = zero_to_one$Start * 29,
        y.2 = zero_to_one$Rate,
        Iter = one_to_zero$Iter
      )
      
      ggplot(data = new) +
        geom_step(aes(x = x.1, y = y.1)) +
        geom_step(aes(x = x.2, y = y.2)) + scale_x_log10() + ylim(ylim) +
        transition_states(Iter) +
        ease_aes('linear') +
        scale_x_log10(limits=c(min(xlim),max(xlim)),breaks=xlim, labels = labels) +
        scale_y_continuous(limits = ylim) +
        ggtitle(main) + 
        xlab("Time (years)") + 
        ylab("Migration Events per year")
      
      ggplot(data = df, 
             aes(x = Start*29, 
                 y = Rate)) + 
        geom_step() + 
        #     shadow_wake(0.5) +
        scale_x_log10() +
        ylim(ylim) +
        transition_time(Iter)
    }
    
    # This is still implimented but I don't think we want to use it.
  } else if(length(out) > 1){  # Need to average everything
    
    zeros_to_ones <- lapply(out, function(path){
      df <- data.table::fread (path, h = T) %>% filter(Type == "Migr") %>% filter(Iter == max(Iter))
      zero_to_one <- df %>% filter(From == 0) %>% dplyr::select(Epoch, Start,End, Count, Rate, From) %>% mutate(diff = End - Start)
      return(zero_to_one)
    })
    
    ones_to_zeros <- lapply(out, function(path){
      df <- data.table::fread (path, h = T) %>% filter(Type == "Migr") %>% filter(Iter == max(Iter))
      one_to_zero <- df %>% filter(From == 1) %>% dplyr::select(Epoch, Start,End, Count, Rate, From) %>% mutate(diff = End - Start) %>% filter(Epoch < 18)
      return(one_to_zero)
    })
    
    # take the first one as a template
    # 
    zero_to_one <- zeros_to_ones[[2]][-32,]
    one_to_zero <- ones_to_zeros[[2]][-32,]
    
    m0 <- vector()
    s0 <- vector()
    for(i in 1:nrow(zero_to_one)){
      mean = mean(unlist(lapply(zeros_to_ones, function(df) df[i, "Rate"])))
      sd = sd(unlist(lapply(zeros_to_ones, function(df) df[i, "Rate"])))
      
      m0 <- c(m0, mean)
      s0 <- c(s0, sd)
    }
    
    m1 <- vector()
    s1 <- vector()
    for(i in 1:nrow(one_to_zero)){
      mean = mean(unlist(lapply(ones_to_zeros, function(df) df[i, "Rate"])))
      sd = sd(unlist(lapply(ones_to_zeros, function(df) df[i, "Rate"])))
      
      m1 <- c(m1, mean)
      s1 <- c(s1, sd)
    }
    
    zero_to_one$mean <- m0
    zero_to_one$sd <- s0
    one_to_zero$mean <- m1
    one_to_zero$sd <- s1
    
    if(plot_diff){
      
      new <- zero_to_one %>% filter(Epoch < 18)
      new$mean <- abs(zero_to_one$Rate[zero_to_one$Epoch < 18] - one_to_zero$Rate)
      new$sd <- sqrt(zero_to_one$sd[zero_to_one$Epoch < 18]^2 + one_to_zero$sd^2 )
      new$low <- (new$mean - new$sd)*new$diff*29
      new$low[new$low < 0] <- 0
      
      ggplot() + 
        geom_step(data = new,
                  aes(
                    x = Start * 29,
                    y = mean
                  ), 
                  colour = "#002147") +
        geom_ribbon(data = new,
                    aes(
                      x = Start * 29,
                      ymin = low,
                      ymax = (mean + sd)*diff*29
                    ),
                    stat="stepribbon",
                    fill = "#002147",
                    alpha = 0.25
        ) +
        scale_x_log10(limits=c(1000, 10000,2e5),
                      breaks=c(1000, 10000,30000, 50000 , 70000, 100000, 200000), 
                      labels=c("1","10","30", "50", "70", "100", "200")
        ) +
        ylim(ylim[1], ylim[2]) +
        theme_minimal() +
        ggtitle(main) + 
        xlab("Time (kya)") + 
        ylab("Migration Events per year")
      
    } else{
      
      ggplot() + 
        geom_step(data = zero_to_one,
                  aes(
                    x = Start * 29,
                    y = mean
                  ), 
                  colour = "#002147") +
        geom_ribbon(data = zero_to_one,
                    aes(
                      x = Start * 29,
                      ymin = (mean - sd),
                      ymax = (mean + sd)
                    ),
                    stat="stepribbon",
                    fill = "#002147",
                    alpha = 0.25
        ) +
        geom_step(data = one_to_zero,
                  aes(
                    x = Start * 29,
                    y = mean
                  ),
                  colour = "green") +
        geom_ribbon(data = one_to_zero,
                    aes(
                      x = Start * 29,
                      ymin = (mean - sd),
                      ymax = (mean + sd)
                    ),
                    stat="stepribbon",
                    fill = "green",
                    alpha = 0.25
        ) +
        scale_x_log10(limits=c(10000,2e5),breaks=c(10000,50000, 70000,100000, 200000), labels=c("10", "50", "70", "100",  "200")) +
        theme_minimal() +
        ggtitle(main) + 
        xlab("Time (kya)") + 
        ylab("Migration Events per year")
    }
    #ggplotly(p)
    
  } 
}

#' Plot effective population size over time.
#'
#' @param out Either i. A character path to an out file or ii. A character vector of paths.
#' @param pop0 An optional label for Pop0. Defaults to 'Pop0'.
#' @param pop1 An optional label for Pop1. Defaults to 'Pop1'
#' @export
plot_ne <- function(
  out, 
  pop0 = "Pop1", 
  pop1 = "Pop2",
  main = "", 
  plot_diff = F,
  plotly = F,
  xlim = c(1e3, 1e4, 1e5, 1e6),
  labels = c("1", "10", "100","1000"),
  ylim = c(6.5,12), 
  t0 = 1000,
  t1 = 1500000, 
  return = F, 
  previous = NULL) {
  
  g = 29
  
  df <- data.table::fread (out, h = T) %>% 
    filter(Type == "Coal") %>% 
    filter(Iter == max(Iter)) %>% 
    filter(Clump== -1)
  
  # If you want to add on to a previous plot
  if (!is.null(previous)){
    p <- previous 
  } else {
    p <- ggplot()
  }
  
  
  df$From <- as.factor(df$From)
  p <- p + 
    geom_step(
      data = df,
      aes(x = Start*g, 
          y = log(Ne), 
          group = From, 
          col = From)) + 
    scale_x_log10(limits=c(min(xlim),max(xlim)),breaks=xlim, labels = labels) + 
    scale_y_continuous(limits = ylim) +
    xlab("Time (thousands of years)") + 
    ylab("Mij (% Population / Generation)") + 
    scale_color_manual(values = c('#002147', 'green', 'blue', 'black'), labels = c("YRI to CEU", "CEU to YRI")) + 
    theme(legend.position = "bottom") 
  
  if (return){
    return (p)
  }
}
