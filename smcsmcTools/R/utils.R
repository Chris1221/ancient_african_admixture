#' @export
names_from_config <- function(table){
  names = as.data.frame(table)
  names = colnames(names)
  return ( gsub(pattern = "\\.", replacement = "-", x = names))
}

#' @export
id_to_name <- function(name, source = "sgdp"){
  if(source == "hgdp"){
    return(hgdp$population[hgdp$sample == name] %>% as.character())
  } else if(source == "sgdp"){
    id = gsub(x = name, pattern = "_|-", replacement = " ")
    return(
      strsplit(id, " ")[[1]] %>%
      head(-1) %>%
      tail(-1) %>% 
      paste(collapse = ' ')
    )
  }
}

#' @export
ids_to_names <- Vectorize(id_to_name)

#### Functions for dealing with language families.

#' @export
which_lang <- function(sample, languages = smcsmcTools::sample_languages){
  for(language in names(languages)){
    if (sample %in% unlist(languages[language])) return(language)}
  stop("Sample not found.")
}

#' @export
list_samples_from_lang <- function(lang, languages = smcsmcTools::sample_languages){
  return(as.vector(unlist(languages[lang])))
}

#' @export 
list_language_families <- function(languages = smcsmcTools::sample_languages){
  return(names(languages))
}

##### Functions for working with lots of ggplots 

#' @importFrom grid unit.pmax
#' @importFrom ggplot2 ggplotGrob
#' @export
get_width <- function(p){
  p_grob <- ggplotGrob(p)
  return(p_grob$widths)
}

#' Set the width of p according to p2
#' 
#' @importFrom ggplot2 ggplotGrob
#' @export
set_width <- function(p, p2){
  p_grob <- ggplotGrob(p)
  p2_grob <- ggplotGrob(p2)
  p_grob$widths[1:3] <- p2_grob$widths[1:3]
  return(p_grob)
}

#' Plot several ggmatrix objects
#' 
#' Arrange ggmatrix objects by retrieving their gtables
#' and manually binding them with a specified layout.
#' Not much documentation on this stuff so mostly cobbled togheter
#' from SO:
#' 
#' - https://stackoverflow.com/questions/17126128/ggplot2-does-not-appear-to-work-when-inside-a-function-r
#' - https://stackoverflow.com/questions/49439982/ggplot-and-grob-arrangement
#' - * https://stackoverflow.com/questions/24331107/the-perils-of-aligning-plots-in-ggplot
#' - * https://stackoverflow.com/questions/27361364/set-the-height-of-the-graphs-y-axis-in-grid-arrange-but-not-of-the-entire-plot
#' -  https://stackoverflow.com/questions/35068129/arrange-ggplot-plots-grobs-with-same-widths-using-gtable-to-create-2x2-layout
#'  
#' @param m List of ggmatrices
#' @param layout A table with numeric placement of plots in the grid
#' 
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' 
#' @return gtable object to be plotted with grid.draw
#' 
#' @export
arrange_ggmatrix <- function(m, lay){
  grobz <- lapply(m, ggmatrix_gtable)
  grobz.plot <- arrangeGrob( grobs = grobz,
                             layout_matrix = lay)
  
  return(grobz.plot)
  
}

###### Some misc functions for reading and writing

#' Read an SMC2 output file
#' 
#' This seems excessive for now but will be helpful
#' when I want to do more complicated things
#' @export
smcsmc <- function(file){
  s <- new("smcsmc", file = file)
  s@data <- as.data.frame(fread(file, h = T))
  return(s)
  
}

#' Read an msmc output file
#' 
#' This seems excessive for now but will be helpful
#' when I want to do more complicated things
#' @export
msmc <- function(file){
  m <- new("msmc", file = file)
  m@data <- as.data.frame(fread(file))
  return(m)
  
}
