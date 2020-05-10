#' @exportClass msmc 
setClass("msmc",
         representation(file = "character",
                        data = "data.frame"))

#' @exportClass smcsmc
setClass("smcsmc",
         representation(file = "character",
                        data = "data.frame"))

setMethod("show", signature("smcsmc"), function(object) {
  print("This is an SMC2 output file.")
})


#' Retrieve population size.
#' 
#' Fetches effective population size estimates at a particular time. Dispatches to 
#' MSMC and SMC2 specific functions. 
#' 
#' @param time Time, in years, to retrieve
#' @param g Generation time in years
#' @param mu Mutation rate for MSMC scaling
#' 
#' @return Vector of population sizes for each population, in the same order as the labels.
ne_at <- function(object, ...) 0

#' @describeIn ne_at SMC2 implementation
#' @importFrom data.table fread
#' @importFrom dplyr %>%
#' @importFrom dplyr filter select
#' @export 
setMethod("ne_at", signature("smcsmc"), function(object, time, g = 29){
  ne <- fread(object@file, h = T) %>% 
    filter(Iter == max(Iter)) %>%
    filter(Start <= time/g) %>%
    filter(Start == max(Start)) %>%
    filter(Type == "Coal")
    
  out <- vector()
  npops = length(levels(factor(ne$From)))
  pops = levels(factor(ne$From))
  for(pop in c(1:npops)){
    out[pop] <- ne$Ne[ne$From == pops[pop]] 
  }

  return(out)
  
})

#' @describeIn ne_at MSMC implementation
#' @importFrom dplyr mutate
#' @importFrom magrittr %<>%
#' @importFrom data.table fread
setMethod("ne_at", signature("msmc"), function(object, time, g = 29, mu = 1.25e-8){
  df = fread(object@file)
  df %<>% 
    mutate(left_time_boundary = (left_time_boundary / mu) * g) %>%
    mutate(right_time_boundary = (right_time_boundary / mu) * g) %>%
    mutate(lambda_00 = (1 / lambda_00) / (2*mu)) %>% 
    mutate(lambda_11 = (1 / lambda_11) / (2*mu))
  
  ne <- df %>% 
    filter(left_time_boundary <= time) %>%
    filter(left_time_boundary == max(left_time_boundary)) %>%
    select(lambda_00, lambda_11) %>%
    unlist %>%
    as.vector
  
  return(ne)
  
})