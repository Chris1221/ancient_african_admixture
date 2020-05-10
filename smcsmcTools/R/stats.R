#' Find the average migration during a period
#'
#' Take the sum of rate over given epochs and average by the number of generations. 
#' @param file Path to the output file containing the migration information.
#' @param ancient More ancient (further from present) boundary of the period.
#' @param modern More modern (closer to present) boundary of the period. 
#' @param g Generation time in years. 
#' 
#' @return A vector of average weighted mijs for each population. (Unsafe)
#' 
#' @export
#
avg_migr <- function(
  file,
  ancient,
  modern,
  g
){
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


