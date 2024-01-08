#' Computes a tidy correlation
#'
#' @param input data set 
#' @param name of variable 1 
#' @param name of variable 2 
#'
#' @return A tibble with the Pearson correlation and the p-value
#' @export
#'
#' @examples compute_corr(data = faithful, var1 = eruptions, var2 = waiting)
compute_corr <- function(data, 
                         var1, 
                         var2){
  
  results <- stats::cor.test(x = data %>% dplyr::pull({{var1}}),
                             y = data %>% dplyr::pull({{var2}})) %>% 
             broom::tidy()%>% 
                  dplyr::select(correlation = estimate, 
                                pval = p.value)
  
  return(results)
  
}