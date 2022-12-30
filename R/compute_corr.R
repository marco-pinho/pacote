#' Computa correlacao tidy
#'
#' @param data dados de entrada
#' @param var1 nome da variavel 1
#' @param var2 nome da variavel 2
#'
#' @return Uma tibble com a correlacao de  Pearson  e o p-valor
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' compute_corr(data = faithful, var1 = eruptions, var2 = waiting)


compute_corr <- function(data, var1, var2){
  
  # compute correlation ----
  
  stats::cor.test(
    x = data %>% dplyr::pull({{var1}}),
    y = data %>% dplyr::pull({{var2}})
  ) %>% 
    # tidy up results ----
  broom::tidy() %>% 
    # retain and rename relevant bits ----
  dplyr::select(
    correlation = .data$estimate, 
    pval = .data$p.value
  )
  
}