#' @title A Subset of the Ames Data Set with Imputed Values Only Including Factor Variables and Sale Price
#' @description A randomly selected subset of the Ames data set.
#' The dataset contains only factor class variables and the SalePrice variable.
#' Missing values have been imputed.
#' @name AmesImpFctrs
#' @docType data
#' @usage AmesImpFctrs
#' @format A data frame with 2049 observations on 39 variables.
#' @keywords datasets
#' @source \url{https://ww2.amstat.org/publications/jse/v19n3/Decock/DataDocumentation.txt}
#' @examples
#' str(AmesImpFctrs)
#' plot(AmesImpFctrs$Neighborhood, y = AmesImpFctrs$SalePrice)
NULL
