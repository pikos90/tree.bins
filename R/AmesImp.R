#' @title A Subset of the Ames Data Set with Imputed Values
#' @description A randomly selected subset of the Ames data set.
#' The dataset has had its values imputed and any remaining NA values removed.
#' @name AmesImp
#' @docType data
#' @usage AmesImp
#' @format A data frame with 2047 observations on 74 variables.
#' @keywords datasets
#' @source \url{https://ww2.amstat.org/publications/jse/v19n3/Decock/DataDocumentation.txt}
#' @examples
#' str(AmesImp)
#' plot(AmesImp$Neighborhood, y = AmesImp$SalePrice)
NULL
