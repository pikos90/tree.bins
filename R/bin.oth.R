#' @title Recategorization of Variables by Mapping Tables Within a List
#' @description The functions purpose is to recategorize a data.frame's variables
#' by the elements identified in a list. Each element of the list must contain two columns.
#' The first column contains the original values, and the second column contains the new values.
#' The first column name of each element of the list must be a variable name in the data.frame.
#' Effectively, each element of the list is a mapping table. The list generated from the tree.bins()
#' function can be directly passed as an element to this function.
#' @param list A list generated from the tree.bins function or created by the user to the specifications laid out in the description.
#' @param data A data.frame.
#' @export
#' @import dplyr
#' @importFrom  data.table as.data.table
#' @importFrom  data.table setnames
#' @keywords rpart factor relevel
#' @seealso \code{\link[tree.bins]{tree.bins}}, \code{\link[forcats]{fct_relevel}}, \code{\link[base]{factor}}, \code{\link[dplyr]{left_join}}
#' @return NULL
#' @examples
#' #Allows the user to generate a list from the tree.bins() function
#' library(dplyr)
#' sample.df <- AmesImpFctrs %>% select(Neighborhood, MS.Zoning, SalePrice)
#' lookup.list <- tree.bins(data = sample.df, y = SalePrice, return = "lkup.list")
#'
#' #Create a new data.frame and use the created list to map recategorize its values
#' new.df <- AmesImpFctrs %>% select(Neighborhood, MS.Zoning, Lot.Shape, BsmtFin.Type.1, SalePrice) %>% head(100)
#' oth.binned.df <- bin.oth(list = lookup.list, data = new.df)

bin.oth <- function(list, data) {

  #Removes null elements of list.
  list.rm.null = list[!sapply(list, is.null)]

  #Removes "test.fctrs."
  if (exists("test.fctrs", inherits = TRUE)) {
    rm(test.fctrs, envir = environment(), inherits = TRUE)
  }

  #loop through each element of the list to extract lookup tables and map values.
  for (i in 1:length(list.rm.null)) {

    #Extract a lookup table
    lkup.tbl <- list.rm.null[[i]] %>% as.data.frame()
    #Extract the first name of the lookup table.
    nm1 <- names(lkup.tbl)[1]
    #Extract the second name of the lookup table.
    nm2 <- names(lkup.tbl)[2]
    #Assign data to df to avoid manipulating the original data set.
    df <- data

    #Converts the considered variable into a character.
    #Left joins by matching the considered variable's manipulated
    #data.frame with the lookup table. The manipulated data.frame is created
    #and adjusted by each variable that is considered. The manipulated data.frame
    #is the final data.frame that produces the recategorized variables. The "Categories"
    #column is added onto the manipulated data.frame. The original variable
    #is removed and "Categories" is renamed into the original variable's name.
    if(exists("test.fctrs")) {

      test.fctrs <- as.data.frame(test.fctrs)
      test.fctrs[,nm1] <- as.character(test.fctrs[, nm1])

      join.test.fctrs <- test.fctrs %>% left_join(lkup.tbl, by = paste(nm1))
      test.fctrs <- join.test.fctrs[ , !(names(join.test.fctrs) %in% nm1)] %>% as.data.table() %>% setnames(nm2, paste0(nm1))

    }

    #Converts the considered variable into a character.
    #Left joins by matching the considered variable's original
    #data.frame with the lookup table. The "Categories" column is
    #added onto it the original data.frame. The original variable
    #is removed and "Categories" is renamed into the original variable's name.
    if(!exists("test.fctrs")){

      df[,nm1] <- as.character(df[, nm1])

      join.test.fctrs <- df %>% left_join(lkup.tbl, by = paste(nm1))
      test.fctrs <- join.test.fctrs[ , !(names(join.test.fctrs) %in% nm1)] %>% as.data.table() %>% setnames(nm2, paste0(nm1))

    }

  }

  #Returns the data.frame of recategorized variable.
  return(test.fctrs)

}
