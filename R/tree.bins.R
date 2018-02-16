#' @title Recategorization of Factor Variables by Decision Tree Leaves
#' @description The function takes in a data set that contains categorical variable(s) and a response variable.
#' It creates a decision tree by using one of the categorical variables (class factor) and the response variable.
#' The decision tree is created from the rpart function from the rpart library.
#' The rules from the leaves of the decision tree are extracted, and used to recategorize the appropriate categorical variable (predictor).
#' This step is performed for each of the categorical (class factor) variables that is fed into the data component of the function.
#' Only variables containing more than 2 factors will be considered in the function.
#' The final output generates a data set containing the recategorized variables or a list containing a mapping table
#' for each of the candidate variables.
#' @param data A data.frame.
#' @param y The response variables to be used in the rpart function.
#' @param bin.nm The string that will be used to categorize the variables. The default "Group." will be assigned.
#' E.g. If a variable of 6 factors is recategorized into 3 factors, then setting bin.name equal to "Group." will
#' name the three new factors to "Group.1", "Group.2", and "Group.3"
#' @param method This is the method that will be used in the rpart function.
#' If null, the default method will be used. See rpart() for further detail.
#' @param control This is the control that will be used in the rpart function. See rpart() and rpart.control() for further detail.
#' @param return This is what the function will return. There are two options:
#' 1) lkup.list - will provide a list of lookup tables. Each element will contain the original to new mapping for each recategorized variable.
#' 2) new.fctrs - will provide a data.frame with the recategorized categorical variables.
#' @export
#' @import dplyr
#' @importFrom rpart rpart
#' @importFrom rpart rpart.control
#' @importFrom rpart.utils rpart.lists
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @keywords rpart, factor, relevel
#' @seealso \code{\link[tree.bins]{bin.oth}}, \code{\link[rpart]{rpart}}, \code{\link[rpart]{rpart.control}},\code{\link[rpart.utils]{rpart.lists}}
#' @return NULL
#' @examples
#' #Returns a data.frame of recategorized variables
#' library(rpart)
#' sample.df <- AmesImpFctrs[, c("Neighborhood", "MS.Zoning", "SalePrice")]
#' tree.bins(data = sample.df, y = SalePrice)
#'
#' #Returns a list of mapping tables generated from tree.bins()
#' tree.bins(data = sample.df, y = SalePrice, return = "lkup.list")
#'
#' #Allows the user to choose the naming convention for the attribute naming convention
#' tree.bins(data = sample.df, y = SalePrice, bin.nm = "bin#")
#'
#' #Allows user to manually assign a cp to each decision tree evaluated using rpart()
#' tree.bins(data = sample.df, y = SalePrice, control = rpart.control(cp = .01))

tree.bins <- function(data, y, bin.nm = "Group.",
                      method = NULL, control = NULL, return = "new.fctrs") {

  #Allowing y to pass in to the rpart function.
  colnm <- eval(substitute(y),data, parent.frame())

  #Placing the y factor to the end of the table so it will not be valuated.
  only.y <- data[ ,deparse(substitute(y)), drop = FALSE]

  if(dim(data)[2] == 2) {

    only.fctrs <- data[ , !(names(data) %in% names(only.y)), drop = FALSE] %>% as.data.frame()

  }else{

    only.fctrs <- data[ , !(names(data) %in% names(only.y))] %>% as.data.frame()

  }

  #Removing non-factor variables.
  only.fctrs.final = select_if(only.fctrs, is.factor)

  #Joining only factor variables and the response.
  #The response will be the last variable of the data.frame.
  df = cbind(only.fctrs.final, only.y)

  if (exists("new.fctrs", inherits = TRUE)) {
    rm(new.fctrs, envir = environment(), inherits = TRUE)
    #rm(new.fctrs)
  }

  #Initiating a list variable for a loop to pass through and store lookup tables.
  lkup.list <- list()

  #Creating for loop to create a decision tree for each variable.
  #Subtracting 1 so it will not evaluate the last variable => y the predictor.
  for (i in 1:(dim(df)[2]-1)) {

    #Making sure to remove any object that is final.groups
    #Final.groups is generated running the for loop, so need to remove this
    #at the beginning of each for loop. Final.groups is stored into a list
    #at the end of each loops cycle.
    if (exists("final.groups", inherits = TRUE)) {
      rm(final.groups, envir = environment(), inherits = TRUE)
      #rm(final.groups, envir = .GlobalEnv)
    }

    #Assessing which type of rpart part to run based off of user inputs.
    if (is.null(method) & is.null(control)) {

      tree = rpart(formula = colnm ~ df[,i], data = df)
      tree.rules = unlist(rpart.lists(tree))

    } else  if (!is.null(method) & is.null(control)){

      tree = rpart(formula = colnm ~ df[,i], data = df, method = method)
      tree.rules = unlist(rpart.lists(tree))

    } else  if (is.null(method) & !is.null(control)){

      tree = rpart(formula = colnm ~ df[,i], data = df, control = control)
      tree.rules = unlist(rpart.lists(tree))

    } else  if (!is.null(method) & !is.null(control)){

      tree = rpart(formula = colnm ~ df[,i], data = df, method = method, control = control)
      tree.rules = unlist(rpart.lists(tree))

    }

    #Assessing criteria on whether or not a particular variable meets the criteria for recategorization.
    #The variable must be a factor, cannot have only 2 levels, and must contain leaves. Variables with
    #only 1 level will be caught by the "must contain leaves" rule.
    if (is.factor(df[,i]) & length(levels(df[, i])) != 2 & !is.null(c(names(tree.rules)))) {

      #Creating a df of the tree.rules
      tree.df <- data_frame(Names = c(names(tree.rules)),
                            Values = unlist(tree.rules),
                            #LR = ifelse(grepl("R.", Names), "R.", "L."),  # Not needed for code to run
                            #VarName = paste(names(df[,i, drop = FALSE])), # Not needed for code ro run
                            Number = ifelse(sub(".*]", "", Names) != "", sub(".*]", "", Names), 0) %>% as.integer())

      #Creating an identifier for each split. "End.Group" is where the group will end.
      tree.df.v2 <- tree.df %>%
        mutate(Cuts = ifelse(is.na(lag(Number)) | lag(Number) < Number, "Continue", "End.Group"),
               Cuts = lead(Cuts),
               Cuts = ifelse(Number == 0 | is.na(Cuts), "End.Group", Cuts),
               Index = seq(1:nrow(tree.df)))

      #Extracting the Index number for each End.Group to start creating character arrays.
      tree.df.v3 <- tree.df.v2 %>%
        filter(Cuts == "End.Group")

      #Creating subsets of tree.df.v2 through the Index column and creating
      #combined character string vectors for each group to store in a list
      #(Group defined by beginning after an "End.Group" in the "Cuts" column).

      #Initiating a list variable for a loop to pass through and string vectors.
      string.list <- list()
      for (j in 1:length(tree.df.v3$Index)) {

        if(j ==1) {

          strings = data.frame(Values = c(tree.df.v2[j:tree.df.v3$Index[j],]$Values),
                               Strings = c(tree.df.v2[j:tree.df.v3$Index[j],]$Cuts),
                               Index = tree.df.v3$Index[j], stringsAsFactors = FALSE)

        } else{

          strings = data.frame(Values = c(tree.df.v2[(tree.df.v3$Index[j - 1] + 1):tree.df.v3$Index[j],]$Values),
                               Strings = c(tree.df.v2[(tree.df.v3$Index[j - 1] + 1):tree.df.v3$Index[j],]$Cuts),
                               Index = tree.df.v3$Index[j], stringsAsFactors = FALSE)

        }

        string.list[[j]] <- strings

      }

      #Looping for each element of the list through each of the other element
      #in the list to identify final groupings

      for (k in 1:length(string.list)) {

        # string.list[[4]][, 1] %in% string.list[[1]][, 1]
        #
        # sum((string.list[[1]][, 1] %in% string.list[[4]][, 1]) == TRUE) >= 1


        #Create list to store checks
        check.list <- list()
        #Loops through
        for (m in 1:length(string.list)) {

          #Checks to make sure the "Value" column of the kth element of the list is within each
          #element of the list, IF that element contains at least one value in its "Value" column
          #that matches any one of the kth element's values from its "Value" column
          if (all((string.list[[k]][, 1] %in% string.list[[m]][, 1] ) == TRUE) &
              sum((string.list[[k]][, 1] %in% string.list[[m]][, 1]) == TRUE) >= 1) {

            check.list[[m]] <- "True"
            #Evaluates whether or not kth element vector is within the mth element. If no strings are present,
            #then it should not be considered into the True/False evaluation and returns NULL instead.
          } else if (sum((string.list[[k]][, 1] %in% string.list[[m]][, 1]) == TRUE) == 0) {

            check.list[[m]] <- "NULL"

          }else{

            check.list[[m]] <- "FALSE"
          }

        }

        #Remove NULL values from the list so it will not be considered for True/False evaluation
        list.no.null = check.list[!check.list == "NULL"]

        #Creates a data.frame that indicates whether the string is a group or not
        if (all(list.no.null == "True")) {

          string.list[[k]] <- cbind(string.list[[k]],
                                    data.frame(GroupOrNot = rep("Yes", times = nrow(string.list[[k]])), stringsAsFactors = FALSE))
        } else{

          string.list[[k]] <- cbind(string.list[[k]],
                                    data.frame(GroupOrNot = rep("No", times = nrow(string.list[[k]])), stringsAsFactors = FALSE))

        }

      }

      #Checking for strings that should be grouped and overwritting those that shouldn't with NULL
      for (n in 1:length(string.list)) {

        if(all(string.list[[n]]$GroupOrNot == "No")){
          string.list[[n]] <- "NULL"
        }

      }

      #Remove NULL values from the list so it will not be considered in the final groupings
      final.string.list = string.list[!string.list == "NULL"]

      #Creating category names
      for (p in 1:length(final.string.list)) {
        final.string.list[[p]] <- final.string.list[[p]] %>%
          mutate(Categories = paste0(bin.nm, p))
      }

      #Converting list into a data.frame
      for (q in 1:length(final.string.list)) {

        if (exists("final.groups")){
          final.groups <- rbind(final.groups, final.string.list[[q]])
        }

        if (!exists("final.groups")){
          final.groups <- rbind(final.string.list[[q]])
        }

      }

      #Creating a lookup data.frames to save as a list to reference lookup tables.
      lkup.df <- final.groups %>% select(Values, Categories)
      #Create variables to assign and filter data.frames by.
      nm = names(df[i])
      #Correctly name the lookup data.frames
      colnames(lkup.df) = c(nm, "Categories")
      #Store the values of each data.frame into a list.
      lkup.list[[i]] <- lkup.df

      #Create a holder for data to avoid manipulation of the data variable.
      #Will be used to convert factor level variables that are considered for
      #recategorizing into character level variables.
      if(!exists("chr.fctrs")){
        chr.fctrs <- data
      }

      #Converts the considered variable into a character.
      #Left joins by matching the considered variable's manipulated
      #data.frame with the lookup table. The manipulated data.frame is created
      #and adjusted by each variable that is considered. The manipulated data.frame
      #is the final data.frame that produces the recategorized variables. The "Categories"
      #column is added onto the manipulated data.frame. The original variable
      #is removed and "Categories" is renamed into the original variable's name.
      if(exists("new.fctrs")) {

        new.fctrs <- as.data.frame(new.fctrs)
        new.fctrs[, nm] <- as.character(new.fctrs[, nm])

        join.fctrs <- new.fctrs %>% left_join(lkup.df, by = paste(nm))
        new.fctrs <- join.fctrs[ , !(names(join.fctrs) %in% nm)] %>% as.data.table() %>% setnames("Categories", paste0(nm))

      }

      #Converts the considered variable into a character.
      #Left joins by matching the considered variable's original
      #data.frame with the lookup table. The "Categories" column is
      #added onto it the original data.frame. The original variable
      #is removed and "Categories" is renamed into the original variable's name.
      if(!exists("new.fctrs")){

        chr.fctrs[,nm] <- as.character(chr.fctrs[, nm])

        join.fctrs <- chr.fctrs %>% left_join(lkup.df, by = paste(nm))
        new.fctrs <- join.fctrs[ , !(names(join.fctrs) %in% nm)] %>% as.data.table() %>% setnames("Categories", paste0(nm))

      }

    }

  }

  #Allows the user to return either the recategorized data.frame by selecting "new.fctrs"
  #or the list containing all of the lookup tables by selecting "lkup.list."
  if(return == "new.fctrs"){
    return(new.fctrs)
  } else{
    return(lkup.list)
  }

}
