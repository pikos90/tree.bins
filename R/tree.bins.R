#' @title <Recategorization of Factor Variables by Decision Tree Leaves>
#'
#' @description <The package takes in a data set of categorical variables and a response variable.
#' It creates a decision tree by using one of the categorical variables and the response variable.
#' The decision tree is created from the rpart function from the rpart library. The rules from the leaves of the decision tree are extracted, and used to recategorize the used variable (predictor). This step is performed for each of the categorical variables that is fed into the data component of the function. Only variables containing more than 2 factors will be considered in the function. The final output generates a data set containing the recategorized variables.>
#' @param df a data.frame
#' @param y the response variables to be used in the rpart function
#' @param bin.nm the string that will be used to categorize the variables. E.g. "Group." will be used to recategorize a variable of 6 factors to 3 factors. Will result in buckets of "Group.1", "Group.2", and "Group.3"
#' @param method this is the method that will be used in the rpart function. If null, the default method will be used. See rpart() for further detail
#' @param control this is the control that will be used in the rpart function. See rpart() for further detail.
#' @param return this is what the function will return. There are two options: 1) lkup.list - will provide a list of the factors of each categorical variable to the new factors of each variable. 2) new.fctrs - will provide a data.frame with the recategorized categorical variables
#' @export
#' @import dplyr, rpart, rpart.utils, data.table
#' @keywords rpart, factor, relevel
#' @seealso \code{\link[dplyr]{mutate}}
#' @return NULL
#' @examples \dontrun{ "asdfsdf"}


tree.bins <- function(df = fctrs, y = SalePrice, bin.nm = "Group.",
                     method = NULL, control = NULL, return = lkup.list) {

  #allowing y = SalePrice to pass in to rpart function
  colnm <- eval(substitute(y),df, parent.frame())

  #placing the y factor to the end of the table so it will not be valuated
  only.y <- df[ ,deparse(substitute(y)), drop = FALSE]

  only.fctrs <- df[ , !(names(df) %in% names(only.y))]

  only.fctrs.final = only.fctrs[,sapply(only.fctrs, class) == 'factor']

  df = cbind(only.fctrs.final, only.y)

  if (exists("new.fctrs", inherits = TRUE)) {
    rm(new.fctrs, envir = environment(), inherits = TRUE)
    #rm(new.fctrs)
  }

  lkup.list <- list()

  # minus 1 so it will not evaluate the y (predictor)
  for (i in 1:(dim(df)[2]-1)) {

    #remember to comment out i either 38 - sale.condition or 8 - neighborhood or 28 - Central.air
    if (exists("final.groups", inherits = TRUE)) {
      rm(final.groups, envir = environment(), inherits = TRUE)
      #rm(final.groups, envir = .GlobalEnv)
    }
    #i = 38

    #Assessing which type of rpart part to run based off of user inputs
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


    if (is.factor(df[,i]) & length(levels(df[, i])) != 2 & !is.null(c(names(tree.rules)))) {

      #creating a df of the tree.rules
      tree.df = data_frame(Names = c(names(tree.rules)),
                           Values = unlist(tree.rules),
                           LR = ifelse(grepl("R.", Names), "R.", "L."),
                           VarName = paste(names(fctrs[,i, drop = FALSE])),
                           Number = ifelse(sub(".*]", "", Names) != "", sub(".*]", "", Names), 0) %>% as.integer())

      #Identifying the first nodes string, so can identify which strings split left and which split right from it
      L.Numb = tree.df %>% group_by(LR) %>% filter(LR == "L.")
      max.num = max(L.Numb$Number)
      max.num.df.tree <- tree.df[1:max.num,]
      first.node.str <- c(max.num.df.tree$Values)

      #L.subset = new.tree.df %>% filter(LR == "L.")

      #creating a group for each bucket
      new.tree.df <- tree.df %>%
        mutate(LR.Orig = ifelse(Values %in% first.node.str, "L.", "R.")) %>%
        group_by(LR.Orig, LR, Values) %>%
        mutate(Repeated.Count = n(),
               Categories = ifelse(Number != 0,
                                   ifelse(LR == "L.",
                                          paste0(bin.nm, Repeated.Count),
                                          paste0(bin.nm, max(table(tree.df$Values)) + Repeated.Count)),
                                   paste0(bin.nm, Number))
        ) %>%
        ungroup()

      #*** if unique thing exists and and matches other unique thing do nothing, otherwise replace it by other value
      #e.g. df[ wehre thing equals other thing] <- overwrite value

      uniqs <- unique(new.tree.df$Values)

      #Must do different operations for a 2 factor tree vs greater than a 2 factor tree
      #rm(final.groups)
      if(max(new.tree.df$Number) != 0){

        #Look through each of the unique value of factors for the variable and assign proper groupings
        for (j in 1:length(uniqs)) {

          R.R = paste0(new.tree.df %>% filter(LR.Orig == "R." & LR == "R." & Values == uniqs[j]) %>% distinct(Categories))
          L.L = paste0(new.tree.df %>% filter(LR.Orig == "L." & LR == "L." & Values == uniqs[j]) %>% distinct(Categories))

          if(exists("final.groups")){

            final.groups <- final.groups %>%
              mutate(Categories = ifelse(LR.Orig == "R." & LR == "L." & Values == uniqs[j],
                                         R.R,
                                         ifelse(LR.Orig == "L." & LR == "R." & Values == uniqs[j],
                                                L.L,
                                                Categories))
              )
          }

          if(!exists("final.groups")){

            final.groups <- new.tree.df %>%
              mutate(Categories = ifelse(LR.Orig == "R." & LR == "L." & Values == uniqs[j],
                                         R.R,
                                         ifelse(LR.Orig == "L." & LR == "R." & Values == uniqs[j],
                                                L.L,
                                                Categories))
              )
          }

          #Keep to catch "Group0" categories for only one branch with no splits => 3 groups only
          if(exists("final.groups")) {

            if (any(is.na(final.groups$Categories))) {
              final.groups <- final.groups %>%
                mutate(Categories = ifelse(is.na(final.groups$Categories) , paste0(bin.nm, Number), Categories))
            }

          }

        }
      } else {
        #Keep to catch "Group0" categories for both nodes having no more splits => 2 groups only
        final.groups <- new.tree.df %>%
          mutate(ifelse(LR.Orig == "L.", paste0(bin.nm, Number), paste0(bin.nm, 1)))

      }


      #Creating a lookup df SAVE AS A LIST TO REFERENCE LOOKUP TABLES
      lkup.df <- final.groups[!duplicated(final.groups$Values), ] %>% select(Values, Categories)
      #need this to keep names consistent
      nm = names(fctrs[i])
      #need this for leftjoin to work
      colnames(lkup.df) = c(nm, "Categories")
      #List of lookup tables
      lkup.list[[i]] <- lkup.df

      chr.fctrs <- df

      chr.fctrs[] <- lapply(chr.fctrs, as.character)

      #Keep to catch "Group0" categories
      if(exists("new.fctrs")) {

        join.fctrs <- new.fctrs %>% left_join(lkup.df, by = paste(nm))
        new.fctrs <- join.fctrs[ , !(names(join.fctrs) %in% nm)] %>% as.data.table() %>% setnames("Categories", paste0(nm))

      }

      if(!exists("new.fctrs")){

        join.fctrs <- chr.fctrs %>% left_join(lkup.df, by = paste(nm))
        new.fctrs <- join.fctrs[ , !(names(join.fctrs) %in% nm)] %>% as.data.table() %>% setnames("Categories", paste0(nm))

      }

    }

  }

  return(return)

}








