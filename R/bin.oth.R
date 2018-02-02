bin.oth <- function(list = a, df = test.na.rm) {

  list.rm.null = list[-which(sapply(list, is.null))]

  if (exists("test.fctrs", inherits = TRUE)) {
    rm(test.fctrs, envir = environment(), inherits = TRUE)
  }

  for (i in 1:length(list.rm.null)) {

    #i = 2

    #Creating a lookup df SAVE AS A LIST TO REFERENCE LOOKUP TABLES
    lkup.tbl <- list.rm.null[[i]] %>% as.data.frame()
    nm1 <- names(lkup.tbl)[1]
    nm2 <- names(lkup.tbl)[2]

    #Keep to catch "Group0" categories
    if(exists("test.fctrs")) {

      join.test.fctrs <- test.fctrs %>% left_join(lkup.tbl, by = paste(nm1))
      test.fctrs <- join.test.fctrs[ , !(names(join.test.fctrs) %in% nm1)] %>% as.data.table() %>% setnames(nm2, paste0(nm1))

    }

    if(!exists("test.fctrs")){

      join.test.fctrs <- df %>% left_join(lkup.tbl, by = paste(nm1))
      test.fctrs <- join.test.fctrs[ , !(names(join.test.fctrs) %in% nm1)] %>% as.data.table() %>% setnames(nm2, paste0(nm1))

    }

  }

  return(test.fctrs)

}
