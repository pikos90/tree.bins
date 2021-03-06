[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tree.bins)](https://cran.r-project.org/package=tree.bins)
[![Downloads](http://cranlogs.r-pkg.org/badges/tree.bins)](http://cran.rstudio.com/package=tree.bins)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/tree.bins)](http://cran.rstudio.com/package=tree.bins)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
--------

The package 'tree.bins' provides users the ability to recategorize categorical variables dependent on a response variable by iteratively creating a decision tree for each of the categorical variables (class factor) and the selected response variable. The decision tree is created from the rpart() function from the 'rpart' package. The rules from the leaves of the decision tree are extracted, and used to recategorize (bin) the appropriate categorical variable (predictor). This step is performed for each of the categorical variables that is passed onto the data component of the function. Only variables containing more than 2 factor levels will be considered in the function. The final output generates a data set containing the recategorized variables and/or a list containing a mapping table for each of the candidate variables. For more details see Dr. Yan-yan Song article (<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4466856/>) or T. Hastie et al (2009, ISBN: 978-0-387-84857-0). For detailed examples and functionality see vignettes.

Installation
------------

You can install tree.bins with:

``` r
#Easiest way to install tree.bins is by:
install.packages("tree.bins")

#Alternatively, the development version from GitHub:
# install.packages("devtools")
devtools::install_github("pikos90/tree.bins")
```

Usage
-----

Uses tree.bins() to recategorize your data.

``` r
## basic example code
sample.df <- AmesImpFctrs[, c("Neighborhood","MS.Zoning", "SalePrice" )]
recategorized.df <- tree.bins(data = sample.df, y = SalePrice)
head(recategorized.df)
#>    SalePrice Neighborhood MS.Zoning
#> 1:     105.0      Group.4   Group.1
#> 2:     244.0      Group.4   Group.2
#> 3:     189.9      Group.3   Group.2
#> 4:     195.5      Group.3   Group.2
#> 5:     191.5      Group.5   Group.2
#> 6:     236.5      Group.5   Group.2
```

Uses tree.bins() to create a list of mapping tables.

``` r
## basic example code
sample.df <- AmesImpFctrs[, c("Neighborhood","MS.Zoning", "SalePrice" )]
recategorized.list <- tree.bins(data = sample.df, y = SalePrice, return = "lkup.list")
head(recategorized.list[[1]])
#>   Neighborhood Categories
#> 1       BrDale    Group.1
#> 2      BrkSide    Group.1
#> 3       IDOTRR    Group.1
#> 4      MeadowV    Group.1
#> 5      OldTown    Group.1
#> 6      Somerst    Group.2
```

Use that list to recategorize your a different data set with bin.oth().

``` r
other.sample.df <- AmesImpFctrs[, c("Neighborhood","MS.Zoning", "Sale.Condition", "SalePrice" )]
other.df <- bin.oth(list = recategorized.list, data = other.sample.df)
head(other.df)
#>    Sale.Condition SalePrice Neighborhood MS.Zoning
#> 1:         Normal     105.0      Group.4   Group.1
#> 2:         Normal     244.0      Group.4   Group.2
#> 3:         Normal     189.9      Group.3   Group.2
#> 4:         Normal     195.5      Group.3   Group.2
#> 5:         Normal     191.5      Group.5   Group.2
#> 6:         Normal     236.5      Group.5   Group.2
```
