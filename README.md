# Overview of dgen

* dgen, or dplyr generator, is intended to be used within the dplyr::mutate command to generate a new variable that is a combination of other variables.

# dgen functions:

## dgen::row_any_count(..., values, maxmiss_n = 0)

  * For each row, returns the count of variables in the variable list (...) equal to a specified value or values. Includes options for handling missing data.
    
## dgen::row_any_match(..., value, ignoreNA = TRUE, ignoreAllNA = FALSE)

  * Returns a 1 if at least one of variable in the variable list (...) is equal to a specified value across variables within each row. Returns a value of 1 if at least one variable equals the value; 0 or NA otherwise. Includes options for handling missing data.
    
## dgen::row_mean(..., n = 0)

  * Calculates the average across variables (columns) within each row. Includes an option for handling missing data.
    
## dgen::row_na(...)

  * Counts the number of missing values across variables (columns) within each row.
    
## dgen::row_nm(...)

  * Counts the number of non-missing values across variables (columns) within each row.

## dgen::row_sum(..., n = 0)

  * Sums values across variables (columns) within each row. Includes an option for handling missing data.

