#' Row Sum
#'
#' Sums values across variables (columns) within each row. Includes an option for handling missing data.
#'
#' @param ... Variables to be summed (no limit on number of variables, but must be numeric).
#' @param n Maximum number of non-missing values allowed in order to compute a sum; default is 0. Missing values are treated as 0.
#'
#' @return Vector of length of each input variable.
#'
#' @export




row_sum = function(..., n = 0){

  x = ifelse(rowSums(is.na(cbind(...))) <= n,
             rowSums(cbind(...), na.rm = TRUE),
             NA)
  x

}


#' Row Missing
#'
#' Counts the number of missing values across variables (columns) within each row.
#'
#' @param ... Variables over which to count missing values (no limit on number of variables)
#' @return Vector of length of each input variable
#'
#' @export

row_na =  function(...){

  x = rowSums(is.na(cbind(...)))

  return(x)

}



#' Row Non-Missing
#'
#' Counts the number of non-missing values across variables (columns) within each row.
#'
#' @param ... Variables over which to count non-missing values (no limit on number of variables)
#' @return Vector of length of each input variable
#'
#' @export

row_nm =  function(...){

  x = rowSums(!is.na(cbind(...)))

  return(x)

}


#' Row Any Count
#'
#' Counts the number of variables (columns) equal to a specified value across variables within each row.
#'
#' @param ... Variables over which to count non-missing values (no limit on number of variables)
#' @param value Specified value to count; can be numeric or nominal or character
#' @param ignoreNA Indicates whether missing values should be
#' ignored while counting the number of specified values; default is ignoreNA = TRUE. However,
#' if all variables are missing, then a count is not computed, but is NA.
#' If ignoreNA = TRUE, then columns with a missing value are ignored in each row as long
#' as at least one variable is non-missing.
#' If ignoreNA = FALSE, then a count is not computed regardless of the number of missing variables,
#' but is NA.
#' @param ignoreAllNA When TRUE, indicates that a count should be computed even if all
#' variables are missing. Default is FALSE. If ignoreAllNA is TRUE, then ignoreNA is assumed
#' to be and must also be TRUE.
#' @return Vector of length of each input variable
#'
#' @export

row_any_count =  function(..., value, ignoreNA = TRUE, ignoreAllNA = FALSE){

  df = (data.frame(...))

  nmiss = rowSums(is.na(df))
  nvars = NCOL(df)


  if(ignoreAllNA == FALSE){
      if(ignoreNA == TRUE){

        # Identifies vars that are factors and converts to character
        df[sapply(df, is.factor)] <- lapply(df[sapply(df, is.factor)],
                                               as.character)
        df[is.na(df)] <- "Missing_Missing_Missing_Missing"

        # This was my original approach. Is this reliable?
        #df <- as.data.frame(df)

        # From https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors

        # Identifies vars that are character and converts to factor
        df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],
                                               as.factor)


        x = (rowSums(df == value))
        x = ifelse(nmiss == nvars, NA, x)

      }

      if(ignoreNA == FALSE){

        x = (rowSums(df == value))

      }

    final = x
  }

  if(ignoreAllNA == TRUE){


        # Identifies vars that are factors and converts to character
        df[sapply(df, is.factor)] <- lapply(df[sapply(df, is.factor)],
                                            as.character)
        df[is.na(df)] <- "Missing_Missing_Missing_Missing"

        # Identifies vars that are character and converts to factor
        df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],
                                               as.factor)


        x = (rowSums(df == value))



      final = x


  }

  final
}





