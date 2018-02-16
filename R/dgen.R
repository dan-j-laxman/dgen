#' Row Any Count
#'
#' For each row, returns the count of variables in the variable list (...) equal to a
#' specified value or values. Includes options for handling missing data.
#'
#' @param ... Variable list over which to count instances equal to value (no limit on number of
#' variables)
#' @param values Specified value or values to count; can be numeric, nominal, or character
#' @param maxmiss_n Maximum number of missing values allowed in the variables list in
#' order to return a count; default is 0. For each row, if # of missing values <= maxmiss_n,
#' then the count is calculated using the remaining variables with non-missing values; if # of
#' missing values > maxmiss_n, then NA is returned.
#' @return Vector of length of each input variable
#'
#' @export

row_any_count =  function(..., values, maxmiss_n = 0){

  require(tidyverse)

  # Variables are combined into a dataframe
  df = (data.frame(...))

  # Check that variables in "..." are in the data frame.
  # Needs to be added

  # Number of missing in each row is calculated.
  nmissing = rowSums(is.na(df))

  # Number of variables is calculated.
  nvars = NCOL(df)

  # Check that maxmiss_n is an integer
  # (Check will fail if deviation from integer is less than 1e15)
  intcheck = maxmiss_n%%1 == 0

  if(intcheck == FALSE){
    stop("Number of maximum missing values must be an integer")
  }

  else{

    # Check for negative number of maximum missing values
    if(maxmiss_n < 0){
      stop("Negative number of maximum missing values is not allowed")
    }

    # Check if number of maximum missing values greater than number
    # of variables.
    if(maxmiss_n > nvars){
      stop("Number of maximum missing values exceeds number of variables")
    }

    # If maxmiss_n is 0 or a positive integer and maxmiss_n does not exceed
    # the number of variables, then calculates count.

    if(maxmiss_n <= nvars & maxmiss_n >= 0){

      # Count is calculated only for cases missing data on no more
      # than the maximum number of missing values allowed.

      datalist <- list()

      for (i in values ){
      count = ifelse(nmissing > maxmiss_n,
                   NA,
                   rowSums(df == i, na.rm = TRUE))
      datalist[[i]] <- count
      }
      df.count <- dplyr::bind_cols(datalist)

      final = rowSums(df.count)

      }

  }

  final

}



#' Row Greater Count
#'
#' For each row, returns the count of variables in the variable list (...) with values greater
#' than a specified value. Includes options for handling missing data.
#'
#' @param ... Variable list over which to count instances equal to value (no limit on number of
#' variables)
#' @param value Specified value; must be numeric. Values greater than than the specified value
#' will be counted.
#' @param maxmiss_n Maximum number of missing values allowed in the variables list in
#' order to return a count; default is 0. For each row, if # of missing values <= maxmiss_n,
#' then the count is calculated using the remaining variables with non-missing values; if # of
#' missing values > maxmiss_n, then NA is returned.
#' @return Vector of length of each input variable
#'
#' @export

row_greater_count =  function(..., value, maxmiss_n = 0){

  require(tidyverse)

  # Variables are combined into a dataframe
  df = (data.frame(...))

  # Check that variables in "..." are in the data frame.
  # Needs to be added

  # Number of missing in each row is calculated.
  nmissing = rowSums(is.na(df))

  # Number of variables is calculated.
  nvars = NCOL(df)

  # Check that maxmiss_n is an integer
  # (Check will fail if deviation from integer is less than 1e15)
  intcheck = maxmiss_n%%1 == 0

  if(intcheck == FALSE){
    stop("Number of maximum missing values must be an integer")
  }

  else{

    # Check that value is numeric
    numeric.check = is.numeric(value)

    if(numeric.check == FALSE){
      stop("Value must be numeric. Note: numeric values should not be placed in parentheses")
    }

    else{

      # Check for negative number of maximum missing values
      if(maxmiss_n < 0){
        stop("Negative number of maximum missing values is not allowed")
      }

      # Check if number of maximum missing values greater than number
      # of variables.
      if(maxmiss_n > nvars){
        stop("Number of maximum missing values exceeds number of variables")
      }

      # If maxmiss_n is 0 or a positive integer and maxmiss_n does not exceed
      # the number of variables, then calculates count.

      if(maxmiss_n <= nvars & maxmiss_n >= 0){

        # Count is calculated only for cases missing data on no more
        # than the maximum number of missing values allowed.
        x = ifelse(nmissing > maxmiss_n, NA, rowSums(df > value, na.rm = TRUE))
      }

      }

    }

    final = x

  }


#' Row Less Count
#'
#' For each row, returns the count of variables in the variable list (...) with values less
#' than a specified value. Includes options for handling missing data.
#'
#' @param ... Variable list over which to count instances equal to value (no limit on number of
#' variables)
#' @param value Specified value; must be numeric. Values greater than than the specified value
#' will be counted.
#' @param maxmiss_n Maximum number of missing values allowed in the variables list in
#' order to return a count; default is 0. For each row, if # of missing values <= maxmiss_n,
#' then the count is calculated using the remaining variables with non-missing values; if # of
#' missing values > maxmiss_n, then NA is returned.
#' @return Vector of length of each input variable
#'
#' @export

row_less_count =  function(..., value, maxmiss_n = 0){

  require(tidyverse)

  # Variables are combined into a dataframe
  df = (data.frame(...))

  # Check that variables in "..." are in the data frame.
  # Needs to be added

  # Number of missing in each row is calculated.
  nmissing = rowSums(is.na(df))

  # Number of variables is calculated.
  nvars = NCOL(df)

  # Check that maxmiss_n is an integer
  # (Check will fail if deviation from integer is less than 1e15)
  intcheck = maxmiss_n%%1 == 0

  if(intcheck == FALSE){
    stop("Number of maximum missing values must be an integer")
  }

  else{

    # Check that value is numeric
    numeric.check = is.numeric(value)

    if(numeric.check == FALSE){
      stop("Value must be numeric. Note: numeric values should not be placed in parentheses")
    }

    else{

      # Check for negative number of maximum missing values
      if(maxmiss_n < 0){
        stop("Negative number of maximum missing values is not allowed")
      }

      # Check if number of maximum missing values greater than number
      # of variables.
      if(maxmiss_n > nvars){
        stop("Number of maximum missing values exceeds number of variables")
      }

      # If maxmiss_n is 0 or a positive integer and maxmiss_n does not exceed
      # the number of variables, then calculates count.

      if(maxmiss_n <= nvars & maxmiss_n >= 0){

        # Count is calculated only for cases missing data on no more
        # than the maximum number of missing values allowed.
        x = ifelse(nmissing > maxmiss_n, NA, rowSums(df < value, na.rm = TRUE))
      }

    }

  }

  final = x

}

#' Row Any Match
#'
#' Checks whether the value of any variable in a variable list matches a specified value or values.
#' Returns a value of 1 if at least one variable in the variable list (...) is equal to a
#' specified value or values; 0 or NA otherwise. Includes options for handling missing data.
#'
#' @param ... List of variables over which to check for a match (no limit
#' on number of variables)
#' @param values Specified value or values to count; can be numeric, nominal, or character
#' @param maxmiss_n Maximum number of missing values allowed in the variables list in
#' order to return a value; default is 0. For each row, if # of missing values <= maxmiss_n,
#' then whether (1) or not (0) there is a match is returned using the remaining variables with
#' non-missing values; if # of missing values > maxmiss_n, then NA is returned.
#' @return Vector of length of each input variable
#'
#' @return Vector of length of each input variable
#'
#' @export

row_any_match =  function(..., values, maxmiss_n = 0){

  require(tidyverse)

  # Variables are combined into a dataframe
  df = (data.frame(...))

  # Check that variables in "..." are in the data frame.
  # Needs to be added

  # Number of missing in each row is calculated.
  nmissing = rowSums(is.na(df))

  # Number of variables is calculated.
  nvars = NCOL(df)

  # Check that maxmiss_n is an integer
  # (Check will fail if deviation from integer is less than 1e15)
  intcheck = maxmiss_n%%1 == 0

  if(intcheck == FALSE){
    stop("Number of maximum missing values must be an integer")
  }

  else{

    # Check for negative number of maximum missing values
    if(maxmiss_n < 0){
      stop("Negative number of maximum missing values is not allowed")
    }

    # Check if number of maximum missing values is greater than number
    # of variables.
    if(maxmiss_n > nvars){
      stop("Number of maximum missing values exceeds number of variables")
    }

    # If maxmiss_n is 0 or a positive integer and maxmiss_n does not exceed
    # the number of variables, then check for any match.

    if(maxmiss_n <= nvars & maxmiss_n >= 0){

    # Checks for any match and returns a value of 1 if there is a match or 0
    # if there is not, but does so only for rows missing data on no more
    # than the maximum number of missing values allowed. Otherwise, returns NA.


      datalist <- list()

      for (i in values ){
        count = ifelse(nmissing > maxmiss_n,
                       NA,
                       rowSums(df == i, na.rm = TRUE))
        match = ifelse(count >= 1, 1,
                       ifelse(count == 0, 0, NA))
        datalist[[i]] <- match
      }
      df.match <- dplyr::bind_cols(datalist)

      final = rowSums(df.match)
      final = ifelse(final >= 1, 1,
                     ifelse(final == 0, 0, NA))


    }

  }

    final
}





#' Row Mean
#'
#' Calculates the average value across variables within each row. Includes an option for
#' handling missing data.
#'
#' @param ... Variables to be averaged (no limit on number of variables, but must be numeric).
#' @param maxmiss_n Maximum number of missing values allowed in the variables list in
#' order to return an average; default is 0. For each row, if # of missing values <= maxmiss_n,
#' then the average is calculated using the remaining variables with non-missing values; if # of
#' missing values > maxmiss_n, then NA is returned. maxmiss_n must be less than the number of
#' variables.
#' @return Vector of length of each input variable.
#'
#' @export


row_mean =  function(..., maxmiss_n = 0){

  # Variables are combined into a dataframe
  df = (data.frame(...))

  # Check that variables in "..." are in the data frame.
  # Needs to be added

  # Number of missing in each row is calculated.
  nmissing = rowSums(is.na(df))

  # Number of variables is calculated.
  nvars = NCOL(df)

  # Check that maxmiss_n is an integer
  # (Check will fail if deviation from integer is less than 1e15)
  intcheck = maxmiss_n%%1 == 0

  if(intcheck == FALSE){
    stop("Number of maximum missing values must be an integer")
  }

  else{

    # Check for negative number of maximum missing values
    if(maxmiss_n < 0){
      stop("Negative number of maximum missing values is not allowed")
    }

    # Check if number of maximum missing values is greater than or equal to  number
    # of variables.
    if(maxmiss_n >= nvars){
      stop("Number of maximum missing values is equal to or exceeds number of variables. A
           mean cannot be computed if values are missing for all variables")
    }

    # If maxmiss_n is 0 or a positive integer and maxmiss_n does not exceed
    # the number of variables, then calculates mean.

    if(maxmiss_n <= nvars & maxmiss_n >= 0){

      # Mean is calculated only for cases missing data on no more
      # than the maximum number of missing values allowed.

    x = ifelse(nmissing > maxmiss_n, NA, rowMeans(df, na.rm = TRUE))

  }

  }

  final = x

}






#' Row Missing
#'
#' Counts the number of missing values across variables (columns) within each row.
#'
#' @param ... Variables over which to count missing values (no limit on number of variables)
#' @return Vector of length of each input variable
#'
#' @export

row_nmiss =  function(...){

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

row_nvalid =  function(...){

  x = rowSums(!is.na(cbind(...)))

  return(x)

}






#' Row Sum
#'
#' Calculates sum across variables within each row. Includes an option for handling missing data.
#'
#' @param ... Variables to be summed (no limit on number of variables, but must be numeric).
#' @param maxmiss_n Maximum number of missing values allowed in the variables list in
#' order to return a sum; default is 0. For each row, if # of missing values <= maxmiss_n,
#' then the sum is calculated using the remaining variables with non-missing values; if # of
#' missing values > maxmiss_n, then NA is returned. maxmiss_n must be less than the number of
#' variables.
#'
#' @return Vector of length of each input variable.
#'
#' @export


row_sum =  function(..., maxmiss_n = 0){

  # Variables are combined into a dataframe
  df = (data.frame(...))

  # Check that variables in "..." are in the data frame.
  # Needs to be added

  # Number of missing in each row is calculated.
  nmissing = rowSums(is.na(df))

  # Number of variables is calculated.
  nvars = NCOL(df)

  # Check that maxmiss_n is an integer
  # (Check will fail if deviation from integer is less than 1e15)
  intcheck = maxmiss_n%%1 == 0

  if(intcheck == FALSE){
    stop("Number of maximum missing values must be an integer")
  }

  else{

    # Check for negative number of maximum missing values
    if(maxmiss_n < 0){
      stop("Negative number of maximum missing values is not allowed")
    }

    # Check if number of maximum missing values is greater than or equal to  number
    # of variables.
    if(maxmiss_n >= nvars){
      stop("Number of maximum missing values is equal to or exceeds number of variables. A
           sum cannot be computed if values are missing for all variables")
    }

    # If maxmiss_n is 0 or a positive integer and maxmiss_n does not exceed
    # the number of variables, then calculates mean.

    if(maxmiss_n <= nvars & maxmiss_n >= 0){

      # Sum is calculated only for cases missing data on no more
      # than the maximum number of missing values allowed.

      x = ifelse(nmissing > maxmiss_n, NA, rowSums(df, na.rm = TRUE))

    }

    }

  final = x

}


