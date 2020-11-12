# -------------------------------------------------------------------------------------------------
# TITLE: `above_median.R`
# AUTHOR: Anthony Nguyen
# CONTACT: anguyen1210@hotmail.com
# LAST UPDATE: 5 June 2020
# 
# DESCRIPTION: 
# -------------------------------------------------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

#this function returns a dummy column indicating whether each element in the
#input vector is x-percent above its median
above_median <- function(col_vec, pct, dummy=TRUE){
    vec <- na.omit(col_vec) #remove NAs
    vec <- vec[vec > 0] #remove missing values
    vec_median <- median(vec)
    th_median <- vec_median * (pct/100 + 1)
    col_vec_logical <- col_vec > th_median
    col_vec_logical[col_vec < 0] <- NA
    col_vec_dummy <- if_else(col_vec_logical==TRUE, 1, 0)
    
    if (dummy==TRUE){
        return(col_vec_dummy)
    } else {
        return(col_vec_logical)
    }
}


below_median <- function(col_vec, pct, dummy=TRUE){
    vec <- na.omit(col_vec) #remove NAs
    vec <- vec[vec > 0] #remove missing value
    vec_median <- median(vec)
    th_median <- vec_median * (pct/100)
    col_vec_logical <- col_vec < th_median
    col_vec_logical[col_vec < 0] <- NA
    col_vec_dummy <- if_else(col_vec_logical==TRUE, 1, 0)
    
    if (dummy==TRUE){
        return(col_vec_dummy)
    } else {
        return(col_vec_logical)
    }
}