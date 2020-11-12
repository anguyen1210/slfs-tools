# --------------------------------------------------------------------------------------- TITLE: `read_slfs`
# AUTHOR: Anthony Nguyen
# CONTACT: anthony.nguyen@hotmail.com
# LAST UPDATE: 12 November 2020
# 
# DESCRIPTION: These functions can be used to read in the data files (CSV),
# variable labels (TXT), and value labels (TXT) for the Swiss Labor Force survey
# (SLFS/ESPA/SAKE) data issued by the Swiss FSO. Both the variable and value
# label files are available as SAS and SPSS formatted TXT files. These functions
# to read in the label files work for either format style. 
#
# The functions depend on These functions depend on a number of functions from
# the following packages: readr, stringr, tidyr, magrittr, and tibble. Those
# packages can be loaded individually, or they can all be called together under
# the tidyverse package.
# --------------------------------------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

read_slfs_var <- function(varlab){
#this function returns a named list of variables that can be used with the
#`labelled` package
    if(stringr::str_detect(varlab, "VAR-LABEL-")) {
        
        if(stringr::str_detect(varlab, "-SAS")) {
            varlist <- readr::read_delim(varlab, 
                                 delim = " = ", 
                                 col_names = FALSE, 
                                 col_types = cols(X1 = col_character(),
                                                  X2 = col_skip(),
                                                  X3 = col_character(),
                                                  X4 = col_skip()
                                 ),
                                 locale = locale(encoding = "Latin1"), 
                                 trim_ws=TRUE)
            varlist <- tibble::deframe(varlist)
            
        } else {
            #the last line of the SPSS final throws throws up a warning before
            #we can remove it
            varlist <- suppressWarnings(
                       readr::read_delim(varlab, 
                                  delim = " ", 
                                  col_names = FALSE, 
                                  skip = 1,
                                  col_types = cols(X1 = col_character(),
                                                   X2 = col_character(),
                                                   X3 = col_skip()),
                                  locale = locale(encoding = "Latin1"),
                                  trim_ws = TRUE)
                        )
            varlist <- varlist[!(varlist$X1 == "." | varlist$X2==""), ]
            varlist <- tibble::deframe(varlist)
        }
        
        return(varlist)
    }
    
    else {
        
        print("This function only accepts 'VAR-LABEL' files")
    }
}


read_slfs_val <- function(valuelab) {
#this function returns a named list of variables that can be used with the
#`labelled` package
    if (str_detect(valuelab, "FORMAT-") | str_detect(valuelab, "VALUE-LABEL-")) {
        
        if(str_detect(valuelab, "-SAS")) {
            value_df <- readr::read_fwf(valuelab, 
                                    fwf_positions(c(1, 9, 29, 43, 49), 
                                                  c(7, 25, 43, 47, NA), 
                                                  c("junk", "var", "val", "delim", "val_lab")),
                                    col_types = cols(junk = col_skip(),
                                                     var = col_character(),
                                                     val = col_integer(),
                                                     delim = col_skip(),
                                                     val_lab = col_character()),
                                    locale = locale(encoding = "Latin1"),
                                    trim_ws = TRUE)
            
            value_df <- value_df %>% tidyr::fill(var, .direction="down")
            value_df$var <- value_df$var %>% substr(., 1, nchar(.)-1) #strip extra char from var
            value_df$val_lab <- value_df$val_lab %>% str_replace_all("\"", "")
            
        } else {
            value_df <- read_fwf(valuelab, 
                                     fwf_positions(c(1, 9, 25, 45), 
                                                   c(8, 24, 41, NA), 
                                                   c("junk", "var", "val", "val_lab")),
                                     col_types = cols(junk = col_skip(),
                                                      var = col_character(),
                                                      val = col_integer(),
                                                      val_lab = col_character()),
                                     locale = locale(encoding = "Latin1"),
                                     trim_ws = TRUE,
                                     skip = 1)
            
            value_df <- value_df %>% fill(var, .direction="down")
            value_df$val_lab <- value_df$val_lab %>% str_replace_all("\"", "")
            value_df <- value_df %>% tidyr::drop_na(val) #drop final row 
            
        }
        
        valuelist <- value_df %>% group_split(var) 
        names(valuelist) <- value_df %>% group_keys(var) %>% pull
        
        valuelist <- sapply(valuelist, function(x) {
            x %>% select(val_lab, val) %>% tibble::deframe
                }
            )
        }
    
    else {
        
        print("This function only accepts value-label files")
    }
}

read_slfs <- function(data, varlab=NULL, valuelab=NULL) {
    
    #if (str_detect(data, "SAKE*.CSV")) {
    if (str_detect(data, regex("sake.*csv", ignore_case = TRUE))) {
        
        
        if (is.null(varlab) & is.null(valuelab)) {
            print(data)
            print("Reading in data file only")
            
            read_delim(data, 
                       delim = ";", 
                       col_types = cols(.default = col_double()))
            
        } else if (!is.null(varlab) & is.null(valuelab)) {
            print(data)
            print("Reading in data and variable file, and assigning variable labels to data")
            
            df <- read_delim(data, 
                             delim = ";", 
                             col_types = cols(.default = col_double()))
            
            varlabs <- read_slfs_var(varlab)
            
            var_label(df) <- varlabs
            
            df
            
        } else if (is.null(varlab) & !is.null(valuelab)) {
            print(data)
            print("Reading in data and value file, and assinging values to data ")
            
            df <- read_delim(data, 
                             delim = ";", 
                             col_types = cols(.default = col_double()))
            
            valuelabs <- read_slfs_val(valuelab)
            
            val_labels(df) <- valuelabs
            
            df
            
        } else {
            print(data)
            print("Reading in all files, and assinging values and variables to data")
            
            df <- read_delim(data, 
                             delim = ";", 
                             col_types = cols(.default = col_double()))
            
            valuelabs <- read_slfs_val(valuelab)
            
            varlabs <- read_slfs_var(varlab)
            
            #value labels should be applied before variable labels as
            #var_label() overwrites
            if (length(df) > length(valuelabs)) {
                print(data)
                print("data contains more variables than value label file")
                print("the following data variables do not have value labels:")
                
                missing_valuelabs_index <- !(names(df) %in% names(valuelabs))
                print(names(df[missing_valuelabs_index]))
                
                val_labels(df) <- valuelabs 
                
            } else if (length(df) < length(valuelabs)) {
                print(data)
                print("data contains less variables than value label file")
                print("the following variable value labels are unused and will be ignored:")
                
                df_cols <- names(df) 
                missing_data_val_index <- !(names(valuelabs) %in% df_cols) 
                print(names(valuelabs[missing_data_val_index]))
                
                valuelabs_sub <- valuelabs[df_cols] #subset only valuelabs that exist in df
                val_labels(df) <- valuelabs_sub #apply valuelabs
                
            } else {
                val_labels(df) <- valuelabs 
            }
            
            if (length(df) > length(varlabs)) {
                print(data)
                print("data contains more variables than variable label file")
                print("the following data variables do not have variable labels:")
                
                missing_varlabs_index <- !(names(df) %in% names(varlabs))
                print(names(df[missing_varlabs_index]))
                
                var_label(df) <- varlabs
                
            } else if (length(df) < length(varlabs)) {
                print(data)
                print("data contains less variables than variable label file")
                print("the following variable labels are unused and will be ignored:")
                
                df_cols <- names(df) #get names of df cols
                missing_data_var_index <- !(names(varlabs) %in% df_cols) 
                print(names(varlabs[missing_data_var_index]))
                
                varlabs_sub <- varlabs[df_cols] #subset only varlabels that exist in df
                var_label(df) <- varlabs_sub #apply varlabels
                
            } else {
                var_label(df) <- varlabs
            }
            
            df
        }
        
    } else {
        print("error: first argument must be the filename")
    }

}
