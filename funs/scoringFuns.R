
# Here we have two different scoring functions

# funScore - this scores directly, replacing the response wording with numeric scores.

# funScore2 - this codes responses into factors.


funScore  <- function(df, cols, responsescale, reverse, custom = NULL) {
  # arguments:
  # df (our existing dataframe)
  # columns - a column or series of columns
  # the response scale we are using
  # whether it should be reversed or not
  # custom - a custom response option vector (used if response scale is "custom")
  
  
  #check  response scale  is valid
  
  valid_scales <- c("agreement", "frequency", "truth", "extent", "yesno", "custom")
  
  if (!(responsescale %in% valid_scales)) {
    stop("Invalid response scale. Choose from: agreement, frequency, extent, yesno, truth, custom.")
  }
  
  
  # Check if custom is provided when responsescale is "custom"
  
  if (responsescale == "custom" && is.null(custom)) {
    stop("If using 'custom' response scale, you must provide a 'custom' vector of response options.")
  }
  
  dat <- df
  
  
  #here we specify which response scale we are using
  
  response_scales <- list( "agreement" = c("Strongly Disagree",
                                           "Moderately Disagree",
                                           "Slightly Disagree",
                                           "Neither Agree nor Disagree",
                                           "Slightly Agree",
                                           "Moderately Agree",
                                           "Strongly Agree"),
                           
                           "frequency" = c("Never",
                                           "Very Infrequently",
                                           "Infrequently",
                                           "Sometimes",
                                           "Frequently",
                                           "Very Frequently",
                                           "Always"),
                           
                           "truth" = c("Absolutely Untrue",
                                       "Mostly Untrue",
                                       "Somewhat Untrue",
                                       "Can't Say True or False",
                                       "Somewhat True",
                                       "Mostly True",
                                       "Absolutely True"),
                           
                           "custom" = custom
    )
  
  
  #allocate the correct response labels to "labels"
  
labels <- response_scales[[responsescale]]

  
  # recode the selected columns according to these labels
  
  # first we do extent and yesno, as these need custom recoding

  #extent
  
  if (responsescale == "extent") {
    
    
    # here we loop through the columns input into the function
    
      for (x in cols) {
        dat[[x]] <- recode(
          dat[[x]],
          
          #this line recodes "7 - To a Great Extent" to 1 if reverse is TRUE, and 7 if not, and so on
          
          "7 - To a Great Extent" = ifelse(reverse, 1, 7),
          "6" = ifelse(reverse, 2, 6),
          "5" = ifelse(reverse, 3, 5),
          "4" = 4,
          "3" = ifelse(reverse, 5, 3),
          "2" = ifelse(reverse, 6, 2),
          "1 - To No Extent At All" = ifelse(reverse, 7, 1)
        )
      }

    
    #same with yesno
    
  } else if (responsescale == "yesno") {
    
      for (x in cols) {
        dat[[x]] <- recode(dat[[x]],"Yes" =  ifelse(reverse, 0, 1),
                                    "No" =   ifelse(reverse, 1, 0))

  }  
    #we now do all the other scales
    
  } else { 
    
    if (reverse == FALSE) {
      
      #this code
      
      for (x in cols) {
        
        
        # this line takes the column then matches the elements in the column to the corresponding position in our labels vector
        # so "slightly agree" would be 5 as its in the 5th place in that vector
        
        dat[[x]] <- as.numeric(match(dat[[x]], labels))
        
      }
    } else if (reverse ==  TRUE) {
      
      # here we do the same as above, but we match it to its place on the *reverse* of our labels vector 
      # so "slightly agree" would no longer be the 5th item in the vector, it would be the 2nd
      # as such elements of the column with "slightly agree" would be coded as 2
      
      for (x in cols) {
        dat[[x]] <- as.numeric(match(dat[[x]], rev(labels)))
        
      }
    }
    
  }
  
  return(dat)
}




#####################################################################################################################################################################

#####################################################################################################################################################################

#####################################################################################################################################################################

#####################################################################################################################################################################




funScore2 <- function(df, cols, responsescale, customlev = NULL, ordered = FALSE) {
  
  # arguments:
  # df (our existing dataframe)
  # columns - a column or series of columns
  # the response scale we are using
  # customlev - a custom factor levels vector (used if response scale is "custom")
  # ordered - whether the factor should be ordered, default FALSE
  
  dat <- df

    if (responsescale == "custom") {
    
    if (is.null(customlev)) {
      
      cat("Custom response scale requires levels defined with customlev.")
      
      return(NULL)
    }
    
    levels <- customlev
    
  }
  
  
#create factor info for response scales
  
#these are created such that the levels interpretation makes sense (e.g. higher levels aremore agreement, truth, frequency, extent etc.)
  
  
response_scales <-   list("agreement" = list(levels = c("Strongly Disagree",
                                                        "Moderately Disagree",
                                                        "Slightly Disagree",
                                                        "Neither Agree nor Disagree",
                                                        "Slightly Agree",
                                                        "Moderately Agree",
                                                        "Strongly Agree")),
                          
                          "frequency" = list(levels = c("Never",
                                                        "Very Infrequently",
                                                        "Infrequently",
                                                        "Sometimes",
                                                        "Frequently",
                                                        "Very Frequently",
                                                        "Always")),
                          
                          "truth" = list(levels = c("Absolutely Untrue",
                                                    "Mostly Untrue",
                                                    "Somewhat Untrue",
                                                    "Can't Say True or False",
                                                    "Somewhat True", "Mostly True",
                                                    "Absolutely True")),
                          
                          "extent" = list(levels = c("1 - To No Extent At All",
                                                     "2",
                                                     "3",
                                                     "4",
                                                     "5",
                                                     "6",
                                                     "7 - To a Great Extent")),
                          
                          "yesno" = list(levels = c("No", "Yes")),
                          
                          "custom" = list(levels = customlev))
  
  
  
  if (responsescale %in% names(response_scales)) {
    
    
    levels <- response_scales[[responsescale]]$levels
  }
    
  

if (ordered == TRUE){  
    
for (x in cols) {
  dat <- dat %>% mutate(!!sym(x) :=
                          factor(!!sym(x),
                                 levels = levels,
                                 ordered = TRUE))
}
  
} else {
  
  for (x in cols) {
    dat <- dat %>% mutate(!!sym(x) :=
                            factor(!!sym(x),
                                   levels = levels))
  
  
  }
}

return(dat)
}










