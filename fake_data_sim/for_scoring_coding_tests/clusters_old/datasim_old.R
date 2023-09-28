## Here we will have the functions for creating a variable in R

# qn - this is the question numbers your want to simulate (e.g. putting 11 will simulate x_01 through x_11)
# qlab - the question prefix, so "bla_" would result in bla_01 through bla_11
# rlab - our response labels, a vector with possible responses you want to sample from, either  one of options or custom
# num - the number of rows fo which you want to sim data
# custom - if rlab is set to "custom" we can specify a custom response scale
# qns - this allows a custom start number for the question numbers e.g. 4:8. It is NULL (and therefore 1) by default.


#easim samples randomly easim2 just repeats the vector instead, and easim_odd repeats the vector, but in the reverse order for odd numbers. The latter two are to allow visual inspection when testing.

easim <- function(df, qn, qlab, rlab, rnum, custom, qns = NULL){
  
  dat <- df
  
  #here we add the possible response scales. If "agreement" then the agreement scale, and so on.
  
  if(rlab == "agreement"){
    
    rlab <- c("Strongly Disagree", "Moderately Disagree","Slightly Disagree","Neither Agree nor Disagree","Slightly Agree", "Moderately Agree","Strongly Agree")
  
  } else if (rlab == "truth"){
    
    rlab <- c("Absolutely Untrue", "Mostly Untrue", "Somewhat Untrue", "Can't Say True or False", "Somewhat True", "Mostly True", "Absolutely True")
  
  } else if (rlab == "extent") {
    
    rlab <- c("1 - To No Extent At All",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7 - To a Great Extent")
    
  } else if (rlab == "yesno") {
    
    rlab <- c("Yes",
              "No")
    
  } else if (rlab == "truefalse") {
    
    rlab <- c("True", "False")
    
  } else if (rlab == "frequency") {
    
    rlab <- c(
      "Never",
      "Very Infrequently",
      "Infrequently",
      "Sometimes",
      "Frequently",
      "Very Frequently",
      "Always"
    )
    
    # here we allow custom scales, if "custom" is input as rlab.
    
  } else if (rlab == "custom") {
    
    # if the custom argument is not empty we assign that to rlab
    
    if (!is.null(custom)) {
      
      rlab <- custom
      
    } else {
      
      #if its empty we return an error
      
      stop("Unknown response scale with no custom specified.")
    }
  }
  
  
  # here we check whether qns (the starting digit for the question numbers has been specified)
  # if it has we use it, if not we set it to 1
  
    if (!is.null(qns)) {
      qns <- qns
      
    } else {
      qns <- 1
    }
  
  
  # Here we have loop which selects i from the sequence defined by str_pad(qns:qn, 2, "left", "0"). 
  # This sequence is simply qns:qn, but forced to be at least two digits, adding a "0" to the left if this is not the case (for 1 through 9).
  # e.g. 1:3 would become 01, 02, 03
  # The loop then combines each element of this index with the question label, defined by qlab.
  # e.g. if qlab was "x_" with the qn above, we would get x_01, x_02, x_03
  # It then samples a response from the possible response vector rlab, with rnum defining the number of times this is sampled.
  # This is then added to the data frame dat/df.
  
  
  
  for (i in str_pad(qns:qn, 2, "left",  "0")){
    dat[paste0(qlab, i)] <- sample(rlab, rnum, replace = TRUE)
  }
  
  return(dat)
  
}


# easim2 is the same as above, except instead of sampling from rlab, it just repeats it over and over
# (this allows better visual inspection of any scoring)



easim2 <- function(df, qn, qlab, rlab, rnum, qns = NULL) {
  
  dat <- df

  if(rlab == "agreement"){
    
    rlab <- c("Strongly Disagree", "Moderately Disagree","Slightly Disagree","Neither Agree nor Disagree","Slightly Agree", "Moderately Agree","Strongly Agree")
    
  } else if (rlab == "truth"){
    
    rlab <- c("Absolutely Untrue", "Mostly Untrue", "Somewhat Untrue", "Can't Say True or False", "Somewhat True", "Mostly True", "Absolutely True")
    
  } else if (rlab == "extent") {
    
    rlab <- c("1 - To No Extent At All",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7 - To a Great Extent")
    
  } else if (rlab == "yesno") {
    
    rlab <- c("Yes",
              "No")
    
  } else if (rlab == "truefalse") {
    
    rlab <- c("True", "False")
    
  } else if (rlab == "frequency") {
    
    rlab <- c(
      "Never",
      "Very Infrequently",
      "Infrequently",
      "Sometimes",
      "Frequently",
      "Very Frequently",
      "Always"
    )
  } else if (rlab == "custom") {
    
    
    if (!is.null(custom)) {
      rlab <- custom
      
    } else {
      stop("Unknown response scale with no custom specified.")
    }
  }
  
  
  if (!is.null(qns)) {
    qns <- qns
    
  } else {
    qns <- 1
  }
  
  for (i in str_pad(qns:qn, 2, "left", "0")) {
    dat[paste0(qlab, i)] <- rep(rlab, length.out = rnum)
  }
  
  return(dat)
}


# easim_odd works the same as easim2 except for odd numbers we reverse the order by which the response options are repeated
# e.g. x_01 might repeat the responsese "a", "b", "c", x_02 would be "c", "b", "a".


easim_odd <- function(df, qn, qlab, rlab, rnum, custom, qns = NULL){
  
  
  dat <- df
  
  if(rlab == "agreement"){
    
    rlab <- c("Strongly Disagree", "Moderately Disagree","Slightly Disagree","Neither Agree nor Disagree","Slightly Agree", "Moderately Agree","Strongly Agree")
    
  } else if (rlab == "truth"){
    
    rlab <- c("Absolutely Untrue", "Mostly Untrue", "Somewhat Untrue", "Can't Say True or False", "Somewhat True", "Mostly True", "Absolutely True")
    
  } else if (rlab == "extent") {
    
    rlab <- c("1 - To No Extent At All",
              "2",
              "3",
              "4",
              "5",
              "6",
              "7 - To a Great Extent")
    
  } else if (rlab == "yesno") {
    
    rlab <- c("Yes",
              "No")
    
  } else if (rlab == "truefalse") {
    
    rlab <- c("True", "False")
    
  } else if (rlab == "frequency") {
    
    rlab <- c(
      "Never",
      "Very Infrequently",
      "Infrequently",
      "Sometimes",
      "Frequently",
      "Very Frequently",
      "Always"
    )
  } else if (rlab == "custom") {
    
    
    if (!is.null(custom)) {
      
      rlab <- custom
      
    } else {
      
      stop("Unknown response scale with no custom specified.")
    }
  }
  
  
  if (!is.null(qns)) {
    qns <- qns
    
  } else {
    qns <- 1
  }
  
  
  for (i in str_pad(qns:qn, 2, "left",  "0")){
    
    #here we check whether the last digits of i are odd or even
    
    if(as.numeric(str_extract(i, "\\d+")) %% 2 == 0) {
      
      dat[paste0(qlab, i)] <- rep(rlab, length.out = rnum)
      
    } else {
      
      dat[paste0(qlab, i)] <- rep(rev(rlab), length.out = rnum)
      
    }
    }
      
      return(dat)
    
  }