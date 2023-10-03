
#this path needs to be same as that used in script this fun is called to
source("../../funs/scoringFuns.R")  


######### This funsction uses funScore and funScore2, the former codes as numeric, the latter as factors.

funBeliefCode <- function(dat) {
  
# ISSP  
  
#here we set the response options (which will be the factor levels)
  
  issp_01_levels  <- c(                                                                        
    "I don't believe in God",
    "I don't know whether there is a God, and I don't believe there is any way to find out",
    "I don't believe in a personal God, but I do believe in a Higher Power of some kind",
    "I find myself believing in God some of the time, but not at others",
    "While I have doubts, I feel that I do believe in God.",
    "I know God really exists and I have no doubt about it.",
    "Don't know")
  
  issp_02_levels <- c(
    "Spiritual but not religious",
    "Seeker",
    "Non-religious",
    "Atheist",
    "Agnostic",
    "Humanist",
    "Sceptic",
    "Free thinker",
    "Rationalist",
    "Secular",
    "Christian",
    "Muslim",
    "Jewish",
    "Buddhist",
    "Hindu",
    "Daoist",
    "Shinto",
    "Confucian",
    "Other (please specify)")
  
#here we turn the variable into a factor.
  dat <- funScore2(dat, "issp_01", responsescale = "custom", customlev = issp_01_levels)  
  
  dat <- funScore2(dat, "issp_02", responsescale = "custom", customlev = issp_02_levels)  
  
# Religious Identification

# scored numerically with funScore
 
  dat <- funScore(dat, "rid_01", "yesno", FALSE)

  rid_02_levels <- c(
    "Protestant",
    "Catholic",
    "Orthodox (e.g. Greek Orthodox, Russian Orthodox)",
    "Sunni",
    "Shiite",
    "Buddhist",
    "Confucian",
    "Daoist",
    "Hindu",
    "Jewish",
    "Shinto",
    "Other (please specify)")
  
  dat <- funScore2(dat, "rid_02", responsescale = "custom", customlev = rid_02_levels)
  
  
# Religious Practice
  
  rp_01_levels <- rev(c(    #we reverse this to make the order lowest to highest
    "More than once a week",
    "Once a week",
    "Once a month",
    "Only on special holy days",
    "Once a year",
    "Less often",
    "Never, practically never"
  ))
  
  rp_02_levels <- rev(c(  #we reverse this to make the order lowest to highest
    "Several times a day",
    "Once a day",
    "Several times each week",
    "Only when attending religious services",
    "Only on special holy days",
    "Once a year",
    "Less often",
    "Never, practically never"
  ))
  
  rp_03_levels <- c(
    "Yes, for religious reasons",
    "Yes, for non-religious reasons",
    "No")
  
  dat <- funScore2(dat, "rp_01", responsescale = "custom", customlev = rp_01_levels, ordered = TRUE)
    
  dat <- funScore2(dat, "rp_02", responsescale = "custom", customlev = rp_02_levels, ordered = TRUE) 
  
  dat <- funScore2(dat, "rp_03", responsescale = "custom", customlev = rp_03_levels)
  
  
# Supernatural Belief
  
  snb_cols <- colnames(dat %>%
                           select(starts_with("snb_")) %>%
                           select(-snb_16, - snb_17)) 
  snb_16_levels <- c(
    "Yes, I’ve had an experience like this.",
    "I’ve had an experience like this, but I didn’t associate it with a spiritual force.",
    "No, I’ve never had an experience like this.")
    
  dat <- funScore2(dat, snb_cols, "agreement", ordered = TRUE)
  
  dat <- funScore2(dat, "snb_16", "custom", snb_16_levels)
  
  dat <- funScore(dat, "snb_17", "yesno", reverse = FALSE)
  
  
# General Belief
  
  gb_cols <- colnames(dat %>%
                        select(starts_with(c("belief", "conf_", "ar_", "apth_", "agn_", "mean_"))))
  
  dat <- funScore2(dat, gb_cols, "agreement", ordered = TRUE)
  
  
# Atheist Membership
  
  dat <- funScore(dat, "am_01", "yesno", reverse = FALSE)
  
  
return(dat)
  
}
