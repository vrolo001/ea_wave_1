funBeliefCode <- function(df){
  
  df <- df %>%
    mutate(
      
      #Belief in God (categorical)
      belief_god_cat = issp_01, 
      
      #Belief in God (binary)
      belief_god_bin = case_when(as.numeric(issp_01) == 5 | as.numeric(issp_01) == 6  ~ 1,
                                 TRUE ~ 0),
      # Belief in God (continuous)
      belief_god_con = belief_01, 
      
      # Agnosticism (continuous)
      agnosticism_cont = agn_01, 
      
      # Agnostic Identity 
      agnosticism_bin  = case_when(as.numeric(issp_02) == 5 ~ 1,
                                   TRUE ~ 0),
      # Atheist Membership
      atheist_mem = am_01, 
      
      # Atheist Identity
      atheist_id = case_when(as.numeric(issp_02) == 4 ~ 1,
                             TRUE ~ 0), 
      # Religious Identification
      relig__id_1 = rid_01, 
      
      # Religious Identity, 
      relig_id_2 = rid_02, 
      
      # Religious Attendence
      relig_attend = rp_01,
      
      # Prayer Frequency
      prayer_freq = rp_02, 
      
      # Religious Objects
      relig_object = rp_03, 
      
      # Anti-religiosity
      anti_relig = ar_01,
      
      # Possibility of Knowing (God)
      god_knowing = belief_02, 
      
      # Confidence in Belief
      belief_confidence = conf_01, 
      
      # Apatheism (god)
      apatheism_god = apth_01,
      
      # Apatheism (life purpose)
      apatheism_purpose = apth_02,
      
      # Apatheism (combined)
      apx1 = as.numeric(apth_01),  #was buggy using as numeric within rowMeans so these are for computing 
      apx2 = as.numeric(apth_02),
      
      apatheism = rowMeans(across(c("apx1", "apx2"))),
      
      # Possibility of Truth
      truth_possibility = mean_01,
      
      # Naturalism  - NEEDS TESTING
      naturalism = case_when(
        rowSums(across(all_of(snb_cols), ~grepl("Strongly Disagree |Moderately Disagree | Slightly Disagree", .))) > 0 ~ 1, 
        TRUE ~ 0),
      
      # Afterlife Existence
      afterlife_exist = snb_01,
      
      # Afterlife Punishment
      afterlife_punish= snb_13,
      
      # Afterlife Reward
      afterlife_reward= snb_14,
      
      # Reincarnation
      reincarnation = snb_02,
      
      # Astrology
      astrology = snb_03,
      
      # Mystical People
      msystical_people= snb_04,
      
      # Mystical Objects
      mystical_objects= snb_05,
      
      # Good and Evil
      good_evil = snb_06,
      
      # Universal Spirit or Life Force
      lifeforce = snb_07,
      
      # Karma
      karma = snb_08,
      
      # Fate
      fate= snb_09,
      
      # Supernatural Beings Existence
      supernat_beings = snb_10,
      
      # Good Supernatural Beings
      good_beings = snb_11,
      
      # Harmful Supernatural Beings
      harmful_beings= snb_12,
      
      # Evil Eye
      evil_eye= snb_15,
      
      # Personal Spiritual Experience
      spiritual_force = snb_16,
      
      #Lucky Object Carrying
      lucky_objects = snb_17  )
  
  return(df)
}