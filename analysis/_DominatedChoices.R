# Meta --------------------------------------------------------------------

## Date Created:  3/4/2020
## Date Edited:   6/5/2020
## Description:   Simple regressions of dominated choices


# Initial Regressions -----------------------------------------------------


reg1 <- felm(dominated_choice ~ any_assist + lang_english + lang_spanish +
               low_income + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
               perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
               perc_other + hh_single + SEP + new_enrollee 
             | region + year + insurer | 0 | region, 
     data=final.data)

reg2 <- felm(dominated_choice ~ any_assist + lang_english + lang_spanish +
               low_income + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
               perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
               perc_other + hh_single + SEP + new_enrollee
             | household_id + year + insurer | 0 | household_id, 
             data=final.data)

reg1.b <- felm(dominated_choice ~ assist_agent + assist_other + lang_english + lang_spanish +
                 low_income + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
                 perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
                 perc_other + hh_single + SEP + new_enrollee 
               | region + year + insurer  | 0 | region, 
               data=final.data)


reg2.b <- felm(dominated_choice ~ assist_agent + assist_other + lang_english + lang_spanish +
               low_income + perc_0to17 + perc_18to25 + perc_26to34 + perc_35to44 +
               perc_45to54 + perc_male + perc_asian + perc_black + perc_hispanic +
               perc_other + hh_single + SEP + new_enrollee
             | household_id + year + insurer | 0 | household_id, 
             data=final.data)
