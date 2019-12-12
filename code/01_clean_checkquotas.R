# Replication archive for "Where Self-Interest Trumps Ideology: Liberal
# Homeowners and Local Opposition to Housing Development" by William Marble
# and Clayton Nall.

# Takes the csv from qualtrics export and cleans it. Also generates a csv
# that shows sample statistics on the variable that we quota-sampled on.

library(stringr)
library(dplyr)
library(car)


source("code/functions.R")

dat = read_qualtrics_csv("data/National_Top_20_MSA_Sample_v_22_final_sample.csv",
                         skiplines = 2, stringsAsFactors = FALSE)
varrecode = read.table("data/var_recodes.tsv", stringsAsFactors = FALSE, sep = "\t", header = TRUE)
names(dat) = tolower(varrecode$newname)



# trim responses of people who were filtered out --------------------------

dat = dat[dat$gc == 1 & !is.na(dat$gc), ]



# make sure zip code is character -----------------------------------------

dat$zip = sprintf(as.numeric(dat$zip), fmt="%05d")
dat$zip_selfreport = sprintf(as.numeric(dat$zip_selfreport), fmt="%05d")


# check quotas ------------------------------------------------------------

# gen income variable that corresponds to quotas
dat$inc_quota = dat$income
dat$inc_quota[dat$inc_quota %in% c("$75,000 – 99,999", 
                                   "$100,000 -119,999",
                                   "$120,000-$149,999",
                                   "$150,000 or greater")] = "$75,000+"
dat$inc_comb = with(dat, ifelse(income == "$150,000 or greater", income_over150, income))
increcode = "'Prefer not to answer' = NA;
             'Less than $15,000' = 10; '$15,000 - $24,999' = 20; '$25,000 - $34,999' = 30; 
             '$35,000 - $49,999' = 42.5; '$50,000 – 74,999' = 67.5; 
             '$75,000 – 99,999' = 87.5; '$100,000 -119,999' = 110;
             '$120,000-$149,999' = 135; '$150,000-$199,999' = 175;
             '$200,000-$299,999' = 250; '$300,000-$399,999' = 350;
             '$400,000 or greater' = 425"
dat$inc_num = car::recode(dat$inc_comb, increcode)


dat$sex = ifelse(dat$gender == "Male", "male",
                 ifelse(dat$gender == "Female", "Female", NA))

dat$age = dat$agegrp
dat$age[dat$age %in% c("25-34 years", "35-44 years")] = "25-44 years"
dat$age[dat$age %in% c("45-54 years", "55-64 years")] = "45-64 years"
dat$age[dat$age %in% c("65-74 years", "75 years and up")] = "65+ years"

race_dist = dat %>% group_by(region, race) %>% 
  summarise(n = n()) %>% mutate(prop = n / sum(n))
race_dist$category = paste0("race: ", race_dist$race)

age_dist = dat %>% group_by(region, age) %>% 
  summarise(n = n()) %>% mutate(prop = n / sum(n))
age_dist$category = paste0("age: ", age_dist$age)

sex_dist = dat %>% group_by(region, sex) %>% 
  summarise(n = n()) %>% mutate(prop = n / sum(n))
sex_dist$category = paste0("sex: ", sex_dist$sex)

inc_dist = dat %>% group_by(region, inc_quota) %>% 
  summarise(n = n()) %>% mutate(prop = n / sum(n))
inc_dist$category = paste0("income: ", inc_dist$inc_quota)

msa_dist = dat %>% group_by(region, msa) %>% 
  summarise(n = n()) %>% mutate(prop = n / sum(n))
msa_dist$category = paste0("msa: ", msa_dist$msa)

quotas = rbind(race_dist, sex_dist, inc_dist, msa_dist, age_dist)[, c("region", "category", "n", "prop")]
quotas = quotas[order(quotas$region, quotas$category),]
write.csv(quotas, "data/sample_demographics.csv", row.names = FALSE)



# Experimental conditions -------------------------------------------------

dat$nimby_control = dat$`do-bl-nimbysurveyexperiment` == "Q12.1"
dat$nimby_qtr_mile = dat$`do-bl-nimbysurveyexperiment` %in% c("Q12.3", "Q12.5")
dat$nimby_two_mile = dat$`do-bl-nimbysurveyexperiment` %in% c("Q12.4", "Q12.6")
dat$nimby_lowinc = dat$`do-bl-nimbysurveyexperiment` %in% c("Q12.2", "Q12.3", "Q12.4")
dat$nimby_market = dat$`do-bl-nimbysurveyexperiment` %in% c("Q12.5", "Q12.6")

dat$nimby_condition = NA
dat$nimby_condition[dat$nimby_control == T] = "control"
dat$nimby_condition[dat$nimby_lowinc == T] = "low income"
dat$nimby_condition[dat$nimby_lowinc == T & dat$nimby_qtr_mile == T] = "low inc/qtr mile"
dat$nimby_condition[dat$nimby_lowinc == T & dat$nimby_two_mile == T] = "low inc/two miles"
dat$nimby_condition[dat$nimby_market == T & dat$nimby_qtr_mile == T] = "market rate/qtr mile"
dat$nimby_condition[dat$nimby_market == T & dat$nimby_two_mile == T] = "market rate/two miles"


dat$gs_control = dat$`do-bl-housingconstructiongoldenstate` == "Q10.1"
dat$gs_econ = dat$`do-bl-housingconstructiongoldenstate` == "Q10.2"
dat$gs_econ_escape = dat$`do-bl-housingconstructiongoldenstate` == "Q10.3"
dat$gs_econ_fam = dat$`do-bl-housingconstructiongoldenstate` == "Q10.4"
dat$gs_condition = with(dat, 
                        ifelse(gs_control, "Control",
                        ifelse(gs_econ, "Economist",
                        ifelse(gs_econ_escape, "Economist/Escape",
                        ifelse(gs_econ_fam, "Economist/Families", NA)))))




# Combine randomized questions  ---------------------------------------
# a few questions were the same but were recorded as different questions due to
# randomization. combine these together. 


# building support
dat$build_apt = with(dat, 
                     ifelse(!(build_control_apt == ""), build_control_apt,
                     ifelse(!(build_econ_apt == ""), build_econ_apt,
                     ifelse(!(build_escape_apt == ""), build_escape_apt,
                     ifelse(!(build_lowinc_apt == ""), build_lowinc_apt, NA)))))
dat$build_ldh = with(dat, 
                     ifelse(!(build_control_ldh == ""), build_control_ldh,
                     ifelse(!(build_econ_ldh == ""), build_econ_ldh,
                     ifelse(!(build_escape_ldh == ""), build_escape_ldh,
                     ifelse(!(build_lowinc_ldh == ""), build_lowinc_ldh, NA)))))
dat$build_mixed = with(dat, 
                       ifelse(!(build_control_mixed == ""), build_control_mixed,
                       ifelse(!(build_econ_mixed == ""), build_econ_mixed,
                       ifelse(!(build_escape_mixed == ""), build_escape_mixed,
                       ifelse(!(build_lowinc_mixed == ""), build_lowinc_mixed, NA)))))
dat$build_hdh = with(dat, 
                     ifelse(!(build_control_hdh == ""), build_control_hdh,
                     ifelse(!(build_econ_hdh == ""), build_econ_hdh,
                     ifelse(!(build_escape_hdh == ""), build_escape_hdh,
                     ifelse(!(build_lowinc_hdh == ""), build_lowinc_hdh, NA)))))
dat$build_mfh = with(dat, 
                     ifelse(!(build_control_mfh == ""), build_control_mfh,
                     ifelse(!(build_econ_mfh == ""), build_econ_mfh,
                     ifelse(!(build_escape_mfh == ""), build_escape_mfh,
                     ifelse(!(build_lowinc_mfh == ""), build_lowinc_mfh, NA)))))
                                   

# nimby svy exp
dat$nimby_response = with(dat, 
                          ifelse(!(nimby1 == ""), nimby1,
                          ifelse(!(nimby2 == ""), nimby2,
                          ifelse(!(nimby3 == ""), nimby3,
                          ifelse(!(nimby4 == ""), nimby4,
                          ifelse(!(nimby5 == ""), nimby5,
                          ifelse(!(nimby6 == ""), nimby6, NA)))))))
dat$nimby_response_support = NA
dat$nimby_response_support = ifelse(dat$nimby_response %in% c("Somewhat support", "Strongly support"), 1, 0)
dat$nimby_response_oppose = ifelse(dat$nimby_response %in% c("Somewhat oppose", "Strongly oppose"), 1, 0)

# housing concerns
dat$concerns_lowinc = with(dat, 
                           ifelse(!(concerns1_lowinc == ""), 1,
                           ifelse(!(concerns2_lowinc == ""), 1, 0)))
dat$concerns_midinc = with(dat, 
                           ifelse(!(concerns1_midinc == ""), 1,
                           ifelse(!(concerns2_midinc == ""), 1, 0)))
dat$concerns_cantafford = with(dat, 
                               ifelse(!(concerns1_cantafford == ""), 1,
                               ifelse(!(concerns2_cantafford == ""), 1, 0)))
dat$concerns_homevalue = with(dat, 
                              ifelse(!(concerns1_homevalue == ""), 1,
                              ifelse(!(concerns2_homevalue == ""), 1, 0)))
dat$concerns_younggen = with(dat, 
                             ifelse(!(concerns1_younggen == ""), 1,
                             ifelse(!(concerns2_younggen == ""), 1, 0)))
dat$concerns_commute = with(dat, 
                            ifelse(!(concerns1_commute == ""), 1,
                            ifelse(!(concerns2_commute == ""), 1, 0)))
dat$concerns_none = with(dat, 
                         ifelse(!(concerns1_none == ""), 1,
                         ifelse(!(concerns2_none == ""), 1, 0)))
dat$concerns_notsure = with(dat, 
                            ifelse(!(concerns1_notsure == ""), 1,
                            ifelse(!(concerns2_notsure == ""), 1, 0)))



# clean up other questions -----------------------------------------------------

dat$homeowner = dat$ownrent == "I own it"
dat$renter = dat$ownrent == "I rent it"

dat$pid5 = NA
dat$pid5[dat$partyid == "Democrat"] = 1
dat$pid5[dat$partylean == "Democratic Party"] = 2
dat$pid5[(dat$partyid == "Independent" | dat$partyid == "Other") & dat$partylean == "Neither"] = 3
dat$pid5[dat$partylean == "Republican Party"] = 4
dat$pid5[dat$partyid == "Republican"] = 5
dat$party = recode(dat$pid5, 
                   "c(1, 2) = 'Democrat';
                    c(4, 5) = 'Republican';
                    3 = 'Independent';
                    else = NA")

dat$voted2016 = recode(dat$voted2016, recodes = "'I am sure I voted in the presidential election' = 1; else = 0")

dat$locgov_attendmeet = recode(dat$locgov_attendmeet, recodes = "'Yes' = 1; else = 0")
dat$locgov_contact = recode(dat$locgov_contact, recodes = "'Yes' = 1; else = 0")
dat$locgov_vote_num = recode(dat$locgov_vote, 
                             "'Never' = 0; 'Rarely' = 1; 'Sometimes' = 2; 
                              'Often' = 3; 'Always' = 4; else = NA")
dat$locgov_news_num = recode(dat$locgov_news,
                             "'Never' = 0; 'About once a month' = 1; 
                              'Once or twice a week' = 2; 
                              'Three or four times a week' = 3;
                              'Every day' = 4; else=NA")

dat$metro_housing_perc = gsub("for the housing you get", "", dat$metro_housing_perc)
dat$state_housing_perc = gsub("for the housing you get", "", dat$state_housing_perc)



# redistribution questions ------------------------------------------------

# redistribution questions:  high = high redist
dat$redist_housing = substr(dat$redist_housing, 1, 1) %>% as.numeric 
dat$redist_housing = dat$redist_housing * -1 + 6

dat$redist_incdiff = substr(dat$redist_incdiff, 1, 1) %>% as.numeric
dat$redist_incdiff = dat$redist_incdiff * -1 + 6

dat$redist_tax = recode(dat$redist_tax, 
                        "'No, our government should not redistribute wealth through much higher taxes on the rich' = 0;
                        'No opinion' = 1;
                        'Yes, our government should redistribute wealth through much higher taxes on the rich'= 2")
  
dat$freemarket = recode(dat$freemarket, 
                        "'Strongly agree' = 1;
                         'Somewhat agree' = 2;
                         'Neither agree nor disagree' = 3;
                         'Somewhat disagree' = 4;
                         'Strongly disagree' = 5")

# generate redistribution indices. first with the housing q, then wihout.
pca.vars = c("redist_incdiff", "redist_housing", "freemarket", "redist_tax")
impute.vars = c("sex", "income", "agegrp", "pid5", "educ", "race")
redist.score = genPCAscores(data = dat, pca.vars = pca.vars, testvar = "redist_incdiff",
                            impute.vars = impute.vars, 
                            noms = c("educ", "agegrp", "income", "sex", "race"))
dat$redist.index = redist.score$index
dat$redist_terc = tercileAssign(dat$redist.index)

# without housing
pca.vars = c("redist_incdiff", "freemarket", "redist_tax")
impute.vars = c("sex", "income", "agegrp", "pid5", "educ", "race")
redist.score2 = genPCAscores(data = dat, pca.vars = pca.vars, testvar = "redist_incdiff",
                             impute.vars = impute.vars, 
                             noms = c("educ", "agegrp", "income", "sex", "race"))
dat$redist.index2 = redist.score2$index
dat$redist_terc2 = tercileAssign(dat$redist.index2)



# local political involvement ---------------------------------------------


pca.vars = c("locgov_attendmeet", "locgov_contact", "locgov_vote_num", "locgov_news_num")
impute.vars = c("sex", "income", "agegrp", "pid5", "educ", "race")
locgov.score = genPCAscores(dat, pca.vars = pca.vars, testvar = "locgov_vote_num",
                            impute.vars = impute.vars, 
                            noms = c("educ", "agegrp", "income", "sex", "race"),
                            ords = c("locgov_attendmeet", "locgov_contact", "locgov_vote_num", 
                                     "locgov_news_num"))
dat$locgov.score = locgov.score$index
dat$locgov_terc = tercileAssign(dat$locgov.score)



# generate some more variables related to affect --------------------------


# do you trust renters or homeowners more? 
dat$trust_rentown_diff = as.integer(as.factor(dat$trust_homeowners)) - as.integer(as.factor(dat$trust_aptrent))
dat$trust_owners_more = dat$trust_rentown_diff > 0
dat$trust_aptrent_binary = !grepl("Distrust", dat$trust_aptrent)

# racial stereotypes - create negative affect score as total number of 
# negative traits ascribed to blacks
dat$black_negative_affect = with(dat, 
                                   (black_intel == "Unintelligent") + 
                                   (black_peace == "Violent") + 
                                   (black_hardworking == "Lazy"))
dat$black_negative_affect_binary = dat$black_negative_affect > 0

dat$white_negative_affect = with(dat, 
                                   (white_intel == "Unintelligent") + 
                                   (white_peace == "Violent") + 
                                   (white_hardworking == "Lazy"))
dat$racial_diff = with(dat, white_negative_affect - black_negative_affect)


# indicator for distrusting developers
dat$distrust_developers = grepl("Distrust", dat$trust_realestate)

# indicator that they support housing for all
dat$fed_housing_bin = dat$redist_housing > 3

# free market 
dat$freemarket_bin = dat$freemarket < 3

# education
dat$hsless = dat$educ %in% c("High School / GED", "Less than High School")
dat$bahigher = dat$educ %in% c("4-year College Degree", "Doctoral Degree", "Professional Degree (JD, MD)", "Masters Degree")

save(dat, file = "data/cleaned_top20.RData")

