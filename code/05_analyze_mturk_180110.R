# Replication archive for "Where Self-Interest Trumps Ideology: Liberal
# Homeowners and Local Opposition to Housing Development" by William Marble
# and Clayton Nall. 

# Clean and analyze MTurk survey from January 2018. Results are in the online
# appendix.

library(sandwich)
library(dplyr)
library(stargazer)
library(ggplot2)
library(ggthemes)
library(lmtest)
library(car)

options(stringsAsFactors = FALSE)

source("code/functions.R")
dat = read.csv("data/Jan2018_MTurk_housing_survey.csv")

select <- dplyr::select

# clean data --------------------------------------------------------------


# drop people who fail speed trap
dat = dat %>% filter(speedtrap == 4)

# rename some vars
dat = rename(dat, trust_locgov =  trust_2, 
                  trust_stategov =  trust_3,
                  trust_fedgov =  trust_4,
                  trust_dev =  trust_5,
                  trust_corp =  trust_6,
                  trust_homeowners =  trust_7,
                  trust_renters =  trust_8,
                  trust_econ =  trust_14,
                  trust_collegeprof =  trust_15,
                  trust_govscientist =  trust_16,
                  trust_corpscientists =  trust_17)

# combine the outcome vars
dat = dat %>% mutate(build_apt = NA,
                     build_mixed = NA,
                     build_mfh = NA,
                     build_hdh = NA,
                     build_ldh = NA, 
                     treatment = NA)


# create treatment variable
dat$treatment = with(dat, ifelse(!is.na(build_control_1) |
                                 !is.na(build_control_4) |
                                 !is.na(build_control_5) |
                                 !is.na(build_control_6) |
                                 !is.na(build_control_7), "Control", treatment))

dat$treatment = with(dat, ifelse(  !is.na(build_univ_1) |
                                   !is.na(build_univ_4) |
                                   !is.na(build_univ_5) |
                                   !is.na(build_univ_6) |
                                   !is.na(build_univ_7), "Explanation", treatment))

dat$treatment = with(dat, ifelse(  !is.na(build_economist_1) |
                                   !is.na(build_economist_4) |
                                   !is.na(build_economist_5) |
                                   !is.na(build_economist_6) |
                                   !is.na(build_economist_7), "Economist", treatment))


# fill in variables for "economist" condition
dat$build_apt   = with(dat, ifelse(treatment == "Economist", build_economist_1, build_apt))
dat$build_mixed = with(dat, ifelse(treatment == "Economist", build_economist_6, build_mixed))
dat$build_mfh   = with(dat, ifelse(treatment == "Economist", build_economist_7, build_mfh))
dat$build_hdh   = with(dat, ifelse(treatment == "Economist", build_economist_5, build_hdh))
dat$build_ldh   = with(dat, ifelse(treatment == "Economist", build_economist_4, build_ldh))


# fill in variables for "control" condition
dat$build_apt   = with(dat, ifelse(treatment == "Control", build_control_1, build_apt))
dat$build_mixed = with(dat, ifelse(treatment == "Control", build_control_6, build_mixed))
dat$build_mfh   = with(dat, ifelse(treatment == "Control", build_control_7, build_mfh))
dat$build_hdh   = with(dat, ifelse(treatment == "Control", build_control_5, build_hdh))
dat$build_ldh   = with(dat, ifelse(treatment == "Control", build_control_4, build_ldh))


# fill in variables for "explanation" condition
dat$build_apt   = with(dat, ifelse(treatment == "Explanation", build_univ_1, build_apt))
dat$build_mixed = with(dat, ifelse(treatment == "Explanation", build_univ_6, build_mixed))
dat$build_mfh   = with(dat, ifelse(treatment == "Explanation", build_univ_7, build_mfh))
dat$build_hdh   = with(dat, ifelse(treatment == "Explanation", build_univ_5, build_hdh))
dat$build_ldh   = with(dat, ifelse(treatment == "Explanation", build_univ_4, build_ldh))


# drop original outcome vars
dat = dat %>%
  select(-starts_with("build_economist"), -starts_with("build_control"), -starts_with("build_univ"))








# trust variables ---------------------------------------------------------

dat = dat %>% mutate(econ_nobias = as.numeric(trust_econ_multi == "1"),
                     econ_trust_bin = as.numeric(trust_econ %in% c(1, 2)))
dat = dat %>% mutate(econ_trust_summary = econ_nobias + econ_trust_bin)




# demographics/respondent characteristics ---------------------------------

dat = dat %>% mutate(homeowner = as.numeric(ownrent == 4),
                     female = as.numeric(gender == 2),
                     ba = as.numeric(educ >= 4))
dat$pid5 = NA
dat$pid5[dat$pid == 1] = 1 
dat$pid5[dat$pid == 2] = 5
dat$pid5[dat$pid %in% c(3, 4) & dat$pidlean == 1] = 2
dat$pid5[dat$pid %in% c(3, 4) & dat$pidlean == 2] = 4
dat$pid5[dat$pid %in% c(3, 4) & dat$pidlean == 3] = 3
dat$pid3 = car::recode(dat$pid5, "c(1,2) = 1; 3 = 2; c(4,5) = 3")

# income in thousands (midpoint of bins, $450k for $400+, $7.5k for <$15k)
dat$inc_num = car::recode(dat$faminc, 
                          "12 = 7.5; 
                           13 = 20;
                           1 = 30;
                           2 = 42.5;
                           4 = 57.5;
                           6 = 87.5;
                           9 = 110;
                           10 = 130")
dat$inc_num[dat$faminc == 11] = car::recode(dat$faminc150[dat$faminc==11],
                                            "11 = 175;
                                             12 = 250;
                                             13 = 350; 
                                             14 = 450")




# manipulation check ------------------------------------------------------


# outcome variable: correctly say that economist think increasing supply
# decreases costs and saying that respondent themself thinks that. 
dat$manip_econ[dat$manip_econ %in% c(2, 3)] = 0
dat$manip_resp[dat$manip_resp %in% c(2, 3)] = 0

check_econ = lm(manip_econ ~ treatment, dat)
check_econ_vcv = vcovHC(check_econ, "HC2")
coeftest(check_econ, check_econ_vcv)
linearHypothesis(check_econ, "treatmentEconomist = treatmentExplanation", vcov. = check_econ_vcv)


check_econ_hitrust = lm(manip_econ ~ treatment, subset(dat, econ_nobias == 1))
check_econ_hitrust_vcv = vcovHC(check_econ_hitrust, "HC2")
coeftest(check_econ_hitrust, check_econ_hitrust_vcv)
linearHypothesis(check_econ_hitrust, "treatmentEconomist = treatmentExplanation", vcov. = check_econ_hitrust_vcv)


check_econ_lotrust = lm(manip_econ ~ treatment, subset(dat, econ_nobias == 0))
check_econ_lotrust_vcv = vcovHC(check_econ_lotrust, "HC2")
coeftest(check_econ_lotrust, check_econ_lotrust_vcv)
linearHypothesis(check_econ_lotrust, "treatmentEconomist = treatmentExplanation", vcov. = check_econ_lotrust_vcv)

check_econ_interact = lm(manip_econ ~ treatment * econ_nobias, dat)
check_econ_interact_vcv = vcovHC(check_econ_interact, "HC2")
coeftest(check_econ_interact, check_econ_interact_vcv)
stargazer(check_econ_interact, se = list(sqrt(diag(check_econ_interact_vcv)) ),
          type = "text", no.space = TRUE)


check_resp = lm(manip_resp ~ treatment, dat)
check_resp_vcv = vcovHC(check_resp, "HC2")
coeftest(check_resp, check_resp_vcv)
linearHypothesis(check_resp, "treatmentEconomist = treatmentExplanation", vcov. = check_resp_vcv)


check_resp_hitrust = lm(manip_resp ~ treatment, subset(dat, econ_nobias == 1))
check_resp_hitrust_vcv = vcovHC(check_resp_hitrust, "HC2")
coeftest(check_resp_hitrust, check_resp_hitrust_vcv)
linearHypothesis(check_resp_hitrust, "treatmentEconomist = treatmentExplanation", vcov. = check_resp_hitrust_vcv)


check_resp_lotrust = lm(manip_resp ~ treatment, subset(dat, econ_nobias == 0))
check_resp_lotrust_vcv = vcovHC(check_resp_lotrust, "HC2")
coeftest(check_resp_lotrust, check_resp_lotrust_vcv)
linearHypothesis(check_resp_lotrust, "treatmentEconomist = treatmentExplanation", vcov. = check_resp_lotrust_vcv)



## Appendix Table A-10
stargazer(check_resp, check_resp_hitrust, check_resp_lotrust,
          check_econ, check_econ_hitrust, check_econ_lotrust,
          se = lapply(list(check_resp_vcv, check_resp_hitrust_vcv, check_resp_lotrust_vcv,
                           check_econ_vcv, check_econ_hitrust_vcv, check_econ_lotrust_vcv), 
                      function(x) sqrt(diag(x))),
          dep.var.labels = c("Respondent's Own Beliefs", "Beliefs about economists"),
          no.space = TRUE, add.lines = list(c("Sample", "Full", "High trust", "Low trust", "Full", "High trust", "Low trust")),
          covariate.labels = c("Economist", "Explanation"),
          align=TRUE)


## xtabs of the two manipulation checks, by treatment condition ##
xtab.control = xtabs( ~ manip_econ + manip_resp, dat, subset = treatment == "Control")
xtab.econ = xtabs( ~ manip_econ + manip_resp, dat, subset = treatment == "Economist")
xtab.expl = xtabs( ~ manip_econ + manip_resp, dat, subset = treatment == "Explanation")

lapply(list(xtab.control, xtab.econ, xtab.expl), function(x) round(prop.table(x), 2))



# see that the treatments increase the proportion of people getting both 
# manipulation checks "correct"
bothright =  lm(I(manip_econ & manip_resp) ~ treatment, dat)
bothright.vcv = vcovHC(bothright, "HC2")
coeftest(bothright, bothright.vcv)


## figure for manipulation check ##
manip_econ_sum = dat %>% group_by(treatment) %>% 
  summarise(coef = mean(manip_econ, na.rm=TRUE),
            se = sd(manip_econ, na.rm=TRUE) / sqrt(sum(!is.na(manip_econ))))
manip_econ_sum$type = "Economists' beliefs"
manip_resp_sum = dat %>% group_by(treatment) %>% 
  summarise(coef = mean(manip_resp, na.rm=TRUE),
            se = sd(manip_resp, na.rm=TRUE) / sqrt(sum(!is.na(manip_resp))))
manip_resp_sum$type = "Own beliefs"
manip_sum = rbind(manip_econ_sum, manip_resp_sum)
manip_sum$treatment = factor(manip_sum$treatment, rev(c("Control", "Economist", "Explanation")))

ggplot(manip_sum) + 
  aes(y = treatment, x = coef, xmin = coef - 2*se, xmax = coef + 2*se, colour=type) + 
  geom_errorbarh(height=0, position = position_dodgev(height=.25)) + 
  geom_point(position = position_dodgev(height=.25)) + 
  theme_bw() + 
  scale_colour_hc(name=NULL) + 
  theme(panel.spacing = unit(0, "in"), legend.position = "bottom") +
  labs(x = "Proportion answering that more housing reduces costs", y = NULL)
# ggsave("figs/manip_check.pdf", width=6, height=4)



## broke down by trust in economists ## 
manip_econ_sum_trust = dat %>% group_by(treatment, econ_nobias) %>% 
  summarise(coef = mean(manip_econ, na.rm=TRUE),
            se = sd(manip_econ, na.rm=TRUE) / sqrt(sum(!is.na(manip_econ))))
manip_econ_sum_trust$type = "Economists' beliefs"

manip_resp_sum_trust = dat %>% group_by(treatment, econ_nobias) %>% 
  summarise(coef = mean(manip_resp, na.rm=TRUE),
            se = sd(manip_resp, na.rm=TRUE) / sqrt(sum(!is.na(manip_resp))))
manip_resp_sum_trust$type = "Own beliefs"
manip_sum_trust = rbind(manip_econ_sum_trust, manip_resp_sum_trust)
manip_sum_trust$econ_nobias = ifelse(manip_sum_trust$econ_nobias == 1, "Trusts economists", "Doesn't trust economists")
manip_sum_trust$econ_nobias = factor(manip_sum_trust$econ_nobias, c("Trusts economists", "Doesn't trust economists"))
manip_sum$treatment = factor(manip_sum$treatment, rev(c("Control", "Economist", "Explanation")))


ggplot(manip_sum_trust) + 
  aes(y = treatment, x = coef, xmin = coef - 2*se, xmax = coef + 2*se, colour=type, group=type) + 
  geom_errorbarh(height=0, position = position_dodgev(height=.25)) + 
  geom_point(position = position_dodgev(height=.25)) + 
  theme_bw() + 
  scale_colour_hc(name=NULL) + 
  theme(panel.spacing = unit(0, "in"), legend.position = "bottom") +
  facet_wrap(~econ_nobias) +
  labs(x = "Proportion answering that more housing reduces costs", y = NULL)
# ggsave("figs/mturk180110/manip_check_bytrust.pdf", width=6, height=4)


# what correlates w/ trust in experts? ------------------------------------

trust_homeown = lm(econ_nobias ~ homeowner, dat)
trust_homeown_vcv = vcovHC(trust_homeown, "HC2")
coeftest(trust_homeown, trust_homeown_vcv)

trust_educ = lm(econ_nobias ~ ba, dat)
trust_educ_vcv = vcovHC(trust_educ, "HC2")
coeftest(trust_educ, trust_educ_vcv)

lm(econ_nobias ~ factor(educ), dat) %>% summary
lm(econ_nobias ~ factor(pid5), dat) %>% summary
lm(econ_nobias ~ inc_num, dat) %>% summary
ggplot(dat, aes(x = inc_num, y = econ_nobias)) + 
  geom_smooth() + 
  geom_rug(sides = "b", position = position_jitter(5), alpha = .3) + 
  coord_cartesian(ylim = c(0,1))


lm(econ_nobias ~ homeowner + female + ba + factor(pid3) + inc_num + I(inc_num^2), dat) %>% summary


# treatment effects -------------------------------------------------------
dat = dat %>% mutate(apt_bin = as.numeric(build_apt %in% c(1, 2)),
                     mixed_bin = as.numeric(build_mixed %in% c(1, 2)),
                     mfh_bin = as.numeric(build_mfh %in% c(1, 2)),
                     hdh_bin = as.numeric(build_hdh %in% c(1, 2)),
                     ldh_bin = as.numeric(build_ldh %in% c(1, 2)))
dat$sum_dev = with(dat, apt_bin + mixed_bin + mfh_bin + hdh_bin)
cor(subset(dat, treatment == "Control", select = c("apt_bin", "mixed_bin", "mfh_bin", "hdh_bin", "ldh_bin")))

# apartment
apt_mod = lm(apt_bin ~ treatment, dat)
apt_mod_vcv = vcovHC(apt_mod, "HC2")
coeftest(apt_mod, apt_mod_vcv)
linearHypothesis(apt_mod, "treatmentEconomist = treatmentExplanation", vcov. = apt_mod_vcv)


# mixed use
mixed_mod = lm(mixed_bin ~ treatment, dat)
mixed_mod_vcv = vcovHC(mixed_mod, "HC2")
coeftest(mixed_mod, mixed_mod_vcv)
linearHypothesis(mixed_mod, "treatmentEconomist = treatmentExplanation", vcov. = mixed_mod_vcv)


# multifam
mfh_mod = lm(mfh_bin ~ treatment, dat)
mfh_mod_vcv = vcovHC(mfh_mod, "HC2")
coeftest(mfh_mod, mfh_mod_vcv)
linearHypothesis(mfh_mod, "treatmentEconomist = treatmentExplanation", vcov. = mfh_mod_vcv)


# high density SFH
hdh_mod = lm(hdh_bin ~ treatment, dat)
hdh_mod_vcv = vcovHC(hdh_mod, "HC2")
coeftest(hdh_mod, hdh_mod_vcv)
linearHypothesis(hdh_mod, "treatmentEconomist = treatmentExplanation", vcov. = hdh_mod_vcv)


# low density SFH
ldh_mod = lm(ldh_bin ~ treatment, dat)
ldh_mod_vcv = vcovHC(ldh_mod, "HC2")
coeftest(ldh_mod, ldh_mod_vcv)
linearHypothesis(ldh_mod, "treatmentEconomist = treatmentExplanation", vcov. = ldh_mod_vcv)


# numer of high density types
sum_mod = lm(sum_dev ~ treatment, dat)
sum_mod_vcv = vcovHC(sum_mod, "HC2")
coeftest(sum_mod, sum_mod_vcv)
linearHypothesis(sum_mod, "treatmentEconomist = treatmentExplanation", vcov. = sum_mod_vcv)





# among high econ trust ---------------------------------------------------

# apartment
apt_mod = lm(apt_bin ~ treatment, subset(dat, econ_nobias == 1))
apt_mod_vcv = vcovHC(apt_mod, "HC2")
coeftest(apt_mod, apt_mod_vcv)
linearHypothesis(apt_mod, "treatmentEconomist = treatmentExplanation", vcov. = apt_mod_vcv)


# mixed use
mixed_mod = lm(mixed_bin ~ treatment, subset(dat, econ_nobias == 1))
mixed_mod_vcv = vcovHC(mixed_mod, "HC2")
coeftest(mixed_mod, mixed_mod_vcv)
linearHypothesis(mixed_mod, "treatmentEconomist = treatmentExplanation", vcov. = mixed_mod_vcv)


# multifam
mfh_mod = lm(mfh_bin ~ treatment, subset(dat, econ_nobias == 1))
mfh_mod_vcv = vcovHC(mfh_mod, "HC2")
coeftest(mfh_mod, mfh_mod_vcv)
linearHypothesis(mfh_mod, "treatmentEconomist = treatmentExplanation", vcov. = mfh_mod_vcv)


# high density SFH
hdh_mod = lm(hdh_bin ~ treatment, subset(dat, econ_nobias == 1))
hdh_mod_vcv = vcovHC(hdh_mod, "HC2")
coeftest(hdh_mod, hdh_mod_vcv)
linearHypothesis(hdh_mod, "treatmentEconomist = treatmentExplanation", vcov. = hdh_mod_vcv)


# low density SFH
ldh_mod = lm(ldh_bin ~ treatment, subset(dat, econ_nobias == 1))
ldh_mod_vcv = vcovHC(ldh_mod, "HC2")
coeftest(ldh_mod, ldh_mod_vcv)
linearHypothesis(ldh_mod, "treatmentEconomist = treatmentExplanation", vcov. = ldh_mod_vcv)


# numer of high density types
sum_mod = lm(sum_dev ~ treatment, subset(dat, econ_nobias == 1))
sum_mod_vcv = vcovHC(sum_mod, "HC2")
coeftest(sum_mod, sum_mod_vcv)
linearHypothesis(sum_mod, "treatmentEconomist = treatmentExplanation", vcov. = sum_mod_vcv)



# High econ trust + homeowner ---------------------------------------------


# apartment
apt_mod = lm(apt_bin ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 1))
apt_mod_vcv = vcovHC(apt_mod, "HC2")
coeftest(apt_mod, apt_mod_vcv)
linearHypothesis(apt_mod, "treatmentEconomist = treatmentExplanation", vcov. = apt_mod_vcv)


# mixed use
mixed_mod = lm(mixed_bin ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 1))
mixed_mod_vcv = vcovHC(mixed_mod, "HC2")
coeftest(mixed_mod, mixed_mod_vcv)
linearHypothesis(mixed_mod, "treatmentEconomist = treatmentExplanation", vcov. = mixed_mod_vcv)


# multifam
mfh_mod = lm(mfh_bin ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 1))
mfh_mod_vcv = vcovHC(mfh_mod, "HC2")
coeftest(mfh_mod, mfh_mod_vcv)
linearHypothesis(mfh_mod, "treatmentEconomist = treatmentExplanation", vcov. = mfh_mod_vcv)


# high density SFH
hdh_mod = lm(hdh_bin ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 1))
hdh_mod_vcv = vcovHC(hdh_mod, "HC2")
coeftest(hdh_mod, hdh_mod_vcv)
linearHypothesis(hdh_mod, "treatmentEconomist = treatmentExplanation", vcov. = hdh_mod_vcv)


# low density SFH
ldh_mod = lm(ldh_bin ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 1))
ldh_mod_vcv = vcovHC(ldh_mod, "HC2")
coeftest(ldh_mod, ldh_mod_vcv)
linearHypothesis(ldh_mod, "treatmentEconomist = treatmentExplanation", vcov. = ldh_mod_vcv)


# numer of high density types
sum_mod = lm(sum_dev ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 1))
sum_mod_vcv = vcovHC(sum_mod, "HC2")
coeftest(sum_mod, sum_mod_vcv)
linearHypothesis(sum_mod, "treatmentEconomist = treatmentExplanation", vcov. = sum_mod_vcv)




# High econ trust + homeowner ---------------------------------------------


# apartment
apt_mod = lm(apt_bin ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 0))
apt_mod_vcv = vcovHC(apt_mod, "HC2")
coeftest(apt_mod, apt_mod_vcv)
linearHypothesis(apt_mod, "treatmentEconomist = treatmentExplanation", vcov. = apt_mod_vcv)


# mixed use
mixed_mod = lm(mixed_bin ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 0))
mixed_mod_vcv = vcovHC(mixed_mod, "HC2")
coeftest(mixed_mod, mixed_mod_vcv)
linearHypothesis(mixed_mod, "treatmentEconomist = treatmentExplanation", vcov. = mixed_mod_vcv)


# multifam
mfh_mod = lm(mfh_bin ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 0))
mfh_mod_vcv = vcovHC(mfh_mod, "HC2")
coeftest(mfh_mod, mfh_mod_vcv)
linearHypothesis(mfh_mod, "treatmentEconomist = treatmentExplanation", vcov. = mfh_mod_vcv)


# high density SFH
hdh_mod = lm(hdh_bin ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 0))
hdh_mod_vcv = vcovHC(hdh_mod, "HC2")
coeftest(hdh_mod, hdh_mod_vcv)
linearHypothesis(hdh_mod, "treatmentEconomist = treatmentExplanation", vcov. = hdh_mod_vcv)


# low density SFH
ldh_mod = lm(ldh_bin ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 0))
ldh_mod_vcv = vcovHC(ldh_mod, "HC2")
coeftest(ldh_mod, ldh_mod_vcv)
linearHypothesis(ldh_mod, "treatmentEconomist = treatmentExplanation", vcov. = ldh_mod_vcv)


# numer of high density types
sum_mod = lm(sum_dev ~ treatment, subset(dat, econ_nobias == 1 & homeowner == 0))
sum_mod_vcv = vcovHC(sum_mod, "HC2")
coeftest(sum_mod, sum_mod_vcv)
linearHypothesis(sum_mod, "treatmentEconomist = treatmentExplanation", vcov. = sum_mod_vcv)




# among low econ trust ----------------------------------------------------


# apartment
apt_mod = lm(apt_bin ~ treatment, subset(dat, econ_nobias == 0))
apt_mod_vcv = vcovHC(apt_mod, "HC2")
coeftest(apt_mod, apt_mod_vcv)

# mixed use
mixed_mod = lm(mixed_bin ~ treatment, subset(dat, econ_nobias == 0))
mixed_mod_vcv = vcovHC(mixed_mod, "HC2")
coeftest(mixed_mod, mixed_mod_vcv)

# multifam
mfh_mod = lm(mfh_bin ~ treatment, subset(dat, econ_nobias == 0))
mfh_mod_vcv = vcovHC(mfh_mod, "HC2")
coeftest(mfh_mod, mfh_mod_vcv)

# high density SFH
hdh_mod = lm(hdh_bin ~ treatment, subset(dat, econ_nobias == 0))
hdh_mod_vcv = vcovHC(hdh_mod, "HC2")
coeftest(hdh_mod, hdh_mod_vcv)

# low density SFH
ldh_mod = lm(ldh_bin ~ treatment, subset(dat, econ_nobias == 0))
ldh_mod_vcv = vcovHC(ldh_mod, "HC2")
coeftest(ldh_mod, ldh_mod_vcv)

# numer of high density types
sum_mod = lm(sum_dev ~ treatment, subset(dat, econ_nobias == 0))
sum_mod_vcv = vcovHC(sum_mod, "HC2")
coeftest(sum_mod, sum_mod_vcv)
linearHypothesis(sum_mod, "treatmentEconomist = treatmentExplanation", vcov. = sum_mod_vcv)



# among homeowners --------------------------------------------------------


# apartment
apt_mod = lm(apt_bin ~ treatment, subset(dat, homeowner == 1))
apt_mod_vcv = vcovHC(apt_mod, "HC2")
coeftest(apt_mod, apt_mod_vcv)

# mixed use
mixed_mod = lm(mixed_bin ~ treatment, subset(dat, homeowner == 1))
mixed_mod_vcv = vcovHC(mixed_mod, "HC2")
coeftest(mixed_mod, mixed_mod_vcv)

# multifam
mfh_mod = lm(mfh_bin ~ treatment, subset(dat, homeowner == 1))
mfh_mod_vcv = vcovHC(mfh_mod, "HC2")
coeftest(mfh_mod, mfh_mod_vcv)

# high density SFH
hdh_mod = lm(hdh_bin ~ treatment, subset(dat, homeowner == 1))
hdh_mod_vcv = vcovHC(hdh_mod, "HC2")
coeftest(hdh_mod, hdh_mod_vcv)

# low density SFH
ldh_mod = lm(ldh_bin ~ treatment, subset(dat, homeowner == 1))
ldh_mod_vcv = vcovHC(ldh_mod, "HC2")
coeftest(ldh_mod, ldh_mod_vcv)

# numer of high density types
sum_mod = lm(sum_dev ~ treatment, subset(dat, homeowner == 1))
sum_mod_vcv = vcovHC(sum_mod, "HC2")
coeftest(sum_mod, sum_mod_vcv)
linearHypothesis(sum_mod, "treatmentEconomist = treatmentExplanation", vcov. = sum_mod_vcv)





# among renters -----------------------------------------------------------



# apartment
apt_mod = lm(apt_bin ~ treatment, subset(dat, homeowner == 0))
apt_mod_vcv = vcovHC(apt_mod, "HC2")
coeftest(apt_mod, apt_mod_vcv)

# mixed use
mixed_mod = lm(mixed_bin ~ treatment, subset(dat, homeowner == 0))
mixed_mod_vcv = vcovHC(mixed_mod, "HC2")
coeftest(mixed_mod, mixed_mod_vcv)

# multifam
mfh_mod = lm(mfh_bin ~ treatment, subset(dat, homeowner == 0))
mfh_mod_vcv = vcovHC(mfh_mod, "HC2")
coeftest(mfh_mod, mfh_mod_vcv)

# high density SFH
hdh_mod = lm(hdh_bin ~ treatment, subset(dat, homeowner == 0))
hdh_mod_vcv = vcovHC(hdh_mod, "HC2")
coeftest(hdh_mod, hdh_mod_vcv)

# low density SFH
ldh_mod = lm(ldh_bin ~ treatment, subset(dat, homeowner == 0))
ldh_mod_vcv = vcovHC(ldh_mod, "HC2")
coeftest(ldh_mod, ldh_mod_vcv)

# numer of high density types
sum_mod = lm(sum_dev ~ treatment, subset(dat, homeowner == 0))
sum_mod_vcv = vcovHC(sum_mod, "HC2")
coeftest(sum_mod, sum_mod_vcv)
linearHypothesis(sum_mod, "treatmentEconomist = treatmentExplanation", vcov. = sum_mod_vcv)




# create own/rent graph -------------------------------------------------------

# Appendix Figure A-7

apt_sum_ownrent = dat %>% group_by(homeowner, treatment) %>% 
  summarise(coef = mean(apt_bin, na.rm=TRUE),
            se = sd(apt_bin, na.rm=TRUE) / sqrt(sum(!is.na(apt_bin))))
apt_sum_ownrent$type = "Apt-Only Buildings"

mixed_sum_ownrent = dat %>% group_by(homeowner, treatment) %>% 
  summarise(coef = mean(mixed_bin, na.rm=TRUE),
            se = sd(mixed_bin, na.rm=TRUE) / sqrt(sum(!is.na(mixed_bin))))
mixed_sum_ownrent$type = "Mixed-use"

mfh_sum_ownrent = dat %>% group_by(homeowner, treatment) %>% 
  summarise(coef= mean(mfh_bin, na.rm=TRUE),
            se =sd(mfh_bin, na.rm=TRUE) / sqrt(sum(!is.na(mfh_bin))))
mfh_sum_ownrent$type = "Townhouses"

hdh_sum_ownrent = dat %>%  group_by(homeowner, treatment) %>% 
  summarise(coef = mean(hdh_bin, na.rm=TRUE),
            se = sd(hdh_bin, na.rm=TRUE) / sqrt(sum(!is.na(hdh_bin))))
hdh_sum_ownrent$type = "High-Density SFH"

ldh_sum_ownrent = dat %>% group_by(homeowner, treatment) %>% 
  summarise(coef = mean(ldh_bin, na.rm=TRUE),
            se = sd(ldh_bin, na.rm=TRUE) / sqrt(sum(!is.na(ldh_bin))))
ldh_sum_ownrent$type = "Low-Density SFH"

sum_ownrent = rbind(apt_sum_ownrent, mixed_sum_ownrent, mfh_sum_ownrent, ldh_sum_ownrent, hdh_sum_ownrent)
sum_ownrent$treatment = factor(sum_ownrent$treatment, c("Explanation", "Economist", "Control"))
sum_ownrent$type = factor(sum_ownrent$type, unique(sum_ownrent$type))
sum_ownrent$homeowner = car::recode(sum_ownrent$homeowner, "1 = 'Homeowner'; 0 = 'Renter'")

ggplot(sum_ownrent) + 
  aes(y = treatment, x = coef, xmin = coef - 2*se, xmax = coef + 2*se, colour = homeowner) + 
  geom_errorbarh(height=0, position = position_dodgev(height=.5)) + 
  geom_point(position = position_dodgev(height=.5)) + 
  facet_wrap(~type) + 
  scale_colour_hc(name=NULL) + 
  guides(colour = guide_legend(rev = TRUE)) + 
  labs(x = "Support", y = NULL) + 
  theme_bw() + 
  theme(legend.position = c(.85, .25))
ggsave("figs/f_a7.eps", width=6*1.25, height=4*1.25)



# create trust econ graph -------------------------------------------------------

## Appendix Figure A-8


apt_sum_econtrust = dat %>% group_by(econ_nobias, treatment) %>% 
  summarise(coef = mean(apt_bin, na.rm=TRUE),
            se = sd(apt_bin, na.rm=TRUE) / sqrt(sum(!is.na(apt_bin))))
apt_sum_econtrust$type = "Apt-Only Buildings"

mixed_sum_econtrust = dat %>% group_by(econ_nobias, treatment) %>% 
  summarise(coef = mean(mixed_bin, na.rm=TRUE),
            se = sd(mixed_bin, na.rm=TRUE) / sqrt(sum(!is.na(mixed_bin))))
mixed_sum_econtrust$type = "Mixed-use"

mfh_sum_econtrust = dat %>% group_by(econ_nobias, treatment) %>% 
  summarise(coef= mean(mfh_bin, na.rm=TRUE),
            se =sd(mfh_bin, na.rm=TRUE) / sqrt(sum(!is.na(mfh_bin))))
mfh_sum_econtrust$type = "Townhouses"

hdh_sum_econtrust = dat %>%  group_by(econ_nobias, treatment) %>% 
  summarise(coef = mean(hdh_bin, na.rm=TRUE),
            se = sd(hdh_bin, na.rm=TRUE) / sqrt(sum(!is.na(hdh_bin))))
hdh_sum_econtrust$type = "High-Density SFH"

ldh_sum_econtrust = dat %>% group_by(econ_nobias, treatment) %>% 
  summarise(coef = mean(ldh_bin, na.rm=TRUE),
            se = sd(ldh_bin, na.rm=TRUE) / sqrt(sum(!is.na(ldh_bin))))
ldh_sum_econtrust$type = "Low-Density SFH"

sum_econtrust = rbind(apt_sum_econtrust, mixed_sum_econtrust, mfh_sum_econtrust, ldh_sum_econtrust, hdh_sum_econtrust)
sum_econtrust$treatment = factor(sum_econtrust$treatment, c("Explanation", "Economist", "Control"))
sum_econtrust$type = factor(sum_econtrust$type, unique(sum_econtrust$type))
sum_econtrust$econ_nobias = car::recode(sum_econtrust$econ_nobias, "1 = 'Economists aren\\'t biased'; 0 = 'Economists are biased'")

ggplot(sum_econtrust) + 
  aes(y = treatment, x = coef, xmin = coef - 2*se, xmax = coef + 2*se, colour = econ_nobias) + 
  geom_errorbarh(height=0, position = position_dodgev(height=.5)) + 
  geom_point(position = position_dodgev(height=.5)) + 
  facet_wrap(~type) + 
  scale_colour_hc(name=NULL) + 
  guides(colour = guide_legend(rev = TRUE)) + 
  labs(x = "Support", y = NULL) + 
  theme_bw() + 
  theme(legend.position = c(.85, .25))
ggsave("figs/f_a8.eps", width=6*1.25, height=4*1.25)

