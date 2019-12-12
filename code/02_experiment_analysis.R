# Replication archive for "Where Self-Interest Trumps Ideology: Liberal
# Homeowners and Local Opposition to Housing Development" by William Marble
# and Clayton Nall. 

# Analysis of Experiments 1 and 2.

library(assertthat)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(reshape2)
library(estimatr)
library(car); library(sandwich)
library(stargazer)
library(texreg)
source("code/functions.R")
load("data/cleaned_top20.RData")

select <- dplyr::select

theme_set(theme_bw())

se_mean = function(x, na.rm=TRUE){
  sd(x, na.rm=na.rm) / sqrt(sum(!is.na(x)))
}





# Recode Variables --------------------------------------------------------


## Experiment 1 ##
# Code likert into 5-point numeric scale
levs = c("Strongly oppose", "Somewhat oppose", "Neither support nor oppose", 
         "Somewhat support", "Strongly support")

dat$build_apt_num   = as.numeric(factor(dat$build_apt, levels = levs))
dat$build_mixed_num = as.numeric(factor(dat$build_mixed, levels = levs))
dat$build_hdh_num   = as.numeric(factor(dat$build_hdh, levels = levs))
dat$build_mfh_num   = as.numeric(factor(dat$build_mfh, levels = levs))
dat$build_ldh_num   = as.numeric(factor(dat$build_ldh, levels = levs))

# recode race
dat$race = factor(dat$race, c("White", "Black", "Hispanic", "Asian", "other"))
dat$nonwhite = as.numeric(dat$race != "White")


# classify into: (1) apt-only housing; (2) medium-density housing; (3) low-density SFH
dat$hdh_support = dat$build_apt_num
dat$mdh_support = apply(dat[, c("build_mixed_num", "build_hdh_num", "build_mfh_num")], 1, mean)
dat$ldh_support = dat$build_ldh_num





## Experiment 2 ##
# Recode likert to binary support/oppose
dat$nimby_response = as.numeric(factor(dat$nimby_response, levels = levs))
dat$nimby_support = as.numeric(dat$nimby_response)
table(dat$nimby_support, dat$nimby_response_support)


# Rename the conditions for better printing
dat$nimby_condition[dat$nimby_condition == "control"] = "Control"
dat$nimby_condition[dat$nimby_condition == "low inc/qtr mile"] = "Low-Income, Quarter Mile"
dat$nimby_condition[dat$nimby_condition == "low inc/two miles"] = "Low-Income, Two Miles"
dat$nimby_condition[dat$nimby_condition == "low income"] = "Low-Income, Distance Not Specified"
dat$nimby_condition[dat$nimby_condition == "market rate/qtr mile"] = "Market Rate, Quarter Mile"
dat$nimby_condition[dat$nimby_condition == "market rate/two miles"] = "Market Rate, Two Miles"
dat$nimby_condition = factor(
  dat$nimby_condition, 
  (c("Control", 
     "Low-Income, Distance Not Specified", "Low-Income, Quarter Mile", "Low-Income, Two Miles", 
     "Market Rate, Quarter Mile", "Market Rate, Two Miles")
  ))



# create income tercile variable
dat = dat %>% mutate(inc_terc = tercileAssign(inc_num)) 

# log income
dat = dat %>% mutate(loginc = log(inc_num*1000))

# add zip code density variables from 2012-16 ACS
zipdens = read.csv("data/zcta_population_density.csv")
zipdens$zcta = sprintf(zipdens$zcta, fmt="%05d")
zipdens = zipdens %>% 
  rename(zip_pop = population, 
         zip_pop_density = pop_density_mile2) %>% 
  select(zcta, zip_pop, zip_pop_density) %>% 
  rename(zip_selfreport = zcta)

n1 = nrow(dat)
dat = left_join(dat, zipdens, by = "zip_selfreport")
assert_that(n1 == nrow(dat), msg = "not the same number of rows after merge!!")
dat = dat %>% 
  mutate(zip_density_terc = tercileAssign(zip_pop_density))




# Descriptive statistics of 2 x 2 -----------------------------------------

# Tables 1 and 2 of the manuscript

dat$homeownerXguar = NA
dat$homeownerXguar[dat$homeowner == 1 & dat$fed_housing_bin == 0] = "Anti-guarantee homeowner"
dat$homeownerXguar[dat$homeowner == 1 & dat$fed_housing_bin == 1] = "Pro-guarantee homeowner"
dat$homeownerXguar[dat$homeowner == 0 & dat$fed_housing_bin == 0] = "Anti-guarantee renter"
dat$homeownerXguar[dat$homeowner == 0 & dat$fed_housing_bin == 1] = "Pro-guarantee renter"

## Descriptive Statistics: What proportion fall into homeowner/guarantee 2x2 ##
# Table 1 #
with(dat, table(homeowner, fed_housing_bin)) %>% 
  prop.table %>% 
  round(2)


## Crosstabs of 2x2 schema with other variables ## 
# Table 2 #

# Party ID
prop_dem = dat %>% 
  filter(party == "Democrat") %>% 
  filter(!is.na(homeownerXguar)) %>% 
  group_by(homeownerXguar) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(propround = round(prop, 2)*100)

prop_rep = dat %>% 
  filter(party == "Republican") %>% 
  filter(!is.na(homeownerXguar)) %>% 
  group_by(homeownerXguar) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(propround = round(prop, 2)*100)

prop_ind = dat %>% 
  filter(party == "Independent") %>% 
  filter(!is.na(homeownerXguar)) %>% 
  group_by(homeownerXguar) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(propround = round(prop, 2)*100)


# Race
prop_white = dat %>% 
  filter(race == "White") %>% 
  filter(!is.na(homeownerXguar)) %>% 
  group_by(homeownerXguar) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(propround = round(prop, 2)*100)

prop_black = dat %>% 
  filter(race == "Black") %>% 
  filter(!is.na(homeownerXguar)) %>% 
  group_by(homeownerXguar) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(propround = round(prop, 2)*100)

prop_other_race = dat %>% 
  filter(!race %in% c("White", "Black")) %>% 
  filter(!is.na(homeownerXguar)) %>% 
  group_by(homeownerXguar) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(propround = round(prop, 2)*100)


# Racial affect (old fashioned racism)
prop_neg_affect = dat %>% 
  filter(black_negative_affect_binary == 1) %>% 
  filter(!is.na(homeownerXguar)) %>% 
  group_by(homeownerXguar) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(propround = round(prop, 2)*100)

prop_no_neg_affect = dat %>% 
  filter(black_negative_affect_binary == 0) %>% 
  filter(!is.na(homeownerXguar)) %>% 
  group_by(homeownerXguar) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(propround = round(prop, 2)*100)



# Create latex for table 2 #
tab = data.frame(
  cat = c("Party", "", "", "", "", "",
          "Race", "", "", "", "", "",
          "Racial affect", "", "", ""),
  group = c("Democrat", "", "Republican", "", "Independent/Other", "",
            "White", "", "Black", "", "Other", "",
            "Negative affect", "", "No negative affect", "")
)
tab2 = lapply(list(prop_dem, prop_rep, prop_ind, prop_white, prop_black, prop_other_race, prop_neg_affect, prop_no_neg_affect),
              function(x) {
                x = x[, c("propround", "n")]
                x$propround = paste0(x$propround, "\\%")
                x$n = paste0("(", x$n, ")")
                return(t(x))
              }
)
tab2 = do.call(rbind, tab2)
tab = cbind(tab, tab2)

# print out as latex table, then copy/paste and manually add header, lines, etc.
tab = apply(tab, 1, function(x) paste(x, collapse = "  &  "))
tab = paste0(tab, "  \\\\")
cat(tab, sep = "\n")






# Make subset data frames -------------------------------------------------

## Create Subset df's ##
homeowners     = dat %>% filter(homeowner == TRUE)
lib_homeowners = dat %>% filter(homeowner == TRUE, fed_housing_bin == TRUE)
con_homeowners = dat %>% filter(homeowner == TRUE, fed_housing_bin == FALSE)
renters        = dat %>% filter(homeowner == FALSE)
lib_renters    = dat %>% filter(homeowner == FALSE, fed_housing_bin == TRUE)
con_renters    = dat %>% filter(homeowner == FALSE, fed_housing_bin == FALSE)




# Exp. 1: Descriptive stats -----------------------------------------------


# These outcome variables are 1-5 likert scales, named so that:
# hdh_support = support for High Density Housing
# mdh_support =    "     "  Medium-Density Housing
# ldh_support =    "     "  Low-Density Housing



# across all conditions: mean and sd for apartment support (1-5 scale) 
mean(dat$hdh_support, na.rm=TRUE)
sd(dat$hdh_support, na.rm=TRUE)

# In control condition only #
# apts
mean(dat$hdh_support[dat$gs_condition=="Control"], na.rm=TRUE)
sd(dat$hdh_support[dat$gs_condition=="Control"], na.rm=TRUE)

# medium density
mean(dat$mdh_support[dat$gs_condition=="Control"], na.rm=TRUE)
sd(dat$mdh_support[dat$gs_condition=="Control"], na.rm=TRUE)

# single family homes
mean(dat$ldh_support[dat$gs_condition=="Control"], na.rm=TRUE)
sd(dat$ldh_support[dat$gs_condition=="Control"], na.rm=TRUE)



# Table 4 - Means across treatment conditions
# All the work is done in the exp1means functions defined in functions.R

# high-density housing table #
exp1means(dat, outcome = "hdh_support", 
          outcomelab = "Support for Apartment-Only Buildings (High Threat to Homeowner Self-Interest)",
          savetex = "tables/exp1_means_apts.tex", incl.con.renters = TRUE)

# medium-density housing table
exp1means(dat, outcome = "mdh_support", 
          outcomelab = "Support for Middle-Density Housing (Moderate Threat to Homeowner Self-Interest)",
          savetex = "tables/exp1_means_mdh.tex", incl.con.renters = TRUE)

# low-density housing table
exp1means(dat, outcome = "ldh_support", 
          outcomelab = "Support for Single-Family Housing (Low Threat to Homeowner Self-Interest)",
          savetex = "tables/exp1_means_sfh.tex", incl.con.renters = TRUE)






# Exp. 1: Treatment effects -----------------------------------------------


# Regressions that are reported in Table 5 + Figure 1

### APT-ONLY BUILDINGS ### 
hdhform = hdh_support ~ gs_condition + age + race + loginc + sex + msa
exp1hdhmods = list()
exp1hdhmods[[1]] = list(mod = lm_robust(hdhform, data = lib_homeowners), group = "Pro-guarantee homeowners")
exp1hdhmods[[2]] = list(mod = lm_robust(hdhform, data = con_homeowners), group = "Anti-guarantee homeowners")
exp1hdhmods[[3]] = list(mod = lm_robust(hdhform, data = lib_renters), group = "Pro-guarantee renters")
exp1hdhmods[[4]] = list(mod = lm_robust(hdhform, data = con_renters), group = "Anti-guarantee renters")

# make coefplot data frame 
exp1hdhmods_toplot = lapply(exp1hdhmods, function(x){
  out = tidy(x$mod)
  out$term = gsub("gs_condition", "", out$term)
  out$term = gsub("Economist/", "", out$term)
  out = filter(out, term %in% c("Economist", "Escape", "Families"))
  out = bind_rows(data.frame(term = "Control", estimate = 0, std.error = 0, conf.low = 0, conf.high=0), out)
  out$term = factor(out$term, out$term)
  out$group = x$group
  out$outcome = "Apartment-Only Buildings"
  return(out)
})
exp1hdhmods_toplot = bind_rows(exp1hdhmods_toplot)

### SINGLE-FAMILY HOMES ###
ldhform = ldh_support ~ gs_condition + age + loginc + race + sex + msa
exp1ldhmods = list()
exp1ldhmods[[1]] = list(mod = lm_robust(ldhform, data = lib_homeowners), group = "Pro-guarantee homeowners")
exp1ldhmods[[2]] = list(mod = lm_robust(ldhform, data = con_homeowners), group = "Anti-guarantee homeowners")
exp1ldhmods[[3]] = list(mod = lm_robust(ldhform, data = lib_renters), group = "Pro-guarantee renters")
exp1ldhmods[[4]] = list(mod = lm_robust(ldhform, data = con_renters), group = "Anti-guarantee renters")

# make coefplot data frame
exp1ldhmods_toplot = lapply(exp1ldhmods, function(x){
  out = tidy(x$mod)
  out$term = gsub("gs_condition", "", out$term)
  out$term = gsub("Economist/", "", out$term)
  out = filter(out, term %in% c("Economist", "Escape", "Families"))
  out = bind_rows(data.frame(term = "Control", estimate = 0, std.error = 0, conf.low = 0, conf.high=0), out)
  out$term = factor(out$term, out$term)
  out$group = x$group
  out$outcome = "Single-Family Housing"
  return(out)
})
exp1ldhmods_toplot = bind_rows(exp1ldhmods_toplot)


# Make coefplot for both high- and low-density housing support
exp1coefs = bind_rows(exp1hdhmods_toplot, exp1ldhmods_toplot)
exp1coefs$term = factor(exp1coefs$term, rev(levels(exp1coefs$term)))
exp1coefs$group = factor(exp1coefs$group, c("Pro-guarantee renters", "Anti-guarantee renters", "Pro-guarantee homeowners", "Anti-guarantee homeowners"))
ggplot(exp1coefs) + 
  aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, lty = outcome) + 
  geom_vline(xintercept = 0, colour = "grey50") + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.25)) + 
  geom_point(position = position_dodgev(height=.25)) + 
  facet_wrap(~group) + 
  labs(x = "Average Treatment Effect Estimate", y = NULL) + 
  scale_x_continuous(breaks = seq(-.75, .5, .25)) + 
  guides(lty = guide_legend(title = "Support for", nrow=2, rev=TRUE)) + 
  theme(panel.grid = element_blank(), 
        legend.position = "bottom",
        panel.spacing = unit(0, "in"))
ggsave("figs/f_1.eps", width=4.5, height=4.5)




# Table 5 - regression tables with treatment effect estimates.
# use texreg() to print out basic latex table, then copy/paste and 
# edit style manually. 

# Low-density housing - bottom panel of Table 5
exp1ldhmods_totex = lapply(exp1ldhmods, function(x) x$mod)
groupnames = unlist(lapply(exp1ldhmods, function(x) x$group))
names(exp1ldhmods_totex) = groupnames
exp1ldhmods_totex = exp1ldhmods_totex[c("Anti-guarantee homeowners", "Pro-guarantee homeowners",  "Anti-guarantee renters", "Pro-guarantee renters")]
texreg(exp1ldhmods_totex, custom.model.names = names(exp1ldhmods_totex),
       custom.coef.map = list('gs_conditionEconomist' = 'Economist',
                              'gs_conditionEconomist/Escape' = 'Escape',
                              'gs_conditionEconomist/Families' = 'Families'),
       dcolumn = TRUE, booktabs = TRUE, include.ci = FALSE,
       include.adjrs = FALSE, include.rmse = FALSE, custom.gof.names = c("$R^2$", "$N$"))


# Apartment-only housing - top panel of Table 5
exp1hdhmods_totex = lapply(exp1hdhmods, function(x) x$mod)
groupnames = unlist(lapply(exp1hdhmods, function(x) x$group))
names(exp1hdhmods_totex) = groupnames
exp1hdhmods_totex = exp1hdhmods_totex[c("Anti-guarantee homeowners",  "Pro-guarantee homeowners", "Anti-guarantee renters", "Pro-guarantee renters")]
texreg(exp1hdhmods_totex, custom.model.names = names(exp1hdhmods_totex),
       custom.coef.map = list('gs_conditionEconomist' = 'Economist',
                              'gs_conditionEconomist/Escape' = 'Escape',
                              'gs_conditionEconomist/Families' = 'Families'),
       dcolumn = TRUE, booktabs = TRUE, include.ci = FALSE,
       include.adjrs = FALSE, include.rmse = FALSE, custom.gof.names = c("$R^2$", "$N$"))





# Exp. 1: Dichtomized outcome (appendix) ----------------------------------

# Appendix Table A-4 and Figure A-3

### APT-ONLY BUILDINGS ### 
# Run regressions with covariates to report ATEs in table + coefplot
hdhform_dich = I(hdh_support >= 3) ~ gs_condition + age + race + loginc + sex + msa
exp1hdhmods_dich = list()
exp1hdhmods_dich[[1]] = list(mod = lm_robust(hdhform_dich, data = lib_homeowners), group = "Pro-guarantee homeowners")
exp1hdhmods_dich[[2]] = list(mod = lm_robust(hdhform_dich, data = con_homeowners), group = "Anti-guarantee homeowners")
exp1hdhmods_dich[[3]] = list(mod = lm_robust(hdhform_dich, data = lib_renters), group = "Pro-guarantee renters")
exp1hdhmods_dich[[4]] = list(mod = lm_robust(hdhform_dich, data = con_renters), group = "Anti-guarantee renters")

# make coefplot data frame 
exp1hdhmods_dich_toplot = lapply(exp1hdhmods_dich, function(x){
  out = tidy(x$mod)
  out$term = gsub("gs_condition", "", out$term)
  out$term = gsub("Economist/", "", out$term)
  out = filter(out, term %in% c("Economist", "Escape", "Families"))
  out = bind_rows(data.frame(term = "Control", estimate = 0, std.error = 0, conf.low = 0, conf.high=0), out)
  out$term = factor(out$term, out$term)
  out$group = x$group
  out$outcome = "Apartment-Only Buildings"
  return(out)
})
exp1hdhmods_dich_toplot = bind_rows(exp1hdhmods_dich_toplot)


### SINGLE-FAMILY HOMES ###
# Run regressions with covariates to report ATEs in table + coefplot
ldhform_dich = I(ldh_support >= 3) ~ gs_condition + age + loginc + race + sex + msa
exp1ldhmods_dich = list()
exp1ldhmods_dich[[1]] = list(mod = lm_robust(ldhform_dich, data = lib_homeowners), group = "Pro-guarantee homeowners")
exp1ldhmods_dich[[2]] = list(mod = lm_robust(ldhform_dich, data = con_homeowners), group = "Anti-guarantee homeowners")
exp1ldhmods_dich[[3]] = list(mod = lm_robust(ldhform_dich, data = lib_renters), group = "Pro-guarantee renters")
exp1ldhmods_dich[[4]] = list(mod = lm_robust(ldhform_dich, data = con_renters), group = "Anti-guarantee renters")

# make coefplot data frame
exp1ldhmods_dich_toplot = lapply(exp1ldhmods_dich, function(x){
  out = tidy(x$mod)
  out$term = gsub("gs_condition", "", out$term)
  out$term = gsub("Economist/", "", out$term)
  out = filter(out, term %in% c("Economist", "Escape", "Families"))
  out = bind_rows(data.frame(term = "Control", estimate = 0, std.error = 0, conf.low = 0, conf.high=0), out)
  out$term = factor(out$term, out$term)
  out$group = x$group
  out$outcome = "Single-Family Housing"
  return(out)
})
exp1ldhmods_dich_toplot = bind_rows(exp1ldhmods_dich_toplot)


# Make coefplot for both high- and low-density housing support
exp1coefs_dich = bind_rows(exp1hdhmods_dich_toplot, exp1ldhmods_dich_toplot)
exp1coefs_dich$term = factor(exp1coefs_dich$term, rev(levels(exp1coefs_dich$term)))
exp1coefs_dich$group = factor(exp1coefs_dich$group, c("Pro-guarantee renters", "Anti-guarantee renters", "Pro-guarantee homeowners", "Anti-guarantee homeowners"))
ggplot(exp1coefs_dich) + 
  aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, lty = outcome) + 
  geom_vline(xintercept = 0, colour = "grey50") + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.25)) + 
  geom_point(position = position_dodgev(height=.25)) + 
  facet_wrap(~group) + 
  labs(x = "Average Treatment Effect Estimate", y = NULL) + 
  scale_x_continuous(breaks = seq(-.75, .5, .1)) + 
  guides(lty = guide_legend(title = "Support for", nrow=2, rev=TRUE)) + 
  theme(panel.grid = element_blank(), 
        legend.position = "bottom",
        panel.spacing = unit(0, "in"))
ggsave("figs/f_a3.eps", width=4.5, height=4.5)



# Table A-4
# make regression tables for both high- and low-density housing support
exp1ldhmods_dich_totex = lapply(exp1ldhmods_dich, function(x) x$mod)
groupnames = unlist(lapply(exp1ldhmods_dich, function(x) x$group))
names(exp1ldhmods_dich_totex) = groupnames
exp1ldhmods_dich_totex = exp1ldhmods_dich_totex[c("Anti-guarantee homeowners", "Pro-guarantee homeowners",  "Anti-guarantee renters", "Pro-guarantee renters")]
texreg(exp1ldhmods_dich_totex, custom.model.names = names(exp1ldhmods_dich_totex),
       custom.coef.map = list('gs_conditionEconomist' = 'Economist',
                              'gs_conditionEconomist/Escape' = 'Escape',
                              'gs_conditionEconomist/Families' = 'Families'),
       dcolumn = TRUE, booktabs = TRUE, include.ci = FALSE,
       include.adjrs = FALSE, include.rmse = FALSE, custom.gof.names = c("$R^2$", "$N$"))


# apartments
exp1hdhmods_dich_totex = lapply(exp1hdhmods_dich, function(x) x$mod)
groupnames = unlist(lapply(exp1hdhmods_dich, function(x) x$group))
names(exp1hdhmods_dich_totex) = groupnames
exp1hdhmods_dich_totex = exp1hdhmods_dich_totex[c("Anti-guarantee homeowners",  "Pro-guarantee homeowners", "Anti-guarantee renters", "Pro-guarantee renters")]
texreg(exp1hdhmods_dich_totex, custom.model.names = names(exp1hdhmods_dich_totex),
       custom.coef.map = list('gs_conditionEconomist' = 'Economist',
                              'gs_conditionEconomist/Escape' = 'Escape',
                              'gs_conditionEconomist/Families' = 'Families'),
       dcolumn = TRUE, booktabs = TRUE, include.ci = FALSE,
       include.adjrs = FALSE, include.rmse = FALSE, custom.gof.names = c("$R^2$", "$N$"))






# Exp. 1: Democrat-only analysis (appendix) -------------------------------

# Re-do the above analysis, subsetting to Democrats only, for Appendix Table A-6
# Run regressions with covariates to report ATEs in table + coefplot
exp1ldhmods_dem = list()
exp1ldhmods_dem[[1]] = list(mod = lm_robust(ldhform, data = subset(lib_homeowners, party == "Democrat")), group = "Pro-guarantee homeowners")
exp1ldhmods_dem[[2]] = list(mod = lm_robust(ldhform, data = subset(con_homeowners, party == "Democrat")), group = "Anti-guarantee homeowners")
exp1ldhmods_dem[[3]] = list(mod = lm_robust(ldhform, data = subset(lib_renters, party == "Democrat")), group = "Pro-guarantee renters")
exp1ldhmods_dem[[4]] = list(mod = lm_robust(ldhform, data = subset(con_renters, party == "Democrat")), group = "Anti-guarantee renters")

exp1hdhmods_dem = list()
exp1hdhmods_dem[[1]] = list(mod = lm_robust(hdhform, data = subset(lib_homeowners, party == "Democrat")), group = "Pro-guarantee homeowners")
exp1hdhmods_dem[[2]] = list(mod = lm_robust(hdhform, data = subset(con_homeowners, party == "Democrat")), group = "Anti-guarantee homeowners")
exp1hdhmods_dem[[3]] = list(mod = lm_robust(hdhform, data = subset(lib_renters, party == "Democrat")), group = "Pro-guarantee renters")
exp1hdhmods_dem[[4]] = list(mod = lm_robust(hdhform, data = subset(con_renters, party == "Democrat")), group = "Anti-guarantee renters")


# make regression tables for both high- and low-density housing support
exp1ldhmods_totex_dem = lapply(exp1ldhmods_dem, function(x) x$mod)
groupnames = unlist(lapply(exp1ldhmods_dem, function(x) x$group))
names(exp1ldhmods_totex_dem) = groupnames
exp1ldhmods_totex_dem = exp1ldhmods_totex_dem[c("Anti-guarantee homeowners", "Pro-guarantee homeowners",  "Anti-guarantee renters", "Pro-guarantee renters")]
texreg(exp1ldhmods_totex_dem, custom.model.names = names(exp1ldhmods_totex_dem),
       custom.coef.map = list('gs_conditionEconomist' = 'Economist',
                              'gs_conditionEconomist/Escape' = 'Escape',
                              'gs_conditionEconomist/Families' = 'Families'),
       dcolumn = TRUE, booktabs = TRUE, include.ci = FALSE,
       include.adjrs = FALSE, include.rmse = FALSE, custom.gof.names = c("$R^2$", "$N$"))


# apartments
exp1hdhmods_totex_dem = lapply(exp1hdhmods_dem, function(x) x$mod)
groupnames = unlist(lapply(exp1hdhmods_dem, function(x) x$group))
names(exp1hdhmods_totex_dem) = groupnames
exp1hdhmods_totex_dem = exp1hdhmods_totex_dem[c("Anti-guarantee homeowners",  "Pro-guarantee homeowners", "Anti-guarantee renters", "Pro-guarantee renters")]
texreg(exp1hdhmods_totex_dem, custom.model.names = names(exp1hdhmods_totex_dem),
       custom.coef.map = list('gs_conditionEconomist' = 'Economist',
                              'gs_conditionEconomist/Escape' = 'Escape',
                              'gs_conditionEconomist/Families' = 'Families'),
       dcolumn = TRUE, booktabs = TRUE, include.ci = FALSE,
       include.adjrs = FALSE, include.rmse = FALSE, custom.gof.names = c("$R^2$", "$N$"))


# Make Figure A-4.
sumdat2_dem = dat %>% 
  mutate(homeowner2 = case_when(homeowner == TRUE ~ "Homeowner",
                                homeowner == FALSE ~ "Renter", 
                                TRUE ~ NA_character_),
         fed_housing2 = case_when(fed_housing_bin == TRUE ~ "Pro-guarantee", 
                                  fed_housing_bin == FALSE ~ "Anti-guarantee", 
                                  TRUE ~ NA_character_),
         nimby_condition = gsub("Income", "Inc.", nimby_condition),
         nimby_condition = gsub("Market", "Mkt.", nimby_condition),
         nimby_condition = gsub("Control", "No Info", nimby_condition),
         nimby_condition = gsub("Quarter", "1/4", nimby_condition),
         nimby_condition = gsub("Two", "2", nimby_condition),
         nimby_condition = gsub("Distance", "Dist.", nimby_condition)) %>% 
  mutate(nimby_condition = factor(nimby_condition, 
                                  c("No Info", "Low-Inc., Dist. Not Specified", 
                                    "Low-Inc., 1/4 Mile", "Low-Inc., 2 Miles", 
                                    "Mkt. Rate, 1/4 Mile", "Mkt. Rate, 2 Miles")),
         fed_housing2 = factor(fed_housing2, c("Pro-guarantee", "Anti-guarantee"))) %>% 
  group_by(homeowner2, fed_housing2, nimby_condition) %>% 
  summarise(est = mean(nimby_support),
            se  = sd(nimby_support) / sqrt(n()))

ggplot(sumdat2_dem) + 
  aes(x = nimby_condition, y = est, ymin = est - 1.96 * se, ymax = est + 1.96 * se) + 
  geom_errorbar(width = 0) +
  geom_point() + 
  facet_grid(fed_housing2 ~ homeowner2) + 
  labs(x = NULL, y = "Average Support") + 
  coord_cartesian(ylim = c(2, 4)) + 
  theme(axis.text.x = element_text(angle = 62, hjust = 1))
ggsave("figs/f_a4.eps", width=6,height=4)





# Exp. 1: Pop. density analysis (appendix) --------------------------------

# Analysis for Appendix section "Population Density Analysis"

# Among liberal homeowners, people in the densest areas are substantially more
# likely to support building dense housing, and we cannot reject the null of no
# treatment effects among that subgroup of liberal homeowners.
basemod = lm_robust(hdh_support ~ gs_condition, data = lib_homeowners)
densmod = lm_robust(hdh_support ~ gs_condition * factor(zip_density_terc), data = lib_homeowners)
lmtest::waldtest(densmod, basemod)

linearHypothesis(densmod, hypothesis.matrix = "gs_conditionEconomist")
linearHypothesis(densmod, hypothesis.matrix = "gs_conditionEconomist + gs_conditionEconomist:factor(zip_density_terc)2")
linearHypothesis(densmod, hypothesis.matrix = "gs_conditionEconomist + gs_conditionEconomist:factor(zip_density_terc)3")

linearHypothesis(densmod,  hypothesis.matrix = "gs_conditionEconomist/Families")
linearHypothesis(densmod,  hypothesis.matrix = "gs_conditionEconomist/Families + gs_conditionEconomist/Families:factor(zip_density_terc)2")
linearHypothesis(densmod,  hypothesis.matrix = "gs_conditionEconomist/Families + gs_conditionEconomist/Families:factor(zip_density_terc)3")

linearHypothesis(densmod,  hypothesis.matrix = "gs_conditionEconomist/Escape")
linearHypothesis(densmod,  hypothesis.matrix = "gs_conditionEconomist/Escape + gs_conditionEconomist/Escape:factor(zip_density_terc)2")
linearHypothesis(densmod,  hypothesis.matrix = "gs_conditionEconomist/Escape + gs_conditionEconomist/Escape:factor(zip_density_terc)3")

densmod = lm(hdh_support ~ gs_condition * factor(zip_density_terc), data = lib_homeowners)
stargazer(densmod,se= starprep(densmod), type = "text", 
          covariate.labels = c("Economist", "Escape", "Families",
                               "Density Terc = 2", "Density Terc = 3", 
                               "Economist * Density Terc = 2", "Escape * Density Terc = 2", "Families * Density Terc = 2",
                               "Economist * Density Terc = 3", "Escape * Density Terc = 3", "Families * Density Terc = 3"))




# density interacted with treatment for the full sample. Doesn't look like there
# are heterogeneous effects in the full sample, though again people in denser
# zip codes are much more supportive of high density housing.
basemod = lm_robust(hdh_support ~ gs_condition, data = dat)
densmod = lm_robust(hdh_support ~ gs_condition * factor(zip_density_terc), data = dat)
lmtest::waldtest(densmod, basemod)

linearHypothesis(densmod, hypothesis.matrix = "gs_conditionEconomist")
linearHypothesis(densmod, hypothesis.matrix = "gs_conditionEconomist + gs_conditionEconomist:factor(zip_density_terc)2")
linearHypothesis(densmod, hypothesis.matrix = "gs_conditionEconomist + gs_conditionEconomist:factor(zip_density_terc)3")

linearHypothesis(densmod,  hypothesis.matrix = "gs_conditionEconomist/Families")
linearHypothesis(densmod,  hypothesis.matrix = "gs_conditionEconomist/Families + gs_conditionEconomist/Families:factor(zip_density_terc)2")
linearHypothesis(densmod,  hypothesis.matrix = "gs_conditionEconomist/Families + gs_conditionEconomist/Families:factor(zip_density_terc)3")




## Appendix Figure A-5
## Partial out the treatment effects, them plot support for apartments by groups
## in the 2x2.

mod1exp1 = lm(hdh_support ~ gs_condition, dat)
dat$hdh_support_partial_1 = NA
dat$hdh_support_partial_1[-na.action(mod1exp1)] = mod1exp1$residuals


# get pop density terciles
terc = quantile(dat$zip_pop_density, c(0, 1/3, 2/3, 1))

# plot only from 1st percentile of density tercile to 99th
one_99 = quantile(dat$zip_pop_density, c(.02, .98))

ggplot(dat) + 
  aes(x = zip_pop_density, y = hdh_support_partial_1, group = homeownerXguar) + 
  geom_vline(xintercept = terc[2]) + 
  geom_vline(xintercept = terc[3]) + 
  geom_smooth(se = F, colour = "black") + 
  annotate(geom = "text", label = "Anti-guarantee homeowner", x = 700, y = -.6) + 
  annotate(geom = "text", label = "Pro-guarantee homeowner",  x = 600, y = -0.21) + 
  annotate(geom = "text", label = "Anti-guarantee renter",    x = 700, y = 0.05) + 
  annotate(geom = "text", label = "Pro-guarantee renter",     x = 700, y = 0.4) + 
  scale_x_log10() +
  coord_cartesian(xlim = one_99, ylim = c(-.75, .75)) + 
  scale_linetype(name = NULL) + 
  theme(legend.position = "bottom") + 
  guides(linetype = FALSE) + 
  labs(x = "ZIP Code Population Density", y = "Residualized Support for\nApt. Construction")
ggsave("figs/f_a5.eps", width=6, height = 4)




# Exp. 2: Main analysis ---------------------------------------------------

# Experiment 2 Means - Table 6
exp2means(dat, savetex = "tables/exp2_means.tex", incl.con.renters = TRUE)[[1]]



# Experiment 2 ATEs - regressions as above for table + coefplot
nimbyform = nimby_support ~ nimby_condition + age + loginc + race + sex + msa
exp2mods = list()
exp2mods[[1]] = list(mod = lm_robust(nimbyform, data = lib_homeowners), group = "Pro-guarantee homeowners")
exp2mods[[2]] = list(mod = lm_robust(nimbyform, data = con_homeowners), group = "Anti-guarantee homeowners")
exp2mods[[3]] = list(mod = lm_robust(nimbyform, data = lib_renters), group = "Pro-guarantee renters")
exp2mods[[4]] = list(mod = lm_robust(nimbyform, data = con_renters), group = "Anti-guarantee renters")


# coefplot - Figure 2
exp2mods_toplot = lapply(exp2mods, function(x){
  out = tidy(x$mod)
  out = filter(out, grepl("nimby_condition", out$term))
  out = bind_rows(data.frame(term = "No Info", estimate = 0, std.error = 0, conf.low = 0, conf.high=0), out)
  
  out$term = gsub("nimby_condition", "", out$term)
  out$term = gsub(pattern = "Control", replacement = "No Info", x = out$term)
  out$term = gsub(pattern = "Income", replacement = "Inc.", x = out$term)
  out$term = gsub(pattern = "Quarter", replacement = "1/4", x = out$term)
  out$term = gsub(pattern = "Two", replacement = "2", x = out$term)
  out$term = gsub(pattern = "Market", replacement = "Mkt.", x = out$term)
  out$term = gsub(pattern = "Distance Not Specified", replacement = "Dist. Not Given", x = out$term)
  out$term = gsub(pattern = "Low-Inc", replacement = "Low Inc", x = out$term)
  out$term = trimws(out$term)
  out$term = factor(out$term,
                    c("No Info", "Low Inc., Dist. Not Given", "Low Inc., 1/4 Mile", 
                      "Low Inc., 2 Miles", "Mkt. Rate, 1/4 Mile", "Mkt. Rate, 2 Miles"))
  
  out$term = factor(out$term, out$term)
  out$group = x$group
  out$outcome = "Support for 120-Unit Apt. Building"
  return(out)
})
exp2coefs = bind_rows(exp2mods_toplot)
exp2coefs$term = factor(exp2coefs$term, rev(levels(exp2coefs$term)))
exp2coefs$group = factor(exp2coefs$group, c("Pro-guarantee renters", "Anti-guarantee renters", "Pro-guarantee homeowners", "Anti-guarantee homeowners"))


ggplot(exp2coefs) + 
  aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high) + 
  geom_vline(xintercept = 0, colour = "grey50") + 
  geom_errorbarh(height = 0, position = position_dodgev(height=.25)) + 
  geom_point(position = position_dodgev(height=.25)) + 
  facet_wrap(~group) + 
  labs(x = "Average Treatment Effect Estimate", y = NULL) + 
  scale_x_continuous(breaks = seq(-1, 1, .5)) + 
  theme(panel.grid = element_blank(), 
        legend.position = "bottom",
        panel.spacing = unit(0, "in"))
ggsave("figs/f_2.eps", width=6, height=4.5)


# Make table of ATE results - Table 7
exp2mods_totex = lapply(exp2mods, function(x) x$mod)
groupnames = unlist(lapply(exp2mods, function(x) x$group))
names(exp2mods_totex) = groupnames
exp2mods_totex = exp2mods_totex[c("Anti-guarantee homeowners",  "Pro-guarantee homeowners", "Anti-guarantee renters", "Pro-guarantee renters")]

exp2coefnames = list("nimby_conditionLow-Income, Distance Not Specified" = "Low Inc., Dist. Not Given",
                     "nimby_conditionLow-Income, Quarter Mile" = "Low Inc., 1/4 Mile",
                     "nimby_conditionLow-Income, Two Miles" = "Low Inc., 2 Miles",
                     "nimby_conditionMarket Rate, Quarter Mile" = "Mkt. Rate, 1/4 Mile",
                     "nimby_conditionMarket Rate, Two Miles" = "Mkt. Rate, 2 Miles")
texreg(exp2mods_totex, custom.model.names = names(exp2mods_totex),
       custom.coef.map = exp2coefnames,
       dcolumn = TRUE, booktabs = TRUE, include.ci = FALSE,
       include.adjrs = FALSE, include.rmse = FALSE, custom.gof.names = c("$R^2$", "$N$"))





# Exp. 2: Democrats-only analysis (appendix) ------------------------------

# For appendix Table A-7

### DEMOCRATS ONLY ###
nimbyform = nimby_support ~ nimby_condition + age + loginc + race + sex + msa
exp2mods_dem = list()
exp2mods_dem[[1]] = list(mod = lm_robust(nimbyform, data = subset(lib_homeowners, party == "Democrat")), group = "Pro-guarantee homeowners")
exp2mods_dem[[2]] = list(mod = lm_robust(nimbyform, data = subset(con_homeowners, party == "Democrat")), group = "Anti-guarantee homeowners")
exp2mods_dem[[3]] = list(mod = lm_robust(nimbyform, data = subset(lib_renters, party == "Democrat")), group = "Pro-guarantee renters")
exp2mods_dem[[4]] = list(mod = lm_robust(nimbyform, data = subset(con_renters, party == "Democrat")), group = "Anti-guarantee renters")


# make regression table
exp2mods_totex_dem = lapply(exp2mods_dem, function(x) x$mod)
groupnames = unlist(lapply(exp2mods_dem, function(x) x$group))
names(exp2mods_totex_dem) = groupnames
exp2mods_totex_dem = exp2mods_totex_dem[c("Anti-guarantee homeowners",  "Pro-guarantee homeowners", "Anti-guarantee renters", "Pro-guarantee renters")]

exp2coefnames = list("nimby_conditionLow-Income, Distance Not Specified" = "Low Inc., Dist. Not Given",
                     "nimby_conditionLow-Income, Quarter Mile" = "Low Inc., 1/4 Mile",
                     "nimby_conditionLow-Income, Two Miles" = "Low Inc., 2 Miles",
                     "nimby_conditionMarket Rate, Quarter Mile" = "Mkt. Rate, 1/4 Mile",
                     "nimby_conditionMarket Rate, Two Miles" = "Mkt. Rate, 2 Miles")
texreg(exp2mods_totex_dem, custom.model.names = names(exp2mods_totex_dem),
       custom.coef.map = exp2coefnames,
       dcolumn = TRUE, booktabs = TRUE, include.ci = FALSE,
       include.adjrs = FALSE, include.rmse = FALSE, custom.gof.names = c("$R^2$", "$N$"))




# Exp. 2: Pop. density analysis (appendix) --------------------------------

# Figure A-6

## Density analysis: partial out the treatment effects, them plot support for
## apartment building by groups in the 2x2
mod_1 = lm(nimby_support ~ nimby_condition, dat)
dat$nimby_support_partial = mod_1$residuals


# get pop density terciles
terc = quantile(dat$zip_pop_density, c(0, 1/3, 2/3, 1))

# plot only from 1st percentile of density tercile to 99th
one_99 = quantile(dat$zip_pop_density, c(.02, .98))

ggplot(dat) + 
  aes(x = zip_pop_density, y = nimby_support_partial, group = homeownerXguar) + 
  geom_vline(xintercept = terc[2]) + 
  geom_vline(xintercept = terc[3]) + 
  geom_smooth(se = F, colour = "black", method = "gam", formula = y ~ s(x, bs = "tp")) + 
  annotate(geom = "text", label = "Anti-guarantee homeowner", x = 700, y = -.7) +
  annotate(geom = "text", label = "Pro-guarantee homeowner",  x = 600, y = -0.4) +
  annotate(geom = "text", label = "Anti-guarantee renter",    x = 400, y = 0) +
  annotate(geom = "text", label = "Pro-guarantee renter",     x = 700, y = 0.5) +
  scale_x_log10() +
  coord_cartesian(xlim = one_99, ylim = c(-.75, .75)) + 
  theme(legend.position = "bottom") +
  labs(x = "ZIP Code Population Density", y = "Residualized Support for\nBuilding 120-Unit Apartment")
ggsave("figs/f_a6.eps", width=6, height = 4)




# Analysis of race/racial affect (appendix) -------------------------------

# Experiment 1 -- control for race, examine coef
lm_robust(hdh_support ~ gs_condition + homeowner + race, data = dat)
lm_robust(hdh_support ~ gs_condition , data = subset(dat, homeowner == 1 & race == "White"))
lm_robust(hdh_support ~ gs_condition , data = subset(dat, homeowner == 1 & race == "Black"))
lm_robust(hdh_support ~ gs_condition + race, data = lib_homeowners)
lm_robust(hdh_support ~ gs_condition + race, data = con_homeowners)

lm_robust(hdh_support ~ gs_condition + nonwhite + homeowner + income + educ + age, data = dat)
lm_robust(hdh_support ~ gs_condition + nonwhite + income + educ + age, data = lib_homeowners)
lm_robust(hdh_support ~ gs_condition + nonwhite + income + educ + age, data = con_homeowners)


# use the negative affect towards blacks measure, subsetting to whites
lm_robust(hdh_support ~ gs_condition + black_negative_affect_binary + homeowner + fed_housing_bin + income + educ + age, data = subset(dat, race == "White"))
lm_robust(hdh_support ~ gs_condition + black_negative_affect_binary + income + educ + age, data = subset(lib_homeowners, race=="White"))


# Correlation between strenthening anti-discrimination laws and making it
# easier to build more apts is relatively low.
dat$changstatee_apt = as.numeric(factor(dat$changstatee_apt, levs))
dat$changstatee_discrim = as.numeric(factor(dat$changstatee_discrim, levs))

# spearman correlation
cor(dat$changstatee_discrim, dat$changstatee_apt, method = "spearman")

# only among whites
cor(dat$changstatee_discrim[dat$race == "White"], dat$changstatee_apt[dat$race == "White"], method = "spearman")

# regression controlling for baseline characteristics among white homeowners
lm_robust(changstatee_apt ~ changstatee_discrim + age + income + sex, filter(dat, nonwhite == 0, homeowner == 1))




## Make Appendix Table A-8
cov.labs.race = c("Condition: Economist", "Condition: Economic/Escape", "Condition: Economist/Families",
                  "Negative racial affect",
                  "Homeowner", 
                  "Anti-housing guarantee",
                  "Income (log)",
                  "Educ: BA or higher",
                  "Age: 25-44",
                  "Age: 45-64", 
                  "Age: 65+")
mod1 = lm(hdh_support ~ gs_condition + black_negative_affect_binary + homeowner + I(1-fed_housing_bin) + loginc + bahigher + age, data = subset(dat, race == "White"))
vcv1 = vcovHC(mod1, "HC2")
mod2 = lm(hdh_support ~ gs_condition + black_negative_affect_binary + homeowner + I(1-fed_housing_bin) + loginc + bahigher + age, data = subset(dat, race == "White" & homeowner == TRUE))
vcv2 = vcovHC(mod2, "HC2")
mod3 = lm(hdh_support ~ gs_condition + black_negative_affect_binary + loginc + bahigher + age, data = subset(lib_homeowners, race == "White"))
vcv3 = vcovHC(mod3, "HC2")
mod4 = lm(hdh_support ~ gs_condition + black_negative_affect_binary  + loginc + bahigher + age, data = subset(con_homeowners, race == "White"))
vcv4 = vcovHC(mod4, "HC2")

stargazer(mod1, mod2, mod3, mod4,
          se = lapply(list(vcv1, vcv2, vcv3, vcv4), function(x) sqrt(diag(x))), 
          covariate.labels = cov.labs.race,
          align=TRUE, style = "AJPS", float = FALSE, 
          omit.stat = c("ser", "F", "adj.rsq"))




# Mover analysis (appendix) -----------------------------------------------



# Experiment 1 -- Examine effects among people who moved to a more diverse
# zip code in the past 5 years

# read geo data
geo = read.csv("data/zipcode_inced.csv")
geo$zip = as.character(geo$zip)
geo$zip = with(geo, ifelse(nchar(zip)  < 5, paste0("0", zip), zip))
geo = subset(geo, select = c("zip", "pctwht"))


# merge in pct_white for respondents' current zip
t = nrow(dat)
dat = merge(dat, geo, by.x = "zip_selfreport", by.y = "zip", all.x = TRUE, all.y = FALSE)
dat$pctwht_current = dat$pctwht
dat$pctwht = NULL
assert_that(nrow(dat) == t)

# merge in pct_white for respondents' previous zip
t = nrow(dat)
dat = merge(dat, geo, by.x = "prevzip", by.y = "zip", all.x = TRUE, all.y = FALSE)
dat$pctwht_prev = dat$pctwht
dat$pctwht = NULL
assert_that(nrow(dat) == t)

# subset to recent movers
recent_movers = subset(dat, yearmoved >= 2012)


# indicator for moving to a zip code that is less diverse than prev zipcode
# how much less diverse is current zip code compared to previous? 
recent_movers$pctwht_delta = recent_movers$pctwht_current - recent_movers$pctwht_prev
recent_movers$move_less_diverse = as.numeric(recent_movers$pctwht_delta > 0)

exp2table_2(
  recent_movers[recent_movers$race == "White" & recent_movers$move_less_diverse == 1, ],
  outcome = "nimby_support",
  outcomelab = "Support for Proposal to Build 120-Unit Apartment Building",
  savetex = "tables/ate_table_nimby_moverwhite.tex"
)
exp2table_2(
  recent_movers[recent_movers$race == "White" & recent_movers$move_less_diverse == 0,],
  outcome = "nimby_support",
  outcomelab = "Support for Proposal to Build 120-Unit Apartment Building",
  savetex = "tables/ate_table_nimby_movernonwhite.tex"
)

## Plots
white_recent_movers<-recent_movers[recent_movers$race=="White",]

# Regress support for high density housing on treatment + control variables, including 
# indicator for having moved recently to a less diverse area. 
# Reported in Appendix Table A-9.

# among all whites - column 1
mod1 = lm(hdh_support ~ gs_condition + move_less_diverse + homeowner + I(1-fed_housing_bin) + loginc + bahigher + age, data = subset(recent_movers, race == "White"))
vcv1 = vcovHC(mod1, "HC2")

# among all white homeowners - column 2
mod2 = lm(hdh_support ~ gs_condition + move_less_diverse + homeowner + I(1-fed_housing_bin) + loginc + bahigher + age, data = subset(recent_movers, race == "White" & homeowner == TRUE))
vcv2 = vcovHC(mod2, "HC2")

# among lib white homeowners - column 3
mod3 = lm(hdh_support ~ gs_condition + move_less_diverse + homeowner + I(1-fed_housing_bin) + loginc + bahigher + age, data = subset(recent_movers, race == "White" & homeowner == TRUE & fed_housing_bin == TRUE))
vcv3 = vcovHC(mod3, "HC2")

# among conservative white homeowners - column 4
mod4 = lm(hdh_support ~ gs_condition + move_less_diverse + I(1-fed_housing_bin) + loginc + bahigher + age, data = subset(recent_movers, race == "White" & homeowner == TRUE & fed_housing_bin == FALSE))
vcv4 = vcovHC(mod4, "HC2")


# Appendix Table A-9
cov.labs.movers = c("Condition: Economist", "Condition: Economic/Escape", "Condition: Economist/Families", 
                    "Moved to less diverse Zip", "Homeowner", "Anti-housing guarantee", 
                    "Income (log)", "Educ: BA or higher", "Age: 25-44", 
                    "Age: 45-64", "Age: 65+")
stargazer(mod1, mod2, mod3, mod4,
          se = lapply(list(vcv1, vcv2, vcv3, vcv4), function(x) sqrt(diag(x))), 
          covariate.labels = cov.labs.movers, 
          align=TRUE, style = "AJPS", float = FALSE, 
          omit.stat = c("ser", "F", "adj.rsq"))







# Randomization check (appendix) ------------------------------------------

# Runs multinomial logit models predicting treatment assignment as a function 
# of covariates. Reported in Appendix section "Balance Tables," along with
# Appendix Table A-3.

# fit multinomial logit models 
# looks like there's some imbalance in this sample for experiment 1.
library(nnet)

mnl_alt1 = multinom(gs_condition ~ I(1-fed_housing_bin) + homeowner + race + loginc + bahigher + age, data = dat)
mnl_null1 = multinom(gs_condition ~ 1, data = dat[-mnl_alt1$na.action,])
mnl_sum1 = summary(mnl_alt1)
anova(mnl_alt1, mnl_null1)

# Calculate coefficients in terms of relative risk ratio (expoentiated coefs).
econ_coefs = data.frame(est = mnl_sum1$coefficients[1,], se = mnl_sum1$standard.errors[1,])
econ_coefs$t = econ_coefs$est / econ_coefs$se
econ_coefs$p = (1 - pnorm(abs(econ_coefs$t))) * 2
econ_coefs$rrr = exp(mnl_sum1$coefficients[1,])
econ_coefs$treatment = "econ"

esca_coefs = data.frame(est = mnl_sum1$coefficients[2,], se = mnl_sum1$standard.errors[2,])
esca_coefs$t = esca_coefs$est / esca_coefs$se
esca_coefs$p = (1 - pnorm(abs(esca_coefs$t))) * 2
esca_coefs$rrr = exp(mnl_sum1$coefficients[2,])
esca_coefs$treatment = "escape"

fam_coefs = data.frame(est = mnl_sum1$coefficients[3,], se = mnl_sum1$standard.errors[3,])
fam_coefs$t = fam_coefs$est / fam_coefs$se
fam_coefs$p = (1 - pnorm(abs(fam_coefs$t))) * 2
fam_coefs$rrr =  exp(mnl_sum1$coefficients[3,])
fam_coefs$treatment = "family"

exp1_coefs = rbind(econ_coefs, esca_coefs, fam_coefs)

# What proportion of coefs are significant? plot calibration curve for different
# conf levels. If all coefs are 0, the curve should fall along the 45-degree line.
calcurve = data.frame(alpha =  seq(.001, .4, .001),
                      prop.sig = NA)
calcurve$prop.sig = unlist(lapply(calcurve$alpha, function(x) mean(exp1_coefs$p <= x)))
plot(
  prop.sig ~ alpha,
  calcurve,
  type = "l",
  xlab = "Alpha level",
  ylab = "Proportion significant"
)
abline(a = 0, b = 1, lty = 3)


# Make balance table - Appendix Table A-3.
baltab_fedhou = dat %>% 
  filter(!is.na(gs_condition)) %>% 
  group_by(gs_condition) %>% 
  mutate(fed_housing_bin = 1 - fed_housing_bin) %>% 
  summarise(mean = mean(fed_housing_bin, na.rm=TRUE)) %>% 
  mutate(mean = mean*100) %>% 
  mutate(var = "Opposed") %>% 
  mutate(cat = "Housing guarantee")

baltab_homeowner = dat %>% 
  filter(!is.na(gs_condition)) %>% 
  group_by(gs_condition) %>% 
  summarise(mean = mean(homeowner)) %>% 
  mutate(mean = mean*100) %>% 
  mutate(var = "Owns home") %>% 
  mutate(cat = "Homeownership")

baltab_party = dat %>% 
  filter(!is.na(gs_condition)) %>% 
  group_by(gs_condition, partyid) %>% 
  summarise(mean = n()) %>% 
  ungroup() %>% 
  group_by(gs_condition) %>% 
  mutate(mean = mean / sum(mean)) %>% 
  mutate(mean = mean*100) %>% 
  rename( var = partyid) %>% 
  mutate(cat = "Party ID")

baltab_race = dat %>% 
  filter(!is.na(gs_condition)) %>% 
  group_by(gs_condition, race) %>% 
  summarise(mean = n()) %>% 
  ungroup() %>% 
  group_by(gs_condition) %>% 
  mutate(mean = mean / sum(mean)) %>% 
  mutate(mean = mean*100) %>% 
  rename(var = race) %>% 
  mutate(cat = "Race")

baltab_inc = dat %>% 
  filter(!is.na(gs_condition)) %>% 
  group_by(gs_condition) %>% 
  summarise(mean = mean(exp(loginc), na.rm=TRUE)) %>% 
  mutate(var = "Income") %>% 
  mutate(cat = "Income")

baltab_educ = dat %>% 
  filter(!is.na(gs_condition)) %>% 
  group_by(gs_condition) %>% 
  summarise(mean = mean(bahigher, na.rm=TRUE)) %>% 
  mutate(mean = mean*100) %>% 
  mutate(var = "BA or higher") %>% 
  mutate(cat = "Education")

baltab_age = dat %>% 
  filter(!is.na(gs_condition)) %>% 
  group_by(gs_condition, age) %>% 
  summarise(mean = n()) %>% 
  ungroup() %>% 
  group_by(gs_condition) %>% 
  mutate(mean = mean / sum(mean)) %>% 
  mutate(mean = mean*100) %>% 
  rename(var = age) %>% 
  mutate(cat = "Age")

baltab = list(baltab_fedhou, baltab_homeowner, baltab_party, baltab_race, baltab_age, baltab_educ, baltab_inc)
baltab = lapply(baltab, function(x) {x$var = as.character(x$var); x$gs_condition = as.character(x$gs_condition); return(x)})
baltab = bind_rows(baltab)
baltab = reshape2::dcast(baltab, cat + var ~ gs_condition, value.var = "mean")
baltab$`NA` = NULL
baltab$`Max Diff` = apply(baltab[3:6], 1, function(x) max(x) - min(x))
baltab[,3:7] = round(baltab[,3:7], 1)
baltab[,3:7] = lapply(baltab[,3:7], prettyNum, big.mark = ",")
baltab %>% xtable::xtable()



# Experiment 2 -- covariates don't predict treatment here
mnl_alt2 = multinom(nimby_condition ~ I(1-fed_housing_bin) + homeowner + race + loginc + bahigher + age, data = dat)
mnl_null2 = multinom(nimby_condition ~ 1, data = dat[-mnl_alt2$na.action,])
anova(mnl_alt2, mnl_null2)

# The two experimental treatments aren't correlated with each other.
mnl_alt3 = multinom(nimby_condition ~ gs_condition, dat)
mnl_null3 = multinom(nimby_condition ~ 1, dat[-mnl_alt3$na.action,])
anova(mnl_alt3, mnl_null3)


# Concluded from this analysis that the imbalance in experiment 1 was probably
# due to bad luck - but just in case we control for the relevant covariates in
# the main analysis. In the text we report both raw means and treatment effects
# estimated with controls.






# Anti-guarantee renter analysis ------------------------------------------

# Based on the analysis of zip code density above, it appears that anti-guarantee
# renters in middle- to high-density areas are less supportive of (some) apts
# than pro-guarantee homeownes, at least based on experiment 2. What explains
# this? Didn't come up with much in this analysis.


prop.table(table(con_renters$age))
prop.table(table(lib_renters$age))
with(renters, chisq.test(table(fed_housing_bin, age)))

summary(con_renters$inc_num)
summary(lib_renters$inc_num)

# income 
ggplot(dat) + 
  aes(x = zip_pop_density, y = inc_num, colour = homeownerXguar) + 
  geom_vline(xintercept = terc[2]) + 
  geom_vline(xintercept = terc[3]) + 
  stat_smooth(se = FALSE) + 
  guides(colour = guide_legend(title = NULL, nrow = 2)) + 
  theme(legend.position = "bottom") +
  scale_x_log10() + 
  coord_cartesian(xlim = one_99) 

# what cities are they in? maybe some small differences but seems unlikely to explain
cbind(cons = prop.table(table(con_renters$msa)), libs = prop.table(table(lib_renters$msa)))
with(renters, chisq.test(table(msa, fed_housing_bin)))

# zip code density
prop.table(table(con_renters$zip_density_terc))
prop.table(table(lib_renters$zip_density_terc))
with(renters, chisq.test(table(zip_density_terc, fed_housing_bin)))

# relatively small sample sizes -- about 158 conservative renters in
# middle-tercile areas
renters %>% group_by(fed_housing_bin, zip_density_terc) %>% summarise(n=n())





