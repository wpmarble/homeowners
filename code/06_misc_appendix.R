# Replication archive for "Where Self-Interest Trumps Ideology: Liberal
# Homeowners and Local Opposition to Housing Development" by William Marble
# and Clayton Nall. 


# Miscellaneous appendix/robustness/response to review stuff

library(dplyr)
library(xtable)
library(ggplot2)
library(estimatr)
library(lmtest)
options(stringsAsFactors = FALSE)
theme_set(theme_bw())
select <- dplyr::select
source("code/functions.R")

load("data/cleaned_top20.RData")


# correlation between our measure of housing liberalism and economic ideology questions.
# Reported in Table A-11
cors = cor(select(dat, redist_housing, freemarket, redist_tax, redist_incdiff), use = "pai")
cors = ifelse(lower.tri(cors, diag=TRUE), cors, NA)
xtable(cors)
 



# Observational: Interactions with Education ------------------------------


# add zip code density variables from 2012-16 ACS
zipdens = read.csv("data/zcta_population_density.csv")
zipdens$zcta = sprintf(zipdens$zcta, fmt="%05d")
zipdens = zipdens %>% 
  rename(zip_pop = population, 
         zip_pop_density = pop_density_mile2) %>% 
  select(zcta, zip_pop, zip_pop_density)
dat = merge(dat, zipdens, by.x = "zip_selfreport", by.y = "zcta")
dat = dat %>% 
  mutate(zip_density_terc = tercileAssign(zip_pop_density))


# re-order race levels so white is baseline
dat$race = factor(dat$race, c("White", "Black", "Hispanic", "Asian", "other"))


# create log income variable
dat$loginc = log(dat$inc_num * 1000)

# create 2x2 variable
dat$homeownerXguar = NA
dat$homeownerXguar[dat$homeowner == 1 & dat$fed_housing_bin == 0] = "Anti-guarantee homeowner"
dat$homeownerXguar[dat$homeowner == 1 & dat$fed_housing_bin == 1] = "Pro-guarantee homeowner"
dat$homeownerXguar[dat$homeowner == 0 & dat$fed_housing_bin == 0] = "Anti-guarantee renter"
dat$homeownerXguar[dat$homeowner == 0 & dat$fed_housing_bin == 1] = "Pro-guarantee renter"
dat$homeownerXguar = relevel(factor(dat$homeownerXguar), "Pro-guarantee renter")

# against fed housing indicator
dat$against_fed_housing_binary = 1 - dat$fed_housing_bin



changevars = c("Relax environmental limits" = "changestate_env",
               "Combating housing discrimination" = "changstatee_discrim",
               "Require accepting Section 8 tenants" = "changstatee_sec8",
               "Fund public transit" = "changstatee_transit",
               "Tax credits for renters" = "changstatee_renttax",
               "Require local govs allow apts" = "changstatee_apt",
               "Allowing development of open space" = "changelocal_openspace",
               "Expanding transit service" = "changelocal_transit",
               "Passing rent control" = "changelocal_rentcontrol",
               "Giving neighborhoods more voice" = "changelocal_neighvoice",
               "Changing laws to allow more construction" = "changelocal_construction"
)

res = data.frame(var = changevars, name = names(changevars), p = NA_real_)
support = c("Strongly support", "Somewhat support")
for (v in changevars){
  interact = as.formula(paste0("(", v, "%in% support) ~ homeownerXguar * bahigher + agegrp + race + sex + loginc + factor(zip_density_terc) + msa"))
  base = as.formula(paste0("(", v, "%in% support) ~ homeownerXguar + bahigher + agegrp + race + sex + loginc + factor(zip_density_terc) + msa"))
  mod1 = lm_robust(interact, dat)
  mod2 = lm_robust(base, dat)
  test = waldtest(mod2, mod1)
  res$p[res$var == v] = test$`Pr(>Chisq)`[2]
}

sum(res$p < .05)
mean(res$p < .05)
