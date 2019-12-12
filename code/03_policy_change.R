# Replication archive for "Where Self-Interest Trumps Ideology: Liberal
# Homeowners and Local Opposition to Housing Development" by William Marble
# and Clayton Nall. 

# Observation analysis reported in the first results section of the paper.

library(ggplot2);library(ggthemes)
library(sandwich)
library(lmtest)
library(reshape2)
library(coefplot)
library(stargazer)
library(multcomp)
library(estimatr)
library(dplyr)


load("data/cleaned_top20.RData")
source("code/functions.R")

options(stringsAsFactors = FALSE)

select <- dplyr::select
theme_set(theme_bw())




# Prepare data ------------------------------------------------------------


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




# Estimate models on all data ---------------------------------------------

# This is a list of policy proposals that respondents were asked about, plus
# labels for each of them. The code below loops through to run the same 
# regression models for each outcome variable. 
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

# Data frame to store coefs for baseline models with no interaction between
# homeownership and ideology.
coefs = data.frame(vars = rep(changevars, 2), labs = rep(names(changevars),2),
                        vname = c("Homeownership", "Fed housing guarantee"),
                        coef = NA, se = NA, intercov = NA)

# Data frame to store coefs for models that interact homeownership and ideology. 
# These are the ones reported in the paper.
intercoefs = data.frame(vars = rep(changevars, 4), labs = rep(names(changevars),4),
                        group = c("Pro-housing renter", "Anti-housing renter", "Pro-housing homeowner", "Anti-housing homeowner"),
                        diff = NA, se = NA)
# baseline for dummy vars
intercoefs$diff[intercoefs$group == "Pro-housing renter"] = 0
intercoefs$se[intercoefs$group == "Pro-housing renter"] = 0

# Lists to store results
results_lm = list()
results_vcv = list()
contrasts_homeowners = data.frame(outcome = changevars, est = NA, se = NA)

# Outcome: binary variable for supporting policy proposal.
support = c("Strongly support", "Somewhat support")

for (v in changevars){
  
  # Regress policy support on homeownership, support for fed housing guarantee. 
  # No interactions.
  form = as.formula(paste0("(", v, "%in% support)  ~ against_fed_housing_binary + homeowner + agegrp + race + bahigher + sex + loginc + factor(zip_density_terc) + msa"))
  mod = lm_robust(form, dat)
  se = mod$vcov
  
  # Store results in coefs data frame
  coefs$coef[coefs$vars == v & coefs$vname == "Homeownership"] = coef(mod)["homeownerTRUE"]
  coefs$se[coefs$vars == v & coefs$vname == "Homeownership"] = sqrt(se["homeownerTRUE", "homeownerTRUE"])
  coefs$coef[coefs$vars == v & coefs$vname == "Fed housing guarantee"] = coef(mod)["against_fed_housing_binary"]
  coefs$se[coefs$vars == v & coefs$vname == "Fed housing guarantee"] = sqrt(se["against_fed_housing_binary", "against_fed_housing_binary"])
  modnointerax = mod
  
  
  # Include interaction between federal housing guarantee and homeownership, via
  # homeowerXguar variable.
  interform = as.formula(paste0("(", v, "%in% support) ~ homeownerXguar + agegrp + race + bahigher + sex + loginc + factor(zip_density_terc) + msa"))
  mod = lm_robust(interform, dat)
  
  # Store results in intercoefs data frame.
  prohousinghomeowner = coef(mod)["homeownerXguarPro-guarantee homeowner"]
  prohousinghomeowner_se = mod$std.error["homeownerXguarPro-guarantee homeowner"]
  intercoefs$diff[intercoefs$vars == v & intercoefs$group == "Pro-housing homeowner"] = prohousinghomeowner
  intercoefs$se[intercoefs$vars == v & intercoefs$group == "Pro-housing homeowner"] = prohousinghomeowner_se
  
  antihousinghomeowner = coef(mod)["homeownerXguarAnti-guarantee homeowner"]
  antihousinghomeowner_se = mod$std.error["homeownerXguarAnti-guarantee homeowner"]
  intercoefs$diff[intercoefs$vars == v & intercoefs$group == "Anti-housing homeowner"] = antihousinghomeowner
  intercoefs$se[intercoefs$vars == v & intercoefs$group == "Anti-housing homeowner"] = antihousinghomeowner_se
  
  antihousingrenter = coef(mod)["homeownerXguarAnti-guarantee renter"]
  antihousingrenter_se = mod$std.error["homeownerXguarAnti-guarantee renter"]
  intercoefs$diff[intercoefs$vars == v & intercoefs$group == "Anti-housing renter"] = antihousingrenter
  intercoefs$se[intercoefs$vars == v & intercoefs$group == "Anti-housing renter"] = antihousingrenter_se
  
  
  # Compare liberal and conservative homeowners
  agh_ind = which(names(mod$coefficients) == "homeownerXguarAnti-guarantee homeowner")
  pgh_ind = which(names(mod$coefficients) == "homeownerXguarPro-guarantee homeowner")
  testmat = matrix(0, ncol = length(coef(mod)))
  testmat[,agh_ind] = -1
  testmat[,pgh_ind] = 1
  comp = summary(glht(mod, testmat))
  contrasts_homeowners$est[contrasts_homeowners$outcome == v] = comp$test$coefficients
  contrasts_homeowners$se[contrasts_homeowners$outcome == v] = comp$test$sigma
  
  ## Save results 
  mod = lm(interform, dat)
  vcv = vcovHC(mod, "HC2")
  results_lm[[v]] = mod
  results_vcv[[v]] = vcv
}

# What level of gov't is each policy at? 
coefs$govlevel = with(coefs, ifelse(grepl("local", vars), "Local Policy", "State Policy"))
coefs$govlevel = factor(coefs$govlevel, c("State Policy", "Local Policy"))
coefs$vname = car::recode(coefs$vname, "'Homeownership' = 'Homeowner'; 'Fed housing guarantee' = 'Opposes fed. housing guarantee'")
coefs$vname[coefs$vname == "Opposes fed. housing guarantee"] = "Opposes fed.\nhousing guarantee"

intercoefs$govlevel = with(intercoefs, ifelse(grepl("local", vars), "Local Policy", "State Policy"))
intercoefs$govlevel = factor(intercoefs$govlevel, c("State Policy", "Local Policy"))



# Estimate models with only Democrats -----------------------------------

# For Appendix Table A-5. Basically the same code as above.
intercoefs_dem = data.frame(
  vars = rep(changevars, 4),
  labs = rep(names(changevars), 4),
  group = c(
    "Pro-housing renter",
    "Anti-housing renter",
    "Pro-housing homeowner",
    "Anti-housing homeowner"
  ),
  diff = NA,
  se = NA
)
intercoefs_dem$diff[intercoefs_dem$group == "Pro-housing renter"] = 0
intercoefs_dem$se[intercoefs_dem$group == "Pro-housing renter"] = 0


dem_results_lm = list()
dem_results_vcv = list()

contrasts_homeowners_dem = data.frame(outcome = changevars, est = NA, se = NA)
support = c("Strongly support", "Somewhat support")
for (v in changevars){
  
  ## interaction between federal housing guarantee and homeownership 
  interform = as.formula(paste0("(", v, ") %in% support ~ homeownerXguar + agegrp + race + bahigher + sex + loginc + factor(zip_density_terc) + msa"))
  mod = lm_robust(interform, subset(dat, partyid != "Republican"))
  
  prohousinghomeowner = coef(mod)["homeownerXguarPro-guarantee homeowner"]
  prohousinghomeowner_se = mod$std.error["homeownerXguarPro-guarantee homeowner"]
  intercoefs_dem$diff[intercoefs_dem$vars == v & intercoefs_dem$group == "Pro-housing homeowner"] = prohousinghomeowner
  intercoefs_dem$se[intercoefs_dem$vars == v & intercoefs_dem$group == "Pro-housing homeowner"] = prohousinghomeowner_se
  
  antihousinghomeowner = coef(mod)["homeownerXguarAnti-guarantee homeowner"]
  antihousinghomeowner_se = mod$std.error["homeownerXguarAnti-guarantee homeowner"]
  intercoefs_dem$diff[intercoefs_dem$vars == v & intercoefs_dem$group == "Anti-housing homeowner"] = antihousinghomeowner
  intercoefs_dem$se[intercoefs_dem$vars == v & intercoefs_dem$group == "Anti-housing homeowner"] = antihousinghomeowner_se
  
  antihousingrenter = coef(mod)["homeownerXguarAnti-guarantee renter"]
  antihousingrenter_se = mod$std.error["homeownerXguarAnti-guarantee renter"]
  intercoefs_dem$diff[intercoefs_dem$vars == v & intercoefs_dem$group == "Anti-housing renter"] = antihousingrenter
  intercoefs_dem$se[intercoefs_dem$vars == v & intercoefs_dem$group == "Anti-housing renter"] = antihousingrenter_se
  
  
  # compare liberal and conservative homeowners
  agh_ind = which(levels(dat$homeownerXguar) == "Anti-guarantee homeowner") + 1
  pgh_ind = which(levels(dat$homeownerXguar) == "Pro-guarantee homeowner") + 1
  testmat = matrix(0, ncol = length(coef(mod)))
  testmat[,agh_ind] = -1
  testmat[,pgh_ind] = 1
  comp = summary(glht(mod, testmat))
  contrasts_homeowners_dem$est[contrasts_homeowners_dem$outcome == v] = comp$test$coefficients
  contrasts_homeowners_dem$se[contrasts_homeowners_dem$outcome == v] = comp$test$sigma
  
  ## save dem_results 
  mod = lm(interform, dat)
  vcv = vcovHC(mod, "HC2")
  dem_results_lm[[v]] = mod
  dem_results_vcv[[v]] = vcv
}

intercoefs_dem$govlevel = with(intercoefs_dem, ifelse(grepl("local", vars), "Local Policy", "State Policy"))
intercoefs_dem$govlevel = factor(intercoefs_dem$govlevel, c("State Policy", "Local Policy"))


# Are there different conclusions when we exclude Republicans? Compare contrasts
# with and without republicans in the model. Answer: no - maximum absolute diff
# in contrast is 0.03. This is reported in 
summary(abs(intercoefs$diff - intercoefs_dem$diff))




# Make Plots --------------------------------------------------------------



## Subset and reorganize these for plotting ##


# categorize into "regulatory/nondiscrimination", "redistributive", and 
# "development" policies. Don't include the transit questions items. 
regvars = c("changstatee_discrim", "changstatee_sec8", "changestate_env", "changelocal_neighvoice")
redistvars =  c("changelocal_rentcontrol", "changstatee_renttax")
devvars = c("changelocal_construction", "changstatee_apt", "changelocal_openspace")


coefs = coefs %>% 
  filter(!vars %in% c("changstatee_transit", "changelocal_transit")) %>% 
  mutate(labs = ifelse(grepl("local", vars), paste0(labs, " (local)"), paste0(labs, " (state)")),
         schema = ifelse(vars %in% regvars, "Regulation", ifelse(vars %in% redistvars, "Redistribution", "Development")))

intercoefs = intercoefs %>% 
  filter(!vars %in% c("changstatee_transit", "changelocal_transit")) %>% 
  mutate(labs = ifelse(grepl("local", vars), paste0(labs, " (local)"), paste0(labs, " (state)")),
         schema = ifelse(vars %in% regvars, "Regulation", ifelse(vars %in% redistvars, "Redistribution", "Development")))


ggplot(coefs) + 
  aes(x = coef, xmin = coef - 2 * se, xmax = coef + 2 * se, y = labs, colour = vname) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_point(position = position_dodgev(height= .25)) + 
  geom_errorbarh(position = position_dodgev(height= .25), height=0) +
  scale_colour_hc(name = "Variable") + 
  labs(x = "Multiple regression coefficient", y = NULL) + 
  facet_grid(schema~.,  scales = "free_y", space = "free") 
ggsave("figs/policy_support_coefplot.pdf", width=8, height=6)



intercoefs = intercoefs %>% 
  mutate(owner = ifelse(grepl("homeowner", group), "Homeowners", "Renters"),
         fed_housing = ifelse(grepl("^Pro", group), "For housing\nguarantee", "Against housing\nguarantee"))
ggplot(intercoefs) + 
  aes(x = diff, xmin = diff - 2 * se, xmax = diff + 2 * se, y = labs, colour = fed_housing, shape = owner) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_point(position = position_dodgev(height= .5), cex = 2) + 
  geom_errorbarh(position = position_dodgev(height= .5), height=0) +
  scale_colour_hc(name = NULL) + 
  scale_shape_manual(name = NULL, values = c(16, 15)) + 
  guides(colour = guide_legend(rev = TRUE, override.aes = list(shape = NA, lty = 1, cex = 1.5))) + 
  labs(x = "Difference relative to pro-housing-guarantee renters", y = NULL) + 
  facet_grid(schema~.,  scales = "free_y", space = "free") 
ggsave("figs/policy_support_coefplot_interactions.pdf", width=8, height=6)





# Make tables -------------------------------------------------------------

regvars = c("changstatee_discrim", "changelocal_neighvoice", "changestate_env", "changstatee_sec8")
redistvars =  c("changelocal_rentcontrol", "changstatee_renttax")
devvars = c("changelocal_openspace", "changelocal_construction", "changstatee_apt")


# organize coefficients 
results_lm = results_lm[names(results_lm) %in% c(regvars, redistvars, devvars)]
results_lm = results_lm[c(regvars, redistvars, devvars)]
results_vcv = results_vcv[names(results_vcv) %in% c(regvars, redistvars, devvars)]
results_vcv = results_vcv[c(regvars, redistvars, devvars)]

dem_results_lm = dem_results_lm[names(dem_results_lm) %in% c(regvars, redistvars, devvars)]
dem_results_lm = dem_results_lm[c(regvars, redistvars, devvars)]
dem_results_vcv = dem_results_vcv[names(dem_results_vcv) %in% c(regvars, redistvars, devvars)]
dem_results_vcv = dem_results_vcv[c(regvars, redistvars, devvars)]


# check lm and vcv are in the same order
stopifnot(all(names(results_lm) == names(results_vcv)))
stopifnot(all(names(dem_results_lm) == names(dem_results_vcv)))


# covariate labels
cov.labs = c(
  "Anti-guarantee homeowner", "Anti-guarantee renter", "Pro-guarantee homeowner",
  "Age: 25-34", "Age: 35-44", "Age: 45-54", "Age: 55-64", "Age: 65-74", "Age: 75+",
  "Race: Black", "Race: Hispanic", "Race: Asian", "Race: Other",
  "Educ: BA or higher", "Male", "Income (log)", "Moderate pop. density", "High pop. density"
)


## FULL SAMPLE FIGURES ##
# Appendix Table A-1 #
stargazer(results_lm, se = lapply(results_vcv, function(x) sqrt(diag(x))), 
          covariate.labels = cov.labs,omit = "msa",
          dep.var.labels = NULL, dep.var.labels.include = FALSE,
          style = "ajps", align=TRUE, float = FALSE,
          omit.stat = c("F", "ser", "adj.rsq"))


# Make a table corresponding to the figure above, that shows contrasts between
# the different categories (but not the full coefficients).
# Table 3 in the main text.

# This code is not very elegant. 
contrast_tab = dcast(filter(intercoefs, group %in% c("Pro-housing homeowner", "Anti-housing homeowner")),  vars + labs  ~ group, value.var = "diff")
contrast_se = dcast(filter(intercoefs, group %in% c("Pro-housing homeowner", "Anti-housing homeowner")),   vars + labs  ~ group, value.var = "se")
contrast_tstat = contrast_tab[, 3:4] / contrast_se[, 3:4]
contrast_pval = apply(contrast_tstat, c(1,2), function(x) pnorm(-1 * abs(x)) * 2)

contrast_tab_temp = contrast_tab[, 3:4]
contrast_tab_temp = format(round(contrast_tab_temp, 2), nsmall = 2)
for (i in 1:nrow(contrast_tab_temp)){
  for (j in 1:(ncol(contrast_tab_temp))){
    # contrast_tab_temp[i,j] = ifelse(contrast_pval[i,j] < .01, paste0(contrast_tab_temp[i,j], "^{**}"), ifelse(contrast_pval[i,j] < .05, paste0(contrast_tab_temp[i,j], "^{*}"), contrast_tab_temp[i,j]))
    repl = case_when(contrast_pval[i,j] < .05 & contrast_pval[i,j] >= .01 ~ paste0(contrast_tab_temp[i,j], "^{*}"),
                     contrast_pval[i,j] < .01 ~ paste0(contrast_tab_temp[i,j], "^{**}"),
                     TRUE ~ paste0(contrast_tab_temp[i,j]))
    contrast_tab_temp[i,j] = repl
  }
}
contrast_tab[,3:4] = contrast_tab_temp

# put in standard errors
contrast_tab$type = "estimate"
contrast_se$type = "se"
contrast_se$`Anti-housing homeowner` = paste0("(", format(round(contrast_se$`Anti-housing homeowner`, 3), nsmall = 3), ")")
contrast_se$`Pro-housing homeowner` = paste0("(", format(round(contrast_se$`Pro-housing homeowner`, 3), nsmall = 3), ")")

# add in contrasts between lib-con
contrasts_homeowners$tstat = with(contrasts_homeowners, est / se)
contrasts_homeowners$pval = pnorm(-1 * abs(contrasts_homeowners$tstat)) * 2
contrasts_homeowners_print = contrasts_homeowners
contrasts_homeowners_print$est = format(round(contrasts_homeowners_print$est, 2), nsmall = 2)
contrasts_homeowners_print$est = with(contrasts_homeowners_print, 
                                case_when(
                                  pval < .05 & pval >= .01 ~ paste0(est, "^{*}"),
                                  pval < .01 ~ paste0(est, "^{**}"),
                                  TRUE ~ paste0(est)
                                  ))
contrasts_homeowners_print$se = paste0("(", format(round(contrasts_homeowners_print$se, 3), nsmall = 3), ")")

# merge w/ main table
contrast_tab = left_join(contrast_tab, dplyr::select(contrasts_homeowners_print, outcome, est), c("vars" = "outcome"))
names(contrast_tab)[names(contrast_tab) == "est"] = "Difference"

contrast_se = left_join(contrast_se,  dplyr::select(contrasts_homeowners_print, outcome, se), c("vars" = "outcome"))
names(contrast_se)[names(contrast_se) == "se"] = "Difference"

# put together estimates and se's
contrast_tab2 = bind_rows(contrast_tab, contrast_se) %>% 
  arrange(vars, type) %>% 
  dplyr::select(labs, `Pro-housing homeowner`, `Anti-housing homeowner`, Difference, type)
contrast_tab2$labs[contrast_tab2$type == "se"] = ""
contrast_tab2$type = NULL

# copy-paste this into tables/policy_contrasts_table.tex and reformat manually
contrast_tab2 = apply(contrast_tab2, 1, function(x) paste(x, collapse = "  &  "))
contrast_tab2 = paste0(contrast_tab2, "\\\\")
cat(contrast_tab2, sep = "\n")





# Non-dichotomized version ------------------------------------------------

# Re-run the analyses without dichotomizing the outcome. Reported in 
# Appendix Table A-2.

# function to recode agree-disagree scale to 0-1
likert01 = function(x){
  stopifnot(all(x %in% c("Somewhat support", "Strongly support", "Neither support nor oppose", 
                         "Somewhat oppose", "Strongly oppose")))
  out = car::recode(x,
                    "'Strongly oppose'=1;
                    'Somewhat oppose'=2;
                    'Neither support nor oppose' = 3;
                    'Somewhat support' = 4;
                    'Strongly support' = 5"
                    )
  out = (out - 1) / 4
  return(out)
}

# reverse code 1-5 housing guarantee to be consistent with dichotomization
# and make it 0-1 for interpretability
dat$against_fed_housing = -1 * dat$redist_housing + 6
dat$against_fed_housing = (dat$against_fed_housing - 1) / 4


results_lm_nodi = list()
results_vcv_nodi = list()
for (v in changevars){
  
  form = as.formula(paste0("likert01(", v, ")~  homeownerXguar + agegrp + race + bahigher + sex + loginc + factor(zip_density_terc) + msa"))
  mod = lm(form, dat)
  vcv = vcovHC(mod, "HC2")
  
  results_lm_nodi[[v]] = mod
  results_vcv_nodi[[v]] = vcv
}



regvars = c("changstatee_discrim", "changelocal_neighvoice", "changestate_env", "changstatee_sec8")
redistvars =  c("changelocal_rentcontrol", "changstatee_renttax")
devvars = c("changelocal_openspace", "changelocal_construction", "changstatee_apt")


# organize coefficients 
results_lm_nodi  = results_lm_nodi[names(results_lm_nodi) %in% c(regvars, redistvars, devvars)]
results_lm_nodi  = results_lm_nodi[c(regvars, redistvars, devvars)]
results_vcv_nodi = results_vcv_nodi[names(results_vcv_nodi) %in% c(regvars, redistvars, devvars)]
results_vcv_nodi = results_vcv_nodi[c(regvars, redistvars, devvars)]

# check lm and vcv are in the same order
stopifnot(all(names(results_lm) == names(results_vcv)))

# covariate labels
cov.labs = c("Anti-guarantee homeowner", "Anti-guarantee renter", "Pro-guarantee homeowner", 
             "Age: 25-34", "Age: 35-44", "Age: 45-54", "Age: 55-64", "Age: 65-74", "Age: 75+", 
             "Race: Black", "Race: Hispanic", "Race: Asian", "Race: Other", 
             "Educ: BA or higher", 
             "Male", 
             "Income (log)", 
             "Moderate pop. density", "High pop. density")

stargazer(results_lm_nodi, se = lapply(results_vcv_nodi, function(x) sqrt(diag(x))), 
          covariate.labels = cov.labs, omit = "msa",
          dep.var.labels = NULL, dep.var.labels.include = FALSE,
          style = "ajps", align=TRUE, float = FALSE,
          omit.stat = c("F", "ser", "adj.rsq"))





