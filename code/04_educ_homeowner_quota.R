# Replication archive for "Where Self-Interest Trumps Ideology: Liberal
# Homeowners and Local Opposition to Housing Development" by William Marble
# and Clayton Nall. 

# See how close the sample is to the population on two targets we did not
# explicit quota sample on: education and homeownership.

# Reported in Appendix Figures A-1 and A-2

library(ggplot2)
library(dplyr)
library(reshape2)
select <- dplyr::select

source("code/functions.R")
load("data/cleaned_top20.RData")

educhom = read.table("data/educ_homeownership_targets.tab", sep = "\t", header = TRUE)


# Education ---------------------------------------------------------------

dat$educ2 = with(dat, case_when(
  educ == "2-year College Degree" ~ "some college/assoc. degree",
  educ == "Some College" ~ "some college/assoc. degree",
  educ == "High School / GED" ~ "hs diploma", 
  educ == "4-year College Degree" ~ "bachelors degree",
  educ == "Professional Degree (JD, MD)" ~ "graduate degree",
  educ == "Masters Degree" ~ "graduate degree",
  educ == "Doctoral Degree" ~ "graduate degree",
  educ == "Less than High School" ~ "less than hs",
  TRUE ~ educ
))
sample_educ = dat %>% 
  group_by(region, educ2) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(region = tolower(region)) %>% 
  group_by(region) %>% 
  mutate(sample_prop = 100 * n / sum(n))
educ_target = educhom %>% 
  filter(var == "educ") %>% 
  select(region, level, pct) %>% 
  rename(target_prop = pct, educ2 = level)
sample_educ = left_join(sample_educ, educ_target, by = c("region", "educ2"))

sample_educ2 = reshape2::melt(sample_educ, id.vars = c("region", "educ2"), 
                              measure.vars = c("sample_prop", "target_prop"))
sample_educ2$variable = gsub("_prop", "", sample_educ2$variable, fixed = TRUE)
sample_educ2$variable[sample_educ2$variable=="target"] = "CPS"

sample_educ2$educ = with(sample_educ2, case_when(
  educ2 == "less than hs" ~ "< HS",
  educ2 == "hs diploma" ~ "HS Diploma",
  educ2 == "some college/assoc. degree" ~ "Some college",
  educ2 == "bachelors degree" ~ "Bach. Degree",
  educ2 == "graduate degree" ~ "Grad. Degree"))
sample_educ2$educ = factor(sample_educ2$educ, c("< HS", "HS Diploma", "Some college", "Bach. Degree", "Grad. Degree"))
sample_educ2$region =   factor(sample_educ2$region, c("west", "northeast", "midwest", "south"))

# Figure A-1
ggplot(sample_educ2) +
  aes(x = educ, y = value, fill = variable) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  facet_wrap(~region) + 
  scale_fill_grey(name = NULL) + 
  theme_minimal() + 
  labs(x = NULL, y = "Percent (within region)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("figs/f_a1.eps", height=6,width=6)



# Homeownership -----------------------------------------------------------

homeown_sum = dat %>% 
  mutate(region = tolower(region)) %>% 
  group_by(region) %>% 
  summarise(homeown_sample = 100*mean(homeowner))
target = educhom %>% 
  subset(var == "homeownership") %>% 
  select(region, pct) %>% 
  rename(homeown_CPS = pct)
homeown = left_join(homeown_sum, target, by = "region")

homeown = melt(homeown, id.vars = "region")
homeown$variable = gsub("homeown_", "", homeown$variable, fixed = TRUE)
homeown$region = factor(homeown$region, c("west","midwest",  "south",  "northeast"))

# Figure A-2
ggplot(homeown) + 
  aes(x = region, y = value, fill = variable) + 
  geom_bar(position = position_dodge(), stat = "identity") + 
  scale_fill_grey(name = NULL) + 
  scale_y_continuous(breaks = seq(0, 100, 10), label = function(x) paste0(x, "%")) + 
  theme_minimal() + 
  labs(x = "Census Region", y = "Homeownership rate") 
ggsave("figs/f_a2.eps", width=4,height=3)




