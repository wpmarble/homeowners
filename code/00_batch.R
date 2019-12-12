# Replication archive for "Where Self-Interest Trumps Ideology: Liberal
# Homeowners and Local Opposition to Housing Development" by William Marble
# and Clayton Nall. 

# Running this script will replicate all the results in the paper and 
# appendix. 

# If you run this file via source() the next lines automatically set the
# correct working directory. Otherwise comment out the next two lines and set
# the working directory to the name of the replication archive folder.
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir); setwd("..")
 

source("code/01_clean_checkquotas.R"); rm(list=ls());
source("code/02_experiment_analysis.R"); rm(list=ls());
source("code/03_policy_change.R"); rm(list=ls());
source("code/04_educ_homeowner_quota.R"); rm(list=ls());
source("code/05_analyze_mturk_180110.R"); rm(list=ls());
source("code/06_misc_appendix.R")