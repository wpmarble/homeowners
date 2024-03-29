# Replication archive

Replication archive for "[Where Self-Interest Trumps Ideology: Liberal Homeowners and Local Opposition to Housing Development](https://www.doi.org/10.1086/711717)" by William Marble and Clayton Nall in _The Journal of Politics_.

To download this archive, run the following command in Terminal:
```
git clone https://github.com/wpmarble/homeowners.git
```

You can also find the data and code on the _JOP_ Dataverse: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NIMLAR


# Code directory
* `00_batch.R` a batch file that will conduct all the analyses.
* `01_clean_checkquotas.R` cleans the csv exported from Qualtrics and shows the proportion of respondents in each quota target. Need to download the raw Qualtrics file from Dataverse to run this script ([see below](#data-files)).
* `02_experiment_analysis.R` conducts the analysis of experiments 1 and 2 in the main text, including balance tables. Generates Tables 1, 2, 4, 5, 6, 7, A-3, A-4, A-6, A-7, A-8, and A-9. Generates Figures 1, 2, A-3, A-4, A-5, and A-6. 
* `03_policy_change.R` conducts the observational analysis in the first results section of the paper. Generates Tables 3, A-1, A-2, and A-5. Generates several figures but none that appear in the manuscript of appendix.
* `04_educ_homeowner_quota.R` compares the sample distribution of education and homeownership to CPS targets. Generates Figures A-1 and A-2. 
* `05_analyze_mturk_180110.R` analyzes the supplemental MTurk survey reported in the appendix. Generates Table A-10 and Figures A-7 and A-8. 
* `06_misc_appendix.R` miscellaneous stuff that's in the appendix. Generates Table A-11.
* `functions.R` miscellaneous functions used throughout
* `generate_quotas.do`: a Stata file that generates the survey quotas using the November 2014 CPS supplement. 

# Data files
All data files needed to reproduce the results are included. The cleaned survey data is in the data file `cleaned_top20.RData`, which is called by the subsequent scripts. 

If you want to run `01_clean_checkquotas.R`, you need to separately download the raw csv Qualtrics export from the Dataverse. (The file is too large to be hosted on Github.) Download the file called `National_Top_20_MSA_Sample_v_22_final_sample.csv` from [Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NIMLAR) and place it in the `data` folder. 

# Software version information
All analysis were last run on a MacBook Pro running macOS 10.15.1 (Catalina) using R version 3.6.1. A list of all R packages called via `require()` or `library()` in the replication archive is below:
* Amelia 1.7.6
* assertthat 0.2.1
* car 3.0-5
* coefplot 1.2.6
* dplyr 0.8.3
* estimatr 0.20.0
* ggplot2 3.2.1
* ggthemes 4.2.0
* lmtest 0.9-37
* multcomp 1.4-11
* nnet 7.3-12
* reshape2 1.4.3
* sandwich 2.5-1
* stargazer 5.2.2
* stringr 1.4.0
* texreg 1.36.23
* xtable 1.8-4
