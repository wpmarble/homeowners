# Replication archive for "Where Self-Interest Trumps Ideology: Liberal
# Homeowners and Local Opposition to Housing Development" by William Marble
# and Clayton Nall.


# This file has miscellaneous functions used throughout the replication 
# archive. It is sourced at the top of each R file. 

library(dplyr)
select <- dplyr::select


# make directories --------------------------------------------------------

cat("Checking to make sure directories figs/ and tables/ exist and creating if not...\n\n")
if (!dir.exists("figs")) {dir.create("figs")}
if (!dir.exists("tables")) {dir.create("tables")}



# read_qualtrics_csv ------------------------------------------------------


# read csv file but skip some lines (useful for qualtrics-generate csv file)
read_qualtrics_csv = function(filename, skiplines, ...){
  whole_file = readLines(filename)
  dat = read.csv(textConnection(whole_file[-skiplines]), ...)
  return(dat)
}


# vcovCluster -------------------------------------------------------------

# compute clustered standard errors
# updated to handle missing data automatically
vcovCluster <- function(
  model,
  cluster
)
{
  require(sandwich)
  require(lmtest)
  
  cluster = as.factor(cluster)
  
  if (!is.null(model$na.action)){
    omit.rows = model$na.action
    cluster = cluster[-omit.rows]
  }
  if (nrow(model.matrix(model)) != length(cluster)){
    stop("something's not working: cluster variable has different N than model")
  }
  M <- length(unique(cluster))
  N <- length(cluster)           
  K <- model$rank   
  
  if(M<50){
    warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
  }
  
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj  <- na.omit(apply(estfun(model), 2, function(x) tapply(x, cluster, sum)))
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  return(rcse.cov)
}



# pcaEst -----------------------------------------------------------------

# run PCA on some data, imputing missing values using Amelia, run PCA on
# each of those datasets, then generate scores (first principal component)
# as the average across imputed datasets.
# Arguments: data - data.frame; impute.vars - character of names of variables 
# used to impute missing data; pca.vars - character of names of variables to
# run the PCA analysis on. ... - further arguments to Amelia

# For each imputed set, we need to make sure the direction of the first
# principal component matches. So we run a regression on one of the 
# variables used to create the index. This variable should be coded
# such that higher values indicate more support for redistribution.
# If the regression coefficient is negative, we flip the index scores 
# so that higher scores indicate more support for redistribution.
genPCAscores = function(data, impute.vars, pca.vars, testvar, ...) {
  
  library(Amelia)
  
  # subset to impute.vars and pca.vars
  data = data[, c(impute.vars, pca.vars)]
  data.imp = amelia(data, ...)
  
  data.imp$pca.out = list()
  scores = matrix(NaN, nrow = nrow(data), ncol = data.imp$m)
  for (i in 1:data.imp$m){
    this.dat = data.imp$imputations[[i]][, pca.vars]
    
    # standardize data
    this.dat = apply(this.dat, 2, function(x) (x - mean(x, na.rm=T)) / sd(x, na.rm=T))
    
    # perform PCA 
    data.imp$pca.out[[i]] = prcomp(this.dat, center=T, scale.=T)
    scores[,i] = data.imp$pca.out[[i]]$x[,1]
    
    mod = lm(scores[,i] ~ data[, testvar])
    if (mod$coefficients[2] < 0){
      scores[,i] = -1 * scores[, i]
    }
  }
  
  # take average of scores across m imputations
  avg.score = apply(scores, 1, function(x) mean(x))
  
  return(list(index = avg.score, imputes = data.imp))
}





# ggplot functions for coefplots ------------------------------------------

# all written by Jared Lander available from 
# https://github.com/jaredlander/coefplot/blob/master/R/position.r/
# Detect and prevent collisions.
# Powers dodging, stacking and filling.
library(ggplot2)
collidev <- function(data, height = NULL, name, strategy, check.height = TRUE) {
  # Determine height
  if (!is.null(height)) {
    # height set manually
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y - height / 2
      data$ymax <- data$y + height / 2
    }
  } else {
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y
      data$ymax <- data$y
    }
    
    # height determined from data, must be floating point constant
    heights <- unique(data$ymax - data$ymin)
    heights <- heights[!is.na(heights)]
    
    #   # Suppress warning message since it's not reliable
    #     if (!zero_range(range(heights))) {
    #       warning(name, " requires constant height: output may be incorrect",
    #         call. = FALSE)
    #     }
    height <- heights[1]
  }
  
  # Reorder by x position, relying on stable sort to preserve existing
  # ordering, which may be by group or order.
  data <- data[order(data$ymin), ]
  
  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("ymin", "ymax")])))
  intervals <- intervals[!is.na(intervals)]
  
  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping y intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots.
    # The American Statistician, 1999.] should be used
  }
  
  if (!is.null(data$xmax)) {
    plyr::ddply(data, "ymin", strategy, height = height)
  } else if (!is.null(data$x)) {
    data$xmax <- data$x
    data <- plyr::ddply(data, "ymin", strategy, height = height)
    data$x <- data$xmax
    data
  } else {
    stop("Neither x nor xmax defined")
  }
}

# Stack overlapping intervals.
# Assumes that each set has the same horizontal position
pos_stackv <- function(df, height) {
  if (nrow(df) == 1) return(df)
  
  n <- nrow(df) + 1
  x <- ifelse(is.na(df$x), 0, df$x)
  if (all(is.na(df$y))) {
    heights <- rep(NA, n)
  } else {
    heights <- c(0, cumsum(x))
  }
  
  df$xmin <- heights[-n]
  df$xmax <- heights[-1]
  df$x <- df$xmax
  df
}

# Stack overlapping intervals and set height to 1.
# Assumes that each set has the same horizontal position.
pos_fillv <- function(df, height) {
  stacked <- pos_stackv(df, height)
  stacked$xmin <- stacked$xmin / max(stacked$xmax)
  stacked$xmax <- stacked$xmax / max(stacked$xmax)
  stacked$x <- stacked$xmax
  stacked
}

# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.
pos_dodgev <- function(df, height) {
  n <- length(unique(df$group))
  if (n == 1) return(df)
  
  if (!all(c("ymin", "ymax") %in% names(df))) {
    df$ymin <- df$y
    df$ymax <- df$y
  }
  
  d_height <- max(df$ymax - df$ymin)
  
  # df <- data.frame(n = c(2:5, 10, 26), div = c(4, 3, 2.666666,  2.5, 2.2, 2.1))
  # ggplot(df, aes(n, div)) + geom_point()
  
  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidy <- match(df$group, sort(unique(df$group)))
  
  # Find the center for each group, then use that to calculate xmin and xmax
  df$y <- df$y + height * ((groupidy - 0.5) / n - .5)
  df$ymin <- df$y - d_height / n / 2
  df$ymax <- df$y + d_height / n / 2
  
  df
}


#' Adjust position by dodging overlaps to the side.
#'
#' @inheritParams ggplot2::position_identity
#' @param height Dodging height, when different to the height of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. See the examples for a use case.
#' @family position adjustments
#' @export
#' @examples
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
#'   geom_bar(position = "dodge")
#' \donttest{
#' ggplot(diamonds, aes(price, fill = cut)) +
#'   geom_histogram(position="dodge")
#' # see ?geom_boxplot and ?geom_bar for more examples
#'
#' # To dodge items with different heights, you need to be explicit
#' df <- data.frame(x=c("a","a","b","b"), y=2:5, g = rep(1:2, 2))
#' p <- ggplot(df, aes(x, y, group = g)) +
#'   geom_bar(
#'     stat = "identity", position = "dodge",
#'     fill = "grey50", colour = "black"
#'   )
#' p
#'
#' # A line range has no height:
#' p + geom_linerange(aes(ymin = y-1, ymax = y+1), position = "dodge")
#' # You need to explicitly specify the height for dodging
#' p + geom_linerange(aes(ymin = y-1, ymax = y+1),
#'   position = position_dodge(width = 0.9))
#'
#' # Similarly with error bars:
#' p + geom_errorbar(aes(ymin = y-1, ymax = y+1), width = 0.2,
#'   position = "dodge")
#' p + geom_errorbar(aes(ymin = y-1, ymax = y+1, height = 0.2),
#'   position = position_dodge(width = 0.90))
#' }
position_dodgev <- function(height = NULL) {
  ggproto(NULL, PositionDodgeV, height = height)
}



PositionDodgeV <- ggproto("PositionDodgeV", Position,
                          required_aes = "y",
                          height = NULL,
                          setup_params = function(self, data) {
                            if (is.null(data$ymin) && is.null(data$ymax) && is.null(self$height)) {
                              warning("height not defined. Set with `position_dodgev(height = ?)`",
                                      call. = FALSE)
                            }
                            list(height = self$height)
                          },
                          
                          compute_panel = function(data, params, scales) {
                            collidev(data, params$height, "position_dodgev", pos_dodgev, check.height = FALSE)
                          }
)


# get terciles ------------------------------------------------------------

tercileAssign = function(x){
  stopifnot(class(x) %in% c("numeric", "integer"))
  x = Hmisc::cut2(x, g = 3, levels.mean = T) %>% as.character %>% as.numeric
  ind.extrema = quantile(x, c(0, 1), na.rm=T)
  x = ifelse(x == ind.extrema[1], 1,
             ifelse(x == ind.extrema[2], 3,
             ifelse(is.na(x), NA, 2)))
  return(x)
}




# table for experiment 1 --------------------------------------------------
# Create tables for Experiment 1. There are separate tables for each outcome
# (high-, medium-, and low-density housing). Each row refers to a location
# on the homeownership-ideology 2x2. The first column refers to the mean outcome
# in the no-info condition. The other columns refer to ATEs relative to the
# no-info condition. We will also show the difference between owners and 
# renters in each condition. Significant levels will refer to the ATE comparisons,
# except in the own-rent difference rows, in which case it will refer to the
# comparison between owners and renters within that treatment condition.

exp1table = function(data, outcome, outcomelab, savetex = stdout()) { 
  stopifnot(outcome %in% names(data))
  require(estimatr);require(multcomp)
  data$outcome = data[[outcome]]
  
  # need to get slashes out of the gs_condition levels for multcomp::glht 
  data$gs_condition = gsub(pattern = "Economist/", replacement = "", x = data$gs_condition)
  
  # run fully saturated regression of outcome on interaction of treatment,
  # homeownership, and ideology
  reg = lm_robust(outcome ~ gs_condition * homeowner * fed_housing_bin, data, se_type = "HC2")

  
  
  
  # create data frame that will be used to store results
  df = data.frame(Ideology = c("Pro-Guarantee:", rep("", 5), "Anti-Guarantee:", rep("", 5)),
                  Homeownership = rep(c("Owners", "", "Renters", "", "Rent-Own Diff.", ""), 2),
                  No.Info = NA,Economist = NA, 
                  Escape = NA, Families = NA)
  
  # fill in results with appropriate significance levels
  # no info condition -- pro-guarantee
  # owners
  df$No.Info[1] = round(coef(reg)["(Intercept)"] + coef(reg)["fed_housing_binTRUE"] + coef(reg)["homeownerTRUE"] + coef(reg)["homeownerTRUE:fed_housing_binTRUE"], 2) %>% format(nsmall=2)
  
  # renters
  df$No.Info[3] = round(coef(reg)["(Intercept)"] + coef(reg)["fed_housing_binTRUE"], 2) %>% format(nsmall=2)
  
  # difference
  comp = summary(glht(reg, "-1 * (homeownerTRUE + homeownerTRUE:fed_housing_binTRUE) = 0"))
  pv = comp$test$pvalues
  se = comp$test$sigma
  df$No.Info[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$No.Info[5] = ifelse(pv < .01, paste0(df$No.Info[5], "^{**}"), ifelse(pv < .05, paste0(df$No.Info[5], "^{*}"), df$No.Info[5]))
  df$No.Info[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # No-info condition -- anti-guarantee
  # owners
  df$No.Info[7] = round(coef(reg)["(Intercept)"] + coef(reg)["homeownerTRUE"], 2) %>% format(nsmall=2)
  
  # renters
  df$No.Info[9] = round(coef(reg)["(Intercept)"], 2) %>% format(nsmall=2)
  
  # difference
  comp = summary(glht(reg, "-1 * homeownerTRUE = 0"))
  pv = comp$test$pvalues
  se = comp$test$sigma
  df$No.Info[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$No.Info[11] = ifelse(pv < .01, paste0(df$No.Info[11], "^{**}"), ifelse(pv < .05, paste0(df$No.Info[5], "^{*}"), df$No.Info[11]))
  df$No.Info[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Economist condition -- Pro-guarantee
  # owners
  comp = summary(glht(reg, "gs_conditionEconomist + gs_conditionEconomist:homeownerTRUE + gs_conditionEconomist:fed_housing_binTRUE + gs_conditionEconomist:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[1] = ifelse(pv < .01, paste0(df$Economist[1], "^{**}"), ifelse(pv < .05, paste0(df$Economist[1], "^{*}"), df$Economist[1]))
  df$Economist[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "gs_conditionEconomist + gs_conditionEconomist:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[3] = ifelse(pv < .01, paste0(df$Economist[3], "^{**}"), ifelse(pv < .05, paste0(df$Economist[3], "^{*}"), df$Economist[3]))
  df$Economist[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * (gs_conditionEconomist:homeownerTRUE + gs_conditionEconomist:homeownerTRUE:fed_housing_binTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[5] = ifelse(pv < .01, paste0(df$Economist[5], "^{**}"), ifelse(pv < .05, paste0(df$Economist[5], "^{*}"), df$Economist[5]))
  df$Economist[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Economist condition -- Anti-guarantee
  # owners
  comp = summary(glht(reg, "gs_conditionEconomist + gs_conditionEconomist:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[7] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[7] = ifelse(pv < .01, paste0(df$Economist[7], "^{**}"), ifelse(pv < .05, paste0(df$Economist[7], "^{*}"), df$Economist[7]))
  df$Economist[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "gs_conditionEconomist = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[9] = ifelse(pv < .01, paste0(df$Economist[9], "^{**}"), ifelse(pv < .05, paste0(df$Economist[9], "^{*}"), df$Economist[9]))
  df$Economist[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * gs_conditionEconomist:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[11] = ifelse(pv < .01, paste0(df$Economist[11], "^{**}"), ifelse(pv < .05, paste0(df$Economist[11], "^{*}"), df$Economist[11]))
  df$Economist[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  # Escape condition -- pro-guarantee
  # owners
  comp = summary(glht(reg, "gs_conditionEscape + gs_conditionEscape:homeownerTRUE + gs_conditionEscape:fed_housing_binTRUE + gs_conditionEscape:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Escape[1] = ifelse(pv < .01, paste0(df$Escape[1], "^{**}"), ifelse(pv < .05, paste0(df$Escape[1], "^{*}"), df$Escape[1]))
  df$Escape[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "gs_conditionEscape + gs_conditionEscape:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Escape[3] = ifelse(pv < .01, paste0(df$Escape[3], "^{**}"), ifelse(pv < .05, paste0(df$Escape[3], "^{*}"), df$Escape[3]))
  df$Escape[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * (gs_conditionEscape:homeownerTRUE + gs_conditionEscape:homeownerTRUE:fed_housing_binTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Escape[5] = ifelse(pv < .01, paste0(df$Escape[5], "^{**}"), ifelse(pv < .05, paste0(df$Escape[5], "^{*}"), df$Escape[5]))
  df$Escape[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Escape condition -- Anti-guarantee
  # owners
  comp = summary(glht(reg, "gs_conditionEscape + gs_conditionEscape:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[7] = round(comp$test$coefficients, 3)
  df$Escape[7] = ifelse(pv < .01, paste0(df$Escape[7], "^{**}"), ifelse(pv < .05, paste0(df$Escape[7], "^{*}"), df$Escape[7]))
  df$Escape[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "gs_conditionEscape = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Escape[9] = ifelse(pv < .01, paste0(df$Escape[9], "^{**}"), ifelse(pv < .05, paste0(df$Escape[9], "^{*}"), df$Escape[9]))
  df$Escape[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * (gs_conditionEscape:homeownerTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Escape[11] = ifelse(pv < .01, paste0(df$Escape[11], "^{**}"), ifelse(pv < .05, paste0(df$Escape[11], "^{*}"), df$Escape[11]))
  df$Escape[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Families condition -- pro-guarantee
  # owners
  comp = summary(glht(reg, "gs_conditionFamilies + gs_conditionFamilies:homeownerTRUE + gs_conditionFamilies:fed_housing_binTRUE + gs_conditionFamilies:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Families[1] = ifelse(pv < .01, paste0(df$Families[1], "^{**}"), ifelse(pv < .05, paste0(df$Families[1], "^{*}"), df$Families[1]))
  df$Families[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "gs_conditionFamilies + gs_conditionFamilies:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Families[3] = ifelse(pv < .01, paste0(df$Families[3], "^{**}"), ifelse(pv < .05, paste0(df$Families[3], "^{*}"), df$Families[3]))
  df$Families[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * (gs_conditionFamilies:homeownerTRUE + gs_conditionFamilies:homeownerTRUE:fed_housing_binTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Families[5] = ifelse(pv < .01, paste0(df$Families[5], "^{**}"), ifelse(pv < .05, paste0(df$Families[5], "^{*}"), df$Families[5]))
  df$Families[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Families condition -- Anti-guarantee
  # owners
  comp = summary(glht(reg, "gs_conditionFamilies + gs_conditionFamilies:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[7] = round(comp$test$coefficients, 3)
  df$Families[7] = ifelse(pv < .01, paste0(df$Families[7], "^{**}"), ifelse(pv < .05, paste0(df$Families[7], "^{*}"), df$Families[7]))
  df$Families[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "gs_conditionFamilies = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Families[9] = ifelse(pv < .01, paste0(df$Families[9], "^{**}"), ifelse(pv < .05, paste0(df$Families[9], "^{*}"), df$Families[9]))
  df$Families[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * gs_conditionFamilies:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Families[11] = ifelse(pv < .01, paste0(df$Families[11], "^{**}"), ifelse(pv < .05, paste0(df$Families[11], "^{*}"), df$Families[11]))
  df$Families[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
 
  
  # format table as latex
  tex = df
  tex[is.na(tex)] = ""
  tex = apply(tex, 1, function(x) paste(x, collapse = "   &   "))
  tex = paste0(tex, "  \\\\")
  tex[12] = paste0(tex[12], " \\bottomrule")
  tex = c("\\multicolumn{1}{c}{Ideology}  &  \\multicolumn{1}{c}{Homeownership}  &  \\multicolumn{1}{c}{No-Info}  &  \\multicolumn{1}{c}{Economist}  &  \\multicolumn{1}{c}{Escape}  &  \\multicolumn{1}{c}{Families} \\\\ \\midrule", tex)
  
  # put it into a tabular environment
  top = paste0("\\begin{tabular}{rrD{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}}  \\multicolumn{6}{c}{", outcomelab, "} \\\\ \\toprule" )
  second = "  &  &  \\multicolumn{1}{c}{\\underline{Outcome Mean}}  &  \\multicolumn{3}{c}{\\underline{Average Treatment Effect}} \\\\ "
  bottom = "\\end{tabular}"
  
  tex = c(top, second, tex, bottom)
  tex = paste(tex, collapse = "\n")
  
  # print output to file
  cat(tex, file = savetex)
  
  # return
  out = list(df, tex)
  return(out)
}





# table for experiment 1, v2 --------------------------------------------------

# Create tables for Experiment 1. There are separate tables for each outcome
# (high-, medium-, and low-density housing). Each row refers to a location
# on the homeownership-ideology 2x2. The first column refers to the mean outcome
# in the no-info condition. The other columns refer to ATEs relative to the
# no-info condition. We will also show the difference between liberals and 
# conservatives in each condition. Significance levels will refer to the ATE
# ATE comparisons.

exp1table_2 = function(data, outcome, outcomelab, savetex = stdout()) { 
  stopifnot(outcome %in% names(data))
  require(estimatr);require(multcomp)
  data$outcome = data[[outcome]]
  
  # need to get slashes out of the gs_condition levels for multcomp::glht 
  data$gs_condition = gsub(pattern = "Economist/", replacement = "", x = data$gs_condition)
  
  # run fully saturated regression of outcome on interaction of treatment,
  # homeownership, and ideology
  reg = lm_robust(outcome ~ gs_condition * homeowner * fed_housing_bin, data, se_type = "HC2")
  
  
  
  
  # create data frame that will be used to store results
  df = data.frame(Homeownership = c("Homeowners:", rep("", 5), "Renters:", rep("", 5)),
                  Ideology = rep(c("Pro-Guarantee", "", "Anti-Guarantee", "", "Lib.-Con. Difference", ""), 2),
                  No.Info = NA,Economist = NA, 
                  Escape = NA, Families = NA)
  
  
  # fill in results with appropriate significance levels
  # no info condition -- homeowners
  # liberal
  df$No.Info[1] = round(coef(reg)["(Intercept)"] + coef(reg)["homeownerTRUE"] + coef(reg)["fed_housing_binTRUE"] + coef(reg)["homeownerTRUE:fed_housing_binTRUE"], 2) %>% format(nsmall=2)
  
  # conservative
  df$No.Info[3] = round(coef(reg)["(Intercept)"] + coef(reg)["homeownerTRUE"], 2) %>% format(nsmall=2)
  
  # difference
  comp = summary(glht(reg, "(fed_housing_binTRUE + homeownerTRUE:fed_housing_binTRUE) = 0"))
  pv = comp$test$pvalues
  se = comp$test$sigma
  df$No.Info[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$No.Info[5] = ifelse(pv < .01, paste0(df$No.Info[5], "^{**}"), ifelse(pv < .05, paste0(df$No.Info[5], "^{*}"), df$No.Info[5]))
  df$No.Info[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # No-info condition -- renters
  # liberal
  df$No.Info[7] = round(coef(reg)["(Intercept)"] + coef(reg)["fed_housing_binTRUE"], 2)
  
  # conservative
  df$No.Info[9] = round(coef(reg)["(Intercept)"], 2)
  
  # difference
  comp = summary(glht(reg, "fed_housing_binTRUE = 0"))
  pv = comp$test$pvalues
  se = comp$test$sigma
  df$No.Info[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$No.Info[11] = ifelse(pv < .01, paste0(df$No.Info[11], "^{**}"), ifelse(pv < .05, paste0(df$No.Info[5], "^{*}"), df$No.Info[11]))
  df$No.Info[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Economist condition -- Homeowners
  # liberal
  comp = summary(glht(reg, "gs_conditionEconomist + gs_conditionEconomist:homeownerTRUE + gs_conditionEconomist:fed_housing_binTRUE + gs_conditionEconomist:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[1] = ifelse(pv < .01, paste0(df$Economist[1], "^{**}"), ifelse(pv < .05, paste0(df$Economist[1], "^{*}"), df$Economist[1]))
  df$Economist[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  # conservative
  comp = summary(glht(reg, "gs_conditionEconomist + gs_conditionEconomist:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[3] = ifelse(pv < .01, paste0(df$Economist[3], "^{**}"), ifelse(pv < .05, paste0(df$Economist[3], "^{*}"), df$Economist[3]))
  df$Economist[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "(gs_conditionEconomist:fed_housing_binTRUE + gs_conditionEconomist:homeownerTRUE:fed_housing_binTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[5] = ifelse(pv < .01, paste0(df$Economist[5], "^{**}"), ifelse(pv < .05, paste0(df$Economist[5], "^{*}"), df$Economist[5]))
  df$Economist[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  # Economist condition -- renters
  # liberal
  comp = summary(glht(reg, "gs_conditionEconomist + gs_conditionEconomist:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[7] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[7] = ifelse(pv < .01, paste0(df$Economist[7], "^{**}"), ifelse(pv < .05, paste0(df$Economist[7], "^{*}"), df$Economist[7]))
  df$Economist[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservative
  comp = summary(glht(reg, "gs_conditionEconomist = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[9] = ifelse(pv < .01, paste0(df$Economist[9], "^{**}"), ifelse(pv < .05, paste0(df$Economist[9], "^{*}"), df$Economist[9]))
  df$Economist[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "gs_conditionEconomist:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Economist[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Economist[11] = ifelse(pv < .01, paste0(df$Economist[11], "^{**}"), ifelse(pv < .05, paste0(df$Economist[11], "^{*}"), df$Economist[11]))
  df$Economist[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  # Escape condition -- homeowners
  # liberal
  comp = summary(glht(reg, "gs_conditionEscape + gs_conditionEscape:homeownerTRUE + gs_conditionEscape:fed_housing_binTRUE + gs_conditionEscape:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Escape[1] = ifelse(pv < .01, paste0(df$Escape[1], "^{**}"), ifelse(pv < .05, paste0(df$Escape[1], "^{*}"), df$Escape[1]))
  df$Escape[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservative
  comp = summary(glht(reg, "gs_conditionEscape + gs_conditionEscape:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Escape[3] = ifelse(pv < .01, paste0(df$Escape[3], "^{**}"), ifelse(pv < .05, paste0(df$Escape[3], "^{*}"), df$Escape[3]))
  df$Escape[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "gs_conditionEscape:fed_housing_binTRUE + gs_conditionEscape:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Escape[5] = ifelse(pv < .01, paste0(df$Escape[5], "^{**}"), ifelse(pv < .05, paste0(df$Escape[5], "^{*}"), df$Escape[5]))
  df$Escape[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Escape condition -- Renters
  # liberal
  comp = summary(glht(reg, "gs_conditionEscape + gs_conditionEscape:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[7] = round(comp$test$coefficients, 3)
  df$Escape[7] = ifelse(pv < .01, paste0(df$Escape[7], "^{**}"), ifelse(pv < .05, paste0(df$Escape[7], "^{*}"), df$Escape[7]))
  df$Escape[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservative
  comp = summary(glht(reg, "gs_conditionEscape = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Escape[9] = ifelse(pv < .01, paste0(df$Escape[9], "^{**}"), ifelse(pv < .05, paste0(df$Escape[9], "^{*}"), df$Escape[9]))
  df$Escape[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "(gs_conditionEscape:fed_housing_binTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Escape[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Escape[11] = ifelse(pv < .01, paste0(df$Escape[11], "^{**}"), ifelse(pv < .05, paste0(df$Escape[11], "^{*}"), df$Escape[11]))
  df$Escape[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Families condition -- homeowners
  # liberal
  comp = summary(glht(reg, "gs_conditionFamilies + gs_conditionFamilies:homeownerTRUE + gs_conditionFamilies:fed_housing_binTRUE + gs_conditionFamilies:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Families[1] = ifelse(pv < .01, paste0(df$Families[1], "^{**}"), ifelse(pv < .05, paste0(df$Families[1], "^{*}"), df$Families[1]))
  df$Families[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservative
  comp = summary(glht(reg, "gs_conditionFamilies + gs_conditionFamilies:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Families[3] = ifelse(pv < .01, paste0(df$Families[3], "^{**}"), ifelse(pv < .05, paste0(df$Families[3], "^{*}"), df$Families[3]))
  df$Families[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, " gs_conditionFamilies:fed_housing_binTRUE + gs_conditionFamilies:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Families[5] = ifelse(pv < .01, paste0(df$Families[5], "^{**}"), ifelse(pv < .05, paste0(df$Families[5], "^{*}"), df$Families[5]))
  df$Families[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Families condition -- renters
  # liberal
  comp = summary(glht(reg, "gs_conditionFamilies + gs_conditionFamilies:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[7] = round(comp$test$coefficients, 3)
  df$Families[7] = ifelse(pv < .01, paste0(df$Families[7], "^{**}"), ifelse(pv < .05, paste0(df$Families[7], "^{*}"), df$Families[7]))
  df$Families[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservative
  comp = summary(glht(reg, "gs_conditionFamilies = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Families[9] = ifelse(pv < .01, paste0(df$Families[9], "^{**}"), ifelse(pv < .05, paste0(df$Families[9], "^{*}"), df$Families[9]))
  df$Families[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "gs_conditionFamilies:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Families[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Families[11] = ifelse(pv < .01, paste0(df$Families[11], "^{**}"), ifelse(pv < .05, paste0(df$Families[11], "^{*}"), df$Families[11]))
  df$Families[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  # format table as latex
  tex = df
  tex[is.na(tex)] = ""
  tex = apply(tex, 1, function(x) paste(x, collapse = "   &   "))
  tex = paste0(tex, "  \\\\")
  tex[12] = paste0(tex[12], " \\bottomrule")
  tex = c("\\multicolumn{1}{c}{Ideology}  &  \\multicolumn{1}{c}{Homeownership}  &  \\multicolumn{1}{p{1.95cm}}{No-Info}  &  \\multicolumn{1}{p{1.95cm}}{Economist}  &  \\multicolumn{1}{p{1.95cm}}{Economist + Escape}  &  \\multicolumn{1}{p{1.95cm}}{Economist + Families} \\\\ \\midrule", tex)
  
  # put it into a tabular environment
  top = paste0("\\begin{tabular}{rrD{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}}  \\multicolumn{6}{c}{", outcomelab, "} \\\\ \\toprule" )
  second = "  &  &  \\multicolumn{1}{c}{\\underline{Outcome Mean}}  &  \\multicolumn{3}{c}{\\underline{Average Treatment Effect}} \\\\ "
  bottom = "\\end{tabular}"
  
  tex = c(top, second, tex, bottom)
  tex = paste(tex, collapse = "\n")
  
  # print output to file
  cat(tex, file = savetex)
  
  # return
  out = list(df, tex)
  return(out)
}




# experiment 1 means ------------------------------------------------------


# Creates a table that looks like this:

# Condition   Lib Renters       Con Renters     Lib HO     Con HO 
# ------------------------------------------------------------------
# Control      (Mean)              ....           ...        ... 
#              ( SE )              ....           ...        ... 
# Economist    (Mean)              ....           ...        ... 
#              ( SE )              ....           ...        ... 
# ...

# optional args for labeling the outcome and file to write tex 
exp1means = function(data, outcome, outcomelab = outcome, savetex = stdout(), incl.con.renters = TRUE) { 
 
  stopifnot(outcome %in% names(data))
  data$outcome = data[[outcome]]
  
  # need to get slashes out of the gs_condition levels for multcomp::glht 
  data$gs_condition = gsub(pattern = "Economist/", replacement = "", x = data$gs_condition)
  
  # omit conservative renters? 
  if (!incl.con.renters) data = subset(data, homeownerXguar != "Anti-guarantee renter")
  
  # create data frame that will be used to store results
  df = data %>% 
    filter(!is.na(outcome)) %>% 
    group_by(gs_condition, homeownerXguar) %>% 
    summarise(mean = sprintf(round(mean(outcome),2), fmt= "%0.2f"),
              se   = sprintf(round(se_mean(outcome), 2), fmt = "%0.2f"))
  
  df = reshape2::melt(df, id.vars = c("gs_condition", "homeownerXguar"))
  df = reshape2::dcast(df, gs_condition + variable ~ homeownerXguar, value.var = "value")
  
  if (incl.con.renters){
    df[df$variable == "se", 3:6] = apply(df[df$variable == "se", 3:6], 1, function(x) paste0("(", x,")"))
    df$gs_condition[seq(2, nrow(df), 2)] = ""
    df$variable = NULL
    df = df[, c("gs_condition", "Anti-guarantee homeowner","Pro-guarantee homeowner", "Anti-guarantee renter",  "Pro-guarantee renter")]
  } else {
    df[df$variable == "se", 3:5] = apply(df[df$variable == "se", 3:5], 1, function(x) paste0("(", x,")"))
    df$gs_condition[seq(2, nrow(df), 2)] = ""
    df$variable = NULL
    df = df[, c("gs_condition", "Anti-guarantee homeowner", "Pro-guarantee homeowner", "Pro-guarantee renter")]
  }
  
  # format as latex table
  tex = df
  tex = apply(tex, 1, function(x) paste(x, collapse = "   &   "))
  tex = paste0(tex, "  \\\\")
  tex[length(tex)] = paste0(tex[length(tex)], " \\bottomrule")
  
  
  # put it into a tabular environment
  outcomelab = gsub("_", "\\_", outcomelab, fixed = TRUE)
  if (incl.con.renters){
    top = paste0("\\begin{tabular}{rD{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}} \n \\multicolumn{5}{c}{", outcomelab, "} \\\\ \\toprule" )
    second = "  &  \\multicolumn{1}{r}{Anti-guarantee }  &    \\multicolumn{1}{r}{Pro-guarantee } &  \\multicolumn{1}{r}{Anti-guarantee }  & \\multicolumn{1}{r}{Pro-guarantee } \\\\  "
    third = "Condition  &  \\multicolumn{1}{r}{ homeowner}   &  \\multicolumn{1}{r}{ homeowner} &  \\multicolumn{1}{r}{ renter} & \\multicolumn{1}{r}{ renter} \\\\ \\midrule "
  } else {
    top = paste0("\\begin{tabular}{rD{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}} \n \\multicolumn{4}{c}{", outcomelab, "} \\\\ \\toprule" )
    second = "  &  \\multicolumn{1}{r}{Anti-guarantee }    &  \\multicolumn{1}{r}{Pro-guarantee } & \\multicolumn{1}{r}{Pro-guarantee } \\\\  "
    third = "Condition  &  \\multicolumn{1}{r}{ homeowner}  &    \\multicolumn{1}{r}{ homeowner} & \\multicolumn{1}{r}{ renter} \\\\ \\midrule "
  }
  bottom = "\\end{tabular}"
  
  tex = c(top, second, third, tex, bottom)
  tex = paste(tex, collapse = "\n")
  
  # print output to file
  cat(tex, file = savetex)
  
  # return
  out = list(df, tex)
  return(out)
}




# Experiment 2 means ------------------------------------------------------

# same as above
exp2means = function(data, savetex = stdout(), incl.con.renters = TRUE) { 
  
  # need to get slashes out of the gs_condition levels for multcomp::glht 
  data$nimby_condition = gsub(pattern = "Control", replacement = "No Info", x = data$nimby_condition)
  data$nimby_condition = gsub(pattern = "Income", replacement = "Inc.", x = data$nimby_condition)
  data$nimby_condition = gsub(pattern = "Quarter", replacement = "1/4", x = data$nimby_condition)
  data$nimby_condition = gsub(pattern = "Two", replacement = "2", x = data$nimby_condition)
  data$nimby_condition = gsub(pattern = "Market", replacement = "Mkt.", x = data$nimby_condition)
  data$nimby_condition = gsub(pattern = "Distance Not Specified", replacement = "Dist. Not Given", x = data$nimby_condition)
  data$nimby_condition = gsub(pattern = "Low-Inc", replacement = "Low Inc", x = data$nimby_condition)
  data$nimby_condition = trimws(data$nimby_condition)
  data$nimby_condition = factor(data$nimby_condition,
                                c("No Info", "Low Inc., Dist. Not Given", 
                                  "Low Inc., 1/4 Mile", "Low Inc., 2 Miles",
                                  "Mkt. Rate, 1/4 Mile", "Mkt. Rate, 2 Miles"))
  
  # omit conservative renters? 
  if (!incl.con.renters) data = subset(data, homeownerXguar != "Anti-guarantee renter")
  
  # create data frame that will be used to store results
  df = data %>% 
    filter(!is.na(nimby_support)) %>% 
    group_by(nimby_condition, homeownerXguar) %>% 
    summarise(mean = sprintf(round(mean(nimby_support),2), fmt= "%0.2f"),
              se   = sprintf(round(se_mean(nimby_support), 2), fmt = "%0.2f"))
  
  df = reshape2::melt(df, id.vars = c("nimby_condition", "homeownerXguar"))
  df = reshape2::dcast(df, nimby_condition + variable ~ homeownerXguar, value.var = "value")
  df$nimby_condition = as.character(df$nimby_condition)
  
  if (incl.con.renters){
    df[df$variable == "se", 3:6] = apply(df[df$variable == "se", 3:6], 1, function(x) paste0("(", x,")"))
    df$nimby_condition[seq(2, nrow(df), 2)] = ""
    df$variable = NULL
    df = df[, c("nimby_condition", "Anti-guarantee homeowner", "Pro-guarantee homeowner", "Anti-guarantee renter", "Pro-guarantee renter")]
  } else {
    df[df$variable == "se", 3:5] = apply(df[df$variable == "se", 3:5], 1, function(x) paste0("(", x,")"))
    df$nimby_condition[seq(2, nrow(df), 2)] = ""
    df$variable = NULL
    df = df[, c("nimby_condition", "Anti-guarantee homeowner", "Pro-guarantee homeowner", "Pro-guarantee renter")]
  }
  
  # format as latex table
  tex = df
  tex = apply(tex, 1, function(x) paste(x, collapse = "   &   "))
  tex = paste0(tex, "  \\\\")
  tex[length(tex)] = paste0(tex[length(tex)], " \\bottomrule")
  
  
  # put it into a tabular environment
  if (incl.con.renters){
    top = paste0("\\begin{tabular}{rD{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}} \n \\multicolumn{5}{c}{Support for Building 120-Unit Apt. Building} \\\\ \\toprule" )
    second = "  &  \\multicolumn{1}{r}{Anti-guarantee }  &  \\multicolumn{1}{r}{Pro-guarantee }  &  \\multicolumn{1}{r}{Anti-guarantee } & \\multicolumn{1}{r}{Pro-guarantee } \\\\  "
    third = "Condition  &  \\multicolumn{1}{r}{ homeowner}  &  \\multicolumn{1}{r}{ homeowner}  &  \\multicolumn{1}{r}{ renter} & \\multicolumn{1}{r}{ renter} \\\\ \\midrule "
  } else {
    top = paste0("\\begin{tabular}{rD{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}} \n \\multicolumn{4}{c}{Support for Building 120-Unit Apt. Building} \\\\ \\toprule" )
    second = "  &  \\multicolumn{1}{r}{Anti-guarantee }    &  \\multicolumn{1}{r}{Pro-guarantee } & \\multicolumn{1}{r}{Pro-guarantee } \\\\  "
    third = "Condition  &  \\multicolumn{1}{r}{ homeowner}  &    \\multicolumn{1}{r}{ homeowner} & \\multicolumn{1}{r}{ renter} \\\\ \\midrule "
  }
  bottom = "\\end{tabular}"
  
  tex = c(top, second, third, tex, bottom)
  tex = paste(tex, collapse = "\n")
  
  # print output to file
  cat(tex, file = savetex)
  
  # return
  out = list(df, tex)
  return(out)
}
  


# table for experiment 2 --------------------------------------------------

# Basically the same as exp1table()

exp2table = function(data, outcome, outcomelab, savetex = stdout()) { 
  stopifnot(outcome %in% names(data))
  require(estimatr);require(multcomp)
  data$outcome = data[[outcome]]
  
  # get rid of spaces in nimby_condition
  data$nimby_condition = gsub(" ", "", data$nimby_condition)
  data$nimby_condition = gsub(",", "", data$nimby_condition)
  data$nimby_condition = gsub("-", "", data$nimby_condition)
  
  
  # run fully saturated regression of outcome on interaction of treatment,
  # homeownership, and ideology
  reg = lm_robust(outcome ~ nimby_condition * homeowner * fed_housing_bin, data, se_type = "HC2")
  
  
  
  
  # create data frame that will be used to store results
  df = data.frame(Ideology = c("Pro-Guarantee:", rep("", 5), "Anti-Guarantee:", rep("", 5)),
                  Homeownership = rep(c("Owners", "", "Renters", "", "Rent-Own Diff.", ""), 2),
                  No.Info = NA, Low.Income = NA, 
                  Low.Income.Quarter.Mile = NA,
                  Low.Income.Two.Miles = NA,
                  Market.Rate.Quarter.Mile = NA,
                  Market.Rate.Two.Miles = NA)
  
  # fill in results with appropriate significance levels
  # no info condition -- pro-guarantee
  # owners
  df$No.Info[1] = round(coef(reg)["(Intercept)"] + coef(reg)["fed_housing_binTRUE"] + coef(reg)["homeownerTRUE"] + coef(reg)["homeownerTRUE:fed_housing_binTRUE"], 2) %>% format(nsmall=2)
  
  # renters
  df$No.Info[3] = round(coef(reg)["(Intercept)"] + coef(reg)["fed_housing_binTRUE"], 2) %>% format(nsmall=2)
  
  # difference
  comp = summary(glht(reg, "-1 * (homeownerTRUE + homeownerTRUE:fed_housing_binTRUE) = 0"))
  pv = comp$test$pvalues
  se = comp$test$sigma
  df$No.Info[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$No.Info[5] = ifelse(pv < .01, paste0(df$No.Info[5], "^{**}"), ifelse(pv < .05, paste0(df$No.Info[5], "^{*}"), df$No.Info[5]))
  df$No.Info[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # No-info condition -- anti-guarantee
  # owners
  df$No.Info[7] = round(coef(reg)["(Intercept)"] + coef(reg)["homeownerTRUE"], 2)
  
  # renters
  df$No.Info[9] = round(coef(reg)["(Intercept)"], 2)
  
  # difference
  comp = summary(glht(reg, "-1 * homeownerTRUE = 0"))
  pv = comp$test$pvalues
  se = comp$test$sigma
  df$No.Info[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$No.Info[11] = ifelse(pv < .01, paste0(df$No.Info[11], "^{**}"), ifelse(pv < .05, paste0(df$No.Info[5], "^{*}"), df$No.Info[11]))
  df$No.Info[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  # Low income, no distance condition -- Pro-guarantee
  # owners
  comp = summary(glht(reg, "nimby_conditionLowIncomeDistanceNotSpecified + nimby_conditionLowIncomeDistanceNotSpecified:homeownerTRUE + nimby_conditionLowIncomeDistanceNotSpecified:fed_housing_binTRUE + nimby_conditionLowIncomeDistanceNotSpecified:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[1] = ifelse(pv < .01, paste0(df$Low.Income[1], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[1], "^{*}"), df$Low.Income[1]))
  df$Low.Income[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "nimby_conditionLowIncomeDistanceNotSpecified + nimby_conditionLowIncomeDistanceNotSpecified:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[3] = ifelse(pv < .01, paste0(df$Low.Income[3], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[3], "^{*}"), df$Low.Income[3]))
  df$Low.Income[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * (nimby_conditionLowIncomeDistanceNotSpecified:homeownerTRUE + nimby_conditionLowIncomeDistanceNotSpecified:homeownerTRUE:fed_housing_binTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[5] = ifelse(pv < .01, paste0(df$Low.Income[5], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[5], "^{*}"), df$Low.Income[5]))
  df$Low.Income[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Low income, no distance info condition -- Anti-guarantee
  # owners
  comp = summary(glht(reg, "nimby_conditionLowIncomeDistanceNotSpecified + nimby_conditionLowIncomeDistanceNotSpecified:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[7] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[7] = ifelse(pv < .01, paste0(df$Low.Income[7], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[7], "^{*}"), df$Low.Income[7]))
  df$Low.Income[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "nimby_conditionLowIncomeDistanceNotSpecified = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[9] = ifelse(pv < .01, paste0(df$Low.Income[9], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[9], "^{*}"), df$Low.Income[9]))
  df$Low.Income[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * nimby_conditionLowIncomeDistanceNotSpecified:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[11] = ifelse(pv < .01, paste0(df$Low.Income[11], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[11], "^{*}"), df$Low.Income[11]))
  df$Low.Income[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  
  
  # Low income, quarter mile condition -- pro-guarantee
  # owners
  comp = summary(glht(reg, "nimby_conditionLowIncomeQuarterMile + nimby_conditionLowIncomeQuarterMile:homeownerTRUE + nimby_conditionLowIncomeQuarterMile:fed_housing_binTRUE + nimby_conditionLowIncomeQuarterMile:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Quarter.Mile[1] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[1], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[1], "^{*}"), df$Low.Income.Quarter.Mile[1]))
  df$Low.Income.Quarter.Mile[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "nimby_conditionLowIncomeQuarterMile + nimby_conditionLowIncomeQuarterMile:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Quarter.Mile[3] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[3], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[3], "^{*}"), df$Low.Income.Quarter.Mile[3]))
  df$Low.Income.Quarter.Mile[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * (nimby_conditionLowIncomeQuarterMile:homeownerTRUE + nimby_conditionLowIncomeQuarterMile:homeownerTRUE:fed_housing_binTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Quarter.Mile[5] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[5], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[5], "^{*}"), df$Low.Income.Quarter.Mile[5]))
  df$Low.Income.Quarter.Mile[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Low income, quarter mile condition -- Anti-guarantee
  # owners
  comp = summary(glht(reg, "nimby_conditionLowIncomeQuarterMile + nimby_conditionLowIncomeQuarterMile:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[7] = round(comp$test$coefficients, 3)
  df$Low.Income.Quarter.Mile[7] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[7], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[7], "^{*}"), df$Low.Income.Quarter.Mile[7]))
  df$Low.Income.Quarter.Mile[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "nimby_conditionLowIncomeQuarterMile = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Quarter.Mile[9] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[9], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[9], "^{*}"), df$Low.Income.Quarter.Mile[9]))
  df$Low.Income.Quarter.Mile[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * (nimby_conditionLowIncomeQuarterMile:homeownerTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Quarter.Mile[11] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[11], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[11], "^{*}"), df$Low.Income.Quarter.Mile[11]))
  df$Low.Income.Quarter.Mile[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  # Low income, two miles condition -- pro-guarantee
  # owners
  comp = summary(glht(reg, "nimby_conditionLowIncomeTwoMiles + nimby_conditionLowIncomeTwoMiles:homeownerTRUE + nimby_conditionLowIncomeTwoMiles:fed_housing_binTRUE + nimby_conditionLowIncomeTwoMiles:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Two.Miles[1] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[1], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[1], "^{*}"), df$Low.Income.Two.Miles[1]))
  df$Low.Income.Two.Miles[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "nimby_conditionLowIncomeTwoMiles + nimby_conditionLowIncomeTwoMiles:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Two.Miles[3] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[3], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[3], "^{*}"), df$Low.Income.Two.Miles[3]))
  df$Low.Income.Two.Miles[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * (nimby_conditionLowIncomeTwoMiles:homeownerTRUE + nimby_conditionLowIncomeTwoMiles:homeownerTRUE:fed_housing_binTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Two.Miles[5] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[5], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[5], "^{*}"), df$Low.Income.Two.Miles[5]))
  df$Low.Income.Two.Miles[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Low income, two miles condition -- Anti-guarantee
  # owners
  comp = summary(glht(reg, "nimby_conditionLowIncomeTwoMiles + nimby_conditionLowIncomeTwoMiles:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[7] = round(comp$test$coefficients, 3)
  df$Low.Income.Two.Miles[7] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[7], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[7], "^{*}"), df$Low.Income.Two.Miles[7]))
  df$Low.Income.Two.Miles[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "nimby_conditionLowIncomeTwoMiles = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Two.Miles[9] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[9], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[9], "^{*}"), df$Low.Income.Two.Miles[9]))
  df$Low.Income.Two.Miles[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * nimby_conditionLowIncomeTwoMiles:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Two.Miles[11] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[11], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[11], "^{*}"), df$Low.Income.Two.Miles[11]))
  df$Low.Income.Two.Miles[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  
  # Market rate, quarter mile condition -- pro-guarantee
  # owners
  comp = summary(glht(reg, "nimby_conditionMarketRateQuarterMile + nimby_conditionMarketRateQuarterMile:homeownerTRUE + nimby_conditionMarketRateQuarterMile:fed_housing_binTRUE + nimby_conditionMarketRateQuarterMile:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Quarter.Mile[1] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[1], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[1], "^{*}"), df$Market.Rate.Quarter.Mile[1]))
  df$Market.Rate.Quarter.Mile[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "nimby_conditionMarketRateQuarterMile + nimby_conditionMarketRateQuarterMile:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Quarter.Mile[3] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[3], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[3], "^{*}"), df$Market.Rate.Quarter.Mile[3]))
  df$Market.Rate.Quarter.Mile[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * (nimby_conditionMarketRateQuarterMile:homeownerTRUE + nimby_conditionMarketRateQuarterMile:homeownerTRUE:fed_housing_binTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Quarter.Mile[5] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[5], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[5], "^{*}"), df$Market.Rate.Quarter.Mile[5]))
  df$Market.Rate.Quarter.Mile[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Market rate, quarter mile condition -- Anti-guarantee
  # owners
  comp = summary(glht(reg, "nimby_conditionMarketRateQuarterMile + nimby_conditionMarketRateQuarterMile:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[7] = round(comp$test$coefficients, 3)
  df$Market.Rate.Quarter.Mile[7] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[7], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[7], "^{*}"), df$Market.Rate.Quarter.Mile[7]))
  df$Market.Rate.Quarter.Mile[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "nimby_conditionMarketRateQuarterMile = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Quarter.Mile[9] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[9], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[9], "^{*}"), df$Market.Rate.Quarter.Mile[9]))
  df$Market.Rate.Quarter.Mile[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * nimby_conditionMarketRateQuarterMile:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Quarter.Mile[11] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[11], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[11], "^{*}"), df$Market.Rate.Quarter.Mile[11]))
  df$Market.Rate.Quarter.Mile[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  # Market rate, two miles condition -- pro-guarantee
  # owners
  comp = summary(glht(reg, "nimby_conditionMarketRateTwoMiles + nimby_conditionMarketRateTwoMiles:homeownerTRUE + nimby_conditionMarketRateTwoMiles:fed_housing_binTRUE + nimby_conditionMarketRateTwoMiles:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Two.Miles[1] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[1], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[1], "^{*}"), df$Market.Rate.Two.Miles[1]))
  df$Market.Rate.Two.Miles[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "nimby_conditionMarketRateTwoMiles + nimby_conditionMarketRateTwoMiles:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Two.Miles[3] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[3], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[3], "^{*}"), df$Market.Rate.Two.Miles[3]))
  df$Market.Rate.Two.Miles[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * (nimby_conditionMarketRateTwoMiles:homeownerTRUE + nimby_conditionMarketRateTwoMiles:homeownerTRUE:fed_housing_binTRUE) = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Two.Miles[5] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[5], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[5], "^{*}"), df$Market.Rate.Two.Miles[5]))
  df$Market.Rate.Two.Miles[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # Market rate, two miles condition -- Anti-guarantee
  # owners
  comp = summary(glht(reg, "nimby_conditionMarketRateTwoMiles + nimby_conditionMarketRateTwoMiles:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[7] = round(comp$test$coefficients, 3)
  df$Market.Rate.Two.Miles[7] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[7], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[7], "^{*}"), df$Market.Rate.Two.Miles[7]))
  df$Market.Rate.Two.Miles[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "nimby_conditionMarketRateTwoMiles = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Two.Miles[9] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[9], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[9], "^{*}"), df$Market.Rate.Two.Miles[9]))
  df$Market.Rate.Two.Miles[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "-1 * nimby_conditionMarketRateTwoMiles:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Two.Miles[11] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[11], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[11], "^{*}"), df$Market.Rate.Two.Miles[11]))
  df$Market.Rate.Two.Miles[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  # format table as latex
  tex = df
  tex[is.na(tex)] = ""
  tex = apply(tex, 1, function(x) paste(x, collapse = "   &   "))
  tex = paste0(tex, "  \\\\")
  tex[12] = paste0(tex[12], " \\bottomrule")
  tex = c("\\multicolumn{1}{c}{Ideology}  &  \\multicolumn{1}{c}{Homeownership}  &  \\multicolumn{1}{c}{No Info}  &  \\multicolumn{1}{c}{Low Inc., No Dist.}  &  \\multicolumn{1}{c}{Low Inc., 1/4 Mile}  &  \\multicolumn{1}{c}{Low Inc., 2 Miles}   &  \\multicolumn{1}{c}{Market Rate, 1/4 Mile}   &  \\multicolumn{1}{c}{Market Rate, 2 Miles} \\\\ \\midrule", tex)
  
  # put it into a tabular environment
  top = paste0("\\begin{tabular}{rrD{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}} \n \\multicolumn{8}{c}{", outcomelab, "} \\\\ \\toprule" )
  second = "  &  &  \\multicolumn{1}{c}{{Outcome Mean}}  &  \\multicolumn{5}{c}{{Average Treatment Effect}} \\\\ \\cmidrule{4-8} "
  bottom = "\\end{tabular}"
  
  tex = c(top, second, tex, bottom)
  tex = paste(tex, collapse = "\n")
  
  # print output to file
  cat(tex, file = savetex)
  
  # return
  out = list(df, tex)
  return(out)
}




# table for experiment 2, v2 --------------------------------------------------

# Basically the same as exp1table()

exp2table_2 = function(data, outcome, outcomelab, savetex = stdout()) { 
  stopifnot(outcome %in% names(data))
  require(estimatr);require(multcomp)
  data$outcome = data[[outcome]]
  
  # get rid of spaces in nimby_condition
  data$nimby_condition = gsub(" ", "", data$nimby_condition)
  data$nimby_condition = gsub(",", "", data$nimby_condition)
  data$nimby_condition = gsub("-", "", data$nimby_condition)
  
  
  # run fully saturated regression of outcome on interaction of treatment,
  # homeownership, and ideology
  reg = lm_robust(outcome ~ nimby_condition * homeowner * fed_housing_bin, data, se_type = "HC2")
  
  
  
  
  # create data frame that will be used to store results
  df = data.frame(Homeownership = c("Homeowners:", rep("", 5), "Renters:", rep("", 5)),
                  Ideology = rep(c("Pro-Guarantee", "", "Anti-Guarantee", "", "Lib.-Con. Difference", ""), 2),
                  No.Info = NA, Low.Income = NA, 
                  Low.Income.Quarter.Mile = NA,
                  Low.Income.Two.Miles = NA,
                  Market.Rate.Quarter.Mile = NA,
                  Market.Rate.Two.Miles = NA)
  
  # fill in results with appropriate significance levels
  # no info condition -- homeowners
  # liberals
  df$No.Info[1] = round(coef(reg)["(Intercept)"] + coef(reg)["fed_housing_binTRUE"] + coef(reg)["homeownerTRUE"] + coef(reg)["homeownerTRUE:fed_housing_binTRUE"], 2) %>% format(nsmall=2)
  
  # conservatives
  df$No.Info[3] = round(coef(reg)["(Intercept)"] + coef(reg)["homeownerTRUE"], 2) %>% format(nsmall=2)
  
  # difference
  comp = summary(glht(reg, "(fed_housing_binTRUE + homeownerTRUE:fed_housing_binTRUE) = 0"))
  pv = comp$test$pvalues
  se = comp$test$sigma
  df$No.Info[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$No.Info[5] = ifelse(pv < .01, paste0(df$No.Info[5], "^{**}"), ifelse(pv < .05, paste0(df$No.Info[5], "^{*}"), df$No.Info[5]))
  df$No.Info[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  # No-info condition -- renters
  # liberals
  df$No.Info[7] = round(coef(reg)["(Intercept)"] + coef(reg)["fed_housing_binTRUE"], 2)
  
  # conservatives
  df$No.Info[9] = round(coef(reg)["(Intercept)"], 2)
  
  # difference
  comp = summary(glht(reg, "fed_housing_binTRUE = 0"))
  pv = comp$test$pvalues
  se = comp$test$sigma
  df$No.Info[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$No.Info[11] = ifelse(pv < .01, paste0(df$No.Info[11], "^{**}"), ifelse(pv < .05, paste0(df$No.Info[5], "^{*}"), df$No.Info[11]))
  df$No.Info[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  # Low income, no distance condition -- Homeowners
  # liberals
  comp = summary(glht(reg, "nimby_conditionLowIncomeDistanceNotSpecified + nimby_conditionLowIncomeDistanceNotSpecified:homeownerTRUE + nimby_conditionLowIncomeDistanceNotSpecified:fed_housing_binTRUE + nimby_conditionLowIncomeDistanceNotSpecified:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[1] = ifelse(pv < .01, paste0(df$Low.Income[1], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[1], "^{*}"), df$Low.Income[1]))
  df$Low.Income[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservatives
  comp = summary(glht(reg, "nimby_conditionLowIncomeDistanceNotSpecified + nimby_conditionLowIncomeDistanceNotSpecified:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[3] = ifelse(pv < .01, paste0(df$Low.Income[3], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[3], "^{*}"), df$Low.Income[3]))
  df$Low.Income[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "nimby_conditionLowIncomeDistanceNotSpecified:fed_housing_binTRUE + nimby_conditionLowIncomeDistanceNotSpecified:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[5] = ifelse(pv < .01, paste0(df$Low.Income[5], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[5], "^{*}"), df$Low.Income[5]))
  df$Low.Income[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  # Low income, no distance info condition -- Renters
  # liberals
  comp = summary(glht(reg, "nimby_conditionLowIncomeDistanceNotSpecified + nimby_conditionLowIncomeDistanceNotSpecified:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[7] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[7] = ifelse(pv < .01, paste0(df$Low.Income[7], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[7], "^{*}"), df$Low.Income[7]))
  df$Low.Income[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservatives
  comp = summary(glht(reg, "nimby_conditionLowIncomeDistanceNotSpecified = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[9] = ifelse(pv < .01, paste0(df$Low.Income[9], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[9], "^{*}"), df$Low.Income[9]))
  df$Low.Income[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "nimby_conditionLowIncomeDistanceNotSpecified:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income[11] = ifelse(pv < .01, paste0(df$Low.Income[11], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income[11], "^{*}"), df$Low.Income[11]))
  df$Low.Income[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  
  
  # Low income, quarter mile condition -- homeowners
  # liberals
  comp = summary(glht(reg, "nimby_conditionLowIncomeQuarterMile + nimby_conditionLowIncomeQuarterMile:homeownerTRUE + nimby_conditionLowIncomeQuarterMile:fed_housing_binTRUE + nimby_conditionLowIncomeQuarterMile:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Quarter.Mile[1] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[1], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[1], "^{*}"), df$Low.Income.Quarter.Mile[1]))
  df$Low.Income.Quarter.Mile[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservatives
  comp = summary(glht(reg, "nimby_conditionLowIncomeQuarterMile + nimby_conditionLowIncomeQuarterMile:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Quarter.Mile[3] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[3], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[3], "^{*}"), df$Low.Income.Quarter.Mile[3]))
  df$Low.Income.Quarter.Mile[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "nimby_conditionLowIncomeQuarterMile:fed_housing_binTRUE + nimby_conditionLowIncomeQuarterMile:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Quarter.Mile[5] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[5], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[5], "^{*}"), df$Low.Income.Quarter.Mile[5]))
  df$Low.Income.Quarter.Mile[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  # Low income, quarter mile condition -- Renters
  # liberals
  comp = summary(glht(reg, "nimby_conditionLowIncomeQuarterMile + nimby_conditionLowIncomeQuarterMile:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[7] = round(comp$test$coefficients, 3)
  df$Low.Income.Quarter.Mile[7] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[7], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[7], "^{*}"), df$Low.Income.Quarter.Mile[7]))
  df$Low.Income.Quarter.Mile[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservatives
  comp = summary(glht(reg, "nimby_conditionLowIncomeQuarterMile = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Quarter.Mile[9] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[9], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[9], "^{*}"), df$Low.Income.Quarter.Mile[9]))
  df$Low.Income.Quarter.Mile[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "nimby_conditionLowIncomeQuarterMile:fed_housing_binTRUE= 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Quarter.Mile[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Quarter.Mile[11] = ifelse(pv < .01, paste0(df$Low.Income.Quarter.Mile[11], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Quarter.Mile[11], "^{*}"), df$Low.Income.Quarter.Mile[11]))
  df$Low.Income.Quarter.Mile[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  # Low income, two miles condition -- Homeowners
  # liberal
  comp = summary(glht(reg, "nimby_conditionLowIncomeTwoMiles + nimby_conditionLowIncomeTwoMiles:homeownerTRUE + nimby_conditionLowIncomeTwoMiles:fed_housing_binTRUE + nimby_conditionLowIncomeTwoMiles:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Two.Miles[1] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[1], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[1], "^{*}"), df$Low.Income.Two.Miles[1]))
  df$Low.Income.Two.Miles[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # renters
  comp = summary(glht(reg, "nimby_conditionLowIncomeTwoMiles + nimby_conditionLowIncomeTwoMiles:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Two.Miles[3] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[3], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[3], "^{*}"), df$Low.Income.Two.Miles[3]))
  df$Low.Income.Two.Miles[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "nimby_conditionLowIncomeTwoMiles:fed_housing_binTRUE + nimby_conditionLowIncomeTwoMiles:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Two.Miles[5] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[5], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[5], "^{*}"), df$Low.Income.Two.Miles[5]))
  df$Low.Income.Two.Miles[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  # Low income, two miles condition -- Renters
  # liberals
  comp = summary(glht(reg, "nimby_conditionLowIncomeTwoMiles + nimby_conditionLowIncomeTwoMiles:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[7] = round(comp$test$coefficients, 3)
  df$Low.Income.Two.Miles[7] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[7], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[7], "^{*}"), df$Low.Income.Two.Miles[7]))
  df$Low.Income.Two.Miles[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservatives
  comp = summary(glht(reg, "nimby_conditionLowIncomeTwoMiles = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Two.Miles[9] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[9], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[9], "^{*}"), df$Low.Income.Two.Miles[9]))
  df$Low.Income.Two.Miles[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "nimby_conditionLowIncomeTwoMiles:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Low.Income.Two.Miles[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Low.Income.Two.Miles[11] = ifelse(pv < .01, paste0(df$Low.Income.Two.Miles[11], "^{**}"), ifelse(pv < .05, paste0(df$Low.Income.Two.Miles[11], "^{*}"), df$Low.Income.Two.Miles[11]))
  df$Low.Income.Two.Miles[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  
  # Market rate, quarter mile condition -- Homeowners
  # liberals
  comp = summary(glht(reg, "nimby_conditionMarketRateQuarterMile + nimby_conditionMarketRateQuarterMile:homeownerTRUE + nimby_conditionMarketRateQuarterMile:fed_housing_binTRUE + nimby_conditionMarketRateQuarterMile:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Quarter.Mile[1] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[1], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[1], "^{*}"), df$Market.Rate.Quarter.Mile[1]))
  df$Market.Rate.Quarter.Mile[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservatives
  comp = summary(glht(reg, "nimby_conditionMarketRateQuarterMile + nimby_conditionMarketRateQuarterMile:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Quarter.Mile[3] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[3], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[3], "^{*}"), df$Market.Rate.Quarter.Mile[3]))
  df$Market.Rate.Quarter.Mile[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "nimby_conditionMarketRateQuarterMile:fed_housing_binTRUE + nimby_conditionMarketRateQuarterMile:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Quarter.Mile[5] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[5], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[5], "^{*}"), df$Market.Rate.Quarter.Mile[5]))
  df$Market.Rate.Quarter.Mile[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  
  
  # Market rate, quarter mile condition -- Renters
  # liberals
  comp = summary(glht(reg, "nimby_conditionMarketRateQuarterMile + nimby_conditionMarketRateQuarterMile:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[7] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Quarter.Mile[7] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[7], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[7], "^{*}"), df$Market.Rate.Quarter.Mile[7]))
  df$Market.Rate.Quarter.Mile[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservatives
  comp = summary(glht(reg, "nimby_conditionMarketRateQuarterMile = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Quarter.Mile[9] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[9], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[9], "^{*}"), df$Market.Rate.Quarter.Mile[9]))
  df$Market.Rate.Quarter.Mile[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "nimby_conditionMarketRateQuarterMile:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Quarter.Mile[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Quarter.Mile[11] = ifelse(pv < .01, paste0(df$Market.Rate.Quarter.Mile[11], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Quarter.Mile[11], "^{*}"), df$Market.Rate.Quarter.Mile[11]))
  df$Market.Rate.Quarter.Mile[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  # Market rate, two miles condition -- Homeownwers
  # liberals
  comp = summary(glht(reg, "nimby_conditionMarketRateTwoMiles + nimby_conditionMarketRateTwoMiles:homeownerTRUE + nimby_conditionMarketRateTwoMiles:fed_housing_binTRUE + nimby_conditionMarketRateTwoMiles:homeownerTRUE:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[1] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Two.Miles[1] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[1], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[1], "^{*}"), df$Market.Rate.Two.Miles[1]))
  df$Market.Rate.Two.Miles[2] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservatives
  comp = summary(glht(reg, "nimby_conditionMarketRateTwoMiles + nimby_conditionMarketRateTwoMiles:homeownerTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[3] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Two.Miles[3] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[3], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[3], "^{*}"), df$Market.Rate.Two.Miles[3]))
  df$Market.Rate.Two.Miles[4] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "nimby_conditionMarketRateTwoMiles:fed_housing_binTRUE + nimby_conditionMarketRateTwoMiles:homeownerTRUE:fed_housing_binTRUE  = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[5] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Two.Miles[5] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[5], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[5], "^{*}"), df$Market.Rate.Two.Miles[5]))
  df$Market.Rate.Two.Miles[6] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  
  
  # Market rate, two miles condition -- Renters
  # liberals
  comp = summary(glht(reg, "nimby_conditionMarketRateTwoMiles + nimby_conditionMarketRateTwoMiles:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[7] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Two.Miles[7] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[7], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[7], "^{*}"), df$Market.Rate.Two.Miles[7]))
  df$Market.Rate.Two.Miles[8] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # conservatives
  comp = summary(glht(reg, "nimby_conditionMarketRateTwoMiles = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[9] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Two.Miles[9] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[9], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[9], "^{*}"), df$Market.Rate.Two.Miles[9]))
  df$Market.Rate.Two.Miles[10] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  # difference
  comp = summary(glht(reg, "nimby_conditionMarketRateTwoMiles:fed_housing_binTRUE = 0"))
  se = comp$test$sigma
  pv = comp$test$pvalues
  df$Market.Rate.Two.Miles[11] = format(round(comp$test$coefficients, 2), nsmall=2)
  df$Market.Rate.Two.Miles[11] = ifelse(pv < .01, paste0(df$Market.Rate.Two.Miles[11], "^{**}"), ifelse(pv < .05, paste0(df$Market.Rate.Two.Miles[11], "^{*}"), df$Market.Rate.Two.Miles[11]))
  df$Market.Rate.Two.Miles[12] = paste0("(", format(round(se, 2), nsmall=2), ")")
  
  
  # format table as latex
  tex = df
  tex[is.na(tex)] = ""
  tex = apply(tex, 1, function(x) paste(x, collapse = "   &   "))
  tex = paste0(tex, "  \\\\")
  tex[12] = paste0(tex[12], " \\bottomrule")
  tex = c("\\multicolumn{1}{c}{Homeownership}  &  \\multicolumn{1}{c}{Ideology}  &  \\multicolumn{1}{p{1.95cm}}{No Info}  &  \\multicolumn{1}{p{1.95cm}}{Low Inc., No Dist.}  &  \\multicolumn{1}{p{1.95cm}}{Low Inc., 1/4 Mile}  &  \\multicolumn{1}{p{1.95cm}}{Low Inc., 2 Miles}   &  \\multicolumn{1}{p{1.95cm}}{Mkt. Rate, 1/4 Mile}   &  \\multicolumn{1}{p{1.95cm}}{Mkt. Rate, 2 Miles} \\\\ \\midrule", tex)
  
  # put it into a tabular environment
  top = paste0("\\begin{tabular}{rrD{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}D{.}{.}{-3}} \n \\multicolumn{8}{c}{", outcomelab, "} \\\\ \\toprule" )
  second = "  &  &  \\multicolumn{1}{c}{{Outcome Mean}}  &  \\multicolumn{5}{c}{{Average Treatment Effect}} \\\\ \\cmidrule{4-8} "
  bottom = "\\end{tabular}"
  
  tex = c(top, second, tex, bottom)
  tex = paste(tex, collapse = "\n")
  
  # print output to file
  cat(tex, file = savetex)
  
  # return
  out = list(df, tex)
  return(out)
}

