library(fastverse)
fastverse_update()
fastverse_packages()

# alt function ---------
gini <- function(welfare, weight) {

  # Compute weighted welfare
  o <- radixorder(welfare)
  welfare <- welfare[o]
  weight  <- weight[o]
  weighted_welfare <- welfare * weight
  weighted_welfare_lag <- flag(weighted_welfare, fill = 0)

  # Compute area under the curve using
  # Area of trapezoid = Base * Average height
  v <- (fcumsum(weighted_welfare_lag) + (weighted_welfare / 2)) * weight
  auc <- fsum(v) # Area Under the Curve

  # Compute Area Under the Lorenz Curve
  # Normalize auc so it is always between 0 and 0.5
  auc <- (auc / fsum(weight)) / fsum(weighted_welfare)

  # Compute Gini
  gini <- 1 - (2 * auc)

  return(gini)
}


# load data ----
## Auxiliary data ---------

pipload::pip_load_all_aux(replace = TRUE)


## microdata --------
years <- c(2019:2021)
# ind <- lapply(years,
#               \(x) pipload::pip_load_data("IND", x))
#

df <- lapply(years,
             \(x) pipload::pip_load_cache("IND", x,
                                          version = "20230626_2017_01_02_TEST")) |>
  rbindlist()


# basic calculations --------

## population factor ----

df |>
  fgroup_by(survey_year, reporting_level) |>
  fselect(pop_fact, cpi, ppp) |>
  fmin()

## number of obs ----

dim(df)

df[, .N, by = survey_year]

## welfare mean --------

### Urban/rural --------
df |>
  fgroup_by(survey_year, reporting_level, imputation_id) |>
  fselect(welfare_ppp, weight) |>
  # get mean by imputation
  fmean(w = weight) |>
  fgroup_by(survey_year, reporting_level) |>
  fselect(welfare_ppp) |>
  # average of mean in each imputation
  fmean()


### National level -------

df |>
  fgroup_by(survey_year, imputation_id) |>
  fselect(welfare_ppp, weight) |>
  # get mean by imputation
  fmean(w = weight) |>
  fgroup_by(survey_year) |>
  fselect(welfare_ppp) |>
  # average of mean in each imputation
  fmean()

## Gini -------

### Urban/rural ---------
g <- GRP(df, ~ survey_year + reporting_level + imputation_id)
w <- df$weight
# BY(df$welfare_ppp, g, gini, weight = w)


df |>
  fgroup_by(survey_year, reporting_level, imputation_id) |>
  fselect(gini = welfare_ppp) |>
  # get gini by imputation and reporting level
  BY(gini, weight = w) |>
  fgroup_by(survey_year, reporting_level) |>
  fselect(gini) |>
  fmean()

### National ---------
df |>
  fgroup_by(survey_year, imputation_id) |>
  fselect(gini = welfare_ppp) |>
  # get gini by imputation and reporting level
  BY(gini, weight = w) |>
  fgroup_by(survey_year) |>
  fselect(gini) |>
  fmean()


### Wrong process -----
df |>
  fgroup_by(survey_year) |>
  fselect(gini = welfare_ppp) |>
  # get gini by imputation and reporting level
  BY(gini, weight = w)













