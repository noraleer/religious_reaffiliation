
library(tidyverse)
library(survey)
library(gt)

weights <- read.table("customweight_nlsy97_6989725f19c40e6c583.dat") %>%
  rename(ID = V1, Weight = V2) %>%
  mutate(Weight = Weight / 100)

dat <- read_csv("FinalCleanedData_long1.csv")

final <-
  dat %>%
  left_join(weights, by = "ID") %>%
  mutate(
    ID = as.factor(ID),
    Time = as.factor(Time),
    GrossHHIncome = as.factor(GrossHHIncome),
    IncomeRatio = as.factor(IncomeRatio),
    Time_year = as.factor(Time_year),
    race = as.factor(race),
    birth_month = as.factor(birth_month)
  ) %>%
  na.omit()

nlsy_design <- svydesign(
  id = ~ID,
  weights = ~Weight,
  data = final
)

vars <- setdiff(names(final), c("ID", "Weight", "Time"))

f_all <- as.formula(
  paste("~", paste(vars, collapse = " + "))
)


res <- svyby(
  f_all,
  ~Time,
  nlsy_design,
  svymean,
  na.rm = TRUE,
  vartype = "se",
  keep.names = TRUE
)

tab_final <-
  res %>%
  as.data.frame() %>%
  pivot_longer(
    cols = -Time,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    stat = ifelse(grepl("^se\\.", variable), "se", "mean"),
    variable = sub("^se\\.", "", variable)
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    percent = mean * 100,
    percent_se = se * 100,
    estimate = sprintf("%.1f (%.1f)", percent, percent_se)
  ) %>%
  select(Time, variable, estimate)

tab_final %>%
  gt()



m_overall <- svymean(
  f_all,
  nlsy_design,
  na.rm = TRUE
)

tab_overall <-
  data.frame(
    variable = names(m_overall),
    mean = coef(m_overall),
    se = SE(m_overall),
    row.names = NULL
  ) %>%
  filter(
    !variable %in% c("ID", "Weight"),
    !grepl("^Time$", variable),
    !grepl("^Time_year", variable)
  ) %>%
  mutate(
    percent = mean * 100,
    percent_se = se * 100,
    estimate = sprintf("%.1f (%.1f)", percent, percent_se)
  ) %>%
  select(variable, estimate)

tab_overall %>%
  gt()