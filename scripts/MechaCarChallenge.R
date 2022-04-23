library(dplyr)

mpg_df <- read.csv(
  file = '../data/MechaCar_mpg.csv',
  check.names = F,
  stringsAsFactors = F
  )

head(mpg_df)

mpg_lm <- lm(
  mpg ~ .,
  data = mpg_df
  )

summary(mpg_lm)

suspension_df <- read.csv(
  file = '../data/Suspension_Coil.csv',
  check.names = F,
  stringsAsFactors = F
  )

head(suspension_df)

suspension_summary <- summarize(
  suspension_df,
  Mean = mean(PSI),
  Median = median(PSI),
  Variance = var(PSI),
  SD = sd(PSI)
  )

print(suspension_summary)

lot_group <- group_by(
  suspension_df,
  Manufacturing_Lot
  )

lot_summary <- summarize(
  lot_group,
  Mean = mean(PSI),
  Median = median(PSI),
  Variance = var(PSI),
  SD = sd(PSI)
  )

print(lot_summary)

lot1_ttest <- t.test(
  suspension_df$PSI,
  mu = 1500,
  subset = Manufacturing_Lot == "Lot1"
  )

print(lot1_ttest)

lot2_ttest <- t.test(
  suspension_df$PSI,
  mu = 1500,
  subset = Manufacturing_Lot == "Lot2"
  )

print(lot2_ttest)

lot3_ttest <- t.test(
  suspension_df$PSI,
  mu = 1500,
  subset = Manufacturing_Lot == "Lot3"
  )

print(lot3_ttest)