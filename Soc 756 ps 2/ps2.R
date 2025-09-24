# SOC 756 ps2
# Author: Chong-Jiu Zhang
# Last modified: 2025-9-24


setwd("D:/Wisconsin/2526F - Soc 756 Dem Tech II/Soc 756 ps 2")



#==================================================================
#=============================================================== Q1 

library(HMDHFDplus)
library(tidyverse)

# Type in username & password
my_username <- userInput()
my_password <- userInput()

us_male <- readHMDweb("USA", "mltper_5x1", my_username, my_password, fixup = TRUE)
us_female <- readHMDweb("USA", "fltper_5x1", my_username, my_password, fixup = TRUE)

us_male <- subset(us_male, Year == 2004) %>% 
  mutate(sex = "male") %>%
  rename(age = Age)

us_female <- subset(us_female, Year == 2004) %>% 
  mutate(sex = "female") %>%
  rename(age = Age)

lifetable <- rbind(us_male, us_female)




#==================================================================
#=============================================================== Q2

library(knitr)

gss_poverty <- read_csv("ps2_Fall2025_data.csv") %>%
  rename(
    age_group = x,
    pov = `proportion_poverty_1-n_x`,
    n = number_sampled_N
  ) %>%
  mutate(
    n = as.numeric(gsub(",", "", n)),
    age = case_when(
      age_group == "110+" ~ 110,
      TRUE ~ as.numeric(str_extract(age_group, "^\\d+"))
    )
  )

data <- lifetable %>% left_join(gss_poverty, by = c("age", "sex"))


# =================================================
# (a) Expected Years in Poverty 
# =================================================

sullivan <- function(data) {
  
  data %>%
    # person-years lived in poverty
    mutate(Lx_pov = Lx * pov) %>%
    group_by(sex) %>%
    arrange(age) %>%
    # Tx
    mutate(Tx_pov = rev(cumsum(rev(Lx_pov)))) %>%
    # ex in poverty
    mutate(pov_ex = Tx_pov / lx) %>%
    ungroup() %>%
    
    mutate(
      # variance of poverty prevalence
      var_p = (pov * (1 - pov)) / n
    ) %>%
    group_by(sex) %>%
    arrange(age) %>%
    mutate(
      # variance of ex
      var_ex = rev(cumsum(rev((Lx^2) * var_p))) / (lx^2),
      se = sqrt(var_ex)
    ) %>%
    ungroup() %>%
    select(age, sex, age_group, pov_ex, var_ex, se)
}

results_pov_ex <- sullivan(data)

table_a <- results_pov_ex %>%
  select(age_group, sex, pov_ex) %>%
  pivot_wider(names_from = sex, values_from = pov_ex) %>%
  rename("Age_Group" = age_group, "Male" = male, "Female" = female) %>%
  mutate(across(c(Male, Female), round, 2))

# Display
cat("Q2(a): Expected years in poverty")
print(kable(table_a, col.names = c("Age Group", "Male", "Female")))

# Save
write_csv(table_a, "q2a.csv")

options(knitr.kable.NA = "")
latex_a <- knitr::kable(
  table_a,
  format = "latex",
  booktabs = TRUE,
  col.names = c("Age Group", "Male", "Female"),
  caption = "Expected years lived in poverty by sex (USA, 2004)"
)
writeLines(as.character(latex_a), "q2a.tex")


# ==================================================
# (b) Sex Differences at birth
# ==================================================

sex_diff <- results_pov_ex %>%
  select(age, age_group, sex, pov_ex, se, var_ex) %>%
  pivot_wider(names_from = sex, values_from = c(pov_ex, se, var_ex)) %>%
  mutate(
    diff = pov_ex_female - pov_ex_male,
    z = diff / (sqrt(se_male^2) + sqrt(se_female^2)),
    p = 2 * pnorm(-abs(z)),
    sig = p < 0.05,
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.1 ~ "+",
      TRUE ~ ""
    )
  )

birth <- sex_diff[sex_diff$age == 0, ]

table_b <- data.frame(
  "Measure" = c("Male Expected Years", "Female Expected Years", 
                "Difference (F-M)", "Z-Score", "P-Value", "Significant"),
  "Value" = c(round(birth$pov_ex_male, 2),
              round(birth$pov_ex_female, 2), 
              round(birth$diff, 2),
              round(birth$z, 2),
              round(birth$p, 4),
              ifelse(birth$sig, "YES", "NO"))
)

cat("Q2(b): Sex differences at birth\n")
print(kable(table_b))

write_csv(table_b, "q2b.csv")

latex_b <- knitr::kable(
  table_b,
  format = "latex",
  booktabs = TRUE,
  col.names = c("", ""),
  caption = "Sex difference at birth: expected years lived in poverty (USA, 2004)"
)
writeLines(as.character(latex_b), "q2b.tex")


# ==================================================
# (c) Sex Differences for all ages
# ==================================================

table_c <- sex_diff %>%
  select(age_group, pov_ex_male, pov_ex_female, diff, z, p, stars) %>%
  rename("Age_Group" = age_group,
         "Male" = pov_ex_male,
         "Female" = pov_ex_female, 
         "Difference" = diff,
         "Z_Score" = z,
         "P_Value" = p,
         "Sig" = stars) %>%
  mutate(across(c(Male, Female, Difference, Z_Score), round, 2),
         P_Value = round(P_Value, 4))

cat("Q2(c): Sex differences at all ages\n")
print(kable(table_c, col.names = c("Age Group", "Male", "Female", "Diff (F-M)", 
                                   "Z-Score", "P-Value", "Sig")))

write_csv(table_c, "q2c.csv")

latex_c <- knitr::kable(
  table_c,
  format = "latex",
  booktabs = TRUE,
  col.names = c("Age Group", "Male", "Female", "Diff (F--M)", "Z-Score", "P-Value", "Sig"),
  caption = "Sex differences in expected years lived in poverty by age (USA, 2004)"
)
writeLines(as.character(latex_c), "q2c.tex")
