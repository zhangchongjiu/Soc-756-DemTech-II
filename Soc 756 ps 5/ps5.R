# SOC 756 Problem Set 5
# Author: Chong-Jiu Zhang
# Last modified: 2025-10-15

library(ggplot2)
library(tidyr)

setwd("D:/Wisconsin/25-26-F Soc 756 Dem Tech II/Soc 756 ps 5")


# ============================================================================
# 1
# ============================================================================

# Data
age_groups <- c(10, 15, 20, 25, 30, 35, 40, 45)
females <- c(9315000, 9302000, 8591000, 9446000, 10447000, 11373000, 10800000, 9409000)
all_births <- c(10121, 483220, 942048, 1069436, 886798, 409710, 76084, 3333)
female_births <- c(4899, 236207, 460534, 523179, 432638, 200533, 37288, 1617)
nLx <- c(495678, 494913, 493741, 492428, 490757, 488395, 484977, 479969)
lx <- c(99174, 99083, 98868, 98624, 98336, 97945, 97381, 96561)

males_total <- 130783000
females_total <- 137001000
total_population <- males_total + females_total
l0 <- 100000

# A. CBR
CBR <- (sum(all_births) / total_population) * 1000
print(CBR)

# B. GFR
women_reproductive <- sum(females[2:7]) 
GFR <- (sum(all_births) / women_reproductive) * 1000
print(GFR)

# C. ASFR
ASFR <- (all_births / females) * 1000

fertility_df <- data.frame(age = age_groups, ASFR = ASFR)

p1 <- ggplot(fertility_df, aes(x = age, y = ASFR)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Age-Specific Fertility Rates, United States 1997",
       x = "Age Group",
       y = "ASFR (births per 1,000 women)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("ASFR.png", plot = p1, width = 8, height = 6, dpi = 300)

# D. TFR
TFR <- sum(ASFR) * 5 / 1000
print(TFR)

# E. GFR
GRR <- TFR / (1 + 1.05)
print(GRR)

# F. NRR
NRR <- sum((female_births / females) * (nLx / l0))
print(NRR)

# G. NRR Approximation
Am <- sum((female_births/females) * (age_groups + 2.5)) / sum((female_births/females))

# l(Am)
idx <- findInterval(Am, age_groups)
if (idx > 0 && idx < length(age_groups)) {
  x1 <- age_groups[idx]
  x2 <- age_groups[idx + 1]
  l1 <- lx[idx]
  l2 <- lx[idx + 1]
  l_Am <- l1 + (l2 - l1) * (Am - x1) / (x2 - x1)
} else {
  l_Am <- lx[idx]
}

p_Am <- l_Am / l0
NRR2 <- p_Am * GRR

print(NRR2)




# ============================================================================
# 2
# ============================================================================

fecund_period <- 250
fecundability <- 0.2
anovulatory_postpartum <- 13
duration_abortion <- 2
anovulatory_postabortion <- 3
effectiveness <- seq(0.45, 0.95, by = 0.05)

# TFR function
calculate_TFR <- function(e, abort_ratio = 0) {
  fecund_adj <- fecundability * (1 - e)
  if (fecund_adj <= 0) return(0)
  
  waiting_time <- 1 / fecund_adj
  duration_livebirth <- 9
  interval_livebirth <- waiting_time + duration_livebirth + anovulatory_postpartum
  
  if (abort_ratio == 0) {
    return(fecund_period / interval_livebirth)
  } else {
    interval_abortion <- waiting_time + duration_abortion + anovulatory_postabortion
    avg_interval <- (interval_livebirth + abort_ratio * interval_abortion) / (1 + abort_ratio)
    return(fecund_period / avg_interval)
  }
}

# Calculate TFR
TFR_no_abortion <- sapply(effectiveness, calculate_TFR, abort_ratio = 0)
TFR_with_abortion <- sapply(effectiveness, calculate_TFR, abort_ratio = 1)
percent_decrease <- ((TFR_no_abortion - TFR_with_abortion) / TFR_no_abortion) * 100

# Data frame
tfr_df <- data.frame(
  effectiveness = effectiveness,
  no_abortion = TFR_no_abortion,
  with_abortion = TFR_with_abortion
)

tfr_long <- pivot_longer(tfr_df, 
                         cols = c(no_abortion, with_abortion),
                         names_to = "scenario",
                         values_to = "TFR")

# Graph 1: TFR by Contraceptive Effectiveness
p2 <- ggplot(tfr_long, aes(x = effectiveness, y = TFR, color = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("no_abortion" = "blue", "with_abortion" = "red"),
                     labels = c("No Abortion", "With Abortion (1:1 ratio)")) +
  labs(title = "TFR by Contraceptive Effectiveness",
       x = "Contraceptive Effectiveness",
       y = "Total Fertility Rate",
       color = "Scenario") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")

print(p2)
ggsave("TFR_contraceptive.png", plot = p2, width = 8, height = 6, dpi = 300)

# Graph 2: Percent Decrease in TFR
decrease_df <- data.frame(effectiveness = effectiveness, 
                          percent_decrease = percent_decrease)

p3 <- ggplot(decrease_df, aes(x = effectiveness, y = percent_decrease)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 3) +
  labs(title = "Percent Decrease in TFR Due to Abortion",
       x = "Contraceptive Effectiveness",
       y = "Percent Decrease in TFR (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(p3)
ggsave("TFR_decrease.png", plot = p3, width = 8, height = 6, dpi = 300)
