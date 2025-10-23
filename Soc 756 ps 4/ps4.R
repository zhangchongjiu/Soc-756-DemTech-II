# SOC 756 ps4
# Author: Chong-Jiu Zhang
# Last modified: 2025-10-07


library(readxl)
library(xtable)
setwd("D:/Wisconsin/25-26-F Soc 756 Dem Tech II/Soc 756 ps 4")

data <- read_excel("ps4_Fall2025_data.xls", skip = 4, col_names = FALSE)
colnames(data) <- c("age", "pNS", "pSN", "pND", "pSD")

l10 <- 1740000
n_ages <- nrow(data)

lx <- numeric(n_ages)
lxN <- numeric(n_ages)
lxS <- numeric(n_ages)
dNS <- numeric(n_ages)
dND <- numeric(n_ages)
dSN <- numeric(n_ages)
dSD <- numeric(n_ages)
Lx <- numeric(n_ages)
LxN <- numeric(n_ages)
LxS <- numeric(n_ages)

lx[1] <- l10
lxN[1] <- l10 * 0.98
lxS[1] <- l10 * 0.02

for (i in 1:(n_ages - 1)) {
  dNS[i] <- lxN[i] * data$pNS[i]
  dND[i] <- lxN[i] * data$pND[i]
  dSN[i] <- lxS[i] * data$pSN[i]
  dSD[i] <- lxS[i] * data$pSD[i]
  
  lxN[i + 1] <- lxN[i] - dNS[i] - dND[i] + dSN[i]
  lxS[i + 1] <- lxS[i] + dNS[i] - dSN[i] - dSD[i]
  lx[i + 1] <- lxN[i + 1] + lxS[i + 1]
  
  LxN[i] <- (lxN[i] + lxN[i + 1]) / 2
  LxS[i] <- (lxS[i] + lxS[i + 1]) / 2
  Lx[i] <- LxN[i] + LxS[i]
}

life_table <- data.frame(
  age = data$age,
  lx = lx,
  lxN = lxN,
  lxS = lxS,
  dNS = dNS,
  dND = dND,
  dSN = dSN,
  dSD = dSD,
  Lx = Lx,
  LxN = LxN,
  LxS = LxS
)

print(life_table)

# Output
print(xtable(life_table, caption = "Increment-Decrement Life Table"), 
      file = "lt.tex", include.rownames = FALSE)


#==================================================================
#================================================================ 1 

total_ever_smoked <- lxS[1] + sum(dNS)
p_ever_smoked <- total_ever_smoked / l10

print(p_ever_smoked)


#==================================================================
#================================================================ 2 

tx_smoker <- sum(LxS, na.rm = TRUE)
tx_nonsmoker <- sum(LxN, na.rm = TRUE)
tx <- sum(Lx, na.rm = TRUE)

ex_smoker <- tx_smoker / l10
ex_nonsmoker <- tx_nonsmoker / l10
ex <- tx / l10

cat("Expected years as smoker:", round(ex_smoker, 2), "\n")
cat("Expected years as non-smoker:", round(ex_nonsmoker, 2), "\n")
cat("Expected years alive:", round(ex, 2), "\n\n")


#==================================================================
#================================================================ 3 

avg_age_smoker <- sum((data$age + 0.5) * LxS) / sum(LxS)
avg_age_nonsmoker <- sum((data$age + 0.5) * LxN) / sum(LxN)

cat("Average age of smokers:", round(avg_age_smoker, 2), "\n")
cat("Average age of non-smokers:", round(avg_age_nonsmoker, 2), "\n")


#==================================================================
#================================================================ 4 

library(ggplot2)

transition <- data.frame(
  age = rep(data$age, 2),
  probability = c(data$pNS, data$pSN),
  type = c(rep("Starting smoking (N->S)", n_ages),
           rep("Quitting smoking (S->N)", n_ages))
)

p <- ggplot(transition, aes(x = age, y = probability, color = type)) +
  geom_line(linewidth = 1) +
  labs(x = "Age", y = "Probability",
       title = "Age-Specific Transition Probabilities",
       color = "Transition") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)

ggsave("transition_p.png", p, width = 8, height = 6)


#==================================================================
#================================================================ 7

# Function to calculate life table
calc_lifetable <- function(N, S) {
  lx <- numeric(n_ages)
  lxN <- numeric(n_ages)
  lxS <- numeric(n_ages)
  dNS <- numeric(n_ages)
  dND <- numeric(n_ages)
  dSN <- numeric(n_ages)
  dSD <- numeric(n_ages)
  Lx <- numeric(n_ages)
  LxN <- numeric(n_ages)
  LxS <- numeric(n_ages)
  
  lx[1] <- N + S
  lxN[1] <- N
  lxS[1] <- S
  
  for (i in 1:(n_ages - 1)) {
    dNS[i] <- lxN[i] * data$pNS[i]
    dND[i] <- lxN[i] * data$pND[i]
    dSN[i] <- lxS[i] * data$pSN[i]
    dSD[i] <- lxS[i] * data$pSD[i]
    
    lxN[i + 1] <- lxN[i] - dNS[i] - dND[i] + dSN[i]
    lxS[i + 1] <- lxS[i] + dNS[i] - dSN[i] - dSD[i]
    lx[i + 1] <- lxN[i + 1] + lxS[i + 1]
    
    LxN[i] <- (lxN[i] + lxN[i + 1]) / 2
    LxS[i] <- (lxS[i] + lxS[i + 1]) / 2
    Lx[i] <- LxN[i] + LxS[i]
  }
  
  life_table_df <- data.frame(
    age = data$age,
    lx = lx,
    lxN = lxN,
    lxS = lxS,
    dNS = dNS,
    dND = dND,
    dSN = dSN,
    dSD = dSD,
    Lx = Lx,
    LxN = LxN,
    LxS = LxS
  )
  
  return(list(LxN = LxN, LxS = LxS, radix = lx[1], table = life_table_df))
}

# Life table: Non-smokers at age 10
lt_nonsmoker <- calc_lifetable(N = 100, S = 0)
ex_N_nonsmoker <- sum(lt_nonsmoker$LxN) / lt_nonsmoker$radix
ex_S_nonsmoker <- sum(lt_nonsmoker$LxS) / lt_nonsmoker$radix

# Output
print(xtable(lt_nonsmoker$table, caption = "Life Table: Non-smokers at Age 10"), 
      file = "lt_nonsmoker.tex", include.rownames = FALSE)

# Life table: Smokers at age 10
lt_smoker <- calc_lifetable(N = 0, S = 100)
ex_N_smoker <- sum(lt_smoker$LxN) / lt_smoker$radix
ex_S_smoker <- sum(lt_smoker$LxS) / lt_smoker$radix

# Output
print(xtable(lt_smoker$table, caption = "Life Table: Smokers at Age 10"), 
      file = "lt_smoker.tex", include.rownames = FALSE)

# Graph
plot_data <- data.frame(
  status_at_10 = rep(c("Non-smoker at age 10", "Smoker at age 10"), each = 2),
  state = rep(c("Non-smoker", "Smoker"), 2),
  years = c(ex_N_nonsmoker, ex_S_nonsmoker,
            ex_N_smoker, ex_S_smoker)
)

p <- ggplot(plot_data, aes(x = status_at_10, y = years, fill = state)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Smoking Status at Age 10",
       y = "Expected Years",
       title = "Expected Duration in Each State by Smoking Status at Age 10",
       fill = "State") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)

ggsave("ex_N_S.png", p, width = 8, height = 6)