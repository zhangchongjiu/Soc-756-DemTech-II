# SOC 756 ps3
# Author: Chong-Jiu Zhang
# Last modified: 2025-9-30


setwd("D:/Wisconsin/2526F - Soc 756 Dem Tech II/Soc 756 ps 3")
library(HMDHFDplus)


#==================================================================
#================================================================ A 

us_lt <- read.table("bltper_1x1.txt", header = TRUE, skip = 2, stringsAsFactors = FALSE)
us_lt_2005 <- us_lt[us_lt$Year == 2005, ]

us_lt_2005$Age <- as.numeric(gsub("\\+", "", us_lt_2005$Age))

# Add p_accident
f_accident <- function(age) {
  return(0.062 - 0.000053 * (age^2))
}
us_lt_2005$p_accident <- sapply(us_lt_2005$Age, f_accident)

# Create lx_surv_acc
us_lt_2005$lx_surv_acc <- numeric(nrow(us_lt_2005))
us_lt_2005$lx_surv_acc[1] <- us_lt_2005$lx[1] 

for(i in 2:nrow(us_lt_2005)) {
  us_lt_2005$lx_surv_acc[i] <- us_lt_2005$lx_surv_acc[i-1] * 
    (1 - us_lt_2005$qx[i-1]) * 
    (1 - us_lt_2005$p_accident[i-1])
}


p_1631 <- us_lt_2005$lx_surv_acc[us_lt_2005$Age == 31] / 
          us_lt_2005$lx_surv_acc[us_lt_2005$Age == 16]

print(p_1631)


#==================================================================
#================================================================ B 

d_acc <- sum(us_lt_2005$lx_surv_acc[us_lt_2005$Age >= 25 & us_lt_2005$Age <= 30] * 
             us_lt_2005$p_accident[us_lt_2005$Age >= 25 & us_lt_2005$Age <= 30])

p_acc_2530 <- d_acc / us_lt_2005$lx_surv_acc[us_lt_2005$Age == 25]

print(p_acc_2530)


#==================================================================
#================================================================ C 

# Cumulative no-accident probability
us_lt_2005$cum_no_acc <- numeric(nrow(us_lt_2005))

for(i in 1:nrow(us_lt_2005)) {
  us_lt_2005$cum_no_acc[i] <- prod(1 - us_lt_2005$p_accident[1:i])
}

death_noacc_1630 <- sum(us_lt_2005$dx[us_lt_2005$Age >= 16 & us_lt_2005$Age <= 30] * 
                        us_lt_2005$cum_no_acc[us_lt_2005$Age >= 16 & us_lt_2005$Age <= 30])

p_death_noacc_1630 <- death_noacc_1630 / us_lt_2005$lx[us_lt_2005$Age == 16]

print(p_death_noacc_1630)

