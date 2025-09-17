# SOC 756 ps1
# Author: Chong-Jiu Zhang
# Last modified: 2025-9-16


library(readr)
library(dplyr)


setwd("D:/Wisconsin/2526F - Soc 756 Dem Tech II/Soc 756 ps 1")
data <- read_csv("ps1_data_F2023.csv")
print(data)


data$nNx <- as.numeric(gsub(",", "", data$nNx))
data$nDx <- as.numeric(gsub(",", "", data$nDx))
# print(data)




#==================================================================
#============================================================= Q1 a

# n -- age interval
data$n <- c(1, 4, rep(5, nrow(data)-3), Inf) 

#==========================================
# nmx -- age-specific mortality rate
data$nmx <- data$nDx / data$nNx

#==========================================
# nqx -- probability of dying
# nqx = n * nmx / (1 + (n - nax) * nmx)
data$nqx <- (data$n * data$nmx) / (1 + (data$n - data$nax) * data$nmx)
data$nqx[nrow(data)] <- 1.0

#==========================================
# npx
data$npx <- 1 - data$nqx

#==========================================
# lx -- survivors to age x
data$lx <- numeric(nrow(data))
data$lx[1] <- 100
for(i in 2:nrow(data)) {
  data$lx[i] <- data$lx[i-1] * data$npx[i-1]
}

#==========================================
# ndx -- # deaths
data$ndx <- data$lx * data$nqx

#==========================================
# nLx -- person-years lived in interval
# nLx = n * lx+n + nax * ndx
data$nLx <- numeric(nrow(data))
for(i in 1:(nrow(data)-1)) {
  data$nLx[i] <- data$n[i] * data$lx[i+1] + data$nax[i] * data$ndx[i]
}
data$nLx[nrow(data)] <- data$lx[nrow(data)] / data$nmx[nrow(data)]

# nLx2 = (n - nax) * lx+n + nax * lx
data$nLx2 <- numeric(nrow(data))
for(i in 1:(nrow(data)-1)) {
  data$nLx2[i] <- (data$n[i] - data$nax[i]) * data$lx[i+1] + data$nax[i] * data$lx[i]
}
data$nLx2[nrow(data)] <- data$lx[nrow(data)] / data$nmx[nrow(data)]

# Check
cat("\n=== CHECKING nLx CALCULATIONS ===\n")
differences <- abs(data$nLx - data$nLx2)
max_diff <- max(differences)
cat("Max difference:", max_diff, "\n")
tolerance <- 1e-10
are_equal <- all(differences < tolerance)
cat("Are nLx and nLx2 equal (within tolerance of", tolerance, "):", are_equal, "\n")
if(are_equal) {
  cat("nLx and nLx2 are equal - dropping nLx2 column\n")
  data$nLx2 <- NULL
} else {
  cat("nLx and nLx2 are NOT equal - keeping both columns\n")
  # Show where they differ
  diff_rows <- which(differences >= tolerance)
  if(length(diff_rows) > 0) {
    cat("Rows where they differ:\n")
    for(row in diff_rows) {
      cat(sprintf("Age %d: nLx=%.6f, nLx2=%.6f, diff=%.6f\n", 
                  data$x[row], data$nLx[row], data$nLx2[row], differences[row]))
    }
  }
}

#==========================================
# Tx -- person-years lived above x
data$Tx <- numeric(nrow(data))
data$Tx[nrow(data)] <- data$nLx[nrow(data)]
for(i in (nrow(data)-1):1) {
  data$Tx[i] <- data$Tx[i+1] + data$nLx[i]
}

#==========================================
# ex
data$ex <- data$Tx / data$lx

#==========================================
# Creat lt
life_table <- data %>%
  select(x, n, nNx, nDx, nax, nmx, nqx, npx, lx, ndx, nLx, Tx, ex) %>%
  mutate(
    nmx = round(nmx, 8),
    nqx = round(nqx, 8),
    npx = round(npx, 8),
    lx = round(lx, 8),
    ndx = round(ndx, 8),
    nLx = round(nLx, 8),
    Tx = round(Tx, 8),
    ex = round(ex, 8)
  )
print(life_table)
write_csv(life_table, "ps1.csv")




#==================================================================
#============================================================= Q1 b

library(ggplot2)

colors <- c(
  lx_color = "#2C3E50", 
  ndx_color = "#E74C3C",
  nmx_color = "#27AE60" 
  )

#==========================================
# lx 
p1 <- ggplot(life_table, aes(x = x, y = lx)) +
  geom_line(color = colors["lx_color"], size = 1.3, alpha = 0.9) +
  geom_point(color = colors["lx_color"], size = 2.5, alpha = 0.8) +
  labs(title = "Survivors to Age x (lx)",
       subtitle = "French Males, 1985",
       x = "Age",
       y = "Number of Survivors") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#7F8C8D"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "#ECF0F1", size = 0.5),
    panel.grid.minor = element_line(color = "#F8F9FA", size = 0.3),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(labels = scales::comma)

print(p1)

#==========================================
# ndx
p2 <- ggplot(life_table, aes(x = x, y = ndx)) +
  geom_line(color = colors["ndx_color"], size = 1.3, alpha = 0.9) +
  geom_point(color = colors["ndx_color"], size = 2.5, alpha = 0.8) +
  labs(title = "Deaths (ndx)",
       subtitle = "French Males, 1985",
       x = "Age",
       y = "Number of Deaths") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#7F8C8D"),
    panel.grid.major = element_line(color = "#ECF0F1", size = 0.5),
    panel.grid.minor = element_line(color = "#F8F9FA", size = 0.3),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(labels = scales::comma)

print(p2)

#==========================================
# nmx
p3 <- ggplot(life_table, aes(x = x, y = nmx)) +
  geom_line(color = colors["nmx_color"], size = 1.3, alpha = 0.9) +
  geom_point(color = colors["nmx_color"], size = 2.5, alpha = 0.8) +
  labs(title = "Age-Specific Mortality Rate (nmx)",
       subtitle = "French Males, 1985",
       x = "Age",
       y = "Mortality Rate") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#7F8C8D"),
    panel.grid.major = element_line(color = "#ECF0F1", size = 0.5),
    panel.grid.minor = element_line(color = "#F8F9FA", size = 0.3),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(labels = scales::scientific)

print(p3)

ggsave("lx.png", p1, width = 6, height = 5, dpi = 300, bg = "white")
ggsave("ndx.png", p2, width = 6, height = 5, dpi = 300, bg = "white")
ggsave("nmx.png", p3, width = 6, height = 5, dpi = 300, bg = "white")




