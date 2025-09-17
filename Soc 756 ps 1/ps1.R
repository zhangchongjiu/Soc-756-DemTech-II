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
lt <- data %>%
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
print(lt)
write_csv(lt, "ps1.csv")




#==================================================================
#============================================================= Q1 b

library(ggplot2)

# Color style
colors <- c(
  lx_color = "#2C3E50", 
  ndx_color = "#E74C3C",
  nmx_color = "#27AE60" 
  )

# Plot function
ltplot <- function(data, x_var, y_var, color_name, title, y_label, y_scale = "comma") {
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_line(color = colors[color_name], size = 1.3, alpha = 0.9) +
    geom_point(color = colors[color_name], size = 2.5, alpha = 0.8) +
    labs(title = title,
         subtitle = "French Males, 1985",
         x = "Age",
         y = y_label) +
    ltplot_theme()
  
  # y-axis scaling
  if (y_scale == "comma") {
    p <- p + scale_y_continuous(labels = scales::comma)
  } else if (y_scale == "scientific") {
    p <- p + scale_y_continuous(labels = scales::scientific)
  }
  
  return(p)
}

# Theme function
ltplot_theme <- function() {
  theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#7F8C8D"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "#ECF0F1", size = 0.5),
      panel.grid.minor = element_line(color = "#F8F9FA", size = 0.3),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Create plot
p1 <- ltplot(lt, "x", "lx", "lx_color", "Survivors to Age x (lx)",
                 "Number of Survivors", "comma")

p2 <- ltplot(lt, "x", "ndx", "ndx_color", "Deaths (ndx)", 
                 "Number of Deaths", "comma")

p3 <- ltplot(lt, "x", "nmx", "nmx_color","Age-Specific Mortality Rate (nmx)",
                 "Mortality Rate", "scientific")

ggsave("lx.png", p1, width = 6, height = 5, dpi = 300, bg = "white")
ggsave("ndx.png", p2, width = 6, height = 5, dpi = 300, bg = "white")
ggsave("nmx.png", p3, width = 6, height = 5, dpi = 300, bg = "white")




