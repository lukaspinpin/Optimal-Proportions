library(reshape2)
library(ggplot2)
library(cowplot)
library(mgcv)  # Ensure mgcv is loaded
library(latex2exp)

########## Bernoulli ####################
# CR 
result_CR <- read.csv(file = '~/Desktop/BSU/ErrorRatesPaper/ER_50_25_bern_All_t1.csv')[,-1]

# Neyman for Wald
result_W <- read.csv(file = '~/Desktop/BSU/ErrorRatesPaper/ERADE_Neyman_50_2_bern_All_t1.csv')[,-1]

# Neyman for Fleiss
result_F <- read.csv(file = '~/Desktop/BSU/ErrorRatesPaper/ERADE_Neyman_F_50_2_bern_All_t1.csv')[,-1]

#minF
result_minF <- read.csv(file = '~/Desktop/BSU/ErrorRatesPaper/ERADE_R_minF_50_2_bern_All_t1.csv')[,-1]

#minF
result_minF_F <- read.csv(file = '~/Desktop/BSU/ErrorRatesPaper/ERADE_R_minF_F_50_2_bern_All_t1.csv')[,-1]

# If tables with type-I error are loaded
typeI <- result_CR[result_CR$p11==result_CR$p12, c("p11","Z", "Z_A", "Z_F")]
typeI <- cbind(typeI, result_W[result_W$p11 == result_W$p12, c("Z", "Z_F")],
               result_F[result_F$p11 == result_F$p12, c("Z", "Z_F")], 
               result_minF[result_minF$p11 == result_minF$p12, c("Z", "Z_F")],
               result_minF_F[result_minF_F$p11 == result_minF_F$p12, c("Z", "Z_F")])
names(typeI) <- c("p1", "CR_W", "CR_A", "CR_F", "N_W", "N_F", "F_W", "F_F", "R_W", "R_F", "RF_W", "RF_F")


# Part 1 of Figure 1
ggplot(typeI, aes(x = p1)) +
  geom_smooth(aes(y = N_W, color = "N_W"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              method.args = list(family = betar()),  # Use beta regression
              se = FALSE, linetype = "solid", size = 1.2) +
  geom_smooth(aes(y = R_W, color = "R_W"), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs"), 
              method.args = list(family = betar()),  # Use beta regression
              se = FALSE, linetype = "solid", size = 1.2) +
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "black", show.legend = FALSE) +
  labs(
    title = expression("n=50"),
    x = expression(p[1]),
    y = "Type-I Error Rate"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "N_W" = "lightblue",  
      "R_W" = "lightcoral"
    ),
    name = "Method",
    labels = c(
      N_W = expression(rho[N[1]]), 
      R_W = expression(rho[R[1]])
    )
  ) +
  scale_y_continuous(limits = c(0, 0.9), expand = c(0, 0)) +  # Ensure full probability range
  theme(
    legend.title = element_text(),
    plot.title = element_text(hjust = 0.5)
  )


#Part 2 of Figure 1
ggplot(typeI, aes(x = p1)) +
  geom_smooth(aes(y = CR_W, color = "CR_W"), method = "gam", se = FALSE, linetype = "solid", size = 1.2) +
  geom_smooth(aes(y = CR_F, color = "CR_F"), method = "gam", se = FALSE, linetype = "solid", size = 1.2) +
  geom_smooth(aes(y = F_F, color = "F_F"), method = "gam", se = FALSE, linetype = "solid", size = 1.2) +
  geom_smooth(aes(y = RF_F, color = "RF_F"), method = "gam", se = FALSE, linetype = "solid", size = 1.2) +
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "black", show.legend = FALSE) +
  labs(
    title = expression("n=50"),
    x = expression(p[1]),
    y = "Type-I Error Rate"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "CR_W" = "lightgreen",
      "CR_F" = "darkgreen",
      "F_F" = "blue",
      "RF_F" = "red"
    ),
    name = "Method",
    labels = c(
      CR_W = expression(rho[CR[1]]), 
      CR_F = expression(rho[CR[0]]), 
      F_F = expression(rho[N[0]^n]), 
      RF_F = expression(rho[R[0]^n])
    )
  ) +
  scale_y_continuous(limits = c(0, 0.07), expand = c(0, 0)) + 
  theme(
    legend.title = element_text(),
    plot.title = element_text(hjust = 0.5)
  )