library(reshape2)
library(ggplot2)
library(cowplot)
library(mgcv)  # Ensure mgcv is loaded
library(latex2exp)
library(pracma)
library(splines)

########## Bernoulli ####################
# CR 
result_CR <- read.csv(file = '~/ER_50_25_bern_All_Table.csv')[,-1]

# Neyman for Wald
result_W <- read.csv(file = '~/ERADE_Neyman_Z1_50_2_bern_All_Table.csv')[,-1]

# Neyman for Fleiss
result_F <- read.csv(file = '~/ERADE_Neyman_Z0_50_2_bern_All_Table.csv')[,-1]

#minF
result_minF <- read.csv(file = '~/ERADE_RSHIR_Z1_50_2_bern_All_Table.csv')[,-1]

#minF
result_minF_F <- read.csv(file = '~/ERADE_RSHIR_Z0_50_2_bern_All_Table.csv')[,-1]

# If tables with type-I error are loaded
typeI <- result_CR[result_CR$p11==result_CR$p12, c("p11","Z", "Z_A", "Z_F")]
typeI <- cbind(typeI, result_W[result_W$p11 == result_W$p12, c("Z", "Z_F")],
               result_F[result_F$p11 == result_F$p12, c("Z", "Z_F")], 
               result_minF[result_minF$p11 == result_minF$p12, c("Z", "Z_F")],
               result_minF_F[result_minF_F$p11 == result_minF_F$p12, c("Z", "Z_F")])
names(typeI) <- c("p1", "CR_W", "CR_A", "CR_F", "N_W", "N_F", "F_W", "F_F", "R_W", "R_F", "RF_W", "RF_F")


# Figure 1.0: Plotting the Raw Data without smoothing
ggplot(typeI, aes(x = p1)) +
  # Add a line for each method being compared
  geom_line(aes(y = CR_W, color = "CR_W"), size = 1.0) +
  geom_line(aes(y = CR_F, color = "CR_F"), size = 1.0) +
  geom_line(aes(y = F_F, color = "F_F"), size = 1.0) +
  geom_line(aes(y = RF_F, color = "RF_F"), size = 1.0) +
  
  # Add a reference line for the nominal alpha level
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "black") +
  
  # Set labels and title using LaTeX expressions
  labs(
    title = TeX("n=50"),
    x = TeX("$p_1$"),
    y = "Type-I Error Rate"
  ) +
  theme_minimal() +
  
  # Manually define the colors and legend labels
  scale_color_manual(
    name = "Method",
    values = c(
      "CR_W" = "lightgreen",
      "CR_F" = "darkgreen",
      "F_F" = "blue",
      "RF_F" = "red"
    ),
    labels = c(
      CR_W = TeX("$\\rho_{CR[1]}$"),
      CR_F = TeX("$\\rho_{CR[0]}$"),
      F_F = TeX("$\\rho_{N[0]^n}$"),
      RF_F = TeX("$\\rho_{R[0]^n}$")
    )
  ) +
  
  # Set y-axis limits and remove padding
  scale_y_continuous(limits = c(0, 0.18), expand = c(0, 0)) +
  
  # Customize theme elements like legend and title
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14)
  )


# Plot Figure 1.1
thetas = typeI$p1
gridlength = length(thetas)
M = 10^4# number of simulations
tests_considered = c("N_W", "R_W")
successes <- typeI[,tests_considered]*M
failures <- M - successes
total_trials <- successes + failures
pred_probs = zeros(gridlength, length(tests_considered)+1)
colnames(pred_probs) = c("p1",tests_considered)
for(test in tests_considered){
  data <- data.frame(thetas = thetas, successes = successes[,test], trials = total_trials[,test])
  
  model <- glm(cbind(successes, trials - successes) ~ bs(thetas, df = 20, degree = 4),
               data = data, family = binomial)
  
  theta_grid <- seq(0, 1, length.out = gridlength)
  pred_probs[,test] <- predict(model, newdata = data.frame(thetas = theta_grid), type = "response")
}
pred_probs[,"p1"]  <- seq(0, 1, length.out = gridlength)
pred_probs_df <- as.data.frame(pred_probs)

ggplot(pred_probs_df, aes(x = p1)) +
  geom_line(aes(y = N_W, color = "N_W"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = R_W, color = "R_W"), linetype = "solid", size = 1.2) +
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
  scale_y_continuous(limits = c(0, 0.9), expand = c(0, 0)) +
  theme(
    plot.title = element_text(hjust = 0.5, size=15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 14),
    # Add these lines to control axis title size
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

# Plot Figure 1.2
thetas = typeI$p1
gridlength = length(thetas)
M = 10^4# number of simulations
tests_considered = c("CR_W", "CR_F", "F_F", "RF_F")
successes <- typeI[,tests_considered]*M
failures <- M - successes
total_trials <- successes + failures
pred_probs = zeros(gridlength, length(tests_considered)+1)
colnames(pred_probs) = c("p1",tests_considered)
for(test in tests_considered){
  data <- data.frame(thetas = thetas, successes = successes[,test], trials = total_trials[,test])
  
  model <- glm(cbind(successes, trials - successes) ~ bs(thetas, df = 20, degree = 4),
               data = data, family = binomial)
  
  theta_grid <- seq(0, 1, length.out = gridlength)
  pred_probs[,test] <- predict(model, newdata = data.frame(thetas = theta_grid), type = "response")
}
pred_probs[,"p1"]  <- seq(0, 1, length.out = gridlength)
pred_probs_df <- as.data.frame(pred_probs)

ggplot(pred_probs_df, aes(x = p1)) +
  geom_line(aes(y = CR_W  , color = "CR_W"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = CR_F, color = "CR_F"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = F_F, color = "F_F"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = RF_F, color = "RF_F"), linetype = "solid", size = 1.2) +
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
  scale_y_continuous(limits = c(0, 0.18), expand = c(0, 0)) + 
  theme(
    plot.title = element_text(hjust = 0.5, size=15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 14),
    # Add these lines to control axis title size
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
