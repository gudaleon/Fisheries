###########################################################################
#####################################################################################
##### Identification of mean and confidence interval through Monte Carlo simulations
#####################################################################################
intercept_mean<- -0.076
intercept_sd <- 0.774

#fixed effect for TP
slope_mean<-0.036
slope_sd <-0.015

#Interaction effect
slope_int_mean<- 0.508
slope_int_sd<- 0.003

random_effect1_mean <--0.2455365
random_effect2_mean <-0.3116444
random_effect3_mean <--0.2243787
random_effect4_mean <--0.3436556
random_effect5_mean <-0.5028705

random_effect1_sd <-0.2793612
random_effect2_sd <-0.2150276
random_effect3_sd <-0.1952225
random_effect4_sd <-0.2307524
random_effect5_sd <-0.2932039


set.seed(123) # For reproducibility
n_samples <- 100

getwd()
setwd("/Users/alex.neumann/Downloads")
data1<-read.csv("fish_data_set1.csv")
min(data1$logTP_stp_)
max(data1$logTP_stp_)

x_values <- seq(min(data1$logTP_stp_), max(data1$logTP_stp_), length.out = 100)

# Sample intercept, slope, and random effects
sampled_intercepts <- rnorm(n_samples, intercept_mean, intercept_sd)

sampled_slopes <- rnorm(n_samples, slope_mean, slope_sd)

sampled_random_effect1 <- rnorm(n_samples, random_effect1_mean, random_effect1_sd)
sampled_random_effect2 <- rnorm(n_samples, random_effect2_mean, random_effect2_sd)
sampled_random_effect3 <- rnorm(n_samples, random_effect3_mean, random_effect3_sd)
sampled_random_effect4 <- rnorm(n_samples, random_effect4_mean, random_effect4_sd)
sampled_random_effect5 <- rnorm(n_samples, random_effect5_mean, random_effect5_sd)

sampled_slope_interaction <- rnorm(n_samples, slope_int_mean, slope_int_sd)



y_preds1 <- matrix(0, nrow=n_samples, ncol=length(x_values))
y_preds2 <- matrix(0, nrow=n_samples, ncol=length(x_values))
y_preds3 <- matrix(0, nrow=n_samples, ncol=length(x_values))
y_preds4 <- matrix(0, nrow=n_samples, ncol=length(x_values))
y_preds5 <- matrix(0, nrow=n_samples, ncol=length(x_values))



for (i in 1:n_samples) {
  y_preds1[i,] <- sampled_slopes[i] * x_values + (sampled_intercepts[i] + sampled_random_effect1[i] + sampled_slope_interaction*1)
  y_preds2[i,] <- sampled_slopes[i] * x_values + (sampled_intercepts[i] + sampled_random_effect2[i] + sampled_slope_interaction*2)
  y_preds3[i,] <- sampled_slopes[i] * x_values + (sampled_intercepts[i] + sampled_random_effect3[i] + sampled_slope_interaction*3)
  y_preds4[i,] <- sampled_slopes[i] * x_values + (sampled_intercepts[i] + sampled_random_effect4[i] + sampled_slope_interaction*4)
  y_preds5[i,] <- sampled_slopes[i] * x_values + (sampled_intercepts[i] + sampled_random_effect5[i] + sampled_slope_interaction*5)
  
}

y_median1 <- apply(y_preds1, 2, median)
y_median2 <- apply(y_preds2, 2, median)
y_median3 <- apply(y_preds3, 2, median)
y_median4 <- apply(y_preds4, 2, median)
y_median5 <- apply(y_preds5, 2, median)


ci_low1 <- apply(y_preds1, 2, quantile, probs=0.025)
ci_high1 <- apply(y_preds1, 2, quantile, probs=0.975)

ci_low2 <- apply(y_preds2, 2, quantile, probs=0.025)
ci_high2 <- apply(y_preds2, 2, quantile, probs=0.975)

ci_low3 <- apply(y_preds3, 2, quantile, probs=0.025)
ci_high3 <- apply(y_preds3, 2, quantile, probs=0.975)

ci_low4 <- apply(y_preds4, 2, quantile, probs=0.025)
ci_high4 <- apply(y_preds4, 2, quantile, probs=0.975)

ci_low5 <- apply(y_preds5, 2, quantile, probs=0.025)
ci_high5 <- apply(y_preds5, 2, quantile, probs=0.975)

library(ggplot2)

df <- data.frame(x=x_values,
                 y_median1=y_median1, y_median2=y_median2,y_median3=y_median3,y_median4=y_median4,y_median5=y_median5,
                 ci_low1=ci_low1, ci_high1=ci_high1,
                 ci_low1=ci_low2, ci_high1=ci_high2,
                 ci_low1=ci_low3, ci_high1=ci_high3,
                 ci_low1=ci_low4, ci_high1=ci_high4,
                 ci_low2=ci_low5, ci_high2=ci_high5)


#plot without filling, with dashed lines for uncertainty
library(ggplot2)

df <- data.frame(x=x_values,
                 y_median1=y_median1, y_median2=y_median2,y_median3=y_median3,y_median4=y_median4,y_median5=y_median5,
                 ci_low1=ci_low1, ci_high1=ci_high1,
                 ci_low1=ci_low2, ci_high1=ci_high2,
                 ci_low1=ci_low3, ci_high1=ci_high3,
                 ci_low1=ci_low4, ci_high1=ci_high4,
                 ci_low2=ci_low5, ci_high2=ci_high5)

ggplot(df, aes(x=x_values)) +
  geom_line(aes(y=y_median1, color="Planktivorous"), linewidth=1) +
  geom_line(aes(y=ci_low1, color="Planktivorous"), linetype="dashed") +
  geom_line(aes(y=ci_high1, color="Planktivorous"), linetype="dashed") +
  geom_line(aes(y=y_median2, color="Omnivorous"), linewidth=1) +
  geom_line(aes(y=ci_low2, color="Omnivorous"), linetype="dashed") +
  geom_line(aes(y=ci_high2, color="Omnivorous"), linetype="dashed") +
  geom_line(aes(y=y_median3, color="Piscivorous"), linewidth=1) +
  geom_line(aes(y=ci_low3, color="Piscivorous"), linetype="dashed") +
  geom_line(aes(y=ci_high3, color="Piscivorous"), linetype="dashed") +
  geom_line(aes(y=y_median4, color="Invertivorous"), linewidth=1) +
  geom_line(aes(y=ci_low4, color="Invertivorous"), linetype="dashed") +
  geom_line(aes(y=ci_high4, color="Invertivorous"), linetype="dashed") +
  geom_line(aes(y=y_median5, color="Benthivorous"), linewidth=1) +
  geom_line(aes(y=ci_low5, color="Benthivorous"), linetype="dashed") +
  geom_line(aes(y=ci_high5, color="Benthivorous"), linetype="dashed") +
  labs(title="Linear Model with Random Effects(Trophic Level)", x="TP", y="Fish_Biomass(ln)") +
  scale_color_manual(values=c(Planktivorous="blue",Omnivorous="red",Piscivorous="black",Invertivorous="green",Benthivorous="purple")) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 1)) +
  coord_cartesian(clip = "off")
