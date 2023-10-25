#Plot mixed model
#If y=mx+c is linear equation, and and if r is the random effect (which eg takes values of 1 or 2), then the equation becomes:
# y=mx+(c+r)

# Assuming x is your predictor variable
getwd()
setwd("/Users/alexneumann/Downloads")
data1<-read.csv("fish_dataset1.csv")
min(data1$logTP_stp_)
max(data1$logTP_stp_)

x_values <- seq(min(data1$logTP_stp_), max(data1$logTP_stp_), length.out = 100)

# Calculate predicted values for each random effect
slope_mean<-0.036
intercept_mean<--0.076
intercept_sd <-0.774   
random_effect1_mean <- 1.56
random_effect2_mean <- -0.09
random_effect3_mean <- -1.47

y_pred1 <- slope_mean * x_values + (intercept_mean + random_effect1_mean)
y_pred2 <- slope_mean * x_values + (intercept_mean + random_effect2_mean)
y_pred3 <- slope_mean * x_values + (intercept_mean + random_effect3_mean)


# For random effect 1
ci_low1 <- slope_mean * x_values + (qnorm(0.025, intercept_mean, intercept_sd) + random_effect1_mean)
ci_high1 <- slope_mean * x_values + (qnorm(0.975, intercept_mean, intercept_sd) + random_effect1_mean)

# For random effect 2
ci_low2 <- slope_mean * x_values + (qnorm(0.025, intercept_mean, intercept_sd) + random_effect2_mean)
ci_high2 <- slope_mean * x_values + (qnorm(0.975, intercept_mean, intercept_sd) + random_effect2_mean)

# For random effect 3
ci_low3 <- slope_mean * x_values + (qnorm(0.025, intercept_mean, intercept_sd) + random_effect3_mean)
ci_high3 <- slope_mean * x_values + (qnorm(0.975, intercept_mean, intercept_sd) + random_effect3_mean)

plot(x_values, y_pred1, type="l", frame = FALSE, xlim=c(1,5),ylim=c(-4,4), col="blue", lwd=1, main="Linear Model with Random Effects", xlab="Predictor", ylab="Response")

# Add lines for random effect 1
#lines(x_values, y_pred1, type="l", col="blue", lwd=2)
lines(x_values, ci_low1, type="l", col="blue", lty=1)
lines(x_values, ci_high1, type="l", col="blue", lty=1)

# Add lines for random effect 2
lines(x_values, y_pred2, col="red", lwd=2)
lines(x_values, ci_low2, col="red", lty=2)
lines(x_values, ci_high2, col="red", lty=2)

# Add lines for random effect 3
lines(x_values, y_pred3, col="green", lwd=2)
lines(x_values, ci_low3, col="green", lty=2)
lines(x_values, ci_high3, col="green", lty=2)

legend("topright", legend=c("Random Effect 1", "Random Effect 2", "Random Effect 3"),
       col=c("blue", "red","green"), lty=1, lwd=2, cex=0.8)

##### Identification of mean and confidence interval through Monte Carlo simulations
intercept_mean<--0.076
intercept_sd <-0.774 

slope_mean<-0.036
slope_sd <- 0.015
  
random_effect1_mean <- 1.56
random_effect2_mean <- -0.09
random_effect3_mean <- -1.47

random_effect1_sd <-0.77
random_effect2_sd <-0.77
random_effect3_sd <-0.77

set.seed(123) # For reproducibility
n_samples <- 10000

setwd("/Users/alexneumann/Downloads")
data1<-read.csv("fish_dataset1.csv")
min(data1$logTP_stp_)
max(data1$logTP_stp_)

x_values <- seq(min(data1$logTP_stp_), max(data1$logTP_stp_), length.out = 100)

# Sample intercept, slope, and random effects
sampled_intercepts <- rnorm(n_samples, intercept_mean, intercept_sd)
sampled_slopes <- rnorm(n_samples, slope_mean, slope_sd)
sampled_random_effect1 <- rnorm(n_samples, random_effect1_mean, random_effect1_sd)
sampled_random_effect2 <- rnorm(n_samples, random_effect2_mean, random_effect2_sd)
sampled_random_effect3 <- rnorm(n_samples, random_effect3_mean, random_effect3_sd)

y_preds1 <- matrix(0, nrow=n_samples, ncol=length(x_values))
y_preds2 <- matrix(0, nrow=n_samples, ncol=length(x_values))
y_preds3 <- matrix(0, nrow=n_samples, ncol=length(x_values))


for (i in 1:n_samples) {
  y_preds1[i,] <- sampled_slopes[i] * x_values + (sampled_intercepts[i] + sampled_random_effect1[i])
  y_preds2[i,] <- sampled_slopes[i] * x_values + (sampled_intercepts[i] + sampled_random_effect2[i])
  y_preds3[i,] <- sampled_slopes[i] * x_values + (sampled_intercepts[i] + sampled_random_effect3[i])
  
  }

y_median1 <- apply(y_preds1, 2, median)
y_median2 <- apply(y_preds2, 2, median)
y_median3 <- apply(y_preds3, 2, median)

ci_low1 <- apply(y_preds1, 2, quantile, probs=0.025)
ci_high1 <- apply(y_preds1, 2, quantile, probs=0.975)

ci_low2 <- apply(y_preds2, 2, quantile, probs=0.025)
ci_high2 <- apply(y_preds2, 2, quantile, probs=0.975)

ci_low3 <- apply(y_preds3, 2, quantile, probs=0.025)
ci_high3 <- apply(y_preds3, 2, quantile, probs=0.975)

library(ggplot2)

df <- data.frame(x=x_values, 
                 y_median1=y_median1, y_median2=y_median2, y_median3=y_median3,
                 ci_low1=ci_low1, ci_high1=ci_high1, 
                 ci_low2=ci_low2, ci_high2=ci_high2,
                 ci_low3=ci_low3, ci_high3=ci_high3)

ggplot(df, aes(x=x_values)) +
  geom_line(aes(y=y_median1, color="Random Effect 1")) +
  geom_ribbon(aes(ymin=ci_low1, ymax=ci_high1, fill="Random Effect 1"), alpha=0.3) +
  geom_line(aes(y=y_median2, color="Random Effect 2")) +
  geom_ribbon(aes(ymin=ci_low2, ymax=ci_high2, fill="Random Effect 2"), alpha=0.3) +
  geom_line(aes(y=y_median3, color="Random Effect 3")) +
  geom_ribbon(aes(ymin=ci_low3, ymax=ci_high3, fill="Random Effect 2"), alpha=0.3) +
  labs(title="Linear Model with Random Effects (Monte Carlo)", x="Predictor", y="Response") +
  scale_color_manual(values=c("blue", "red","green")) +
  scale_fill_manual(values=c("blue", "red","green")) +
  theme_minimal()


#plot without filling, with dashed lines for uncertainty
library(ggplot2)

df <- data.frame(x=x_values, 
                 y_median1=y_median1, y_median2=y_median2, 
                 ci_low1=ci_low1, ci_high1=ci_high1, 
                 ci_low2=ci_low2, ci_high2=ci_high2,
                 ci_low3=ci_low3, ci_high3=ci_high3)

ggplot(df, aes(x=x_values)) +
  geom_line(aes(y=y_median1, color="Random Effect 1"), linewidth=1) +
  geom_line(aes(y=ci_low1, color="Random Effect 1"), linetype="dashed") +
  geom_line(aes(y=ci_high1, color="Random Effect 1"), linetype="dashed") +
  geom_line(aes(y=y_median2, color="Random Effect 2"), linewidth=1) +
  geom_line(aes(y=ci_low2, color="Random Effect 2"), linetype="dashed") +
  geom_line(aes(y=ci_high2, color="Random Effect 2"), linetype="dashed") +
  geom_line(aes(y=y_median3, color="Random Effect 3"), linewidth=1) +
  geom_line(aes(y=ci_low3, color="Random Effect 3"), linetype="dashed") +
  geom_line(aes(y=ci_high3, color="Random Effect 3"), linetype="dashed") +
  labs(title="Linear Model with Random Effects (Monte Carlo)", x="Predictor", y="Response") +
  scale_color_manual(values=c("blue", "red","green")) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 1)) +
  coord_cartesian(clip = "off")


