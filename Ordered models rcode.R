# Installing and loading packages
install.packages("MASS")
install.packages("margins")
install.packages("ggplot2")
install.packages("reshape2")

library(MASS)
library(margins)
library(ggplot2)
library(reshape2)

# Creating the dataset
set.seed(123)
n <- 400
dat <- data.frame(
  apply = ordered(sample(c("unlikely", "somewhat likely", "very likely"), n, replace = TRUE),
                  levels = c("unlikely", "somewhat likely", "very likely")),
  pared = sample(0:1, n, replace = TRUE),
  public = sample(0:1, n, replace = TRUE),
  gpa = round(runif(n, 2.0, 4.0), 2)
)

# Estimating models
logit_model <- polr(apply ~ pared + public + gpa, data = dat, Hess = TRUE)
probit_model <- polr(apply ~ pared + public + gpa, data = dat, method = "probit", Hess = TRUE)

# Creating prediction dataset for GPA
gpa_vals <- seq(2.0, 4.0, by = 0.05)
new_data <- data.frame(
  pared = 1,      # fixed values
  public = 1,
  gpa = gpa_vals
)

# Predicting probabilities (logit model)
logit_probs <- predict(logit_model, newdata = new_data, type = "probs")
logit_df <- cbind(new_data["gpa"], logit_probs)
logit_long <- melt(logit_df, id.vars = "gpa", variable.name = "category", value.name = "probability")

# Predicting probabilities (probit model)
probit_probs <- predict(probit_model, newdata = new_data, type = "probs")
probit_df <- cbind(new_data["gpa"], probit_probs)
probit_long <- melt(probit_df, id.vars = "gpa", variable.name = "category", value.name = "probability")

# Plotting the logit model
ggplot(logit_long, aes(x = gpa, y = probability, color = category)) +
  geom_line(size = 1) +
  labs(title = "Predicted Probabilities: Ordered Logit Model",
       x = "GPA",
       y = "Probability",
       color = "Category") +
  theme_minimal()

# Plotting the probit model
ggplot(probit_long, aes(x = gpa, y = probability, color = category)) +
  geom_line(size = 1) +
  labs(title = "Predicted Probabilities: Ordered Probit Model",
       x = "GPA",
       y = "Probability",
       color = "Category") +
  theme_minimal()
