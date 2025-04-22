library(readxl)
library(car)    
library(ggplot2) 
library(lmtest)
library(forcats)
library(dplyr)
library(sandwich)
library(lmtest)
library(MASS)


data <- read_excel("Datainsamling_volvo_sammanstalldV10.xlsx")

str(data)

summary(data)

data$Age <- 2025 - data$Modellår

data <- subset(data, select = -c(URL, Märke, Datum_i_trafik, Färg, Motorstorlek))

data$Säljare <- as.factor(data$Säljare)
data$Bränsle <- as.factor(data$Bränsle)
data$Växellåda <- as.factor(data$Växellåda)
data$Biltyp <- as.factor(data$Biltyp)
data$Drivning <- as.factor(data$Drivning)
data$Modell <- as.factor(data$Modell)
data$Region <- as.factor(data$Region)

data$Försäljningspris <- as.numeric(data$Försäljningspris)
data$Miltal <- as.numeric(data$Miltal)
data$Hästkrafter <- as.numeric(data$Hästkrafter)
data$Age <- as.numeric(data$Age)

colSums(is.na(data))

data <- na.omit(data[, c("Försäljningspris", "Age", "Miltal", "Hästkrafter", "Säljare", "Bränsle", "Växellåda", "Biltyp", "Drivning", "Modell", "Region")])

model2 <- lm(Försäljningspris ~ Age + Miltal + Hästkrafter + Säljare + Bränsle + Växellåda + Biltyp + Drivning + Modell + Region, data = data)

#running diagnostics to evaluate teoretical assumptions
#Non-linearity
plot(model2, which = 1)  # Residuals vs. Fitted
par(mfrow = c(1, 3))
plot(data$Age, data$Försäljningspris, xlab = "Age", ylab = "Försäljningspris")
plot(data$Miltal, data$Försäljningspris, xlab = "Miltal", ylab = "Försäljningspris")
plot(data$Hästkrafter, data$Försäljningspris, xlab = "Hästkrafter", ylab = "Försäljningspris")

#Correlated residuals
library(lmtest)
dwtest(model2)
plot(resid(model2), type = "p", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

#Heteroskedasticity
plot(model2, which = 3)
bptest(model2)

#Non-normality of residuals
plot(model2, which = 2)
shapiro.test(resid(model2))

#Outliers
plot(model2, which = 5)
std_resid <- rstandard(model2)
outliers <- which(abs(std_resid) > 3)
if (length(outliers) > 0) print(data[outliers, ]) else print("No significant outliers detected.")

#High leverage points
plot(model2, which = 5)
cooks_d <- cooks.distance(model2)
high_leverage <- which(cooks_d > 4 / nrow(data))
if (length(high_leverage) > 0) print(data[high_leverage, ]) else print("No high leverage points detected.")

#Multicollinearity 
cor(data[, c("Age", "Miltal", "Hästkrafter")], use = "complete.obs")
vif(model2)

#Based on above diagnostics the below changes are applied to conform to teoretical assumptions
#Transform variables
data$log_Pris <- log(data$Försäljningspris)
data$log_Miltal <- log(data$Miltal + 1)

#Simplify Modell 
data$Modell <- fct_lump(data$Modell, n = 10)

#Refit model without Region
model_fixed <- lm(log_Pris ~ poly(Age, 2) + log_Miltal + Hästkrafter +
                    Säljare + Bränsle + Växellåda + Biltyp + Drivning +
                    Modell, data = data)

#Identify problematic points
std_resid <- rstandard(model_fixed)
cooks_d <- cooks.distance(model_fixed)
outliers <- which(abs(std_resid) > 3)
high_leverage <- which(cooks_d > 4 / nrow(data))
problematic <- union(outliers, high_leverage)

#Remove those points and refit final model 
data_clean <- data[-problematic, ]

model_final <- lm(log_Pris ~ poly(Age, 2) + log_Miltal + Hästkrafter +
                    Säljare + Bränsle + Växellåda + Biltyp + Drivning +
                    Modell, data = data_clean)


#Re-checking diagnostics
plot(model_final, which = 1)

dwtest(model_final)

plot(model_final, which = 3)
bptest(model_final)

plot(model_final, which = 2)
shapiro.test(resid(model_final))

std_resid <- rstandard(model_final)
outliers <- which(abs(std_resid) > 3)
if (length(outliers) > 0) print(data_clean[outliers, ]) else print("No significant outliers detected.")

plot(model_final, which = 5)

vif(model_final)

#re-fitting model and using Robust Regression
model_robust <- rlm(log_Pris ~ poly(Age, 2) + log_Miltal + Hästkrafter + Säljare + Bränsle + Växellåda + Biltyp + Drivning + Modell, data = data_clean)
summary(model_robust)


#Re-checking diagnostics once again
plot(model_robust, which = 1)

dwtest(model_robust)

plot(model_robust, which = 3)
bptest(model_robust)

plot(model_robust, which = 2)
shapiro.test(resid(model_robust))

std_resid <- rstandard(model_robust)
outliers <- which(abs(std_resid) > 3)
if (length(outliers) > 0) print(data_clean[outliers, ]) else print("No significant outliers detected.")

plot(model_robust, which = 5)

vif(model_robust)

summary(model_robust)

#checking for errors in flagged data points
data_clean[c(410, 479), ]

# Create a new data frame for predictions
# specifically measuring the effect of Age on price (creating 100 values between min and max) while other
# variables remain constant 

new_data_age <- data.frame(
  Age = seq(min(data_clean$Age), max(data_clean$Age), length.out = 100),
  log_Miltal = mean(data_clean$log_Miltal),
  Hästkrafter = mean(data_clean$Hästkrafter),
  Säljare = "Företag",
  Bränsle = "Bensin",
  Växellåda = "Automat",
  Biltyp = "Kombi",
  Drivning = "Fyrhjulsdriven",
  Modell = "V60"
)

# Predict log_Pris and convert to Försäljningspris
new_data_age$pred_log_Pris <- predict(model_robust, newdata = new_data_age)
new_data_age$pred_Pris <- exp(new_data_age$pred_log_Pris)

ggplot(new_data_age, aes(x = Age, y = pred_Pris)) +
  geom_line(color = "blue") +
  labs(x = "Age (Years)", y = "Predicted Försäljningspris (SEK)", 
       title = "Effect of Age on Car Price") +
  theme_minimal()

# specifically measuring the effect of Miltal on price (creating 100 values between min and max) while other
# variables remain constant 

new_data_miltal <- data.frame(
  Age = mean(data_clean$Age),
  log_Miltal = seq(min(data_clean$log_Miltal), max(data_clean$log_Miltal), length.out = 100),
  Hästkrafter = mean(data_clean$Hästkrafter),
  Säljare = "Företag",
  Bränsle = "Bensin",
  Växellåda = "Automat",
  Biltyp = "Kombi",
  Drivning = "Fyrhjulsdriven",
  Modell = "V60"
)

new_data_miltal$pred_log_Pris <- predict(model_robust, newdata = new_data_miltal)
new_data_miltal$pred_Pris <- exp(new_data_miltal$pred_log_Pris)

ggplot(new_data_miltal, aes(x = exp(log_Miltal) - 1, y = pred_Pris)) +  # Convert back to Miltal
  geom_line(color = "red") +
  labs(x = "Mileage (Miltal)", y = "Predicted Försäljningspris (SEK)", 
       title = "Effect of Mileage on Car Price") +
  theme_minimal()

# specifically measuring the effect of Horsepower on price (creating 100 values between min and max) while other
# variables remain constant 

new_data_horsepower <- data.frame(
  Age = mean(data_clean$Age),
  log_Miltal = mean(data_clean$log_Miltal),
  Hästkrafter = seq(min(data_clean$Hästkrafter), max(data_clean$Hästkrafter), length.out = 100),
  Säljare = "Företag",
  Bränsle = "Bensin",
  Växellåda = "Automat",
  Biltyp = "Kombi",
  Drivning = "Fyrhjulsdriven",
  Modell = "V60"
)

new_data_horsepower$pred_log_Pris <- predict(model_robust, newdata = new_data_horsepower)
new_data_horsepower$pred_Pris <- exp(new_data_horsepower$pred_log_Pris)

ggplot(new_data_horsepower, aes(x = Hästkrafter, y = pred_Pris)) +
  geom_line(color = "green") +
  labs(x = "Horsepower (Hästkrafter)", y = "Predicted Försäljningspris (SEK)", 
       title = "Effect of Horsepower on Car Price") +
  theme_minimal()

# specifically measuring the effect of salestype on price (only create two rows, company vs private) while other
# variables remain constant 

new_data_säljare <- data.frame(
  Age = mean(data_clean$Age),
  log_Miltal = mean(data_clean$log_Miltal),
  Hästkrafter = mean(data_clean$Hästkrafter),
  Säljare = factor(c("Företag", "Privat")),
  Bränsle = "Bensin",
  Växellåda = "Automat",
  Biltyp = "Kombi",
  Drivning = "Fyrhjulsdriven",
  Modell = "V60"
)

new_data_säljare$pred_log_Pris <- predict(model_robust, newdata = new_data_säljare)
new_data_säljare$pred_Pris <- exp(new_data_säljare$pred_log_Pris)

ggplot(new_data_säljare, aes(x = Säljare, y = pred_Pris, fill = Säljare)) +
  geom_bar(stat = "identity") +
  labs(x = "Seller Type (Säljare)", y = "Predicted Försäljningspris (SEK)", 
       title = "Effect of Seller Type on Car Price") +
  theme_minimal()

# specifically measuring the effect of Modell on price (one row per modell) while other
# variables remain constant with their mean

new_data_modell <- data.frame(
  Age = mean(data_clean$Age),
  log_Miltal = mean(data_clean$log_Miltal),
  Hästkrafter = mean(data_clean$Hästkrafter),
  Säljare = "Företag",
  Bränsle = "Bensin",
  Växellåda = "Automat",
  Biltyp = "Kombi",
  Drivning = "Fyrhjulsdriven",
  Modell = factor(levels(data_clean$Modell))
)

new_data_modell$pred_log_Pris <- predict(model_robust, newdata = new_data_modell)
new_data_modell$pred_Pris <- exp(new_data_modell$pred_log_Pris)

ggplot(new_data_modell, aes(x = Modell, y = pred_Pris, fill = Modell)) +
  geom_bar(stat = "identity") +
  labs(x = "Model (Modell)", y = "Predicted Försäljningspris (SEK)", 
       title = "Effect of Model on Car Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Calculating pseudo R^2 since rlm don't output automatically in model summary

y <- data_clean$log_Pris
y_hat <- predict(model_robust)
rss <- sum((y - y_hat)^2)
tss <- sum((y - mean(y))^2)
pseudo_r2 <- 1 - rss / tss
pseudo_r2


#calculating and including p-value for the robust regression model
summary_model <- summary(model_robust)
t_values <- summary_model$coefficients[, "t value"]
p_values <- 2 * pt(-abs(t_values), df = summary_model$df[2])
cbind(summary_model$coefficients, "Pr(>|t|)" = p_values)


#randomly select 10 cars from the data set
set.seed(123) 
sample_rows <- sample(nrow(data_clean), 10)
sample_data <- data_clean[sample_rows, ]

#predict log price and convert to price
sample_data$pred_log_Pris <- predict(model_robust, newdata = sample_data)
sample_data$pred_Pris <- exp(sample_data$pred_log_Pris)

#show comparison
sample_data[c("Försäljningspris", "pred_Pris")]

#creating column with predictions for all data 
data_clean$pred_log_Pris <- predict(model_robust)
data_clean$pred_Pris <- exp(data_clean$pred_log_Pris)

#Residuals
residuals <- data_clean$Försäljningspris - data_clean$pred_Pris

#Metrics
MAE <- mean(abs(residuals))                         # Mean Absolute Error
RMSE <- sqrt(mean(residuals^2))                     # Root Mean Squared Error
MAPE <- mean(abs(residuals) / data_clean$Försäljningspris) * 100  # in percentage

cat("MAE: ", round(MAE, 2), "\n")
cat("RMSE: ", round(RMSE, 2), "\n")
cat("MAPE: ", round(MAPE, 2), "%\n")