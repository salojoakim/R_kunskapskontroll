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


# 1. Non-linearity
plot(model2, which = 1)  # Residuals vs. Fitted
par(mfrow = c(1, 3))
plot(data$Age, data$Försäljningspris, xlab = "Age", ylab = "Försäljningspris")
plot(data$Miltal, data$Försäljningspris, xlab = "Miltal", ylab = "Försäljningspris")
plot(data$Hästkrafter, data$Försäljningspris, xlab = "Hästkrafter", ylab = "Försäljningspris")

# 2. Correlated residuals
library(lmtest)
dwtest(model2)
plot(resid(model2), type = "p", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

# 3. Heteroskedasticity
plot(model2, which = 3)
bptest(model2)

# 4. Non-normality of residuals
plot(model2, which = 2)
shapiro.test(resid(model2))

# 5. Outliers
plot(model2, which = 5)
std_resid <- rstandard(model2)
outliers <- which(abs(std_resid) > 3)
if (length(outliers) > 0) print(data[outliers, ]) else print("No significant outliers detected.")

# 6. High leverage points
plot(model2, which = 5)
cooks_d <- cooks.distance(model2)
high_leverage <- which(cooks_d > 4 / nrow(data))
if (length(high_leverage) > 0) print(data[high_leverage, ]) else print("No high leverage points detected.")

# 7. Multicollinearity (already checked, but confirming)
cor(data[, c("Age", "Miltal", "Hästkrafter")], use = "complete.obs")
vif(model2)


# --- Transform variables ---
data$log_Pris <- log(data$Försäljningspris)
data$log_Miltal <- log(data$Miltal + 1)

# --- Simplify Modell only (no Region anymore) ---
data$Modell <- fct_lump(data$Modell, n = 10)

# --- Refit model without Region ---
model_fixed <- lm(log_Pris ~ poly(Age, 2) + log_Miltal + Hästkrafter +
                    Säljare + Bränsle + Växellåda + Biltyp + Drivning +
                    Modell, data = data)

# --- Identify problematic points ---
std_resid <- rstandard(model_fixed)
cooks_d <- cooks.distance(model_fixed)
outliers <- which(abs(std_resid) > 3)
high_leverage <- which(cooks_d > 4 / nrow(data))
problematic <- union(outliers, high_leverage)

# --- Remove those points and refit final model ---
data_clean <- data[-problematic, ]

model_final <- lm(log_Pris ~ poly(Age, 2) + log_Miltal + Hästkrafter +
                    Säljare + Bränsle + Växellåda + Biltyp + Drivning +
                    Modell, data = data_clean)


# Non-linearity
plot(model_final, which = 1)

# Correlated residuals
dwtest(model_final)

# Heteroskedasticity
plot(model_final, which = 3)
bptest(model_final)

# Non-normality
plot(model_final, which = 2)
shapiro.test(resid(model_final))

# Outliers
std_resid <- rstandard(model_final)
outliers <- which(abs(std_resid) > 3)
if (length(outliers) > 0) print(data_clean[outliers, ]) else print("No significant outliers detected.")

# High leverage points
plot(model_final, which = 5)

# Multicollinearity
vif(model_final)


model_robust <- rlm(log_Pris ~ poly(Age, 2) + log_Miltal + Hästkrafter + Säljare + Bränsle + Växellåda + Biltyp + Drivning + Modell, data = data_clean)
summary(model_robust)



# Non-linearity
plot(model_robust, which = 1)

# Correlated residuals
dwtest(model_robust)

# Heteroskedasticity
plot(model_robust, which = 3)
bptest(model_robust)

# Non-normality
plot(model_robust, which = 2)
shapiro.test(resid(model_robust))

# Outliers
std_resid <- rstandard(model_robust)
outliers <- which(abs(std_resid) > 3)
if (length(outliers) > 0) print(data_clean[outliers, ]) else print("No significant outliers detected.")

# High leverage points
plot(model_robust, which = 5)

# Multicollinearity
vif(model_robust)

summary(model_robust)

data_clean[c(410, 479), ]



# Create a new data frame for predictions
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

# Plot
ggplot(new_data_age, aes(x = Age, y = pred_Pris)) +
  geom_line(color = "blue") +
  labs(x = "Age (Years)", y = "Predicted Försäljningspris (SEK)", 
       title = "Effect of Age on Car Price") +
  theme_minimal()


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

# Plot
ggplot(new_data_miltal, aes(x = exp(log_Miltal) - 1, y = pred_Pris)) +  # Convert back to Miltal
  geom_line(color = "red") +
  labs(x = "Mileage (Miltal)", y = "Predicted Försäljningspris (SEK)", 
       title = "Effect of Mileage on Car Price") +
  theme_minimal()



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

# Plot
ggplot(new_data_horsepower, aes(x = Hästkrafter, y = pred_Pris)) +
  geom_line(color = "green") +
  labs(x = "Horsepower (Hästkrafter)", y = "Predicted Försäljningspris (SEK)", 
       title = "Effect of Horsepower on Car Price") +
  theme_minimal()


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

# Plot
ggplot(new_data_säljare, aes(x = Säljare, y = pred_Pris, fill = Säljare)) +
  geom_bar(stat = "identity") +
  labs(x = "Seller Type (Säljare)", y = "Predicted Försäljningspris (SEK)", 
       title = "Effect of Seller Type on Car Price") +
  theme_minimal()


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

# Plot
ggplot(new_data_modell, aes(x = Modell, y = pred_Pris, fill = Modell)) +
  geom_bar(stat = "identity") +
  labs(x = "Model (Modell)", y = "Predicted Försäljningspris (SEK)", 
       title = "Effect of Model on Car Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


y <- data_clean$log_Pris
y_hat <- predict(model_robust)
rss <- sum((y - y_hat)^2)
tss <- sum((y - mean(y))^2)
pseudo_r2 <- 1 - rss / tss
pseudo_r2

summary(model_robust)

summary_model <- summary(model_robust)
t_values <- summary_model$coefficients[, "t value"]
p_values <- 2 * pt(-abs(t_values), df = summary_model$df[2])
cbind(summary_model$coefficients, "Pr(>|t|)" = p_values)