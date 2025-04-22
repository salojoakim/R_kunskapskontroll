library(readxl)
library(car)    
library(ggplot2) 
library(lmtest)
library(forcats)
library(dplyr)
library(sandwich)
library(MASS)
library(scales)

#loading data and updating based on the model development made in RegressionModelDevelopment5.0.R

data <- read_excel("Datainsamling_volvo_sammanstalldV10.xlsx")
data$Age <- 2025 - data$Modellår
data <- subset(data, select = -c(URL, Märke, Datum_i_trafik, Färg, Motorstorlek))


data <- data %>%
  mutate(across(c(Säljare, Bränsle, Växellåda, Biltyp, Drivning, Modell, Region), as.factor),
         across(c(Försäljningspris, Miltal, Hästkrafter, Age), as.numeric))

data <- na.omit(data)
data$log_Pris <- log(data$Försäljningspris)
data$log_Miltal <- log(data$Miltal + 1)
data$Modell <- fct_lump(data$Modell, n = 10)
data <- subset(data, select = -Region)


model_dev <- lm(log_Pris ~ poly(Age, 2) + log_Miltal + Hästkrafter +
                Säljare + Bränsle + Växellåda + Biltyp + Drivning + Modell,
                data = data)

#checking diagnostics
par(mfrow = c(2, 2))
plot(model_dev)

#splitting data into train and test
set.seed(123)
train_index <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

#refitting model with train data
model_robust <- rlm(log_Pris ~ poly(Age, 2) + log_Miltal + Hästkrafter +
                      Säljare + Bränsle + Växellåda + Biltyp + Drivning + Modell, 
                    data = train_data)

summary(model_robust)

#evaluating model on test data
test_data$pred_log_Pris <- predict(model_robust, newdata = test_data)
test_data$pred_Pris <- exp(test_data$pred_log_Pris)

resid_test <- test_data$Försäljningspris - test_data$pred_Pris
MAE_test <- mean(abs(resid_test))
RMSE_test <- sqrt(mean(resid_test^2))
MAPE_test <- mean(abs(resid_test) / test_data$Försäljningspris) * 100

cat("TEST MAE:", round(MAE_test, 2), "\n")
cat("TEST RMSE:", round(RMSE_test, 2), "\n")
cat("TEST MAPE:", round(MAPE_test, 2), "%\n")




new_data_age <- data.frame(
  Age = seq(min(data$Age), max(data$Age), length.out = 100),
  log_Miltal = mean(data$log_Miltal),
  Hästkrafter = mean(data$Hästkrafter),
  Säljare = "Företag",
  Bränsle = "Bensin",
  Växellåda = "Automat",
  Biltyp = "Kombi",
  Drivning = "Fyrhjulsdriven",
  Modell = "V60"
)
new_data_age$pred_log_Pris <- predict(model_robust, newdata = new_data_age)
new_data_age$pred_Pris <- exp(new_data_age$pred_log_Pris)

ggplot(new_data_age, aes(x = Age, y = pred_Pris)) +
  geom_line(color = "blue") +
  labs(x = "Ålder", y = "Predikterat Pris (SEK)", title = "Effekt av Ålder") +
  scale_y_continuous(labels = comma_format(big.mark = " ", decimal.mark = ",")) +
  theme_minimal()
