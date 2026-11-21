library(dplyr)
library(tidyr)
library(ggplot2)
library(car)

#rm(list = ls()) #clear environment

diabetes <- read.csv(file = "/Users/thomaseapen/Downloads/PHB221 Notebooks/Diabetes Dataset.csv",header=TRUE, sep = ",")
diabetes <- na.omit(diabetes) #Clear rows with missing values in this case there are none
head(diabetes)
diabetes$Gender <- as.factor(diabetes$Gender)
diabetes$Family.History <- as.factor(diabetes$Family.History)
diabetes$Hypertension <- as.factor(diabetes$Hypertension) #Convert char vars to binary factor variables
diabetes$Outcome <- as.factor(diabetes$Outcome) #Binary vars

diabetes$Physical.Activity <- as.factor(diabetes$Physical.Activity)
diabetes$Smoking.Status <- as.factor(diabetes$Smoking.Status)
diabetes$Alcohol.Intake <- as.factor(diabetes$Alcohol.Intake) #3 level variables

dmodel <- lm(Glucose ~ Gender + Age + Physical.Activity + Smoking.Status 
             + Alcohol.Intake + Blood.Pressure + Skin.Thickness + Insulin + BMI
             + Cholesterol + Diabetes.Pedigree.Function + Family.History 
             + Hypertension, data = diabetes)
summary(dmodel)

residualPlots(dmodel)

#From Residual Plots now try with log(Skin.Thickness)
dmodel2 <- lm(Glucose ~ Gender + Age + Physical.Activity + Smoking.Status + 
    Alcohol.Intake + Blood.Pressure + log(Skin.Thickness) + Insulin + BMI + 
    Cholesterol + Diabetes.Pedigree.Function + Family.History + 
    Hypertension, data = diabetes)
summary(dmodel2)
#Notice model doesn't explain much variance in our data very small R-squared
#Only statistically significant variable for predicting glucose is cholesterol

# 1. Inspect just a subset of variables per screen (e.g., 2x2 grid)
residualPlots(dmodel2, terms = ~ Age + Blood.Pressure + log(Skin.Thickness) + Insulin,
  layout = c(2, 2),
  col = rgb(0, 0, 0, alpha = 0.08), # Make points semi-transparent
  pch = 16,                         # Solid dots
  cex = 0.6,                        # Smaller point size
  col.quad = "red",                 # High-contrast LOESS/quadratic line
  lwd = 2
)

# 2. Plot the remaining continuous predictors + fitted values
residualPlots(
  dmodel2, terms = ~ BMI + Cholesterol + Diabetes.Pedigree.Function,
  layout = c(2, 2), col = rgb(0, 0, 0, alpha = 0.08), pch = 16,
  cex = 0.6, col.quad = "red", lwd = 2
)

#Check if a highly focused model can isolate a real signal
dmodel_simple <- lm(Glucose ~ Insulin + BMI + Cholesterol, data = diabetes)
summary(dmodel_simple)

car::vif(dmodel)

log_model <- glm(Outcome ~ Age + BMI + Cholesterol + Blood.Pressure, 
                 data = diabetes, family = "binomial")
summary(log_model) #finds BMI is low p-value

# Fits Glucose ~ Cholesterol + BMI + (Cholesterol x BMI) + Age
int_model1 <- lm(Glucose ~ Cholesterol * BMI + Age + Gender, data = diabetes)

# Check the coefficient for the interaction term (Cholesterol:BMI)
summary(int_model1)

# Run a Type II ANOVA to test the overall significance of the interaction
car::Anova(int_model1, type = 2)

# 1. Create clinical categories for BMI to act as grouping variables
diabetes_vis <- diabetes %>%
  mutate(BMI_Category = case_when(
    BMI < 25 ~ "Normal",
    BMI >= 25 & BMI < 30 ~ "Overweight",
    BMI >= 30 ~ "Obese"
  )) %>%
  filter(!is.na(BMI_Category))

# 2. Plot Cholesterol vs Glucose, with separate trend lines for each BMI category
ggplot(diabetes_vis, aes(x = Cholesterol, y = Glucose, color = BMI_Category)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  labs(
    title = "Interaction Effect: How BMI Alters the Cholesterol-Glucose Relationship",
    x = "Cholesterol Level",
    y = "Fasting Glucose",
    color = "BMI Category"
  ) +
  theme_minimal(base_size = 12) +
  scale_color_brewer(palette = "Set1")

# Calculate means and counts for clinical variables grouped by Outcome
summary_table <- diabetes %>%
  group_by(Outcome) %>%
  summarise(
    N = n(),
    Mean_BMI = mean(BMI, na.rm = TRUE),
    Mean_Cholesterol = mean(Cholesterol, na.rm = TRUE),
    Mean_Blood_Pressure = mean(Blood.Pressure, na.rm = TRUE),
    Mean_Glucose = mean(Glucose, na.rm = TRUE)
  )

print(summary_table)

# Two-sample t-test for BMI
t.test(BMI ~ Outcome, data = diabetes)

# Cross-tabulate and run Chi-Square for Family History
table_family <- table(diabetes$Family.History, diabetes$Outcome)
chisq.test(table_family)

# Synthesized Logistic Model including the biomarker (Glucose)
combined_model <- glm(
  Outcome ~ Glucose + BMI + Cholesterol + Age + Blood.Pressure, 
  family = "binomial", 
  data = diabetes
)

summary(combined_model)

# Combined Linear Model predicting Glucose
combined_glucose_model <- lm(
  Glucose ~ Cholesterol + BMI + Age + Blood.Pressure, 
  data = diabetes
)

summary(combined_glucose_model)
