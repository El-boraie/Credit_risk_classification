#Libraries to be used
library(DataExplorer)
library(RSQLite)
library(lattice)
library(ggplot2)
library(caret)
library(plyr)
library(dplyr)
library(stringr)
library(colorspace)
library(VIM)
library(mice)
library(class)
library(tidyverse)
library(janitor)
library(carData)
library(car)
library(vcd)
library(effects)
library(reshape2)
library(GGally)
options(scipen = 999)

# Load the data
riskfile = "C:\\Users\\aley0\\OneDrive\\Documents\\College Documents\\Year 2 Sem 1\\Programming for Data Analysis\\Assignment\\credit_risk_classification.csv"
rf = read.csv(riskfile)
rf

#Summary of the data set
summary(rf)

#View table
View(rf)

#Plot the NA values in each column
plot_missing(rf)

#-------------------------------------------------------------------------------

#Validation of the ordered categorization for the job column

# Filter the data to focus only on "unskilled resident" and "unskilled foreigner"
job_filtered <- rf %>% filter(job %in% c("unskilled resident", "unskilled foreign"))

# Create a contingency table for the filtered data
job_class <- table(job_filtered$job, job_filtered$class)
job_class


#-------------------------------------------------------------------------------
#STEP 1 (Round)

rf <- rf %>%
  mutate(
    credit_amount = round(credit_amount), 
    age = round(age), 
    duration = round(duration), 
    
    #round the installment commitment to the nearest hundredths place
    installment_commitment = round(installment_commitment, 2), 
    
    residence_since = round(residence_since), 
    existing_credits = round(existing_credits), 
    num_dependents = round(num_dependents) 
  )

#-------------------------------------------------------------------------------
#Step 2 (change column names)

#change the personal_status column to gender column
names(rf)[names(rf) == "personal_status"] <- "gender"

#-------------------------------------------------------------------------------
#STEP 3 (Replacing)

rf <- rf %>%
  mutate(
    # Credit History
    credit_history = replace(credit_history, credit_history %in% c("all paid", "no credits/all paid"), "paid fully"),
    credit_history = replace(credit_history, credit_history %in% c("critical/order existing credit"), "critical"),
    credit_history = replace(credit_history, credit_history %in% c("existing paid"), "taking credits"),
    
    # Saving Status
    savings_status = replace(savings_status, savings_status %in% c("500<=X<10000"), "500<=X<1000"),
    
    # Purpose
    purpose = replace(purpose, purpose %in% c("radio/tv", "furniture/equipment", "domestic appliance"), "consumer goods"),
    purpose = replace(purpose, purpose %in% c("new car", "used car"), "car purchase"),
    
    # Employment
    employment = replace(employment, employment %in% c("unemployed"), "0"),
    employment = replace(employment, employment %in% c("<1"), "0<X<1"),
    
    # Gender
    gender = replace(gender, gender %in% c("female div/dep/mar"), "female"),
    gender = replace(gender, gender %in% c("male single", "male div/sep", "male mar/wid"), "male"),
    
    # Property Magnitude
    property_magnitude = replace(property_magnitude, property_magnitude %in% c("no known property"), "unknown"),
    
    # Other Payment Plans
    other_payment_plans = replace(other_payment_plans, other_payment_plans == '', NA),
    
    # Job
    job = replace(job, job == "high qualif/self emp/mgmt", "high qualified"),
    job = replace(job, job == "unemp/unskilled non res", "unskilled foreign"),
    
    # Own Telephone
    own_telephone = replace(own_telephone, own_telephone == "none", "no")
  )

#-------------------------------------------------------------------------------
#STEP 4 (Factoring Ordered Categorization)

# ORDERED CATEGORIZATION

rf <- rf %>%
  mutate(
    
    checking_status = factor(checking_status, levels = c("no checking", "<0", "0<=X<200", ">=200"),ordered = TRUE),
    
    credit_history = factor(credit_history, levels = c("critical", "delayed previously", "taking credits", "paid fully"),ordered = TRUE),
    
    savings_status = factor(savings_status, levels = c("no known savings", "<100", "100<=X<500", "500<=X<1000", ">=1000"),ordered = TRUE),
    
    employment = factor(employment, levels = c("0", "0<X<1", "1<=X<4", "4<=X<7", ">=7"), ordered = TRUE),
    
    job = factor(job, levels = c("unskilled foreign", "unskilled resident", "skilled", "high qualified"), ordered = TRUE)
  
  )

#-------------------------------------------------------------------------------
#STEP 5 (Factoring Non Ordered Categorization)
#NON ORDERED CATEGORIZATION

rf <- rf %>%
  mutate(
    #Purpose
    purpose = factor(purpose, levels = c("consumer goods", "car purchase", "education",  "retraining", "business", "repairs", "other")),
    
    #Property Magnitude
    property_magnitude = factor(property_magnitude, levels = c("real estate", "life insurance", "car", "unknown")),
    
    #Housing
    housing = factor(housing, levels = c("own", "for free", "rent")),
    
    #Other Payment Plans 
    other_payment_plans = factor(other_payment_plans, levels = c("bank", "stores")),
    
    #Other Parties
    other_parties = factor(other_parties, levels = c("none", "guarantor", "co applicant")),
    
    #Class
    class = factor(class, levels = c("good", "bad"))
  
  )
#-------------------------------------------------------------------------------

#STEP 6 (Binary Values Treatment)
rf <- rf %>%
  mutate(
    gender = ifelse(gender == "male", 1, 0),
    num_dependents = ifelse(num_dependents == 2, 1, 0),
    own_telephone = ifelse(own_telephone == "yes", 1, 0),
    foreign_worker = ifelse(foreign_worker == "yes", 1, 0)
  )

#-------------------------------------------------------------------------------
#Step 7 (Set seed)
set.seed(123)

#-------------------------------------------------------------------------------

#STEP 8 (Missing Categorical Values Treatment)

# 1. MICE for Ordered Categorical Data
mice_vars <- c("checking_status", "credit_history", "savings_status", "employment", "job")
mice_imputed <- mice(rf[, mice_vars], method = 'cart', m = 5, maxit = 20)

rf[, mice_vars] <- complete(mice_imputed)


# 2. KNN IMPUTATION for Non-Ordered Categorical Data
knn_vars <- c("purpose", "other_parties", "property_magnitude", 
              "other_payment_plans", "housing")
rf <- VIM::kNN(rf, variable = knn_vars, k = 5, imp_var = FALSE)


#-------------------------------------------------------------------------------

#STEP 9 (Missing Continuous Values Treatment)

# Linear Regression Imputation
linear_vars <- c("duration", "credit_amount", "installment_commitment", "residence_since", "age", "existing_credits")
for (var in linear_vars) {
  model <- lm(as.formula(paste(var, "~ .")), data = rf)
  rf[[var]][is.na(rf[[var]])] <- predict(model, newdata = rf[is.na(rf[[var]]), ])
}

#-------------------------------------------------------------------------------

#STEP 10 (Missing Binary Values Treatment)

# Logistic Regression Imputation
logistic_vars <- c("gender", "num_dependents", "own_telephone", "foreign_worker")
for (var in logistic_vars) {
  model <- glm(as.formula(paste(var, "~ .")), data = rf, family = binomial)
  predicted <- predict(model, newdata = rf[is.na(rf[[var]]), ], type = "response")
  rf[[var]][is.na(rf[[var]])] <- ifelse(predicted > 0.5, 1, 0)
}

#-------------------------------------------------------------------------------

#Step 11 (The Remaining Missing Values Treatment)

# Subset columns with missing values
columns_with_na <- colnames(rf)[colSums(is.na(rf)) > 0]

rf_na <- rf[, columns_with_na]

# Define MICE methods
mice_methods <- make.method(rf_na)

mice_methods[c("duration", "credit_amount", "installment_commitment", "residence_since", "age", "existing_credits")] <- "pmm"

mice_methods[c("gender", "num_dependents", "own_telephone", "foreign_worker")] <- "cart"

# Perform MICE
mice_imputed <- mice(rf_na, method = mice_methods, m = 5, maxit = 50, seed = 123)

# Replace imputed data in the original data set
rf_imputed <- complete(mice_imputed)
rf[, columns_with_na] <- rf_imputed

#-------------------------------------------------------------------------------

#STEP 12 (Round the numbers again)

rf <- rf %>%
  mutate(
    credit_amount = round(credit_amount), 
    age = round(age), 
    duration = round(duration),
    
    #round the installment commitment to the nearest hundredths place
    installment_commitment = round(installment_commitment, 2), 
    
    residence_since = round(residence_since), 
    existing_credits = round(existing_credits), 
    num_dependents = round(num_dependents) s
  )

#-------------------------------------------------------------------------------
#Step 13 (installment commitment value fixing)

rf <- rf %>%
  mutate(
    installment_commitment = ifelse(installment_commitment < 1, 1, installment_commitment),
    installment_commitment = ifelse(installment_commitment > 4, 4, installment_commitment)
  )

#-------------------------------------------------------------------------------

#Step 14 (Treat the binary variables back to the original form)

# Revert binary variables back to original format
rf <- rf %>%
  mutate(
    gender = ifelse(gender == 1,"male", "female"),
    num_dependents = ifelse(num_dependents == 1, 2, 1),
    own_telephone = ifelse(own_telephone == 1, "yes", "no"),
    foreign_worker = ifelse(foreign_worker == 1, "yes", "no")
  )

#Categorize them to factor

rf <- rf %>%
  mutate(
    gender = factor(gender, levels =c("male", "female")), 
    num_dependents = factor(num_dependents, levels = c("1", "2")),
    own_telephone = factor(own_telephone, levels = c("yes", "no")),
    foreign_worker = factor(foreign_worker, levels = c("yes", "no"))
  )

#-------------------------------------------------------------------------------
#Step 15 (Check the progress)

#Summary of the data set
summary(rf)

#View table
View(rf)

#Plot the NA values in each column
plot_missing(rf)

#-------------------------------------------------------------------------------

#Step 16 (Export the cleaned data into a new csv file)
write.csv(rf, "C:\\Users\\aley0\\OneDrive\\Documents\\College Documents\\Year 2 Sem 1\\Programming for Data Analysis\\Assignment\\credit_risk_classification_cleaned.csv")

#-------------------------------------------------------------------------------------------------------------------------------------------

#ANALYSIS
newrf <- read.csv("C:\\Users\\aley0\\OneDrive\\Documents\\College Documents\\Year 2 Sem 1\\Programming for Data Analysis\\Assignment\\credit_risk_classification_cleaned.csv")
newrf


View(newrf)


#drop the first column
newrf <- newrf[,-1]

#-------------------------------------------------------------------------------

#Categorizing the data for analysis

# ORDERED CATEGORIZATION
newrf <- newrf %>%
  mutate(
    
    checking_status = factor(checking_status, levels = c("no checking", "<0", "0<=X<200", ">=200"),ordered = TRUE),
    
    credit_history = factor(credit_history, levels = c("critical", "delayed previously", "taking credits", "paid fully"),ordered = TRUE),
    
    savings_status = factor(savings_status, levels = c("no known savings", "<100", "100<=X<500", "500<=X<1000", ">=1000"),ordered = TRUE),
    
    employment = factor(employment, levels = c("0", "0<X<1", "1<=X<4", "4<=X<7", ">=7"), ordered = TRUE),
    
    job = factor(job, levels = c("unskilled foreign", "unskilled resident", "skilled", "high qualified"), ordered = TRUE)
    
  )

#NON ORDERED CATEGORIZATION

newrf <- newrf %>%
  mutate(
    purpose = factor(purpose, levels = c("consumer goods", "car purchase", "education",  "retraining", "business", "repairs", "other")),
    
    property_magnitude = factor(property_magnitude, levels = c("real estate", "life insurance", "car", "unknown")),
    
    housing = factor(housing, levels = c("own", "for free", "rent")),
    
    other_payment_plans = factor(other_payment_plans, levels = c("bank", "stores")),
    
    other_parties = factor(other_parties, levels = c("none", "guarantor", "co applicant")),
    
    class = factor(class, levels = c("good", "bad")),
    
    gender = factor(gender, levels = c("male", "female")),
    
    num_dependents = factor(num_dependents, levels = c("1", "2")),
    
    own_telephone = factor(own_telephone, levels = c("yes", "no")),
    
    foreign_worker = factor(foreign_worker, levels = c("yes", "no"))
  )


#summary of the data
summary(newrf)

#-------------------------------------------------------------------------------

#Analysis
#Objective 1 : Examine whether owning real estate (property magnitude) is associated with a higher likelihood of good credit

#Analysis 1-1: Is there a relationship between real estate ownership and the likelihood of having "good" credit?

#1 : Bar Chart
data_summary1 <- newrf %>%
  count(property_magnitude, class, name = "count")

data_summary_1 <- data_summary1 %>%
  group_by(property_magnitude) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Bar chart with proportions and percentage labels
ggplot(data_summary_1, aes(x = property_magnitude, y = proportion, fill = class)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), size = 4) +
  labs(
    title = "Proportional Distribution of Credit Class by Property Magnitude",
    x = "Property Magnitude",
    y = "Proportion",
    fill = "Credit Class"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))


#2 : Mosaic Plot
mosaic(~ property_magnitude + class, data = newrf, 
       shade = TRUE, legend = TRUE, 
       main = "Association Between Property Magnitude and Credit Class")


# Chi-Square Test
chi_sq_property <- chisq.test(property_class_table)
print(chi_sq_property)       # Chi-Square Test result

# Chi-Square Test Interpretation
if (chi_sq_property$p.value < 0.05) {
  print("There is a statistically significant association between property magnitude and credit class (p < 0.05).")
} else {
  print("There is no statistically significant association between property magnitude and credit class (p >= 0.05).")
}


#Analysis 1-2: What is the proportion of "good" and "bad" credit classes for each type of property ownership?

#3 : Heatmap
# Convert contingency table to proportions
property_class_table <- table(newrf$property_magnitude, newrf$class)
prop_table_heat <- prop.table(property_class_table, margin = 1)
heatmap_data <- melt(prop_table_heat)

# Heatmap plot with geom_text
ggplot(heatmap_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(value, accuracy = 0.1)), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue", labels = scales::percent) +
  labs(
    title = "Heatmap of Proportions: Property Magnitude and Credit Class",
    x = "Credit Class",
    y = "Property Magnitude",
    fill = "Proportion"
  ) +
  theme_minimal()


#4 : Association Plot
assoc(~ property_magnitude + class, data = newrf, 
shade = TRUE, legend = TRUE, 
main = "Association Plot: Property Magnitude and Credit Class")



#Analysis 1-3: How strong is the association between property ownership and credit class?

# Proportions
prop_table <- prop.table(property_class_table, margin = 1)
print(prop_table)           # Proportions


# Cramér's V
cramers_v <- assocstats(property_class_table)$cramer
print(paste("Cramér's V:", round(cramers_v, 2)))


# Cramér's V Interpretation
if (cramers_v < 0.1) {
  print("There is a weak association between property magnitude and credit class (Cramér's V < 0.1).")
} else if (cramers_v < 0.3) {
  print("There is a moderate association between property magnitude and credit class (0.1 <= Cramér's V < 0.3).")
} else {
  print("There is a strong association between property magnitude and credit class (Cramér's V >= 0.3).")
}

#-------------------------------------------------------------------------------
#Objective 2 : Investigate the relationship between foreign worker status (foreign_worker) and the likelihood of having good credit

#Analysis 2-1: Is there a significant difference in the distribution of "good" and "bad" credit classes between foreign workers and non-foreign workers?
# 1 : Bar Chart
data_summary2 <- newrf %>%
  count(foreign_worker, class, name = "count")

data_summary_2 <- data_summary2 %>%
  group_by(foreign_worker) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

ggplot(data_summary_2, aes(x = foreign_worker, y = proportion, fill = class)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Proportional Distribution of Credit Class by Foreign Worker Status",
       x = "Foreign Worker",
       y = "Proportion") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))


# 2 : Mosaic Plot  
mosaic(~ foreign_worker + class, data = newrf,
       shade = TRUE, legend = TRUE,
       main = "Association Between Foreign Worker Status and Credit Class")

# Chi-Square Test
chi_sq_foreign <- chisq.test(foreign_class_table)
print(chi_sq_foreign)        # Chi-Square Test result

# Chi-Square Test Interpretation
if (chi_sq_foreign$p.value < 0.05) {
  print("There is a statistically significant relationship between foreign worker status and credit class (p < 0.05).")
} else {
  print("There is no statistically significant relationship between foreign worker status and credit class (p >= 0.05).")
}


#Analysis 2-2: What proportion of foreign workers and non-foreign workers belong to the "good" and "bad" credit classes, and how do these proportions compare?


# 3 : Heatmap
# Create contingency table
foreign_class_table <- table(newrf$foreign_worker, newrf$class)

# Convert table to proportions
prop_table_foreign <- prop.table(foreign_class_table, margin = 1)
heatmap_data <- melt(prop_table_foreign)

# Add labels for the proportions
heatmap_data$label <- scales::percent(heatmap_data$value)

# Heatmap with text
ggplot(heatmap_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), color = "black", size = 4) +  # Add proportion text
  scale_fill_gradient(low = "white", high = "steelblue", labels = scales::percent) +
  labs(
    title = "Heatmap of Proportions: Foreign Worker Status and Credit Class",
    x = "Credit Class",
    y = "Foreign Worker Status",
    fill = "Proportion"
  ) +
  theme_minimal()


# 4 : Association Plot
assoc(~ foreign_worker + class, data = newrf,
      shade = TRUE, legend = TRUE,
      main = "Association Plot: Foreign Worker Status and Credit Class")


# Proportions
foreign_prop_table <- prop.table(foreign_class_table, margin = 1)
print(foreign_prop_table)    # Proportions



#Analysis 2-3: How does the relationship between foreign worker status and credit class change when considering an additional financial factor, such as savings status?

# Cramér's V
cramers_v_foreign <- assocstats(foreign_class_table)$cramer
print(paste("Cramér's V:", round(cramers_v_foreign, 2)))


#Logistic regression
colnames(newrf)

newrf$CreditClass <- ifelse(newrf$class == "good", 1, 0)
table(newrf$CreditClass)

newrf$foreign_worker <- as.factor(newrf$foreign_worker)
table(newrf$foreign_worker)

log_model <- glm(CreditClass ~ foreign_worker, data = newrf, family = "binomial")
summary(log_model)

#-------------------------------------------------------------------------------
#Objective 3 : Analyze the interaction between employment status (employment) and foreign worker status on credit class

#Analysis 3-1: How do employment status and foreign worker status interact to influence credit class?

# 1 : BAR CHART NUMBER 1
interaction_summary <- newrf %>%
  group_by(employment, foreign_worker, class) %>%
  summarise(count = n(), .groups = "drop")
interaction_summary

ggplot(interaction_summary, aes(x = employment, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ foreign_worker) +
  labs(
    title = "Interaction Between Employment Status and Foreign Worker Status on Credit Class",
    x = "Employment Status",
    y = "Count",
    fill = "Credit Class"
  ) +
  theme_minimal()


# 2 : BAR CHART NUMBER 2
interaction_sum <- newrf %>%
  group_by(employment, foreign_worker, class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(employment, foreign_worker) %>%
  mutate(proportion = count / sum(count))
interaction_sum

ggplot(interaction_sum, aes(x = employment, y = proportion, fill = class)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), size = 3) +
  facet_grid(~ foreign_worker, labeller = labeller(foreign_worker = c("0" = "No", "1" = "Yes"))) +
  labs(
    title = "Proportional Distribution of Credit Class by Employment and Foreign Worker Status",
    x = "Employment Status",
    y = "Proportion",
    fill = "Credit Class"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))



#Analysis 3-2: Does employment duration impact on the likelihood of "good" credit for local and foreign workers?
#3 : # Interaction plot
interaction_summary <- newrf %>%
  group_by(employment, foreign_worker, class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(employment, foreign_worker) %>%
  mutate(proportion = count / sum(count))

interaction_data <- interaction_summary %>%
  filter(class == "good")

ggplot(interaction_data, aes(x = employment, y = proportion, color = foreign_worker, group = foreign_worker)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Interaction Between Employment and Foreign Worker Status on Good Credit",
    x = "Employment Status",
    y = "Proportion of Good Credit",
    color = "Foreign Worker Status"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


# Two-Way Contingency Table
employment_foreign_class_table <- xtabs(~ employment + foreign_worker + class, data = newrf)
print(employment_foreign_class_table)  # Counts


#Analysis 3-3: How strong is the combined effect of employment status and foreign worker status on credit class?

# Logistic Regression
logistic_model <- glm(class ~ employment * foreign_worker, data = newrf, family = binomial)
summary(logistic_model)  # Coefficients and interaction effects
