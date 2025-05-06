🚨 Credit Risk Classification: Data Preprocessing and Analysis 🚨
📋 Overview
This project focuses on preprocessing and analyzing a credit risk classification dataset to prepare it for machine learning modeling and statistical analysis. The R script performs comprehensive data cleaning, missing value imputation, feature engineering, and exploratory data analysis (EDA) to uncover relationships between key variables and credit risk.

📑 Table of Contents
📦 Libraries Used

📊 Dataset

🔄 Data Processing Pipeline

1️⃣ Initial Cleaning

2️⃣ Missing Value Treatment

3️⃣ Feature Engineering

4️⃣ Data Export

🔍 Key Analyses

Analysis 1: Property Ownership vs. Credit Risk

Analysis 2: Foreign Worker Status vs. Credit Risk

Analysis 3: Employment-Foreign Worker Interaction

⚖️ Findings

🌟 Strengths

🚀 Potential Improvements

💡 Conclusion

🛠️ Libraries Used
The analysis leverages the following R packages:

Data Manipulation: dplyr, plyr, janitor, tidyverse, reshape2

Data Visualization: ggplot2, lattice, vcd, GGally, colorspace

Missing Data Handling: VIM, mice

Statistical Analysis: car, carData, effects

Machine Learning: caret, class

📊 Dataset
The dataset, credit_risk_classification.csv, contains features related to credit applicants, including:

Demographic Information: Age, gender, employment status

Financial Attributes: Credit amount, savings status, property ownership

Loan Characteristics: Purpose, duration

Target Variable: class (Credit risk classification: good or bad)

🔄 Data Processing Pipeline
1️⃣ Initial Cleaning
Loaded and inspected the raw data

Renamed columns for clarity (e.g., personal_status → gender)

Rounded numerical variables to appropriate precision

Standardized categorical values (e.g., "unemployed" → "0")

2️⃣ Missing Value Treatment
Visualized missing data patterns using DataExplorer::plot_missing()

Applied multiple imputation strategies:

MICE (Multivariate Imputation by Chained Equations) for ordered categoricals

KNN imputation for non-ordered categoricals

Linear regression imputation for continuous variables

Logistic regression imputation for binary variables

3️⃣ Feature Engineering
Converted variables to appropriate types:

Binary encoding for gender, telephone ownership, and foreign worker status

Ordered factors for variables with natural hierarchies

Non-ordered factors for nominal categoricals

4️⃣ Data Export
Saved the cleaned dataset as credit_risk_classification_cleaned.csv

🔍 Key Analyses
📊 Analysis 1: Property Ownership vs. Credit Risk
Visualizations: Bar charts, mosaic plots, heatmaps, association plots

Statistical Tests:

Chi-Square Test of Independence

Cramér's V for association strength

Findings: Property ownership is positively associated with being classified as "good" credit.

📊 Analysis 2: Foreign Worker Status vs. Credit Risk
Visualizations: Bar charts, mosaic plots

Statistical Tests:

Chi-Square Test

Logistic regression model

Findings: Foreign workers tend to be classified as "bad" credit more frequently compared to locals.

📊 Analysis 3: Employment-Foreign Worker Interaction
Visualizations: Stacked bar charts (counts and proportions)

Analysis: Evaluated conditional dependencies between employment status, foreign worker status, and credit risk

Findings: The interaction between employment and foreign worker status significantly affects the credit risk classification, with varying patterns across different employment groups.

⚖️ Findings
Property Magnitude: Significant positive association with "good" credit status.

Foreign Worker Status: Foreign workers are more likely to be classified as "bad" credit.

Employment-Foreign Worker Interaction: The combination of employment and foreign worker status influences credit classification, with some interesting conditional dependencies.

🌟 Strengths
Comprehensive Data Preparation: Rigorous handling of missing data and appropriate variable transformations.

Diverse Visualization Techniques: Multiple plot types used to reveal different aspects of relationships.

Robust Statistical Framework: Proper use of Chi-Square tests, supplemented with association measures like Cramér's V.

Modeling Foundation: Logistic regression implementation provides a basis for more complex predictive modeling.

🚀 Potential Improvements
Enhanced Missing Data Documentation: A more explicit description of missing data handling decisions.

Advanced Modeling: Incorporation of interaction terms in regression models for a more nuanced understanding of credit risk.

Results Interpretation: A more expanded discussion of statistical test outcomes and their practical implications.

Feature Importance: Preliminary analysis of variable importance for credit risk prediction.

💡 Conclusion
This project demonstrates a thorough approach to credit risk data preparation and analysis, combining careful data cleaning with insightful exploratory analysis. The cleaned dataset and analysis results provide a strong foundation for building predictive models and making data-driven credit risk assessments.
