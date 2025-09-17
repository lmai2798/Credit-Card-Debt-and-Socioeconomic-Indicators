# Credit-Card-Debt-and-Socioeconomic-Indicators
## 📌 Project Overview
This project examines the relationship between **credit card debt** and **socioeconomic indicators** across U.S. states, utilizing machine learning techniques.  

- Built a **balanced panel dataset (2022–2023)** by combining multiple datasets from:  
  - New York Federal Reserve (credit card debt per capita)  
  - Federal Reserve Economic Data (FRED): unemployment rate, CPI  
  - U.S. Census: median household income, educational attainment  
  - State population estimates  
- Preprocessed data through transformations, harmonization, and restructuring into panel format.  
- Designed a **forecasting experiment**: trained models on 2022 and tested on 2023 to evaluate out-of-sample accuracy.  

---

## 🛠️ Methodology
Three models were implemented in **R**:  

1. **Multiple Linear Regression** – baseline interpretable model.  
2. **Lasso Regression** – feature selection with regularization.  
3. **Random Forest** – non-linear, interaction-aware ensemble method.  

**Evaluation metric**: R² and prediction accuracy on 2023 test data.  

---

## 📊 Results
- **Random Forest** achieved the highest explanatory power (**R² = 0.76**).  
- **Key predictors**: median household income, education attainment, and unemployment rate.  
- **Less impactful**: population size and CPI.
  
---

## 📂 Repository Layout
- `data/` – raw & processed datasets with documentation.  
- `scripts/` – R scripts for preprocessing, modeling, and visualization.  
- `results/` – figures, tables, and model outputs.  
- `docs/` – project documentation.

  
