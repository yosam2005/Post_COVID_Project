# Post-COVID Economic Panel Analysis (2015–2023)
### A Study Using World Bank and IMF Data in R

## 📘 Overview
This project analyzes how key macroeconomic indicators influenced inflation (CPI) across 121 countries between 2015 and 2023 using combined World Bank and IMF data.  
It focuses on the post-COVID recovery period, integrating data cleaning, visualization, and fixed-effects panel regression in **R**.

## 🧮 Methods
- Data collected from World Bank and IMF
- Cleaned and reshaped to long panel format (Country–Year)
- Exploratory analysis and visualization
- Fixed-effects regression with `plm()` to model CPI against GDP, exports, unemployment, and government expenditure
- Model diagnostics: residual, histogram, and QQ plots

## 📊 Key Findings
- GDP growth slightly **reduced inflation** after COVID.
- Government spending helped **stabilize prices** through fiscal stimulus.
- Unemployment showed expected **negative relation** to inflation.
- Inflation spiked in 2020–2022 and **stabilized post-2022**.

## 🛠️ Tools
R | tidyverse | plm | ggplot2 | World Bank API | IMF Data

## 📁 Files
- `Post_COVID_Project.Rmd` — Main R Markdown analysis  
- `WBD_IMF.data/` — Raw and cleaned data  
- `README.md` — Project documentation  

## 📈 Conclusion
This work demonstrates how panel regression helps explain cross-country macroeconomic patterns, showing that post-COVID inflation depends on growth, labor markets, and fiscal policy.

---

*Author: Yoni | Created: 2025-11-05*
