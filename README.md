# University Student Equity and Retention Modeling

This project explores student diversity and persistence in an American university program with descriptive statistics and logistic regression analysis. The final report also includes interactive dashboards built in Tableau and a static visual overview of important descriptive statistics. You can view the final report in my [portfolio](https://ashleyrsanders.com/portfolio/university-student-diversity-persistence-analysis).

## 📊 Project Goals

- Explore trends in minor completion across demographic subgroups
- Identify equity gaps in student persistence
- Evaluate interaction effects (e.g., intersectional identities) using logistic regression
- Generate actionable insights and recommendations for program administrators
- Create shareable visual outputs for web presentation (e.g., HTML tables, ggplot charts)

## 🔍 Key Features

- **Descriptive Statistics & Visualizations**: Completion rates across demographic variables
- **Chi-Square Tests**: Association between race/ethnicity and transfer status
- **Logistic Regression Modeling**: Estimate likelihood of completion and explore main and interaction effects
- **Predicted Probabilities**: Calculate and highlight subgroup outcomes
- **HTML Tables**: Polished output for embedding in web reports

## 📦 Packages Used

- `tidyverse` / `dplyr` / `ggplot2`: Data manipulation and plotting
- `readxl`: Excel import
- `gt`: Generate HTML tables
- `ggeffects`: Extract predicted probabilities
- `broom`: Tidy model outputs
- `here`: Manage project paths
- `chisq.posthoc.test`: Run post hoc chi-square tests

## 🔒 Data Privacy

This repository does not include the original student data files because they contain unique identifiers (UCLA ID numbers) that are considered personally identifiable information (PII), even without names or email addresses.

To maintain student privacy and comply with institutional data protection policies (e.g., FERPA), the raw data is excluded from this public repo.

### 🧪 Reproducibility Without Data

To make the code reusable:
- All data paths are handled with the here() package, so you can easily plug in your own dataset with similar structure.
- The R script is modular and well-commented, so you can adapt it for other institutions or contexts using your own anonymized student data.

## 📝 Notes

- The analysis focuses on declared DH minors and does not include students from the College of Engineering who selected DH as their Technical Breadth Area.
- The project intentionally filters out uncommon or missing combinations of demographic traits for modeling reliability.
- Reminder: Visualizations for this project also appear on [the final published report](https://ashleyrsanders.com/portfolio/university-student-diversity-persistence-analysis/).

## 📣 Attribution

Analysis and code by Ashley Sanders. Data provided by the UCLA Digital Humanities Program (2011–2024).

Feel free to fork or adapt this project with attribution.
