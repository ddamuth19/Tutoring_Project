# Tutoring Data Analysis (Capstone Project)

This project investigates the impact of **embedded tutoring programs** on student course outcomes at SUNY Oswego. Using anonymized institutional data, I built and analyzed logistic regression models to determine whether final course grades are influenced by the presence of embedded tutors, tutoring hours, and student characteristics.  

The research provides insight into the efficacy of tutoring interventions and their potential to bridge achievement gaps, particularly in mathematics courses.

### Skills Applied

- Logistic regression modeling and interpretation in R  

- Data cleaning, preprocessing, and transformation of institutional data  

- Model evaluation using ROC curves, accuracy, sensitivity, and specificity  

- Statistical testing (Likelihood Ratio tests, VIF analysis)  

- Visualization of tutoring effects and outcome distributions  

---

### Dataset
Tutoring Records Dataset (SUNY Oswego, 2024) 

- Fully anonymized institutional data  

- Includes student demographics, tutoring hours, embedded tutor status, and final course grades  

- Focused on mathematics (MAT) courses with 431 consolidated records after preprocessing  

---

### Methodology

- Cleaned and prepared raw tutoring datasets, consolidating by student and course  

- Defined binary outcome variable: Pass (≥C-/P/S) vs. Fail (W/I/F/U)

- Built logistic regression models across multiple math courses (MAT158, MAT106, MAT220)  

- Evaluated predictors including:
  - Embedded tutor status  
  - Tutoring duration (hours)  
  - Course retakes  

- Converted log-odds to interpretable percentage changes in odds of success  

- Evaluated model predictive power via:

  - ROC curves and optimal cutpoints  

  - Confusion matrices  

  - Accuracy, sensitivity, and specificity metrics  

---

### Tools & Technologies
- Programming Language: R  

- Libraries: `dplyr`, `tidyverse`, `caret`, `ROCR`, `broom`, `car`, `psych`, `DescTools`  

- Software: RStudio

---

### Results & Evaluation

- **MAT158:** Accuracy ~62%, sensitivity 69%, specificity 52%  

- **MAT106:** Accuracy ~65%, sensitivity 60%, specificity 75%  

- **MAT220:** Accuracy ~79%, sensitivity 82%, specificity 70% (only model with significant predictor: retakes)  

- Predictors such as course retakes and tutoring hours showed consistent positive effects, while embedded tutor status had mixed results across courses.  

---

### Challenges & Learning

- Small and imbalanced sample sizes limited statistical significance  

- Institutional data lacks true randomization, introducing self-selection bias  

- Explored model assumptions (sample size, predictor significance) and potential alternative methods (effect sizes, normalized gain scores)  

- Gained deeper expertise in applying logistic regression to real-world, imperfect datasets

---

### Contribution
Author: Dawson Damuth (Capstone Research, MAT 499)  
Advisor: Dr. Mark Baker  

- My Role:
  - Data collection and preprocessing pipeline

  - Designed and implemented logistic regression models in R  

  - Conducted statistical testing, model validation, and visualizations  

  - Interpreted results in the context of tutoring program efficacy  

---

Reference: [Theobald, R., & Freeman, S. (2017). *Is It the Intervention or the Students? Using Linear Regression to Control for Student Characteristics in Undergraduate STEM Education Research.* CBE-Life Sciences Education.](www.lifescied.org/doi/10.1187/cbe-13-07-0136)

