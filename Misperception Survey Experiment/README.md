# Survey Incentives and Belief Measurement: An Experimental Analysis

**Author:** Md Johirul Islam

**Context:** Methodological Experiment embedded in a survey in Rural Bangladesh

**Paper Title:** *Belief Measurement: The Role of Survey Incentives*

**Note:** I completed this project under the supervision of Atiya Rahman; however, all coding and written analysis are solely my own. Because the paper is currently under review and includes proprietary organizational data, the dataset cannot be shared at this time. A sample dataset can be provided upon request.

## 📄 Project Overview

This repository contains the replication code for the paper **"Belief Measurement: The Role of Survey Incentives."** The study investigates a methodological question central to development economics: **Do monetary incentives improve the accuracy of reported beliefs regarding sensitive social norms?**\



We conducted a randomized survey experiment with **2,914 fathers** in rural Bangladesh. The treatment group was informed of a financial reward for accurately guessing the beliefs of their peers regarding child marriage and girls' education. The study tests whether this incentive reduces "social desirability bias" and reveals deeper misperceptions.

**Key Findings:**

* Monetary incentives lead to limited and statistically fragile improvements in eliciting peer perceptions.

* While initial OLS results showed some significant effects, these disappeared after applying rigorous corrections for multiple hypothesis testing (Anderson’s q-values and Romano-Wolf FWER).

* **Conclusion:** Incentives may not be a cost-effective tool for reducing bias in social norm surveys.

## 💻 Code Structure (`.do` file)

The Stata do-file (`misperception_survey_analysis.do`) performs the end-to-end workflow for the paper:

1. **Data Cleaning:**

   * Winsorization of continuous variables (e.g., age).

   * Categorization of demographic variables (Poverty, Education, Age cohorts).

   * Construction of the "Misperception Gap" outcome variable ($Guess - Actual$).

2. **Descriptive Analysis:**

   * Balance tests using joint orthogonality checks (`reghdfe`).

   * Automated generation of Balance Tables in LaTeX.

3. **Econometric Analysis:**

   * **OLS Regressions:** Estimating treatment effects with District and Enumerator Fixed Effects.

   * **Heterogeneity Analysis:** Interaction models to test effects across Poverty, Age, and Education levels.

   * **Robustness Checks:** Coefficient plots (`coefplot`) comparing models with and without fixed effects.

## 🔧 Technical Highlight: The Challenge of Multiple Hypothesis Testing

A major technical challenge in this project was ensuring statistical rigor through **Multiple Hypothesis Testing (MHT)** corrections. Standard Stata table commands (`esttab` or `outreg2`) do not natively support the integration of advanced MHT corrections into a single regression table.

To solve this, I implemented a custom solution using `texdoc` to build complex LaTeX tables programmatically from within Stata.

### The Workflow Implemented:

1. **Raw Estimation:** Run main regressions and store coefficients/p-values.

2. **Anderson’s Sharpened q-values (BKY 2006):**

   * I manually coded the Benjamini-Krieger-Yekutieli (2006) step-down procedure in Stata.

   * This involved reshaping the results data, calculating ranks, and running a loop to adjust the False Discovery Rate (FDR) dynamically.

3. **Romano-Wolf FWER:**

   * Used the `rwolf2` command with 3,000 bootstrap replications to control the Family-Wise Error Rate (FWER).

4. **Custom Table Generation:**

   * Instead of standard outputs, I used `texdoc` to write raw LaTeX code line-by-line.

   * This allowed me to append a custom row at the bottom of the regression tables containing the **\[q-value, FWER p-value\]** for each coefficient, providing a transparent view of statistical significance before and after correction.

## 📊 Outputs

The code automatically generates:

* **`balance_table.tex`**: Randomization balance checks.

* **`new_overall.tex`**: Main treatment effects with MHT corrections.

* **`new_hetero_*.tex`**: Heterogeneity analysis tables (Poverty, Age, Education).

* **Coefficient Plots:** Visual robustness checks saved as PDFs.

## 🛠️ Requirements

* **Software:** Stata 17 or higher.

* **User-written packages:**

  * `reghdfe` (High-dimensional fixed effects)

  * `ftools` (Faster Stata commands)

  * `winsor2` (Winsorization)

  * `fre` (Frequency tables)

  * `texdoc` (LaTeX interaction)

  * `coefplot` (Visualizing coefficients)

  * `rwolf2` (Romano-Wolf correction)

