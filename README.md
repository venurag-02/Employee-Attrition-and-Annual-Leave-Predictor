# 📊 Employee Attrition & Leave Analytics Dashboard

A reactive **R Shiny** web application that leverages machine learning models to project multi-quarter employee attrition risks and annual leave trends.

------------------------------------------------------------------------

## 📌 Overview

This interactive dashboard enables HR teams and operational managers to evaluate retention risk factors for individual employees. By adjusting key workplace, satisfaction, and demographic variables, the app calculates predictive outputs across a four-quarter horizon to support proactive talent management.

------------------------------------------------------------------------

## 🛠️️ Key Features

- **Dynamic Risk Assessment**: Calculates baseline attrition risk using a trained **Logistic Regression** model and categorizes outputs into **High**, **Medium**, or **Low Risk** tiers.
- **Leave Forecasting**: Estimates expected quarterly leave usage and leave-taking probabilities using a **Poisson Count Model**.
- **4-Quarter Trend Projections**: Visualizes projected attrition and leave probabilities across upcoming quarters ($Q_1$ through $Q_4$) via interactive donut charts.
- **Scenario Logging & Export**: Saves configured employee profiles during a session and exports history directly to a structured `.csv` file.
- **Printable Assessment Reports**: Generates a clean, print-friendly HTML summary table ready for PDF export or manager review.
- **Multiple ML Models**: **Logistic Regression**, **XGBoost Model**, **Poisson Count Model** and **10,000 Monte Carlo Simulations**. All the model structures and their respective outcomes available.

------------------------------------------------------------------------

## 📁 Repository Structure

``` text
.                      
├── models/
│   ├── log_fit.rds             # Serialized Logistic Regression model
│   ├── poisson_fit.rds         # Serialized Poisson Count model
│   └── xgb_fit.rds             # Serialized XGBoost model
│
├── outputs/
│   ├── final_analytics_dataset.rds    # Baseline schema & factor reference dataset
│   └── ...                            # Additional model metrics & simulation outputs
│
├── scripts_2026/
│   ├── EAP_app.R                                   # Main Shiny application entry point (UI & Server)
│   └── scripts_01_build_models_and_simulations.R   # Model building & simulation pipelines
│
├── README.md                   # Project documentation
├── report.pdf                  # Complete report detailing OR and IRR from statistical models
└── report.qmd                  # Quarto source script for report.pdf
```

------------------------------------------------------------------------

## 🚀 Getting Started

### Prerequisites

Ensure you have R (version 4.0 or higher) installed. The following R packages are required:

```{r}

install.packages(c(
  "shiny",
  "bslib",
  "bsicons",
  "dplyr",
  "ggplot2",
  "DT",
  "readr",
  "shinyjs",
  "parsnip",
  "workflows"
))

```

### Running the Application Locally

1.  Clone the repository:

```{bash}

git clone https://github.com/venurag-02/Employee-Attrition-and-Annual-Leave-Predictor.git
cd Employee-Attrition-and-Annual-Leave-Predictor

```

2.  Launch the application in RStudio or Terminal:

```{r}

shiny::runApp("scripts_2026/EAP_app.R")

```

------------------------------------------------------------------------

## ⚙️ Model Framework

- **Attrition Model**: Logistic Regression trained on historical HR features (`overtime`, `job_satisfaction`, `income`, `years_at_company`, etc.) predicting binary attrition probability.

- **Leave Model**: Poisson Regression modeling expected quarterly leave days based on employee workload and tenure factors.

- **Risk Simulations**: 10,000 Monte Carlo iterations evaluating model reliability and metric variances.

------------------------------------------------------------------------

## 📄 License

Distributed under the MIT License. See `LICENSE` for details.

