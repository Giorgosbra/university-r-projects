# University R Projects

A collection of academic projects developed in R as part of my university coursework in Business Administration and Information Systems.

The projects cover time series forecasting, retail data analysis, data wrangling, visualization, regression modelling, and web harvesting.

---

## 1. Time Series Forecasting

Demand forecasting project based on retail sales data at SKU level.

The workflow includes:

- Preparation of complete product time series
- Feature engineering using date-related variables
- Training and validation split
- Application of multiple forecasting models
- Parallel model execution
- Model accuracy evaluation

Forecasting methods used include:

- ETS
- ARIMA
- TSLM
- Croston
- Croston variants
- NNETAR

**Main script:**  
`01-time-series-forecasting/time_series_forecasting.R`

---

## 2. Retail Data Wrangling & Visualization

Exploratory Data Analysis of retail transaction data across multiple stores.

The project focuses on cleaning, transforming, analysing, and visualizing retail sales data using the tidyverse ecosystem.

The analysis includes:

- Data cleaning and consistency checks
- Product category transformation
- Daily revenue analysis
- Weekly revenue visualization by store
- Identification of top-selling products
- Basket composition analysis
- Product category probability analysis
- Identification of products shared across stores
- Analysis of the relationship between basket variety and basket value
- Regression analysis related to food products and basket value

**Main files:**

- `02-retail-data-analysis/retail_data_analysis.R`
- `02-retail-data-analysis/retail_data_analysis.Rmd`

---

## 3. IMDb Data Wrangling & Regression

Data wrangling, exploratory analysis, data integration, and regression modelling using IMDb movie data.

The project combines multiple IMDb datasets containing information about movies, ratings, contributors, directors, writers, actors, and other movie-related characteristics.

The analysis includes:

- Cleaning and transformation of IMDb datasets
- Exploratory Data Analysis
- Movie-level data integration
- Contributor-level analysis
- Feature engineering
- Genre-based dummy variables
- Multivariate regression modelling
- Comparison between IMDb and TMDB ratings

The regression model uses IMDb average rating as the dependent variable and multiple movie-related characteristics as explanatory variables.

**Main files:**

- `03-imdb-wrangling-regression/imdb_analysis.R`
- `03-imdb-wrangling-regression/imdb_analysis.Rmd`

---

## 4. IMDb Web Harvesting & Regression

Web harvesting project focused on collecting movie data directly from IMDb and using the collected information for regression analysis.

The workflow starts from an IMDb movie and recursively discovers related movies through the **More Like This** section.

The project is divided into three stages:

### Data Collection

Movie data is collected using R web scraping tools.

Collected information includes:

- Movie title
- Release year
- Runtime
- IMDb rating
- Number of votes
- Popularity
- Metascore
- User reviews
- Critic reviews
- Genres
- Review topics
- Budget
- Worldwide gross
- Rating distribution from 1 to 10

### Data Cleaning & Transformation

The collected dataset is cleaned and prepared for statistical modelling.

This stage includes:

- Duplicate removal
- Missing value handling
- Data type conversion
- Genre one-hot encoding
- Selection of variables for regression

### Regression Analysis

A multiple linear regression model is developed to predict IMDb rating.

The modelling workflow includes:

- Full linear regression
- Stepwise variable selection
- 10-fold cross-validation using `caret`
- RMSE and R-squared evaluation

**Main scripts:**

- `04-imdb-web-scraping/0_get_data.R`
- `04-imdb-web-scraping/1_data_clean_and_transform.R`
- `04-imdb-web-scraping/2_linear_regression.R`

---

## Repository Structure

```text
university-r-projects/
│
├── 01-time-series-forecasting/
│   ├── time_series_forecasting.R
│   └── data/
│
├── 02-retail-data-analysis/
│   ├── retail_data_analysis.R
│   ├── retail_data_analysis.Rmd
│   └── data/
│
├── 03-imdb-wrangling-regression/
│   ├── imdb_analysis.R
│   ├── imdb_analysis.Rmd
│   └── data/
│
├── 04-imdb-web-scraping/
│   ├── 0_get_data.R
│   ├── 1_data_clean_and_transform.R
│   ├── 2_linear_regression.R
│   ├── regression_results.txt
│   └── data/
│
├── .gitattributes
├── .gitignore
└── README.md
```

---

## Technologies & R Packages

The projects use a range of R packages and tools, including:

- R
- tidyverse
- dplyr
- tidyr
- ggplot2
- lubridate
- forecast
- fable
- tsibble
- furrr
- rvest
- httr
- jsonlite
- stringr
- caret
- jtools

---

## Large Data Files

Some datasets used in these projects exceed GitHub's standard file size limits and are therefore stored using **Git Large File Storage (Git LFS)**.

To clone the repository and download the LFS files:

```bash
git clone https://github.com/Giorgosbra/university-r-projects.git
cd university-r-projects
git lfs install
git lfs pull
```

---

## Purpose

These projects were developed as part of university assignments and are published here as a collection of practical work in data analysis and statistical modelling using R.

They demonstrate experience with:

- Data cleaning and transformation
- Exploratory Data Analysis
- Data visualization
- Time series forecasting
- Feature engineering
- Regression modelling
- Model evaluation
- Web scraping and data collection