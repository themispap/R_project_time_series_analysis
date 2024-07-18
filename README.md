# Time Series Analysis and Forecasting: Technical Olympic S.A. Stock

## Project Description

This project involves a comprehensive time series analysis and forecasting endeavor focused on the closing values of Technical Olympic S.A. stock. The dataset, representing historical stock prices, is loaded and preprocessed using the R programming language. The analysis encompasses various stages, including:

1. **Time Series Decomposition**: Uncovering trends, seasonality, and residuals.
2. **Polynomial Approach**: Exploring the fit of polynomial models of different degrees.
3. **Box-Cox Transformation**: Stabilizing variance.
4. **Box-Jenkins Method**: Using the Augmented Dickey-Fuller (ADF) and KPSS tests to address stationarity and identify optimal parameters for ARIMA modeling.
5. **ARIMA Modeling**: Fitting ARIMA models, conducting diagnostic checks of residuals, and generating forecasts for future stock closing values.

Through these analytical steps, the aim is to provide insights into the underlying patterns of Technical Olympic S.A. stock prices and to make informed predictions for future market behavior.

## Dataset

The dataset contains historical stock prices for Technical Olympic S.A. It includes the following columns:
- `date`: The date of the stock price observation.
- `close`: The closing value of the stock on the given date.

## Tools and Libraries Used

- **R**: The programming language used for the project.
- **Libraries**: The following R libraries are used:
  - `forecast`
  - `tseries`
  - `ggplot2`
  - `dplyr`
  - `readr`

## Analysis Steps

1. **Loading the Dataset**: Importing the dataset using `readr`.
2. **Preprocessing**: Handling missing values and formatting the date column.
3. **Time Series Decomposition**: Using the `stl` function to decompose the time series into trend, seasonal, and residual components.
4. **Polynomial Approach**: Fitting polynomial models of various degrees and evaluating their performance.
5. **Box-Cox Transformation**: Applying the Box-Cox transformation to stabilize variance.
6. **Stationarity Tests**: Conducting ADF and KPSS tests to check for stationarity.
7. **ARIMA Modeling**: Identifying optimal ARIMA parameters, fitting the model, conducting diagnostic checks, and generating forecasts.
8. **Visualization**: Creating plots to visualize the results of the analysis.

## How to Run the Project

1. **Clone the Repository**: Clone this repository to your local machine using:
   ```sh
   git clone https://github.com/your-username/technical-olympic-stock-analysis.git

2. **Navigate to the Project Directory**:
   ```sh
   cd technical-olympic-stock-analysis
   ```

3. **Install Required Libraries**: Install the required R libraries using:
   ```r
   install.packages(c("forecast", "tseries", "ggplot2", "dplyr", "readr"))
   ```

4. **Open the R Script**: Open and run the time_series_analysis.R script in your R environment.

## Results

### The project results include:

- **Decomposed Time Series Components**: Trend, seasonality, and residuals.
- **Polynomial Model Fits**: Performance evaluation of polynomial models.
- **ARIMA Model**: Fitted ARIMA model with diagnostic checks.
- **Forecasts**: Predictions for future stock closing values.

## Repository Structure
```kotlin
technical-olympic-stock-analysis/
│
├── data/
│   └── technical_olympic_stock.csv
│
├── results/
│   ├── decomposed_time_series.png
│   ├── polynomial_fit.png
│   ├── arima_diagnostics.png
│   ├── forecasts.png
│
├── scripts/
│   └── time_series_analysis.R
│
├── README.md
└── requirements.txt
```
