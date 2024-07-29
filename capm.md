
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
df <- df %>%
  mutate(
    AMD_return = ((AMD - lag(AMD)) / lag(AMD)),
    GSPC_return = ((GSPC - lag(GSPC))/lag(GSPC))
  ) %>%
  na.omit
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
df <- df %>%
  mutate(
    DRFrate = (1 + (RF/100))^(1/360) - 1
  )
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
df <- df %>%
  na.omit %>% #there is no returns on first day
  mutate(
    AMD_excess_returns = (AMD_return - DRFrate),
    GSPC_excess_returns = (GSPC_return - DRFrate)
  )
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
model <- lm(AMD_excess_returns ~ GSPC_excess_returns, data = df)
summary(model)
anova(model)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
My beta value is 1.5699987 which shows that the AMD stock is more volatile than the market(S&P 500 returns) as it has a coefficient greater than 1.


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
# Scatter plot of the data
ggplot(df, aes(x = GSPC_excess_returns, y = AMD_excess_returns)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(x = "AMD_excess_returns", y = "GSPC_excess_returns", title = "CAPM plot")
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
# Length of S&P500 excess returns column
n <- length(df$GSPC_excess_returns)
# 90% confidence level, degrees of freedom equals to n-1-1 or (n-2) 
alpha <- 0.10
t_value <- qt(1 - alpha/2, df = n - 2)
#estimated returns: expected return = intercept + coefficient*(0.133 - 0.05) + 0.05
estimated_return <- 0.0011041 + 1.5699987*(0.133 - 0.05) + 0.05
mean_GSPC = mean(df$GSPC_excess_returns)
#Standard error of the estimate (square root of MSE)
se <- sqrt(sum(residuals(model)^2)/(n-2))
SST <- sum((df$GSPC_excess_returns - mean_GSPC)^2)

# Calculate the standard error of the forecast, 
# let X_f = 0.133 divided by sqrt(252)
X_f <- 0.133/sqrt(252)
sf <- se * sqrt(1 + 1/n + (X_f - mean_GSPC)^2 / SST)

# Then to find the annual standard error for 
# prediction for the forecast times sf by sqrt(252)
asf <- sf * sqrt(252)

# Prediction interval 
lower_bound <- estimated_return - t_value* asf
upper_bound <- estimated_return + t_value * asf
cat("The estimated returns for AMD given an expected return of %13.3 for S&P 500 is",
    round (estimated_return*100, 2),"%\n")
cat("The 90% prediction interval is [", round(lower_bound, 2), ",", 
    round(upper_bound, 2), "]\n")
```
