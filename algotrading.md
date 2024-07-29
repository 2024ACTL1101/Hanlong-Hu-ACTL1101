
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  if (previous_price == 0) {
    # On the first iteration, execute a buy transaction
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- amd_df$close[i] * share_size * -1
    amd_df$accumulated_shares[i] <- share_size
    accumulated_shares <- share_size
  } else if (i == nrow(amd_df)) {
    # On the last iteration, execute a sell transaction
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * accumulated_shares
  } else if (amd_df$close[i] < previous_price) {
    # Execute a buy transaction if the current closing price is lower than the previous day's price
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * share_size * -1
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
  }
  
  # Update the previous price to the current closing price
  previous_price <- amd_df$close[i]
  
  # Ensure that the 'costs_proceeds' column has a default value of 0 if no trade was executed
  if (is.na(amd_df$costs_proceeds[i])) {
    amd_df$costs_proceeds[i] <- 0
  }
}
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
start_date <- as.Date('2021/08/01')
end_date <- as.Date('2022/08/01')
amd_df <- amd_df[amd_df$date <= end_date & amd_df
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Initialize variables to store total investment, total profit/loss, and ROI
ROI <- 0
total_investment <- 0
total_profit_loss <- 0

# Loop through each row in the dataframe 'amd_df'
for (i in 1:nrow(amd_df)) {
  # Accumulate the total profit or loss from the 'costs_proceeds' column
  total_profit_loss = total_profit_loss + amd_df$costs_proceeds[i]
  
  # Check if the current 'costs_proceeds' value is negative (indicating a buy transaction)
  if (amd_df$costs_proceeds[i] < 0) {
    # Add the absolute value of the negative 'costs_proceeds' to the total investment
    total_investment = total_investment - amd_df$costs_proceeds[i]
  }
}

# Calculate ROI using the formula: ROI = (Total Profit or Loss / Total Capital Invested) * 100
ROI <- (total_profit_loss / total_investment) * 100

# Output the results
cat("Total Profit/Loss:", total_profit_loss, "\n")
cat("Total Investment:", total_investment, "\n")
cat("ROI:", ROI, "%", "\n")
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# This is implementing the profit taking Mechanism
# Initialize columns for trade type, cost/proceeds, and accumulated 
# shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking
amd_df$average_purchase_price <- NA 
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
# money currently spent (used to find average purchase price)
invested_capital <- 0

for (i in 1:nrow(amd_df)) {
  if (previous_price == 0) {
    # First trade: Buy initial shares
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- amd_df$close[i] * share_size * -1
    accumulated_shares <- share_size
    invested_capital <- amd_df$close[i] * accumulated_shares
    amd_df$accumulated_shares[i] <- accumulated_shares
    amd_df$average_purchase_price[i] <- invested_capital / accumulated_shares
  } else if (i == nrow(amd_df)) {
    # Last trade: Sell all remaining shares
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * accumulated_shares
    amd_df$accumulated_shares[i] <- 0  # Selling all shares
  } else if (amd_df$average_purchase_price[i-1] * 1.2 <= amd_df$close[i]) {
    # Stop loss condition: Sell half of the shares if the price increases by 
    # 20% of the average purchase price
    amd_df$trade_type[i] <- 'sell'
    shares_to_sell <- accumulated_shares / 2
    amd_df$costs_proceeds[i] <- shares_to_sell * amd_df$close[i]
    accumulated_shares <- accumulated_shares - shares_to_sell
    amd_df$accumulated_shares[i] <- accumulated_shares
    invested_capital <- invested_capital - 
      (shares_to_sell * amd_df$average_purchase_price[i-1])
    amd_df$average_purchase_price[i] <- ifelse(
      accumulated_shares == 0, 0, invested_capital / accumulated_shares)
  } else if (amd_df$close[i] < previous_price) {
    # Buying condition: Buy more shares if today's closing price is 
    # less than yesterday's
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * share_size * -1
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
    invested_capital <- invested_capital + amd_df$close[i] * share_size
    amd_df$average_purchase_price[i] <- invested_capital / accumulated_shares
  } else {
    # No trade condition: Carry forward the previous average purchase price
    amd_df$average_purchase_price[i] <- amd_df$average_purchase_price[i-1]
  }
  
  # Update the previous price for the next iteration
  previous_price <- amd_df$close[i]
  
  # Ensure all values in the costs_proceeds column are numeric
  if (is.na(amd_df$costs_proceeds[i])) {
    amd_df$costs_proceeds[i] <- 0
  }
}
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# Chosen period from 01/08/2021 to 01/08/2022
# Results of first investment strategy
# Total Profit/Loss: -179912 
# Total Investment: 1438052 
# ROI: -12.51081 %
# Results of 
# Total Profit/Loss: 72242.33 
# Total Investment: 1051409 
# ROI: 6.871001 % 
```

My P/L and ROI improved over my chosen time period using the profit taking implementation was seen to be more effective than the first strategy. Following a strong growth period in 2020 AMS earnings surged 53% in the 2020 financial year to 3.2 billion and increased its earnings per share by 63% to 0.53.Following the growth We significantly accelerated our business in 2020, delivering record annual revenue while expanding gross margin and more than doubling net income from 2019,” AMD CEO Lisa Su said in a press release. “Our 2021 financial outlook highlights the strength of our product portfolio and robust demand for high-performance computing across the PC, gaming, and data center markets.” But followed with a drop in share price, showing that investors were dissapointed with the company’s performance after its dominant position over Intel previously. This lead on to a downhill trend that ended up losing money in the investment if we implemented the first strategy as it would have to sell all its stock at a low point due to my set period. Conversely using the profit taking impelmentation, any increases in price, if high enough will cause a selling point to migigate any losses as well as possibly turning a profit, hence resulting in better performance over the first method.




