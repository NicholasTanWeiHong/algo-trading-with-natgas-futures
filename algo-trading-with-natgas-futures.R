## ------------------------------------------------------------------------
library(forecast)
library(IKTrading)
library(knitr)
library(PerformanceAnalytics)
library(Quandl)
library(quantmod)
library(quantstrat)
library(tidyverse)
library(TTR)


## ------------------------------------------------------------------------
# Import Henry Hub Natural Gas Front Month Contract Data from Quandl
Quandl.api_key("rn2xyN_hG9XfxN_9ibFJ")
natgas <- Quandl("CHRIS/CME_NG1")

# Print the structure of the Natural Gas Data
glimpse(natgas)

# Print the first 6 rows of the data.frame
head(natgas)

# Print the last 6 rows of the data.frame
tail(natgas)

## ------------------------------------------------------------------------
# Convert the data.frame object to an xts object
natgas_xts <- xts(x = natgas[, -1], order.by = natgas$Date)

# Examine the natgas_xts object
glimpse(natgas_xts)

## ------------------------------------------------------------------------
# Define a standard theme for each visualization
theme_report <- function() {
  theme_minimal() +
  theme(
    text = element_text(family = "serif", color = "gray25"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = "gray30"),
    plot.background = element_rect(fill = "gray95"),
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  )
}

# Plot the historical data for HH Futures with rectanges identifying spikes
autoplot(object = natgas_xts$Settle) +
  labs(
    x = "Date",
    y = "Price (USD/MMBtu)",
    title = "Time Series Plot of Henry Hub Physical Futures", 
    subtitle = "Henry Hub Natural Gas Prices, 1990 to 2019",
    caption = "Data source: Quandl Wiki Continuous Futures") +
  theme_report()


## ------------------------------------------------------------------------
# Compute the daily returns from HH Natural Gas Physical Futures
natgas_returns <- Return.calculate(prices = natgas_xts$Settle)

# Examine the natgas_returns xts objects
tail(natgas_returns)

## ------------------------------------------------------------------------
# Find the largest up move and down move in HH Natural Gas Futures
largest_up_move <- max(natgas_returns$Settle, na.rm = TRUE)
largest_down_move <- min(natgas_returns$Settle, na.rm = TRUE)

# Subset the xts object to return the occurrences with the largest movements
natgas_returns[which(natgas_returns$Settle == largest_up_move)]
natgas_returns[which(natgas_returns$Settle == largest_down_move)]


## ------------------------------------------------------------------------
# Plot a histogram of Natural Gas returns
ggplot(data = natgas_returns, mapping = aes(x = Settle)) +
  geom_histogram(alpha = 0.75, binwidth = .01, col = "black") +
  labs(
    x = "Returns",
    y = "Frequency",
    title = "Histogram of Henry Hub Natural Gas Returns", 
    subtitle = "Frequency Distribution of Returns, 1990 to 2019",
    caption = "Data source: Quandl, Wiki Continuous Futures") +
  theme_report()


## ------------------------------------------------------------------------
# Plot a series of boxplots per year
natgas %>% 
  mutate(Year = format(Date, "%Y"), Returns = (Settle / lead(Settle) - 1)) %>% 
  group_by(Year) %>% 
  ggplot(mapping = aes(x = Year, y = Returns, fill = Year)) +
    geom_boxplot() +
    labs(
      x = "Year",
      y = "Returns %",
      title = "Boxplots of Natural Gas Returns", 
      subtitle = "Henry Hub Natural Gas Returns, 1990 to 2019",
      caption = "Source: Quandl, Wiki Continuous Futures") +
    theme_report() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, size = 8),
      )


## ------------------------------------------------------------------------
# Plot a QQ PLot of Returns
natgas_returns %>% 
  ggplot(mapping = aes(sample = Settle)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
    title = "Quantile-Quantile Plot of Natural Gas Returns",
    subtitle = "Distribution of Natural Gas Returns, 1990 to 2019",
    caption = "Data source: Quandl, Wiki Continuous Futures"
  ) +
  theme_report()


## ------------------------------------------------------------------------
# Define initial system parameters
initdate <- "2010-01-01"
from <- "2011-01-01"
to <- "2019-01-01"

# Set timezone as "UTC" with Sys.setenv()
Sys.setenv(TZ = "UTC")

# Define the currency used as USD
currency("USD")

# Create a filtered xts object "NG" for the purpose of backtesting
asset <- "NG"
NG <- natgas_xts[, -c(4, 5, 8)]
NG <- NG["2011-01-01/2019-01-01"]
colnames(NG) <- c("NG.Open", "NG.High", "NG.Low", "NG.Close", "NG.Volume")
head(NG)
tail(NG)
future(primary_id = asset, currency = "USD", multiplier = 1)

## ------------------------------------------------------------------------
# Define tradesize and initial equity amounts as $10,000 and $100,000 respectively
tradesize <- 10000
initeq <- 100000

# Name all strategy instruments
strategy.st <- portfolio.st <- account.st <- "algo_strategy"

# Remove old portfolio and strategy from the environment
rm.strat(portfolio.st)
rm.strat(strategy.st)

# Initialize portfolio, account, orders and strategy objects
initPortf(portfolio.st, symbols = asset, initDate = initdate, currency = "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEQ = initeq)
initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = TRUE)


## ------------------------------------------------------------------------
# Add a long-dated Simple Moving Average indicator to the strategy
add.indicator(
  strategy = strategy.st, 
  name = "SMA", 
  arguments = list(
    x = quote(Cl(mktdata)), 
    n = 200), 
  label = "SMA200")

# Add a short-dated Simple Moving Average indicator to the strategy
add.indicator(
  strategy = strategy.st, 
  name = "SMA", 
  arguments = list(
    x = quote(Cl(mktdata)), 
    n = 50), 
  label = "SMA50")

# Add a Relative Strength Index indicator to the strategy
add.indicator(
  strategy = strategy.st,
  name = "RSI",
  arguments = list(
    price = quote(Cl(mktdata)),
    n = 3),
  label = "RSI3"
  )


## ------------------------------------------------------------------------
# Add a buy signal when SMA50 crosses above SMA200
add.signal(
  strategy = strategy.st, 
  name = "sigComparison", 
  arguments = list(
    columns = c("SMA50", "SMA200"),
    relationship = "gt"),
  label = "long_filter")

# Add a buy signal when the RSI is below 30
add.signal(
  strategy = strategy.st,
  name = "sigThreshold",
  arguments = list(
    column = "RSI3",
    threshold = 30,
    relationship = "lt",
    cross = FALSE),
  label = "long_threshold")

# Add a "long_entry" signal when the two buy signals converge
add.signal(
  strategy = strategy.st,
  name = "sigFormula",
  arguments = list(
    formula = "long_filter & long_threshold",
    cross = TRUE),
  label = "long_entry")

# Add a sell signal when SMA50 crosses below SMA200
add.signal(
  strategy = strategy.st,
  name = "sigComparison",
  arguments = list(
    columns = c("SMA50", "SMA200"),
    relationship = "lt"),
  label = "exit_filter")

# Add an Exit signal when RSI crosses above 70
add.signal(
  strategy = strategy.st,
  name = "sigThreshold",
  arguments = list(
    column = "RSI3",
    threshold = 70,
    relationship = "gt",
    cross = TRUE),
  label = "exit_threshold")


## ------------------------------------------------------------------------
# Add a rule to go long when the long_entry signal is TRUE
add.rule(
  strategy = strategy.st, 
  name = "ruleSignal", 
  arguments = list(
    sigcol = "long_entry", 
    sigval = TRUE, 
    orderqty = 1, 
    ordertype = "market", 
    orderside = "long", 
    replace = FALSE, 
    prefer = "Open",
    OSFUN = IKTrading::osMaxDollar,
    tradesize = tradesize,
    maxSize = tradesize), 
  type = "enter")

# Add a rule to exit the position when exit_filter is TRUE
add.rule(
  strategy = strategy.st, 
  name = "ruleSignal", 
  arguments = list(
    sigcol = "exit_filter", 
    sigval = TRUE, 
    orderqty = "all", 
    ordertype = "market", 
    orderside = "long", 
    replace = FALSE,
    prefer = "Open"), 
  type = "exit")

# Add a rule to exit the position when exit_threshold is TRUE
add.rule(
  strategy = strategy.st, 
  name = "ruleSignal", 
  arguments = list(
    sigcol = "exit_threshold", 
    sigval = TRUE, 
    orderqty = "all", 
    ordertype = "market", 
    orderside = "long", 
    replace = FALSE,
    prefer = "Open"), 
  type = "exit")


## ------------------------------------------------------------------------
# Apply the strategy onto Natural Gas Historical Data
output <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

# Update the portfolio object accordingly
updatePortf(Portfolio = portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(account.st, daterange)
updateEndEq(account.st)


## ------------------------------------------------------------------------
# Plot a summary chart of the Portfolio's performance
chart.Posn(Portfolio = portfolio.st, Symbol = asset)


## ------------------------------------------------------------------------
# Generate trade statistics for this strategy
tstats <- tradeStats(Portfolios = portfolio.st)
data.frame(t(tstats))

## ------------------------------------------------------------------------
# Export analysis to an R Script
purl("algo-trading-with-natgas-futures.Rmd")

