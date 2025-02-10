## Original script from ChatGPT ################################################
library(quantmod)
library(lubridate)
library(sendmailR)
library(jsonlite)

# Your target asset allocation
target_allocation <- c(FITLX = 0.55, FNIDX = 0.25, FNDSX = 0.20)

# Fund tickers
funds <- c("FITLX", "FNIDX", "FNDSX")

# Path to the JSON file that stores the last rebalancing date and prices
last_rebalance_file <- "last_rebalance.json"

# Regular rebalancing dates (MM-DD)
regular_rebalance_dates <- c("05-01", "11-01")

# Fetch current fund prices
fetch_fund_data <- function(funds) {
  prices <- sapply(funds, function(fund) {
    getSymbols(fund, src = "yahoo", auto.assign = FALSE)
    last_price <- Cl(get(fund))[length(Cl(get(fund)))]  # Latest price
    return(last_price)
  })
  return(prices)
}

# Fetch historical data (for storing from the last rebalancing)
fetch_historical_data <- function(funds, start_date) {
  prices <- sapply(funds, function(fund) {
    getSymbols(fund, src = "yahoo", from = start_date, to = Sys.Date(), auto.assign = FALSE)
    first_price <- Cl(get(fund))[1]  # Price on the first date
    return(first_price)
  })
  return(prices)
}

# Calculate portfolio allocation based on the change in price since last rebalance
calculate_allocation <- function(fund_data, historical_prices) {
  total_value <- sum(fund_data)
  historical_total <- sum(historical_prices)

  allocation <- sapply(names(fund_data), function(fund) {
    current_value <- fund_data[fund]
    historical_value <- historical_prices[fund]

    if (historical_value > 0) {
      change_ratio <- (current_value / historical_value) - 1
      return(change_ratio)
    } else {
      return(0)
    }
  })

  return(allocation)
}

# Check if rebalancing is needed based on the allocation change
check_rebalance <- function(target_allocation, current_allocation, threshold = 0.05) {
  alerts <- c()
  for (fund in names(target_allocation)) {
    target_weight <- target_allocation[fund]
    current_weight <- current_allocation[fund]

    if (abs(current_weight - target_weight) > threshold) {
      alerts <- c(alerts, paste("Rebalance needed:", fund, "- Target:", target_weight * 100, "%, Current:", current_weight * 100, "%"))
    }
  }
  return(alerts)
}

# Send email notification
send_email <- function(alerts) {
  from <- "your_email@example.com"
  to <- "your_email@example.com"
  subject <- "Rebalancing Alert"
  body <- paste(alerts, collapse = "\n")

  email <- list(
    from = from,
    to = to,
    subject = subject,
    body = body
  )

  sendmail(from = email$from, to = email$to, subject = email$subject, body = email$body, smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = from, passwd = "your_password", ssl = TRUE))
}

# Load the last rebalancing date and historical prices
load_last_rebalance_data <- function() {
  if (file.exists(last_rebalance_file)) {
    data <- fromJSON(last_rebalance_file)
    last_rebalance_date <- as.Date(data$last_rebalance_date)
    historical_prices <- data$historical_prices
  } else {
    last_rebalance_date <- as.Date("2020-01-01")  # Default date
    historical_prices <- numeric(0)
  }
  return(list(last_rebalance_date = last_rebalance_date, historical_prices = historical_prices))
}

# Save the last rebalancing date and current prices
save_last_rebalance_data <- function(date, current_prices) {
  data <- list(
    last_rebalance_date = as.character(date),
    historical_prices = current_prices
  )
  write_json(data, last_rebalance_file, pretty = TRUE)
}

# Check if the program should trigger rebalancing based on the current date
should_trigger_rebalance <- function(last_rebalance_date) {
  current_date <- Sys.Date()

  # Check if today is within 30 days of the next regular rebalance date
  for (regular_date in regular_rebalance_dates) {
    next_rebalance <- as.Date(paste0(format(current_date, "%Y-"), regular_date))
    if (next_rebalance < current_date) {  # If the regular date has already passed, check next year
      next_rebalance <- next_rebalance + years(1)
    }

    # If the next rebalance is within 30 days, skip rebalancing
    if (next_rebalance - current_date < days(30)) {
      if (current_date - last_rebalance_date < days(30)) {
        print("Skipping regular rebalancing due to recent rebalancing")
        return(FALSE)
      }
    }
  }

  return(TRUE)
}

# Main process
last_rebalance_data <- load_last_rebalance_data()
last_rebalance_date <- last_rebalance_data$last_rebalance_date
historical_prices <- last_rebalance_data$historical_prices

# Fetch current prices and calculate change since last rebalance
current_fund_data <- fetch_fund_data(funds)

# If we don't have historical prices, fetch them
if (length(historical_prices) == 0) {
  historical_prices <- fetch_historical_data(funds, last_rebalance_date)
}

current_allocation <- calculate_allocation(current_fund_data, historical_prices)
rebalance_alerts <- check_rebalance(target_allocation, current_allocation)

# Check if it's time for rebalancing
if (length(rebalance_alerts) > 0 && should_trigger_rebalance(last_rebalance_date)) {
  send_email(rebalance_alerts)
  save_last_rebalance_data(Sys.Date(), current_fund_data)
} else {
  print("No rebalancing needed.")
}

## Gmail notification provided by ChatGPT after ################################
library(gmailr)

send_gmail_notification <- function(subject, body) {
  email <- mime() %>%
    to("your-email@gmail.com") %>%
    from("your-email@gmail.com") %>%
    subject(subject) %>%
    text_body(body)

  send_message(email)
}

# Example usage
send_gmail_notification("Rebalance Alert", "Your portfolio needs rebalancing. Check your investments.")


## Sticky desktop notification provided by ChatGPT after #######################
library(tcltk)

send_persistent_notification <- function(message) {
  tt <- tktoplevel()
  tkwm.title(tt, "Portfolio Alert")
  tkpack(tklabel(tt, text = message), side = "top")
  tkpack(tkbutton(tt, text = "OK", command = function() tkdestroy(tt)), side = "bottom")
}

# Example usage
send_persistent_notification("Time to rebalance your portfolio!")


## Combining gmail and desktop notifications ###################################
send_notifications <- function(message) {
  # Desktop pop-up
  send_persistent_notification(message)

  # Email notification
  send_email_notification("Rebalance Alert", message)
}

# Example usage
send_notifications("Your portfolio needs rebalancing. Check your investments.")
