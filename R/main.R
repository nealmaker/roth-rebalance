## Later Chat GPT Script that preportedly integrates everything in main-v0 #####
## Maybe created by worse version of chatbot, check against v0 #################

# Install necessary packages (if not already installed)
if (!require("httr")) install.packages("httr", dependencies = TRUE)
if (!require("gmailr")) install.packages("gmailr", dependencies = TRUE)
if (!require("tcltk")) install.packages("tcltk", dependencies = TRUE)

# Function to send Gmail notification
send_email_notification <- function(subject, body, recipient_email) {
  library(gmailr)

  # Authenticate Gmail
  gm_auth_configure(path = "path_to_your_gmail_credentials.json")  # Your Gmail credentials

  # Create and send the email
  email <- gm_mime() %>%
    gm_to(recipient_email) %>%
    gm_from("your_email@gmail.com") %>%
    gm_subject(subject) %>%
    gm_text_body(body)

  gm_send_message(email)
}

# Function to show a sticky desktop alert
show_sticky_alert <- function(message) {
  library(tcltk)

  tcl("wm", "attributes", ".", "-topmost", TRUE)  # Ensure the alert stays on top
  tkmessageBox(message = message, icon = "info", type = "ok")
}

# Main script logic to calculate portfolio imbalance and notify
rebalance_check <- function(last_rebalance_date, asset_allocation, fund_data) {

  # Define the rules for when to trigger rebalance (e.g., 10% change in asset allocation)
  trigger_threshold <- 0.10

  # Calculate current allocation based on changes since last rebalance
  current_allocation <- calculate_allocation(last_rebalance_date, fund_data)

  # Compare the current allocation to the target allocation
  portfolio_imbalance <- abs(current_allocation - asset_allocation) / asset_allocation

  if (portfolio_imbalance >= trigger_threshold) {
    # Send email notification
    subject <- "Portfolio Rebalance Needed"
    body <- paste("Your portfolio allocation has changed significantly. Please consider rebalancing.")
    recipient_email <- "your_email@gmail.com"
    send_email_notification(subject, body, recipient_email)

    # Show desktop alert
    show_sticky_alert("Portfolio imbalance detected. Please rebalance your portfolio.")
  }
}

# Function to calculate current allocation based on fund data and last rebalancing date
calculate_allocation <- function(last_rebalance_date, fund_data) {

  # Fetch historical fund prices (you may need to replace this with actual data retrieval logic)
  historical_prices <- fetch_historical_prices(last_rebalance_date, fund_data)

  # Calculate the percentage change in the price for each fund since the last rebalancing
  allocation_changes <- (fund_data$current_price / historical_prices) - 1

  # Calculate current allocation based on price changes
  current_allocation <- fund_data$allocation * (1 + allocation_changes)

  return(current_allocation)
}

# Function to fetch historical fund prices (this is a placeholder, needs actual implementation)
fetch_historical_prices <- function(last_rebalance_date, fund_data) {
  # Placeholder code: Replace this with actual logic to fetch historical prices
  historical_prices <- rep(100, length(fund_data))  # Assume all funds were at price 100 at last rebalance
  return(historical_prices)
}

# Define your target asset allocation (example)
asset_allocation <- c(domestic_stock = 0.55, foreign_stock = 0.25, bond_index = 0.20)

# Example fund data (you need to replace with actual fund data)
fund_data <- data.frame(
  fund_name = c("domestic_stock", "foreign_stock", "bond_index"),
  current_price = c(150, 120, 110),  # Current prices of your funds
  allocation = c(0.55, 0.25, 0.20)  # Your target allocation for each fund
)

# Specify the date of last rebalancing
last_rebalance_date <- as.Date("2024-05-01")

# Run the rebalance check
rebalance_check(last_rebalance_date, asset_allocation, fund_data)
