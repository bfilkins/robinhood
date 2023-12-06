# Author: Brendan Filkins
# Date: 2023-12-1
# Description: Automate stock trading

# Load packages
source("src/packages.R")

# Theme
source("src/theme.R")

# Authenticate: Requires updated MFA for now
RH = RobinHood(
  username = Sys.getenv("personal_email"), 
  password = Sys.getenv("robinhood_password"),
  mfa_code = "748463")

# Query Data 
source("src/data.R")

# Exploratory data analysis
source("src/eda.R")
# Identify trades

# Execute trades

