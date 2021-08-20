# GPP data download script.

# Load packages and helper functions --------
# devtools::install_github("cboettig/neonstore")
library(neonUtilities)
library(neonstore)
library(tidyverse)


# download the products

products = neonstore::neon_products()

dissolved_gas_tables = neonstore::neon_index("DP1.20288.001")

# download the water quality data
neon_download("DP1.20288.001", site = "BLDE", start_date = "2018-01-01",
              table = "waq_instantaneous-basic",type = "basic", unique = TRUE)

discharge = neonstore::neon_index("DP4.00130.001")

# download the depth data
neon_download("DP4.00130.001", site = "BLDE", start_date = "2018-01-01",
              type = "basic", unique = TRUE)
