# 0. Set up Environment ----
rm(list = ls())

source("./01_code/00_initialize.R")

# Define necessary packages
pkgs <- c(data.table = "1.15.0")

# Check if all packages are installed with the correct versions.
check_packages(pkgs)

# Load all necessary packages.
load_packages(pkgs)

# 1. Load Data ----
# exported in script 01_macro_model_update.R 
dt_base <- fread(paste0(path_02, "style_ind_incl.csv"))

# 2. Add trailing relative cumulative returns for all styles (Quality, Value, 
# Min-Vol) and for all time frames computed (1M (lagged to avoid mean reversion), 
# 3M, 6M, 12M). These scores are then normalized...
dt_base <- momentum_pillar(dt_base)

# ... and aggregated via weights to the momentum score, resp. the leader (style 
# with the hightest score) is selected if preferred.
dt_base <- build_momentum_composite(dt_base)

# 3. Export Data ----
# Export the data including momentum score and momentum leader. 

fwrite(dt_base, paste0(path_02, "style_ind_incl.csv"))


