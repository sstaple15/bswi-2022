###########################################################################
# Goal:    Collect BSWI Socio-economic indicators
# Author:  Stephen Stapleton
# Created: 2022-07-01
# Updated: 2022-07-07
###########################################################################

# data source links:
# evictions: https://eviction-lab-data-downloads.s3.amazonaws.com/estimating-eviction-prevalance-across-us/state_eviction_estimates_2000_2018.csv
# infant mortality: blob:https://www.cdc.gov/1c017f24-7c51-4f52-b162-25e4ffcca7a4
# food security: https://www.ers.usda.gov/media/rbmpu1zi/mapdata2020.xlsx
# GDP and gov GDP: https://apps.bea.gov/itable/iTable.cfm?ReqID=70&step=1

# origin ->

# Steps of this script:
# (1) collect data from Census API/local file
# (2) pull most recent BSWI index score data
# (3) consolidate into single state-level dataframe

# -> bswi_correlations.R

#--------------------------------------------------------------------------
# setup

# check for packages
list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here')
new.packages  <- list.packages[!( list.packages %in% installed.packages()[, 'Package'] )]

# install if not currently
if(length(new.packages)) { install.packages(new.packages);
  if(grepl('pewmethods', new.packages)) { # exception for git-only packages
    if(!( 'devtools' %in% installed.packages()[, 'Package'] )) { install.packages('devtools') }
    require(devtools); install_github("pewresearch/pewmethods") } }; rm(new.packages) 

lapply(list.packages, require, character.only = T); rm(list.packages) # load into environment

#--------------------------------------------------------------------------
# collect Census API data for correlates

# check variable names to pull
acs.vars <- load_variables( 2019, 'acs5' )

acs.ind <- get_acs( geography = 'state',
                    year = 2019,
                    variables = c('median_hhld_income' = 'B06011_001',
                                  'below_100_fpl' = 'B06012_002',
                                  'fpl_total' = 'B06012_001') ) %>%
  
  # pivot data
  select( -moe ) %>%
  pivot_wider( names_from = 'variable',
               values_from = 'estimate' ) %>%
  
  # format variables
  mutate( below_100_fpl = below_100_fpl / fpl_total ) %>%
  
  # rename
  rename_all( tolower )

#--------------------------------------------------------------------------
# collect locally saved data

evictions <- read_csv("/projects/sstapleton/min_wage_fight/public_use_data/bswi_correlates/evictionlab_state_eviction_estimates_2000_2018.csv") %>%
  filter( year == 2018 ) %>%
  select( state, FIPS_state, filings_estimate, renting_hh )

infantmort <- read_csv("/projects/sstapleton/min_wage_fight/public_use_data/bswi_correlates/cdc_infant_mortality_estimates.csv") %>%
  filter( YEAR == 2019 ) %>%
  rename_all( tolower ) %>%
  mutate( infantmort = as.numeric(rate)/100 ) %>%
  select( state, infantmort )
  
foodsecure <- read_csv("/projects/sstapleton/min_wage_fight/public_use_data/bswi_correlates/usda_food_scarcity.csv") %>%
  mutate( food_scarce = food_scarce_est / 100 ) %>%
  select( state_abbrev, food_scarce ) %>%
  filter( !is.na(state_abbrev) )

bea_gdp <- read_csv("/projects/sstapleton/min_wage_fight/public_use_data/bswi_correlates/bea_gdp.csv") %>%
  mutate( gdp = ( q1_2019 + q2_2019 + q3_2019 + q4_2019 ) / 4 ) %>%
  rename_all( tolower ) %>%
  select( fips, state, description, gdp, bea_region ) %>%
  pivot_wider( names_from = 'description', values_from = gdp ) %>%
  mutate( gov_share_gdp = government_only / all_industries ) %>%
  select( -government_only ) %>%
  rename( gdp_millions = 'all_industries' )

#--------------------------------------------------------------------------
# collect other model attributes

acs.dem <- get_acs( geography = 'state',
                    year = 2019,
                    variables = c('eth_total' = 'B03002_001',
                                  'eth_latinx' = 'B03002_012',
                                  'eth_afram' = 'B03002_004') ) %>%
  
  # pivot data
  select( -moe ) %>%
  pivot_wider( names_from = 'variable',
               values_from = 'estimate' ) %>%
  
  # format variables
  mutate( eth_latinx = eth_latinx / eth_total,
          eth_afram = eth_afram / eth_total ) %>%
  
  # rename
  rename_all( tolower )

#--------------------------------------------------------------------------
# collect bswi 2022 index scores

bswi <- read_csv("/projects/sstapleton/min_wage_fight/public_use_data/bswi_correlates/bswi_2022_scores.csv")

#--------------------------------------------------------------------------
# reconcile into single dataframe

bswi %<>%
  
  # join in all data
  left_join( acs.dem, by = c('state' = 'name') ) %>%
  left_join( acs.ind, by = c('state' = 'name') ) %>%
  left_join( bea_gdp, by = 'state' ) %>%
  left_join( evictions, by = 'state' ) %>%
  left_join( foodsecure, by = 'state_abbrev' ) %>%
  left_join( infantmort, by = c('state_abbrev' = 'state') ) %>%
  
  # create remaining vars of interest
  mutate( filings_by_renting = filings_estimate / renting_hh,
          filings_by_total   = filings_estimate / fpl_total,
          gdp_per_capita     = gdp_millions * 1000000 / fpl_total ) %>%
  
  # do some col cleanup
  select( geoid.x, state, state_abbrev, # primary key
          bswi_2022,                    # independent var
          
          # dependent vars
          median_hhld_income, below_100_fpl, filings_by_renting,
          filings_by_total, gdp_per_capita, food_scarce, infantmort,
          
          # control vars
          eth_afram, eth_latinx, bea_region, gov_share_gdp, gdp_millions )

# clean up
rm( acs.dem, acs.ind, acs.vars, bea_gdp, evictions, foodsecure, infantmort )
