###########################################################################
# Goal:    Collect BSWI Socioeconomic indicators
# Author:  Stephen Stapleton
# Created: 2022-07-26
# Updated: 2023-08-04
###########################################################################

# data source links:
# median household income: tidycensus API
# food insecurity: https://www.ers.usda.gov/media/rbmpu1zi/mapdata2021.xlsx
# poverty: tidycensus API
# GDP per capita: https://www.bea.gov/sites/default/files/2023-06/stgdppi1q23.xlsx
# unemployment: tidycensus API
# unionization: https://unionstats.com/state/xls/state_2022.xlsx

# NOTE: REQUIRES DOWNLOAD:
# infant mortality: https://wonder.cdc.gov/ 

# origin ->

# Steps of this script:
# (1) collect data from Census API/local files
# (2) pull most recent BSWI index score data
# (3) consolidate into single state-level dataframe

# -> bswi_correlations.R

#--------------------------------------------------------------------------
# setup

# check for packages
list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here', 'openxlsx')
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
acs.vars <- load_variables( 2021, 'acs5' )

acs.ind <- get_acs( geography = 'state',
                    year = 2021,
                    variables = c('median_hhld_income' = 'B06011_001',
                                  'below_100_fpl' = 'B06012_002',
                                  'fpl_total' = 'B06012_001',
                                  'lf_total' = 'B23025_003',
                                  'unemployed' = 'B23025_005') ) %>%
  
  # pivot data
  select( -moe ) %>%
  pivot_wider( names_from = 'variable',
               values_from = 'estimate' ) %>%

  # format variables
  mutate( below_100_fpl = below_100_fpl / fpl_total,
          unemployed = unemployed / lf_total ) %>%
  
  # rename
  rename_all( tolower ); rm( acs.vars )

#--------------------------------------------------------------------------
# collect other API data

# food insecurity
foodsecure <- read.xlsx('https://www.ers.usda.gov/media/rbmpu1zi/mapdata2021.xlsx',
                        startRow = 6,
                        colNames = F )
colnames(foodsecure) <- c('state_abbrev', 'hhlds_1921', 'interviewed',
                          'pct_loworless', 'V1', 'V2', 'pct_verylow', 'V3', 'V4')
foodsecure %<>%
  select( -starts_with('V') ) %>%
  mutate_at( vars('pct_loworless', 'pct_verylow'), function(x) x / 100 ) %>%
  filter( !grepl('\\.', state_abbrev) ) %>%
  select( state_abbrev, pct_loworless, pct_verylow )

# GDP per capita
bea.gdp <- read.xlsx('https://www.bea.gov/sites/default/files/2023-06/stgdppi1q23.xlsx',
                     startRow = 7,
                     colNames = F,
                     rows = c(1:66))[ , c(1,3) ]
colnames(bea.gdp) <- c('state', 'gdp_22' )

bea.gdp %<>%
  mutate_at( vars(state), trimws ) %>%
  mutate_at( vars(gdp_22), function(x) x * 1000000 )

bea.gdp %<>%
  
  # collect total population to produce gdp/capita
  left_join( acs.ind %>%
               select( state = name, pop = fpl_total) ) %>%
  mutate( gdp_pc_22 = gdp_22 / pop ) %>%
  na.omit( )

# unionization
unionized <- read.xlsx('https://unionstats.com/state/xls/state_2022.xlsx',
                       startRow = 2,
                       rows = seq(1, 258) )[ c(2:3, 5:9)]
colnames(unionized) <- c('state', 'sector', 'emp_1000', 'mem_1000',
                         'cov_1000', 'pct_member', 'pct_covered')

unionized %<>%
  filter( sector == 'Total' ) %>%
  select( -sector )

#--------------------------------------------------------------------------
# collect locally saved data

infant <- read_delim( 'public_use_data/bswi_correlates/infant_mortality_2017-2020.txt' )[ c(1:51), c(2:6) ]
colnames(infant) <- c('state', 'state_code', 'deaths', 'births', 'rate_1000')

infant %<>%
  mutate_at( vars(rate_1000), parse_number )

#--------------------------------------------------------------------------
# collect other model attributes

acs.dem <- get_acs( geography = 'state',
                    year = 2021,
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

bea.reg <- read_csv("public_use_data/bswi_correlates/bea_gdp.csv") %>%
  select( state, bea_region ) %>%
  distinct( )

#--------------------------------------------------------------------------
# collect bswi 2022 index scores

bswi <- read_csv("public_use_data/bswi_correlates/bswi_2023.csv")

#--------------------------------------------------------------------------
# reconcile into single dataframe

bswi %<>%
  
  # join in all data
  left_join( acs.dem, by = c('state' = 'name') ) %>%
  left_join( acs.ind, by = c('state' = 'name') ) %>%
  left_join( bea.gdp, by = 'state' ) %>%
  left_join( foodsecure, by = 'state_abbrev' ) %>%
  left_join( infant, by = 'state' ) %>%
  left_join( unionized, by = 'state' ) %>%
  left_join( bea.reg, by = 'state' ) %>%
  
  # do some col cleanup
  select( fips = geoid.x, state, state_abbrev, # primary key
          main_index:org_index,                # independent vars
          
          # dependent vars
          median_hhld_income,
          below_100_fpl,
          gdp_pc = gdp_pc_22,
          foodscarce = pct_loworless,
          union = pct_member,
          infantmort = rate_1000,
          unemployed,
          
          # control vars
          eth_afram, eth_latinx, gdp_22, bea_region )

# clean up
rm( acs.dem, acs.ind, bea.gdp, foodsecure, infant, unionized, bea.reg )
