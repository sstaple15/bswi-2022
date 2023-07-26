library(tidycensus)
library(tidyverse)

load_variables( 2021, 'acs5' )

acs.dict <- load_variables( 2021, 'acs5' )

#B27011_008

get_acs(geography = "state", variables = "B19013_001", year = 2021)

get_acs(geography = "state", variables = "B27011_008", year = 2021)

med.hous.inc <- get_acs(geography = "state", variables = "B19013_001", year = 2021)
tot.unemp.rate <- get_acs(geography = "state", variables = "B27011_008", year = 2021)

names(med.hous.inc) <- c("Order", "State", "Med. Household Income", "Estimate", "moe")

names(tot.unemp.rate) <- c("Order", "State", "Tot. Unemployment Rate", "Estimate", "moe")
