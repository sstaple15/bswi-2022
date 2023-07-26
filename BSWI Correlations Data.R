# check for packages
list.packages <- c('tidyverse', 'tidycensus', 'magrittr', 'here')
new.packages  <- list.packages[!( list.packages %in% installed.packages()[, 'Package'] )]

# install if not currently
if(length(new.packages)) { install.packages(new.packages);
  if(grepl('pewmethods', new.packages)) { # exception for git-only packages
    if(!( 'devtools' %in% installed.packages()[, 'Package'] )) { install.packages('devtools') }
    require(devtools); install_github("pewresearch/pewmethods") } }; rm(new.packages) 

library(tidyverse)
library(tidycensus)
getwd()

lapply(list.packages, require, character.only = T); rm(list.packages) # load into environment

acs.vars <- load_variables( 2021, 'acs5' )

acs.poverty <- get_acs( geography = 'state',
                    year = 2021,
                    variables = c(
                                  'below_100_fpl' = 'B06012_002',
                                  'fpl_total' = 'B06012_001') ) 

library(readxl)

read.csv("SAGDP1__ALL_AREAS_1997_2022.csv")

bea_gdp <- read.csv("SAGDP1__ALL_AREAS_1997_2022.csv")

gdp_2022 <-bea_gdp$X2022

bea_gdp$

  str(bea_gdp)
unique(bea_gdp$GeoName)
unique(bea_gdp$LineCode)

gdp <- bea_gdp$LineCode = '3'

simple_22 <-bea_gdp[ ,c("GeoFIPS","GeoName","LineCode","X2022")]

simple_22$GeoFIPS <- as.integer(simple_22$GeoFIPS)

simple_22_clean <-simple_22[simple_22$LineCode ==3 & simple_22$GeoFIPS<60000,]

simple_22_clean<-simple_22_clean[!is.na(simple_22_clean$GeoFIPS),]




