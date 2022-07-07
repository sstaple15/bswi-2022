###########################################################################
# Goal:    Conduct BSWI correlations
# Author:  Stephen Stapleton
# Created: 2022-07-01
# Updated: 2022-07-07
###########################################################################

# bswi_collect_data.R ->

# Steps of this script:
# (1) create base model
# (2) create controlled model
# (3) create robust model
# (4) save outputs as graphs and regression tables

# -> complete

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
# create models and store output

# create list of outcome variables
outcomes <- c('median_hhld_income', 'below_100_fpl', 'filings_by_renting',
              'filings_by_total', 'food_scarce', 'infantmort')

# create list to store results
output.list <- list( base_model = list(),
                     control_model = list(),
                     robust_model = list() )

# define base model
for ( var in outcomes ) {
  
  # conduct correlation test
  temp <- summary( lm( formula = paste( var, '~ bswi_2022' ),
                       data = bswi ) )
  
  # save in list of outputs
  output.list$base_model[[var]] = temp
  
}; rm( temp, var )

# define controlled model
for ( var in outcomes ) {
  
  # conduct correlation test
  temp <- summary( lm( formula = paste( var, '~ bswi_2022 + eth_afram + eth_latinx + gov_share_gdp + gdp_millions' ),
                       data = bswi ) )
  
  # save in list of outputs
  output.list$control_model[[var]] = temp
  
}; rm( temp, var )

# define robust model
for ( var in outcomes ) {
  
  # conduct correlation test
  temp <- summary( lm( formula = paste( var, '~ bswi_2022 + eth_afram + eth_latinx + gov_share_gdp + gdp_millions + as.factor(bea_region)' ),
                       data = bswi ) )
  
  # save in list of outputs
  output.list$robust_model[[var]] = temp
  
}; rm( temp, var )

#--------------------------------------------------------------------------
# consolidate output into simple summary table

for ( m in output.list) {
  
  for ( var in m ) {
    
    temp <- as.data.frame( var$coefficients[2,] %>% t() )
    
    if ( !exists('lm.estimates') ) {
      
      lm.estimates <- as.data.frame(var$coefficients[2,] %>% t()) %>% head(0) }
    
    lm.estimates %<>% rbind( temp )
    
  }
  
}; rm( m, var, temp )

# add in names of models and outcomes
rownames(lm.estimates) <- paste0( c(rep('base: ', length(outcomes)),
                                    rep('control: ', length(outcomes)),
                                    rep('robust: ', length(outcomes))),
                                  rep(outcomes, 3) )
