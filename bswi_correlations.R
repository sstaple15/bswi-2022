###########################################################################
# Goal:    Conduct BSWI correlations
# Author:  Stephen Stapleton
# Created: 2022-07-01
# Updated: 2023-08-04
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

lapply(list.packages, require, character.only = T); rm(list.packages) # load into environment

# set oxfam color defaults
oxfam_colors <- c('#61A534', '#0C884A', '#E70052')

#--------------------------------------------------------------------------
# create models and store output

# create list of outcome variables
outcomes <- c('median_hhld_income', 'below_100_fpl', 'gdp_pc',
              'foodscarce', 'union', 'infantmort', 'unemployed')

# create list to store results
output.list <- list( base_model = list(),
                     control_model = list(),
                     robust_model = list() )

# define base model
for ( var in outcomes ) {
  
  # conduct correlation test
  temp <- summary( lm( formula = paste( var, '~ main_index' ),
                       data = bswi ) )
  
  # save in list of outputs
  output.list$base_model[[var]] = temp
  
}; rm( temp, var )

# define controlled model
for ( var in outcomes ) {
  
  # conduct correlation test
  temp <- summary( lm( formula = paste( var, '~ main_index + eth_afram + eth_latinx' ),
                       data = bswi ) )
  
  # save in list of outputs
  output.list$control_model[[var]] = temp
  
}; rm( temp, var )

# define robust model
for ( var in outcomes ) {
  
  # conduct correlation test
  temp <- summary( lm( formula = paste( var, '~ main_index + eth_afram + eth_latinx + gdp_22 + as.factor(bea_region)' ),
                       data = bswi ) )
  
  # save in list of outputs
  output.list$robust_model[[var]] = temp
  
}; rm( temp, var )

#--------------------------------------------------------------------------
# consolidate output into simple summary tables

# machine-readable version
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

options(scipen=999)

# make human-readable
lm.table <- cbind(
  lm.estimates[1:length(outcomes), ],
  lm.estimates[(length(outcomes)+1):(length(outcomes)*2), ],
  lm.estimates[(length(outcomes)*2+1):(length(outcomes)*3), ]
)

# make cols readable
colnames(lm.table) <- c('base_coef', 'x1', 'x2', 'base_pval',
                        'ctrl_coef', 'x2', 'x3', 'ctrl_pval',
                        'rbst_coef', 'x4', 'x5', 'rbst_pval')
# make rows readable
rownames(lm.table) <- c('Median Household Income (USD)', 'Below 100% FPL', 'GDP per Capita',
                        'Food Scarcity', 'Unionization Rate', 'Infant Mortality', 'Unemployment Rate')
lm.table %<>% select( !contains('x') )

# helper function for significance stars
sigStars <- function( coef, pval ) {
  
  coef <- round( coef, 4 )
  
  out <- case_when( pval <= 0.01 ~ paste0( coef, '***'),
                    pval <= 0.05 ~ paste0( coef, '**'),
                    pval <= 0.10 ~ paste0( coef, '*'),
                    T ~ as.character(coef) )
  return( out )
}

# create significance stars
lm.table %<>%
  
  mutate( base_coef = sigStars( base_coef, base_pval ),
          ctrl_coef = sigStars( ctrl_coef, ctrl_coef ),
          rbst_coef = sigStars( rbst_coef, ctrl_coef ) ) %>%
  
  mutate_at( vars(base_pval, ctrl_pval, rbst_pval),
             function(x) round(x, 3) )

colnames(lm.table) <- rep(c('Coefficient', 'p-value'), 3)
lm.table %>%
  knitr::kable() %>%
  kableExtra::add_header_above( c('Base Model' = 2, 'Control Model' = 2, 'Robust Model' = 3) ) %>%
  kableExtra::kable_styling()

#--------------------------------------------------------------------------
# present base model output in visuals

outcomes.nice <- c('Median Household Income (USD)', 'Percent (%) Below 100% FPL',
                   'GDP per Capita (USD)', 'Percent (%) Food Scarce',
                   'Percent (%) Union Participation', 'Percent (%) Infant Mortality',
                   'Percent (%) Unemployed')

visual.list <- list()

i = 1

while ( i <= length(outcomes) ) {
  
  visual.list[[ outcomes[[i]] ]] <- bswi %>%
    ggplot( aes_string( x = 'main_index', y = outcomes[[i]] ) ) +
    geom_point( color = oxfam_colors[1],
                fill = 'white',
                shape = 1,
                stroke = 1.5,
                size = 5 ) +
    
    # geom_smooth( method = lm, color = oxfam_colors[3], se = F ) +
    geom_line( data = fortify( lm( paste0(outcomes[[i]], '~ main_index'), data = bswi ) ),
               aes_string( x = 'main_index', y = '.fitted' ),
               color = oxfam_colors[2],
               size = 1 ) +
    geom_line( data = fortify( lm( paste0(outcomes[[i]], '~ main_index + eth_afram + eth_latinx'), data = bswi ) ),
               aes_string( x = 'main_index', y = '.fitted' ),
               color = oxfam_colors[3] ) +
    
    geom_text( aes_string( x = 'main_index', y = outcomes[[i]], label = 'state_abbrev' ),
               size = 2 ) +
    
    theme_minimal( ) +
    labs( x = 'BSWI 2022',
          y = outcomes.nice[[i]],
          title = paste( 'BSWI 2022 Correlation to:', outcomes.nice[[i]] ),
          subtitle = 'Base OLS regression fit presented in dark green, model controlling for state demographics in red.',
          caption = 'Data is sourced from U.S. Government sources, including the Census ACS, the Bureau of Labor Statistics, and the Centers for Disease Control and Prevention')
  
  i = i + 1
  
}; rm( i )

