# BSWI 2022

This repo contains code for the Oxfam Best States to Work Index (BSWI) 2022 correlation checks. Specifically, we're interested in the following potential relationships with U.S. State's relative friendliness towards worker rights, protections, and wages (as summarized by the BSWI 2022 index):

- median household income
- GDP per capita
- unionization rates
- infant mortality rates
- maternal mortality rates
- prevalence of hunger
- poverty rates
- eviction rates
- homelessness rates

Data for each of these dimensions is being indexed to 2019, and comes from various governmental or non-profit sources. All socio-economic correlates are collected as rates or medians to account for the differences in State population size.

## Data Sources

The specific link or agency used to collect each of these measures is outlined below:

### Citations

State level eviction filing data made possible through the Eviction Lab:

Ashley Gromis, Ian Fellows, James R. Hendrickson, Lavar Edmonds, Lillian Leung, Adam Porton,
and Matthew Desmond. Estimating Eviction Prevalence across the United States. Princeton
University Eviction Lab. https://data-downloads.evictionlab.org/#estimating-eviction-prevalanceacross-
us/. Deposited May 13, 2022.

## Modeling Approach

The authors recognize that the worker protections passed in a given state are likely endogenous to many of the socio-economic indicators outlined above. This makes this purely a correlational exercise, with the goal being to better understand the potential relationships that may exist for further exploration by other researchers. With this limited scope in mind, we plan to run a simple OLS regression on each socio-economic metric listed above. We plan to run three renditions of this model to vet the robustness of any potential correlation:

1. *Base Model:* a model containing only the dependent and independent variable (BSWI). This will serve as a baseline for report generation.
2. *Controlled Model:* Base Model, with the addition of fixed or otherwise slow-changing attributes serving as controls. The intention of this model is to attempt to account for any latent characteristics of a state's population or economy that might contribute to our metrics of interest, including citizen demographics, economy size, relative industry composition, and the extent of federal government spending accounted for in GDP.
3. *Robust Model:* the Controlled Model, with the addition of fixed effects for State Bureau of Economic Analysis region. The inention of this model is to further account for different regional traditions with respect to labor policy, as well as the potential that regions likely have some degree of intertwined socio-economic outcomes due to cross-border industries. By controlling for region, this model is implictly controlling for differences in score we might see _across_ regions, and is instead comparing BSWI scores _within_ regions.
