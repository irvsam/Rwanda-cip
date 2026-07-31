# Rwanda-cip
Quantitative analysis on the social impacts of the Crop Intensification Program in Rwanda


# Data

The data used in this project is from the following sources:

* the Rwanda Integrated Household Living Conditions Survey (EICV7) covering October 2023 to October 2024 website: https://microdata.statistics.gov.rw/index.php/catalog/119/get_microdata
* the Rwanda Seasonal Agricultural Survey 2025 (SAS) covering September, 2023 to September, 2024 website: https://microdata.statistics.gov.rw/index.php/catalog/124/get_microdata
For the above microdata it is necessary to create an account to access the files but summary tables are readily available without an account.

* administrative boundaries were downloaded from the Humanitarian Data Exchange (HDX) website: https://data.humdata.org/dataset/rwanda-administrative-boundaries 
* Rainfall data was downloaded from the Humanitarian Data Exchange (HDX) website: https://data.humdata.org/dataset/rwa-rainfall-subnational

# Research Project: CIP, Food Welfare, and Smallholder Vulnerability in Rwanda

## Overview
This project investigates whether district-level land use consolidation (LUC)
intensity under Rwanda's Crop Intensification Program (CIP) reduces household
food consumption welfare, and whether this effect is worse for poorer households.
The broader argument is that CIP forces households into drought-vulnerable
monocrops, reducing food welfare in ways that increase climate vulnerability
and household instability.

## Data Sources
- **EICV7 (2023/24):** Household-level survey from World Bank Microdata Catalog
  - `CS_S01_S5_S7_Household.dta` — shock variables (s5eq1, s5eq2a, s5eq3a, s5eq4a), wealth quintile, urban/rural
  - `CS_EICV7_poverty_file.dta` — food consumption (food), aggregate consumption (sol_jan), adult equivalents (ae), wealth quintiles
  - `CS_S10A1_A2_credits.dta` — credit access controls
  - `CS_S10C_Savings.dta` — savings controls
- **NISR Seasonal Agricultural Survey (SAS) 2024:** Plot-level microdata
  - Seasons A, B, C — used to construct district-level LUC intensity

## Treatment Variable
District-level LUC intensity: population-weighted average share of agricultural
land under consolidation across SAS 2024 Seasons A, B, and C. Constructed by
filtering to agricultural plots (s2q7 == NA), summing weighted plot area where
s2q12 == 1 (Yes to consolidation) divided by total weighted plot area, averaged
across seasons. n = 30 districts, range 0.17% to 26.3%.

---

## CHOSEN DESIGN: Food Consumption Welfare

**Research question:** Does district-level LUC intensity reduce household food
consumption per adult equivalent, and is this effect larger for poorer households?

**Sample:** Full nationally representative EICV7 sample, n ≈ 15,000

**Dependent variable:** Log food consumption per adult equivalent (log(food/ae))
from EICV7 poverty file

**Model:** OLS regression
`log(food_ae) ~ luc_intensity + quintile_f + luc_intensity:quintile_f + ur_f + province_f`

**Test result:** luc_intensity significant at p = 0.017, negative direction
as expected. Interaction terms not significant in test but main effect is clean.

**Climate/conflict link:** CIP forces households into drought-vulnerable monocrops
(Clay and Zimmerer show CIP crops have 2x higher yield loss rates in climate
shocks than traditional crops). Reduced food welfare is an established antecedent
of household instability (Hsiang et al., 2013). The seminar link is made in the
theoretical framework, not the regression.

**Known limitations:**
- Cross-sectional: correlation not causation
- Endogeneity: high-LUC districts may be systematically different (soil quality,
  market access) — province fixed effects partially address this
- No household-level CIP participation data in EICV7, only district-level proxy
- Interaction terms (H2) not significant in test regression

---

## ALTERNATIVE DESIGN: Distress Coping Under Shock

**Research question:** Among shock-affected households, does higher LUC intensity
increase the likelihood of distress coping strategies?

**Sample:** Households reporting agricultural/weather shocks (s5eq2a %in%
c(1,2,3,4,5,6,8,9)), n ≈ 3,000

**Dependent variable:** Binary distress coping from s5eq3a:
- Distress (1): reduced food expenditure, sold land/productive assets,
  sold last female animals, withdrew child from school, begging,
  entire household migrated (codes 2,3,4,8,9,11,13,14)
- Adaptive (0): used savings, borrowed from bank, worked more hours,
  received help from family/friends/government/NGO, started business

**Why not chosen:** LUC intensity did not predict distress coping in any
specification tested — full sample, rural only, farming households only,
drought only. District-level treatment variable too coarse to detect
household-level coping differences. No clean pattern in raw data across
LUC quartiles either.

**Could be revisited if:** Household-level CIP participation data becomes
available, or shock severity data is added to allow controlling for
heterogeneous shock exposure across households.

---

## Key Variables Confirmed in Data
| Variable | File | Variable name |
|---|---|---|
| Food consumption | CS_EICV7_poverty_file | food |
| Adult equivalents | CS_EICV7_poverty_file | ae |
| Aggregate consumption/AE | CS_EICV7_poverty_file | sol_jan |
| Wealth quintile | CS_S01_S5_S7_Household | quintile |
| Urban/rural | CS_S01_S5_S7_Household | ur |
| Province | CS_S01_S5_S7_Household | province |
| Shock indicator | CS_S01_S5_S7_Household | s5eq1 |
| Shock type | CS_S01_S5_S7_Household | s5eq2a |
| Coping strategy | CS_S01_S5_S7_Household | s5eq3a |
| Recovery time | CS_S01_S5_S7_Household | s5eq4a |
| Farming household | CS_S01_S5_S7_Household | s7aq4 |
| LUC indicator (plot) | SAS 2024 Seasons A/B/C | s2q12 |
| Plot size | SAS 2024 Seasons A/B/C | Plot_size_ha |
| Plot weight | SAS 2024 Seasons A/B/C | plot_weight |
| District | SAS 2024 Seasons A/B/C | s1q2 |

## Notes
- SAS agricultural plots identified by s2q7 == NA (non-NA values are
  non-agricultural land uses: buildings, roads, forest, etc.)
- EICV7 and SAS district codes are consistent (both use numeric codes
  11-57 for Rwanda's 30 districts)
- CIP is ongoing as of 2024 — do not cite Heinen's "2007-2020" framing
- SAS data considered reliable post-2013 (Heinen, 2022)