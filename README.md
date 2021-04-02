# Matricula Consular de Alta Seguridad (MCAS) Mapping Visualization Tool

## The Data

The main data set contains, as of March-21, 174,281 observations with the following columns:

* `cve_ent`: Mexican state ID
* `nom_ent`: Mexican state name
* `cve_mun`: Mexican municipio ID
* `nom_mun`: Mexican municipio name
* `statefip`: FIP code that uniquely identify each state (see [Federal Information Processing System (FIPS) Codes for States and Counties](https://transition.fcc.gov/oet/info/maps/census/fips/fips.txt#:~:text=FIPS%20codes%20are%20numbers%20which,to%20which%20the%20county%20belongs.) for more information).
* `state`: U.S. county state
* `countyfip`: FIP code that uniquely identify each county (five digits of which the 
* `county`: U.S. county name
first two are the FIPS code of the state to which the county belongs)
* `matricula`