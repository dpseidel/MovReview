[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1185383.svg)](https://doi.org/10.5281/zenodo.1185383)

Ecological metrics and methods for GPS movement data: Reproducible Examples
=======================================================================

This package contains the source code, analysis, and figures from my 
review with Eric Dougherty, Colin Carlson, and Wayne M. Getz, submitted for
consideration to the International Journal of Geographical Information Science
in February 2018, accepted in July 2018. 

- Author: Dana Seidel 
- License: [CC0](http://creativecommons.org/publicdomain/zero/1.0/)

The associated review can be found [here](https://doi.org/10.1080/13658816.2018.1498097).

## Files
- `Behavior.R` --  Script for fitting an intercept-only HMM. Used to produce Fig 1.
- `UtilDist.R` -- Script for estimating homerange using MCP, Kernel Density, and LoCoh. Used to produce Fig 2. 
- `SSF.R` -- author: Eric Dougherty (@doughertyeric), Script used to produce Fig 3. 
- `Data Layers/AG256_2010_Extract.csv` -- the data file used in the submitted SSF. 
 
All data can be found in the `Data Layers/` directory. All figures produced
are found in the `Figures/` directory. 

### Funding Statement
This research was supported by grants NIH-GM 083863 and NSF-EEID 1617982.
