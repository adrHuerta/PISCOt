# PISCO_Temp
PISCO_Temp ([PISCO](http://ons.snirh.gob.pe/Peru/maproom/Monitoring/Meteorological/PISCO_reporte.pdf) Temperature) is an open source framework written in R for interpolating daily temperature observations at "topoclimatic" spatial scale (< 10km). Using digital elevation model (DEM) and remotely sensed observations of land skin temperature.The present interpolation procedure is mainly based on regression kriging. Likewise, the temporal variability of temperature is assured trought an [infilling procedure](https://link.springer.com/article/10.1007/s00704-017-2082-0) and a [pairwise homogenization](https://cran.r-project.org/web/packages/snht/snht.pdf). PISCO_Temp is currently being developed at [National Service of Meteorology and Hydrology](http://www.senamhi.gob.pe/). It is expected to produce a 1981-present (~1km or ~5km) gridded dataset of daily maximum and minimum air temperature for Peru.

## Dataset
PISCO_Temp can be downloaded [here](https://drive.google.com/open?id=1eGqhmJXBJfFSzUFz2RVqtbKIlOphpkcs)

## Code 
It is expected to create an R package soon

## References

Oyler, J.W., Ballantyne, A., Jencso, K., Sweet, M. and Running, S. W. (2014). Creating a topoclimatic daily air temperature dataset for the conterminous United States using homogenized station data and remotely sensed land skin temperature. Int. J. Climatol. http://dx.doi.org/10.1002/joc.4127.

Hengl T., Heuvelink G., Rossiter D. (2007). About regression-kriging: From equations to case studies. Computers & Geosciences. https://doi.org/10.1016/j.cageo.2007.05.001.

Woldesenbet, T. A., Elagib, N. A., Ribbe, L. and Heinrich, J. (2017). Gap filling and homogenization of climatological datasets in the headwater region of the Upper Blue Nile Basin, Ethiopia. Int. J. Climatol. http://dx.doi.org/10.1002/joc.4839

## Acknowledgements

Development of the current version of PISCO_Temp is supported by the [CLIMANDES-2 project](http://www.senamhi.gob.pe/climandes/).

## Acknowledgements II

This work is inspired by [jaredwo/topowx](https://github.com/jaredwo/topowx)
