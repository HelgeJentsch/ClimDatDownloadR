# Readme ClimDatDownloadR

Hello and welcome to the ClimDatDownloadR R-package.

With this package **cli**mate **dat**asets provided by [CHELSA](https://www.chelsa-climate.org/) and [WorldClim](https://www.worldclim.org/) can be automatically **download**ed, clipped, and converted with **R**.

To start, you'll have to install the package and it's dependencies first, if not already done. Then you can activate the package with the `library`-function.\
For the installation process you need to need to input the following code:

``` (r)
if(!require(remotes)) install.packages("remotes")
library(remotes)
remotes::install_github("HelgeJentsch/ClimDatDownloadR")
library(ClimDatDownloadR)
```

Now that you have the package installed and loaded, let's start with the data sets of the climatologies of CHELSA and WorldClim.

If you have any feedback or are in need of support, you can reach me via [**helgejentsch.research\@gmail.com**](mailto:helgejentsch.research@gmail.com). Feedback and ideas for improving your user experience are always apprechiated!

## Usage

The ClimDatDownloadR enables

1.  selection of dataset and version (e.g., CHELSA 2.1),
2.  selection of available variable-parameters (e.g., bioclimatic variables and precipitation sums),
3.  pre-processing steps like clipping, and
4.  file management options. After describing the complete extent of the workflow a working example is shown.

While this introduces an overview on how to approach the usage of the download functions of the `ClimDatDownloadR`, we employ users to consult documentations on the specific functions to gain further insights on e.g., variable or model availability or additional function parameters. An up-to-date version of the manual can be accessed on the [GitHub repository](https://github.com/HelgeJentsch/ClimDatDownloadR/blob/master/ClimDatDownloadR_manual.pdf).

1.  Start selecting the dataset with `provider.dataset.download()`. The *provider* are `Chelsa` or `WorldClim`. The following *dataset* completes the function name and specifies the accessed dataset. Function names and accessed datasets are referenced in the leading columns of the table above ([Table: Overview of the functions and respective datasets](#table-overview-of-the-functions-and-respective-datasets)). The version can be specified with `version.var`.
2.  The availability of parameters differs between datasets, despite every dataset providing data on monthly temperature means, minima, and maxima, as well as precipitation sums. Bioclimatic variables are also available for all climatologies. Here, the function parameters `month.var` and `bio.var` can be used to specify a subset from the dataset. For past (last glacial maximum) and future (CMIP 5/6) model climatologies a set of function parameters to specify models (`model.var`), scenarios (`emission.scenario.var`), and time interval (`time.interval.var`) can be used. The WorldClim datasets come in different spatial resolutions (10/5/2.5 arc-min., 30 arc-sec.), while CHELSA data is always in 30 arc-sec. resolution. Therefore, `WorldClim.*.download()`-functions have an additional `resolution` function parameter.
3.  The pre-processing steps consist of two levels. While the processing of the provided data to interpretable values cannot be addressed by the user, clipping needs to be enabled with the eponymous function parameter and specified further with parameters like `clip.shapefile` or `clip.extent` with `buffer` as additional parameter.
4.  File-management addresses the conversion to different file formats, the decluttering of the working directory, and a citation file that includes the providers' relevant publications. Currently, the functions parameter `convert.files.to.asc` and `stacking.data` convert the data to ASCII raster and netCDF format, respectively. For a storage efficient way, the user can decide to delete the downloaded datasets (`delete.raw.data`), save them in a zip-archive file (`combine.raw.zip` (CHELSA), `keep.raw.zip` (WorldClim)), or keep them in the directory as-is. The function parameter `save.bib.file` adds a citation file of the publications from the data provider to the working directory for convenience.

In the following, the basic usage of the download functions are demonstrated by applying an example call of the `Chelsa.Clim.download()` function, aiming at downloading CHELSA V2.1 bioclimatic and precipitation data, with clipping to the European continent's extent, and file management tools:

<!-- prettier-ignore -->

``` r
ClimDatDownloadR::Chelsa.Clim.Download(  
  save.location = "./",  
  version.var = "2.1",  
  parameter = c("bio", "prec"),  
  month.var = c(4:9),  
  clipping = TRUE,  
  clip.extent = c(-5,25,40,62),  
  buffer = 0,  
  clip.shapefile = NULL,  
  convert.files.to.asc = FALSE,  
  stacking.data = FALSE,  
  combine.raw.zip = TRUE,  
  delete.raw.data = TRUE,  
  save.download.table = TRUE,  
  save.bib.file = TRUE  
)
```

## Overview of download-functions

Besides the functions to download the current climatologies of [CHELSA](https://www.chelsa-climate.org/) and [WorldClim](https://www.worldclim.org/), described below as [`Chelsa.Clim.download()`](./man/Chelsa.Clim.download.Rd) and [`WorldClim.HistClim.download()`](./man/WorldClim.HistClim.download.Rd), the package offers more download functions.

-   Beginning with the Last Glacial Maximum-data set (LGM), CHELSA offers a data set with parameters like precipitation, temperature, and also bioclim variables, driven by various models. It can be called with [`Chelsa.lgm.download()`](./man/Chelsa.lgm.download.Rd).
-   [CHELSA's](https://www.chelsa-climate.org/) timeseries dataset can be downloaded via the [`Chelsa.timeseries.download()`](./man/Chelsa.timeseries.download.Rd)-function.
-   For projected climatic conditions both [CHELSA](https://www.chelsa-climate.org/) and [WorldClim](https://www.worldclim.org/) provide various options.
-   [CHELSA's](https://www.chelsa-climate.org/) options can be downloaded through the functions [`Chelsa.CMIP_5.download()`](./man/Chelsa.CMIP_5.download.Rd) and/or [`Chelsa.CRUts.download()`](./man/Chelsa.CRUts.download.Rd).
-   [WorldClim's](https://www.worldclim.org/) options can be downloaded through the functions [`WorldClim.CMIP_5.download()`](./man/WorldClim.CMIP_5.download.Rd) and/or [`WorldClim.CMIP_6.download()`](./man/WorldClim.CMIP_6.download.Rd).

### Table: Overview of the functions and respective datasets {#table-overview-of-the-functions-and-respective-datasets}

<!-- prettier-ignore -->

| Function Name | Accessed Dataset | Version | Reference Period | Emission Scenario | Models | Spatial Resolution | Temporal Resolution | Parameters | Citation | Documentation |
|-------|-------|-------|-------|-------|-------|-------|-------|-------|-------|-------|
| `Chelsa.Clim.download` | CHELSA Climatologies | 1.2, 2.1 | 1.2: 1979–2013; 2.1: 1989-2010 | N/A | N/A | 30 arc-seconds | Monthly | 19 Bioclim. Param., Precip. (sum), Temp. (mean, min, max) | Karger et al. (2017), Karger et al. (2018), Beck et al. (2020) | Karger et al. (2021), Karger and Zimmermann (2019) |
| `Chelsa.timeseries.download` | CHELSA Climatology Timeseries | 1.2, 2.1 | 1.2: 1979–2013; 2.1: 1980-06.2019 | N/A | N/A | 30 arc-seconds | Monthly | 19 Bioclim. Param., Precip. (sum), Temp. (mean, min, max), Potential available transpiration (PET) | Karger et al. (2017), Karger et al. (2018), Beck et al. (2020) | Karger et al. (2021), Karger and Zimmermann (2019) |
| `Chelsa.CRUts.download` | CHELSA CRU Timeseries | 1.2 | 1901-2016 | N/A | N/A | 30 arc-seconds | Monthly | 19 Bioclim. Param., Precip. (sum), Temp. (mean, min, max), Potential available transpiration (PET) | Karger et al. (2017), Karger et al. (2018), Karger and Zimmermann (2018) | (Karger and Zimmermann (2019), p. 23) |
| `Chelsa.CMIP_6.download` | CHELSA CMIP6 dataset | 2.1 | 2011-2040, 2041-2070, and 2071-2100 | SSP scenarios 1 (SSP126), 3 (SSP370), and 5 (SSP585) | Multiple CMIP6 Models [ref. Karger et al. (2021), p. 7] | 30 arc-seconds | Monthly | 19 Bioclim. Param., Precip. (sum), Temp. (mean, min, max) | Karger et al. (2017), Karger et al. (2018) | Karger et al. (2021) |
| `Chelsa.CMIP_5.download` | CHELSA CMIP5 dataset | 1.2 | 2041-2060 and 2061-2080 | RCP 2.6, 4.5, 6.0, and 8.5 | Multiple CMIP5 Models [ref. Karger and Zimmermann (2019), pp. 24] | 30 arc-seconds | Monthly | 19 Bioclim. Param., Precip. (sum), Temp. (mean, min, max) | Karger et al. (2017), Karger et al. (2018) | Karger and Zimmermann (2019) |
| `Chelsa.lgm.download` | CHELSA PMIP3 dataset | 1.2 | 21.000 years before 1950 | N/A | Multiple Paleoclimatic Models [ref. Karger and Zimmermann (2019), p. 25] | 0.5 degree | N/A | 19 Bioclim. Param., Precip. (sum), Temp. (mean, min, max) | Karger et al. (2017), Karger et al. (2018) | Karger and Zimmermann (2019) |
| `WorldClim.HistClim.download` | WorldClim historical climate data | 1.4, 2.1 | 1.4: 1960-1990; 2.1: 1970-2000 | N/A | N/A | 10 minutes to 30 arc-seconds | Monthly | [Bioclim. Param.](https://www.worldclim.org/data/bioclim.html), Precip. (sum), Temp. (mean, min, max), Solar Radiation, Wind Speed, Water Vapor Pressure | Hijmans et al. (2005), Fick and Hijmans (2017) | Version 1.4: [Methods](https://www.worldclim.org/data/v1.4/methods.html), [Data Formats](https://www.worldclim.org/data/v1.4/formats.html); Version 2.1: [Download Page](https://www.worldclim.org/data/worldclim21.html) |
| `WorldClim.CMIP_6.download` | WorldClim CMIP6 dataset | 2.1 | 2021-2040, 2041-2060, 2061-2080, and 2081-2100 | SSP scenarios 1 (SSP126),2 (SSP245), 3 (SSP370), and 5 (SSP585) | Multiple CMIP5 Models, ref. [10-](https://www.worldclim.org/data/cmip6/cmip6_clim10m.html), [5-](https://www.worldclim.org/data/cmip6/cmip6_clim5m.html), [2.5-](https://www.worldclim.org/data/cmip6/cmip6_clim2.5m.html)minutes, and [30-](https://www.worldclim.org/data/cmip6/cmip6_clim30s.html)second download sites | 10 minutes to 30 arc-seconds | Monthly | [Bioclim. Param.](https://www.worldclim.org/data/bioclim.html), Precip. (sum), Temp. (mean, min, max) | Fick and Hijmans (2017) | [Downscaling methods](https://www.worldclim.org/data/downscaling.html) |
| `WorldClim.CMIP_6.download` | WorldClim CMIP5 dataset | 1.4 | 2041-2060, 2061-2080 | rcp26, rcp45, rcp60, and rcp85 | Multiple CMIP5 Models, ref. [10-](https://www.worldclim.org/data/v1.4/cmip5_10m.html), [5-](https://www.worldclim.org/data/v1.4/cmip5_5m.html), [2.5-](https://www.worldclim.org/data/v1.4/cmip5_2.5m.html)minutes, and [30-](https://www.worldclim.org/data/v1.4/cmip5_30s.html)second download sites | 10 minutes to 30 arc-seconds | Monthly | [Bioclim. Param.](https://www.worldclim.org/data/bioclim.html), Precip. (sum), Temp. (mean, min, max) | Hijmans et al. (2005), Fick and Hijmans (2017) | [Downscaling methods](https://www.worldclim.org/data/downscaling.html) |

<details>

<summary>Bibliography</summary>

Beck, Hylke E., Eric F. Wood, Tim R. McVicar, et al. 2020. “Bias Correction of Global High-Resolution Precipitation Climatologies Using Streamflow Observations from 9372 Catchments.” Journal of Climate 33 (4): 1299–315. <https://doi.org/10.1175/jcli-d-19-0332.1>.\
Fick, Stephen E., and Robert J. Hijmans. 2017. “WorldClim 2: New 1‐km Spatial Resolution Climate Surfaces for Global Land Areas.” International Journal of Climatology 37 (12): 4302–15. <https://doi.org/10.1002/joc.5086>.\
Hijmans, Robert J., Susan E. Cameron, Juan L. Parra, Peter G. Jones, and Andy Jarvis. 2005. “Very High Resolution Interpolated Climate Surfaces for Global Land Areas.” International Journal of Climatology 25 (15): 1965–78. <https://doi.org/10.1002/joc.1276>.\
Karger, D. N., Philipp Brun, and Niklaus E. Zimmermann. 2021. CHELSA V2.1: Technical Specification. Swiss Federal Research Institute WSL. <https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf>.\
Karger, D. N., Olaf Conrad, Jürgen Böhner, et al. 2017. “Climatologies at High Resolution for the Earth’s Land Surface Areas.” Scientific Data 4 (1): 170122. <https://doi.org/10.1038/sdata.2017.122>.\
Karger, D. N., Olaf Conrad, Jürgen Böhner, et al. 2018. “Data from: Climatologies at High Resolution for the Earth’s Land Surface Areas.” Dryad. <https://doi.org/10.5061/DRYAD.KD1D4>.\
Karger, D. N., and Niklaus E. Zimmermann. 2018. “CHELSAcruts - High Resolution Temperature and Precipitation Timeseries for the 20th Century and Beyond.” EnviDat. <http://dx.doi.org/10.16904/envidat.159>.\
Karger, D. N., and Niklaus E. Zimmermann. 2019. CHELSA V1.2: Technical Specification. Swiss Federal Research Institute WSL. <https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification.pdf>.

</details>

To get more information, please proceed [here to the documentation](./docs/index.html).

This R-package was developed as a student project for the masters programm Geography at the Universität Hamburg, Germany.

Feedback, support, and ideas for improving your user experience are apprechiated at [**helgejentsch.research\@gmail.com**](mailto:helgejentsch.research@gmail.com).

If you want to cite ClimDatDownloadR you can find the package on Zenodo via [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15663182.svg)](https://doi.org/10.5281/zenodo.15663182)

We would like to be cited as:

Jentsch, Helge, Weidinger, Johannes, & Bobrowski, Maria. (*current year e.g. 2023*). ClimDatDownloadR: Downloads Climate Data from Chelsa and WorldClim (\_current version e.g. 1.0.0). Zenodo. DOI: [10.5281/zenodo.15663182](https://doi.org/10.5281/zenodo.15663182). URL: <https://github.com/HelgeJentsch/ClimDatDownloadR>.

````{=html}
<!-- ## Download Climatologies

In the help pages of [`Chelsa.Clim.download()`](./man/Chelsa.Clim.download.Rd) and [`WorldClim.HistClim.download()`](./man/WorldClim.HistClim.download.Rd) you can find further information about the handling of these functions. In fact running the functions all by itself bulk-downloads all the climatology data sets from the servers to your current working directory.
Let's start with a example of the Chelsa climatologies:

```(r)
Chelsa.Clim.download(
  # first you'll have to choose your working directory
  # don't worry about having a directory for every parameter you want to download
  # ClimDatDownloadR sorts this out for you
  save.location = "./",
  # now you'll have to choose parameters.
  # since there is the possibility to download more than one data set
  # the parameters must be a string-vector input.
  # Single parameters, however, can be just put in as a string.
  # the valid parameter inputs can be found in the help (linked s.o.)
  parameter = c("temp", "bio"),
  # Now, since you chose "temp" and "bio" as input parameters,
  # you can specify the months and bioclim-variables to download.
  # If you want all of them, just leave the default values.
  # It is crutial, however, that the inputs are integer number values.
  month.var = c(1), # Here January was chosen to be downloaded for demonstration purposes
  bio.var =  c(1), # Here the first bioclim-variable was chosen to be downloaded for demonstration purposes
  # For Chelsa a newer Version of their climatologies was published in 2019.
  # They still got their old version still hosted on their website.
  # So you can download it as well, if you want to reproduce some research you base your studies on.
  version.var = "1.2", # Here the newer version is chosen
  # Now you can choose whether you want the data set clipped
  clipping = TRUE, # Here TRUE was chosen to show a basic introduction to the function
  # Since "clipping" is enganged now you can specify the extent you want to have for your analysis
  # This is possible via the parameters "clip.shapefile", "clip.extent", and "buffer"
  clip.extent = c(-9,20,35,80), # Here the extent for Europe was used ...
  buffer = 5, # ... with a 5 arc-degree buffer.
  # Now, since some might prefer older file formats there is a possibility to convert
  # clipped files and raw data into ESRI-ASCII format
  convert.files.to.asc = FALSE,
  # now you can stack the data ...
  stacking.data = FALSE,
  # ... and choose if you want to combine the raw data in a .zip-file ...
  combine.raw.zip = FALSE,
  # and whether raw data should be deleted.
  delete.raw.data = FALSE,
  # Finally you are presented with the option to save a bibliography file at the save location.
  save.bib.file = TRUE
)
```

---

With this showing the basic principle of these functions, here is a example of a WorldClim climatology download:

```(r)
WorldClim.HistClim.download(
  # As you can see, the structure of this function is very similar to the Chelsa-function
  save.location = "./",
  parameter = c("temp", "bio"),
  month.var = c(1),
  bio.var = c(1),
  # Here the resolution of the downloaded data set must be added
  # If no input is given all resolutions will be downloaded
  resolution = "10m", # here 10 arc-minutes are chosen
  # WorldClim also recently had an update to version 2.1
  version.var = "2.1", # Here the newer version is chosen
  clipping = TRUE,
  clip.extent = c(-9,20,35,80),
  buffer = 5,
  convert.files.to.asc = FALSE,
  stacking.data = FALSE,
  # here you can choose if you want to keep the downloaded zip-file
  keep.raw.zip = FALSE,
  delete.raw.data = FALSE,
  save.bib.file = TRUE
)
``` -->
````
