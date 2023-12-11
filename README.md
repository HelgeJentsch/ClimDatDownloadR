## Readme - Welcome to the ClimDatDownloadR - Update 05.09.2023

Hello and welcome to the ClimDatDownloadR R-package.

With this package **cli**mate **dat**asets provided by [Chelsa](http://chelsa-climate.org/) and [WorldClim](https://www.worldclim.org/) can be automatically **download**ed, clipped, and converted with **R**.

To start, you'll have to install the package and it's dependencies first, if not already done. Then you can activate the package with the `library`-function.\
For the installation process you need to need to input the following code:

``` (r)
if(!require(devtools)) install.packages("devtools")
library(devtools)
devtools::install_github("HelgeJentsch/ClimDatDownloadR")
library(ClimDatDownloadR)
```

Very well, now that you have the package installed and attached, let's start with the data sets of the climatologies of Chelsa and WorldClim.

If you have any feedback or are in need of support, you can reach me via [**helgejentsch.research\@gmail.com**](mailto:helgejentsch.research@gmail.com){.email}. Feedback and ideas for improving your user experience are always apprechiated!

## Overview of download-functions

Besides the functions to download the currend climatologies of [Chelsa](http://chelsa-climate.org/) and [WorldClim](https://www.worldclim.org/), described below as [`Chelsa.Clim.download()`](./man/Chelsa.Clim.download.Rd) and [`WorldClim.HistClim.download()`](./man/WorldClim.HistClim.download.Rd), the package offers more download functions.\
- Beginning with the 'Last Glacial Maximum'-data set (LGM), Chelsa offers a data set with parameters like precipitation, temperature, and also bioclim variables, driven by various models. It can be called with [`Chelsa.lgm.download()`](./man/Chelsa.lgm.download.Rd).\
- [Chelsa's](http://chelsa-climate.org/) timeseries dataset can be downloaded via the [`Chelsa.timeseries.download()`](./man/Chelsa.timeseries.download.Rd)-function.\
- For projected climatic conditions both [Chelsa](http://chelsa-climate.org/) and [WorldClim](https://www.worldclim.org/) provide various options. - [Chelsa's](http://chelsa-climate.org/) options can be downloaded through the functions [`Chelsa.CMIP_5.download()`](./man/Chelsa.CMIP_5.download.Rd) and/or [`Chelsa.CRUts.download()`](./man/Chelsa.CRUts.download.Rd). - [WorldClim's](https://www.worldclim.org/) options can be downloaded through the functions [`WorldClim.CMIP_5.download()`](./man/WorldClim.CMIP_5.download.Rd) and/or [`WorldClim.CMIP_6.download()`](./man/WorldClim.CMIP_6.download.Rd).

## Download Climatologies

In the help pages of [`Chelsa.Clim.download()`](./man/Chelsa.Clim.download.Rd) and [`WorldClim.HistClim.download()`](./man/WorldClim.HistClim.download.Rd) you can find further information about the handling of these functions. In fact running the functions all by itself bulk-downloads all the climatology data sets from the servers to your current working directory.\
Let's start with a example of the Chelsa climatologies:

``` (r)
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

------------------------------------------------------------------------

With this showing the basic principle of these functions, here is a example of a WorldClim climatology download:

``` (r)
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
```

To get more information, please proceed [here](./articles/ClimDatDownloadR.html).

This R-package was developed as a student project for the masters programm Geography at the Universität Hamburg, Germany.

Feedback, support, and ideas for improving your user experience are apprechiated at [**helgejentsch.research\@gmail.com**](mailto:helgejentsch.research@gmail.com){.email}.

If you want to cite ClimDatDownloadR you can find the package on Zenodo via [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7924343.svg)](https://doi.org/10.5281/zenodo.7924343)

We would like to be cited as:

Jentsch, Helge, Weidinger, Johannes, & Bobrowski, Maria. (*current year e.g. 2023*). ClimDatDownloadR: Downloads Climate Data from Chelsa and WorldClim (*current version e.g. 0.1.7-4*). Zenodo. DOI: [10.5281/zenodo.7924343](http://doi.org/10.5281/zenodo.7924343). URL: <https://github.com/HelgeJentsch/ClimDatDownloadR>.
