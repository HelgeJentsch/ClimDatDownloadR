## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=FALSE--------------------------------------------------------
# if(!require(remotes)) install.packages("remotes")
# library(remotes)
# remotes::install_github("HelgeJentsch/ClimDatDownloadR")
# library(ClimDatDownloadR)

## ----eval=FALSE---------------------------------------------------------------
# Chelsa.Clim.download(
#   # First you'll have to choose your working directory
#   # don't worry about having a directory for every parameter you want to download
#   # ClimDatDownloadR sorts this out for you.
#   save.location = "./",
#   # Second, specify the version of, in this case, CHELSA you want to download.
#   version.var = "2.1",
#   # Third, you'll have to choose parameters.
#   # Since there is the possibility to download more than one data set
#   # the parameters must be a string-vector input.
#   # Single parameters, however, can be just put in as a string.
#   # the valid parameter inputs can be found in the help (linked s.o.)
#   parameter = c("bio", "prec"),
#   # Now, since you chose "bio" and "prec" as input parameters,
#   # you can specify the months and bioclim-variables to download.
#   # If you want all of them, just leave the default values.
#   # It is crutial, however, that the inputs are integer number values.
#   month.var = c(4:9), # Here January was chosen to be downloaded for demonstration purposes
#   bio.var =  c(1), # Here the first bioclim-variable was chosen to be downloaded for demonstration purposes
#   # For Chelsa a newer Version of their climatologies was published in 2019.
#   # They still got their old version still hosted on their website.
#   # So you can download it as well, if you want to reproduce some research you base your studies on.
#   clipping = TRUE, # Here TRUE was chosen to show a basic introduction to the function
#   # Since "clipping" is enganged now you can specify the extent you want to have for your analysis
#   # This is possible via the parameters "clip.shapefile", "clip.extent", and "buffer"
#   clip.extent = c(-5,25,40,62), # Here the extent for Europe was used.
#   # If you work in a specific region and need data just for that region,
#   # you can specify the region with:
#   clip.shapefile = NULL, # in this case no shapefile is used.
#   # If you wish to extend the extent of the shapefile,
#   # you can set a buffer here:
#   buffer = 0,
#   # Now, since some might prefer older file formats there is a possibility to convert
#   # clipped files and raw data into ESRI-ASCII format
#   convert.files.to.asc = FALSE,
#   # now you can stack the data ...
#   stacking.data = FALSE,
#   # ... and choose if you want to combine the raw data in a .zip-file ...
#   combine.raw.zip = FALSE,
#   # and whether raw data should be deleted.
#   delete.raw.data = FALSE,
#   # Finally you are presented with the option to save a bibliography file at the save location.
#   save.bib.file = TRUE
# )

