#'@title Function for downloading the CHELSA climate dataset (1979-2013)
#'@author Helge Jentsch
#'@description This function supports the download, pre-processing and management of CHELSA climate data comprising of monthly precipitation sums in mm, monthly temperature (average, minimum, maximum) in degrees Celsius, and annual chracteristics (19 bioclimatic variables). The spatial resolution of the downloaded data is 30 arc-seconds.\cr To allow pre-processing, clipping and buffering, conversion to ASCII-grids and stacking options are included.\cr Optional an output of a .bib-file of the cited literature can be retrieved.\cr For user convenience, saving directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'@param save.location string. Input where the datasets will be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "temp", "tmax", "tmin", "bio")}
#'@param bio.var integer (vector). Input which monthly data should be downloaded. Only applicable to BIOCLIM variables. For further information see: \url{http://chelsa-climate.org/bioclim/}. \cr Default: \code{c(1:19)}
#'@param month.var integer (vector). Input which monthly data should be downloaded. Only applicable to precipitation and temperature (average, maximum, minimum). \cr Default: \code{c(1:12)}
#'@param version.var string (vector). Input which version of the dataset should be downloaded. Multiple selection is possible. \cr Default:  \code{c("1.2")}
#'@param clipping logical. Input whether the downloaded data should be clipped.\cr If \code{FALSE}: clip.shapefile, buffer, clip.extent will be ignored. \cr Default: \code{FALSE}
#'@param clip.shapefile string. Input which shapefile should be used for clipping. \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop"). \cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether files should be converted into the ASCII format.\cr If \code{TRUE}: a new subdirectory is created and the rawdata is saved there. If \code{clipping} is \code{TRUE}: the clipped raster files are also saved as ASCII grids. \cr  Default: \code{FALSE}
#'@param stacking.data logical. Input whether the downloaded data should be stacked as a netCDF-rasterstack. \cr Default: \code{FALSE}
#'@param combine.raw.zip logical. Should the downloaded raw-data be "zipped". \cr Default: \code{FALSE}
#'@param delete.raw.data  logical. Should the downloaded raw-data be deleted.\cr If \code{combine.raw.zip} is \code{TRUE}: raw-data is still available in the zipped file. \cr Default: \code{FALSE}
#'@param save.bib.file logical. Whether a BibTex-citation file of the dataset should be provided in the Working directory. \cr Default: \code{TRUE}
#'@return CHELSA climate datasets for the period of 1979 - 2013
#'
#'@note Please note that the downloaded data for temperature and the therefore also the first eleven bioclim-variables are processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time.
#'
#'@references D. N. Karger, O. Conrad, J. B{\"o}hner , et al. "Climatologies at high resolution for the earth's land surface areas". In: _Scientific Data_ 4.1 (Sep. 2017). DOI: 10.1038/sdata.2017.122. <URL: https://doi.org/10.1038/sdata.2017.122>.
#'@references D. N. Karger, O. Conrad, J. B{\"o}hner , et al. _Data from: Climatologies at high resolution for the earth's land surface areas_. En. 2018. DOI: 10.5061/DRYAD.KD1D4. <URL: http://datadryad.org/stash/dataset/doi:10.5061/dryad.kd1d4>.
#'
#'@examples
#' \dontrun{
#' # Bioclim
#' Chelsa.Clim.download(parameter = "bio", bio.var = c(1,19))
#' # Precipitation
#' Chelsa.Clim.download(parameter = "prec", month.var = c(1,12))
#' }
#'
#'@import stringr
#'@import RCurl
#'@import ncdf4
#'@import raster
#'@importFrom utils unzip download.file
#'
#'
#'@export
Chelsa.Clim.download <- function(save.location = "./",
                                 parameter = c("prec", "temp", "tmax", "tmin", "bio"),
                                 bio.var = c(1:19),
                                 month.var = c(1:12),
                                 version.var = c("1.2"),
                                 clipping = FALSE,
                                 clip.shapefile = NULL,
                                 clip.extent = c(-180, 180, -90, 90),
                                 buffer = 0,
                                 convert.files.to.asc = FALSE,
                                 stacking.data = FALSE,
                                 combine.raw.zip = FALSE,
                                 delete.raw.data  = FALSE,
                                 save.bib.file = TRUE){
  gc()
  call.time <- stringr::str_replace_all(
    stringr::str_replace_all(paste0(Sys.time()), 
                             pattern = ":",
                             replacement = "-"), 
    pattern = " ", 
    replacement = "_")
  # initial check -----------------------------------------------------------
  # normalize Path for easier application later
  save.location <- normalizePath(save.location, winslash = "/")
  # Check which parameters are put in and if the connected
  # month/bio-variables are correctly input
  if(is.element("prec", parameter)|is.element("temp", parameter)|
     is.element("tmax", parameter)|is.element("tmin", parameter)){
    # if month.var is just a single numeric input it is here casted into
    # a vector for comparabilities
    month.var <- c(month.var)
    # if there is not a numeric input -> prompt error
    if(!is.numeric(month.var)) stop()
    # Padding of "one-digit" months with a 0
    month.var <- stringr::str_pad(month.var, 2, 'left', pad = "0")
  }
  
  # analog to the if-clause before - here the parameter bio.var is checked.
  if(is.element("bio", parameter)){
    bio.var <- c(bio.var)
    if(!is.numeric(bio.var)) stop()
    bio.var <- stringr::str_pad(bio.var, 2, 'left', pad = "0")
  }
  # parameter
  parameter <- sort(parameter)
  DLTparameter <- c(rep(parameter[parameter!="bio"], length(month.var)), rep(parameter[parameter=="bio"], length(bio.var)))
  DLTparameter <- sort(DLTparameter)
  # variables 
  DLTvariable <- NULL
  for(parm in parameter){
    DLTvariable <- c(DLTvariable, switch(parm, 
                                         "prec" = month.var,
                                         "tmax" = month.var,
                                         "temp" = month.var,
                                         "tmin" = month.var, 
                                         bio.var)
    )
  }
  
  # print(length(DLTvariable));print(length(DLTparameter))
  dataDF <- data.frame("parameter" = DLTparameter, 
                       "variable" = DLTvariable, 
                       "version" = rep(version.var, length(DLTvariable)))
  
  dataDF$parmLong <- paste0(dataDF$parameter,"10")
  dataDF$parmLong[dataDF$parameter == "prec"] <- paste0("prec")
  dataDF$parameter[dataDF$parameter == "temp"] <- paste0("tmean")
  
  dataDF$years <- paste0("_1979-2013")
  dataDF$years[dataDF$parameter == "prec" | dataDF$parameter == "bio"] <- paste0("")
  
  dataDF$URL[dataDF$version == "1.2"]  <- 
    paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/",
           dataDF$parameter, 
           "/CHELSA_", dataDF$parmLong , "_", dataDF$variable, dataDF$years,
           "_V1.2_land.tif")
  dataDF$URL[dataDF$version == "1.2" & dataDF$parameter == "bio"]  <- 
    paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/",
           dataDF$parameter[dataDF$version == "1.2" & dataDF$parameter == "bio"], 
           "/CHELSA_",
           dataDF$parmLong[dataDF$version == "1.2" & dataDF$parameter == "bio"] ,
           "_", 
           dataDF$variable[dataDF$version == "1.2" & dataDF$parameter == "bio"], 
           dataDF$years[dataDF$version == "1.2" & dataDF$parameter == "bio"],
           ".tif")
  # print(dataDF$URL)
  for(urlexists in dataDF$URL){
    if(!url.exists(urlexists)){
      cat(paste(urlexists, 
                " does not exist, please check the website of Chelsa.",
                "\n We would greatly apprecheate feedback on this at helge.marc.ole.jentsch@uni-hamburg.de")
      )
      next # stop()
    }
  }
  print(paste0(getDownloadSize(dataDF$URL), " MB will be downloaded."))
  # Progressbar setup
  PGBsum <- nrow(dataDF) + length(unique(dataDF$parameter)) + 1
  PGB <- txtProgressBar(min = 0, max = PGBsum, style = 3)
  PGBstate <- 0
  
  locationSack <- NULL
  for(parm in dataDF$parameter){
    if (!dir.exists(paste0(save.location, "/", parm))){
      dir.create(paste0(save.location, "/", parm))
    }
    if("1.2" %in% dataDF$version){
      if (!dir.exists(paste0(save.location, "/", parm, "/ChelsaV1.2Climatologies"))){
        dir.create(paste0(save.location, "/", parm, "/ChelsaV1.2Climatologies"))
      }
      locationSack <- c(locationSack, paste0(save.location, "/", parm, "/ChelsaV1.2Climatologies/"))
    }
  }
  # print(locationSack)
  dataDF$filepath[dataDF$version == "1.2"]  <- 
    paste0(save.location,"/",
           dataDF$parameter, "/ChelsaV1.2Climatologies", 
           "/CHELSA_", dataDF$parmLong , "_", dataDF$variable, dataDF$years,
           "_V1.2.tif")
  for(fileexists in dataDF$filepath){
    if(!file.exists(fileexists)){
      unlink(list.files(tempdir(), recursive = TRUE, full.names = TRUE))
      download.file(url = dataDF$URL[dataDF$filepath == fileexists],
                    destfile = fileexists,
                    # overwrite is TRUE otherwise a error is caused
                    overwrite = TRUE,
                    # From the description file:
                    # The choice of binary transfer (mode = "wb" or "ab")
                    # is important on Windows, since unlike Unix-alikes
                    # it does distinguish between text and binary files and
                    # for text transfers changes
                    # \n line endings to \r\n (aka ‘CRLF’).
                    mode = 'wb',
                    # to show progression bar
                    quiet = TRUE,
                    cacheOK = FALSE)
    }
    setTxtProgressBar(PGB, PGBstate+1)
    PGBstate <- PGBstate+1
  }
  rescaleDF <- dataDF[dataDF$parameter != "prec",]
  rescaleDF <- rescaleDF[!(rescaleDF$parameter == "bio" & as.numeric(rescaleDF$variable) > 12),]
  # for(prepro in dataDF$filepath){
  #   gc()
  #   preproRaster <- raster(prepro)
  #   gc()
  #   preproRaster <- clamp(preproRaster, lower = -1000,
  #                        useValues = FALSE)
  #   if(prepro %in% rescaleDF$filepath){
  #     print(Sys.time())
  #     gain(preproRaster) <- 0.1
  #     print(Sys.time())
  #   }
  #   gc()
  #   writeRaster(x = preproRaster, filename = prepro, overwrite = T)
  # }
  locationSack <- unique(locationSack)
  for (temp.temp.save.location in locationSack) {
    run <- grep(temp.temp.save.location, locationSack)
    for(i in run){
      # print(i)
      # print(parameter[i])
      variable.numbers <- switch(parameter[i],
                                 "bio" = bio.var,
                                 month.var)
      # if clipping is TRUE ...
      if(clipping == TRUE){
        # the function "clipping.tif" (found in the auxiliary.R-File)
        # is executed. The clip.save.location is the same location as the
        # "current" save location
        clipping.tif(clip.save.location = temp.temp.save.location,
                     # the clip-shapefile is passed
                     # default "NULL" does not produce error
                     clip.shapefile = clip.shapefile,
                     # Clip.extent is passed
                     # default "c(-180, 180, -90, 90)" does not produce errors
                     # simply clips the whole world.
                     clip.extent = clip.extent,
                     # buffer is passed
                     # default: 0. Unit is arc-degrees
                     buffer = buffer,
                     # conversion to ASCII format here integrated into the
                     # clipping function. Since it can be assumed that
                     # they should be converted lateron anyway.
                     convert.files.to.asc = convert.files.to.asc,
                     time.stamp.var = call.time)
      }
      # if converting.files.to.asc is TRUE ...
      if(convert.files.to.asc == TRUE){
        # the function "convert.to.asc" (found in the auxiliary.R-File)
        # is executed. The save.location is the same location as the
        # "current" save location. Also another new subdirectory will
        # be created with the name "ASCII" .
        convert.to.asc(save.location = temp.temp.save.location,
                       time.stamp.var = call.time)
      }
      # if stacking.data is TRUE ...
      if(stacking.data == TRUE){
        # the function "stacking.downloaded.data"
        # (found in the auxiliary.R-File) is executed.
        # The save.location is the same location as the
        # "current" save location.
        if(clipping==TRUE){
          stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                   parameter.var = parameter[i],
                                   variable.numbers = variable.numbers,
                                   stack.clipped = TRUE,
                                   time.stamp.var = call.time)
        }else{
          stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                   parameter.var = parameter[i],
                                   variable.numbers = variable.numbers,
                                   time.stamp.var = call.time)
        }
      }
      # if combine.raw.zip is TRUE ...
      if(combine.raw.zip == TRUE){
        # the function "combine.raw.in.zip"
        # (found in the auxiliary.R-File) is executed.
        # The save.location is the same location as the
        # "current" save location. The name of the zip-file is also
        # passed with the current parameter in it.
        combine.raw.in.zip(save.location = temp.temp.save.location,
                           zip.name = paste0("CHELSAClim_", parameter[i], ""),
                           time.stamp.var = call.time)
      }
      # if delete.raw.data is TRUE ...
      if(delete.raw.data == TRUE){
        # All .tif raster files in the current 2nd order subdirectory are
        # unlinked (deleted).
        unlink(list.files(temp.temp.save.location,
                          pattern = ".tif",
                          include.dirs = FALSE,
                          full.names = T),
               force = TRUE)
      }
    }
    # delete all temporary files
    unlink(list.files(tempdir(), recursive = T, full.names =T))
    setTxtProgressBar(PGB, PGBstate+1)
    PGBstate <- PGBstate+1
  }
  if(save.bib.file == TRUE) {
    save.citation(save.location = save.location, dataSetName = "CHELSA")
  }
  setTxtProgressBar(PGB, PGBstate+1)
  close(PGB)
  # delete all temporary files
  unlink(list.files(tempdir(), recursive = T, full.names =T))
  # print(dataDF)
  
  # print(getDownloadSize(dataDF$URL))
}
# Chelsa.Clim.download(save.location = "../Daten/")
# Chelsa.Clim.download(parameter = c("tmin", "prec", "bio"), month.var = c(1,4,7), bio.var = c(1,13,14,17))
# Chelsa.Clim.download(save.location = "../testing/",
#                      parameter = c("tmin", "bio"), month.var = c(1,4,7), bio.var = c(1,13,14,17))
# Chelsa.Clim.download(save.location = "../testing/",
# parameter = c("tmin", "bio"), month.var = c(8), bio.var = c(19), clipping = T, clip.extent = c(5,10,50,55))
# Chelsa.Clim.download("../testing/", parameter = c("prec", "temp", "bio"),
#                      bio.var = c(1,12), month.var = c(1,12),
#                      clipping = T, clip.extent = c(8,10,50,56),
#                      combine.raw.zip = T)