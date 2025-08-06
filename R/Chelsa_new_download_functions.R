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
#'@param save.download.table logical. Whether a table containing the download infos should be saved. \cr Default: \code{TRUE}
#'@return CHELSA climate datasets for the period of 1979 - 2013
#'
#'@note Please note that the downloaded data for temperature and the first eleven bioclim-variables are processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time. 
#'
#'@references D. N. Karger, O. Conrad, J. Böhner , et al. _Climatologies at high resolution for the earth's land surface areas_. In: _Scientific Data_ 4.1 (Sep. 2017). DOI: 10.1038/sdata.2017.122. <URL: https://doi.org/10.1038/sdata.2017.122>.
#'@references D. N. Karger, O. Conrad, J. Böhner , et al. _Data from: Climatologies at high resolution for the earth's land surface areas_. En. 2018. DOI: 10.5061/DRYAD.KD1D4. <URL: http://datadryad.org/stash/dataset/doi:10.5061/dryad.kd1d4>.
#'@references D. N. Karger, O. Conrad, J. Böhner , et al. _Climatologies at high resolution for the earth’s land surface areas_ EnviDat. (2021) DOI: 10.16904/envidat.228. <URL: https://www.doi.org/10.16904/envidat.228>.
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
#'@import terra
#'@importFrom utils unzip download.file txtProgressBar write.table
#'
#'
#'@export
Chelsa.Clim.download <- function(save.location = "./",
                                 parameter = c("prec", "temp", "tmax", "tmin", "bio"),
                                 bio.var = c(1:19),
                                 month.var = c(1:12),
                                 version.var = c("1.2", "2.1"),
                                 clipping = FALSE,
                                 clip.shapefile = NULL,
                                 clip.extent = c(-180, 180, -90, 90),
                                 buffer = 0,
                                 convert.files.to.asc = FALSE,
                                 stacking.data = FALSE,
                                 combine.raw.zip = FALSE,
                                 delete.raw.data  = FALSE,
                                 save.bib.file = TRUE, 
                                 save.download.table = TRUE){
  gc()
  call.time <- stringr::str_replace_all(
    stringr::str_replace_all(
      stringr::str_split(string = paste0(Sys.time()), 
                         pattern = "\\.")[[1]][1], 
      pattern = ":",
      replacement = "-"), 
    pattern = " ", 
    replacement = "_")
  # initial check -----------------------------------------------------------
  # normalize Path for easier application later
  save.location <- normalizePath(save.location, winslash = "/")
  # Check which parameters are put in and if the connected
  # month/bio-variables are correctly input
  if(base::is.element("prec", parameter)|base::is.element("temp", parameter)|
     base::is.element("tmax", parameter)|base::is.element("tmin", parameter)){
    # if month.var is just a single numeric input it is here casted into
    # a vector for comparabilities
    month.var <- c(month.var)
    # if there is not a numeric input -> prompt error
    if(!is.numeric(month.var)) stop()
    # Padding of "one-digit" months with a 0
    month.var <- stringr::str_pad(month.var, 2, 'left', pad = "0")
  }
  
  # analog to the if-clause before - here the parameter bio.var is checked.
  if(base::is.element("bio", parameter)){
    bio.var <- c(bio.var)
    if(!is.numeric(bio.var)) stop()
    bio.var <- stringr::str_pad(bio.var, 2, 'left', pad = "0")
  }
  
  # Preparations ------------------------------------------------------------
  # parameter
  parameter <- base::sort(parameter)
  DLTparameter <- c(base::rep(parameter[parameter!="bio"], 
                              base::length(month.var)), 
                    base::rep(parameter[parameter=="bio"], 
                              base::length(bio.var)))
  DLTparameter <- base::sort(DLTparameter)
  # variables 
  DLTvariable <- NULL
  for(parm in parameter){
    DLTvariable <- c(DLTvariable, 
                     switch(parm, 
                            "prec" = month.var,
                            "tmax" = month.var,
                            "temp" = month.var,
                            "tmin" = month.var, 
                            bio.var
                     )
    )
    
  }
  
  # Combine search into large dataframe -------------------------------------
  dataDF <- data.frame("parameter" = sort(DLTparameter), 
                       "variable" = DLTvariable
  )
  if(length(version.var)==1){
    dataDF$version <- base::rep(version.var, length(DLTvariable))
  }else{
    dataDF <- data.frame("parameter" = rep(dataDF$parameter,2) , 
                         "variable" = rep(dataDF$variable, 2) ,  
                         "version" = NA)
    temp_version <- c()
    for(i_version in version.var){
      temp_version <- c(temp_version,base::rep(i_version, length(DLTvariable)))
    }
    dataDF$version <- temp_version
    rm(temp_version)
  }
  
  # v1.2
  if(is.element("1.2", dataDF$version)){  
    dataDF$parmLong[dataDF$version == "1.2"] <- base::paste0(dataDF$parameter[dataDF$version == "1.2"],"10")
    dataDF$parmLong[dataDF$version == "1.2" & 
                      dataDF$parameter == "prec"] <- base::paste0("prec")
    dataDF$parameter[dataDF$version == "1.2" &
                       dataDF$parameter == "temp"] <- base::paste0("tmean")
    dataDF$parmLong[dataDF$version == "1.2" &
                      dataDF$parameter == "tmean"] <- base::paste0("temp10")
    dataDF$years[dataDF$version =="1.2"] <- "_1979-2013"
    dataDF$years[dataDF$version == "1.2" & 
                   (dataDF$parameter == "prec" | dataDF$parameter == "bio")] <- base::paste0("")
    # Adding the URL stings
    dataDF$URL[dataDF$version == "1.2" & dataDF$parameter != "bio"]  <-  
      paste0("https://os.zhdk.cloud.switch.ch/chelsav1/climatologies/",
             dataDF$parameter[dataDF$version == "1.2" & dataDF$parameter != "bio"], 
             "/CHELSA_", dataDF$parmLong[dataDF$version == "1.2" & dataDF$parameter != "bio"] , 
             "_", dataDF$variable[dataDF$version == "1.2" & dataDF$parameter != "bio"], 
             dataDF$years[dataDF$version == "1.2" & dataDF$parameter != "bio"],
             "_V1.2_land.tif")
    dataDF$URL[dataDF$version == "1.2" & dataDF$parameter == "bio"]  <- 
      paste0("https://os.zhdk.cloud.switch.ch/chelsav1/climatologies/",
             dataDF$parameter[dataDF$version == "1.2" & dataDF$parameter == "bio"], 
             "/CHELSA_",
             dataDF$parmLong[dataDF$version == "1.2" & dataDF$parameter == "bio"] ,
             "_", 
             dataDF$variable[dataDF$version == "1.2" & dataDF$parameter == "bio"], 
             dataDF$years[dataDF$version == "1.2" & dataDF$parameter == "bio"],
             ".tif")
  }
  
  # v2.1
  if(is.element("2.1", dataDF$version)){ 
    dataDF$parmLong[dataDF$version == "2.1" & 
                      dataDF$parameter == "prec"] <- base::paste0("pr")
    dataDF$parmLong[dataDF$version == "2.1" & 
                      dataDF$parameter == "temp"] <- base::paste0("tas")
    dataDF$parmLong[dataDF$version == "2.1" & 
                      dataDF$parameter == "tmin"] <- base::paste0("tasmin")
    dataDF$parmLong[dataDF$version == "2.1" & 
                      dataDF$parameter == "tmax"] <- base::paste0("tasmax")
    dataDF$parmLong[dataDF$version == "2.1" & 
                      dataDF$parameter == "bio"] <- base::paste0("bio")
    
    dataDF$years[dataDF$version =="2.1"] <- "_1981-2010"
    
    # print(dataDF[dataDF$version == "2.1" & dataDF$parameter != "bio", ])
    
    dataDF$URL[dataDF$version == "2.1" & dataDF$parameter != "bio"]  <-
      paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/",
             dataDF$parmLong[dataDF$version == "2.1" & dataDF$parameter != "bio"],
             "/CHELSA_", 
             dataDF$parmLong[dataDF$version == "2.1" & dataDF$parameter != "bio"] ,
             "_", dataDF$variable[dataDF$version == "2.1" & dataDF$parameter != "bio"] , 
             dataDF$years[dataDF$version == "2.1" & dataDF$parameter != "bio"] ,
             "_V.2.1.tif")
    
    dataDF$URL[dataDF$version == "2.1" & dataDF$parameter == "bio"]  <- 
      paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/",
             dataDF$parameter[dataDF$version == "2.1" & dataDF$parameter == "bio"], 
             "/CHELSA_",
             dataDF$parameter[dataDF$version == "2.1" & dataDF$parameter == "bio"] ,
             "", 
             as.integer(dataDF$variable[dataDF$version == "2.1" & dataDF$parameter == "bio"]), 
             dataDF$years[dataDF$version == "2.1" & dataDF$parameter == "bio"],
             "_V.2.1.tif")
  }
  
  # here might be a good place for another error-check
  
  
  if(save.download.table){
    write.table(x = dataDF, 
                file = paste0(normalizePath(save.location, winslash = "/"), "/", call.time, "_downloadDataframe.csv"), 
                sep = ";", 
                dec = ".", 
                row.names = F, 
                append = F)
  }
  # Check if URL exists!
  for(urlexists in dataDF$URL){ # loop through all URLs
    if(!RCurl::url.exists(urlexists)){ # if not, print warning!
      if(urlexists == dataDF$URL[1]){
        cat(paste("\t If any of these download warnings was prompted incorrectly, we apprecheate a feedback on this at helgejentsch.research@gmail.com \n")
        )}
      cat(paste(urlexists, 
                " does not exist, please check the website of Chelsa. \n")
      )
      next 
    }
  }
  
  # print the amount of data to be downloaded and processed.
  print(paste0(getDownloadSize(dataDF$URL), " MB will be downloaded."))
  # Progressbar setup
  PGBsum <- nrow(dataDF) + 
    length(unique(dataDF$parameter)) + 
    1 + # ?!
    1 + # Location Check
    1 + # Identifier column 
    1 + # save of dataDF
    1  # rescale
  PGB <- utils::txtProgressBar(min = 0, max = PGBsum, style = 3)
  PGBstate <- 0
  # Preparation of save location stack 
  locationSack <- NULL
  # loop through every instance and save the location. 
  # HINT FOR RUNTIME IMPROVEMENT!!!
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
    if("2.1" %in% dataDF$version){
      if (!dir.exists(paste0(save.location, "/", parm, "/ChelsaV2.1Climatologies"))){
        dir.create(paste0(save.location, "/", parm, "/ChelsaV2.1Climatologies"))
      }
      locationSack <- c(locationSack, paste0(save.location, "/", parm, "/ChelsaV2.1Climatologies/"))
    }
  }
  setTxtProgressBar(PGB, PGBstate+1)
  PGBstate <- PGBstate+1
  # print(locationSack)
  dataDF$filepath[dataDF$version == "1.2"]  <- 
    paste0(save.location,"/",
           dataDF$parameter, "/ChelsaV1.2Climatologies", 
           "/CHELSA_", dataDF$parmLong , "_", dataDF$variable, dataDF$years,
           "_V1.2.tif")
  dataDF$filepath[dataDF$version == "2.1"]  <- 
    paste0(save.location,"/",
           dataDF$parameter, "/ChelsaV2.1Climatologies", 
           "/CHELSA_", dataDF$parmLong , "_", dataDF$variable, dataDF$years,
           "_V2.1.tif")
  
  dataDF$fileExisted <- FALSE
  setTxtProgressBar(PGB, PGBstate+1)
  PGBstate <- PGBstate+1
  
  # check for file existance - if not already present - download file 
  for(fileexists in dataDF$filepath){
    if(!file.exists(fileexists)){
      unlink(list.files(tempdir(), recursive = TRUE, full.names = TRUE))
      warning("If download fails reset the timeout option trough your console: e.g. options(timeout=3600)")
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
    }else{
      dataDF$fileExisted[dataDF$filepath == fileexists] <- TRUE
    }
    unlink(list.files(tempdir(), recursive = TRUE, full.names = TRUE))
    setTxtProgressBar(PGB, PGBstate+1)
    PGBstate <- PGBstate+1
  }
  
  if(save.download.table){
    write.table(x = dataDF, 
                file = paste0(normalizePath(save.location, winslash = "/"), "/", call.time, "_downloadDataframe.csv"), 
                sep = ";", 
                dec = ".", 
                row.names = F, 
                append = F)
  }
  setTxtProgressBar(PGB, PGBstate+1)
  PGBstate <- PGBstate+1
  
  # processing of raster into double conversion! 
  #        1.2       2.1
  #  prec  -         x
  #  temp  x         x (K)
  #  tmax  x         x (K)
  #  tmin  x         x (K)
  #  bio   (<12)x    x (K e c(1,5,6, 8:11))
  if(is.element("1.2", dataDF$version)){ 
    # https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification.pdf
    rescaleDF_V12 <- dataDF[dataDF$version == "1.2" & 
                              dataDF$parameter != "prec" &
                              !dataDF$fileExisted
                            ,]
    rescaleDF_V12 <- rescaleDF_V12[!(rescaleDF_V12$parameter == "bio" & 
                                       as.numeric(rescaleDF_V12$variable) > 12)
                                   ,]
    
    if(nrow(rescaleDF_V12)>0){
      for(rescale_i in 1:nrow(rescaleDF_V12)){
        gc()
        tempRast <- terra::rast(rescaleDF_V12$filepath[rescale_i])
        tempRast <- process.raster.int.doub(tempRast)
        tempFilePath <- tempfile(tmpdir = tempdir(), fileext = ".tif")
        terra::writeRaster(x = tempRast,
                           filename = tempFilePath
        )
        terra::writeRaster(x = terra::rast(x = tempFilePath),
                           filename = rescaleDF_V12$filepath[rescale_i], 
                           overwrite = TRUE)
        rm(tempFilePath)
        gc()
        unlink(list.files(tempdir(), recursive = T, full.names =T))
      }
      rm(rescale_i)
      unlink(list.files(tempdir(), recursive = T, full.names =T))
    }
  }
  if(is.element("2.1", dataDF$version)){ 
    # https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf
    
    # rescale
    rescaleDF_V21 <- dataDF[dataDF$version == "2.1"&
                              !dataDF$fileExisted,]
    if(nrow(rescaleDF_V21)>0){
      for(rescale_i in 1:nrow(rescaleDF_V21)){
        gc()
        tempRast <- terra::rast(rescaleDF_V21$filepath[rescale_i])
        tempRast <- process.raster.int.doub(tempRast)
        tempFilePath <- tempfile(tmpdir = tempdir(), fileext = ".tif")
        terra::writeRaster(x = tempRast,
                           filename = tempFilePath
        )
        terra::writeRaster(x = terra::rast(x = tempFilePath),
                           filename = rescaleDF_V21$filepath[rescale_i], 
                           overwrite = TRUE)
        unlink(tempFilePath)
        rm(tempFilePath)
        gc()
      }
      rm(rescale_i)
      unlink(list.files(tempdir(), recursive = T, full.names =T))
    }
    
    # offset
    offsetDF_V21 <- dataDF[dataDF$version == "2.1" & 
                             dataDF$parameter != "prec"&
                             !dataDF$fileExisted,]
    offsetDF_V21 <- offsetDF_V21[
      !(offsetDF_V21$parameter == "bio" &
          is.element(set = c(2:4,7,12:19), 
                     el = as.numeric(offsetDF_V21$variable))
      )
      ,]
    if(nrow(offsetDF_V21)>0){
      for(rescale_i in 1:nrow(offsetDF_V21)){
        gc()
        tempRast <- terra::rast(offsetDF_V21$filepath[rescale_i])
        tempRast <- process.raster.offset(tempRast, offset = 0)
        tempFilePath <- tempfile(tmpdir = tempdir(), fileext = ".tif")
        terra::writeRaster(x = tempRast,
                           filename = tempFilePath
        )
        terra::writeRaster(x = terra::rast(x = tempFilePath),
                           filename = offsetDF_V21$filepath[rescale_i], 
                           overwrite = TRUE)
        unlink(tempFilePath)
        rm(tempFilePath)
        gc()
      }
      rm(rescale_i)
      unlink(list.files(tempdir(), recursive = T, full.names =T))
    }
  }
  setTxtProgressBar(PGB, PGBstate+1)
  PGBstate <- PGBstate+1
  
  locationSack <- unique(locationSack)
  for (temp.temp.save.location in locationSack) {
    run <- grep(temp.temp.save.location, locationSack)
    for(i in run){
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
                                   stack.clipped = FALSE,
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


#'@title Function for downloading CHELSA CMIP 6 future climatologies for the years 2011-2040, 2041-2070, and 2071-2100
#'@author Helge Jentsch
#'@description This function supports the download of CHELSA CMIP6 future climate scenarios comprising of monthly precipitation sums in mm, monthly temperature (average, minimum, maximum) in degrees Celsius, and annual chracteristics (19 bioclimatic variables).\cr To allow pre-processing, clipping and buffering, conversion to ASCII-grids and stacking options are included.\cr Optional an output of a .bib-file of the cited literature can be retrieved.\cr For user convenience, saving directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'@details "The downscaled data has been produced using climatological aided interpolation based on the 1979-2013 reference climatologies from CHELSA." (CHELSA Climate 2020: \url{http://chelsa-climate.org/future/})
#'
#'@note Please note that the downloaded data for temperature and the therefore also the first eleven bioclim-variables are processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time.\cr For some of the datasets not all models and rcps are available. For the ones that are not supported the data will not be downloaded and a warning will be prompted. See parameter \code{model.var} for more information or check the website of CHELSA Climate (\url{http://chelsa-climate.org/future/}). Please note, that the downloaded data for temperature and the therefore also the first eleven bioclim-variables are processed to °C without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time.
#'
#'@param save.location string. Input where the datasets should be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "temp", "tmax", "tmin", "bio")}
#'@param bio.var integer (vector). Input which monthly data should be downloaded. Only applicable to BIOCLIM variables. For further information see: \url{http://chelsa-climate.org/bioclim/}. \cr Default: \code{c(1:19)}
#'@param month.var integer (vector). Input which monthly data should be downloaded. Only applicable to Precipitation and Temperature (average, maximum, minimum). \cr Default: \code{c(1:12)}
#'@param emission.scenario.var string (vector). Input which SSP (Shared Socioeconomic Pathways) scenario dataset should be downloaded. Provided are the SSP scenarios 1 (SSP126), 3 (SSP370), and 5 (SSP585). \cr Default: \code{c("ssp126","ssp370","ssp585")}
#'@param time.interval.var string (vector). Input for which time interval data should be downloaded. CHELSA provides downscaled CMIP6 climatologies for 2050 and 2070. Multiple inputs possible.\cr Default: \code{c("2041-2060", "2061-2080")}
#'@param model.var string (vector). Input which future model dataset should be downloaded. For more information see: \url{http://chelsa-climate.org/future/}.\cr For some of the datasets not all downloads are available. For the ones that are not supported the data will not be downloaded and a warning will be prompted. For an overview please try "warnings()" after execution. \cr Default: \code{c("gfdl-esm4", "ukesm1-0-ll", "mpi-esm1-2-hr", "ipsl-cm6a-lr", "mri-esm2-0")}
#'@param clipping logical. Input whether the downloaded data should be clipped.\cr If \code{FALSE} \code{clip.shapefile}, buffer, clip.extent will be ignored. \cr Default: \code{FALSE}
#'@param clip.shapefile string. Input which shapefile should be used for clipping.  \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether files should be converted into the ASCII format.\cr If \code{TRUE}: a new subdirectory is created and the rawdata is saved there. If \code{clipping} is \code{TRUE}: the clipped raster files are also saved as ASCII grids.  \cr Default: \code{FALSE}
#'@param stacking.data logical. Input whether the downloaded data should be stacked as a netCDF-rasterstack. \cr  Default: \code{FALSE}
#'@param combine.raw.zip logical. Should the downloaded raw-data be "zipped". \cr  Default: \code{FALSE}
#'@param delete.raw.data  logical. Should the downloaded raw-data be deleted. If the \code{combine.raw.zip} is \code{TRUE}: raw-data is still available in the zipped file. \cr Default: \code{FALSE}
#'@param save.bib.file logical. Whether a BibTex-citation file of the dataset should be provided in the Working directory. \cr Default: \code{TRUE}
#'
#'@return CHELSA CMIP6 climatology datasets.
#'
#'@references D. N. Karger, O. Conrad, J. Böhner , et al. "Climatologies at high resolution for the earth's land surface areas". In: _Scientific Data_ 4.1 (Sep. 2017). DOI: 10.1038/sdata.2017.122. <URL: https://doi.org/10.1038/sdata.2017.122>.
#'@references D. N. Karger, O. Conrad, J. Böhner , et al. _Data from: Climatologies at high resolution for the earth's land surface areas_. En. 2018. DOI: 10.5061/DRYAD.KD1D4. <URL: http://datadryad.org/stash/dataset/doi:10.5061/dryad.kd1d4>.
#'
#'@note Specifications: <URL: https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf>
#'@note More information on Shared Socioeconomic Pathways under <URL: https://www.dkrz.de/en/communication/climate-simulations/cmip6-en/the-ssp-scenarios?set_language=en>
#'
#'@examples
#' \dontrun{
#' # Bioclim
#' Chelsa.CMIP_6.download(parameter = "bio",
#'                         bio.var = c(1,19),
#'                         emission.scenario.var = "ssp126",
#'                         time.interval.var = "2011-2040",
#'                         model.var = "mpi-esm1-2-hr")
#' # Precipitation
#' Chelsa.CMIP_6.download(parameter = "prec",
#'                         month.var = c(1,7),
#'                         emission.scenario.var = "ssp585",
#'                         time.interval.var = "2071-2100",
#'                         model.var = "gfdl-esm4")
#' }
#'
#'@import stringr
#'@import RCurl
#'@import ncdf4
#'@import terra
#'@import httr
#'@importFrom utils unzip download.file setTxtProgressBar txtProgressBar
#'
#'
#'@export
Chelsa.CMIP_6.download <- function(save.location = "./",
                                   parameter = c("prec", "temp", "tmax", "tmin", "bio"),
                                   bio.var = c(1:19),
                                   month.var = c(1:12),
                                   emission.scenario.var = c("ssp126","ssp370","ssp585"),
                                   time.interval.var = c("2011-2040", "2041-2070", "2071-2100"),
                                   model.var = c("gfdl-esm4", 
                                                 "ukesm1-0-ll",
                                                 "mpi-esm1-2-hr",
                                                 "ipsl-cm6a-lr",
                                                 "mri-esm2-0"),
                                   clipping = FALSE,
                                   clip.shapefile = NULL,
                                   clip.extent = c(-180, 180, -90, 90),
                                   buffer = 0,
                                   convert.files.to.asc = FALSE,
                                   stacking.data = FALSE,
                                   combine.raw.zip = FALSE,
                                   delete.raw.data  = FALSE,
                                   save.bib.file = TRUE
){
  gc()
  call.time <- stringr::str_replace_all(
    stringr::str_replace_all(
      stringr::str_split(string = paste0(Sys.time()), 
                         pattern = "\\.")[[1]][1], 
      pattern = ":",
      replacement = "-"), 
    pattern = " ", 
    replacement = "_")
  
  # initial check ----------------------------------------------------------  
  
  save.location <- normalizePath(save.location, winslash = "/")
  # the build of this function is very similar to the Chelsa.Clim.download function.
  if(base::is.element("prec", parameter)|base::is.element("temp", parameter)|
     base::is.element("tmax", parameter)|base::is.element("tmin", parameter)){
    
    month.var <- c(month.var)
    if(!is.numeric(month.var)) stop()
    # !!!
    month.var <- stringr::str_pad(month.var, 2, 'left', pad = "0")
  }
  if(base::is.element("bio", parameter)){
    bio.var <- c(bio.var)
    if(!is.numeric(bio.var)) stop()
    bio.var <- stringr::str_pad(bio.var, 2, 'left', pad = "0")
  }
  
  # Preparations ------------------------------------------------------------
  # parameter
  parameter <- base::sort(parameter)
  DLTparameter <- c(base::rep(parameter[parameter!="bio"], 
                              base::length(month.var)), 
                    base::rep(parameter[parameter=="bio"], 
                              base::length(bio.var)))
  DLTparameter <- base::sort(DLTparameter)
  # variables 
  DLTvariable <- NULL
  for(parm in parameter){
    DLTvariable <- c(DLTvariable, 
                     switch(parm, 
                            "prec" = month.var,
                            "tmax" = month.var,
                            "temp" = month.var,
                            "tmin" = month.var, 
                            bio.var
                     )
    )
    
  }
  
  # Combine search into large dataframe -------------------------------------
  dataDF <- data.frame("parameter" = sort(DLTparameter), # parameter = shortname (pt1)
                       "variable" = DLTvariable # variable = shortname (pt2)
  )
  lengthTemp <- nrow(dataDF)
  # Collection of Scenario 
  if(length(emission.scenario.var)==1){
    dataDF$ssp <- base::rep(emission.scenario.var, lengthTemp)
  }else{
    dataDF <- data.frame("parameter" = rep(dataDF$parameter, length(emission.scenario.var)) , 
                         "variable" = rep(dataDF$variable, length(emission.scenario.var)) ,  
                         "ssp" = NA)
    temp_version <- c()
    for(i_ssp in emission.scenario.var){
      temp_version <- c(temp_version,base::rep(i_ssp, lengthTemp))
    }
    dataDF$ssp <- temp_version
    rm(temp_version, i_ssp)
  }
  
  lengthTemp <- nrow(dataDF)
  # Collection of timeperiod 
  if(length(time.interval.var)==1){
    dataDF$timeperiod <- base::rep(time.interval.var, lengthTemp)
  }else{
    dataDF <- data.frame("parameter" = rep(dataDF$parameter, length(time.interval.var)) , 
                         "variable" = rep(dataDF$variable, length(time.interval.var)) ,  
                         "ssp" = rep(dataDF$ssp, length(time.interval.var)) ,  
                         "timeperiod" = NA)
    temp_timeperiod <- c()
    for(i_timeperiod in time.interval.var){
      temp_timeperiod <- c(temp_timeperiod, base::rep(i_timeperiod, lengthTemp))
    }
    dataDF$timeperiod <- temp_timeperiod
    rm(temp_timeperiod, i_timeperiod)
  }
  
  lengthTemp <- nrow(dataDF)
  # Collection of models 
  if(length(model.var)==1){
    dataDF$model <- base::rep(model.var, lengthTemp)
  }else{
    dataDF <- data.frame("parameter" = rep(dataDF$parameter, length(model.var)) , 
                         "variable" = rep(dataDF$variable, length(model.var)) ,  
                         "ssp" = rep(dataDF$ssp, length(model.var)) ,  
                         "timeperiod" = rep(dataDF$timeperiod, length(model.var)) ,  
                         "model" = NA)
    temp_model <- c()
    for(i_model in model.var){
      temp_model <- c(temp_model, base::rep(i_model, lengthTemp))
    }
    dataDF$model <- temp_model
    rm(temp_model, i_model)
  }
  rm(lengthTemp)
  
  # switch?
  dataDF$parm[dataDF$parameter == "prec"] <- base::paste0("pr")
  dataDF$parm[dataDF$parameter == "temp"] <- base::paste0("tas")
  dataDF$parm[dataDF$parameter == "tmin"] <- base::paste0("tasmin")
  dataDF$parm[dataDF$parameter == "tmax"] <- base::paste0("tasmax")
  dataDF$parm[dataDF$parameter == "bio"] <- base::paste0("bio")
  
  dataDF$variableNumeric <- as.numeric(dataDF$variable)
  # naming convention: 
  # CHELSA_[short_name]_[timeperiod]_[model] _[ssp] _[Version].tif
  
  dataDF$URL[dataDF$parameter != "bio"]  <-
    paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/",
           dataDF$timeperiod[dataDF$parameter != "bio"],
           "/",
           base::toupper(dataDF$model[dataDF$parameter != "bio"]),
           "/",
           dataDF$ssp[dataDF$parameter != "bio"],
           "/",
           dataDF$parm[dataDF$parameter != "bio"],
           "/",
           "CHELSA",
           "_",
           dataDF$model[dataDF$parameter != "bio"],"_r1i1p1f1_w5e5",
           "_",
           dataDF$ssp[dataDF$parameter != "bio"],
           "_",
           dataDF$parm[dataDF$parameter != "bio"] ,
           "_", 
           dataDF$variable[dataDF$parameter != "bio"] , 
           "_", 
           stringr::str_replace(dataDF$timeperiod[dataDF$parameter != "bio"], pattern = "-", replacement = "_"),
           "_", 
           "norm.tif")
  
  dataDF$URL[dataDF$parameter == "bio"]  <-
    paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/",
           dataDF$timeperiod[dataDF$parameter == "bio"],
           "/",
           base::toupper(dataDF$model[dataDF$parameter == "bio"]),
           "/",
           dataDF$ssp[dataDF$parameter == "bio"],
           "/",
           dataDF$parm[dataDF$parameter == "bio"],
           "/",
           "CHELSA",
           "_",
           dataDF$parm[dataDF$parameter == "bio"],
           dataDF$variableNumeric[dataDF$parameter == "bio"], 
           "_", 
           dataDF$timeperiod[dataDF$parameter == "bio"] ,
           "_",
           dataDF$model[dataDF$parameter == "bio"],
           "_",
           dataDF$ssp[dataDF$parameter == "bio"],
           "_", 
           "V.2.1.tif")
  
  # Check if URL exists!
  for(urlexists in dataDF$URL){ # loop through all URLs
    if(!RCurl::url.exists(urlexists)){ # if not, print warning!
      cat(paste(urlexists, 
                " does not exist, please check the website of Chelsa. \n")
      )
      if(urlexists == dataDF$URL[1]){
        cat(paste("\t If any of these download warnings was prompted incorrectly, we apprecheate a feedback on this at helgejentsch.research@gmail.com\n")
        )}
      next 
    }
  }
  
  # print the amount of data to be downloaded and processed.
  print(paste0(getDownloadSize(dataDF$URL), " MB will be downloaded."))
  # Progressbar setup
  PGBsum <- nrow(dataDF) + length(unique(dataDF$parameter)) + 1
  PGB <- utils::txtProgressBar(min = 0, max = PGBsum, style = 3)
  PGBstate <- 0
  # Preparation of save location stack 
  locationSack <- NULL
  # loop through every instance and save the location. 
  # HINT FOR RUNTIME IMPROVEMENT!!!
  # !!! Parm oder Parameter?!
  for(parm in dataDF$parameter){
    if (!dir.exists(paste0(save.location, "/", parm))){
      dir.create(paste0(save.location, "/", parm))
    }
    if (!dir.exists(paste0(save.location, "/", parm, "/ChelsaCMIP6Climatologies"))){
      dir.create(paste0(save.location, "/", parm, "/ChelsaCMIP6Climatologies"))
    }
    locationSack <- c(locationSack, paste0(save.location, "/", parm, "/ChelsaCMIP6Climatologies/"))
  }
  
  # CHELSA_[short_name]_[timeperiod]_[model]_[ssp]_[Version].tif
  dataDF$filepath  <- 
    paste0(save.location,"/", dataDF$parameter, "/ChelsaCMIP6Climatologies", 
           "/",
           "CHELSA",
           "_", 
           dataDF$parm, "_", dataDF$variable, 
           "_", 
           dataDF$timeperiod,
           "_", 
           dataDF$model,
           "_", 
           dataDF$ssp,
           "_", 
           "V2.1.tif")
  
  #!!! Value Preprocessing - postponed for now
  
  # check for file existance - if not already present - download file 
  for(fileexists in dataDF$filepath){
    if(!file.exists(fileexists)){
      unlink(list.files(tempdir(), recursive = TRUE, full.names = TRUE))
      # cat("\n",paste0(dataDF$URL[dataDF$filepath == fileexists]),"\n")
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
                    # method = 'wget',
                    mode = 'wb',
                    # to show progression bar
                    quiet = TRUE,
                    cacheOK = FALSE)
    }
    setTxtProgressBar(PGB, PGBstate+1)
    PGBstate <- PGBstate+1
  }
  
  locationSack <- unique(locationSack)
  for (temp.temp.save.location in locationSack) {
    run <- grep(temp.temp.save.location, locationSack)
    for(i in run){
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
                                   stack.clipped = FALSE,
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
}

#'@title CHELSA Timeseries Download
#'@author Helge Jentsch
#'@description This function supports a download of the CHELSA Timeseries dataset (Jan. 1979 - Dec. 2013). This includes precipitation sums (mm) and temperature (average, maximum, minimum; °C) parameters. For further information, please regard \url{http://chelsa-climate.org/timeseries/}.\cr To allow pre-processing, clipping and buffering, conversion to ASCII-grids and stacking options are included.\cr Optional an output of a .bib-file of the cited literature can be retrieved.\cr For user convenience, saving directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'@note Please note that the downloaded data for temperature are processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time.
#'
#'@param save.location string. Input where the datasets should be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "tmax", "tmin")}
#'@param start.year.var integer. Input year the download timeseries starts. \cr Default: 1979 (minimum)
#'@param start.month.var integer. Input month the download timeseries starts. \cr Default: 1 (minimum)
#'@param end.year.var integer. Input year the download timeseries ends. \cr Default: 2013 (maximum)
#'@param end.month.var integer. Input month the download timeseries ends. \cr Default: 12 (maximum)
#'@param include.month.var integer (vector). Input which monthly data should be downloaded. \cr Default: \code{c(1:12)}
#'@param version.var string (vector). Input which version of the dataset should be downloaded. Multiple selection is _not_ possible. Select between version _1.2_ and _2.1_.\cr Default:  \code{c("1.2")}
#'@param clipping logical. Input whether the downloaded data should be clipped.\cr If \code{FALSE}: \code{clip.shapefile}, \code{buffer}, \code{clip.extent} will be ignored. \cr Default: \code{FALSE}
#'@param clip.shapefile string. Input which shapefile should be used for clipping. \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether files should be converted into the ASCII format.\cr If \code{TRUE}: a new subdirectory is created and the rawdata is saved there. \cr If \code{clipping} is \code{TRUE}: the clipped raster files are also saved as ASCII grids. \cr  Default: \code{FALSE}
#'@param combine.raw.zip logical. Should the downloaded raw-data be "zipped". \cr Default: \code{FALSE}
#'@param delete.raw.data  logical. Should the downloaded raw-data be deleted. If \code{combine.raw.zip} is \code{TRUE}: raw-data is still available in the zipped file. \cr Default: \code{FALSE}
#'@param save.bib.file logical. Whether a BibTex-citation file of the dataset should be provided in the Working directory. \cr Default: \code{TRUE}
#'
#'@return Custom dataset of CHELSA Timeseries for a chosen timeseries.
#'
#'@references D. N. Karger, O. Conrad, J. Böhner , et al. "Climatologies at high resolution for the earth's land surface areas". In: _Scientific Data_ 4.1 (Sep. 2017). DOI: 10.1038/sdata.2017.122. <URL: https://doi.org/10.1038/sdata.2017.122>.
#'@references D. N. Karger, O. Conrad, J. Böhner , et al. _Data from: Climatologies at high resolution for the earth's land surface areas_. En. 2018. DOI: 10.5061/DRYAD.KD1D4. <URL: http://datadryad.org/stash/dataset/doi:10.5061/dryad.kd1d4>.
#'
#'@examples
#' \dontrun{
#' Chelsa.timeseries.download(parameter = "prec",
#'                             start.year.var = 2000,
#'                             start.month.var = 1,
#'                             end.year.var = 2002,
#'                             end.month.var = 12,
#'                             version.var = "1.2",
#'                             include.month.var = c(1,12))
#' }
#'
#'@import stringr
#'@import RCurl
#'@import ncdf4
#'@import terra
#'@import httr
#'@importFrom utils unzip download.file setTxtProgressBar txtProgressBar
#'
#'
#'@export
Chelsa.timeseries.download <- function(save.location = "./",
                                       parameter = c("prec", "temp", "tmax", "tmin", "pet"),
                                       start.year.var = 1979,
                                       start.month.var = 1,
                                       end.year.var = 2013,
                                       end.month.var = 12,
                                       include.month.var = c(1:12),
                                       version.var = c("1.2"),
                                       clipping = FALSE,
                                       clip.shapefile = NULL,
                                       buffer = 0,
                                       clip.extent = c(-180, 180, -90, 90),
                                       convert.files.to.asc = FALSE,
                                       combine.raw.zip = FALSE,
                                       delete.raw.data  = FALSE,
                                       save.bib.file = TRUE){
  gc()
  call.time <- stringr::str_replace_all(
    stringr::str_replace_all(
      stringr::str_split(string = paste0(Sys.time()), 
                         pattern = "\\.")[[1]][1], 
      pattern = ":",
      replacement = "-"), 
    pattern = " ", 
    replacement = "_")
  # stringr::str_replace_all(stringr::str_replace_all(stringr::str_split(string = paste0(Sys.time()), pattern = "\."), pattern = ":", replacement = "-"), pattern = " ", replacement = "_")
  # initial check -----------------------------------------------------------
  # normalize Path for easier application later
  save.location <- normalizePath(save.location, 
                                 winslash = "/")
  if(length(version.var) != 1) stop("Version variable 'version.var' should only have either '1.2' or '2.1'.")
  if(!is.null(version.var) & version.var == "1.2"){
    # Check which parameters are put in and if the connected
    # month/bio-variables are correctly input
    if(is.element("prec", parameter)|
       is.element("tmax", parameter)|
       is.element("temp", parameter)|
       is.element("tmin", parameter)){
      include.month.var <- c(include.month.var)
      if(!is.numeric(include.month.var)) stop()
      include.month.var <- str_pad(include.month.var,
                                   2,
                                   'left', 
                                   pad = "0")
      # print(include.month.var)
    }
    if(start.year.var < 1979 | end.year.var > 2013 | end.year.var < 1979 | start.year.var > 2013) {
      stop("Timeseries only available from 01.1979 to 12.2013. \n Please check input!")
    }
    # check for consistent timeseries
    if(end.year.var < start.year.var) stop("Endyear is before the startyear. Please correct the input!")
    if(start.year.var == end.year.var){
      if(start.month.var > end.month.var) stop("End is before the start. Please correct the input!")
    }
    
    ts_string <- seq.Date(as.Date(paste(start.year.var,
                                        start.month.var, "01", sep = "-")),
                          as.Date(paste(end.year.var,
                                        end.month.var, "01", sep = "-")),
                          by = "month")
    ts_string <- format.Date(ts_string, format = "%Y_%m")
    # ts_string <- str_sub(ts_string, 1, end = str_length(ts_string)-3)
    # ts_string <- str_replace_all(ts_string, pattern = "-", replacement = "_")
    
    if(length(include.month.var)!=12){
      ts.string.temp <- c()
      for (incl.month in include.month.var) {
        # print(incl.month)
        ts.string.temp <- c(ts.string.temp,
                            ts_string[grep(pattern = paste0("_", incl.month)
                                           , ts_string)]
        )
      }
      ts_string <- ts.string.temp
    }
  }
  if(!is.null(version.var) & version.var == "2.1"){
    # Check which parameters are put in and if the connected
    # month/bio-variables are correctly input
    if(is.element("prec", parameter)|
       is.element("tmax", parameter)|
       is.element("temp", parameter)|
       is.element("pet", parameter)|
       is.element("tmin", parameter)){
      include.month.var <- c(include.month.var)
      if(!is.numeric(include.month.var)) stop("this is stops")
      include.month.var <- str_pad(include.month.var,
                                   2,
                                   'left', 
                                   pad = "0")
    }
    if(start.year.var < 1979 | end.year.var > 2019 | end.year.var < 1979 | start.year.var > 2019) {
      stop("Timeseries only available from 02.1979 to 12.2019. \n Please check input!")
    }
    # check for consistent timeseries
    if(end.year.var < start.year.var) stop("Endyear is before the startyear. Please correct the input!")
    if(start.year.var == end.year.var){
      if(start.month.var > end.month.var) stop("End is before the start. Please correct the input!")
    }
    
    ts_string <- seq.Date(as.Date(paste(start.year.var,
                                        start.month.var, "01", sep = "-")),
                          as.Date(paste(end.year.var,
                                        end.month.var, "01", sep = "-")),
                          by = "month")
    ts_string <- format.Date(ts_string, format = "%m_%Y")
    # ts_string <- str_sub(ts_string, 1, end = str_length(ts_string)-3)
    # ts_string <- str_replace_all(ts_string, pattern = "-", replacement = "_")
    if(length(include.month.var)!=12){
      ts.string.temp <- c()
      for (incl.month in include.month.var) {
        # print(incl.month)
        ts.string.temp <- c(ts.string.temp,
                            ts_string[grep(pattern = paste0(incl.month, "_")
                                           , ts_string)]
        )
      }
      ts_string <- ts.string.temp
    }
  }
  
  # Preparations ------------------------------------------------------------
  # parameter
  parameter <- base::sort(parameter)
  DLTparameter <- c(base::rep(parameter[parameter!="bio"], 
                              base::length(ts_string)))
  DLTparameter <- base::sort(DLTparameter)
  # variables 
  DLTvariable <- NULL
  for(parm in parameter){
    DLTvariable <- c(DLTvariable, 
                     switch(parm, 
                            "prec" = ts_string,
                            "tmax" = ts_string,
                            "temp" = ts_string,
                            "pet" = ts_string,
                            "tmin" = ts_string, 
                            stop()
                     )
    ) 
  }
  # Combine search into large dataframe -------------------------------------
  dataDF <- data.frame("parameter" = base::sort(DLTparameter), 
                       "variable" = DLTvariable
  )
  if(length(version.var)==1){
    dataDF$version <- base::rep(version.var, length(DLTvariable))
  }
  
  # v1.2
  if(is.element("1.2", dataDF$version)){  
    # dataDF$parmLong[dataDF$version == "1.2"] <- base::paste0(dataDF$parameter[dataDF$version == "1.2"],"10")
    
    dataDF$parmLong[dataDF$version == "1.2"] <- 
      base::paste0(dataDF$parameter[dataDF$version == "1.2"])
    dataDF$parmLong[dataDF$version == "1.2" & dataDF$parameter == "prec"] <- 
      base::paste0("prec")
    dataDF$parmLong[dataDF$version == "1.2" & dataDF$parameter == "tmean"] <- 
      base::paste0("tmean")
    dataDF$parmLong[dataDF$version == "1.2" & dataDF$parameter == "tmax"] <- 
      base::paste0("tmax")
    dataDF$parmLong[dataDF$version == "1.2" & dataDF$parameter == "tmin"] <- 
      base::paste0("tmin")
    
    dataDF$parameter[dataDF$version == "1.2" & dataDF$parameter == "temp"] <- base::paste0("tmean")
    # dataDF$years[dataDF$version =="1.2"] <- "_1979-2013"
    # dataDF$years[dataDF$version == "1.2" & 
    #                (dataDF$parameter == "prec" | dataDF$parameter == "bio")] <- base::paste0("")
    
    # Adding the URL stings
    # https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/timeseries/tmax/CHELSA_tmax_1979_01_V1.2.1.tif
    dataDF$URL[dataDF$version == "1.2" & dataDF$parameter != "bio"]  <-  
      # https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/timeseries/
      paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/timeseries/",
             # tmax
             dataDF$parameter[dataDF$version == "1.2" & dataDF$parameter != "bio"], 
             # /CHELSA_
             "/CHELSA_", 
             # tmax
             dataDF$parameter[dataDF$version == "1.2" & dataDF$parameter != "bio"], 
             # _
             "_", 
             # 1979_01
             dataDF$variable[dataDF$version == "1.2" & dataDF$parameter != "bio"], 
             #_V1.2.1.tif
             "_V1.2.1.tif")
  }
  
  # v2.1
  if(is.element("2.1", dataDF$version)){ 
    dataDF$parmLong[dataDF$version == "2.1" & 
                      dataDF$parameter == "prec"] <- base::paste0("pr")
    dataDF$parmLong[dataDF$version == "2.1" & 
                      dataDF$parameter == "temp"] <- base::paste0("tas")
    dataDF$parmLong[dataDF$version == "2.1" & 
                      dataDF$parameter == "tmin"] <- base::paste0("tasmin")
    dataDF$parmLong[dataDF$version == "2.1" & 
                      dataDF$parameter == "tmax"] <- base::paste0("tasmax")
    dataDF$parmLong[dataDF$version == "2.1" & 
                      dataDF$parameter == "pet"] <- base::paste0("pet_penman")
    # dataDF$years[dataDF$version =="2.1"] <- "_1981-2010"
    
    
    # https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/tas/CHELSA_tas_11_2007_V.2.1.tif
    # https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/pet/CHELSA_pet_penman_01_1980_V.2.1.tif
    dataDF$URL[dataDF$version == "2.1" & 
                 (dataDF$parameter != "bio" | dataDF$parameter != "pet")]  <-
      paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/",
             dataDF$parmLong[dataDF$version == "2.1"  & 
                               (dataDF$parameter != "bio" | dataDF$parameter != "pet")],
             "/CHELSA_", 
             dataDF$parmLong[dataDF$version == "2.1"  & 
                               (dataDF$parameter != "bio" | dataDF$parameter != "pet")],
             "_", 
             dataDF$variable[dataDF$version == "2.1"  & 
                               (dataDF$parameter != "bio" | dataDF$parameter != "pet")],
             "_V.2.1.tif")
    dataDF$URL[dataDF$version == "2.1" & dataDF$parameter != "bio" & dataDF$parameter == "pet"]  <-
      paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/",
             dataDF$parameter[dataDF$version == "2.1" & dataDF$parameter != "bio" & dataDF$parameter == "pet"],
             "/CHELSA_", 
             dataDF$parmLong[dataDF$version == "2.1" & dataDF$parameter != "bio" & dataDF$parameter == "pet"],
             "_", 
             dataDF$variable[dataDF$version == "2.1" & dataDF$parameter != "bio" & dataDF$parameter == "pet"],
             "_V.2.1.tif")
  }
  # write.table(x = dataDF, 
  #             file = normalizePath(paste0(save.location, "/", call.time, "_downloadDataframe.csv"), winslash = "/"), 
  #             sep = ";", 
  #             dec = ".", 
  #             row.names = F, 
  #             append = F)
  # Check if URL exists!
  for(urlexists in dataDF$URL){ # loop through all URLs
    if(!RCurl::url.exists(urlexists)){ # if not, print warning!
      cat(paste(urlexists, 
                " does not exist, please check the website of Chelsa. \n")
      )
      if(urlexists == dataDF$URL[1]){
        cat(paste("\t If any of these download warnings was prompted incorrectly, we apprecheate a feedback on this at helgejentsch.research@gmail.com\n")
        )}
      next 
    }
  }
  # print the amount of data to be downloaded and processed.
  print(paste0(getDownloadSize(dataDF$URL), " MB will be downloaded."))
  # Progressbar setup
  PGBsum <- nrow(dataDF) + length(unique(dataDF$parameter)) + 1
  PGB <- utils::txtProgressBar(min = 0, max = PGBsum, style = 3)
  PGBstate <- 0
  # Preparation of save location stack 
  locationSack <- NULL
  # loop through every instance and save the location. 
  # HINT FOR RUNTIME IMPROVEMENT!!!
  for(parm in dataDF$parameter){
    if (!dir.exists(paste0(save.location, "/", parm))){
      dir.create(paste0(save.location, "/", parm))
    }
    if("1.2" %in% dataDF$version){
      if (!dir.exists(paste0(save.location, "/", parm, "/ChelsaV1.2Timeseries"))){
        dir.create(paste0(save.location, "/", parm, "/ChelsaV1.2Timeseries"), showWarnings = F)
      }
      locationSack <- c(locationSack, paste0(save.location, "/", parm, "/ChelsaV1.2Timeseries/"))
    }
    if("2.1" %in% dataDF$version){
      if (!dir.exists(paste0(save.location, "/", parm, "/ChelsaV2.1Timeseries"))){
        dir.create(paste0(save.location, "/", parm, "/ChelsaV2.1Timeseries"), showWarnings = F)
      }
      locationSack <- c(locationSack, paste0(save.location, "/", parm, "/ChelsaV2.1Timeseries/"))
    }
  }
  # print(locationSack)
  
  dataDF$filepath[dataDF$version == "1.2"]  <- 
    paste0(save.location,"/",
           dataDF$parameter, "/ChelsaV1.2Timeseries", 
           "/CHELSA_", dataDF$parmLong , "_", dataDF$variable, dataDF$years,
           "_V1.2.tif")
  dataDF$filepath[dataDF$version == "2.1"]  <- 
    paste0(save.location,"/",
           dataDF$parameter, "/ChelsaV2.1Timeseries", 
           "/CHELSA_", dataDF$parmLong , "_", dataDF$variable, dataDF$years,
           "_V2.1.tif")
  dataDF$newDownload <- NULL
  # check for file existance - if not already present - download file 
  for(fileexists in dataDF$filepath){
    if(!file.exists(fileexists)){
      dataDF$newDownload[dataDF$filepath == fileexists] <- TRUE
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
    }else{
      dataDF$newDownload[dataDF$filepath == fileexists] <- FALSE
    }
    setTxtProgressBar(PGB, PGBstate+1)
    PGBstate <- PGBstate+1
  }
  write.table(x = dataDF, 
              file = normalizePath(paste0(save.location, "/", call.time, "_downloadDataframe.csv"), winslash = "/"), 
              sep = ";", 
              dec = ".", 
              row.names = F, 
              append = F)
  
  if(is.element("1.2", dataDF$version)){ 
    # https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification.pdf
    rescaleDF_V12 <- dataDF[dataDF$version == "1.2" &
                              dataDF$newDownload == TRUE &
                              dataDF$parameter != "prec"
                            ,]
    
    if(nrow(rescaleDF_V12)>0){
      for(rescale_i in 1:nrow(rescaleDF_V12)){
        gc()
        tempRast <- terra::rast(rescaleDF_V12$filepath[rescale_i])
        tempRast <- process.raster.int.doub(tempRast)
        tempFilePath <- tempfile(tmpdir = tempdir(), fileext = ".tif")
        terra::writeRaster(x = tempRast,
                           filename = tempFilePath
        )
        terra::writeRaster(x = terra::rast(x = tempFilePath),
                           filename = rescaleDF_V12$filepath[rescale_i], 
                           overwrite = TRUE)
        rm(tempFilePath)
        gc()
      }
      rm(rescale_i)
      unlink(list.files(tempdir(), recursive = T, full.names =T))
    }
  }
  if(is.element("2.1", dataDF$version)){
    # https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf
    rescaleDF_V21 <- dataDF[dataDF$version == "2.1" &
                              dataDF$newDownload == TRUE &
                              (dataDF$parameter != "prec" &
                                 dataDF$parameter != "pet"),
    ]
    if(nrow(rescaleDF_V21)>0){
      for(rescale_i in 1:nrow(rescaleDF_V21)){
        gc()
        tempRast <- terra::rast(rescaleDF_V21$filepath[rescale_i])
        tempRast <- process.raster.int.doub(tempRast)
        tempFilePath <- tempfile(tmpdir = tempdir(), fileext = ".tif")
        terra::writeRaster(x = tempRast,
                           filename = tempFilePath
        )
        terra::writeRaster(x = terra::rast(x = tempFilePath),
                           filename = rescaleDF_V21$filepath[rescale_i],
                           overwrite = TRUE)
        rm(tempFilePath)
        gc()
      }
      rm(rescale_i)
    }
    unlink(list.files(tempdir(), recursive = T, full.names =T))
    offsetDF_V21 <- dataDF[dataDF$version == "2.1" &
                             dataDF$newDownload == TRUE &
                             (dataDF$parameter != "prec" &
                                dataDF$parameter != "pet"),
    ]
    if(nrow(offsetDF_V21)>0){
      for(rescale_i in 1:nrow(offsetDF_V21)){
        gc()
        tempRast <- terra::rast(offsetDF_V21$filepath[rescale_i])
        tempRast <- process.raster.offset(tempRast)
        tempFilePath <- tempfile(tmpdir = tempdir(), fileext = ".tif")
        terra::writeRaster(x = tempRast,
                           filename = tempFilePath
        )
        terra::writeRaster(x = terra::rast(x = tempFilePath),
                           filename = offsetDF_V21$filepath[rescale_i],
                           overwrite = TRUE)
        rm(tempFilePath)
        gc()
      }
      rm(rescale_i)
      unlink(list.files(tempdir(), recursive = T, full.names =T))
    }
    rm(rescaleDF_V21, offsetDF_V21)
    if(nrow(dataDF[dataDF$version == "2.1" &
             dataDF$newDownload == TRUE &
             (dataDF$parameter == "prec" |
                dataDF$parameter == "pet"),
    ])){
      warning("Potential evapotranspiration and precipitation variables are not preprocessed in this step. See https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf (Page 18) for unit reference.", 
              immediate. = TRUE)
    }
  }
  
  locationSack <- unique(locationSack)
  for (temp.temp.save.location in locationSack) {
    run <- grep(temp.temp.save.location, locationSack)
    for(i in run){
      # print(ls())
      # variable.numbers <- dataDF$variable[dataDF$parameter == parameter[i]]
      # stop()
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
      # if(stacking.data == TRUE){
      #   # the function "stacking.downloaded.data"
      #   # (found in the auxiliary.R-File) is executed.
      #   # The save.location is the same location as the
      #   # "current" save location.
      #   if(clipping==TRUE){
      #     stacking.downloaded.data(stack.save.location = temp.temp.save.location,
      #                              parameter.var = parameter[i],
      #                              variable.numbers = variable.numbers,
      #                              stack.clipped = TRUE,
      #                              time.stamp.var = call.time)
      #   }else{
      #     stacking.downloaded.data(stack.save.location = temp.temp.save.location,
      #                              parameter.var = parameter[i],
      #                              variable.numbers = variable.numbers,
      #                              stack.clipped = FALSE,
      #                              time.stamp.var = call.time)
      #   }
      # }
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
}
#   stop()
#   # Parameter and directories -----------------------------------------------
#   # work through paramerters
#   for(i in parameter){
#     # clear up the temporary directory
#     unlink(list.files(tempdir(), recursive = T, full.names=T))
#     
#     # create intermediate strings for later use
#     interm <- switch(i,
#                      "prec"  = "prec/",
#                      "temp"  = "tmean/",
#                      "tmax"  = "tmax/",
#                      "tmin"  = "tmin/",
#                      # "bio"  = "bio/",
#                      stop())
#     
#     variable.numbers <- switch(i,
#                                # "bio" = bio.var,
#                                "tmin" = include.month.var,
#                                "tmax" = include.month.var,
#                                "temp" = include.month.var,
#                                "prec" = include.month.var,
#                                stop())
#     
#     # create new directory
#     if(!dir.exists(paste0(save.location, "/", i))){
#       dir.create(paste0(save.location, "/", i), showWarnings = FALSE)
#     }
#     temp.save.location <- paste0(save.location, "/", i, "/")
#     # to go analog to the functions before
#     temp.temp.save.location <- paste0(temp.save.location,
#                                       stringr::str_replace_all(interm,
#                                                                pattern = "/",
#                                                                "_"),
#                                       "timeseries","/")
#     # print(str_sub(temp.temp.save.location, end=-2))
#     if(!dir.exists(temp.temp.save.location)){
#       dir.create(str_sub(temp.temp.save.location, end=-2))
#     }
#     
#     # temp.temp.save.location <- normalizePath(temp.temp.save.location,
#     #                                          winslash = "/")
#     # if(i == "temp"){
#     #   i <- "tmean"
#     # }
#     # print(interm)
#     # Download ----------------------------------------------------------------
#     # if(i != "bio"){
#     for (year_month in ts_string){
#       URL.temp <-
#         paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/timeseries/",
#                interm, "CHELSA_", i,"_",year_month,
#                "_V1.2.1.tif")
#       # check if URL is available
#       if(!http_error(URL.temp)){
#         # clear up the temporary directory
#         unlink(list.files(tempdir(), recursive = T, full.names=T))
#         
#         dest.file <- paste0(temp.temp.save.location, "CHELSA_", i,
#                             "_", year_month, "_V1.2.1.tif")
#         if(!file.exists(dest.file)){
#           # download file to save location
#           download.file(url = URL.temp,
#                         destfile = dest.file,
#                         overwrite = TRUE,
#                         mode = 'wb',
#                         quiet = FALSE)
#           
#           
#           if(i != "prec"){
#             raster.temp <- terra::rast(dest.file)
#             
#             raster.temp <- terra::clamp(raster.temp, lower = -1000, values = FALSE)
#             gc()
#             
#             raster.temp <- process.raster.int.doub(raster.temp)
#             raster.temp <- process.raster.offset(raster.layer = raster.temp)
#             
#             terra::writeRaster(x = raster.temp,
#                                filename = dest.file,
#                                overwrite = TRUE)
#             rm(raster.temp)
#             gc()
#           }
#         }
#       }else{
#         # Warning message
#         warning(paste0("File does not exist. Did not download: \n", URL.temp, "\n\n"),
#                 call. = TRUE, immediate. = FALSE)
#       }
#       if(year_month == ts_string[length(ts_string)] &
#          length(list.files(temp.temp.save.location,
#                            pattern = ".tif",
#                            include.dirs = FALSE)) != 0){
#         if(clipping == TRUE){
#           clipping.tif(clip.save.location = temp.temp.save.location,
#                        clip.shapefile = clip.shapefile,
#                        clip.extent = clip.extent,
#                        convert.files.to.asc = convert.files.to.asc,
#                        buffer = buffer,
#                        time.stamp.var = call.time)
#         }
#         if(convert.files.to.asc == TRUE){
#           convert.to.asc(temp.temp.save.location,
#                          time.stamp.var = call.time)
#         }
#         if(stacking.data == TRUE){
#           if(clipping==TRUE){
#             stacking.downloaded.data(stack.save.location = temp.temp.save.location,
#                                      parameter.var = i,
#                                      variable.numbers = variable.numbers,
#                                      stack.clipped = TRUE,
#                                      stack.time.series = TRUE,
#                                      time.series = ts_string,
#                                      time.stamp.var = call.time)
#           }else{
#             stacking.downloaded.data(stack.save.location = temp.temp.save.location,
#                                      parameter.var = i,
#                                      variable.numbers = variable.numbers,
#                                      stack.time.series = TRUE,
#                                      time.series = ts_string,
#                                      time.stamp.var = call.time)
#           }
#         }
#         if(combine.raw.zip == TRUE){
#           combine.raw.in.zip(save.location = temp.temp.save.location,
#                              zip.name = paste0("CHELSATimeseries_", i, ""),
#                              time.stamp.var = call.time)
#         }
#         if(delete.raw.data == TRUE){
#           unlink(list.files(temp.temp.save.location,
#                             pattern = ".tif",
#                             include.dirs = FALSE, full.names = T), force = TRUE)
#         }
#       }
#     }
#     
#     # Clean up, if no data was downloaded. ------------------------------------
#     
#     
#     if(length(list.files(temp.temp.save.location,
#                          include.dirs = TRUE)) == 0){
#       unlink(str_sub(temp.temp.save.location, 1, end = str_length(temp.temp.save.location)-1),
#              force = T, recursive = TRUE)
#     }
#   }
#   # Saving BIB File
#   if(save.bib.file == TRUE) save.citation(save.location = save.location, dataSetName = "CHELSA")
# }
