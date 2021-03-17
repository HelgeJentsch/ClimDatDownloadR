#'@title Function for downloading the Chelsa Climate dataset
#'@author Helge Jentsch
#'@description This function supports a download of the Chelsa Climate dataset. This includes precipitation (mm), temperature (average, maximum, minimum; °C), and bioclimatic parameters. For convenience also a clipping-, conversion to ascii-grid,and stacking-tool is included.\cr An output of a .bib-file of the literature is also optional.\cr For a clear working environment, directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'
#'@param save.location string. Input where the datasets should be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "temp", "tmax", "tmin", "bio")}
#'@param bio.var integer (vector). Input which monthly data should be downloaded. Only applicable to BIOCLIM variables. For further information see: \url{http://chelsa-climate.org/bioclim/}. \cr Default: \code{c(1:19)}
#'@param month.var integer (vector). Input which monthly data should be downloaded. Only applicable to precipitation and temperature (average, maximum, minimum). \cr Default: \code{c(1:12O)}
#'@param version.var string (vector). Input which version of the data set should be downloaded. Multiple selection is possible. \cr Default:  \code{c("1.1", "1.2")}
#'@param clipping logical. Input whether the downloaded data should be clipped.\cr If \code{FALSE}: clip.shapefile, buffer, clip.extent will be ignored. \cr Default: \code{FALSE}
#'@param clip.shapefile string. Input which shapefile should be used for clipping. \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop"). \cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether files should be converted into the ASCII format.\cr If \code{TRUE}: a new subdirectory is created and the rawdata is saved there. If \code{clipping} is \code{TRUE}: the clipped raster files are also saved as ASCII grids. \cr  Default: \code{FALSE}
#'@param stacking.data logical. Input whether the downloaded data should be stacked as a netCDF-rasterstack. \cr Default: \code{FALSE}
#'@param combine.raw.zip logical. Should the downloaded raw-data be "zipped". \cr Default: \code{FALSE}
#'@param delete.raw.data  logical. Should the downloaded raw-data be deleted.\cr If \code{combine.raw.zip} is \code{TRUE}: raw-data is still available in the zipped file. \cr Default: \code{FALSE}
#'@param save.bib.file logical. Whether a bibTex-citation file of the data set should be provided in the Working directory. \cr Default: \code{TRUE}
#'@return Chelsa climate data sets for the period of 1979 - 2013
#'
#'@examples
#' ## NOT RUN
#' ## Bioclim
#' # Chelsa.Clim.download(parameter = "bio", bio.var = c(1,12))
#' ## Precipitation
#' # Chelsa.Clim.download(parameter = "prec", month.var = c(1,12))
#' ## NOT RUN
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
  call.time <- str_replace_all(str_replace_all(paste0(Sys.time()), pattern = ":", replacement = "-"), pattern = " ", replacement = "_")
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
    month.var <- str_pad(month.var, 2, 'left', pad = "0")
  }

  # analog to the if-clause before - here the parameter bio.var is checked.
  if(is.element("bio", parameter)){
    bio.var <- c(bio.var)
    if(!is.numeric(bio.var)) stop()
    bio.var <- str_pad(bio.var, 2, 'left', pad = "0")
  }

  # Download: 1. work through all parameters -----------------------------------
  for(i in parameter){

    # clear up the temporary directory
    unlink(list.files(tempdir(), recursive = T, full.names=T))

    # create intermediate strings for later use
    interm <- switch(i,
                     "prec" = "prec/",
                     "temp" = "tmean/",
                     "tmax" = "tmax/",
                     "tmin" = "tmin/",
                     "bio"  = "bio/",
                     stop())

    variable.numbers <- switch(i,
                               "bio" = bio.var,
                               "tmin" = month.var,
                               "tmax" = month.var,
                               "temp" = month.var,
                               "prec" = month.var,
                               stop())
    # if not already created, create new directory
    if (!dir.exists(paste0(save.location, "/", i))){
      dir.create(paste0(save.location, "/", i))
    }
    # set the 1. order temporal save location to this directory
    # 1. Order -> parameter!
    temp.save.location <- paste0(save.location, "/", i, "/")

    # Add "10" after parameter string for all parameters except precipitation
    if(i != "prec"){
      i <- paste0(i, "10")
    }
    ## Download: 2. Work through versions as given as initial parameter ---------
    for (version in version.var) {

      if(version == "1.1") next

      # create version string
      vers <- switch(version,
                     "1.1" = "",
                     "1.2" = "_V1.2",
                     stop())
      ### Download: 3. Preparation of the save location 2. order -----------------
      vers_path <- str_remove(vers, pattern = "_")
      # set the 2nd order temporal save location to create a
      # managable directory tree in the 1st order directory
      temp.temp.save.location <- paste0(temp.save.location,
                                        str_replace_all(interm,
                                                        pattern = "/",
                                                        "_"),
                                        vers_path, "/")
      # if not already created, create new directory
      if(!dir.exists(temp.temp.save.location)){
        dir.create(temp.temp.save.location)
      }

      # normalize the path to make it work more easily
      # temp.temp.save.location <- normalizePath(temp.temp.save.location,
      #                                          winslash = "/")

      ##### Download: 4. Check if bio is not requested -----------------------------
      if(i != "bio10"){
        # should years be added? necessary for the download function
        years <- switch(i,
                        "prec" = "",
                        "_1979-2013")
        ###### Download: 5. Work through every requested month ----------------------
        for(month in 1:length(month.var)){
          # create a variable that is a placeholder for the produced output.
          dest.temp <- paste0(temp.temp.save.location, "CHELSA_", i, "_",
                              month.var[month], years, vers, "_land.tif")
          # check if the output already exists
          # - if yes, the download will just be skipped.
          # which makes a huge runtime difference!
          if(!file.exists(dest.temp)){
            # create a variable for the later requested Download-URL to avoid
            # requireing multiple changes, if the link changes.
            URL.temp <-
              paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/",
                     interm, "CHELSA_", i, "_", month.var[month], years,
                     vers, "_land.tif")
            # check if URL is available
            if(url.exists(URL.temp)){
              # clear up the temporary directory
              unlink(list.files(tempdir(), recursive = T, full.names=T))
              # download file to save location
              download.file(url = URL.temp,
                            destfile = dest.temp,
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
                            quiet = FALSE,
                            cacheOK = FALSE)
              if(i != "prec"){
                raster.temp <- raster(dest.temp)

                gc()
                raster.temp <- clamp(raster.temp, lower = -1000,
                                     useValues = FALSE)
                gain(raster.temp) <- 0.1
                gc()

                writeRaster(raster.temp,
                            dest.temp,
                            overwrite = TRUE)
                rm(raster.temp)
                gc()
              }
            }else{
              # Error message if file is not available
              warning(paste0("File does not exist. Did not download: \n",
                             URL.temp, "\n\n"),
                      # call is printed later or within the "warnings()"
                      call. = TRUE, immediate. = FALSE)
            }
          }
          ###### if-clause to include the post-download manipulations of the files -------
          # if clause checks for:
          # Is the month the last month of the demanded months
          # Are .tif raster files downloaded?
          if(month.var[month] == month.var[length(month.var)] &
             length(list.files(temp.temp.save.location,
                               pattern = ".tif",
                               include.dirs = FALSE)) != 0){
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
                           # conversion to ascii format here integrated into the
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
                                         parameter.var = i,
                                         variable.numbers = variable.numbers,
                                         stack.clipped = TRUE,
                                         time.stamp.var = call.time)
              }else{
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = i,
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
                                 zip.name = paste0("ChelsaClim_", i, ""),
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
            # delete all temporary files
            unlink(list.files(tempdir(), recursive = T, full.names =T))
          }
        }
      }else{
        # analog to other parameters
        # just that bio.var instead of month.var is used.
        for(bio in bio.var){
          if(version == "1.1"){
            warning("Chelsa BioClim Version 1.1 not available!")
            next
          }
          dest.temp <- paste0(temp.temp.save.location,
                              "CHELSA_", i, "_", bio, vers, ".tif")
          if(!file.exists(dest.temp)){
            URL.temp <-
              paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/",
                     interm, "CHELSA_", i, "_", bio, ".tif")
            # check if URL is available
            if(url.exists(URL.temp)){
              # clear up the temporary directory
              unlink(list.files(tempdir(), recursive = T, full.names=T))

              # download file to save location
              download.file(url = URL.temp,
                            destfile = dest.temp,
                            overwrite = TRUE,
                            mode = 'wb',
                            quiet = FALSE)

              if(bio <= 11){
                raster.temp <- raster(dest.temp)
                # raster.values <- values(raster.temp)
                # raster.values[raster.values==-32768] <- NA
                # values(raster.temp) <- raster.values
                # rm(raster.values)

                gc()
                raster.temp <- clamp(raster.temp, lower = -1000,
                                     useValues = FALSE)
                gain(raster.temp) <- 0.1
                writeRaster(raster.temp,
                            dest.temp,
                            overwrite = TRUE)
                rm(raster.temp)
                gc()
              }else{
                raster.temp <- raster(paste0(temp.temp.save.location, "CHELSA_",
                                             i, "_", bio, vers, ".tif"))
                gc()
                raster.temp <- clamp(raster.temp, lower = -1000,
                                     useValues = FALSE)
                gc()
                writeRaster(raster.temp,
                            paste0(temp.temp.save.location, "CHELSA_",
                                   i, "_", bio, vers, ".tif"),
                            overwrite = TRUE)
                rm(raster.temp)
                gc()
              }
            }else{
              # Error message
              warning(paste0("File does not exist. Did not download: \n", URL.temp),
                      call. = TRUE, immediate. = FALSE)
            }

          }
          if(bio == bio.var[length(bio.var)] &
             length(list.files(temp.temp.save.location,
                               pattern = ".tif",
                               include.dirs = FALSE)) != 0){
            if(clipping == TRUE){
              clipping.tif(clip.save.location = temp.temp.save.location,
                           clip.shapefile = clip.shapefile,
                           clip.extent = clip.extent,
                           buffer = buffer,
                           convert.files.to.asc = convert.files.to.asc,
                           time.stamp.var = call.time)
            }
            if(convert.files.to.asc == TRUE){
              convert.to.asc(save.location = temp.temp.save.location,
                             time.stamp.var = call.time)
            }
            if(stacking.data == TRUE){
              if(clipping == TRUE){
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = i,
                                         variable.numbers = variable.numbers,
                                         stack.clipped = TRUE,
                                         time.stamp.var = call.time)
              }else{
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = i,
                                         variable.numbers = variable.numbers,
                                         time.stamp.var = call.time)
              }
            }
            if(combine.raw.zip == TRUE){
              combine.raw.in.zip(save.location = temp.temp.save.location,
                                 zip.name = paste0("ChelsaClim_", i, ""),
                                 time.stamp.var = call.time)
            }
            if(delete.raw.data == TRUE){
              unlink(list.files(temp.temp.save.location,
                                pattern = ".tif",
                                include.dirs = FALSE, full.names = T),
                     force = TRUE)
            }
            # delete all temporary files
            unlink(list.files(tempdir(), recursive = T, full.names =T))
          }
        }
      }
      # if no subdirectories and files are to be found in the 2nd order
      # subdirectory the directory will be deleted for a better overview of
      # given data.
      if(length(list.files(temp.temp.save.location,
                           include.dirs = TRUE)) == 0){

        unlink(str_sub(temp.temp.save.location, 1,
                       end = str_length(temp.temp.save.location)-1),
               force = TRUE, recursive = TRUE)
      }

    } # version for-loop END
    # Download END
  } # Parameters for-loop end
  # Saving BIB File
  # if save.bib.file is TRUE, the save.citation function will be called.
  # it saves a .bib-file of the downloaded data-set on the highest level of
  # the save.location. If the user's working directory is the desktop
  # and no other save.location is specified initially, on the desktop.
  if(save.bib.file == TRUE) {
    save.citation(save.location = save.location, dataSetName = "Chelsa")
  }
  # delete all temporary files
  unlink(list.files(tempdir(), recursive = T, full.names =T))
}


#'@title Function for downloading Chelsa CMIP 5 future climatologies data sets for 2050 and 2070
#'@author Helge Jentsch
#'@description This function supports a download of the Chelsa CMIP5 future climate data sets. This includes precipitation (mm), temperature (average, maximum, minimum; °C), and bioclimatic parameters. For convenience also a clipping-, conversion to ascii-grid, and stacking-tool is included.  An output of a .bib-file of the literature is also optional.  For a clear working environment, directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'@details "The Downscaled data has been produced using climatological aided interpolation based on the 1979-2013 reference climatologies from CHELSA." (Chelsa Climate 2020: \url{http://chelsa-climate.org/future/})
#'@note For some of the datasets not all models are available. For the ones that are not supported the data will not be downloaded and a warning will be prompted. See parameter \code{model.var} for more information or check the website of Chelsa Climate (\url{http://chelsa-climate.org/future/}).
#'
#'@param save.location string. Input where the datasets should be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "temp", "tmax", "tmin", "bio")}
#'@param bio.var integer (vector). Input which monthly data should be downloaded. Only applicable to BIOCLIM variables. For further information see: \url{http://chelsa-climate.org/bioclim/}. \cr Default: \code{c(1:19)}
#'@param month.var integer (vector). Input which monthly data should be downloaded. Only applicable to Precipitation and Temperature (average, maximum, minimum). \cr Default: \code{c(1:12)}
#'@param emission.scenario.var string (vector). Input which emission scenario dataset should be downloaded. Provided are the representative concentration pathways (RCP) 2.6, 4.5, 6.0, and 8.5.\cr Default: \code{c("rcp26", "rcp45", "rcp60", "rcp85")}
#'@param time.interval.var string (vector). Input for which time interval data should be downloaded. Chelsa provides downscaled CMIP5 climatologies for 2050 and 2070. Multiple inputs possible.\cr Default: \code{c("2041-2060", "2061-2080")}
#'@param model.var string (vector). Input which future model dataset should be downloaded. For more information see: \url{http://chelsa-climate.org/future/}.\cr For some of the datasets not all downloads are available. For the ones that are not supported the data will not be downloaded and a warning will be prompted. For an overview please try "warnings()" after execution. \cr Default: \code{c("ACCESS1-0", "bcc-csm1-1", "BNU-ESM", "CanESM2", "CCSM4", "CESM1-BGC", } \cr \code{"CESM1-CAM5", "CMCC-CESM", "CMCC-CM", "CMCC-CMS", "CNRM-CM5", "CSIRO-Mk3-6-0",} \cr \code{ "CSIRO-Mk3L-1-2", "EC-EARTH", "FGOALS-g2", "FIO-ESM", "GFDL-CM3", "GFDL-ESM2G", } \cr \code{"GFDL-ESM2M","GISS-E2-H", "GISS-E2-H-CC", "GISS-E2-R", "GISS-E2-R-CC", "HadGEM2-AO",} \cr \code{"HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR","MIROC-ESM",} \cr \code{"MIROC-ESM-CHEM","MIROC5", "MPI-ESM-LR", "MPI-ESM-MR", "MRI-CGCM3", "MRI-ESM1",} \cr \code{ "NorESM1-M","NorESM1-ME")}
#'@param clipping logical. Input whether the downloaded data should be clipped.\cr If \code{FALSE} \code{clip.shapefile}, buffer, clip.extent will be ignored. \cr Default: \code{FALSE}
#'@param clip.shapefile string. Input which shapefile should be used for clipping.  \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether files should be converted into the ASCII format.\cr If \code{TRUE}: a new subdirectory is created and the rawdata is saved there. If \code{clipping} is \code{TRUE}: the clipped raster files are also saved as ASCII grids.  \cr Default: \code{FALSE}
#'@param stacking.data logical. Input whether the downloaded data should be stacked as a netCDF-rasterstack. \cr  Default: \code{FALSE}
#'@param combine.raw.zip logical. Should the downloaded raw-data be "zipped". \cr  Default: \code{FALSE}
#'@param delete.raw.data  logical. Should the downloaded raw-data be deleted. If the \code{combine.raw.zip} is \code{TRUE}: raw-data is still available in the zipped file. \cr Default: \code{FALSE}
#'@param save.bib.file logical. Whether a bibTex-citation file of the data set should be provided in the Working directory. \cr Default: \code{TRUE}
#'
#'@return Downscaled Chelsa CMIP5 climatologies for 2050 and 2070.
#'
#'@examples
#' ## NOT RUN
#' ## Bioclim
#' # Chelsa.CMIP_5.download(parameter = "bio",
#' #                        bio.var = c(1,12),
#' #                        emission.scenario.var = "rcp26",
#' #                        time.interval.var = "2041-2060",
#' #                        model.var = "MPI-ESM-LR")
#' ## Precipitation
#' # Chelsa.CMIP_5.download(parameter = "prec",
#' #                        month.var = c(1,12),
#' #                        emission.scenario.var = "rcp26",
#' #                        time.interval.var = "2041-2060",
#' #                        model.var = "MPI-ESM-LR")
#' ## NOT RUN
#'
#'@import stringr
#'@import RCurl
#'@import ncdf4
#'@import raster
#'@import httr
#'@importFrom utils unzip download.file
#'
#'
#'@export
Chelsa.CMIP_5.download <- function(save.location = "./",
                                   parameter = c("prec", "temp", "tmax", "tmin", "bio"),
                                   bio.var = c(1:19),
                                   month.var = c(1:12),
                                   emission.scenario.var = c("rcp26", "rcp45",
                                                             "rcp60", "rcp85"),
                                   time.interval.var = c("2041-2060", "2061-2080"),
                                   model.var = c("ACCESS1-0", "bcc-csm1-1",
                                                 "BNU-ESM", "CanESM2", "CCSM4",
                                                 "CESM1-BGC", "CESM1-CAM5",
                                                 "CMCC-CESM", "CMCC-CM",
                                                 "CMCC-CMS", "CNRM-CM5",
                                                 "CSIRO-Mk3-6-0",
                                                 "CSIRO-Mk3L-1-2",
                                                 "EC-EARTH", "FGOALS-g2",
                                                 "FIO-ESM", "GFDL-CM3",
                                                 "GFDL-ESM2G", "GFDL-ESM2M",
                                                 "GISS-E2-H", "GISS-E2-H-CC",
                                                 "GISS-E2-R", "GISS-E2-R-CC",
                                                 "HadGEM2-AO", "HadGEM2-CC",
                                                 "HadGEM2-ES", "inmcm4",
                                                 "IPSL-CM5A-LR", "IPSL-CM5A-MR",
                                                 "MIROC-ESM", "MIROC-ESM-CHEM",
                                                 "MIROC5", "MPI-ESM-LR",
                                                 "MPI-ESM-MR", "MRI-CGCM3",
                                                 "MRI-ESM1", "NorESM1-M",
                                                 "NorESM1-ME"),
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
  # requireNamespace("stringr")
  # requireNamespace("RCurl")
  # requireNamespace("ncdf4")
  gc()
  save.location <- normalizePath(save.location, winslash = "/")
  call.time <- str_replace_all(str_replace_all(paste0(Sys.time()), pattern = ":", replacement = "-"), pattern = " ", replacement = "_")
  # the build of this function is very similar to the Chelsa.Clim.download function.
  if(is.element("prec", parameter)|is.element("temp", parameter)|
     is.element("tmax", parameter)|is.element("tmin", parameter)){
    if(!is.numeric(month.var)) stop()

  }
  if(is.element("bio", parameter)){
    if(!is.numeric(bio.var)) stop()

  }
  for(i in parameter){
    # clear up the temporary directory
    unlink(list.files(tempdir(), recursive = T, full.names=T))

    variable.numbers <- switch(i,
                               "bio" = bio.var,
                               "tmin" = month.var,
                               "tmax" = month.var,
                               "temp" = month.var,
                               "prec" = month.var,
                               stop())

    for (time.interval in time.interval.var) {
      interm <- switch(i,
                       "prec" = paste0("cmip5/", time.interval,
                                       "/prec/CHELSA_pr_mon_"),
                       "temp" = paste0("cmip5/", time.interval,
                                       "/temp/CHELSA_tas_mon_"),
                       "tmax" = paste0("cmip5/", time.interval,
                                       "/tmax/CHELSA_tasmax_mon_"),
                       "tmin" = paste0("cmip5/", time.interval,
                                       "/tmin/CHELSA_tasmin_mon_"),
                       "bio" = paste0("cmip5/", time.interval,
                                      "/bio/CHELSA_bio_mon_"),
                       stop())
      # create new directory
      if(!dir.exists(paste0(save.location, "/", i))){
        dir.create(paste0(save.location, "/", i))
      }
      temp.save.location <- paste0(save.location, "/", i, "/")
      # print(temp.save.location)
      for (model in model.var) {
        for (emission.scenario in emission.scenario.var) {
            temp.temp.save.location <- paste0(temp.save.location,
                                              str_replace_all(interm,
                                                              pattern = "/",
                                                              "_"),
                                              model,"_",
                                              emission.scenario, "/")
            if(!dir.exists(str_sub(temp.temp.save.location,
                                   end = str_length(temp.temp.save.location)-1))){
              dir.create(str_sub(temp.temp.save.location,
                                 end = str_length(temp.temp.save.location)-1))
            }

            if(i != "bio"){
              for(month in month.var){
                dest.temp <- paste0(temp.temp.save.location, "CHELSA_", model,
                                    "_", emission.scenario, "_", i, "_", month,
                                    "_", time.interval, ".tif")
                if(!file.exists(dest.temp)){
                  URL.temp <- paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/",
                                     interm, model, "_", emission.scenario,
                                     # "_r1i1p1_g025.nc_", month, "_", time.interval,
                                     "_V1.2.tif")
                  if(!http_error(URL.temp)){
                    # clear up the temporary directory
                    unlink(list.files(tempdir(), recursive = T, full.names=T))

                    download.file(url = URL.temp,
                                  destfile = dest.temp,
                                  overwrite = TRUE,
                                  mode = 'wb',
                                  quiet = FALSE)
                    if(i != "prec"){
                      gc()
                      raster.temp <- raster(dest.temp)
                      raster.temp <- clamp(raster.temp, lower = -1000,
                                           useValues = FALSE)
                      gc()

                      # raster.temp <- process.raster.int.doub(raster.temp)
                      gain(raster.temp) <- 0.1
                      gc()
                      writeRaster(raster.temp,
                                  dest.temp,
                                  overwrite = TRUE)
                      rm(raster.temp)
                      gc()
                    }
                  }else{
                    warning(paste0("File does not exist. Did not download: \n",
                                   URL.temp), call. = TRUE, immediate. = FALSE)
                  }
                }
                if(month.var[month] == month.var[length(month.var)] &
                   length(list.files(temp.temp.save.location,
                                     pattern = ".tif",
                                     include.dirs = FALSE)) != 0){
                  if(clipping == TRUE){
                    clipping.tif(clip.save.location = temp.temp.save.location,
                                 clip.shapefile = clip.shapefile,
                                 clip.extent = clip.extent,
                                 buffer = buffer,
                                 convert.files.to.asc = convert.files.to.asc,
                                 time.stamp.var = call.time)
                  }
                  if(convert.files.to.asc == TRUE){
                    convert.to.asc(save.location = temp.temp.save.location,
                                   time.stamp.var = call.time)
                  }
                  if(stacking.data == TRUE){
                    if(clipping==TRUE){
                      stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                               parameter.var = i,
                                               variable.numbers = variable.numbers,
                                               stack.clipped = TRUE,
                                               time.stamp.var = call.time)
                    }else{
                      stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                               parameter.var = i,
                                               variable.numbers = variable.numbers,
                                               time.stamp.var = call.time)
                    }
                  }
                  if(combine.raw.zip == TRUE){
                    combine.raw.in.zip(save.location = temp.temp.save.location,
                                       zip.name = paste0("ChelsaCMIP5_", i, ""),
                                       time.stamp.var = call.time)
                  }
                  if(delete.raw.data == TRUE){
                    unlink(list.files(temp.temp.save.location,
                                      pattern = ".tif",
                                      include.dirs = FALSE, full.names = T), force = TRUE)
                  }
                }
              }
            }else{
              for(bio in bio.var){
                dest.temp <- paste0(temp.temp.save.location, "CHELSA_", model,
                                    "_", emission.scenario, "_", i, "_", bio,
                                    "_", time.interval,".tif")
                if(!file.exists(dest.temp)){
                  # clear up the temporary directory
                  unlink(list.files(tempdir(), recursive = T, full.names=T))

                  URL.temp <- paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/",
                                     interm, model, "_",
                                     emission.scenario, "_r1i1p1_g025.nc_",
                                     bio, "_", time.interval, "_V1.2.tif")
                  if(!http_error(URL.temp)){
                    download.file(url = URL.temp,
                                  destfile = dest.temp,
                                  overwrite = FALSE,
                                  mode = 'wb',
                                  quiet = FALSE)

                    raster.temp <- raster(dest.temp)
                    # raster.values <- values(raster.temp)
                    # raster.values[raster.values==-32768] <- NA
                    # values(raster.temp) <- raster.values
                    # rm(raster.values); gc()
                    raster.temp <- clamp(raster.temp,
                                         lower = -1000,
                                         useValues = FALSE)
                    gc()
                    if(bio <= 11){
                      gc()
                      gain(raster.temp) <- 0.1
                    }
                    writeRaster(raster.temp,
                                dest.temp,
                                overwrite = TRUE)
                    rm(raster.temp)
                    gc()
                  }else{
                    warning(paste0("File does not exist. Did not download: \n", URL.temp),
                            call.=TRUE, immediate. = FALSE)
                  }
                }
                if(bio == bio.var[length(bio.var)] &
                   length(list.files(temp.temp.save.location,
                                     pattern = ".tif",
                                     include.dirs = FALSE)) != 0){
                  if(clipping == TRUE){
                    clipping.tif(clip.save.location = temp.temp.save.location,
                                 clip.shapefile = clip.shapefile,
                                 clip.extent = clip.extent,
                                 buffer = buffer,
                                 convert.files.to.asc = convert.files.to.asc,
                                 time.stamp.var = call.time)
                  }
                  if(convert.files.to.asc == TRUE){
                    convert.to.asc(save.location = temp.temp.save.location,
                                   time.stamp.var = call.time)
                  }
                  if(stacking.data == TRUE){
                    if(clipping==TRUE){
                      stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                               parameter.var = i,
                                               variable.numbers = variable.numbers,
                                               stack.clipped = TRUE,
                                               time.stamp.var = call.time)
                    }else{
                      stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                               parameter.var = i,
                                               variable.numbers = variable.numbers,
                                               time.stamp.var = call.time)

                    }
                  }
                  if(combine.raw.zip == TRUE){
                    combine.raw.in.zip(save.location = temp.temp.save.location,
                                       zip.name = paste0("ChelsaCMIP5_", i, ""),
                                       time.stamp.var = call.time)
                  }
                  if(delete.raw.data == TRUE){
                    unlink(list.files(temp.temp.save.location,
                                      pattern = ".tif",
                                      include.dirs = FALSE, full.names = T), force = TRUE)
                  }
                }
              }
            }
            if(length(list.files(temp.temp.save.location,
                                 include.dirs = TRUE)) == 0){
              unlink(str_sub(temp.temp.save.location, 1,
                             end = str_length(temp.temp.save.location)-1),
                     force = TRUE,
                     recursive = TRUE)
            }

        }
      }
    }
  }
  # Saving BIB File ---------------------------------------------------------
  if(save.bib.file == TRUE) save.citation(save.location = save.location, dataSetName = "Chelsa")
}

#'@title Function for downloading Chelsa Last Glacial Maximum datasets
#'@author Helge Jentsch
#'@description This function supports a download of the Chelsa Last Glacial Maximum Climate datasets (21.000 years ago). This includes precipitation (mm), temperature (average, maximum, minimum; °C), bioclimatic parameters, and a global digital elevation model. For further information, please regard \url{http://chelsa-climate.org/last-glacial-maximum-climate/}.\cr For convenience also a clipping-, conversion to ascii-grid, and stacking-tool is included. \cr An output of a .bib-file of the literature is also optional. \cr For a clear working environment, directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'@details "The CHELSA LGM data is based on a implementation of the CHELSA algorithm on PMIP3 data." (Chelsa Climate 2020: \url{http://chelsa-climate.org/last-glacial-maximum-climate/})
#'@note For some of the datasets not all models are available. For the ones that are not supported the data will not be downloaded and a warning will be prompted. See parameter \code{model.var} for more information or check the website of Chelsa Climate (\url{http://chelsa-climate.org/last-glacial-maximum-climate/}).
#'
#'
#'@param save.location string. Input where the datasets should be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "temp", "tmax", "tmin", "bio")}
#'@param bio.var integer (vector). Input which bioclim data should be downloaded. Only applicable to BIOCLIM variables. For further information see: \url{http://chelsa-climate.org/bioclim/}. \cr Default: \code{c(1:19)}
#'@param month.var integer (vector). Input which monthly data should be downloaded. Only applicable to Precipitation and Temperature (average, maximum, minimum). \cr Default: \code{c(1:12)}
#'@param model.var string (vector). Input which future model dataset should be downloaded. For more information see: \url{http://chelsa-climate.org/last-glacial-maximum-climate/}. For some of the datasets not all downloads are available. For the ones that are not supported the data will not be downloaded and a warning will be prompted. For an overview please try "warnings()" after execution. \cr Default: \code{c("CCSM4", "MRI-CGCM3", "CNRM-CM5", } \cr \code{"FGOALS-g2", "IPSL-CM5A-LR", "MIROC-ESM", "MPI-ESM-P")}
#'@param download.dem logical. Input whether a LGM digital elevation model should be downloaded. \cr Default: \code{FALSE}
#'@param clipping logical. Input whether the downloaded data should be clipped.\cr If \code{FALSE}; clip.shapefile, buffer, clip.extent will be ignored. \cr Default: \code{FALSE}
#'@param clip.shapefile string. Input which shapefile should be used for clipping. \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether files should be converted into the ASCII format. If \code{TRUE}: a new subdirectory is created and the rawdata is saved there. \cr If \code{clipping} is \code{TRUE}: the clipped raster files are also saved as ASCII grids. \cr  Default: \code{FALSE}
#'@param stacking.data logical. Input whether the downloaded data should be stacked as a netCDF-rasterstack. \cr Default: \code{FALSE}
#'@param combine.raw.zip logical. Should the downloaded raw-data be "zipped". \cr Default: \code{FALSE}
#'@param delete.raw.data  logical. Should the downloaded raw-data be deleted. If the "combine.raw.zip"-option is \code{TRUE}, raw-data is still available in the zipped file.\cr Default: \code{FALSE}
#'@param save.bib.file logical. Whether a bibTex-citation file of the Chelsa-dataset should be provided in the Working directory. \cr Default: \code{TRUE}
#'
#'@return Downscaled global climatological data from the last glacial maximum.
#'
#'@examples
#' ## NOT RUN
#' ## Bioclim
#' # Chelsa.lgm.download(parameter = "bio",
#' #                    bio.var = c(1,12),
#' #                    model.var = "MPI-ESM-P")
#' ## Precipitation
#' # Chelsa.lgm.download(parameter = "prec",
#' #                    month.var = c(1,12),
#' #                    model.var = "MPI-ESM-P")
#' ## NOT RUN
#'
#'
#'@import stringr
#'@import RCurl
#'@import ncdf4
#'@import raster
#'@importFrom utils unzip download.file
#'
#'
#'@export
Chelsa.lgm.download <- function(save.location = "./",
                                parameter = c("prec", "temp", "tmax", "tmin", "bio"),
                                bio.var = c(1:19),
                                month.var = c(1:12),
                                model.var=c("CCSM4", "MRI-CGCM3", "CNRM-CM5",
                                            "FGOALS-g2", "IPSL-CM5A-LR",
                                            "MIROC-ESM", "MPI-ESM-P"
                                ),
                                download.dem = FALSE,
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
  # requireNamespace("stringr")
  # requireNamespace("RCurl")
  # requireNamespace("ncdf4")
  gc()
  call.time <- str_replace_all(str_replace_all(paste0(Sys.time()), pattern = ":", replacement = "-"), pattern = " ", replacement = "_")
  # initial check -----------------------------------------------------------
  # normalize Path for easier application later
  save.location <- normalizePath(save.location, winslash = "/")
  # Check which parameters are put in and if the connected
  # month/bio-variables are correctly input
  if(is.element("prec", parameter)|is.element("temp", parameter)|
     is.element("tmax", parameter)|is.element("tmin", parameter)){
    month.var <- c(month.var)
    if(!is.numeric(month.var)) stop()
    # month.var <- str_pad(month.var, 2, 'left', pad = "0")
  }

  if(is.element("bio", parameter)){
    bio.var <- c(bio.var)
    if(!is.numeric(bio.var)) stop()
    bio.var <- as.character(bio.var)
    bio.var <- str_pad(bio.var, 2, 'left', pad = "0")
    # print(bio.var)
  }

  # through all given parameters --------------------------------------------
  # work through paramerters
  for(i in parameter){
    # clear up the temporary directory
    unlink(list.files(tempdir(), recursive = T, full.names=T))

    # create intermediate strings for later use
    interm <- switch(i,
                     "prec" = "prec/",
                     "temp" = "tmean/",
                     "tmax" = "tmax/",
                     "tmin" = "tmin/",
                     "bio"  = "bioclim/",
                     stop())

    variable.numbers <- switch(i,
                               "bio" = bio.var,
                               "tmin" = month.var,
                               "tmax" = month.var,
                               "temp" = month.var,
                               "prec" = month.var,
                               stop())

    # create new directory
    dir.create(paste0(save.location, "/", i))
    temp.save.location <- paste0(save.location, "/", i, "/")

    for (model in model.var) {
      # download of the requested data sets -------------------------------------
      temp.temp.save.location <- paste0(temp.save.location,
                                        str_replace_all(interm,
                                                        pattern = "/",
                                                        "_"),
                                        "LGM_PMIP_", model, "/")
      if(!dir.exists(temp.temp.save.location)){
        dir.create(temp.temp.save.location)
      }

      # temp.temp.save.location <- normalizePath(temp.temp.save.location,
      #                                          winslash = "/")

      # Check if bio is not requested
      if(i != "bio"){
        if(i == "temp") {
          month.var <- str_pad(month.var, 2, 'left', pad = "0")
        }else{
          month.var <- as.integer(month.var)
        }

        # work through every requested month
        for(month in 1:length(month.var)){
          # clear up the temporary directory
          unlink(list.files(tempdir(), recursive = T, full.names=T))

          dest.temp <- paste0(temp.temp.save.location, "CHELSA_PMIP_", model, "_",
                              i,"_",month.var[month],".tif")
          if(!file.exists(dest.temp)){
            if(i != "temp") {
              URL.temp <-
                paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/pmip3/",
                       interm, "CHELSA_PMIP_", model,"_",i,"_", month.var[month],"_1.tif")
            }else{
              if(model == "CCSM4"){
              URL.temp <-
                paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/pmip3/",
                       interm, "CHELSA_PMIP_", model, "_tmean_", month.var[month],".tif")
              }else{
                month.var <- as.integer(month.var)
                URL.temp <-
                  paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/pmip3/",
                         interm, "CHELSA_PMIP_", model,"_tmean_", month.var[month],"_1.tif")
              }
            }
            # check if URL is available
            if(url.exists(URL.temp)){
              # download file to save location
              download.file(url = URL.temp,
                            destfile = dest.temp,
                            overwrite = TRUE,
                            mode = 'wb',
                            quiet = FALSE)
              if(i != "prec"){
                raster.temp <- raster(dest.temp)

                raster.temp <- clamp(raster.temp, lower = -1000, useValues= FALSE)
                gc()

                # Conversion Float
                gain(raster.temp) <- 0.1
                # umrechnung Kelvin - Celsius
                gc()
                offs(raster.temp) <- -273.15

                writeRaster(raster.temp,
                            dest.temp,
                            overwrite = TRUE)
                rm(raster.temp)
                gc()
              }else{
                # for precipitation as http://chelsa-climate.org/last-glacial-maximum-climate/ says
                raster.temp <- raster(dest.temp)
                raster.temp <- clamp(raster.temp, upper = 30000, useValues= FALSE)
                gc()
                gain(raster.temp) <- 0.1
                writeRaster(raster.temp,
                            dest.temp,
                            overwrite = TRUE)
                rm(raster.temp)
                gc()
              }
            }else{
              # Error message
              warning(paste0("File does not exist. Did not download: \n", URL.temp, "\n\n"),
                      call. = TRUE, immediate. = FALSE)
            }
          }
          if(month.var[month] == month.var[length(month.var)] &
             length(list.files(temp.temp.save.location,
                               pattern = ".tif",
                               include.dirs = FALSE)) != 0){
            if(clipping == TRUE){
              clipping.tif(clip.save.location = temp.temp.save.location,
                           clip.shapefile = clip.shapefile,
                           clip.extent = clip.extent,
                           buffer = buffer,
                           convert.files.to.asc = convert.files.to.asc,
                           time.stamp.var = call.time)
            }
            if(convert.files.to.asc == TRUE){
              convert.to.asc(save.location = temp.temp.save.location,
                             time.stamp.var = call.time)
            }
            if(stacking.data == TRUE){
              if(clipping==TRUE){
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = i,
                                         variable.numbers = variable.numbers,
                                         stack.clipped = TRUE,
                                         time.stamp.var = call.time)
              }else{
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = i,
                                         variable.numbers = variable.numbers,
                                         time.stamp.var = call.time)
              }
            }
            if(combine.raw.zip == TRUE){
              combine.raw.in.zip(save.location = temp.temp.save.location,
                                 zip.name = paste0("ChelsaLGM_", i, ""),
                                 time.stamp.var = call.time)
            }
            if(delete.raw.data == TRUE){
              unlink(list.files(temp.temp.save.location,
                                pattern = ".tif",
                                include.dirs = FALSE, full.names = T), force = TRUE)
            }
          }
        }
      }else{
        # analog to other parameters
        # just that bio.var instead of month.var is used.
        for(bio in bio.var){
          dest.temp <- paste0(temp.temp.save.location, "CHELSA_PMIP_", model,
                              "_BIO_", bio,".tif")
          if(!file.exists(dest.temp)){
            URL.temp <-
              paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/pmip3/",
                     interm, "CHELSA_PMIP_", model, "_BIO_",bio,".tif")

            # check if URL is available
            if(url.exists(URL.temp)){
              # download file to save location
              download.file(url = URL.temp,
                            destfile = dest.temp,
                            overwrite = TRUE,
                            mode = 'wb',
                            quiet = FALSE)
              # Casting into floats and deleting NA values
              raster.temp <- raster(dest.temp)
              raster.temp <- clamp(raster.temp, lower = -1000, useValues= FALSE)
              gc()

              if(bio <= 11){
                # values(raster.temp) <- as.numeric(values(raster.temp)/10)
                gain(raster.temp) <- 0.1
              }
              writeRaster(raster.temp,
                          dest.temp,
                          overwrite = TRUE)
              rm(raster.temp)
              gc()
            }else{
              # Error message
              warning(paste0("File does not exist. Did not download: \n", URL.temp),
                      call. = TRUE, immediate. = FALSE)
            }
          }
          if(bio == bio.var[length(bio.var)] &
             length(list.files(temp.temp.save.location,
                               pattern = ".tif",
                               include.dirs = FALSE)) != 0){
            if(clipping == TRUE){
              clipping.tif(clip.save.location = temp.temp.save.location,
                           clip.shapefile = clip.shapefile,
                           clip.extent = clip.extent,
                           buffer = buffer,
                           convert.files.to.asc = convert.files.to.asc,
                           time.stamp.var = call.time)
            }
            if(convert.files.to.asc == TRUE){
              convert.to.asc(save.location = temp.temp.save.location,
                             time.stamp.var = call.time)
            }
            if(stacking.data == TRUE){
              if(clipping==TRUE){
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = i,
                                         variable.numbers = variable.numbers,
                                         stack.clipped = TRUE,
                                         time.stamp.var = call.time)
              }else{
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = i,
                                         variable.numbers = variable.numbers,
                                         time.stamp.var = call.time)
              }
            }
            if(combine.raw.zip == TRUE){
              combine.raw.in.zip(save.location = temp.temp.save.location,
                                 zip.name = paste0("ChelsaLGM_", i, ""),
                                 time.stamp.var = call.time)
            }
            if(delete.raw.data == TRUE){
              unlink(list.files(temp.temp.save.location,
                                pattern = ".tif",
                                include.dirs = FALSE, full.names = T), force = TRUE)
            }
          }
        }
      }

      if(length(list.files(temp.temp.save.location,
                           include.dirs = TRUE)) == 0){
        unlink(str_sub(temp.temp.save.location, 1, end = str_length(temp.temp.save.location)-1),
               force = T, recursive = TRUE)
      }
    }
  }
  if(download.dem == TRUE){
    if(!dir.exists(paste0(save.location, "/elev"))){
      dir.create(paste0(save.location, "/elev"))
    }
    if(!dir.exists(paste0(save.location, "/elev/Chelsa_LGM_Elevation_Grid"))){
      dir.create(paste0(save.location, "/elev/Chelsa_LGM_Elevation_Grid"))
    }
    if(!file.exists(paste0(save.location, "/elev/Chelsa_LGM_Elevation_Grid/", "CHELSA_PMIP_dem_global.tif"))){
      download.file(url = "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/pmip3/DEM/high_longlat.tif",
                    destfile = paste0(save.location, "/elev/Chelsa_LGM_Elevation_Grid/", "CHELSA_PMIP_dem_global.tif"),
                    overwrite = TRUE,
                    mode = 'wb',
                    quiet = FALSE)
    }
  }
# Saving BIB File
  if(save.bib.file == TRUE) save.citation(save.location = save.location, dataSetName = "Chelsa")
}

#'@title Chelsa Timeseries Download
#'@author Helge Jentsch
#'@description This function supports a download of the Chelsa Timeseries dataset (Jan. 1979 - Dec. 2013). This includes precipitation (mm) and temperature (average, maximum, minimum; °C) parameters. For further information, please regard \url{http://chelsa-climate.org/timeseries/}.\cr For convenience also a clipping-, conversion to ascii-grid, and stacking-tool is included. \cr An output of a .bib-file of the literature is also optional. \cr For a clear working environment, directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'@param save.location string. Input where the datasets should be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "tmax", "tmin")}
#'@param start.year.var integer. Input year the download timeseries starts. \cr Default: 1979 (minimum)
#'@param start.month.var integer. Input month the download timeseries starts. \cr Default: 1 (minimum)
#'@param end.year.var integer. Input year the download timeseries ends. \cr Default: 2013 (maximum)
#'@param end.month.var integer. Input month the download timeseries ends. \cr Default: 12 (maximum)
#'@param include.month.var integer (vector). Input which monthly data should be downloaded. \cr Default: \code{c(1:12)}
#'@param clipping logical. Input whether the downloaded data should be clipped.\cr If \code{FALSE}: \code{clip.shapefile}, \code{buffer}, \code{clip.extent} will be ignored. \cr Default: \code{FALSE}
#'@param clip.shapefile string. Input which shapefile should be used for clipping. \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether files should be converted into the ASCII format.\cr If \code{TRUE}: a new subdirectory is created and the rawdata is saved there. \cr If \code{clipping} is \code{TRUE}: the clipped raster files are also saved as ASCII grids. \cr  Default: \code{FALSE}
#'@param stacking.data logical. Input whether the downloaded data should be stacked as a netCDF-rasterstack. \cr Default: \code{FALSE}
#'@param combine.raw.zip logical. Should the downloaded raw-data be "zipped". \cr Default: \code{FALSE}
#'@param delete.raw.data  logical. Should the downloaded raw-data be deleted. If \code{combine.raw.zip} is \code{TRUE}: raw-data is still available in the zipped file. \cr Default: \code{FALSE}
#'@param save.bib.file logical. Whether a BibTex-citation file of the dataset should be provided in the Working directory. \cr Default: \code{TRUE}
#'
#'@return Custom dataset of Chelsa Timeseries for a chosen timeseries.
#'
#'@examples
#' ## NOT RUN
#' # Chelsa.timeseries.download(parameter = "prec",
#' #                            start.year.var = 2000,
#' #                            start.month.var = 1,
#' #                            end.year.var = 2002,
#' #                            end.month.var = 12,
#' #                            include.month.var = c(1,12))
#' ## END
#'
#'@import stringr
#'@import RCurl
#'@import ncdf4
#'@import raster
#'@import httr
#'@importFrom utils unzip download.file
#'
#'
#'@export
Chelsa.timeseries.download <- function(save.location = "./",
                                       parameter = c("prec", "tmax", "tmin"),
                                       start.year.var = 1979,
                                       start.month.var = 1,
                                       end.year.var = 2013,
                                       end.month.var = 12,
                                       include.month.var = c(1:12),
                                       # bio.var = c(1:19),
                                       clipping = FALSE,
                                       clip.shapefile = NULL,
                                       buffer = 0,
                                       clip.extent = c(-180, 180, -90, 90),
                                       convert.files.to.asc = FALSE,
                                       stacking.data = FALSE,
                                       combine.raw.zip = FALSE,
                                       delete.raw.data  = FALSE,
                                       save.bib.file = TRUE){
  # requireNamespace("stringr")
  # requireNamespace("RCurl")
  # requireNamespace("ncdf4")
  gc()
  call.time <- str_replace_all(str_replace_all(paste0(Sys.time()), pattern = ":", replacement = "-"), pattern = " ", replacement = "_")
  # initial check -----------------------------------------------------------
  # normalize Path for easier application later
  save.location <- normalizePath(save.location, winslash = "/")
  # Check which parameters are put in and if the connected
  # month/bio-variables are correctly input
  if(is.element("prec", parameter)|
     is.element("tmax", parameter)|is.element("tmin", parameter)){
    include.month.var <- c(include.month.var)
    if(!is.numeric(include.month.var)) stop()
    include.month.var <- str_pad(include.month.var, 2, 'left', pad = "0")
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


  # Parameter and directories -----------------------------------------------
  # work through paramerters
  for(i in parameter){
    # clear up the temporary directory
    unlink(list.files(tempdir(), recursive = T, full.names=T))

    # create intermediate strings for later use
    interm <- switch(i,
                     "prec"  = "prec/",
                     "temp"  = "tmean/",
                     "tmax"  = "tmax/",
                     "tmin"  = "tmin/",
                     # "bio"  = "bio/",
                     stop())

    variable.numbers <- switch(i,
                               # "bio" = bio.var,
                               "tmin" = include.month.var,
                               "tmax" = include.month.var,
                               "temp" = include.month.var,
                               "prec" = include.month.var,
                               stop())

    # create new directory
    if(!dir.exists(paste0(save.location, "/", i))){
      dir.create(paste0(save.location, "/", i), showWarnings = FALSE)
    }
    temp.save.location <- paste0(save.location, "/", i, "/")
    # to go analog to the functions before
    temp.temp.save.location <- paste0(temp.save.location,
                                      str_replace_all(interm,
                                                      pattern = "/",
                                                      "_"),
                                      "timeseries","/")
    # print(str_sub(temp.temp.save.location, end=-2))
    if(!dir.exists(temp.temp.save.location)){
      dir.create(str_sub(temp.temp.save.location, end=-2))
    }

    # temp.temp.save.location <- normalizePath(temp.temp.save.location,
    #                                          winslash = "/")
    # if(i == "temp"){
    #   i <- "tmean"
    # }
    # print(interm)
    # Download ----------------------------------------------------------------
    # if(i != "bio"){
    for (year_month in ts_string){
      URL.temp <-
        paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/timeseries/",
               interm, "CHELSA_", i,"_",year_month,
               "_V1.2.1.tif")
      # check if URL is available
      if(!http_error(URL.temp)){
        # clear up the temporary directory
        unlink(list.files(tempdir(), recursive = T, full.names=T))

        dest.file <- paste0(temp.temp.save.location, "CHELSA_", i,
                            "_", year_month, "_V1.2.1.tif")
        if(!file.exists(dest.file)){
          # download file to save location
          download.file(url = URL.temp,
                        destfile = dest.file,
                        overwrite = TRUE,
                        mode = 'wb',
                        quiet = FALSE)


        if(i != "prec"){
          raster.temp <- raster(dest.file)
          # raster.values <- values(raster.temp)
          # raster.values[raster.values==-32768] <- NA
          # values(raster.temp) <- raster.values
          # rm(raster.values)

          raster.temp <- clamp(raster.temp, lower = -1000, useValues = FALSE)
          gc()

          # values(raster.temp) <- as.numeric(values(raster.temp)/10)
          # values(raster.temp) <- as.numeric(values(raster.temp)-273.15)
          gain(raster.temp) <- 0.1
          offs(raster.temp) <- -273.15

          writeRaster(raster.temp,
                      dest.file,
                      overwrite = TRUE)
          rm(raster.temp)
          gc()
        }
        }
      }else{
        # Warning message
        warning(paste0("File does not exist. Did not download: \n", URL.temp, "\n\n"),
                call. = TRUE, immediate. = FALSE)
      }
      if(year_month == ts_string[length(ts_string)] &
         length(list.files(temp.temp.save.location,
                           pattern = ".tif",
                           include.dirs = FALSE)) != 0){
        if(clipping == TRUE){
          clipping.tif(clip.save.location = temp.temp.save.location,
                       clip.shapefile = clip.shapefile,
                       clip.extent = clip.extent,
                       convert.files.to.asc = convert.files.to.asc,
                       buffer = buffer,
                       time.stamp.var = call.time)
        }
        if(convert.files.to.asc == TRUE){
          convert.to.asc(temp.temp.save.location,
                         time.stamp.var = call.time)
        }
        if(stacking.data == TRUE){
          if(clipping==TRUE){
            stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                     parameter.var = i,
                                     variable.numbers = variable.numbers,
                                     stack.clipped = TRUE,
                                     stack.time.series = TRUE,
                                     time.series = ts_string,
                                     time.stamp.var = call.time)
          }else{
            stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                     parameter.var = i,
                                     variable.numbers = variable.numbers,
                                     stack.time.series = TRUE,
                                     time.series = ts_string,
                                     time.stamp.var = call.time)
          }
        }
        if(combine.raw.zip == TRUE){
          combine.raw.in.zip(save.location = temp.temp.save.location,
                             zip.name = paste0("ChelsaTimeseries_", i, ""),
                             time.stamp.var = call.time)
        }
        if(delete.raw.data == TRUE){
          unlink(list.files(temp.temp.save.location,
                            pattern = ".tif",
                            include.dirs = FALSE, full.names = T), force = TRUE)
        }
      }
    }

    # Clean up, if no data was downloaded. ------------------------------------


    if(length(list.files(temp.temp.save.location,
                         include.dirs = TRUE)) == 0){
      unlink(str_sub(temp.temp.save.location, 1, end = str_length(temp.temp.save.location)-1),
             force = T, recursive = TRUE)
    }
  }
  # Saving BIB File
  if(save.bib.file == TRUE) save.citation(save.location = save.location, dataSetName = "Chelsa")
}

#'@title Chelsa CRU Timeseries Download
#'@author Helge Jentsch
#'@description This function supports a download of the Chelsa CRU Timeseries dataset (Jan. 1901 - Dec. 2016). This includes precipitation (mm) and temperature (maximum, minimum; °C) parameters. For further information, please regard \url{http://chelsa-climate.org/chelsacruts/}.\cr For convenience also a clipping-, conversion to ascii-grid, and stacking-tool is included. \cr An output of a .bib-file of the literature is also optional. \cr For a clear working environment, directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'@param save.location string. Input where the datasets should be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "tmax", "tmin")}
#'@param start.year.var integer. Input year the download timeseries starts. \cr Default: 1901 (minimum)
#'@param start.month.var integer. Input month the download timeseries starts. \cr Default: 1 (minimum)
#'@param end.year.var integer. Input year the download timeseries ends. \cr Default: 2016 (maximum)
#'@param end.month.var integer. Input month the download timeseries ends. \cr Default: 12 (maximum)
#'@param include.month.var integer (vector). Input which monthly data should be downloaded. \cr Default: \code{c(1:12)}
#'@param clipping logical. Input whether the downloaded data should be clipped.\cr If \code{FALSE}: \code{clip.shapefile}, \code{buffer}, \code{clip.extent} will be ignored. \cr Default: \code{FALSE}
#'@param clip.shapefile string. Input which shapefile should be used for clipping. \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether files should be converted into the ASCII format.\cr If \code{TRUE}: a new subdirectory is created and the rawdata is saved there. \cr If \code{clipping} is \code{TRUE}: the clipped raster files are also saved as ASCII grids. \cr  Default: \code{FALSE}
#'@param stacking.data logical. Input whether the downloaded data should be stacked as a netCDF-rasterstack. \cr Default: \code{FALSE}
#'@param combine.raw.zip logical. Should the downloaded raw-data be "zipped". \cr Default: \code{FALSE}
#'@param delete.raw.data  logical. Should the downloaded raw-data be deleted. If \code{combine.raw.zip} is \code{TRUE}: raw-data is still available in the zipped file. \cr Default: \code{FALSE}
#'@param save.bib.file logical. Whether a bibTex-citation file of the dataset should be provided in the Working directory. \cr Default: \code{TRUE}
#'
#'@return Custom dataset of Chelsa CRU Timeseries for a chosen timeseries.
#'
#'@examples
#' ## NOT RUN
#' # Chelsa.CRUts.download(parameter = "prec",
#' #                       start.year.var = 2000,
#' #                       start.month.var = 1,
#' #                       end.year.var = 2002,
#' #                       end.month.var = 12,
#' #                       include.month.var = c(1,12))
#' ## END
#'
#'@import stringr
#'@import RCurl
#'@import ncdf4
#'@import raster
#'@import httr
#'@importFrom utils unzip download.file
#'
#'
#'@export
Chelsa.CRUts.download <- function(save.location = "./",
                                  parameter = c("prec", "tmax", "tmin"),
                                  start.year.var = 1901,
                                  start.month.var = 1,
                                  end.year.var = 2016,
                                  end.month.var = 12,
                                  include.month.var = c(1:12),
                                  # float.temperature.format = TRUE,
                                  clipping = FALSE,
                                  clip.shapefile = NULL,
                                  buffer = 0,
                                  clip.extent = c(-180, 180, -90, 90),
                                  convert.files.to.asc = FALSE,
                                  stacking.data = FALSE,
                                  combine.raw.zip = FALSE,
                                  delete.raw.data = FALSE,
                                  save.bib.file = TRUE
){
  # requireNamespace("stringr")
  # requireNamespace("RCurl")
  # requireNamespace("ncdf4")
  gc()
  call.time <- str_replace_all(str_replace_all(paste0(Sys.time()), pattern = ":", replacement = "-"), pattern = " ", replacement = "_")
  # initial check -----------------------------------------------------------
  # normalize Path for easier application later
  save.location <- normalizePath(save.location, winslash = "/")
  # Check which parameters are put in and if the connected
  # month/bio-variables are correctly input
  if(is.element("prec", parameter)|
     is.element("tmax", parameter)|is.element("tmin", parameter)){
    include.month.var <- c(include.month.var)
    if(!is.numeric(include.month.var)) {
      stop(paste0("Chelsa CRU timeseries is only available for ",
                  "precipitation and temperature maximum and minimum"))
    }
    include.month.var <- str_pad(include.month.var, 2, 'left', pad = "0")
    # print(include.month.var)
  }
  if(start.year.var < 1901 | end.year.var > 2016 |
     end.year.var < 1901 | start.year.var > 2016) {
    stop("Timeseries only available from 01.1901 to 12.2016. Please check input!")
  }
  # check for consistent timeseries
  if(end.year.var < start.year.var) {
    stop("Endyear is before the startyear. Please correct the input!")
  }
  if(start.year.var == end.year.var){
    if(start.month.var > end.month.var) {
      stop("End is before the start. Please correct the input!")
    }
  }

  ts_string <- seq.Date(as.Date(paste(start.year.var,
                                      start.month.var, "01", sep = "-")),
                        as.Date(paste(end.year.var,
                                      end.month.var, "01", sep = "-")),
                        by = "month")
  ts_string <- format.Date(ts_string, format = "%m_%Y")

  if(length(include.month.var)!=12){
    ts.string.temp <- c()
    for (incl.month in include.month.var) {
      # print(incl.month)
      ts.string.temp <- c(ts.string.temp,
                          ts_string[grep(pattern = paste0(incl.month,"_"),
                                         ts_string)]
      )
    }
    ts_string <- ts.string.temp
  }

  ts_string <- as.Date(paste0(ts_string,"-01"), format = "%d_%Y-%m")
  ts_string <- format.Date(ts_string, format = "%e_%Y")
  ts_string <- str_remove(ts_string, pattern = " ")

  # Parameter and directories -----------------------------------------------
  # work through paramerters
  for(i in parameter){
    # clear up the temporary directory
    unlink(list.files(tempdir(), recursive = T, full.names=T))

    # create intermediate strings for later use
    interm <- switch(i,
                     "prec"  = "prec/",
                     "tmax"  = "tmax/",
                     "tmin"  = "tmin/",
                     # "temp"  = "tmean/",
                     # "bio"  = "bioclim/",
                     stop())
    variable.numbers <- switch(i,
                               # "bio" = bio.var,
                               "tmin" = include.month.var,
                               "tmax" = include.month.var,
                               # "temp" = month.var,
                               "prec" = include.month.var,
                               stop())
    # create new directory
    dir.create(paste0(save.location, "/", i), showWarnings = FALSE)
    temp.save.location <- paste0(save.location, "/", i, "/")
    # to go analog to the functions before
    temp.temp.save.location <- paste0(temp.save.location,
                                      str_replace_all(interm,
                                                      pattern = "/",
                                                      "_"),
                                      "CRU_timeseries","/")
    if(!dir.exists(temp.temp.save.location)){
      dir.create(temp.temp.save.location)
    }

    # temp.temp.save.location <- normalizePath(temp.temp.save.location,
    #                                          winslash = "/")

    # Download ----------------------------------------------------------------
    for (year_month in ts_string){

      output.year_month <- str_split(year_month, pattern = "_")
      output.year_month <- unlist(output.year_month)
      zero <- switch (as.character(str_length(paste0(output.year_month[2],
                                                     "_", output.year_month[1]))),
                      "7" = "",
                      "6" = "0",
                      "")
      output.year_month <- paste0(output.year_month[2],
                                  "_", zero,
                                  output.year_month[1])
      rm(zero)
      URL.temp <-
        paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/chelsa_cruts/",
               interm, "CHELSAcruts_", i,"_",year_month,
               "_V.1.0.tif")
      dest.temp <- paste0(temp.temp.save.location, "CHELSA_CRUts_", i,
                          "_",output.year_month, "_V_1_0.tif")

      # check if URL is available
      if(!http_error(URL.temp)){
        gc()
        # clear up the temporary directory
        unlink(list.files(tempdir(), recursive = T, full.names=T))

        if(!file.exists(dest.temp)){
          # download file to save location
          download.file(url = URL.temp,
                        destfile = dest.temp,
                        overwrite = TRUE,
                        mode = 'wb',
                        quiet = FALSE)
          gc()
          if(i != "prec"){
            # print(Sys.time())
            raster.temp.file.dest <- paste0(normalizePath(tempdir(),
                                                          winslash = "/",
                                                          mustWork = T),
                                            "/temp_gdalwarped.tif")
            gdalUtils::gdalwarp(dest.temp, raster.temp.file.dest)
            raster.temp <- raster(raster.temp.file.dest)
            raster.temp <- clamp(raster.temp, lower = -1000, useValues = FALSE)
            gain(raster.temp) <- 0.1
            writeRaster(raster.temp,
                        dest.temp,
                        overwrite = TRUE)
            rm(raster.temp)
            gc()
            # print(Sys.time())
          }
        }
      }else{
        # Warning message
        warning(paste0("File does not exist. Did not download: \n", URL.temp, "\n\n"),
                call. = TRUE, immediate. = FALSE)
      }
      if(year_month == ts_string[length(ts_string)] &
         length(list.files(temp.temp.save.location,
                           pattern = ".tif",
                           include.dirs = FALSE)) != 0){
        if(clipping == TRUE){
          clipping.tif(clip.save.location = temp.temp.save.location,
                       clip.shapefile = clip.shapefile,
                       clip.extent = clip.extent,
                       buffer = buffer,
                       convert.files.to.asc = convert.files.to.asc,
                       time.stamp.var = call.time)
        }
        if(convert.files.to.asc == TRUE){
          convert.to.asc(temp.temp.save.location,
                         time.stamp.var = call.time)
        }
        if(stacking.data == TRUE){
          if(clipping==TRUE){
            stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                     parameter.var = i,
                                     variable.numbers = variable.numbers,
                                     stack.clipped = TRUE,
                                     stack.time.series = TRUE,
                                     time.series = ts_string,
                                     time.stamp.var = call.time)
          }else{
            stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                     parameter.var = i,
                                     variable.numbers = variable.numbers,
                                     stack.time.series = TRUE,
                                     time.series = ts_string,
                                     time.stamp.var = call.time)
          }
        }
        if(combine.raw.zip == TRUE){
          combine.raw.in.zip(save.location = temp.temp.save.location,
                             zip.name = paste0("ChelsaCRUts_", i, "_V.1.0"),
                             time.stamp.var = call.time)
        }
        if(delete.raw.data == TRUE){
          unlink(list.files(temp.temp.save.location,
                            pattern = ".tif",
                            include.dirs = FALSE, full.names = T), force = TRUE)
        }
      }
    }

    # Clean up, if no data was downloaded. ------------------------------------

    if(length(list.files(temp.temp.save.location,
                         include.dirs = TRUE)) == 0){
      unlink(str_sub(temp.temp.save.location, 1, end = str_length(temp.temp.save.location)-1),
             force = T, recursive = TRUE)
    }

  }
  # Saving BIB File
  if(save.bib.file == TRUE) save.citation(save.location = save.location, dataSetName = "Chelsa")
}
