#'@title Function for downloading the WorldClim historical climate dataset
#'@author Helge Jentsch
#'@description This function supports the download, pre-processing and management of the WorldClim historical climate dataset.\cr This comprises of monthly precipitation sums (mm), temperature (average, maximum, minimum; °C), monthly solar radiation sums (kJ m-2 day-1), wind speed (m s-1), water vapor pressure (kPa), and annual chracteristics (19 bioclimatic variables). Also an elevation raster is provided.\cr To allow pre-processing, clipping and buffering, conversion to ASCII-grids and stacking options are included.\cr Optional an output of a .bib-file of the cited literature can be retrieved.\cr For user convenience, saving directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'@note Please note that  solar radiation, wind speed, water vapor pressure, bioclimatic parameters, and elevation raster are only provided by the WorldClim Version 2.1 (current version).
#'@note Please note also that the downloaded data for temperature and the therefore also the first eleven bioclim-variables are processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time depending on the spatial resolution.
#'
#'@param save.location string. Input where the datasets should be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "temp", "tmax","tmin", "srad", "wind", "vapr", "bio", "elev").}
#'@param bio.var integer (vector). Input which monthly data should be downloaded. Only applicable to BIOCLIM variables. For further information see: \url{https://www.worldclim.org/data/bioclim.html}. \cr Default: \code{c(1:19)}
#'@param month.var integer (vector). Input which monthly data should be downloaded. \cr Default: \code{c(1:12)}
#'@param resolution string (vector). Ranging from a 10 arc-minute resolution over 5 and 2.5 arc-minute to 30 arc-second resolution.\cr Default: \code{c("10m", "5m", "2.5m", "30s")}
#'@param version.var string (vector). Input which version of the dataset should be downloaded. Multiple selection is possible. \cr Default:  \code{c("1.4", "2.1")}
#'@param clipping logical. Input whether the downloaded data should be clipped. See \code{\link{clipping.tif}} for more information. \cr If \code{FALSE}: \code{clip.shapefile}, \code{buffer}, \code{clip.extent} will be ignored. \cr Default: \code{FALSE}
#'@param clip.shapefile string. Input which shapefile should be used for clipping. \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether files should be converted into the ASCII format. See \code{\link{convert.to.asc}} for more information.\cr If \code{TRUE}: a new subdirectory is created and the rawdata is saved there. \cr If the parameter \code{clipping} is also \code{TRUE}: the clipped raster files are also saved as ASCII grids. \cr  Default: \code{FALSE}
#'@param stacking.data logical. Input whether the downloaded data should be stacked as a netCDF-rasterstack. See \code{\link{stacking.downloaded.data}} for more information. \cr Default: \code{FALSE}
#'@param keep.raw.zip logical. Should the downloaded raw-data be provided as "zip"-file. See \code{\link{combine.raw.in.zip}} for more information. \cr Default: \code{FALSE}
#'@param delete.raw.data  logical. Should the downloaded raw-data be deleted. \cr If the "combine.raw.zip"-option is \code{TRUE}: raw-data is still available in the zipped file. \cr Default: \code{FALSE}
#'@param save.bib.file logical. Whether a BibTex-citation file of the dataset should be provided in the Working directory. See \code{\link{save.citation}} for more information. \cr Default: \code{TRUE}
#'
#'@return WorldClim climate datasets for the period of 1960-1990 (for v1.4) and/or 1970-2000 (for v2.1).
#'
#'@references R. J. Hijmans, S. E. Cameron, J. L. Parra, et al. "Very high resolution interpolated climate surfaces for global land areas". In: _International Journal of Climatology_ 25.15 (2005), pp. 1965-1978. DOI: 10.1002/joc.1276. <URL: https://doi.org/10.1002/joc.1276>.
#'@references S. E. Fick and R. J. Hijmans. "WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas". In: _International Journal of Climatology_ 37.12 (Okt. 2017), pp. 4302-4315. DOI: 10.1002/joc.5086. <URL:https://doi.org/10.1002/joc.5086>.
#'
#'@examples
#' \dontrun{
#' # Bioclim
#' WorldClim.HistClim.download(parameter = "bio",
#'                             bio.var = c(1,12),
#'                             resolution = "10min",
#'                             version.var = c("1.4", "2.1"))
#' # Precipitation
#' WorldClim.HistClim.download(parameter = "prec",
#'                              month.var = c(1,12),
#'                              resolution = "10min",
#'                              version.var = c("1.4", "2.1")
#'                              )
#' }
#'
#'@import stringr
#'@importFrom curl curl_fetch_memory
#'@import RCurl
#'@import ncdf4
#'@import terra
#'@importFrom utils unzip download.file setTxtProgressBar txtProgressBar
#'
#'@export
WorldClim.HistClim.download <- function(save.location = "./",
                                        parameter = c("prec", "temp", "tmax",
                                                      "tmin", "srad", "wind",
                                                      "vapr", "bio", "elev"),
                                        bio.var = c(1:19),
                                        month.var = c(1:12),
                                        resolution = c("10min", "5min", "2.5min", "30s"),
                                        version.var = c("1.4", "2.1"),
                                        clipping = FALSE,
                                        clip.shapefile = NULL,
                                        clip.extent = c(-180, 180, -90, 90),
                                        buffer = 0,
                                        convert.files.to.asc = FALSE,
                                        stacking.data = FALSE,
                                        keep.raw.zip = FALSE,
                                        delete.raw.data  = FALSE,
                                        save.bib.file = TRUE){
  # requireNamespace("stringr")
  # requireNamespace("RCurl")
  # requireNamespace("ncdf4")
  # requireNamespace("raster")
  gc()
  call.time <- stringr::str_replace_all(
    stringr::str_replace_all(paste0(Sys.time()), 
                             pattern = ":", 
                             replacement = "-"), 
    pattern = " ", 
    replacement = "_")
  # initial check -----------------------------------------------------------
  # normalize Path for easier application later
  save.location <- base::normalizePath(save.location, winslash = "/")
  # Check which parameters are put in and if the connected
  # month/bio-variables are correctly input
  if(base::is.element("prec", parameter)| base::is.element("temp", parameter)|
     base::is.element("tmax", parameter)| base::is.element("tmin", parameter)|
     base::is.element("srad", parameter)| base::is.element("wind", parameter)|
     base::is.element("vapr", parameter)
  ){
    # if month.var is just a single numeric input it is here casted into
    # a vector for comparabilities
    month.var <- c(month.var)
    # # if there is not a numeric input -> prompt error
    # if(!is.numeric(month.var)) stop()
    # # Padding of "one-digit" months with a 0
    # month.var <- str_pad(month.var, 2, 'left', pad = "0")
  }

  # analog to the if-clause before - here the parameter bio.var is checked.
  if(is.element("bio", parameter)){
    # bio.var <- c(bio.var)
    # if(!is.numeric(bio.var)) stop()
    # bio.var <- str_pad(bio.var, 2, 'left', pad = "0")
  }

  # analog the elevation can be regarded.


  # First: Work through Version ---------------------------------------------

  for(vers in version.var){
    URL.1 <- switch (vers,
                     "1.4" = "https://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/",
                     "2.1" = "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_",
                     stop()
    )

    # Second: Work through paramters ------------------------------------------
    for(parm in parameter){
      # if not already created, create new directory
      if (!base::dir.exists(base::paste0(save.location, "/", parm))){
        base::dir.create(base::paste0(save.location, "/", parm))
      }
      # set the 1. order temporal save location to this directory
      # 1. Order -> parameter!
      temp.save.location <- base::paste0(save.location, "/", parm, "/")

      # Version Control
      if(vers == "1.4"){
        parm.temp <- switch (parm,
                             "tmin" = "tmin",
                             "tmax" = "tmax",
                             "temp" = "tmean",
                             "prec" = "prec",
                             "bio" = "bio",
                             next
        )
      }
      if(vers == "2.1"){
        parm.temp <- switch (parm,
                             "tmin" = "tmin",
                             "tmax" = "tmax",
                             "temp" = "tavg",
                             "prec" = "prec",
                             "srad" = "srad",
                             "wind" = "wind",
                             "vapr" = "vapr",
                             "elev" = "elev",
                             "bio" = "bio",
                             next
        )

      }
      variable.numbers <- switch(parm,
                                 "bio" = bio.var,
                                 "tmin" = month.var,
                                 "tmax" = month.var,
                                 "temp" = month.var,
                                 "prec" = month.var,
                                 "srad" = month.var,
                                 "wind" = month.var,
                                 "vapr" = month.var,
                                 "elev" = month.var,
                                 stop())
      # Thrid: Through resolution -----------------------------------------------
      for (res in resolution) {
        temp.temp.save.location <- base::paste0(temp.save.location,"/WorldClim_",
                                          vers, "_", parm.temp, "_", res, "/")
        # if not already created, create new directory
        if(!dir.exists(temp.temp.save.location)){
          dir.create(temp.temp.save.location)
        }
        # normalize the path to make it work more easily
        # temp.temp.save.location <- normalizePath(temp.temp.save.location,
        #                                          winslash = "/")
        # Version Control
        if(vers == "1.4"){
          # resolution control
          res.temp <- switch (res,
                              "10min" = "10m",
                              "5min" = "5m",
                              "2.5min" = "2-5m",
                              "30s" = "30s",
                              next
          )
          # warning for longer processing time
          if(res.temp == "30s") {
            warning(
              "Processing might take a while since 30s resolution is downloaded for the whole world",
              call. = TRUE,
              immediate. = TRUE)
          }
          # destination file
          dest.temp <- paste0(temp.temp.save.location, "/WC_",vers, "_",
                              res, "_", parm.temp, "_Bulk.zip")
          if(!(parm.temp == "bio" & res.temp == "30s")){
            if(!file.exists(dest.temp)){
              # create a variable for the later requested Download-URL to avoid
              # requireing multiple changes, if the link changes.
              URL.temp <- paste0(URL.1 , parm.temp, "_", res.temp, "_bil.zip")
              # check if URL is available
              # if(url.exists(URL.temp)){
              if(RCurl::url.exists(url = URL.temp)){
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
                              quiet = TRUE,
                              cacheOK = FALSE)
              }else{
                # Error message if file is not available
                warning(paste0("File does not exist. Did not download: \n",
                               URL.temp, "\n\n"),
                        # call is printed later or within the "warnings()"
                        call. = TRUE, immediate. = FALSE)
              }
            }
          }else{
            # if(parm.temp == "bio" & res.temp == "30s"){
            div.temp <- c("1-9","10-19")
            for (div in div.temp) {
              dest.temp <- paste0(temp.temp.save.location, "waWC_",vers, "_",
                                  res,"_", parm.temp, "_", div, "_Bulk.zip")
              if(!file.exists(dest.temp)){
                # create a variable for the later requested Download-URL to avoid
                # requiring multiple changes, if the link changes.
                URL.temp <- paste0(URL.1 , parm.temp, div, "_", res.temp, "_bil.zip")
                urlCheck <- curl_fetch_memory(url = URL.temp)$status_code
                # check if URL is available
                # if(url.exists(URL.temp)){
                if(urlCheck == 200){
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
                                quiet = FALSE)
                }else{
                  # Error message if file is not available
                  warning(paste0("File does not exist. Did not download: \n",
                                 URL.temp, "\n\n"),
                          # call is printed later or within the "warnings()"
                          call. = TRUE, immediate. = FALSE)
                }
              }
              # }
            }
          }
          # Unzip the files of the Bulk-Download-Zip-File
          files.list <- list.files(temp.temp.save.location,
                                   pattern = ".zip",
                                   full.names = T)
          for (file in files.list) {
            unzip(file, exdir = str_sub(temp.temp.save.location,
                                        start = 0,
                                        end = str_length(temp.temp.save.location)-1)
            )
            if(keep.raw.zip == FALSE){
              unlink(file)
            }
          }
          # check if length of file list is matching the parameters max input
          # length
          # if(parm.temp != "elev"){
          if(parm.temp != "bio"){
            # sammlung der Monate aus den files in
            for (month in month.var) {
              if(parm.temp != "prec"){
                if(res.temp != "30s"){
                  raster.temp <- raster(
                    list.files(path = temp.temp.save.location,
                               pattern = paste0(parm.temp, month,".bil"),
                               full.names = T)
                  )
                }else{
                  raster.temp <- raster(
                    list.files(path = temp.temp.save.location,
                               pattern = paste0(parm.temp, "_", month,".bil"),
                               full.names = T)
                  )
                }
                gc()
                gain(raster.temp) <- 0.1
                gc()
              }else{
                if(res.temp != "30s"){
                  raster.temp <- raster(
                    list.files(path = temp.temp.save.location,
                               pattern = paste0(parm.temp, month,".bil"),
                               full.names = T)
                  )
                }else{
                  raster.temp <- raster(
                    list.files(path = temp.temp.save.location,
                               pattern = paste0(parm.temp, "_", month,".bil"),
                               full.names = T)
                  )
                }
              }
              writeRaster(raster.temp,
                          filename = paste0(temp.temp.save.location,
                                            "WC_",vers, "_",
                                            res, "_", parm.temp, "_",
                                            str_pad(c(1:12)[month], 2, 'left', pad = "0"),
                                            ".tif"),
                          format = "GTiff",
                          overwrite = TRUE)
            }
            unlink(c(list.files(temp.temp.save.location, pattern = ".bil",
                                full.names = TRUE),
                     list.files(temp.temp.save.location, pattern = ".hdr",
                                full.names = TRUE)))
          }else{
            for (bio in bio.var) {
              raster.temp <- raster(
                list.files(temp.temp.save.location,
                           pattern = paste0(parm.temp, bio, ".bil"),
                           full.names = T)
              )
              if(bio <= 11){
                if(res.temp != "30s"){
                  gc()
                  gain(raster.temp) <- 0.1
                  gc()
                }else{
                  # crop
                  gc()
                  gain(raster.temp) <- 0.1
                  gc()
                }
              }
              writeRaster(raster.temp,
                          filename = paste0(temp.temp.save.location,
                                            "WC_",vers, "_",
                                            res, "_", parm.temp, "_",
                                            str_pad(c(1:19)[bio], 2, 'left', pad = "0"),
                                            ".tif"),
                          format = "GTiff",
                          overwrite = TRUE)
            }
            unlink(c(list.files(temp.temp.save.location, pattern = ".bil",
                                full.names = TRUE),
                     list.files(temp.temp.save.location, pattern = ".hdr",
                                full.names = TRUE)))
          }
          # }else{
          #   cat("elev \n")
          # }
        }

        if(vers == "2.1"){
          res.temp <- switch (res,
                              "10min" = "10m",
                              "5min" = "5m",
                              "2.5min" = "2.5m",
                              "30s" = "30s",
                              "10m" = "10m",
                              "5m" = "5m",
                              "2.5m" = "2.5m",
                              "30s" = "30s",
                              next
          )
          dest.temp <- paste0(temp.temp.save.location, "WC_", vers, "_",
                              res, "_", parm.temp,"_Bulk.zip")
          # print(parm.temp)
          # print(res.temp)
          if(!file.exists(dest.temp)){
            # create a variable for the later requested Download-URL to avoid
            # requireing multiple changes, if the link changes.
            URL.temp <- paste0(URL.1, res.temp, "_", parm.temp, ".zip")
            urlCheck <- curl::curl_fetch_memory(url = URL.temp)$status_code
            # check if URL is available
            # if(url.exists(URL.temp)){
            if(urlCheck == 200){
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
                            quiet = FALSE)
            }else{
              # Error message if file is not available
              warning(paste0("File does not exist. Did not download: \n",
                             URL.temp, "\n\n"),
                      # call is printed later or within the "warnings()"
                      call. = TRUE, immediate. = FALSE)
            }
          }
          # Unzip the files of the Bulk-Download-Zip-File
          files.list <- list.files(temp.temp.save.location,
                                   pattern = ".zip",
                                   full.names = T)
          for (file in files.list) {
            unzip(file, exdir = str_sub(temp.temp.save.location,
                                        start = 0,
                                        end = str_length(temp.temp.save.location)-1)
            )
            if(keep.raw.zip == FALSE){
              unlink(file)
            }
          }
          # check if length of file list is matching the parameters max input
          # length
          if(parm.temp != "elev"){
            if(parm.temp != "bio"){
              # sammlung der Monate aus den files in
              keep.files <- c()
              month.var.char <- str_pad(month.var, 2, 'left', pad = "0")
              for (month in month.var.char) {
                keep.files <- c(keep.files,
                                list.files(temp.temp.save.location,
                                           pattern = paste0(parm.temp,"_",
                                                            month,
                                                            ".tif"),
                                           full.names = T)
                )
              }

              unlink(list.files(temp.temp.save.location,
                                pattern = ".tif",
                                full.names = TRUE)[
                                  !is.element(list.files(temp.temp.save.location,
                                                         pattern = ".tif",
                                                         full.names = TRUE),
                                              keep.files)
                                ]
              )
            }else{
              # sammlung der Bio aus den files in
              keep.files <- c()
              for (bio in bio.var) {
                keep.files <- c(keep.files,
                                list.files(temp.temp.save.location,
                                           pattern = paste0(parm.temp,"_",
                                                            bio,
                                                            ".tif"),
                                           full.names = T)
                )
              }
              unlink(list.files(temp.temp.save.location,
                                pattern = ".tif",
                                full.names = TRUE)[
                                  !is.element(list.files(temp.temp.save.location,
                                                         pattern = ".tif",
                                                         full.names = TRUE),
                                              keep.files)
                                ]
              )
            }
          }
        }
        # Hier weiter mit alle-betreffenden Bearbeitungen
        # print(list.files(temp.temp.save.location, pattern = ".tif", full.names = FALSE))
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
                       # they should be converted later on anyway.
                       convert.files.to.asc = convert.files.to.asc,
                       time.stamp.var = call.time)
        }
        # if converting.files.to.asc is TRUE ...
        if(convert.files.to.asc == TRUE){
          # the function "convert.to.asc" (found in the auxiliary.R-File)
          # is executed. The save.location is the same location as the
          # "current" save location. Also another new subdirectory will
          # be created with the name "ASCII" .
          convert.to.asc(temp.temp.save.location,
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
                                     parameter.var = parm,
                                     variable.numbers = variable.numbers,
                                     stack.clipped = TRUE,
                                     time.stamp.var = call.time)
          }else{
            stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                     parameter.var = parm,
                                     variable.numbers = variable.numbers,
                                     time.stamp.var = call.time)
          }
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
        # if no subdirectories and files are to be found in the 2nd order
        # subdirectory the directory will be deleted for a better overview of
        # given data.
        if(length(list.files(temp.temp.save.location,
                             include.dirs = TRUE)) == 0){

          unlink(str_sub(temp.temp.save.location,
                         start = 0,
                         end = str_length(temp.temp.save.location)-1),
                 force = TRUE, recursive = TRUE)
        }
        # Saving BIB File ---------------------------------------------------------
        # if save.bib.file is TRUE, the save.citation function will be called.
        # it saves a .bib-file of the downloaded data-set on the highest level of
        # the save.location. If the user's working directory is the desktop
        # and no other save.location is specified initially, on the desktop.
        data.set.name.temp <- switch(vers,
                                     # "1.4" = "WorldClim1.4",
                                     "2.1" = "WorldClim2.1",
                                     warning())
        if(save.bib.file == TRUE) {
          save.citation(save.location = save.location, dataSetName = data.set.name.temp)
        }
      }
    }
  }
 }


#'@title Function for downloading the WorldClim v1.4 CMIP5 future climate dataset
#'@author Helge Jentsch
#'@description This function supports the download, pre-processing and management of the WorldClim v1.4 CMIP5 future climate dataset.\cr This comprises of monthly precipitation sums (mm), temperature (maximum, minimum; °C), and annual chracteristics (19 bioclimatic variables).\cr To allow pre-processing, clipping and buffering, conversion to ASCII-grids and stacking options are included.\cr Optional an output of a .bib-file of the cited literature can be retrieved.\cr For user convenience, saving directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'@note Please note that this dataset is regarded to as "outdated" by the WorldClim creators. The download of the current dataset "CMIP6" is also provided by this package with the \code{\link{WorldClim.CMIP_6.download}} function.
#'@note Please note also that the downloaded data for temperature and the therefore also the first eleven bioclim-variables are processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time depending on the spatial resolution.
#'
#'@param save.location string. Input where the datasets should be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "tmax", "tmin", "bio")}
#'@param bio.var integer (vector). Input which monthly data should be downloaded. Only applicable to BIOCLIM variables. For further information see: \url{https://www.worldclim.org/data/bioclim.html}. \cr Default: \code{c(1:19)}
#'@param month.var integer (vector). Input which monthly data should be downloaded. Only applicable to precipitation and temperature (maximum, minimum). \cr Default: \code{c(1:12)}
#'@param resolution string (vector). Ranging from a 10 arc-minute resolution over 5 and 2.5 arc-minute to 30 arc-second resolution.\cr Default: \code{c("10m", "5m", "2.5m", "30s")}
#'@param model.var string (vector). Model used to calculate the dataset.\cr Default: \code{c("ACCESS1-0", "BCC-CSM1-1", "CCSM4", "CESM1-CAM5-1-FV2", "CNRM-CM5", }\cr \code{"GFDL-CM3", "GFDL-ESM2G", "GISS-E2-R", "HadGEM2-AO", "HadGEM2-CC", "HadGEM2-ES",} \cr \code{"INMCM4", "IPSL-CM5A-LR", "MIROC-ESM-CHEM", "MIROC-ESM", "MIROC5",} \cr \code{"MPI-ESM-LR","MRI-CGCM3", "NorESM1-M")}
#'@param emission.scenario.var string (vector). Input which emission scenario dataset should be downloaded. Provided are the representative concentration pathways (RCP) 2.6, 4.5, 6.0, and 8.5. \cr Default: \code{c("rcp26", "rcp45", "rcp60", "rcp85")}
#'@param time.interval.var string (vector). Time interval for which the dataset is calculated. The given intervals are 2040-2060 (represented by "2050"), and 2060 to 2080 (represented by "2070"). \cr Default: \code{c("2050", "2070")}
#'@param clipping logical. Input whether the downloaded data should be clipped. \cr If \code{FALSE}; clip.shapefile, buffer, clip.extent will be ignored. \cr Default: \code{FALSE}
#'@param clip.shapefile string. Input which shapefile should be used for clipping. \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether files should be converted into the ASCII format.\cr If \code{TRUE} a new subdirectory is created and the rawdata is saved there.\cr If \code{clipping} is \code{TRUE} the clipped raster files are also saved as ASCII grids.\cr Default: \code{FALSE}
#'@param stacking.data logical. Input whether the downloaded data should be stacked as a netCDF-rasterstack.\cr Default: \code{FALSE}
#'@param keep.raw.zip logical. Should the downloaded raw-data be "zipped".\cr Default: \code{FALSE}
#'@param delete.raw.data  logical. Should the downloaded raw-data be deleted.\cr If the \code{combine.raw.zip}-option is \code{TRUE}, raw-data is still available in the zipped file.\cr Default: \code{FALSE}
#'@param save.bib.file logical. Whether a BibTex-citation file of the dataset should be provided in the Working directory.\cr Default: \code{TRUE}
#'
#'@return WorldClim 1.4 CMIP5 Future climate datasets for the periods of 2041-2060 and/or 2061-2080.
#'
#'@references R. J. Hijmans, S. E. Cameron, J. L. Parra, et al. "Very high resolution interpolated climate surfaces for global land areas". In: _International Journal of Climatology_ 25.15 (2005), pp. 1965-1978. DOI: 10.1002/joc.1276. <URL: https://doi.org/10.1002/joc.1276>.
#'
#'@examples
#' \dontrun{
#' # Bioclim
#' WorldClim.CMIP_5.download(parameter = "bio",
#'                            bio.var = c(1,12),
#'                            resolution = "10min",
#'                            model.var = "MPI-ESM-LR",
#'                            emission.scenario.var = "rcp26",
#'                            time.interval.var = "2050")
#' # Precipitation
#' WorldClim.CMIP_5.download(parameter = "prec",
#'                            month.var = c(1,12),
#'                            resolution = "10min",
#'                            model.var = "MPI-ESM-LR",
#'                            emission.scenario.var = "rcp26",
#'                            time.interval.var = "2050")
#' }
#'
#'@import stringr
#'@importFrom curl curl_fetch_memory
#'@import RCurl
#'@import ncdf4
#'@import raster
#'@importFrom utils unzip download.file  setTxtProgressBar txtProgressBar
#'
#'@export
WorldClim.CMIP_5.download <- function(save.location = "./",
                                      parameter = c("prec", "tmax",
                                                    "tmin", "bio"),
                                      bio.var = c(1:19),
                                      month.var = c(1:12),
                                      resolution = c("10min", "5min", "2.5min", "30s"),
                                      model.var = c("ACCESS1-0", "BCC-CSM1-1",
                                                    "CCSM4", "CESM1-CAM5-1-FV2",
                                                    "CNRM-CM5", "GFDL-CM3",
                                                    "GFDL-ESM2G", "GISS-E2-R",
                                                    "HadGEM2-AO", "HadGEM2-CC",
                                                    "HadGEM2-ES", "INMCM4",
                                                    "IPSL-CM5A-LR",
                                                    "MIROC-ESM-CHEM", "MIROC-ESM",
                                                    "MIROC5", "MPI-ESM-LR",
                                                    "MRI-CGCM3", "NorESM1-M"),
                                      emission.scenario.var = c("rcp26", "rcp45",
                                                                "rcp60", "rcp85"),
                                      time.interval.var = c("2050", "2070"),
                                      clipping = FALSE,
                                      clip.shapefile = NULL,
                                      clip.extent = c(-180, 180, -90, 90),
                                      buffer = 0,
                                      convert.files.to.asc = FALSE,
                                      stacking.data = FALSE,
                                      keep.raw.zip = FALSE,
                                      delete.raw.data  = FALSE,
                                      save.bib.file = TRUE){
  # requireNamespace("stringr")
  # requireNamespace("RCurl")
  # requireNamespace("ncdf4")
  # requireNamespace("raster")
  gc()
  call.time <- stringr::str_replace_all(stringr::str_replace_all(
    paste0(Sys.time()), 
    pattern = ":",
    replacement = "-"), 
    pattern = " ", 
    replacement = "_")  
  # initial check -----------------------------------------------------------
  # normalize Path for easier application later
  save.location <- normalizePath(save.location, winslash = "/")
  # Check which parameters are put in and if the connected
  # month/bio-variables are correctly input
  if(is.element("prec", parameter)|is.element("tmax", parameter)|
     is.element("tmin", parameter)
  ){
    # if month.var is just a single numeric input it is here casted into
    # a vector for comparabilities
    month.var <- c(month.var)
    # # if there is not a numeric input -> prompt error
    # if(!is.numeric(month.var)) stop()
    # # Padding of "one-digit" months with a 0
    # month.var <- str_pad(month.var, 2, 'left', pad = "0")
  }

  # analog to the if-clause before - here the parameter bio.var is checked.
  if(is.element("bio", parameter)){
    # bio.var <- c(bio.var)
    # if(!is.numeric(bio.var)) stop()
    # bio.var <- str_pad(bio.var, 2, 'left', pad = "0")
  }

  # analog the elevation can be regarded.


  # First: Set URL Root --------------------------------------------------------

  URL.1 <- "http://biogeo.ucdavis.edu/data/climate/cmip5/"

  # Second: Parameters ---------------------------------------------------------
  for(parm in parameter){ #parm <- parameter[1]
    # if not already created, create new directory
    if (!dir.exists(paste0(save.location, "/", parm))){
      dir.create(paste0(save.location, "/", parm))
    }
    # set the 1. order temporal save location to this directory
    # 1. Order -> parameter!
    temp.save.location <- paste0(save.location, "/", parm, "/")

    parm.temp <- switch (parm,
                         "tmin" = "tn",
                         "tmax" = "tx",
                         "prec" = "pr",
                         "bio" = "bi",
                         next)
    variable.numbers <- switch(parm,
                               "bio" = bio.var,
                               "tmin" = month.var,
                               "tmax" = month.var,
                               "prec" = month.var,
                               stop())
    # Third: Through resolution ------------------------------------------------
    for (res in resolution) { # res <- resolution[4]
      res.temp <- switch (res,
                          "10min" = "10m",
                          "5min" = "5m",
                          "2.5min" = "2_5m",
                          "30s" = "30s",
                          "10m" = "10m",
                          "5m" = "5m",
                          "2.5m" = "2_5m",
                          "30s" = "30s",
                          next
      )
      if(res.temp == "30s") {
        warning(
          "Processing might take a while since 30s resolution is downloaded for the whole world",
          call. = TRUE,
          immediate. = TRUE)
      }
      URL.2 <- paste0(res.temp, "/")
      # Forth: Global Circulation Model --------------------------------------
      for(gcm in model.var){ # gcm = model.var[1]
        # GCM;OnlyNonCommercialUse
        # AC;TRUE
        # BC;FALSE
        # CC;FALSE
        # CE;FALSE
        # CN;TRUE
        # GF;FALSE
        # GD;FALSE
        # GS;FALSE
        # HD;FALSE
        # HG;FALSE
        # HE;FALSE
        # IN;FALSE
        # IP;FALSE
        # MI;TRUE
        # MR;TRUE
        # MC;TRUE
        # MP;FALSE
        # MG;FALSE
        # NO;FALSE
        gcm.temp <- switch(gcm,
                           "ACCESS1-0" = "AC",
                           "BCC-CSM1-1" = "BC",
                           "CCSM4" = "CC",
                           "CESM1-CAM5-1-FV2" = "CE",
                           "CNRM-CM5" = "CN",
                           "GFDL-CM3" = "GF",
                           "GFDL-ESM2G" = "GD",
                           "GISS-E2-R" = "GS",
                           "HadGEM2-AO" = "HD",
                           "HadGEM2-CC" = "HG",
                           "HadGEM2-ES" = "HE",
                           "INMCM4" = "IN",
                           "IPSL-CM5A-LR" = "IP",
                           "MIROC-ESM-CHEM" = "MI",
                           "MIROC-ESM" = "MR",
                           "MIROC5" = "MC",
                           "MPI-ESM-LR" = "MP",
                           "MRI-CGCM3" = "MG",
                           "NorESM1-M" = "NO",
                           next
        )
        if(gcm == "ACCESS1-0" | gcm == "CNRM-CM5" |
           gcm == "MIROC-ESM-CHEM" | gcm == "MIROC-ESM" |
           gcm == "MIROC5"){
          warning(paste0("Please note that the GCM '",gcm,
                         "' is for non commercial use only"),
                  call. = TRUE,
                  immediate. = TRUE)
        }
        URL.3 <- stringr::str_to_lower(gcm.temp)

        # Fifth: Relative Concentration Pathways ---------------------------------
        for(rcp in emission.scenario.var){ # rcp = "rcp45"
          rcp.temp <- switch (rcp,
                              "rcp26" = "26",
                              "rcp45" = "45",
                              "rcp60" = "60",
                              "rcp85" = "85",
                              next
          )
          URL.4 <- paste0(URL.3, rcp.temp)
          # Sixth: Time interval -----------------------------------------------
          for(year in time.interval.var) { #year <- time.interval.var[1]
            year.temp <- switch (year,
                                 "2050" = "50",
                                 "2070" = "70",
                                 next
            )
            temp.temp.save.location <- paste0(temp.save.location,"WorldClim_CMIP5_",
                                              parm.temp, "_", res,"_",
                                              gcm, "_", rcp, "_",year,"/")
            # if not already created, create new directory
            if(!dir.exists(temp.temp.save.location)){
              dir.create(temp.temp.save.location)
            }
            # normalize the path to make it work more easily
            # temp.temp.save.location <- normalizePath(temp.temp.save.location,
            #                                          winslash = "/")

            dest.temp <- paste0(temp.temp.save.location, "WC_", res, "_",
                                gcm, "_", rcp, "_", year.temp, "_Bulk.zip")
            if(!file.exists(dest.temp)){
              # create a variable for the later requested Download-URL to avoid
              # requireing multiple changes, if the link changes.
              URL.temp <- paste0(URL.1, URL.2, URL.4, parm.temp, year.temp, ".zip")
              # check if URL is available
              if(RCurl::url.exists(url = URL.temp)){
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
                              quiet = FALSE)
              }else{
                # Error message if file is not available
                warning(paste0("File does not exist. Did not download: \n",
                               URL.temp, "\n\n"),
                        # call is printed later or within the "warnings()"
                        call. = TRUE, immediate. = FALSE)
                next()
              }
            }
            # Unzip the files of the Bulk-Download-Zip-File
            files.list <- list.files(temp.temp.save.location,
                                     pattern = ".zip",
                                     full.names = T)
            for (file in files.list) {
              unzip(file, exdir = str_sub(temp.temp.save.location,
                                          start = 0,
                                          end = str_length(temp.temp.save.location)-1)
              )
              if(keep.raw.zip == FALSE){
                unlink(file)
              }
            }
            # check if length of file list is matching the parameters max input
            # length
            if(parm != "bio"){
              # sammlung der Monate aus den files in
              keep.files <- c()
              # month.var.char <- str_pad(month.var, 2, 'left', pad = "0")
              for (month in month.var) {
                keep.files <- c(keep.files,
                                list.files(temp.temp.save.location,
                                           pattern = paste0(URL.4, parm.temp,
                                                            year.temp, month,
                                                            ".tif"),
                                           full.names = T)
                )
              }

              unlink(list.files(temp.temp.save.location,
                                pattern = ".tif",
                                full.names = TRUE)[
                                  !is.element(list.files(temp.temp.save.location,
                                                         pattern = ".tif",
                                                         full.names = TRUE),
                                              keep.files)
                                ]
              )
              if(parm.temp != "prec"){
                for (file.conversion.num in 1:length(keep.files)) {
                  if(res.temp != "30s"){
                    raster.temp <- raster(keep.files[file.conversion.num])
                    gc()
                    gain(raster.temp) <- .1
                    writeRaster(x = raster.temp,
                                filename = keep.files[file.conversion.num],
                                overwrite = TRUE)
                    rm(raster.temp)
                    gc()
                  }else{
                    raster.temp <- raster(keep.files[file.conversion.num])
                    # crop
                    gc()
                    gain(raster.temp) <- .1
                    gc()
                    # save
                    writeRaster(x = raster.temp,
                                filename = keep.files[file.conversion.num],
                                overwrite = TRUE)
                    rm(raster.temp)
                  }
                  gc()
                }
              }
            }else{
              # sammlung der Bio aus den files in
              keep.files <- c()
              for (bio in bio.var) {
                keep.files <- c(keep.files,
                                list.files(temp.temp.save.location,
                                           pattern = paste0(URL.4, parm.temp,
                                                            year.temp, bio,
                                                            ".tif"),
                                           full.names = T)
                )
              }
              unlink(list.files(temp.temp.save.location,
                                pattern = ".tif",
                                full.names = TRUE)[
                                  !is.element(list.files(temp.temp.save.location,
                                                         pattern = ".tif",
                                                         full.names = TRUE),
                                              keep.files)
                                ]
              )
              print(keep.files)
              for (file.conversion.num in 1:length(keep.files)) {
                file.conversion.temp <- keep.files[file.conversion.num]
                file.conversion.temp <- unlist(
                  str_split(file.conversion.temp, pattern = "/")
                )[
                  length(unlist(str_split(file.conversion.temp, pattern = "/")))
                ]
                file.conversion.temp <- str_remove(file.conversion.temp,
                                                   pattern = paste0(
                                                     URL.4,
                                                     parm.temp,
                                                     year.temp
                                                   )
                )
                file.conversion.temp <- str_remove(file.conversion.temp,
                                                   pattern = ".tif")
                file.conversion.temp <- as.numeric(file.conversion.temp)
                print(file.conversion.temp)
                if(file.conversion.temp <= 11){
                  if(res.temp != "30s"){
                    gc()
                    raster.temp <- raster(keep.files[file.conversion.num])
                    gain(raster.temp) <- .1
                    writeRaster(x = raster.temp,
                                filename = keep.files[file.conversion.num],
                                overwrite = TRUE)
                    rm(raster.temp)
                    gc()
                  }else{
                    raster.temp <- raster(keep.files[file.conversion.num])
                    # crop
                    gc()
                    gain(raster.temp) <- .1
                    gc()
                    # save
                    writeRaster(x = raster.temp,
                                filename = keep.files[file.conversion.num],
                                overwrite = TRUE)
                    rm(raster.temp)
                  }
                  gc()
                }
              }
            }
            # Hier weiter mit alle-betreffenden Bearbeitungen
            # print(list.files(temp.temp.save.location, pattern = ".tif", full.names = FALSE))
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
              convert.to.asc(temp.temp.save.location,
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
                                         parameter.var = parm.temp,
                                         variable.numbers = variable.numbers,
                                         stack.clipped = TRUE,
                                         time.stamp.var = call.time)
              }else{
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                             parameter.var = parm.temp,
                                             variable.numbers = variable.numbers,
                                             time.stamp.var = call.time)
                }
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
            # if no subdirectories and files are to be found in the 2nd order
            # subdirectory the directory will be deleted for a better overview of
            # given data.
            if(length(list.files(temp.temp.save.location,
                                 include.dirs = TRUE)) == 0){

              unlink(str_sub(temp.temp.save.location,
                             start = 0,
                             end = str_length(temp.temp.save.location)-1),
                     force = TRUE, recursive = TRUE)
            }
            # Saving BIB File ---------------------------------------------------------
            # if save.bib.file is TRUE, the save.citation function will be called.
            # it saves a .bib-file of the downloaded data-set on the highest level of
            # the save.location. If the user's working directory is the desktop
            # and no other save.location is specified initially, on the desktop.
            # if(save.bib.file == TRUE) {
            #   save.citation(save.location = save.location, dataSetName = "WorldClim1.4")
            # }
          }
        }
      }
    }
  }
}


#'@title Function for downloading the WorldClim v2.1 CMIP6 future climate dataset
#'@author Helge Jentsch
#'@description This function supports the download, pre-processing and management of the WorldClim v2.1 CMIP6 future climate dataset.\cr This comprises of monthly precipitation sums (mm), temperature (maximum, minimum; °C), and annual chracteristics (19 bioclimatic variables).\cr To allow pre-processing, clipping and buffering, conversion to ASCII-grids and stacking options are included.\cr Optional an output of a .bib-file of the cited literature can be retrieved.\cr For user convenience, saving directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'@note Please note that the downloaded data for temperature and the therefore also the first eleven bioclim-variables are processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time depending on the spatial resolution. \cr **The 30 arc-second resolution is not yet available. It was scheduled to be released by March 2020 but until March 2021 no data was released publicly.**
#'
#'@param save.location string. Input where the datasets should be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "tmax", "tmin", "bio")}
#'@param bio.var integer (vector). Input which monthly data should be downloaded. Only applicable to BIOCLIM variables. For further information see: \url{https://www.worldclim.org/data/bioclim.html}. \cr Default: \code{c(1:19)}
#'@param month.var integer (vector). Input which monthly data should be downloaded. Only applicable to precipitation and temperature (maximum, minimum). \cr Default: \code{c(1:12)}
#'@param resolution string (vector). Ranging from a 10 arc-minute resolution over 5 and 2.5 arc-minute to 30 arc-second resolution.\cr Default: \code{c("10m", "5m", "2.5m", "30s")}
#'@param model.var string (vector). Model used to calculate the dataset.\cr Default: \code{c("BCC-CSM2-MR", "CNRM-CM6-1", "CNRM-ESM2-1", "CanESM5", "GFDL-ESM4", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MRI-ESM2-0")}
#'@param emission.scenario.var string (vector). Input which emission scenario dataset should be downloaded. Provided are the shared socio-economic pathways (SSPs) 126, 245, 370, and 585.\cr Further information on SSPs: \url{https://www.carbonbrief.org/cmip6-the-next-generation-of-climate-models-explained} and \url{https://www.carbonbrief.org/explainer-how-shared-socioeconomic-pathways-explore-future-climate-change}.\cr Default: \code{c("ssp126", "ssp245", "ssp370", "ssp585")}
#'@param time.interval.var string (vector). Averaged monthly values over a 20 year period. Periodes availabe are: 2021-2040, 2041-2060, 2061-2080, 2081-2100. \cr Default: \code{c("2021-2040", "2041-2060", "2061-2080", "2081-2100")}
#'@param clipping logical. Input whether the downloaded data should be clipped.\cr If \code{FALSE}; clip.shapefile, buffer, clip.extent will be ignored. \cr Default: \code{FALSE}
#'@param clip.shapefile string. Input which shapefile should be used for clipping. \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether files should be converted into the ASCII format.\cr If \code{TRUE} a new subdirectory is created and the rawdata is saved there. If "clipping" is \code{TRUE} the clipped raster files are also saved as ASCII grids. \cr  Default: \code{FALSE}
#'@param stacking.data logical. Input whether the downloaded data should be stacked as a netCDF-rasterstack. \cr Default: \code{FALSE}
#'@param keep.raw.zip logical. Should the downloaded raw-data be "zipped". \cr Default: \code{FALSE}
#'@param delete.raw.data  logical. Should the downloaded raw-data be deleted. If the "combine.raw.zip"-option is \code{TRUE}, raw-data is still available in the zipped file. \cr Default: \code{FALSE}
#'@param save.bib.file logical. Whether a BibTex-citation file of the dataset should be provided in the Working directory. \cr Default: \code{TRUE}
#'
#'@return WorldClim 2.1 CMIP6 Future climate datasets for the periods of 2021-2040, 2041-2060, 2061-2080 and/or 2081-2100.
#'
#'@references S. E. Fick and R. J. Hijmans. "WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas". In: _International Journal of Climatology_ 37.12 (Okt. 2017), pp. 4302-4315. DOI: 10.1002/joc.5086. <URL:https://doi.org/10.1002/joc.5086>.
#'
#'@examples
#' \dontrun{
#' # Bioclim
#' WorldClim.CMIP_6.download(parameter = "bio",
#'                            bio.var = c(1,12),
#'                            resolution = "10min",
#'                            model.var = "MIROC6",
#'                            emission.scenario.var = "ssp126",
#'                            time.interval.var = "2021-2040")
#' # Precipitation
#' WorldClim.CMIP_6.download(parameter = "prec",
#'                            month.var = c(1,12),
#'                            resolution = "10min",
#'                            model.var = "MIROC6",
#'                            emission.scenario.var = "ssp126",
#'                            time.interval.var = "2021-2040")
#' }
#'
#'@import stringr
#'@import RCurl
#'@import ncdf4
#'@import raster
#'@importFrom utils download.file unzip setTxtProgressBar txtProgressBar
#'@importFrom curl curl_fetch_memory


#'@export
WorldClim.CMIP_6.download <- function(save.location = "./",
                                      parameter = c("prec", "tmax",
                                                    "tmin", "bio"),
                                      bio.var = c(1:19),
                                      month.var = c(1:12),
                                      resolution = c("10min", "5min", "2.5min", "30s"),
                                      model.var = c("BCC-CSM2-MR", "CNRM-CM6-1",
                                                    "CNRM-ESM2-1", "CanESM5",
                                                    "GFDL-ESM4", "IPSL-CM6A-LR",
                                                    "MIROC-ES2L", "MIROC6",
                                                    "MRI-ESM2-0"),
                                      emission.scenario.var = c("ssp126",
                                                                "ssp245",
                                                                "ssp370",
                                                                "ssp585"),
                                      time.interval.var = c("2021-2040",
                                                            "2041-2060",
                                                            "2061-2080",
                                                            "2081-2100"),
                                      clipping = FALSE,
                                      clip.shapefile = NULL,
                                      clip.extent = c(-180, 180, -90, 90),
                                      buffer = 0,
                                      convert.files.to.asc = FALSE,
                                      stacking.data = FALSE,
                                      keep.raw.zip = FALSE,
                                      delete.raw.data  = FALSE,
                                      save.bib.file = TRUE){
  # requireNamespace("stringr")
  # requireNamespace("RCurl")
  # requireNamespace("ncdf4")
  # requireNamespace("raster")
  gc()
  call.time <- stringr::str_replace_all(stringr::str_replace_all(as.character(Sys.time()), pattern = ":", replacement = "-"), pattern = " ", replacement = "_")
  # initial check -----------------------------------------------------------
  # normalize Path for easier application later
  save.location <- normalizePath(save.location, winslash = "/")
  # Check which parameters are put in and if the connected
  # month/bio-variables are correctly input
  if(is.element("prec", parameter)|is.element("tmax", parameter)|
     is.element("tmin", parameter)
  ){
    # if month.var is just a single numeric input it is here casted into
    # a vector for comparabilities
    month.var <- c(month.var)
    # # if there is not a numeric input -> prompt error
    # if(!is.numeric(month.var)) stop()
    # # Padding of "one-digit" months with a 0
    # month.var <- str_pad(month.var, 2, 'left', pad = "0")
  }

  # analog to the if-clause before - here the parameter bio.var is checked.
  if(is.element("bio", parameter)){
    # bio.var <- c(bio.var)
    # if(!is.numeric(bio.var)) stop()
    # bio.var <- str_pad(bio.var, 2, 'left', pad = "0")
  }
  if(is.element("30s", resolution)){
    warning("Data at 30-seconds spatial resolution is expected to be available soon.", call. = TRUE, immediate. = TRUE)
  }
  # analog the elevation can be regarded.


  # First: Set URL Root --------------------------------------------------------

  URL.1 <- "http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/"

  # Second: Parameters ---------------------------------------------------------
  for(parm in parameter){
    # if not already created, create new directory
    if (!dir.exists(paste0(save.location, "/", parm))){
      dir.create(paste0(save.location, "/", parm))
    }
    # set the 1. order temporal save location to this directory
    # 1. Order -> parameter!
    temp.save.location <- paste0(save.location, "/", parm, "/")

    parm.temp <- switch (parm,
                         "tmin" = "tmin",
                         "tmax" = "tmax",
                         "prec" = "prec",
                         "bio" = "bioc",
                         next)
    variable.numbers <- switch(parm,
                               "bio" = bio.var,
                               "tmin" = month.var,
                               "tmax" = month.var,
                               "prec" = month.var,
                               stop())
    # Third: Through resolution ------------------------------------------------
    for (res in resolution) {
      res.temp <- switch (res,
                          "10min" = "10m",
                          "5mmin" = "5m",
                          "2.5min" = "2.5m",
                          "30s" = "30s",
                          "10m" = "10m",
                          "5m" = "5m",
                          "2.5m" = "2.5m",
                          "30s" = "30s",
                          next
      )
      # if(res.temp == "30s") {
      #   warning(
      #     # "Processing might take a while since 30s resolution is downloaded for the whole world",
      #     "30 second data is not yet available. Please check WorldClim website. Skipping 30s data!",
      #     call. = TRUE,
      #     immediate. = TRUE)
      #   next
      # }
      URL.2 <- paste0(res.temp, "/")
      # Forth: Global Circulation Model --------------------------------------
      for(gcm in model.var){
        # Fifth: Relative Concentration Pathways ---------------------------------
        for(rcp.temp in emission.scenario.var){
          # Sixth: Time interval -----------------------------------------------
          for(year.temp in time.interval.var) {

            temp.temp.save.location <- paste0(temp.save.location,"WorldClim_CMIP6_",
                                              parm.temp, "_", res ,"_",
                                              gcm, "_", rcp.temp, "_",year.temp,"/")
            # if not already created, create new directory
            if(!dir.exists(temp.temp.save.location)){
              dir.create(temp.temp.save.location)
            }
            # normalize the path to make it work more easily
            # temp.temp.save.location <- normalizePath(temp.temp.save.location,
            #                                          winslash = "/")
            dest.temp <- paste0(temp.temp.save.location, "WC_", res, "_",
                                gcm, "_", rcp.temp, "_", year.temp, "_Bulk.zip")

            URL.3 <- paste0("wc2.1_",res.temp,"_",parm.temp, "_", gcm, "_",
                            rcp.temp, "_", year.temp)
            print(URL.3)
            if(!file.exists(dest.temp)){
              # create a variable for the later requested Download-URL to avoid
              # requireing multiple changes, if the link changes.
              URL.temp <- paste0(URL.1, URL.2,URL.3, ".zip")
              # check if URL is available
              # if(url.exists(URL.temp)){
              if(RCurl::url.exists(url = URL.temp)){
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
                              quiet = FALSE)
              }else{
                # Error message if file is not available
                warning(paste0("File does not exist. Did not download: \n",
                               URL.temp, "\n\n"),
                        # call is printed later or within the "warnings()"
                        call. = TRUE, immediate. = FALSE)
                next()
              }
            }
            # Unzip the files of the Bulk-Download-Zip-File
            files.list <- list.files(temp.temp.save.location,
                                     pattern = ".zip",
                                     full.names = T)
            for (file in files.list) {
              unzip(file,
                    exdir = stringr::str_sub(temp.temp.save.location,
                                    start = 0,
                                    end = stringr::str_length(temp.temp.save.location)-1)
              )
              file.copy(from = paste0(list.files(temp.temp.save.location,
                                                 pattern = paste0(URL.3, ".tif"),
                                                 recursive = TRUE,
                                                 full.names = TRUE))[1],
                        to = paste0(temp.temp.save.location,
                                    URL.3,"_stacked_RAW.tif"),
                        overwrite = TRUE)
              if(dir.exists(paste0(temp.temp.save.location,"//share"))){
                unlink(paste0(temp.temp.save.location,"//share"),
                       recursive = TRUE)

              }
              tif.list <- list.files(temp.temp.save.location,
                                     pattern = "stacked_RAW.tif",
                                     recursive = TRUE,
                                     full.names = TRUE)
              print(tif.list)
              if (!is.null(tif.list)){
                if(length(tif.list)==1){
                  raster.temp <- raster::unstack(raster::stack(tif.list))
                  if(parm != "bio"){
                    for(layer.month in month.var){
                      writeRaster(x = raster.temp[[layer.month]],
                                  filename = paste0(temp.temp.save.location,
                                                    URL.3,"_",layer.month,".tif"),
                                  overwrite = TRUE
                      )
                    }
                  }else{
                    for(layer.bio in bio.var){
                      writeRaster(x = raster.temp[[layer.bio]],
                                  filename = paste0(temp.temp.save.location,
                                                    URL.3,"_",layer.bio,".tif"),
                                  overwrite = TRUE
                      )
                    }
                  }
                }
                unlink(tif.list)
              }
              if(keep.raw.zip == FALSE){
                print(file)
                unlink(file)
              }
            }

            # print(ls())

            # Hier weiter mit alle-betreffenden Bearbeitungen
            # print(list.files(temp.temp.save.location, pattern = ".tif", full.names = FALSE))
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
              convert.to.asc(temp.temp.save.location,
                             time.stamp.var = call.time)
            }
            # if stacking.data is TRUE ...
            if(stacking.data == TRUE){

              # if(delete.raw.data == FALSE){
              # orig.stack <- list.files(temp.temp.save.location,
              #                          pattern = "RAW.tif",
              #                          include.dirs = FALSE,
              #                          full.names = T)
              # orig.stack.raster <- raster::stack(orig.stack)
              # writeRaster(orig.stack.raster,
              #             paste0(temp.temp.save.location,
              #                    URL.3,"_stacked_raw.nc"),
              #             format = "CDF",
              #             overwrite = TRUE)
              # unlink(orig.stack);rm(orig.stack.raster)
              # }

              # the function "stacking.downloaded.data"
              # (found in the auxiliary.R-File) is executed.
              # The save.location is the same location as the
              # "current" save location.

              if(clipping==TRUE){
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = parm.temp,
                                         variable.numbers = variable.numbers,
                                         stack.clipped = TRUE,
                                         time.stamp.var = call.time)
              }else{
                stacking.downloaded.data(stack.save.location = temp.temp.save.location,
                                         parameter.var = parm.temp,
                                         variable.numbers = variable.numbers,
                                         time.stamp.var = call.time)
              }
            }
            # if delete.raw.data is TRUE ...
            if(delete.raw.data == TRUE){
              # All tif files in the current 2nd order subdirectory are
              # unlinked (deleted).
              unlink(list.files(temp.temp.save.location,
                                pattern = ".tif",
                                include.dirs = FALSE,
                                full.names = T),
                     force = TRUE)
            }
            # if no subdirectories and files are to be found in the 2nd order
            # subdirectory the directory will be deleted for a better overview of
            # given data.
            if(length(list.files(temp.temp.save.location,
                                 include.dirs = TRUE)) == 0){

              unlink(stringr::str_sub(temp.temp.save.location,
                             start = 0,
                             end = stringr::str_length(temp.temp.save.location)-1),
                     force = TRUE, recursive = TRUE)
            }
            # Saving BIB File ---------------------------------------------------------
            # if save.bib.file is TRUE, the save.citation function will be called.
            # it saves a .bib-file of the downloaded data-set on the highest level of
            # the save.location. If the user's working directory is the desktop
            # and no other save.location is specified initially, on the desktop.
            if(save.bib.file == TRUE) {
              save.citation(save.location = save.location, dataSetName = "WorldClim2.1")
            }
          }
        }
      }
    }
  }
}
