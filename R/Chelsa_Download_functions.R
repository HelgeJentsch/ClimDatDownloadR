#'@title Function for downloading CHELSA CMIP 5 future climatologies for the years 2041-2060 and 2061-2080
#'@author Helge Jentsch
#'@description This function supports the download of CHELSA CMIP5 future climate scenarios comprising of monthly precipitation sums in mm, monthly temperature (average, minimum, maximum) in degrees Celsius, and annual chracteristics (19 bioclimatic variables).\cr To allow pre-processing, clipping and buffering, conversion to ASCII-grids and stacking options are included.\cr Optional an output of a .bib-file of the cited literature can be retrieved.\cr For user convenience, saving directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'@details "The downscaled data has been produced using climatological aided interpolation based on the 1979-2013 reference climatologies from CHELSA." (CHELSA Climate 2020: \url{http://chelsa-climate.org/future/})
#'
#'@note Please note that the downloaded data for temperature and the therefore also the first eleven bioclim-variables are processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time.\cr For some of the datasets not all models and rcps are available. For the ones that are not supported the data will not be downloaded and a warning will be prompted. See parameter \code{model.var} for more information or check the website of CHELSA Climate (\url{http://chelsa-climate.org/future/}). Please note, that the downloaded data for temperature and the therefore also the first eleven bioclim-variables are processed to °C without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time.
#'
#'@param save.location string. Input where the datasets should be saved. \cr Default: Working Directory.
#'@param parameter string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "temp", "tmax", "tmin", "bio")}
#'@param bio.var integer (vector). Input which monthly data should be downloaded. Only applicable to BIOCLIM variables. For further information see: \url{http://chelsa-climate.org/bioclim/}. \cr Default: \code{c(1:19)}
#'@param month.var integer (vector). Input which monthly data should be downloaded. Only applicable to Precipitation and Temperature (average, maximum, minimum). \cr Default: \code{c(1:12)}
#'@param emission.scenario.var string (vector). Input which emission scenario dataset should be downloaded. Provided are the representative concentration pathways (RCP) 2.6, 4.5, 6.0, and 8.5.\cr Default: \code{c("rcp26", "rcp45", "rcp60", "rcp85")}
#'@param time.interval.var string (vector). Input for which time interval data should be downloaded. CHELSA provides downscaled CMIP5 climatologies for 2050 and 2070. Multiple inputs possible.\cr Default: \code{c("2041-2060", "2061-2080")}
#'@param model.var string (vector). Input which future model dataset should be downloaded. For more information see: \url{http://chelsa-climate.org/future/}.\cr For some of the datasets not all downloads are available. For the ones that are not supported the data will not be downloaded and a warning will be prompted. For an overview please try "warnings()" after execution. \cr Default: \code{c("ACCESS1-0", "bcc-csm1-1", "BNU-ESM", "CanESM2", "CCSM4", "CESM1-BGC", } \cr \code{"CESM1-CAM5", "CMCC-CESM", "CMCC-CM", "CMCC-CMS", "CNRM-CM5", "CSIRO-Mk3-6-0",} \cr \code{ "CSIRO-Mk3L-1-2", "EC-EARTH", "FGOALS-g2", "FIO-ESM", "GFDL-CM3", "GFDL-ESM2G", } \cr \code{"GFDL-ESM2M","GISS-E2-H", "GISS-E2-H-CC", "GISS-E2-R", "GISS-E2-R-CC", "HadGEM2-AO",} \cr \code{"HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR","MIROC-ESM",} \cr \code{"MIROC-ESM-CHEM","MIROC5", "MPI-ESM-LR", "MPI-ESM-MR", "MRI-CGCM3", "MRI-ESM1",} \cr \code{ "NorESM1-M","NorESM1-ME")}
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
#'@return Downscaled CHELSA CMIP5 climatologies for 2050 and 2070.
#'
#'@references D. N. Karger, O. Conrad, J. B{\"o}hner , et al. "Climatologies at high resolution for the earth's land surface areas". In: _Scientific Data_ 4.1 (Sep. 2017). DOI: 10.1038/sdata.2017.122. <URL: https://doi.org/10.1038/sdata.2017.122>.
#'@references D. N. Karger, O. Conrad, J. B{\"o}hner , et al. _Data from: Climatologies at high resolution for the earth's land surface areas_. En. 2018. DOI: 10.5061/DRYAD.KD1D4. <URL: http://datadryad.org/stash/dataset/doi:10.5061/dryad.kd1d4>.
#'
#'@examples
#' \dontrun{
#' # Bioclim
#' Chelsa.CMIP_5.download(parameter = "bio",
#'                         bio.var = c(1,19),
#'                         emission.scenario.var = "rcp26",
#'                         time.interval.var = "2041-2060",
#'                         model.var = "MPI-ESM-LR")
#' # Precipitation
#' Chelsa.CMIP_5.download(parameter = "prec",
#'                         month.var = c(1,12),
#'                         emission.scenario.var = "rcp26",
#'                         time.interval.var = "2041-2060",
#'                         model.var = "MPI-ESM-LR")
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
  call.time <- stringr::str_replace_all(
    stringr::str_replace_all(
      stringr::str_split(string = paste0(Sys.time()), 
                         pattern = "\\.")[[1]][1], 
      pattern = ":",
      replacement = "-"), 
    pattern = " ", 
    replacement = "_")
  
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
                                            stringr::str_replace_all(interm,
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
                    raster.temp <- terra::rast(dest.temp)
                    raster.temp <- terra::clamp(raster.temp, lower = -1000,
                                         values = FALSE)
                    gc()
                    raster.temp <- process.raster.int.doub(dest.temp)
                    gc()
                    terra::writeRaster(x = raster.temp,
                                       filename = dest.temp,
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
                                     zip.name = paste0("CHELSACMIP5_", i, ""),
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
                  
                  raster.temp <- terra::rast(dest.temp)
                  # raster.values <- values(raster.temp)
                  # raster.values[raster.values==-32768] <- NA
                  # values(raster.temp) <- raster.values
                  # rm(raster.values); gc()
                  raster.temp <- terra::clamp(raster.temp,
                                       lower = -1000,
                                       values = FALSE)
                  gc()
                  if(bio <= 11){
                    gc()
                  raster.temp <- process.raster.int.doub(raster.temp) 
                  }
                    terra::writeRaster(x = raster.temp,
                                       filename = dest.temp,
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
                                     zip.name = paste0("CHELSACMIP5_", i, ""),
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
  if(save.bib.file == TRUE) save.citation(save.location = save.location, dataSetName = "CHELSA")
}



#'@title Function for downloading CHELSA Last Glacial Maximum datasets
#'@author Helge Jentsch
#'@description This function supports a download of the CHELSA Last Glacial Maximum Climate datasets (21.000 BP). This includes monthly precipitation sums in mm, monthly temperature (average, maximum, minimum) in degree Celsius, annual characteristics (19 bioclimatic parameters), and a global digital elevation model. For further information, please regard \url{http://chelsa-climate.org/last-glacial-maximum-climate/}.\cr To allow pre-processing, clipping and buffering, conversion to ASCII-grids and stacking options are included.\cr Optional an output of a .bib-file of the cited literature can be retrieved.\cr For user convenience, saving directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'@details "The CHELSA LGM data is based on a implementation of the CHELSA algorithm on PMIP3 data." (CHELSA Climate 2020: \url{http://chelsa-climate.org/last-glacial-maximum-climate/})
#'
#'@note Please note that the downloaded data for temperature and the therefore also the first eleven bioclim-variables are processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time.\cr For some of the datasets not all models are available. For the ones that are not supported the data will not be downloaded and a warning will be prompted. See parameter \code{model.var} for more information or check the website of CHELSA Climate (\url{http://chelsa-climate.org/last-glacial-maximum-climate/}).
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
#'@param save.bib.file logical. Whether a BibTex-citation file of the CHELSA dataset should be provided in the Working directory. \cr Default: \code{TRUE}
#'
#'@return Downscaled global climatological data from the last glacial maximum.
#'
#'@references D. N. Karger, O. Conrad, J. B{\"o}hner , et al. "Climatologies at high resolution for the earth's land surface areas". In: _Scientific Data_ 4.1 (Sep. 2017). DOI: 10.1038/sdata.2017.122. <URL: https://doi.org/10.1038/sdata.2017.122>.
#'@references D. N. Karger, O. Conrad, J. B{\"o}hner , et al. _Data from: Climatologies at high resolution for the earth's land surface areas_. En. 2018. DOI: 10.5061/DRYAD.KD1D4. <URL: http://datadryad.org/stash/dataset/doi:10.5061/dryad.kd1d4>.
#'
#'@examples
#' \dontrun{
#' # Bioclim
#' Chelsa.lgm.download(parameter = "bio",
#'                     bio.var = c(1,19),
#'                     model.var = "MPI-ESM-P")
#' # Precipitation
#' Chelsa.lgm.download(parameter = "prec",
#'                     month.var = c(1,12),
#'                     model.var = "MPI-ESM-P")
#' }
#'
#'
#'@import stringr
#'@import RCurl
#'@import ncdf4
#'@import terra
#'@importFrom utils unzip download.file setTxtProgressBar txtProgressBar
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
  if(is.element("prec", parameter)|is.element("temp", parameter)|
     is.element("tmax", parameter)|is.element("tmin", parameter)){
    month.var <- c(month.var)
    if(!is.numeric(month.var)) stop("month.var needs to be a numeric vector")
    # month.var <- str_pad(month.var, 2, 'left', pad = "0")
  }
  
  if(is.element("bio", parameter)){
    bio.var <- c(bio.var)
    if(!is.numeric(bio.var)) stop("bio.var needs to be a numeric vector")
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
      # download of the requested datasets -------------------------------------
      temp.temp.save.location <- paste0(temp.save.location,
                                        stringr::str_replace_all(interm,
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
                raster.temp <- terra::rast(dest.temp)
                
                raster.temp <- terra::clamp(raster.temp, lower = -1000, values= FALSE)
                gc()
                
                # Conversion Float
                raster.temp <- process.raster.int.doub(raster.temp)
                # umrechnung Kelvin - Celsius
                gc()
                raster.temp <- process.raster.offset(raster.layer= raster.temp)                
                terra::writeRaster(x = raster.temp,
                            filename = dest.temp,
                            overwrite = TRUE)
                rm(raster.temp)
                gc()
              }else{
                # for precipitation as http://chelsa-climate.org/last-glacial-maximum-climate/ says
                raster.temp <- terra::rast(dest.temp)
                raster.temp <- terra::clamp(raster.temp, upper = 30000, values= FALSE)
                gc()
                raster.temp <- process.raster.int.doub(raster.temp)
                
                terra::writeRaster(x = raster.temp,
                            filename = dest.temp,
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
                                 zip.name = paste0("CHELSALGM_", i, ""),
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
                raster.temp <- terra::rast(dest.temp)
                raster.temp <- terra::clamp(raster.temp, upper = -1000, values= FALSE)
              gc()
              
              if(bio <= 11){
                # values(raster.temp) <- as.numeric(values(raster.temp)/10)
                raster.temp <- process.raster.int.doub(raster.temp)
              }
              terra::writeRaster(x = raster.temp,
                                 filename = dest.temp,
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
                                 zip.name = paste0("CHELSALGM_", i, ""),
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
    if(!dir.exists(paste0(save.location, "/elev/CHELSA_LGM_Elevation_Grid"))){
      dir.create(paste0(save.location, "/elev/CHELSA_LGM_Elevation_Grid"))
    }
    if(!file.exists(paste0(save.location, "/elev/CHELSA_LGM_Elevation_Grid/", "CHELSA_PMIP_dem_global.tif"))){
      download.file(url = "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/pmip3/DEM/high_longlat.tif",
                    destfile = paste0(save.location, "/elev/CHELSA_LGM_Elevation_Grid/", "CHELSA_PMIP_dem_global.tif"),
                    overwrite = TRUE,
                    mode = 'wb',
                    quiet = FALSE)
    }
  }
  # Saving BIB File
  if(save.bib.file == TRUE) save.citation(save.location = save.location, dataSetName = "CHELSA")
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
#'@references D. N. Karger, O. Conrad, J. B{\"o}hner , et al. "Climatologies at high resolution for the earth's land surface areas". In: _Scientific Data_ 4.1 (Sep. 2017). DOI: 10.1038/sdata.2017.122. <URL: https://doi.org/10.1038/sdata.2017.122>.
#'@references D. N. Karger, O. Conrad, J. B{\"o}hner , et al. _Data from: Climatologies at high resolution for the earth's land surface areas_. En. 2018. DOI: 10.5061/DRYAD.KD1D4. <URL: http://datadryad.org/stash/dataset/doi:10.5061/dryad.kd1d4>.
#'
#'@examples
#' \dontrun{
#' Chelsa.timeseries.download(parameter = "prec",
#'                             start.year.var = 2000,
#'                             start.month.var = 1,
#'                             end.year.var = 2002,
#'                             end.month.var = 12,
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
#'
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
                                      stringr::str_replace_all(interm,
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
            raster.temp <- terra::rast(dest.file)
            
            raster.temp <- terra::clamp(raster.temp, lower = -1000, values = FALSE)
            gc()
            
            raster.temp <- process.raster.int.doub(raster.temp)
            raster.temp <- process.raster.offset(raster.layer = raster.temp)
            
            terra::writeRaster(x = raster.temp,
                          filename = dest.file,
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
                             zip.name = paste0("CHELSATimeseries_", i, ""),
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
  if(save.bib.file == TRUE) save.citation(save.location = save.location, dataSetName = "CHELSA")
}

#'@title CHELSA CRU Timeseries Download
#'@author Helge Jentsch
#'@description This function supports a download of the CHELSA CRU Timeseries dataset (Jan. 1901 - Dec. 2016). This includes precipitation sums in mm and temperature (maximum, minimum) in degree Celsius. For further information, please regard \url{http://chelsa-climate.org/chelsacruts/}.\cr To allow pre-processing, clipping and buffering, conversion to ASCII-grids and stacking options are included.\cr Optional an output of a .bib-file of the cited literature can be retrieved.\cr For user convenience, saving directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
#'
#'@note Please note that the downloaded data for temperature is processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time.
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
#'@param save.bib.file logical. Whether a BibTex-citation file of the dataset should be provided in the Working directory. \cr Default: \code{TRUE}
#'
#'@return Custom dataset of CHELSA CRU Timeseries for a chosen timeseries.
#'
#'
#'@references D. N. Karger, O. Conrad, J. B{\"o}hner , et al. "Climatologies at high resolution for the earth's land surface areas". In: _Scientific Data_ 4.1 (Sep. 2017). DOI: 10.1038/sdata.2017.122. <URL: https://doi.org/10.1038/sdata.2017.122>.
#'@references D. N. Karger, O. Conrad, J. B{\"o}hner , et al. _Data from: Climatologies at high resolution for the earth's land surface areas_. En. 2018. DOI: 10.5061/DRYAD.KD1D4. <URL: http://datadryad.org/stash/dataset/doi:10.5061/dryad.kd1d4>.
#'@references D. N. Karger and N. E. Zimmermann. _CHELSAcruts - High resolution temperature and precipitation timeseries for the 20th century and beyond_. 2018. DOI: http://dx.doi.org/10.16904/envidat.159.
#'
#'@examples
#' \dontrun{
#' Chelsa.CRUts.download(parameter = "prec",
#'                        start.year.var = 2000,
#'                        start.month.var = 1,
#'                        end.year.var = 2002,
#'                        end.month.var = 12,
#'                        include.month.var = c(1,12))
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
  if(is.element("prec", parameter)|
     is.element("tmax", parameter)|is.element("tmin", parameter)){
    include.month.var <- c(include.month.var)
    if(!is.numeric(include.month.var)) {
      stop(paste0("CHELSA CRU timeseries is only available for ",
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
                                      stringr::str_replace_all(interm,
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
            # gdalUtils::gdalwarp(dest.temp, raster.temp.file.dest)
            # raster.temp <- terra::rast(raster.temp.file.dest)
            raster.temp <- terra::rast(dest.temp)
            raster.temp <- terra::clamp(raster.temp, lower = -1000)
            raster.temp <- process.raster.int.doub(raster.temp)
            terra::writeRaster(x = raster.temp,
                        filename = dest.temp,
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
                             zip.name = paste0("CHELSACRUts_", i, "_V.1.0"),
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
  if(save.bib.file == TRUE) save.citation(save.location = save.location, dataSetName = "CHELSA")
}

