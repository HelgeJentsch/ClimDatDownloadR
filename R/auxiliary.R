#'@title Clipping .tif-raster files in one specified directory
#'@author Helge Jentsch
#'@description This function clips all .tif-raster files at a defined extent (e.g., shapefile or coordinates). Additionally, a buffer can be specified and added to the clipping extent. For user convenience, clipped rasters will be saved to a specified directory (clip.save.location). Furthermore, an option for conversion to ASCII format is given.
#'
#'@param clip.save.location string (directory path). The directory where .tif-raster files are saved. \cr Default: \code{"./"} (Working directory)
#'@param clip.shapefile string (file path to a ESRI shapefile with the file extension ".shp"). \cr Extent of this shapefile is used to clip the .tif-raster files if no buffer is specified. \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr It is used if no shapefile input is specified. If also left unspecified, the maximum extent (of the raster file) is used.\cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether the clipped output should be converted into ASCII-Grids. \cr Default: \code{FALSE}
#'@param time.stamp.var string. Timestamp to create unique directories for multiple run outputs. \cr Default: \code{stringr::str_replace_all(stringr::str_replace_all(paste0(} \cr \code{Sys.time()),pattern = ":",replacement = "-"))}
#'
#'@return This function returns, depending on the parameter \code{convert.files.to.asc} whether it is ASCII or tif format, clipped raster files to a new directory. This directory is automatically created.
#'
#'@import stringr
#'@import sf
#'@import terra
#'
#' @examples
#' \dontrun{
#' clipping.tif(clip.save.location = terra::rast(
#'                                           paste(
#'                                               system.file("ex/meuse.tif", 
#'                                               package = "terra")
#'                                               )
#'                                               )
#'                                               )
#'}
#'
#'@export
clipping.tif  <- function(clip.save.location = "./",
                          clip.shapefile = NULL,
                          clip.extent = c(-180, 180, -90, 90),
                          buffer = 0,
                          convert.files.to.asc = FALSE,
                          time.stamp.var = str_replace_all(
                            str_replace_all(
                              paste0(Sys.time()), 
                              pattern = ":", 
                              replacement = "-"), 
                            pattern = " ", 
                            replacement = "_")
){
  gc()
  
  # global.crs <- raster::crs(
  #   raster::projection("+proj=longlat +datum=WGS84 +no_defs")
  # )
  # global.crs<- terra::crs("+proj=longlat +datum=WGS84 +no_defs")
  global.crs <- "+proj=longlat +datum=WGS84 +no_defs"
  
  if((length(clip.extent) != 4) & !is.null(clip.extent)){ 
    stop("Please enter a extent as shown in the help! E.g. c(0, 30, 25, 80)")
    }
  if((is.null(clip.shapefile) & is.null(clip.extent))){
    stop("Please provide a valid extent or shapefile to which the files should be clipped!")
  }
  
  temp.list.files <- list.files(clip.save.location,
                                full.names = TRUE,
                                recursive = FALSE,
                                pattern = ".tif")
  
  if(length(temp.list.files) == 0) stop(paste0("No files found at location: ", clip.save.location))
  # print(temp.list.files)
  
  temp.list.file.names <- list.files(clip.save.location,
                                     full.names = FALSE,
                                     recursive = FALSE,
                                     pattern = ".tif")
  
  temp.list.file.names <- stringr::str_remove(temp.list.file.names,
                                              pattern = ".tif")
  
  dir.create(paste0(clip.save.location,"/clipped_",time.stamp.var),
             showWarnings = FALSE)
  
  if(!is.null(clip.shapefile)){
    # temp.shp <- st_read(clip.shapefile, quiet = TRUE)
    temp.shp <- terra::vect(clip.shapefile)
    # temp.shp.crs <- as.character(st_crs(temp.shp$geometry)[[1]])
    if(is.na(terra::is.lonlat(temp.shp))){
      stop(paste0("No spatial reference was found. \n",
                  "Please set the spatial reference of the shapefile and restart!"), )
    }
    if(!terra::is.lonlat(temp.shp)){
      warning("Shapefile is not in GCS! It will be transformed in the next step.",
              immediate. = T)
      temp.clip.extent <- terra::ext(x = terra::project(temp.shp, global.crs))
    }else{
      temp.clip.extent <- terra::ext(x = temp.shp)
    }
    rm(temp.shp)
    gc()
  }else{
    temp.clip.extent <- terra::ext(clip.extent)
  }
  for (temp.file in 1:length(temp.list.files)) {
    gc()
    temp.raster <- terra::rast(temp.list.files[temp.file])
    if(!is.null(terra::intersect(x = terra::ext(temp.raster), 
                                 y = temp.clip.extent+buffer))){
      # if there is a buffer
      if(buffer > 0){
        temp.raster.extent <- temp.clip.extent+buffer
        if(temp.raster.extent > terra::ext(temp.raster)){
          temp.raster.extent <- terra::ext(temp.raster)
          warning("New extent was bigger than the original. So the max extent was used.",
                  call. = TRUE,
                  immediate. = TRUE)
        }
        temp.raster <- terra::crop(temp.raster, temp.raster.extent)
      }else{
        if(buffer == 0){
          if(temp.clip.extent > terra::ext(temp.raster)){
            temp.clip.extent <- terra::ext(temp.raster)
            warning("New extent was bigger than the original. So the max extent was used.",
                    call. = TRUE,
                    immediate. = TRUE)
          }
          temp.raster <- terra::crop(temp.raster, temp.clip.extent)
        }else{
          warning(paste0("Buffer is negative. Please consider adjusting the extent. The current clipping extent was: ", 
                         stringr::str_c(as.list(temp.clip.extent), collapse =  " ")),
                  call. = TRUE,
                  immediate. = TRUE)
        }
      }
    }else{
      warning(paste0("Extent is not overlapping for the '",
                     temp.list.file.names[temp.file],
                     "'-Raster. Please consider adjusting the extent"),
              call. = TRUE,
              immediate. = TRUE)
      next
    }
    if(!dir.exists(paste0(clip.save.location,"/clipped_",time.stamp.var,"/"))){
      dir.create(paste0(clip.save.location,"/clipped_",time.stamp.var,"/"))
    }
    if(convert.files.to.asc == FALSE){
      raster.save.location <- paste0(clip.save.location,
                                     "/clipped_",time.stamp.var,"/",
                                     temp.list.file.names[temp.file],
                                     "_clipped.tif")
      terra::writeRaster(x = temp.raster, 
                         filename = raster.save.location,
                         overwrite = TRUE)
    }else{
      raster.save.location <- paste0(clip.save.location,
                                     "/clipped_",time.stamp.var,"/",
                                     temp.list.file.names[temp.file],
                                     "_clipped.asc")
      terra::writeRaster(x = temp.raster, 
                         filename = raster.save.location,
                         overwrite = TRUE)
    }
    gc()
  }
}

#'@title Converting .tif-raster files in one specified directory into ASCII-Grids
#'@author Helge Jentsch
#'@description This function converts and saves all .tif-raster files to ASCII-file format to a specified location and creates a new folder named "ASCII_files" with a unique timestamp of the current system time.
#'
#'@param save.location string (directory path). The directory where .tif-raster files are saved. \cr Default: \code{"./"} (Working directory)
#'@param time.stamp.var string. Timestamp to create unique directories for multiple run outputs. \cr Default: \code{stringr::str_replace_all(stringr::str_replace_all(paste0(} \cr \code{Sys.time()),pattern = ":",replacement = "-"))}
#'
#'@return This function returns ASCII-format raster files to a new directory. This directory is dynamically created.
#'
#'@import stringr
#'@import terra
#'
#'@examples
#'\dontrun{
#'convert.to.asc(save.location = system.file("pictures/", package = "rgdal"))
#'}
#'@export
convert.to.asc <- function(save.location = "./",
                           time.stamp.var = str_replace_all(str_replace_all(paste0(Sys.time()), pattern = ":", replacement = "-"), pattern = " ", replacement = "_")
){
  gc()
  temp.save.location <- normalizePath(save.location, winslash = "/")
  file.path.list <- list.files(temp.save.location, pattern=".tif", full.names = T)
  temp.save.location <- base::paste0(temp.save.location, "/")
  
  if(!base::dir.exists(base::paste0(temp.save.location, "ASCII_files_",time.stamp.var))){
    base::dir.create(base::paste0(temp.save.location, "ASCII_files_",time.stamp.var))
  }
  temp.save.location <- base::paste0(temp.save.location, "ASCII_files_",time.stamp.var,"/")
  # print(file.path.list)
  file.list <- stringr::str_remove(file.path.list,
                                   pattern = base::paste0(
                                     base::normalizePath(save.location, winslash = "/"), "/"))
  file.list <- stringr::str_remove(file.list, pattern = ".tif")
  if(length(file.list) >= 1){
    for (layer in 1:length(file.list)) {
      # print(file.list[layer])
      # print(file.path.list[layer])
      temp.raster <- terra::rast(file.path.list[layer])
      # temp.raster <- crop(temp.raster, extent(c(5,20,40,55)))
      terra::writeRaster(x = temp.raster,
                         filename = paste0(temp.save.location, file.list[layer], ".asc"),
                         # format = "ascii", 
                         overwrite = TRUE)
      gc()
    }
  }else{
    warning("No .tif- rasterfiles can be found in the given directory.")
  }
}

#'@title Stacking .tif-raster files in one specified directory
#'@author Helge Jentsch
#'@description This function stacks all .tif-raster files of a specified directory and saves the stacked layers as a netCDF-file in that directory.
#'
#'@param stack.save.location string (directory path). The directory where .tif-raster files are saved. Raster-files must be in a Geographic Coordinate System (in arc-degrees) \cr Default: \code{"./"} (Working directory)
#'@param stack.clipped logical. Input whether clipped data should be stacked and saved as netCDF as well. \cr Default: \code{FALSE}
#'@param parameter.var string. Input whether bioclim or climatic parameters are the input for the stacking process. \cr Default: \code{NULL}
#'@param variable.numbers numeric (vector). Input how the stack variables should be called. By default just a sequence from 1 to the number of tif-raster files are used.\cr Default: \code{c(1:length(list.files("./", pattern = ".tif")))}
#'@param stack.time.series logical. Input whether a timeseries should be stacked. \cr Default: \code{FALSE}
#'@param time.series string (vector). String input of timeseries vector. \cr Default: \code{NULL}
#'@param time.stamp.var string. Timestamp to create unique directories for multiple run outputs.\cr Default: \code{stringr::str_replace_all(stringr::str_replace_all(paste0(} \cr \code{Sys.time()),pattern = ":",replacement = "-"))}
#'
#'@import ncdf4
#'@import stringr
#'@import terra
#'
#'@export
stacking.downloaded.data <- function(stack.save.location = "./",
                                     stack.clipped = FALSE,
                                     parameter.var = NULL,
                                     variable.numbers = c(1:length(list.files("./", pattern = ".tif"))),
                                     stack.time.series = FALSE,
                                     time.series = NULL,
                                     time.stamp.var = stringr::str_replace_all(stringr::str_replace_all(paste0(Sys.time()),pattern = ":",replacement = "-"), pattern = " ", replacement = "_")
){
  gc()
  # Abbruchbedingungen
  if(!(is.element("prec", parameter.var)|is.element("temp", parameter.var)|
       is.element("tmax", parameter.var)|is.element("tmin", parameter.var)|
       is.element("srad", parameter.var)|is.element("wind", parameter.var)|
       is.element("vapr", parameter.var)|is.element("bio10", parameter.var)|
       base::is.element("bio", parameter.var))|
     !(length(c(parameter.var))==1)){
    print(parameter.var)
  }
  
  if(stack.time.series == TRUE & is.null(time.series)){
    stop("A timeseries should be stacked but no timeseries vector is given.")
  }
  
  # units
  var.units <- NULL
  if(parameter.var == "bio"){
    var.units <- "Bioclimatic_variables"
  }else{
    var.units <- "Month"
  }
  
  temp.list.files <- list.files(stack.save.location,
                                full.names = TRUE,
                                recursive = FALSE,
                                pattern = ".tif")
  if(length(temp.list.files)==0) stop("No Data found!")
  temp.list.file.names <- list.files(stack.save.location,
                                     full.names = FALSE,
                                     recursive = FALSE,
                                     pattern = ".tif")
  temp.list.file.names <- stringr::str_remove(temp.list.file.names,
                                              pattern = ".tif")
  
  if(stack.clipped == TRUE){
    if(dir.exists(paste0(stack.save.location,"/clipped_", time.stamp.var))){
      stack.save.location <- c(base::paste0(stack.save.location,
                                            "/clipped_",time.stamp.var)
      )
    }else{
      stop(paste0("No clipped data found at: '",
                  paste0(stack.save.location,"/clipped_",time.stamp.var),
                  "'\n Try calling the function directly on the directory '",
                  stack.save.location,
                  "' and set the parameter 'stack.clipped' to FALSE."))
    }
  }
  for (directory in stack.save.location) {
    temp.stack.files <- list.files(path = directory, full.names = TRUE,
                                   recursive = FALSE,
                                   pattern = ".tif")
    if(length(temp.stack.files)==0){
      temp.stack.files <- list.files(path = directory, full.names = TRUE,
                                     recursive = FALSE,
                                     pattern = ".asc")
      if(length(temp.stack.files)==0) stop("No files found. Check directory.")
    }
    temp.stack.file.names <- list.files(path = directory,
                                        full.names = FALSE,
                                        recursive = FALSE,
                                        pattern = ".tif")
    temp.stack.file.names <- stringr::str_remove(temp.stack.file.names,
                                                 pattern = ".tif")
    if(length(temp.stack.files)==0){
      temp.stack.file.names <- list.files(path = directory,
                                          full.names = FALSE,
                                          recursive = FALSE,
                                          pattern = ".asc")
      temp.stack.file.names <- str_remove(temp.stack.file.names,
                                          pattern = ".asc")
    }
    
    if(length(temp.stack.files)!=length(time.series)){
      file.name <- basename(temp.stack.files)
      file.name <- stringr::str_sub(file.name, end = stringr::str_length(file.name)-4)
      time.series <- gsub(".*([0-9]{4}.[0-9]{2}).*",replacement = "\\1", x = file.name)
      # print(time.series)
    }
    if(stack.time.series==FALSE){
      temp.raster <- lapply(temp.stack.files, terra::rast)
      temp.raster.stack <- terra::rast(temp.raster)
      rm(temp.raster)
      gc()
      ncfname <-  paste0(directory,"/stacked", time.stamp.var, ".nc")
      gc()
      terra::writeCDF(x = temp.raster.stack,
                      filename = ncfname,
                      varname = as.character(parameter.var),
                      zname = as.character(var.units),
                      compression = 5,
                      overwrite = TRUE
                      
      )
      gc()
      nc <- ncdf4::nc_open(ncfname, write = TRUE)
      ncdf4::ncvar_put(nc = nc, varid = as.character(var.units), vals = as.integer(variable.numbers))
      ncdf4::nc_close(nc)
      rm(nc)
      gc()
    }else{
      temp.raster <- lapply(temp.stack.files, terra::rast)
      temp.raster.stack <- terra::rast(temp.raster)
      rm(temp.raster)
      gc()
      ncfname <-  paste0(directory,"/stacked", time.stamp.var, ".nc")
      gc()
      terra::writeCDF(x = temp.raster.stack,
                      filename = ncfname,
                      varname = as.character(parameter.var),
                      zname = as.character(var.units),
                      compression = 5,
                      overwrite = TRUE
      )
      gc()
      nc <- ncdf4::nc_open(ncfname, write = TRUE)
      ncdf4::ncvar_put(nc, as.character(var.units), as.integer(str_remove_all(time.series, pattern = "_")))
      ncdf4::nc_close(nc)
      rm(nc)
      gc()
    }
  }
}

#'@title Save the citation of the downloaded dataset
#'@author Helge Jentsch
#'
#'@description Saves the citation of the downloaded dataset into a BibTex-file in the working directory.
#'
#'@param save.location string (directory path). Where the BibTex-file will be saved. \cr Default: \code{"./"} (Working Directory)
#'@param dataSetName string (vector). Specifies which dataset was downloaded or which citation should be saved. \cr Default: \code{c("Chelsa1.2", "WorldClim1.4", "WorldClim2.1")} (all available datasets)
#'
#'@return BibTex-file with biliography of the downloaded dataset
#'@note DISCLAIMER: No warranty or liability! The citations are provided without any warranty of any kind whatsoever, either expressed or implied, including warranties of merchantability and fitness for a particular purpose. The package author will not be responsible for any incomplete citation of datasets or climate data products downloaded through this package.
#'
#'@importFrom RefManageR ReadCrossRef
#'@importFrom RefManageR ReadBib
#'@importFrom RefManageR WriteBib
#'@importFrom utils download.file
#'
#'@examples
#' \dontrun{
#' save.citation(dataSetName = "Chelsa")
#' save.citation(dataSetName = "WorldClim1.4")
#' save.citation(dataSetName = "WorldClim2.1")
#' }
#'@export
save.citation <- function(save.location = "./",
                          dataSetName= c("CHELSA",
                                         "WorldClim1.4",
                                         "WorldClim2.1")){
  gc()
  # requireNamespace("RefManageR")
  save.location <- paste0(normalizePath(save.location, winslash = "/"), "/")
  
  if(dataSetName == "CHELSA"){
    print(cat("\n"))
    print("Please regard 'https://chelsa-climate.org/downloads/' for correct citations.")
    if(!file.exists(paste0(save.location, "chelsa_citation.bib"))){
      # citation_  <- RefManageR::ReadCrossRef("")
      
      citation_paper <- RefManageR::ReadCrossRef("10.1038/sdata.2017.122")
      
      citation_CHELSA_cmip5_ts <- RefManageR::ReadCrossRef("10.1038/s41597-020-00587-y")
      citation_PBCOR <- RefManageR::ReadCrossRef("10.1175/JCLI-D-19-0332.1")
      
      # citation_envidat.332  <- RefManageR::ReadCrossRef("10.16904/envidat.332") #doi.org/10.16904/envidat.332
      utils::download.file("https://www.envidat.ch/dataset/bioclim_plus/export/bibtex.bib",
                           destfile = paste0(tempdir(),"/bib_chelsa.bib"), quiet = T)
      citation_envidat.332 <- RefManageR::ReadBib(paste0(tempdir(),"/bib_chelsa.bib"))
      unlink(x = paste0(tempdir(),"/bib_chelsa.bib"))
      
      # citation_envidat.228.v2.1  <- RefManageR::ReadCrossRef("10.16904/envidat.228.v2.1") # doi.org/10.16904/envidat.228.v2.1
      utils::download.file("https://www.envidat.ch/dataset/chelsa-climatologies/export/bibtex.bib",
                           destfile = paste0(tempdir(),"/bib_chelsa.bib"), quiet = T)
      citation_envidat.228.v2.1 <- RefManageR::ReadBib(paste0(tempdir(),"/bib_chelsa.bib"))
      unlink(x = paste0(tempdir(),"/bib_chelsa.bib"))
      
      # citation_essd2022212  <- RefManageR::ReadCrossRef("10.5194/essd-2022-212") #doi.org/10.5194/essd-2022-212
      utils::download.file("https://essd.copernicus.org/articles/14/5573/2022/essd-14-5573-2022.bib",
                           destfile = paste0(tempdir(),"/bib_chelsa.bib"), quiet = T)
      citation_essd2022212 <- RefManageR::ReadBib(paste0(tempdir(),"/bib_chelsa.bib"))
      unlink(x = paste0(tempdir(),"/bib_chelsa.bib"))
      

      citation_sdata.2017.122 <- RefManageR::ReadCrossRef("10.1038/sdata.2017.122") # 10.1038/sdata.2017.122
      # Data
      citation_data <- RefManageR::ReadCrossRef("10.5061/dryad.kd1d4")
      
      utils::download.file("https://www.envidat.ch/dataset/eur11/export/bibtex.bib",
                           destfile = paste0(tempdir(),"/bib_chelsa.bib"), quiet = T)
      citation_EUR11 <- RefManageR::ReadBib(paste0(tempdir(),"/bib_chelsa.bib"))
      unlink(x = paste0(tempdir(),"/bib_chelsa.bib"))
      
      utils::download.file("https://www.envidat.ch/dataset/chelsacruts/export/bibtex.bib",
                           destfile = paste0(tempdir(),"/bib_chelsa.bib"), quiet = T)
      citation_CHELSAcruts_data   <- RefManageR::ReadBib(paste0(tempdir(),"/bib_chelsa.bib"))
      unlink(x = paste0(tempdir(),"/bib_chelsa.bib"))
      
      utils::download.file("https://www.envidat.ch/dataset/chelsa_cmip5_ts/export/bibtex.bib",
                           destfile = paste0(tempdir(),"/bib_chelsa.bib"), quiet = T)
      citation_CHELSA_cmip5_ts_data  <-  RefManageR::ReadBib(paste0(tempdir(),"/bib_chelsa.bib"))
      unlink(x = paste0(tempdir(),"/bib_chelsa.bib"))
      
      
      
      # Old versions
      citation_Version1.0  <- RefManageR::ReadCrossRef("10.1594/WDCC/CHELSA_v1")
      citation_Version1.1  <- RefManageR::ReadCrossRef("10.1594/WDCC/CHELSA_v1_1")
      
      eval(parse(text = paste0("print(c(", paste(ls(pattern = "citation_"), collapse = ","),"))")))
      eval(parse(text = paste0("RefManageR::WriteBib(bib = c(",
                               paste(ls(pattern = "citation_"), collapse = ","),
                               "), file = paste0(save.location, 'chelsa_citation.bib'))")))
    }
  }
  if (dataSetName == "WorldClim1.4") {
    print(cat("\n"))
    print("Please regard 'www.worldclim.org' for correct citations.")
    if(!file.exists(paste0(save.location, "Worldclim14_citation.bib"))){
      citation_WC14 <- RefManageR::ReadCrossRef("10.1002/joc.1276")
      # citation_  <- RefManageR::ReadCrossRef("")
      print(citation_WC14)
      RefManageR::WriteBib(bib = citation_WC14, 
                           file = paste0(save.location, 
                                         "Worldclim14_citation.bib"))
    }
  }
  if (dataSetName == "WorldClim2.1") {
    print(cat("\n"))
    print("Please regard 'www.worldclim.org' for correct citations.")
    if(!file.exists(paste0(save.location, "WorldClim21_citation.bib"))){
      citation_WC21_hist_Clim_Monthly <- RefManageR::ReadCrossRef("10.1002/joc.5086")
      citation_WC21_CRUTS403 <- RefManageR::ReadCrossRef("10.1002/joc.3711")
      
      print(c(citation_WC21_hist_Clim_Monthly, citation_WC21_CRUTS403))
      RefManageR::WriteBib(bib = c(citation_WC21_hist_Clim_Monthly, citation_WC21_CRUTS403),
                           file = paste0(save.location, "WorldClim21_citation.bib"))
    }
  }
}

#'@title Combines all .tif-raster files into a .zip-file
#'@author Helge Jentsch
#'@description Combines and saves all .tif-raster files to a .zip-file, whereas name and saving location can be specified.
#'
#'@param save.location string (directory path). The directory where .tif-raster files are saved and the created/updated .zip file will be saved. \cr Default: \code{"./"} (Working directory)
#'@param zip.name string. Input how the .zip-file should be named. \cr Default: \code{"RAWDATA"}
#'@param unique.name logical. Should the .zip-file be named uniquely? If TRUE the current system time is added as a timestamp to create unique directories for multiple run outputs. \cr Default: \code{TRUE}
#'@param time.stamp.var string. Input of current system time or, if called within another function the initial time of the execution. \cr Default: \code{stringr::str_replace_all(stringr::str_replace_all(paste0(} \cr \code{Sys.time()),pattern = ":",replacement = "-"))}
#'
#'@import stringr
#'@importFrom utils sessionInfo
#'@importFrom utils zip
#'
#'@export
combine.raw.in.zip <- function(save.location = "./",
                               zip.name = "RAWDATA",
                               unique.name = TRUE,
                               time.stamp.var = stringr::str_replace_all(
                                 stringr::str_replace_all(paste0(Sys.time()), pattern = ":",replacement = "-"),
                                 pattern = " ", replacement = "_")){
  gc()
  if(unique.name == FALSE){
    temp.time.stamp.var <- ""
  }else{
    if(unique.name == TRUE){
      temp.time.stamp.var <- paste0("_", time.stamp.var)
    }else{
      stop("'unique.name' must be logical", call. = TRUE)
    }
  }
  reset.wd <- base::getwd()
  temp.save.location <- base::paste0(normalizePath(save.location, winslash = "/"), "/")
  base::setwd(temp.save.location)
  file.path.list <- base::list.files(temp.save.location, pattern=".tif", full.names = T)
  file.path.list <- stringr::str_replace(file.path.list, pattern = getwd(), replacement = ".")
  
  if(!str_detect(utils::sessionInfo()$platform, pattern = "linux")){
    if(length(file.path.list) >= 1){
      utils::zip(paste0(zip.name, temp.time.stamp.var,".zip"), file.path.list)
    }else{
      warning(paste0("No file in this directory! \n", getwd()))
    }
  }else{
    if(length(file.path.list) >= 1){
      utils::tar(paste0(zip.name, temp.time.stamp.var,".tar"), stringr::str_remove(file.path.list, pattern = ".//"))
    }else{
      warning(paste0("No file in this directory! \n", getwd()))
    }
  }
  setwd(reset.wd)
}


#'@title Preprocessing data to get real values
#'@author Helge Jentsch
#'@description Takes input SpatRaster, processes the integer values into double values, and returns the SpatRaster.
#'
#'@param raster.layer SpatRaster to be processed
#'
#'@return SpatRaster
#'
#'@import sp
#'@import terra
#'
#'@export
process.raster.int.doub <- function(raster.layer = NULL){
  gc()
  if(is.null(raster.layer)){
    stop("raster.layer is NULL")
  }
  if(class(raster.layer)[1] != "SpatRaster"){
    stop("raster.layer is not a 'SpatRaster'")
  }
  terra::scoff(raster.layer)[[1]] <- 0.1
  gc()
  return(raster.layer)
}


#'@title Preprocessing data to undo offsetting values
#'@author Helge Jentsch
#'@description Takes input SpatRaster, deletes offset, and returns the SpatRaster.
#'
#'@param raster.layer SpatRaster to be processed.
#'@param offset Numerical value to be offsetted. Default: -273.15.
#'
#'@return SpatRaster
#'
#'@import sp
#'@import terra
#'
#'@export
process.raster.offset <- function(raster.layer = NULL, offset = -273.15){
  gc()
  if(is.null(raster.layer)){
    stop("raster.layer is NULL")
  }
  if(class(raster.layer)[1] != "SpatRaster"){
    stop("raster.layer is not a 'SpatRaster'")
  }
  terra::scoff(raster.layer)[[2]] <- offset
  gc()
  return(raster.layer)
}



#' @title Get Download Size
#' @author Helge Jentsch
#' @description Helper function that returns the download size of a vector of URLs
#' 
#' @param URLVector Character vector. Multiple vectors of valid URLs.
#' 
#' @return Download size as double numeric value
#' @import httr
#' @export
getDownloadSize <- function(URLVector){
  # helper-function by Allan Cameron (https:\/\/stackoverflow.com\/a\/63852321)
  download_size <- function(url){
    as.numeric(httr::HEAD(url)$headers$`content-length`)
  }
  filesizes <- NULL
  for(i in URLVector){
    # collect file sizes
    fileISize <- download_size(i)
    # and add to another
    filesizes <- sum(filesizes,fileISize)
    # return(Downloadsize)
  }
  if(Sys.info()[names(Sys.info()) == "sysname"] == "Windows"){
    availDiscSpaceMB <- as.numeric(stringr::str_remove(base::system("wmic logicaldisk get freespace", inter=TRUE)[2], pattern = "  \r"))/1024/1024
  }
  # else if(Sys.info()[names(Sys.info()) == "sysname"] != "Windows"){
  # }
  # if(availDiscSpaceMB < round(filesizes*0.000001, 2)){
  #   print(paste0(round(filesizes*0.000001, 2),"MB will be downloaded. These is only ", availDiscSpaceMB, "MB available. Please consider another download location."))
  #   }
  return(round(filesizes*0.000001, 2))
  # Download size in MB
  
  # get duration by calculating with https:\/\/gitlab.com\/hrbrmstr\/speedtest
  # config <- spd_config()
  # servers <- spd_servers(config=config)
  # closest_servers <- spd_closest_servers(servers, config=config)
  # speed <- spd_download_test(close_servers[1,], config=config)
  # medspeed <- speed$median
  # cat("Download-Zeit: \n", downloadSize/medspeed, "s \n")
}
