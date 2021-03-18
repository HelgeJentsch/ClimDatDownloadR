#'@title Clipping .tif-raster files in one specified directory
#'@author Helge Jentsch
#'@description This function clips all .tif-raster files in a specified directory (clip.save.location) to an extent given by a shapefile or user input with the possibility of adding a buffer. Furthermore an option for conversion to ASCII format is given.
#'
#'@param clip.save.location string (directory path). The directory where .tif-raster files are saved. \cr Default: \code{"./"} (Working directory)
#'@param clip.shapefile string (file path to a ESRI shapefile with the file extension ".shp"). \cr Extent of this shapefile is used to clip the .tif-raster files if no buffer is specified. \cr Default: \code{NULL}
#'@param clip.extent numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr It is used if no shapefile input is specified. If also left unspecified, the maximum extent (of the raster file) is used.\cr Default: \code{c(-180, 180, -90, 90)}
#'@param buffer numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}
#'@param convert.files.to.asc logical. Input whether the clipped output should be converted into ASCII-Grids. \cr Default: \code{FALSE}
#'@param time.stamp.var string. Timestamp to create unique directories for multiple run outputs. \cr Default: \code{stringr::str_replace_all(stringr::str_replace_all(paste0(} \cr \code{Sys.time()),pattern = ":",replacement = "-"))}
#'
#'@return This function returns, depending on the parameter \code{convert.files.to.asc} whether it is ASCII- or GEOTIFF-format, clipped raster files to a new directory. This directory is dynamically created.
#'
#'@import raster
#'@import stringr
#'@import sp
#'@import sf
#'
#'@examples
#'#clipping.tif(clip.save.location = system.file("pictures/", package = "rgdal"))
#'
#'@export
clipping.tif  <- function(clip.save.location = "./",
                          clip.shapefile = NULL,
                          clip.extent = c(-180, 180, -90, 90),
                          buffer = 0,
                          convert.files.to.asc = FALSE,
                          time.stamp.var = str_replace_all(str_replace_all(paste0(Sys.time()), pattern = ":", replacement = "-"), pattern = " ", replacement = "_")
){
  gc()
  # requireNamespace("raster")
  # requireNamespace("stringr")
  # requireNamespace("sp")
  global.crs <- raster::crs(
    raster::projection("+proj=longlat +datum=WGS84 +no_defs")
  )
  if((length(clip.extent) != 4) & !is.null(clip.extent)) stop("Please enter a extent as shown in the help! E.g. c(0, 30, 25, 80)")
  if((is.null(clip.shapefile) & is.null(clip.extent))) stop("Please provide a valid extent or shapefile to which the files should be clipped!")
  temp.list.files <- list.files(clip.save.location,
                                full.names = TRUE,
                                recursive = FALSE,
                                pattern = ".tif")
  if(length(temp.list.files) == 0) stop(paste0("No files found at location: ", clip.save.location))
  temp.list.file.names <- list.files(clip.save.location,
                                     full.names = FALSE,
                                     recursive = FALSE,
                                     pattern = ".tif")
  temp.list.file.names <- str_remove(temp.list.file.names,
                                     pattern = ".tif")
  dir.create(paste0(clip.save.location,"/clipped_",time.stamp.var),
             showWarnings = FALSE)
  if(!is.null(clip.shapefile)){
    temp.shp <- st_read(clip.shapefile, quiet = TRUE)
    temp.shp.crs <- as.character(st_crs(temp.shp$geometry)[[1]])
    if(is.na(temp.shp.crs)){
      # print(temp.shp)
      stop(paste0("No spatial reference was found. \n",
                  "Please set the spatial reference of the shapefile and restart!"), )
    }
    if(temp.shp.crs != "GCS"){
      warning("Shapefile is not in GCS! It will be transformed in the next step.",
              immediate. = T)
      crs_wgs84 <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
      new.extent <- as.numeric(st_bbox(obj = st_transform(x = temp.shp,
                                                          crs = crs_wgs84))[c(1,3,2,4)])
      temp.clip.extent <- raster::extent(x = new.extent)
    }else{
      temp.clip.extent <- raster::extent(as.numeric(st_bbox(temp.shp)[c(1,3,2,4)]))
    }
    rm(temp.shp.crs, temp.shp)
    gc()
  }else{
    temp.clip.extent <- extent(clip.extent)
  }
  # print(temp.clip.extent)
  for (temp.file in 1:length(temp.list.files)) {
    gc()
    temp.raster <- raster(temp.list.files[temp.file])
    # print(crs(temp.raster))
    # reproject raster if it is not in GCS
    # if(as.character(crs(temp.raster)) != as.character(global.crs)){
    #   # warning("Raster is not in GCS! It will be transformed in the next step.",
    #   # call. = TRUE,
    #   # immediate. = TRUE)
    #   temp.raster <- projectRaster(from = temp.raster, crs = global.crs)
    # }
    if(!is.null(intersect(x = extent(temp.raster),y = temp.clip.extent+buffer))){
      # if there is a buffer
      if(buffer > 0){
        temp.raster.extent <- extent(temp.clip.extent)+buffer
        if(temp.raster.extent > extent(temp.raster)){
          temp.raster.extent <- extent(temp.raster)
          warning("New extent was bigger than the original. So the max extent was used",
                  call. = TRUE,
                  immediate. = TRUE)
        }
        temp.raster <- crop(temp.raster, temp.raster.extent)
      }else{
        if(buffer == 0){
          if(temp.clip.extent > extent(temp.raster)){
            temp.clip.extent <- extent(temp.raster)
            warning("New extent was bigger than the original. So the max extent was used",
                    call. = TRUE,
                    immediate. = TRUE)
          }
          temp.raster <- crop(temp.raster, temp.clip.extent)
        }else{
          warning("Buffer is negative. Please consider adjusting the extent",
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
      writeRaster(temp.raster, raster.save.location,
                  overwrite = TRUE)
    }else{
      raster.save.location <- paste0(clip.save.location,
                                     "/clipped_",time.stamp.var,"/",
                                     temp.list.file.names[temp.file],
                                     "_clipped.asc")
      writeRaster(temp.raster, raster.save.location,
                  format = "ascii", overwrite = TRUE)
    }
    gc()
  }
}

#'@title Converting .tif-raster files in one specified directory into ASCII-Grids
#'@author Helge Jentsch
#'@description This function Converts all .tif-raster files in a specified directory into an ASCII-File format in a new directory named "ASCII_files" with a unique timestamp of the current system time.
#'
#'@param save.location string (directory path). The directory where .tif-raster files are saved. \cr Default: \code{"./"} (Working directory)
#'@param time.stamp.var string. Timestamp to create unique directories for multiple run outputs. \cr Default: \code{stringr::str_replace_all(stringr::str_replace_all(paste0(} \cr \code{Sys.time()),pattern = ":",replacement = "-"))}
#'
#'@return This function returns ASCII-format raster files to a new directory. This directory is dynamically created.
#'
#'@import raster
#'@import stringr
#'
#'@examples
#'convert.to.asc(save.location = system.file("pictures/", package = "rgdal"))
#'@export
convert.to.asc <- function(save.location = "./",
                           time.stamp.var = str_replace_all(str_replace_all(paste0(Sys.time()), pattern = ":", replacement = "-"), pattern = " ", replacement = "_")
){
  gc()
  # requireNamespace("raster")
  # requireNamespace("stringr")
  temp.save.location <- normalizePath(save.location, winslash = "/")
  file.path.list <- list.files(temp.save.location, pattern=".tif", full.names = T)
  temp.save.location <- paste0(temp.save.location, "/")

  if(!dir.exists(paste0(temp.save.location, "ASCII_files_",time.stamp.var))){
    dir.create(paste0(temp.save.location, "ASCII_files_",time.stamp.var))
  }
  temp.save.location <- paste0(temp.save.location, "ASCII_files_",time.stamp.var,"/")
  # print(file.path.list)
  file.list <- str_remove(file.path.list,
                          pattern = paste0(
                            normalizePath(save.location, winslash = "/"), "/"))
  file.list <- str_remove(file.list, pattern = ".tif")
  if(length(file.list) >= 1){
    for (layer in 1:length(file.list)) {
      # print(file.list[layer])
      # print(file.path.list[layer])
      temp.raster <- raster(file.path.list[layer])
      # temp.raster <- crop(temp.raster, extent(c(5,20,40,55)))
      writeRaster(temp.raster,paste0(temp.save.location, file.list[layer], ".asc"),
                  format = "ascii", overwrite = TRUE)
      gc()
    }
  }else{
    warning("No .tif- rasterfiles can be found in the given directory.")
  }
}

#'@title Stacking .tif-raster files in one specified directory
#'@author Helge Jentsch
#'@description This function stacks all .tif-raster files in a specified directory and saves the Raster*stack File as netCDF-file in the specified directory.
#'
#'@param stack.save.location string (directory path). The directory where .tif-raster files are saved. Rasterfiles must be in a Geographic Coordinate System (in Arc-Degrees) \cr Default: \code{"./"} (Working directory)
#'@param stack.clipped logical. Input whether clipped data should be stacked and saved as netCDF as well. \cr Default: \code{FALSE}
#'@param parameter.var string. Input whether bioclim or climatic parameters are the input for the stacking process. \cr Default: \code{NULL}
#'@param variable.numbers numeric (vector). Input how the stack variables should be called. By default just a sequence from 1 to the number of ".tif"-files are used.\cr Default: \code{c(1:length(list.files("./", pattern = ".tif")))}
#'@param stack.time.series logical. Input whether a timeseries should be stacked. \cr Default: \code{FALSE}
#'@param time.series string (vector). String input of timeseries vector. \cr Default: \code{NULL}
#'@param time.stamp.var string. Timestamp to create unique directories for multiple run outputs.\cr Default: \code{stringr::str_replace_all(stringr::str_replace_all(paste0(} \cr \code{Sys.time()),pattern = ":",replacement = "-"))}
#'
#'@import raster
#'@import stringr
#'@import ncdf4
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
       is.element("bio", parameter.var))|
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
  if(length(temp.list.files)==0) stop()
  temp.list.file.names <- list.files(stack.save.location,
                                     full.names = FALSE,
                                     recursive = FALSE,
                                     pattern = ".tif")
  temp.list.file.names <- str_remove(temp.list.file.names,
                                     pattern = ".tif")

  if(stack.clipped == TRUE){
    if(dir.exists(paste0(stack.save.location,"/clipped_", time.stamp.var))){
      stack.save.location <- c( paste0(stack.save.location,"/clipped_",time.stamp.var)
                                # , stack.save.location
      )
    }else{
      stop(paste0("No clipped data found at: '",
                  paste0(stack.save.location,"/clipped_",time.stamp.var),
                  "'\n Try calling the function directly on the directory '",
                  stack.save.location,
                  "' and set the parameter 'stack.clipped' to FALSE."))
    }
  }
  # print(stack.save.location)
  for (directory in stack.save.location) {
    # print(directory)

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
    temp.stack.file.names <- str_remove(temp.stack.file.names,
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
      file.name <- str_sub(file.name, end = str_length(file.name)-4)
      time.series <- gsub(".*([0-9]{4}.[0-9]{2}).*",replacement = "\\1", x = file.name)
    }

    if(stack.time.series==FALSE){
      for (layer in 1:length(temp.stack.files)){

        if(layer != 1){
          gc()
          temp.raster <- raster(x = temp.stack.files[layer])
          temp.raster.stack <- addLayer(temp.raster.stack, temp.raster)
          rm(temp.raster)
          gc()

        }else{
          gc()
          temp.raster <- raster(x = temp.stack.files[layer])
          temp.raster.stack <- stack(temp.raster)
          rm(temp.raster)
          gc()
        }
        if(layer == length(temp.stack.files)){

          ncfname <-  paste0(directory,"/stacked", time.stamp.var, ".nc")
          gc()
          writeRaster(x = temp.raster.stack,
                      filename = ncfname,
                      varname = as.character(parameter.var),
                      xname = "Longitude",
                      yname = "Latitude",
                      zname = as.character(var.units),
                      overwrite = TRUE
          )
          gc()
          nc <- nc_open(ncfname, write = TRUE)
          ncvar_put(nc = nc, varid = as.character(var.units), vals = as.integer(variable.numbers))
          nc_close(nc)
        }
      }
    }else{
      for (layer in 1:length(temp.stack.files)){
        if(layer != 1){
          gc()
          temp.raster <- raster(x = temp.stack.files[layer])
          temp.raster.stack <- addLayer(temp.raster.stack, temp.raster)
          rm(temp.raster)
          gc()

        }else{
          gc()
          temp.raster <- raster(x = temp.stack.files[layer])
          temp.raster.stack <- stack(temp.raster)
          rm(temp.raster)
          gc()
        }
        if(layer == length(temp.stack.files)){
          ncfname <-  paste0(directory,"/stacked", time.stamp.var, ".nc")
          gc()
          writeRaster(x = temp.raster.stack,
                      filename = ncfname,
                      varname = as.character(parameter.var),
                      xname = "Longitude",
                      yname = "Latitude",
                      zname = as.character(var.units),
                      overwrite = TRUE
          )
          gc()
          nc <- nc_open(ncfname, write = TRUE)
          ncvar_put(nc, as.character(var.units), as.integer(str_remove_all(time.series, pattern = "_")))
          nc_close(nc)
        }
      }
    }
  }
}

#'@title Save the citation of the downloaded dataset
#'@author Helge Jentsch
#'
#'@description Saves the citation of the downloaded dataset into a BibTex-file in the Working Directory.
#'
#'@param save.location string (directory path). Where the BibTex-file will be saved. \cr Default: \code{"./"} (Working Directory)
#'@param dataSetName string (vector). Specifies which dataset was downloaded or which citation should be saved. \cr Default: \code{c("Chelsa1.2", "WorldClim1.4", "WorldClim2.1")} (all available datasets)
#'
#'@return BIBtex-file with biliography of the downloaded dataset
#'@note DISCLAIMER: No warranty or liability! The citations are provided without any warranty of any kind whatsoever, either expressed or implied, including warranties of merchantability and fitness for a particular purpose. The author should not be responsible for any incomplete citation of datasets or climate data products downloaded through this package.
#'
#'@importFrom RefManageR ReadCrossRef
#'@importFrom RefManageR WriteBib
#'
#'@export
save.citation <- function(save.location = "./",
                          dataSetName= c("Chelsa",
                                         "WorldClim1.4",
                                         "WorldClim2.1")){
  gc()
  # requireNamespace("RefManageR")
  save.location <- paste0(normalizePath(save.location, winslash = "/"), "/")

  if(dataSetName == "Chelsa"){
    if(!file.exists(paste0(save.location, "chelsa_citation.bib"))){
      citation_paper <- RefManageR::ReadCrossRef("10.1038/sdata.2017.122")
      citation_data <- RefManageR::ReadCrossRef("10.5061/dryad.kd1d4")
      print(citation_paper)
      print(citation_data)
      RefManageR::WriteBib(bib = c(citation_paper, citation_data), file = paste0(save.location, "chelsa_citation.bib"))
    }
  }
  if (dataSetName == "WorldClim1.4") {
    if(!file.exists(paste0(save.location, "Worldclim14_citation.bib"))){
      citation <- RefManageR::ReadCrossRef("10.1002/joc.1276")
      RefManageR::WriteBib(bib = citation, file = paste0(save.location, "Worldclim14_citation.bib"))
    }
  }
  if (dataSetName == "WorldClim2.1") {
    if(!file.exists(paste0(save.location, "WorldClim21_citation.bib"))){
      citation <- RefManageR::ReadCrossRef("10.1002/joc.5086")
      print(citation)
      RefManageR::WriteBib(bib = citation, file = paste0(save.location, "WorldClim21_citation.bib"))
    }
  }
}

#'@title Combines all .tif-raster files into a .zip-file
#'@author Helge Jentsch
#'@description Combines all .tif-raster files of one directory into a .zip-file with a custom name.
#'
#'@param save.location string (directory path). The directory where .tif-raster files are saved and the created/updated .zip file should be saved. \cr Default: \code{"./"} (Working directory)
#'@param zip.name string. Input how the .zip-file should be named. \cr Default: \code{"RAWDATA"}
#'@param unique.name logical. Should the .zip-file be named uniquely? If TRUE the current system time is added as a timestamp to create unique directories for multiple run outputs. \cr Default: \code{TRUE}
#'@param time.stamp.var string. Input of current system time or, if called within another function the initial time of the execution. \cr Default: \code{stringr::str_replace_all(stringr::str_replace_all(paste0(} \cr \code{Sys.time()),pattern = ":",replacement = "-"))}
#'
#'@importFrom utils zip
#'@importFrom utils sessionInfo
#'@import stringr
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
  reset.wd <- getwd()
  temp.save.location <- paste0(normalizePath(save.location, winslash = "/"), "/")
  setwd(temp.save.location)
  file.path.list <- list.files(temp.save.location, pattern=".tif", full.names = T)
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
#'@description Takes input Raster* File, crops it, processes the integer values into double values, mosaiks the cropped and processed data, and returns the mosaiked Rasterfile
#'
#'@param raster.file Raster*File to be processed
#'
#'@return Rasterfile
#'
#'@import raster
#'@import sp
#'@export
process.raster.int.doub <- function(raster.file = NULL)
{
  gc()
  if(is.null(raster.file)){
    stop("raster.file is NULL")
  }
  if(class(raster.file)[1] != "RasterLayer"){
    stop("raster.file is not a 'RasterLayer'")
  }
  extent_rasterfile <- extent(raster.file)
  # crop
  tl <- crop(raster.file, extent(extent_rasterfile@xmin,
                                 (extent_rasterfile@xmin+extent_rasterfile@xmax)/2,
                                 (extent_rasterfile@ymin+extent_rasterfile@ymax)/2,
                                 extent_rasterfile@ymax)
  )
  bl <- crop(raster.file, extent(extent_rasterfile@xmin,
                                 (extent_rasterfile@xmin+extent_rasterfile@xmax)/2,
                                 extent_rasterfile@ymin,
                                 (extent_rasterfile@ymin+extent_rasterfile@ymax)/2)
  )
  tr <- crop(raster.file, extent((extent_rasterfile@xmin+extent_rasterfile@xmax)/2,
                                 extent_rasterfile@xmax,
                                 (extent_rasterfile@ymin+extent_rasterfile@ymax)/2,
                                 extent_rasterfile@ymax)
  )
  br <- crop(raster.file, extent((extent_rasterfile@xmin+extent_rasterfile@xmax)/2,
                                 extent_rasterfile@xmax,
                                 extent_rasterfile@ymin,
                                 (extent_rasterfile@ymin+extent_rasterfile@ymax)/2)
  )
  # recalculate like:
  # values(raster.temp) <- as.numeric(values(raster.temp)/10)
  # values(tl) <- as.numeric(values(tl)/10)
  tl <- tl/10
  # values(tr) <- as.numeric(values(tr)/10)
  tr <- tr/10
  # values(bl) <- as.numeric(values(bl)/10)
  bl <- bl/10
  # values(br) <- as.numeric(values(br)/10)
  br <- br/10
  # and mosaic:
  gc()
  top <- mosaic(tl,tr, fun = "mean")
  rm(tl, tr)
  gc()
  bottom <- mosaic(bl,br, fun = "mean")
  rm(bl, br)
  gc()
  raster.file <- mosaic(top, bottom, fun = "mean")
  return(raster.file)
}
