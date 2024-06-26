% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Chelsa_Download_functions.R,
%   R/Chelsa_new_download_functions.R
\name{Chelsa.timeseries.download}
\alias{Chelsa.timeseries.download}
\title{CHELSA Timeseries Download}
\usage{
Chelsa.timeseries.download(
  save.location = "./",
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
  delete.raw.data = FALSE,
  save.bib.file = TRUE
)

Chelsa.timeseries.download(
  save.location = "./",
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
  delete.raw.data = FALSE,
  save.bib.file = TRUE
)
}
\arguments{
\item{save.location}{string. Input where the datasets should be saved. \cr Default: Working Directory.}

\item{parameter}{string (vector). Input of parameters which should be downloaded. \cr Default: \code{c("prec", "tmax", "tmin")}}

\item{start.year.var}{integer. Input year the download timeseries starts. \cr Default: 1979 (minimum)}

\item{start.month.var}{integer. Input month the download timeseries starts. \cr Default: 1 (minimum)}

\item{end.year.var}{integer. Input year the download timeseries ends. \cr Default: 2013 (maximum)}

\item{end.month.var}{integer. Input month the download timeseries ends. \cr Default: 12 (maximum)}

\item{include.month.var}{integer (vector). Input which monthly data should be downloaded. \cr Default: \code{c(1:12)}}

\item{version.var}{string (vector). Input which version of the dataset should be downloaded. Multiple selection is _not_ possible. Select between version _1.2_ and _2.1_.\cr Default:  \code{c("1.2")}}

\item{clipping}{logical. Input whether the downloaded data should be clipped.\cr If \code{FALSE}: \code{clip.shapefile}, \code{buffer}, \code{clip.extent} will be ignored. \cr Default: \code{FALSE}}

\item{clip.shapefile}{string. Input which shapefile should be used for clipping. \cr Default: \code{NULL}}

\item{buffer}{numeric. Input of decimal degrees of buffer around the shapefile and/or extent. \cr Default: \code{0}}

\item{clip.extent}{numeric (vector). Input vector with four numeric values. This is following the input order c("xleft", "xright", "ybottom", "ytop").\cr Default: \code{c(-180, 180, -90, 90)}}

\item{convert.files.to.asc}{logical. Input whether files should be converted into the ASCII format.\cr If \code{TRUE}: a new subdirectory is created and the rawdata is saved there. \cr If \code{clipping} is \code{TRUE}: the clipped raster files are also saved as ASCII grids. \cr  Default: \code{FALSE}}

\item{combine.raw.zip}{logical. Should the downloaded raw-data be "zipped". \cr Default: \code{FALSE}}

\item{delete.raw.data}{logical. Should the downloaded raw-data be deleted. If \code{combine.raw.zip} is \code{TRUE}: raw-data is still available in the zipped file. \cr Default: \code{FALSE}}

\item{save.bib.file}{logical. Whether a BibTex-citation file of the dataset should be provided in the Working directory. \cr Default: \code{TRUE}}
}
\value{
Custom dataset of CHELSA Timeseries for a chosen timeseries.

Custom dataset of CHELSA Timeseries for a chosen timeseries.
}
\description{
This function supports a download of the CHELSA Timeseries dataset (Jan. 1979 - Dec. 2013). This includes precipitation sums (mm) and temperature (average, maximum, minimum; °C) parameters. For further information, please regard \url{http://chelsa-climate.org/timeseries/}.\cr To allow pre-processing, clipping and buffering, conversion to ASCII-grids and stacking options are included.\cr Optional an output of a .bib-file of the cited literature can be retrieved.\cr For user convenience, saving directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.

This function supports a download of the CHELSA Timeseries dataset (Jan. 1979 - Dec. 2013). This includes precipitation sums (mm) and temperature (average, maximum, minimum; °C) parameters. For further information, please regard \url{http://chelsa-climate.org/timeseries/}.\cr To allow pre-processing, clipping and buffering, conversion to ASCII-grids and stacking options are included.\cr Optional an output of a .bib-file of the cited literature can be retrieved.\cr For user convenience, saving directories will be created automatically. Also options to "zip" and/or delete the RAW-files are included.
}
\note{
Please note that the downloaded data for temperature are processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time.

Please note that the downloaded data for temperature are processed to °C with one significant decimal without offset and factor. Processing and conversion to other file-formats on a global dataset may take some time.
}
\examples{
\dontrun{
Chelsa.timeseries.download(parameter = "prec",
                            start.year.var = 2000,
                            start.month.var = 1,
                            end.year.var = 2002,
                            end.month.var = 12,
                            include.month.var = c(1,12))
}

\dontrun{
Chelsa.timeseries.download(parameter = "prec",
                            start.year.var = 2000,
                            start.month.var = 1,
                            end.year.var = 2002,
                            end.month.var = 12,
                            version.var = "1.2",
                            include.month.var = c(1,12))
}

}
\references{
D. N. Karger, O. Conrad, J. B{\"o}hner , et al. "Climatologies at high resolution for the earth's land surface areas". In: _Scientific Data_ 4.1 (Sep. 2017). DOI: 10.1038/sdata.2017.122. <URL: https://doi.org/10.1038/sdata.2017.122>.

D. N. Karger, O. Conrad, J. B{\"o}hner , et al. _Data from: Climatologies at high resolution for the earth's land surface areas_. En. 2018. DOI: 10.5061/DRYAD.KD1D4. <URL: http://datadryad.org/stash/dataset/doi:10.5061/dryad.kd1d4>.

D. N. Karger, O. Conrad, J. B{\"o}hner , et al. "Climatologies at high resolution for the earth's land surface areas". In: _Scientific Data_ 4.1 (Sep. 2017). DOI: 10.1038/sdata.2017.122. <URL: https://doi.org/10.1038/sdata.2017.122>.

D. N. Karger, O. Conrad, J. B{\"o}hner , et al. _Data from: Climatologies at high resolution for the earth's land surface areas_. En. 2018. DOI: 10.5061/DRYAD.KD1D4. <URL: http://datadryad.org/stash/dataset/doi:10.5061/dryad.kd1d4>.
}
\author{
Helge Jentsch
}
