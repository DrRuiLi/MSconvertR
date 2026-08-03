
MSConvert_get_dir <- function(){

  pkg.dir <- system.file(package = "MSconvertR")
  msconvert.path <- dir(pkg.dir,pattern = "msconvert.exe$",recursive = T,full.names = T)
  msconvert.path <- ifelse(length(msconvert.path)==0,NA,msconvert.path)
  return(msconvert.path)
}


#' MSConvert_check
#' @describeIn MSConvert check if MSconvert ready
#' @export
#'
MSConvert_check <- function(){

  msconvert <- MSConvert_get_dir()

  ###check msconvert
  {
    msconvert_return <- try(system(msconvert,
                                   intern = T),silent = T)
    if(!any(grepl(pattern = "Usage: msconvert", x = msconvert_return))){
      message("MSConvert not found, re-build?\n 1:yes, 2:no")
      x <- readline()
      if (x==1) {
        #msconvert.zip <- dir(system.file(package = "MSconvertR"),
        #                     pattern = "msconvert.zip",
        #                     recursive = T,full.names = T)
        #unzip(msconvert.zip,exdir = dirname(msconvert.zip))
        #pwiz.tar <- dir(system.file(package = "MSconvertR"),
        #                pattern = "pwiz-bin-",
        #                recursive = T,full.names = T)
        #untar(pwiz.tar,
        #      exdir = paste0(dirname(pwiz.tar),"/pwiz"))
        MSConvert_Deploy()
        return(T)



      }
    }

  }
  message("MSConvert in: ",msconvert)
  message(grep("release",msconvert_return,value = T))
  return(invisible(T))




}



#' MSConvert_Download
#' @describeIn MSConvert MSConvert_Download
#' @export
#'
MSConvert_Download <- function(save_path = tempdir()){



  xml_url <- "https://proteowizard.sourceforge.io/releases/bt83.xml"
  doc <- xml2::read_xml(xml_url)

  artifacts <- xml2::xml_find_all(doc, ".//artifact")
  paths <- xml2::xml_text(artifacts)

  target <- paths[grepl("pwiz-bin-windows-x86_64.*\\.tar\\.bz2$", paths)][1]

  build_id <- sub(".*/id:([0-9]+)/.*", "\\1", target)
  filename <- basename(target)

  s3_base <- "https://mc-tca-01.s3.us-west-2.amazonaws.com/ProteoWizard/bt83"
  s3_url <- sprintf("%s/%s/%s", s3_base, build_id, filename)

  message("Download from: ", s3_url, "\n")
  filename <- paste0(save_path,"/",filename)
  if (!file.exists(filename)) {
    download.file(s3_url, destfile = filename#, method = "wininet"
    )
  }

  message("Save to: ", filename, "\n")
  return(filename)

}




#' MSConvert_Deploy
#' @describeIn MSConvert MSConvert_Deploy
#' @export
#'
MSConvert_Deploy <- function(pwiz.bz = MSConvert_Download()){


  pkg.dir <- system.file(package = "MSconvertR")
  pwiz.dir <- paste0(pkg.dir,"/pwiz")

  untar(pwiz.bz,
        exdir = pwiz.dir )
  message("MSConvert Deployed")

}
