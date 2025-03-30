
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
  return(T)




}


#' msConvertDir
#' @describeIn MSConvert msConvertDir
#' @export
#'
msConvertDir <- function(raw.path,format.to = "mzML"){

  dir.create(paste0(raw.path,"/mzML"),recursive = T)
  raw.files <- data.frame(raw.file = dir(path = raw.path,full.names = T))%>%
    dplyr::mutate(format = case_when(grepl(pattern = ".raw$",x = raw.file)~".raw",
                                     grepl(pattern = ".wiff$",x = raw.file)~".wiff",
                                     T~"unknow"
    ))%>%
    dplyr::filter(format %in% c(".raw",".wiff"))%>%
    dplyr::group_by(raw.file)%>%
    dplyr::mutate(mzML = paste0(dirname(raw.file),
                                "/mzML/",
                                gsub(x = basename(raw.file) ,
                                     replacement = ".mzML",
                                     pattern = paste0(format,"$"))),
                  file.exist = file.exists(mzML))%>%
    dplyr::filter(!file.exist)
  msConvert2mzML(raw.files$raw.file,raw.files$mzML)
  return(raw.files$mzML)


}

#' msConvert2mzML
#' @describeIn MSConvert msConvert2mzML
#' @export
#'
msConvert2mzML <- function(raw.files ,
                           mzML.files,
                           BPPARAM = BiocParallel::SnowParam(workers = parallel::detectCores()-1)){

  msconvert <- MSConvert_get_dir()
  raw.files <- gsub(pattern = "\\",x = raw.files,replacement = "/",fixed = T)%>%
    na.omit()
  mzML.files <- gsub(pattern = "\\",x = mzML.files,replacement = "/",fixed = T)%>%
    na.omit()


  ###check file and directory
  {
    if(!any(file.exists(raw.files))){
      stop(paste0("File not found : ",sum(!file.exists(raw.files)),"/", length(raw.files)))
    }
    if(length(raw.files) != length(mzML.files)){
      stop("raw files and mzml files not match")
    }
    sapply(unique(dirname(mzML.files)),dir.create,recursive =T,showWarnings =F)

    }

  ###msconvert
  {

    shell.commomd <- paste0(msconvert," --ignoreUnknownInstrumentError ",
                            "  --filter \"peakPicking true 1-\" --mzML ",
                            raw.files,
                            " -o ",
                            dirname(mzML.files),
                            " --outfile ",
                            mzML.files)
    #system(shell.commomd,intern = T)


    BiocParallel::bplapply(shell.commomd,
                           FUN = function(x){ system(x,intern = T)},
                           BPPARAM = BPPARAM)
    return(0)

  }


}



MSConvert_Extract_Thermo_data <- function(raw.files){


  ThermoRawMetaDump <- paste0(dirname(MSConvert_get_dir()),
                              "/ThermoRawMetaDump.exe"  )
  if(!file.exists(ThermoRawMetaDump)){
    stop("ThermoRawMetaDump not found")
  }

  shell.commomd <- paste0(ThermoRawMetaDump,"  ",
                          raw.files)

  data.return <- BiocParallel::bplapply(shell.commomd,
                                        FUN = function(x){ system(x,intern = T)},
                                        BPPARAM = BiocParallel::SerialParam())
  return(data.return)

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

  cat("Download from: ", s3_url, "\n")
  filename <- paste0(save_path,"/",filename)
  download.file(s3_url, destfile = filename, method = "wininet")
  cat("Save to: ", filename, "\n")
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

}

