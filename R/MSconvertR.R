#' msConvert
#' @describeIn MSConvert msConvert
#' @export
#'
msConvert <- function(raw.files,
                      ms.data.names,
                      format.to = "mzML",
                      BPPARAM = BiocParallel::SnowParam(workers = parallel::detectCores()-1)
                      ){

  ### pre
  {

    if (length(raw.files)==1)
      BPPARAM = BiocParallel::SerialParam()
    msconvert <- MSConvert_get_dir()
    raw.files <- gsub(pattern = "\\",x = raw.files,replacement = "/",fixed = T)%>%
      na.omit()
    ms.data.names <- gsub(pattern = "\\",x = ms.data.names,replacement = "/",fixed = T)%>%
      na.omit()
    ms.data.files <- sapply(ms.data.names,function(x){
      if (!grepl(paste0(format.to,"$"),x)) {
        paste0(x,".",format.to)
      }else{
        x
      }
    },USE.NAMES = FALSE)

  }

  ###check file and directory
  {
    if(!any(file.exists(raw.files))){
      stop(paste0("File not found : ",sum(!file.exists(raw.files)),"/", length(raw.files)))
    }
    if(length(raw.files) != length(ms.data.files)){
      stop("raw files and mzml files not match")
    }
    sapply(unique(dirname(ms.data.files)),dir.create,recursive =T,showWarnings =F)

  }

  ###msconvert
  {

    shell.commomd <- paste0(msconvert," --ignoreUnknownInstrumentError ",
                            "  --filter \"peakPicking true 1-\" --",format.to," ",
                            raw.files,
                            " -o ",
                            dirname(ms.data.files),
                            " --outfile ",
                            ms.data.files)
    #system(shell.commomd,intern = T)


    BiocParallel::bplapply(shell.commomd,
                           FUN = function(x){ system(x,intern = T)},
                           BPPARAM = BPPARAM)
    return(0)

  }






}


#' msConvertDir
#' @describeIn MSConvert msConvertDir
#' @export
#'
msConvertDir <- function(raw.path,format.to = "mzXML"){

  dir.create(paste0(raw.path,"/msData"),recursive = T)
  raw.files <- data.frame(raw.file = dir(path = raw.path,full.names = T))%>%
    dplyr::mutate(format = case_when(grepl(pattern = ".raw$",x = raw.file)~".raw",
                                     grepl(pattern = ".wiff$",x = raw.file)~".wiff",
                                     T~"unknow"
    ))%>%
    dplyr::filter(format %in% c(".raw",".wiff"))%>%
    dplyr::group_by(raw.file)%>%
    dplyr::mutate(msData = paste0(dirname(raw.file),
                                "/msData/",
                                gsub(x = basename(raw.file) ,
                                     replacement = paste0(".",format.to),
                                     pattern = paste0(format,"$"))),
                  file.exist = file.exists(msData))%>%
    dplyr::filter(!file.exist)
  msConvert(raw.files$raw.file,raw.files$msData,format.to)
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


#' msConvert2mzXML
#' @describeIn MSConvert msConvert2mzXML
#' @export
#'
msConvert2mzXML <- function(raw.files ,
                           mzXML.files,
                           BPPARAM = BiocParallel::SnowParam(workers = parallel::detectCores()-1)){

  msconvert <- MSConvert_get_dir()
  raw.files <- gsub(pattern = "\\",x = raw.files,replacement = "/",fixed = T)%>%
    na.omit()
  mzXML.files <- gsub(pattern = "\\",x = mzXML.files,replacement = "/",fixed = T)%>%
    na.omit()


  ###check file and directory
  {
    if(!any(file.exists(raw.files))){
      stop(paste0("File not found : ",sum(!file.exists(raw.files)),"/", length(raw.files)))
    }
    if(length(raw.files) != length(mzXML.files)){
      stop("raw files and mzXML files not match")
    }
    sapply(unique(dirname(mzXML.files)),dir.create,recursive =T,showWarnings =F)

    }

  ###msconvert
  {

    shell.commomd <- paste0(msconvert," --ignoreUnknownInstrumentError ",
                            "  --filter \"peakPicking true 1-\" --mzXML ",
                            raw.files,
                            " -o ",
                            dirname(mzXML.files),
                            " --outfile ",
                            mzXML.files)
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


#' @describeIn msConvert Convert raw data from mass spectrometry
#' @title msConvert
#' @param raw.files data input
#' @param dir.to dirname(raw.files)
#' @param format.to "mzML
#' @param BPPARAM BPPARAM
#'
#' @export
#'
msConvert2SciexMultipleWiff <- function(raw.files,
                                        dir.to = dirname(raw.files),
                                        format.to = "mzML",
                                        BPPARAM = BiocParallel::SnowParam(workers = parallel::detectCores()-1)){


  ### pre
  {

    if (length(raw.files)==1)
      BPPARAM = BiocParallel::SerialParam()
    msconvert <- MSConvert_get_dir()
    raw.files <- gsub(pattern = "\\",x = raw.files,replacement = "/",fixed = T)%>%
      na.omit()


  }

  ###check file and directory
  {
    if(!any(file.exists(raw.files))){
      stop(paste0("File not found : ",sum(!file.exists(raw.files)),"/", length(raw.files)))
    }


  }

  ###msconvert
  {

    shell.commomd <- paste0(msconvert," --ignoreUnknownInstrumentError ",
                            "  --filter \"peakPicking true 1-\" --",format.to," ",
                            raw.files,
                            " -o ",
                            dir.to)
    #system(shell.commomd,intern = T)


    BiocParallel::bplapply(shell.commomd,
                           FUN = function(x){ system(x,intern = T)},
                           BPPARAM = BPPARAM)
    return(0)

  }


}

