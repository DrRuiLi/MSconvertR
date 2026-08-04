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
    MSConvert_require_ready()
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

    shell.commomd <- mapply(
      function(raw.file, ms.data.file) {
        MSConvert_build_msconvert_cmd(
          raw.file = raw.file,
          out.dir = dirname(ms.data.file),
          outfile = ms.data.file,
          format.to = format.to
        )
      },
      raw.files,
      ms.data.files,
      USE.NAMES = FALSE
    )

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

  MSConvert_require_ready()
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

    shell.commomd <- mapply(
      function(raw.file, mzML.file) {
        MSConvert_build_msconvert_cmd(
          raw.file = raw.file,
          out.dir = dirname(mzML.file),
          outfile = mzML.file,
          format.to = "mzML"
        )
      },
      raw.files,
      mzML.files,
      USE.NAMES = FALSE
    )

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

  MSConvert_require_ready()
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

    shell.commomd <- mapply(
      function(raw.file, mzXML.file) {
        MSConvert_build_msconvert_cmd(
          raw.file = raw.file,
          out.dir = dirname(mzXML.file),
          outfile = mzXML.file,
          format.to = "mzXML"
        )
      },
      raw.files,
      mzXML.files,
      USE.NAMES = FALSE
    )

    BiocParallel::bplapply(shell.commomd,
                           FUN = function(x){ system(x,intern = T)},
                           BPPARAM = BPPARAM)
    return(0)

  }


}



MSConvert_Extract_Thermo_data <- function(raw.files){

  MSConvert_require_ready()
  raw.files <- gsub(pattern = "\\", x = raw.files, replacement = "/", fixed = TRUE) %>%
    na.omit() %>%
    as.character()

  shell.commomd <- vapply(raw.files, function(raw.file) {
    raw.file <- normalizePath(raw.file, winslash = "/", mustWork = TRUE)
    if (MSConvert_is_linux()) {
      MSConvert_build_cmd(
        tool = "ThermoRawMetaDump.exe",
        args = shQuote(paste0("/data/in/", basename(raw.file))),
        in_dir = dirname(raw.file)
      )
    } else {
      MSConvert_build_cmd(
        tool = "ThermoRawMetaDump.exe",
        args = shQuote(raw.file),
        in_dir = dirname(raw.file)
      )
    }
  }, character(1), USE.NAMES = FALSE)

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
    MSConvert_require_ready()
    raw.files <- gsub(pattern = "\\",x = raw.files,replacement = "/",fixed = T)%>%
      na.omit()
    if (length(dir.to) == 1L && length(raw.files) > 1L)
      dir.to <- rep(dir.to, length(raw.files))

  }

  ###check file and directory
  {
    if(!any(file.exists(raw.files))){
      stop(paste0("File not found : ",sum(!file.exists(raw.files)),"/", length(raw.files)))
    }


  }

  ###msconvert
  {

    shell.commomd <- mapply(
      function(raw.file, out.dir) {
        MSConvert_build_msconvert_cmd(
          raw.file = raw.file,
          out.dir = out.dir,
          outfile = NULL,
          format.to = format.to
        )
      },
      raw.files,
      dir.to,
      USE.NAMES = FALSE
    )

    BiocParallel::bplapply(shell.commomd,
                           FUN = function(x){ system(x,intern = T)},
                           BPPARAM = BPPARAM)
    return(0)

  }


}
