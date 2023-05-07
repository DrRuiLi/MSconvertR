

#' @title get_MSconvert_Dir
#' @description
#' get msconvert dir
#'
#'
#' @return
#' @export
#'
#' @examples
get_MSconvert_Dir <- function(silence = F){

  pkg.dir <- system.file(package = "MSconvertR")
  message("MSconvert in: \n\t"
    ,pkg.dir)
  if (file.exists(paste0(pkg.dir,"/data/msconvert/msconvert.exe"))) {


  }else{

    msconvert.zip <- paste0(pkg.dir,"/data/msconvert.zip")
    unzip(msconvert.zip,exdir = dirname(msconvert.zip))

  }
  msconvert.path <- paste0(pkg.dir,"/data/msconvert/msconvert.exe")
  return(msconvert.path)
}





msConvertDir<- function(raw.path,format.to = "mzML"){

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


msConvert2mzML <- function(raw.files ,
                           mzML.files,
                           BPPARAM = BiocParallel::SnowParam(workers = parallel::detectCores()-1)){

  msconvert <- get_MSconvert_Dir(silence = T)
  raw.files <- gsub(pattern = "\\",x = raw.files,replacement = "/",fixed = T)%>%
    na.omit()
  mzML.files <- gsub(pattern = "\\",x = mzML.files,replacement = "/",fixed = T)%>%
    na.omit()

  ###check msconvert
  {
    msconvert_return <- try(system(msconvert,
                                   intern = T))
    if(!any(grepl(pattern = "Usage: msconvert", x = msconvert_return))){
      stop("Command msconvert error, please check environment variables")
    }

    }
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

    shell.commomd <- paste0(msconvert,"  --filter \"peakPicking true 1-\" --mzML ",
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

