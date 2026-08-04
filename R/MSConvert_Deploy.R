
MSConvert_get_dir <- function(){

  pkg.dir <- system.file(package = "MSconvertR")
  msconvert.path <- dir(pkg.dir,pattern = "msconvert.exe$",recursive = T,full.names = T)
  msconvert.path <- ifelse(length(msconvert.path)==0,NA,msconvert.path)
  return(msconvert.path)
}


MSConvert_is_linux <- function(){
  tolower(Sys.info()[["sysname"]]) == "linux"
}


MSConvert_pwiz_docker_image <- function(){
  "proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses"
}


#' MSConvert_docker_available
#' @describeIn MSConvert check if Docker pwiz image is available on Linux
#' @export
#'
MSConvert_docker_available <- function(image = MSConvert_pwiz_docker_image()){

  if (nchar(Sys.which("docker")) == 0) {
    message("Docker not found on PATH")
    return(FALSE)
  }

  docker_ok <- try(
    system2("docker", args = c("info"), stdout = TRUE, stderr = TRUE),
    silent = TRUE
  )
  if (inherits(docker_ok, "try-error") ||
      (!is.null(attr(docker_ok, "status")) && attr(docker_ok, "status") != 0)) {
    message("Docker is installed but not usable (daemon not running or no permission?)")
    return(FALSE)
  }

  img_id <- try(
    system2("docker", args = c("images", "-q", image),
            stdout = TRUE, stderr = TRUE),
    silent = TRUE
  )
  if (inherits(img_id, "try-error") || length(img_id) == 0 || !nzchar(img_id[1])) {
    message("Docker pwiz image not found locally: ", image)
    return(FALSE)
  }

  message("Docker pwiz image available: ", image, " (", img_id[1], ")")
  return(TRUE)
}


MSConvert_require_ready <- function(){

  if (MSConvert_is_linux()) {
    if (!MSConvert_docker_available()) {
      stop("Docker pwiz image not available. Run MSConvert_check() to pull it.")
    }
    return(invisible(TRUE))
  }

  msconvert <- MSConvert_get_dir()
  if (is.na(msconvert) || !file.exists(msconvert)) {
    stop("msconvert.exe not found. Run MSConvert_check() / MSConvert_Deploy().")
  }
  return(invisible(TRUE))
}


#' Build a ProteoWizard tool command for the current OS
#' @param tool Tool name (e.g. "msconvert", "ThermoRawMetaDump.exe")
#' @param args Character vector of CLI arguments (host paths already remapped for Linux)
#' @param in_dir Host directory to mount at /data/in (Linux)
#' @param out_dir Host directory to mount at /data/out (Linux); optional
#' @noRd
#'
MSConvert_build_cmd <- function(tool, args, in_dir, out_dir = NULL){

  args_str <- paste(args, collapse = " ")

  if (!MSConvert_is_linux()) {
    if (identical(tool, "msconvert")) {
      exe <- MSConvert_get_dir()
    } else {
      exe <- file.path(dirname(MSConvert_get_dir()), tool)
    }
    if (is.na(exe) || !file.exists(exe)) {
      stop(tool, " not found. Run MSConvert_check() / MSConvert_Deploy().")
    }
    return(paste(shQuote(exe), args_str))
  }

  in_dir <- normalizePath(in_dir, winslash = "/", mustWork = TRUE)
  mounts <- paste0("-v ", shQuote(in_dir), ":/data/in")
  if (!is.null(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_dir <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)
    mounts <- paste(mounts, paste0("-v ", shQuote(out_dir), ":/data/out"))
  }

  paste(
    "docker run --rm",
    mounts,
    MSConvert_pwiz_docker_image(),
    "wine", tool,
    args_str
  )
}


#' Build an msconvert command for one file
#' @param raw.file Input raw/wiff path
#' @param out.dir Output directory
#' @param outfile Optional output file path or name
#' @param format.to Output format (mzML, mzXML, ...)
#' @noRd
#'
MSConvert_build_msconvert_cmd <- function(raw.file,
                                          out.dir,
                                          outfile = NULL,
                                          format.to = "mzML"){

  raw.file <- normalizePath(raw.file, winslash = "/", mustWork = TRUE)
  dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
  out.dir <- normalizePath(out.dir, winslash = "/", mustWork = TRUE)

  if (MSConvert_is_linux()) {
    container_raw <- paste0("/data/in/", basename(raw.file))
    args <- c(
      "--ignoreUnknownInstrumentError",
      "--filter \"peakPicking true 1-\"",
      paste0("--", format.to),
      shQuote(container_raw),
      "-o", "/data/out"
    )
    if (!is.null(outfile)) {
      args <- c(args, "--outfile", shQuote(basename(outfile)))
    }
    return(MSConvert_build_cmd(
      tool = "msconvert",
      args = args,
      in_dir = dirname(raw.file),
      out_dir = out.dir
    ))
  }

  args <- c(
    "--ignoreUnknownInstrumentError",
    "--filter \"peakPicking true 1-\"",
    paste0("--", format.to),
    shQuote(raw.file),
    "-o", shQuote(out.dir)
  )
  if (!is.null(outfile)) {
    args <- c(args, "--outfile", shQuote(outfile))
  }
  MSConvert_build_cmd(
    tool = "msconvert",
    args = args,
    in_dir = dirname(raw.file),
    out_dir = out.dir
  )
}


#' MSConvert_check
#' @describeIn MSConvert check if MSconvert ready
#' @export
#'
MSConvert_check <- function(){

  os <- tolower(Sys.info()[["sysname"]])
  message("System: ", os)

  ### Linux: require Docker pwiz
  if (MSConvert_is_linux()) {
    if (MSConvert_docker_available()) {
      return(invisible(TRUE))
    }
    message("Pull Docker pwiz image?\n 1:yes, 2:no")
    x <- readline()
    if (identical(x, "1")) {
      image <- MSConvert_pwiz_docker_image()
      status <- system2("docker", args = c("pull", image))
      if (!identical(status, 0L)) {
        stop("Failed to pull Docker image: ", image)
      }
      return(invisible(MSConvert_docker_available()))
    }
    return(invisible(FALSE))
  }

  ### Windows (and other): local msconvert.exe
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
