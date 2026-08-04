#' get_ThermoRawMetaDump
#' @describeIn ThermoRawMetaDump path to ThermoRawMetaDump.exe under pwiz
#' @export
#'
get_ThermoRawMetaDump <- function(){

  ### locate ThermoRawMetaDump.exe under package pwiz
  {
    pkg.dir <- system.file(package = "MSconvertR")
    ThermoRawMetaDump.path <- dir(pkg.dir,
                                  pattern = "ThermoRawMetaDump\\.exe$",
                                  recursive = TRUE,
                                  full.names = TRUE)
    ThermoRawMetaDump.path <- ifelse(length(ThermoRawMetaDump.path) == 0,
                                     NA_character_,
                                     ThermoRawMetaDump.path[1])
    return(ThermoRawMetaDump.path)
  }

}


#' Parse Easy-nLC style gradient table from method text
#' @noRd
#'
.parse_ThermoRaw_LC_gradient_table <- function(method.lines){

  ### find gradient table header
  {
    header.idx <- which(
      grepl("time", method.lines, ignore.case = TRUE) &
        grepl("%\\s*B|percent\\s*B|mixture", method.lines, ignore.case = TRUE, perl = TRUE)
    )
    if (length(header.idx) == 0) {
      grad.idx <- which(grepl("^\\s*gradient\\s*$", method.lines, ignore.case = TRUE))
      if (length(grad.idx) > 0) {
        after <- method.lines[seq.int(grad.idx[1] + 1L, length(method.lines))]
        header.idx <- which(
          grepl("time", after, ignore.case = TRUE) &
            grepl("%\\s*B|percent\\s*B|mixture|flow", after, ignore.case = TRUE, perl = TRUE)
        )
        if (length(header.idx) > 0)
          header.idx <- grad.idx[1] + header.idx[1]
      }
    }
    if (length(header.idx) == 0)
      return(NULL)
    header.line <- method.lines[header.idx[1]]
  }

  ### map header columns
  {
    header.clean <- gsub("\\([^)]*\\)", "", header.line)
    header.clean <- gsub("\\[|\\]", " ", header.clean)
    header.tokens <- strsplit(trimws(header.clean), "\\s+")[[1]]
    header.tokens <- header.tokens[nzchar(header.tokens)]
    col.time <- which(grepl("^time", header.tokens, ignore.case = TRUE))[1]
    col.flow <- which(grepl("^flow", header.tokens, ignore.case = TRUE))[1]
    col.B <- which(grepl("%\\s*B|percent\\s*B|^mixture$|^B$", header.tokens,
                         ignore.case = TRUE, perl = TRUE))[1]
    if (is.na(col.time) || is.na(col.B))
      return(NULL)
  }

  ### read numeric gradient rows after header
  {
    rows <- list()
    if (header.idx[1] >= length(method.lines))
      return(NULL)
    for (line in method.lines[seq.int(header.idx[1] + 1L, length(method.lines))]) {
      if (!nzchar(trimws(line))) {
        if (length(rows) > 0) break
        next
      }
      if (grepl("^(====|sample/file|tune data|instrument methods|creationdate)",
                trimws(line), ignore.case = TRUE))
        break
      nums <- strsplit(trimws(gsub(",", "", line)), "\\s+")[[1]]
      nums <- nums[nzchar(nums)]
      if (!all(grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", nums))) {
        if (length(rows) > 0) break
        next
      }
      vals <- as.numeric(nums)
      if (length(vals) < max(col.time, col.B, na.rm = TRUE)) {
        if (length(rows) > 0) break
        next
      }
      rows[[length(rows) + 1L]] <- data.frame(
        time = vals[col.time],
        flow = if (!is.na(col.flow) && length(vals) >= col.flow)
          vals[col.flow] else NA_real_,
        percentB = vals[col.B],
        curve = NA_real_,
        stringsAsFactors = FALSE
      )
    }
    if (length(rows) == 0)
      return(NULL)
    return(do.call(rbind, rows))
  }

}


#' Parse Chromeleon/Vanquish PumpModule timed script gradient
#' @noRd
#'
.parse_ThermoRaw_LC_gradient_script <- function(method.lines){

  ### collect timed PumpModule.%B.Value steps
  {
    time.pat <- "^\\s*([0-9]*\\.?[0-9]+)\\s*\\[min\\]"
    time.idx <- which(grepl(time.pat, method.lines, perl = TRUE))
    if (length(time.idx) == 0)
      return(NULL)

    rows <- list()
    for (i in seq_along(time.idx)) {
      i0 <- time.idx[i]
      i1 <- if (i < length(time.idx)) time.idx[i + 1L] - 1L else length(method.lines)
      block <- method.lines[i0:i1]
      b.line <- grep("PumpModule[^\\s]*\\.%B\\.Value\\s*:", block, value = TRUE, perl = TRUE)[1]
      if (is.na(b.line))
        next
      time <- as.numeric(sub(paste0(time.pat, ".*"), "\\1", method.lines[i0], perl = TRUE))
      percentB <- as.numeric(sub(".*:\\s*([-+]?[0-9]*\\.?[0-9]+).*", "\\1", b.line, perl = TRUE))
      flow.line <- grep("PumpModule[^\\s]*\\.Flow\\.Nominal\\s*:", block, value = TRUE, perl = TRUE)[1]
      flow <- if (!is.na(flow.line))
        as.numeric(sub(".*:\\s*([-+]?[0-9]*\\.?[0-9]+).*", "\\1", flow.line, perl = TRUE))
      else NA_real_
      curve.line <- grep("PumpModule[^\\s]*\\.Curve\\s*:", block, value = TRUE, perl = TRUE)[1]
      curve <- if (!is.na(curve.line))
        as.numeric(sub(".*:\\s*([-+]?[0-9]*\\.?[0-9]+).*", "\\1", curve.line, perl = TRUE))
      else NA_real_
      rows[[length(rows) + 1L]] <- data.frame(
        time = time,
        flow = flow,
        percentB = percentB,
        curve = curve,
        stringsAsFactors = FALSE
      )
    }
    if (length(rows) == 0)
      return(NULL)
    return(do.call(rbind, rows))
  }

}


#' Parse LC gradient from Thermo instrument method text
#' @noRd
#'
.parse_ThermoRaw_LC_gradient <- function(method.lines){

  ### try Easy-nLC table, then Chromeleon timed script
  {
    grad <- .parse_ThermoRaw_LC_gradient_table(method.lines)
    if (!is.null(grad))
      return(grad)
    return(.parse_ThermoRaw_LC_gradient_script(method.lines))
  }

}


#' get_ThermoRaw_LC_gradient
#' @describeIn ThermoRawMetaDump extract LC gradient from Thermo .raw via ThermoRawMetaDump.exe
#' @param raw.files Thermo .raw file path(s)
#' @return data.frame with columns time, flow, percentB, curve (named list if multiple files)
#' @export
#'
get_ThermoRaw_LC_gradient <- function(raw.files){

  ### pre
  {
    MSConvert_require_ready()
    if (!MSConvert_is_linux()) {
      ThermoRawMetaDump <- get_ThermoRawMetaDump()
      if (is.na(ThermoRawMetaDump) || !file.exists(ThermoRawMetaDump))
        stop("ThermoRawMetaDump.exe not found under package pwiz")
    }
    raw.files <- gsub(pattern = "\\", x = raw.files, replacement = "/", fixed = TRUE) %>%
      na.omit() %>%
      as.character()
    if (length(raw.files) == 0)
      stop("No raw files provided")
    if (!any(file.exists(raw.files)))
      stop(paste0("File not found : ",
                  sum(!file.exists(raw.files)), "/", length(raw.files)))
    empty.grad <- data.frame(
      time = numeric(0),
      flow = numeric(0),
      percentB = numeric(0),
      curve = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  ### run ThermoRawMetaDump and parse LC gradient
  {
    out <- lapply(raw.files, function(raw.file) {
      raw.file <- normalizePath(raw.file, winslash = "/", mustWork = TRUE)
      if (MSConvert_is_linux()) {
        shell.command <- MSConvert_build_cmd(
          tool = "ThermoRawMetaDump.exe",
          args = shQuote(paste0("/data/in/", basename(raw.file))),
          in_dir = dirname(raw.file)
        )
      } else {
        shell.command <- MSConvert_build_cmd(
          tool = "ThermoRawMetaDump.exe",
          args = shQuote(raw.file),
          in_dir = dirname(raw.file)
        )
      }
      dump.lines <- suppressWarnings(
        try(system(shell.command, intern = TRUE), silent = TRUE)
      )
      if (inherits(dump.lines, "try-error") || length(dump.lines) == 0) {
        warning("ThermoRawMetaDump failed for: ", raw.file)
        return(empty.grad)
      }
      grad <- .parse_ThermoRaw_LC_gradient(dump.lines)
      if (is.null(grad)) {
        warning("LC gradient table not found in: ", raw.file)
        return(empty.grad)
      }
      grad[, c("time", "flow", "percentB", "curve")]
    })
    names(out) <- basename(raw.files)
    if (length(out) == 1)
      return(out[[1]])
    return(out)
  }

}
