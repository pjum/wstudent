#' @title Get Section Indicated by Latin Honors
#' @description This function works with .txt file in the package. It picks the 
#' elements that are marked with indicated Latin honors. 
#' @usage getHonorName(file_name, honor)
#' @param file_name Name of a text file in inst/extdata or a datasets in a package. 
#'     See details for file name convention.
#' @param honor Type of Latin honor from c("all", "summa", "magna", "cum", "none").
#'     Pick 'all' for graduates with 'Cum Laude and above.'
#'     Pick 'summa' for graduates with 'Summa Cum Laude.'
#'     Pick 'magna' for graduates with 'Magna Cum Laude.'
#'     Pick 'cum' for graduates with 'Cum Laude.'
#'     Pick 'none' for graduates without Latin honors.
#' @return Data frame of elements with indicated Latin honor.
#' @details files in the package are saved as\cr
#'    "t0203.txt" for class of 2003, \cr
#'    "t0304.txt" for class of 2004, \cr
#'    .\cr
#'    .\cr
#'    .\cr
#'    "t1516.txt" for class of 2016\cr
#'    
#'    file_name can also be datasets in package such as wstudent.three or wstudent.ten
#'    
#' @import stringr
#' @export
getHonorName <- function(file_name, honor){
  
  honor <- match.arg(honor, c("all", "summa", "magna", "cum", "none"), several.ok = FALSE)
  #is input a file name?
  flag = 0
  
  if (is.character(file_name)){
    f_path <- system.file("extdata", file_name, package = "wstudent")
    dat <- readLines(f_path, ok = TRUE, warn = FALSE, skipNul = TRUE)
    flag = 1
  } else if(!is.data.frame(file_name)) stop("recheck input")

  # get indices of the zone
  if(honor == "summa"){
    honor <- "Summa Cum Laude"
    if (flag == 1){
      bp_begin <- which(grepl("Bachelor of Arts, Summa Cum Laude", dat, ignore.case = TRUE))
      bp_end <- which(grepl("Bachelor of Arts, Magna Cum Laude", dat, ignore.case = TRUE))
    }
  } else if (honor == "magna"){
    honor <- "Magna Cum Laude"
    if (flag == 1){
      bp_begin <- which(grepl("Bachelor of Arts, Magna Cum Laude", dat, ignore.case = TRUE))
      bp_end <- which(grepl("Bachelor of Arts, Cum Laude", dat, ignore.case = TRUE))
    }
  } else if (honor == "cum"){
    honor <- "Cum Laude"
    if (flag == 1){
      bp_begin <- which(grepl("Bachelor of Arts, Cum Laude", dat, ignore.case = TRUE))
      bp_end <- which(grepl("Bachelor of Arts", dat, ignore.case = TRUE) &
                        !grepl(",", dat, ignore.case = TRUE))
    }
  } else if (honor == "all"){
    honor <- "Latin Honor"
    if (flag == 1){
      bp_begin <- which(grepl("Bachelor of Arts, Summa Cum Laude", dat, ignore.case = TRUE))
      bp_end <- which(grepl("Bachelor of Arts", dat, ignore.case = TRUE) &
                        !grepl(",", dat, ignore.case = TRUE))
    }
  } else if (honor == "none"){
    if (flag == 1){
      bp_begin <- which(grepl("Bachelor of Arts", dat, ignore.case = TRUE) &
                          !grepl(",", dat, ignore.case = TRUE))
      bp_end <- length(dat)
    }
  }

  if(flag == 1){
    dat_honor <- dat[bp_begin:bp_end]
    dat_honor <- data.frame(dat_honor, honor)
  } else {
    dat_honor <- file_name[which(file_name$honor == honor),]
  }
  dat_honor
}
