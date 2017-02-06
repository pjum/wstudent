#' @title Clean the Data Frame
#' @description Filters out non-name elements, divides and combines columns to get 
#' a clean data frame. Gender column is also added.
#' @usage cleanData(input)
#' @param input Unclean data frame from getHonorName()
#' @return Clean version of input with "firstname", "mid/lastname", "honor", 
#' and "gender" column.
#' @import stringr
#' @export
cleanData <- function(input){
  
  honor = as.character(input$honor[1])
  
  #is the data is clean?
  flag = 0
  
  if ("dat_honor" %in% colnames(input)){
    input <- as.vector(input$dat_honor)
  } else if ("firstname" %in% colnames(input)){
    #data is clean (dataset)
    flag = 1
    allNames <- input
  }

  
  if (flag != 1){
    # test for filtering out non-name elements
    test1 <- grepl("bachelor of", input, ignore.case = TRUE)
    test2 <- grepl("phi beta kappa", input, ignore.case = TRUE)
    test3 <- grepl("sigma xi", input, ignore.case = TRUE)
    test4 <- grepl("___", input)
    test5 <- grepl("degrees conferred", input, ignore.case = TRUE)
    test6 <- grepl("\\d", input)
    test7 <- !grepl("\\s", input)

    #check for illegal degree suffix
    test8 <- sapply(input, isIllegalSuffix, USE.NAMES = FALSE)

    #combine all tests into one big test
    bigTest <- test1|test2|test3|test4|test5|test6|test7|test8

    #Keep all but non-name elements
    output <- input[!bigTest]

    #Manual manipulation for some text files
    clean <- which(grepl("Statistical", output, ignore.case = TRUE))
    if (!identical(clean, integer(0))) output <- output[-clean]

    #Clean data
    allNames <- stringr::str_split(output, ",", simplify = TRUE)
    allNames <- allNames[,1]
    allNames <- stringr::str_split_fixed(allNames, " ", 2)
    allNames[,1] <- stringr::str_replace_all(allNames[,1], "[^[:alnum:]]", "")
  
  
    #Determine genders
    g <- matrix(nrow = nrow(allNames), ncol = 1)
    for(i in 1:nrow(g)){
      if (!identical(gender::gender(allNames[i,1]), gender::gender("ff"))){
        g[i,1] <- gender::gender(allNames[i,1])$gender
      }
    }
  
    allNames <- data.frame(allNames, honor, g)
    colnames(allNames) <- c("firstname", "mid/lastname", "honor", "gender")
    }#end if
  
  allNames
}

#' @title Check Suffix in Line
#' @description  Check if the line contains any suffix listed in the text
#' file 'suffix.txt'
#' @param x a text line
#' @return returns TRUE if the line contains any suffix
#'   listed in the text file 'suffix.txt'
#' @usage checkSuffix(x)
#' @import stringr
#' @keywords internal
checkSuffix <- function(x){
  match <- FALSE
  file_path <- system.file("extdata", "suffix.txt", package = "wstudent")
  majorSuffix <- readLines(file_path, ok = TRUE, warn = FALSE)
  for (i in 1:length(majorSuffix)){
    if (grepl(majorSuffix[i], x, ignore.case = TRUE)) {
      match <- TRUE
    }
  }
  match
}

#' @title Check Illegal Suffix in Line
#' @description Check if a line illegally contains any suffix: containing
#'   a suffix but not ',' within the same line.
#' @usage isIllegalSuffix(x)
#' @param x text line
#' @return returns TRUE if the line illegally contains a suffix
#' @import stringr
#' @keywords internal
isIllegalSuffix <- function(x){
  illegal <- FALSE
  if((!grepl(",", x)) & checkSuffix(x)){
    illegal <- TRUE
  }
  illegal
}
