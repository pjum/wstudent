#' @title Get Proportional Table of Male and Female Students
#' @description  Get a table of proportions of female and male students who 
#' graduated with Latin honors in that class year
#' @usage ratio(input)
#' @param input Clean data frame
#' @return Proportional table of female and male students for that input
#' @import gender
#' @export
ratio <- function(input){
  
  if(!is.data.frame(input)) stop("input must be data frame")
  
  table.all <- table(input$gender)
  table.honor <- table(input[input$honor != "none",]$gender)
  
  table.honor/table.all
}


