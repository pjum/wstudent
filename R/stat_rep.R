#' @title Show Statistical Representations
#' @description Present statistical representations of the proportions of female and
#'     male Williams students who graduated with Latin honors in each year from 2003
#'     to 2016. The function uses internal datasets in the package. The datasets are 
#'     dataframes with students' name, genders, and Latin honors they received. Using 
#'     provided datasets saves computation time.
#'
#'     Five options are available: five-number summary, box plot, hypothesis test, 
#'     time plot, and annual reports.
#'
#' @usage stat_rep(type)
#' @param type Type of statistical representation to be shown.
#'     Pick "prop.sum" for five-number summary.
#'     Pick "boxplot" for box plot.
#'     Pick "t.testing" for hypothesis test.
#'     Pick "timeplot" for proportions over time.
#'     Pick "annual" for annual information.
#' @return Statistical representation according to specified type
#' @import graphics stats utils ggplot2
#' @export
stat_rep <- function(type){
  
  type <- match.arg(type, c("prop.sum", "timeplot", "boxplot", "t.testing", "annual"),
                    several.ok = FALSE)
  
  #import dataset
  data(all.ratio, envir = environment())

  if(type == "prop.sum"){
    
    #1. Summary
    sapply(all.ratio[,-1], summary)
    
  } else if (type == "boxplot"){

    #2. Box plot
    # boxplot: showing the mean of male proportion is less than of female proportion
    graphics::boxplot(all.ratio$female, all.ratio$male, names = c("female", "male"),
            col = c("violetred2", "limegreen"),
            ylab = "Proportion", main = "Comparision of the Proportions")

  } else if (type == "t.testing"){

    #3. t-testing
    stats::t.test(all.ratio$female, all.ratio$male, "greater")

  } else if (type == "timeplot"){

    #4. time plot
    #year <- c(2003:2016)
    #dat <- data.frame(year, frame)
    ggplot2::ggplot(data = all.ratio) +
      ggplot2::geom_point(ggplot2::aes(x = all.ratio$classyear, y = all.ratio$female, col = "female")) +
      ggplot2::geom_line(ggplot2::aes(x = all.ratio$classyear, y = all.ratio$female), col = "violetred2") +
      ggplot2::geom_point(ggplot2::aes(x = all.ratio$classyear, y= all.ratio$male, col ="male")) +
      ggplot2::geom_line(ggplot2::aes(x = all.ratio$classyear, y= all.ratio$male), col = "limegreen") +
      ggplot2::scale_color_manual(name = "Gender", values  = c("violetred2","limegreen")) +
      ggplot2::ylim(c(0.25,0.45)) + ggplot2::ggtitle("Porportions Over Time") +
      ggplot2::labs( x = "Year", y = "Proportion")

  }  else if (type == "annual"){
    
    #import datasets
    data( wstudent.three, wstudent.four, wstudent.five, wstudent.six,
          wstudent.seven, wstudent.eight, wstudent.nine, wstudent.ten,
          wstudent.eleven, wstudent.twelve, wstudent.thirteen, 
          wstudent.fourteen, wstudent.fifteen, wstudent.sixteen, 
          envir = environment())
    
    list(stats::addmargins(table(wstudent.three[,3:4], 
                                 dnn = list("honor", "class of 2003 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.four[,3:4], 
                                 dnn = list("honor", "class of 2004 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.five[,3:4], 
                                 dnn = list("honor", "class of 2005 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.six[,3:4], 
                                 dnn = list("honor", "class of 2006 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.seven[,3:4], 
                                 dnn = list("honor", "class of 2007 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.eight[,3:4], 
                                 dnn = list("honor", "class of 2008 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.nine[,3:4], 
                                 dnn = list("honor", "class of 2009 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.ten[,3:4], 
                                 dnn = list("honor", "class of 2010 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.eleven[,3:4], 
                                 dnn = list("honor", "class of 2011 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.twelve[,3:4], 
                                 dnn = list("honor", "class of 2012 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.thirteen[,3:4], 
                                 dnn = list("honor", "class of 2013 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.fourteen[,3:4], 
                                 dnn = list("honor", "class of 2014 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.fifteen[,3:4], 
                                 dnn = list("honor", "class of 2015 gender"))[-4,], 1),
         stats::addmargins(table(wstudent.sixteen[,3:4], 
                                 dnn = list("honor", "class of 2016 gender"))[-4,], 1))
  }
}



