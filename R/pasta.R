#' @title Lasagna plot (Updated from bruce's)
#'
#' @description Generates a good looking plot #pasta
#'
#' @param ob which row to plot - will change to make more general
#' @param data data frame to plot
#'
#' @return A lasagna plot
#'

lasagna_mc <- function(ob, data){
        x <- data[,c(ob,37 + ob, 37 * 2 + ob)] %>%
                as.matrix()
        x <- x[do.call(order, c(decreasing = TRUE, data.frame(x[,1:3]))),]
        x %>%
                lasagnar::lasagna(legend = T, col = brewer.pal(5,"Greens"),
                                  yaxis=FALSE, xlab = "Survey Number",
                                  main = paste("Q:", ob, " ", data_dic[ob,2], sep = ""),
                                  ylab = "Number of Patients at Baseline")
        label_spots <- cumsum(c(mean(x[,1] == 1),
                                mean(x[,1] == 2),
                                mean(x[,1] == 3),
                                mean(x[,1] == 4),
                                mean(x[,1] == 5))) -
                c(mean(x[,1] == 1)/2,
                  mean(x[,1] == 2)/2,
                  mean(x[,1] == 3)/2,
                  mean(x[,1] == 4)/2,
                  mean(x[,1] == 5)/2)
        axis(2, label_spots, c(sum(x[,1] == 1),
                               sum(x[,1] == 2),
                               sum(x[,1] == 3),
                               sum(x[,1] == 4),
                               sum(x[,1] == 5)),las=2)
        mtext("Question Response", side = 4, line = 4.75, adj = 0.5)
}
