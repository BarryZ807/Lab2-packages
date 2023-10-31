#' boxplot_byDRG
#'
#' Function returns a box plot of payments by DRG code, where the payment type is choosen.
#' @param data is the input dataset
#' @param payment_type is the type of payment chose
#' @param x_name is the name of the variable in x-axis
#'
#' @return a boxplot with DRX markers of \code{payment_type}
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal()
#' @importFrom rlang !
#' @importFrom tidyverse
#' @importFrom dplyr .data
#'
#' @examples
boxplot_byDRG <- function(data,payment_type,x_name){
  data <- data %>%
    mutate(DRG = substr(`DRG Definition`, start=1, stop=3))
  ggplot(mapping=aes(x=payment_type, fill=data$DRG)) +
    geom_boxplot() +
    guides(fill = guide_legend(nrows=13,ncol=7)) +
    labs(x = x_name, fill = "DRG Code") +
    ggtitle(label = cat("Boxplots of",x_name,"By DRG Code")) +
    theme_minimal()
}


#' stats_average_payment
#'
#' Function that calculates statistics over all of the DRG codes for average Medicare payments. variable \code{option}
#' @param data is the dataset
#' @param option is statistics types for mean, median, standard deviation
#'
#'
#' @return required statistics
#' @export
#'
#' @examples
#' df <- read.csv("DRG_data.csv")
#' stats_average_payment(df, 'mean')
stats_average_payment <- function(data, option) {
  ## Stop if option is not equal to either mean, median, or standard deviation
  if (!option %in%  c("mean", "median", "standard deviation")) {
    stop("Invalid input. Please input 'mean', 'median', or 'standard deviation'")
  }

  ## Return the statistic of the input
  if (option == 'mean') {
    ## When option == 'mean'
    mean = mean(data$Average.Medicare.Payments)
    return(round(mean, 2))
  }

  if (option == 'median') {
    ## When option == 'median'
    median = median(data$Average.Medicare.Payments)
    return(round(median, 2))
  }

  if (option == 'standard deviation') {
    ## When option == 'standard deviation'
    sd = sd(data$Average.Medicare.Payments)
    return(round(sd, 2))
  }
}
