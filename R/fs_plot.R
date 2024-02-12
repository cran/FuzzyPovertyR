#' The plot of a FuzzySupplementary object
#' @description plot method for class "FuzzySupplementary"
#'
#' @param x An object of class "FuzzySupplementary"
#' @param ... Additional options
#' @import tidyr
#' @import ggplot2
#' @import dplyr
#'
#' @return The plot
#' @export
#'

plot.FuzzySupplementary <- function(x,...){
  if(names(x)[1]=="step2") {
    plot(x$step2)
    } else if(names(x)[1]=="steps4_5") {

#     x$steps4_5 %>%
#       dplyr::ungroup() %>%
#       dplyr::select(Item, Dimension, w) %>%
#       dplyr::distinct() %>%
#       ggplot2::ggplot(ggplot2::aes(x = Dimension, y = w, fill = reorder(Item, Dimension))) +
#       ggplot2::geom_bar(stat = "identity", position = "fill", color = "black") +
#       # facet_wrap(~Dimension) +
#       ggplot2::theme_minimal() + ggplot2::guides(fill = ggplot2::guide_legend("Items")) +
#       ggplot2::theme(legend.position = "bottom", axis.title.y = element_blank())
#
# #
#     cat("press Enter to move to the next plot")
#     readline()

    x$steps4_5 %>%
      dplyr::group_by(Item, Dimension) %>%
      dplyr::summarise(Mean = mean(s)) %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(Item, Dimension), y = Mean)) +
      ggplot2::geom_bar(ggplot2::aes(fill = factor(Dimension)), color = "black", stat = "identity", alpha = .7) +
      ggplot2::geom_line(ggplot2::aes(x = Item, y = value, group = weight, linetype = weight),
                         data = x$steps4_5 %>%
                           dplyr::group_by(Item) %>%
                           dplyr::summarise(
                             w = mean(w),
                             w_a = mean(w_a),
                             w_b = mean(w_b)) %>%
                           tidyr::pivot_longer(cols = c(w, w_a, w_b), names_to = "weight", values_to = "value")) +
      ggplot2::theme_minimal() +
      ggplot2::scale_linetype_manual(values = c("solid", "dotted", "dashed"), labels = expression(w, w[a], w[b])) +
      ggplot2::scale_x_discrete("Item") + ggplot2::guides(fill = ggplot2::guide_legend("Dimensions")) +
      ggplot2::theme(axis.text.x = element_text(angle = 90),
                     axis.title.y = element_blank(),
                     legend.position = "bottom")


  } else if(names(x)[1]=="membership"){

    plot.data <- lapply(x$membership,
                        function(x)
                        {if("breakdown" %in% colnames(x)) x <- subset(x, select = -breakdown);
                        colnames(x) = c("ID", "s", "weight", "mu");
                        return(x)}
    )
    P <- length(plot.data)
    plot.data <- do.call(rbind, plot.data)
    Dimension  <- paste0("FS", rep(c(1:(P-1), ": Overall"), each = nrow(plot.data)/P))
    Dimension[Dimension=="FS: Overall"]="Overall"

    plot.data$Dimension <- Dimension

    plot.data %>%
      ggplot2::ggplot(ggplot2::aes(x = s, y = mu)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~Dimension) +
      ggplot2::geom_area(alpha = .1) +
      ggplot2::scale_y_continuous(expression(mu)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                     legend.position = "bottom")

  } else if(names(x)[1]=="variance"){
    if(!("size" %in% names(x))) stop("no plot method if breakdown was not specified")
    data.frame(x$variance) %>%
      dplyr::mutate(Breakdown = row.names(.),
                    Size = c(x$size)) %>%
      tidyr::pivot_longer(!c(Size, Breakdown), names_to = "Dimension", values_to = "Variance") %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(Breakdown, Size) , y = Variance, group = 1)) +
      ggplot2::facet_wrap(~Dimension) +
      ggplot2::geom_line() + ggplot2::geom_point() + ggplot2::geom_area(alpha = .1) +
      # geom_bar(stat = "identity", alpha = .7, color = "black") +
      ggplot2::scale_x_discrete("Breakdown (sorted by ascending sample size)") +
      ggplot2::theme_minimal()
  }

}
