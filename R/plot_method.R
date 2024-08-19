#' The plot of a FuzzyMonetary object
#' @description plot method for class "FuzzyMonetary"
#'
#' @param x An object of class "FuzzyMonetary"
#' @param ... Additional options
#' @import tidyr
#' @import ggplot2
#' @return The plot
#' @export
#'
#' @examples
#'
#' #The following example is based on the dataset eusilc
#' #included in the package.
#'
#'
#' #fm = "verma"
#'
#' index = fm_construct(predicate = eusilc$eq_income, weight = eusilc$DB090,
#'              fm = "verma", HCR = 0.154, ID = eusilc$ID)
#'
#' plot(index)

plot.FuzzyMonetary <- function(x,...){
  if(!is.null(x$fm)){
    if(x$fm == "verma") {
      FL.curve = fm_FL(x$results$predicate, x$results$weight)
      x.plot.data = data.frame(x$results, Lorenz = FL.curve$Lorenz, "WECDF" = FL.curve$WECDF)
      x.plot.data %>% dplyr::select(-weight) %>%
        tidyr::pivot_longer(cols = c(mu, Lorenz, WECDF), names_to = "curve", values_to = "value") %>%
        ggplot2::ggplot(ggplot2::aes(x = predicate, y = value, linetype = curve)) +
        ggplot2::geom_area(position = "identity", alpha = .1) +
        ggplot2::scale_linetype_manual(values = c("Lorenz" = "dashed", "mu" = "solid", "WECDF" = "dotted")) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                       legend.position = "bottom")

    } else if (x$fm == "verma1999") {
      FL.curve = fm_FL(x$results$predicate, x$results$weight)
      x.plot.data = data.frame(x$results, Lorenz = FL.curve$Lorenz)
      x.plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = predicate, y = Lorenz)) +
        ggplot2::geom_area(position = "identity", alpha = .1) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                       legend.position = "bottom")

    } else if (x$fm == "TFR") {
      FL.curve = fm_FL(x$results$predicate, x$results$weight)
      x.plot.data = data.frame(x$results, "WECDF" = FL.curve$WECDF)
      x.plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = predicate, y = WECDF)) +
        ggplot2::geom_area(position = "identity", alpha = .1) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                       legend.position = "bottom")
    } else if (x$fm == "belhadj2015") {
      par.df = data.frame(Parameters = names(x$parameters[-4]),
                          value = unlist(x$parameters[-4]),
                          y = c(0))
      par.lab = bquote(.(x$parameters[4]))
      ggplot2::ggplot(x$results, aes(x = predicate, y = mu)) +
        ggplot2::geom_line(linewidth = .8) +
        ggplot2::geom_area(alpha = .1) +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::geom_vline(aes(xintercept = value, colour = Parameters), data = par.df, linetype = "dashed") +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
              legend.position = "bottom")

    } else if (x$fm == "chakravarty" | x$fm == "cerioli" | x$fm == "belhadj2011"){
      par.df = data.frame(Parameters = names(x$parameters),
                          value = unlist(x$parameters),
                          y = c(0))
      ggplot2::ggplot(x$results, aes(x = predicate, y = mu)) +
        geom_area(alpha = .1) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::geom_vline(aes(xintercept = value, colour = Parameters), data = par.df, linetype = "dashed") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
              legend.position = "bottom")

    } else if (x$fm == "ZBM") {
      par.df <- data.frame(Parameters = names(x$parameters),
                          value = unlist(x$parameters),
                          y = c(0))
      par.df <- par.df[par.df$Parameters %in% c("a", "b", "c"),]

      data.frame( predicate = x$results$predicate,
                  mu= FN(x$results$predicate,
                         a=x$parameters$a,
                         b=x$parameters$b,
                         c=x$parameters$c)) %>%
        dplyr::add_row(
          predicate = x$parameters$a,
          mu = 0
        ) %>%
        dplyr::add_row(
          predicate = x$parameters$a,
          mu = 1
        ) %>%
        dplyr::add_row(
          predicate = x$parameters$b,
          mu = 1
        ) %>%
        dplyr::add_row(
          predicate = x$parameters$c,
          mu = 0
        ) %>%
        ggplot2::ggplot( aes(x = predicate, y = mu)) +
        ggplot2::geom_area(alpha = .1) +
        ggplot2::geom_line(linewidth = .8) +
        ggplot2::scale_x_continuous("Predicate", labels = function(x) format(x, scientific = FALSE), n.breaks = 6) +
        ggplot2::scale_y_continuous(expression(mu)) +
        ggplot2::geom_vline(aes(xintercept = value, colour = Parameters), data = par.df, linetype = "dashed") +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
              legend.position = "bottom")
    }
  } else { # variance plot section


    if(!("size" %in% names(x))) stop("no plot method if breakdown was not specified")
    data.frame(x$variance) %>%
      dplyr::mutate(Breakdown = row.names(.),
                    Size = c(x$size)) %>%
      tidyr::pivot_longer(!c(Size, Breakdown), names_to = "Dimension", values_to = "Variance") %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(Breakdown, Size) , y = Variance, group = 1)) +
      ggplot2::geom_line() + ggplot2::geom_point() + ggplot2::geom_area(alpha = .1) +
      # geom_bar(stat = "identity", alpha = .7, color = "black") +
      ggplot2::scale_x_discrete("Breakdown (sorted by ascending sample size)") +
      ggplot2::theme_minimal()

    }

}
