#' @title Add to Plot List
#' @description Adds one or more plots to an existing plot list for later export to PowerPoint.
#' @param plot_list A list to add plots to.
#' @param plot_or_list A plot or list of plots to add.
#' @return An updated plot list.
#' @export
add_to_plot_list <- function(plot_list, plot_or_list) {
  # Check if 'plot_or_list' is a single plot or a list of plots
  if (inherits(plot_or_list, "ggplot")) {
    # If it's a single plot, append it to the list
    plot_list[[length(plot_list) + 1]] <- plot_or_list
  } else if (is.list(plot_or_list)) {
    # If it's a list of plots, append all the plots to the list
    for (plot in plot_or_list) {
      plot_list[[length(plot_list) + 1]] <- plot
    }
  } else {
    stop("Input must be either a ggplot object or a list of ggplot objects.")
  }

  return(plot_list)  # Return the updated list
}