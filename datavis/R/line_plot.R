#' @title Line Plot
#' @description Creates a line chart from survey data.
#' @param data_source A data frame loaded from a Qualtrics CSV.
#' @param variable_groups A list of character vectors of column names to plot.
#' @param title Plot title.
#' @param group_labels Character vector of labels for each group.
#' @param compare_rows Logical. Whether to compare row groups.
#' @param rows1 Row indices for group 1.
#' @param rows2 Row indices for group 2.
#' @param rows3 Row indices for group 3. Default NULL.
#' @param rows4 Row indices for group 4. Default NULL.
#' @param rows5 Row indices for group 5. Default NULL.
#' @param csv_rows Logical. Whether rows are original CSV row numbers.
#' @param filter_by_variable Variable name to filter by.
#' @param filter_values List of values to filter for each group.
#' @param y_range Numeric vector of length 2 for the y-axis range.
#' @param chunk_size Number of variables per plot chunk.
#' @param title_width Integer for title wrapping width.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param subtitle Plot subtitle.
#' @param subtitle2 Second plot subtitle.
#' @param colors Custom color palette.
#' @param show_points Logical. Whether to show points on the line.
#' @param show_error_bars Logical. Whether to show error bars.
#' @param show_labels Logical. Whether to show value labels.
#' @param show_y_labels Logical. Whether to show y-axis labels.
#' @param show_legend Logical. Whether to show the legend.
#' @param legend_stack Logical. Whether to stack legend items vertically.
#' @param angle_labels Logical. Whether to angle x-axis labels.
#' @return A list of ggplot objects.
#' @export
line_plot <- function(data_source,
                      variable_groups,
                      title,
                      group_labels = c("Version 1", "Version 2", "Version 3", "Version 4"),
                      # Row comparison parameters
                      compare_rows = FALSE,
                      rows1 = NULL,
                      rows2 = NULL,
                      rows3 = NULL,
                      rows4 = NULL,
                      rows5 = NULL,
                      csv_rows = TRUE,  # if TRUE, subtracts 3 from row numbers
                      # NEW: Automatic row filtering parameters
                      filter_by_variable = NULL,  # Variable name to filter by (e.g., "Q8")
                      filter_values = NULL,       # List of values to filter for each group
                      # Range and chunking
                      y_range = c(1, 5),
                      chunk_size = 5,
                      # Text parameters
                      title_width = 40,
                      x_label = NULL,
                      y_label = NULL,
                      subtitle = NULL,
                      subtitle2 = NULL,
                      # Style parameters
                      colors = c("#4f74dd", "#efac51", "#d24d77", "#5db693", "#bd73b0"),
                      show_points = TRUE,
                      show_error_bars = TRUE,
                      show_labels = TRUE,
                      show_y_labels = TRUE,
                      show_legend = TRUE,
                      legend_stack = TRUE,
                      angle_labels = FALSE) {

  # Process raw dataframe
  processed <- process_data_with_labels(data_source)
  data <- processed$data
  label_mapping <- processed$labels
  
  # NEW: Automatic row filtering based on filter_by_variable
  # MUST happen BEFORE csv_rows adjustment
  auto_filtered <- FALSE
  if (!is.null(filter_by_variable) && !is.null(filter_values)) {
    if (!filter_by_variable %in% names(data)) {
      stop(paste("Filter variable", filter_by_variable, "not found in data"))
    }
    
    # Validate filter_values is a list
    if (!is.list(filter_values)) {
      stop("filter_values must be a list (e.g., list(NULL, 1, 2))")
    }
    
    # Auto-generate rows based on filter values
    # NULL means use all rows, otherwise filter by the specified value
    if (length(filter_values) >= 1) {
      if (is.null(filter_values[[1]])) {
        rows1 <- 1:nrow(data)
      } else {
        rows1 <- which(data[[filter_by_variable]] == filter_values[[1]])
      }
      auto_filtered <- TRUE
    }
    if (length(filter_values) >= 2) {
      if (is.null(filter_values[[2]])) {
        rows2 <- 1:nrow(data)
      } else {
        rows2 <- which(data[[filter_by_variable]] == filter_values[[2]])
      }
      auto_filtered <- TRUE
    }
    if (length(filter_values) >= 3) {
      if (is.null(filter_values[[3]])) {
        rows3 <- 1:nrow(data)
      } else {
        rows3 <- which(data[[filter_by_variable]] == filter_values[[3]])
      }
      auto_filtered <- TRUE
    }
    if (length(filter_values) >= 4) {
      if (is.null(filter_values[[4]])) {
        rows4 <- 1:nrow(data)
      } else {
        rows4 <- which(data[[filter_by_variable]] == filter_values[[4]])
      }
      auto_filtered <- TRUE
    }
    if (length(filter_values) >= 5) {
      if (is.null(filter_values[[5]])) {
        rows5 <- 1:nrow(data)
      } else {
        rows5 <- which(data[[filter_by_variable]] == filter_values[[5]])
      }
      auto_filtered <- TRUE
    }
    
    # Enable compare_rows mode if we have at least 2 groups
    if (!is.null(rows1) && !is.null(rows2)) {
      compare_rows <- TRUE
    }
  }
  
  # If using original CSV row numbers, subtract 3 (for metadata rows)
  # Skip this if we used automatic filtering (those are already correct indices)
  if (csv_rows && !auto_filtered) {
    if (!is.null(rows1)) rows1 <- rows1 - 3
    if (!is.null(rows2)) rows2 <- rows2 - 3
    if (!is.null(rows3)) rows3 <- rows3 - 3
    if (!is.null(rows4)) rows4 <- rows4 - 3
    if (!is.null(rows5)) rows5 <- rows5 - 3
  }

  ### Process Data --------------------------------------
  all_data <- list()

  if (compare_rows && !is.null(rows1) && !is.null(rows2)) {
    # COMPARISON MODE - compare same variables across different row subsets
    rows_list <- list(rows1, rows2)
    if (!is.null(rows3)) rows_list[[3]] <- rows3
    if (!is.null(rows4)) rows_list[[4]] <- rows4
    if (!is.null(rows5)) rows_list[[5]] <- rows5
    num_groups <- length(rows_list)

    for (i in seq_along(variable_groups)) {
      group <- variable_groups[[i]]

      category_label <- label_mapping[[group[1]]]
      if (is.null(category_label) || category_label == "" || is.na(category_label)) {
        category_label <- group[1]
      }

      for (j in seq_along(group)) {
        for (r in seq_along(rows_list)) {
          row_subset <- rows_list[[r]]
          vals <- data[row_subset, group[j], drop = TRUE]
          mean_val <- mean(vals, na.rm = TRUE)
          se_val <- sd(vals, na.rm = TRUE) / sqrt(sum(!is.na(vals)))
          ci_lower <- mean_val - qt(0.975, df = sum(!is.na(vals)) - 1) * se_val
          ci_upper <- mean_val + qt(0.975, df = sum(!is.na(vals)) - 1) * se_val

          all_data[[length(all_data) + 1]] <- data.frame(
            category = category_label,
            version = group_labels[r],
            mean_value = mean_val,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            version_num = j,
            category_num = i,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    used_labels <- group_labels[1:num_groups]

  } else {
    # STANDARD MODE - rounds on x-axis, questions as lines
    num_groups <- length(variable_groups)

    for (i in seq_along(variable_groups)) {
      group <- variable_groups[[i]]

      category_label <- label_mapping[[group[1]]]
      if (is.null(category_label) || category_label == "" || is.na(category_label)) {
        category_label <- group[1]
      }

      for (j in seq_along(group)) {
        vals <- data[[group[j]]]
        mean_val <- mean(vals, na.rm = TRUE)
        se_val <- sd(vals, na.rm = TRUE) / sqrt(sum(!is.na(vals)))
        ci_lower <- mean_val - qt(0.975, df = sum(!is.na(vals)) - 1) * se_val
        ci_upper <- mean_val + qt(0.975, df = sum(!is.na(vals)) - 1) * se_val

        all_data[[length(all_data) + 1]] <- data.frame(
          category = group_labels[j],   # Round label on x-axis
          version = category_label,      # Question label as line
          mean_value = mean_val,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          version_num = i,
          category_num = j,
          stringsAsFactors = FALSE
        )
      }
    }

    used_labels <- sapply(variable_groups, function(group) {
      lbl <- label_mapping[[group[1]]]
      if (is.null(lbl) || lbl == "" || is.na(lbl)) group[1] else lbl
    })
  }

  # Combine and prepare data
  summary_data <- bind_rows(all_data) %>%
    mutate(
      category = str_wrap(category, width = 20),
      version = factor(version, levels = used_labels),
      category_num = match(category, unique(category)),
      chunk_id = ceiling(category_num / chunk_size)
    )

  # Wrap title and subtitles
  wrapped_title <- stringr::str_wrap(title, width = title_width)
  combined_subtitle <- NULL
  if (!is.null(subtitle) && !is.null(subtitle2)) {
    combined_subtitle <- paste0(subtitle, "\n", subtitle2)
  } else if (!is.null(subtitle)) {
    combined_subtitle <- subtitle
  } else if (!is.null(subtitle2)) {
    combined_subtitle <- subtitle2
  }

  ### Plot --------------------------------------
  summary_chunks <- split(summary_data, summary_data$chunk_id)
  plots <- list()

  for (i in seq_along(summary_chunks)) {
    cur_chunk <- summary_chunks[[i]]

    cur_chunk <- cur_chunk %>%
      mutate(category = factor(category, levels = unique(category)))

    plot <- ggplot(cur_chunk, aes(x = category, y = mean_value, color = version, group = version)) +
      geom_line(linewidth = 1.2)

    if (show_error_bars) {
      plot <- plot + geom_errorbar(
        aes(ymin = pmax(ci_lower, y_range[1]), ymax = pmin(ci_upper, y_range[2])),
        width = 0.15, linewidth = 0.5
      )
    }

    if (show_points) {
      plot <- plot + geom_point(size = 3)
    }

    if (show_labels) {
      plot <- plot + geom_text(
        aes(label = round(mean_value, 2)),
        vjust = -1.2,
        size = 3.5,
        show.legend = FALSE
      )
    }

    plot <- plot +
      scale_y_continuous(
        breaks = seq(y_range[1], y_range[2], by = 1),
        expand = c(0, 0)
      ) +
      coord_cartesian(ylim = c(y_range[1], y_range[2] + 0.5), clip = "off") +
      scale_color_manual(
        values = colors[1:length(unique(cur_chunk$version))],
        breaks = used_labels[1:length(unique(cur_chunk$version))]
      ) +
      labs(
        title = if(length(summary_chunks) > 1) paste0(wrapped_title, " (Part ", i, ")") else wrapped_title,
        subtitle = combined_subtitle,
        x = x_label,
        y = y_label,
        color = ""
      ) +
      theme_minimal() +
      theme(
        plot.title    = element_text(size = 18, face = "bold", hjust = 0.5, colour = "black"),
        plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "black"),
        axis.text.x   = if(show_y_labels) element_text(
          color = "black", size = 10,
          angle = if(angle_labels) 45 else 0,
          hjust = if(angle_labels) 1 else 0.5
        ) else element_blank(),
        axis.ticks.x  = if(show_y_labels) element_line() else element_blank(),
        axis.text.y   = element_text(color = "black", size = 10),
        legend.position  = if(show_legend) "top" else "none",
        legend.direction = if(legend_stack) "vertical" else "horizontal",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(10, 50, 10, 10)
      )

    plots[[length(plots) + 1]] <- plot
  }

  return(plots)
}