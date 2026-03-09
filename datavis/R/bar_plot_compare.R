#' @title Bar Plot Compare
#' @description Creates a comparison bar chart from survey data.
#' @param data_source A data frame loaded from a Qualtrics CSV.
#' @param variable_groups A list of character vectors of column names to compare.
#' @param title Plot title.
#' @param group_labels Character vector of labels for each group.
#' @param compare_time Logical. Whether to compare across time periods.
#' @param time1_rows Row indices for time period 1.
#' @param time2_rows Row indices for time period 2.
#' @param time3_rows Row indices for time period 3. Default NULL.
#' @param time4_rows Row indices for time period 4. Default NULL.
#' @param time5_rows Row indices for time period 5. Default NULL.
#' @param csv_rows Logical. Whether rows are original CSV row numbers.
#' @param filter_by_variable Variable name to filter by.
#' @param filter_values List of values to filter for each time period.
#' @param time_labels Character vector of labels for each time period.
#' @param x_range Numeric vector of length 2 for the x-axis range.
#' @param chunk_size Number of variables per plot chunk.
#' @param title_width Integer for title wrapping width.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param subtitle Plot subtitle.
#' @param subtitle2 Second plot subtitle.
#' @param colors Custom color palette.
#' @param diverging Logical. Whether to use a diverging scale.
#' @param center_value Center value for diverging scale.
#' @param show_y_labels Logical. Whether to show y-axis labels.
#' @param orientation Either "horizontal" or "vertical".
#' @param angle_labels Logical. Whether to angle x-axis labels.
#' @return A list of ggplot objects.
#' @export
bar_plot_compare <- function(data_source,
                         variable_groups, 
                         title,
                         group_labels = c("Version 1", "Version 2", "Version 3", "Version 4"),
                         # Time comparison parameters
                         compare_time = FALSE,
                         time1_rows = 1:15, 
                         time2_rows = 16:30,
                         time3_rows = NULL,           # Optional third time period
                         time4_rows = NULL,           # Optional fourth time period
                         time5_rows = NULL,           # Optional fifth time period
                         csv_rows = TRUE,
                         # NEW: Automatic row filtering parameters
                         filter_by_variable = NULL,  # Variable name to filter by (e.g., "Q8")
                         filter_values = NULL,       # List of values to filter for each time period
                         time_labels = c("Time 1", "Time 2"),
                         # Range and chunking
                         x_range = c(1, 5),
                         chunk_size = 5,
                         # Text parameters
                         title_width = 40,
                         x_label = NULL, 
                         y_label = NULL, 
                         subtitle = NULL, 
                         subtitle2 = NULL,
                         # Style parameters
                         colors = NULL,  # Auto-detect based on compare_time
                         diverging = FALSE, 
                         center_value = 3,
                         show_y_labels = TRUE,
                         # Orientation parameters
                         orientation = "horizontal",
                         angle_labels = TRUE) {

  # Validate orientation
  orientation <- tolower(orientation)
  if (!orientation %in% c("horizontal", "vertical")) {
    stop("orientation must be either 'horizontal' or 'vertical'")
  }
  
  # Determine number of time periods
  num_time_periods <- 2  # Start with minimum
  if (!is.null(time3_rows)) num_time_periods <- 3
  if (!is.null(time4_rows)) num_time_periods <- 4
  if (!is.null(time5_rows)) num_time_periods <- 5
  
  # Validate time_labels length
  if (compare_time && length(time_labels) < num_time_periods) {
    # Auto-extend time_labels if needed
    for (i in (length(time_labels) + 1):num_time_periods) {
      time_labels <- c(time_labels, paste("Time", i))
    }
    warning(paste("time_labels extended to", num_time_periods, "periods with default names"))
  }

  # Process raw dataframe
  processed <- process_data_with_labels(data_source)
  data <- processed$data
  label_mapping <- processed$labels
  
  # NEW: Automatic row filtering based on filter_by_variable
  # MUST happen BEFORE csv_rows adjustment AND before color building
  auto_filtered <- FALSE
  if (!is.null(filter_by_variable) && !is.null(filter_values)) {
    if (!filter_by_variable %in% names(data)) {
      stop(paste("Filter variable", filter_by_variable, "not found in data"))
    }
    
    # Validate filter_values is a list
    if (!is.list(filter_values)) {
      stop("filter_values must be a list (e.g., list(NULL, 1, 2))")
    }
    
    # When filter_by_variable is provided, ALWAYS use automatic filtering
    # (override any default time_rows values)
    if (length(filter_values) >= 1) {
      if (is.null(filter_values[[1]])) {
        time1_rows <- 1:nrow(data)
      } else {
        time1_rows <- which(data[[filter_by_variable]] == filter_values[[1]])
      }
      auto_filtered <- TRUE
    }
    if (length(filter_values) >= 2) {
      if (is.null(filter_values[[2]])) {
        time2_rows <- 1:nrow(data)
      } else {
        time2_rows <- which(data[[filter_by_variable]] == filter_values[[2]])
      }
      auto_filtered <- TRUE
    }
    if (length(filter_values) >= 3) {
      if (is.null(filter_values[[3]])) {
        time3_rows <- 1:nrow(data)
      } else {
        time3_rows <- which(data[[filter_by_variable]] == filter_values[[3]])
      }
      auto_filtered <- TRUE
    }
    if (length(filter_values) >= 4) {
      if (is.null(filter_values[[4]])) {
        time4_rows <- 1:nrow(data)
      } else {
        time4_rows <- which(data[[filter_by_variable]] == filter_values[[4]])
      }
      auto_filtered <- TRUE
    }
    if (length(filter_values) >= 5) {
      if (is.null(filter_values[[5]])) {
        time5_rows <- 1:nrow(data)
      } else {
        time5_rows <- which(data[[filter_by_variable]] == filter_values[[5]])
      }
      auto_filtered <- TRUE
    }
    
    # Enable compare_time mode if we have filtering
    compare_time <- TRUE
    
    # Update num_time_periods based on how many filter_values were provided
    num_time_periods <- length(filter_values)
    
    # Also extend time_labels if needed now that we know num_time_periods
    if (length(time_labels) < num_time_periods) {
      for (i in (length(time_labels) + 1):num_time_periods) {
        time_labels <- c(time_labels, paste("Time", i))
      }
    }
  }

  # NOW build colors — AFTER num_time_periods is finalised
  if (is.null(colors)) {
    if (compare_time) {
      # Nested color scheme for time comparison (shaded groups)
      # Base colors: Blue #4f74dd, Orange #efac51, Pink #d24d77, Green #5db693, Purple #bd73b0
      if (num_time_periods == 5) {
        colors <- list(
          c("#1a3d8f", "#2e52b8", "#4f74dd", "#7a95e6", "#a5b6ef"),  # Blues
          c("#b87a00", "#d49520", "#efac51", "#f4c07a", "#f9d4a3"),  # Oranges
          c("#8f1a47", "#b82d5e", "#d24d77", "#de7a9a", "#eaa7bd"),  # Pinks
          c("#2a7a5a", "#3d9570", "#5db693", "#83c7ae", "#a9d8c9")   # Greens
        )
      } else if (num_time_periods == 4) {
        colors <- list(
          c("#2e52b8", "#4f74dd", "#7a95e6", "#a5b6ef"),  # Blues
          c("#d49520", "#efac51", "#f4c07a", "#f9d4a3"),  # Oranges
          c("#b82d5e", "#d24d77", "#de7a9a", "#eaa7bd"),  # Pinks
          c("#3d9570", "#5db693", "#83c7ae", "#a9d8c9")   # Greens
        )
      } else if (num_time_periods == 3) {
        colors <- list(
          c("#4f74dd", "#7a95e6", "#a5b6ef"),  # Blues
          c("#efac51", "#f4c07a", "#f9d4a3"),  # Oranges
          c("#d24d77", "#de7a9a", "#eaa7bd"),  # Pinks
          c("#5db693", "#83c7ae", "#a9d8c9")   # Greens
        )
      } else {
        colors <- list(
          c("#4f74dd", "#a5b6ef"),  # Blues
          c("#efac51", "#f9d4a3"),  # Oranges
          c("#d24d77", "#eaa7bd"),  # Pinks
          c("#5db693", "#a9d8c9")   # Greens
        )
      }
    } else {
      # Simple color vector for single comparison
      colors <- c("#4f74dd", "#efac51", "#d24d77", "#5db693", "#bd73b0")
    }
  }

  # If using original CSV row numbers, subtract 3 (for metadata rows)
  # Skip this if we used automatic filtering (those are already correct indices)
  if (csv_rows && !auto_filtered) {
    if (!is.null(time1_rows)) time1_rows <- time1_rows - 3
    if (!is.null(time2_rows)) time2_rows <- time2_rows - 3
    if (!is.null(time3_rows)) time3_rows <- time3_rows - 3
    if (!is.null(time4_rows)) time4_rows <- time4_rows - 3
    if (!is.null(time5_rows)) time5_rows <- time5_rows - 3
  }

  ### Process Data --------------------------------------
  if (compare_time) {
    # TIME COMPARISON MODE (e.g., Day 1 vs Day 2 vs Combined)
    all_data <- list()
    
    # Create list of time periods
    time_rows_list <- list(time1_rows, time2_rows)
    if (!is.null(time3_rows)) {
      time_rows_list[[3]] <- time3_rows
    }
    if (!is.null(time4_rows)) {
      time_rows_list[[4]] <- time4_rows
    }
    if (!is.null(time5_rows)) {
      time_rows_list[[5]] <- time5_rows
    }
    
    # Create color mapping - use 1:num_time_periods so Time 1 (Dark) is the first factor level
    all_colors <- c()
    color_names <- c()
    for (j in seq_along(group_labels)) {
      for (t in 1:num_time_periods) {
        color_names <- c(color_names, paste(group_labels[j], "-", time_labels[t]))
        all_colors <- c(all_colors, colors[[j]][t])
      }
    }
    names(all_colors) <- color_names
    
    # Loop through each group of variables
    for (i in seq_along(variable_groups)) {
      group <- variable_groups[[i]]
      
      # Get the label from label_mapping for the first variable in the group
      comparison_label <- label_mapping[[group[1]]]
      
      # If label is missing or empty, use the variable name as fallback
      if (is.null(comparison_label) || comparison_label == "" || is.na(comparison_label)) {
        comparison_label <- group[1]
      }
      
      # Calculate summary statistics for each version in the group
      for (j in seq_along(group)) {
        # Loop through each time period
        for (t in seq_along(time_rows_list)) {
          time_summary <- data[time_rows_list[[t]], ] %>%
            summarise(
              mean_value = mean(.data[[group[j]]], na.rm = TRUE),
              se = sd(.data[[group[j]]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[group[j]]]))),
              ci_lower = mean_value - qt(0.975, df = sum(!is.na(.data[[group[j]]])) - 1) * se,
              ci_upper = mean_value + qt(0.975, df = sum(!is.na(.data[[group[j]]])) - 1) * se,
              comparison = comparison_label,
              version = group_labels[j],
              time = time_labels[t],
              version_num = j,
              comparison_num = i
            )
          
          all_data[[length(all_data) + 1]] <- time_summary
        }
      }
    }
    
    # Combine all data into one dataframe
    summary_data <- bind_rows(all_data) %>%
      mutate(
        comparison = str_wrap(comparison, width = 30),
        version = factor(version, levels = group_labels),
        time = factor(time, levels = time_labels[1:num_time_periods]),
        # Create combined grouping for positioning
        version_time = paste(version, "-", time),
        # Set factor levels in the correct order for legend
        version_time = factor(version_time, levels = color_names)
      )
    
  } else {
    # SINGLE COMPARISON MODE (no time dimension)
    comparison_data <- list()
    
    # Loop through each group of variables
    for (i in seq_along(variable_groups)) {
      group <- variable_groups[[i]]
      
      # Get the label from label_mapping for the first variable in the group
      comparison_label <- label_mapping[[group[1]]]
      
      # If label is missing or empty, use the variable name as fallback
      if (is.null(comparison_label) || comparison_label == "" || is.na(comparison_label)) {
        comparison_label <- group[1]
      }
      
      # Calculate summary statistics for each version in the group
      group_summaries <- list()
      for (j in seq_along(group)) {
        version_summary <- data %>%
          summarise(
            mean_value = mean(.data[[group[j]]], na.rm = TRUE),
            se = sd(.data[[group[j]]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[group[j]]]))),
            ci_lower = mean_value - qt(0.975, df = sum(!is.na(.data[[group[j]]])) - 1) * se,
            ci_upper = mean_value + qt(0.975, df = sum(!is.na(.data[[group[j]]])) - 1) * se,
            comparison = comparison_label,
            version = group_labels[j]
          )
        group_summaries[[j]] <- version_summary
      }
      
      # Combine all versions for this comparison
      comparison_data[[i]] <- bind_rows(group_summaries)
    }
    
    # Combine all comparisons into one dataframe
    summary_data <- bind_rows(comparison_data) %>%
      mutate(
        comparison = str_wrap(comparison, width = 30),
        version = factor(version, levels = rev(group_labels)),  # Reverse for correct bar order
        chunk = ceiling(row_number() / (chunk_size * length(variable_groups[[1]])))
      )
  }
  
  # Add diverging-specific calculations if needed
  if (diverging) {
    summary_data <- summary_data %>%
      mutate(
        plot_value = mean_value - center_value,
        plot_ci_lower = ci_lower - center_value,
        plot_ci_upper = ci_upper - center_value
      )
  } else {
    summary_data <- summary_data %>%
      mutate(
        plot_value = mean_value,
        plot_ci_lower = ci_lower,
        plot_ci_upper = ci_upper
      )
  }

  # Wrap title and combine subtitles
  wrapped_title <- stringr::str_wrap(title, width = title_width)
  
  # Combine subtitle and subtitle2 with a line break
  combined_subtitle <- NULL
  if (!is.null(subtitle) && !is.null(subtitle2)) {
    combined_subtitle <- paste0(subtitle, "\n", subtitle2)
  } else if (!is.null(subtitle)) {
    combined_subtitle <- subtitle
  } else if (!is.null(subtitle2)) {
    combined_subtitle <- subtitle2
  }

  ### Create Plots --------------------------------------
  plots <- list()
  
  if (compare_time) {
    # TIME COMPARISON: Create one plot per comparison
    for (comp_num in unique(summary_data$comparison_num)) {
      cur_data <- summary_data %>% filter(comparison_num == comp_num)
      
      # Adjust dodge width based on number of time periods
      if (num_time_periods >= 4) {
        dodge_width <- 0.85
        bar_width <- 0.8
      } else if (num_time_periods == 3) {
        dodge_width <- 0.8
        bar_width <- 0.75
      } else {
        dodge_width <- 0.7
        bar_width <- 0.65
      }
      
      if (orientation == "horizontal") {
        plot <- ggplot(cur_data, aes(x = if(diverging) plot_value else mean_value, y = version, fill = version_time))
        
        # Add center line if diverging
        if (diverging) {
          plot <- plot + geom_vline(xintercept = 0, color = "black", linewidth = 0.8)
        }
        
        plot <- plot +
          geom_bar(stat = "identity", position = position_dodge(width = dodge_width, reverse = TRUE), width = bar_width)
        
        # Error bars depend on diverging mode
        if (diverging) {
          plot <- plot +
            geom_errorbar(
              aes(xmin = plot_ci_lower, xmax = plot_ci_upper),
              position = position_dodge(width = dodge_width, reverse = TRUE),
              width = 0.3, colour = "black", linewidth = 0.4
            ) +
            geom_text(
              aes(label = round(mean_value, 2),
                  hjust = ifelse(plot_value < 0, 1.2, -0.3)),
              position = position_dodge(width = dodge_width, reverse = TRUE),
              size = 3.5, color = "black"
            )
        } else {
          plot <- plot +
            geom_errorbar(
              aes(xmin = pmax(ci_lower, x_range[1]), xmax = pmin(ci_upper, x_range[2])),
              position = position_dodge(width = dodge_width, reverse = TRUE),
              width = 0.3, colour = "black", linewidth = 0.4
            ) +
            geom_text(
              aes(label = round(mean_value, 2)),
              position = position_dodge(width = dodge_width, reverse = TRUE),
              hjust = -0.3, size = 3.5, color = "black"
            )
        }
        
        # X-axis scale depends on diverging mode
        if (diverging) {
          plot <- plot +
            scale_x_continuous(
              breaks = seq(x_range[1] - center_value, x_range[2] - center_value, by = 1),
              labels = seq(x_range[1], x_range[2], by = 1),
              expand = c(0, 0)
            ) +
            coord_cartesian(xlim = c(x_range[1] - center_value - 0.3, x_range[2] - center_value + 0.3), clip = "off")
        } else {
          plot <- plot +
            scale_x_continuous(
              breaks = seq(x_range[1], x_range[2], by = 1),
              expand = c(0, 0)
            ) +
            coord_cartesian(xlim = c(x_range[1], x_range[2] + 0.5), clip = "on")
        }
        
        plot <- plot +
          scale_y_discrete(
            limits = rev(levels(cur_data$version)),
            expand = expansion(add = c(0.5, 0.5))
          )
          
      } else {
        # Vertical orientation
        plot <- ggplot(cur_data, aes(x = version, y = if(diverging) plot_value else mean_value, fill = version_time))
        
        # Add center line if diverging
        if (diverging) {
          plot <- plot + geom_hline(yintercept = 0, color = "black", linewidth = 0.8)
        }
        
        plot <- plot +
          geom_bar(stat = "identity", position = position_dodge(width = dodge_width, reverse = TRUE), width = bar_width)
        
        # Error bars depend on diverging mode
        if (diverging) {
          plot <- plot +
            geom_errorbar(
              aes(ymin = plot_ci_lower, ymax = plot_ci_upper),
              position = position_dodge(width = dodge_width, reverse = TRUE),
              width = 0.3, colour = "black", linewidth = 0.4
            ) +
            geom_text(
              aes(label = round(mean_value, 2),
                  vjust = ifelse(plot_value < 0, 1.2, -0.3)),
              position = position_dodge(width = dodge_width, reverse = TRUE),
              size = 3.5, color = "black"
            )
        } else {
          plot <- plot +
            geom_errorbar(
              aes(ymin = pmax(ci_lower, x_range[1]), ymax = pmin(ci_upper, x_range[2])),
              position = position_dodge(width = dodge_width, reverse = TRUE),
              width = 0.3, colour = "black", linewidth = 0.4
            ) +
            geom_text(
              aes(label = round(mean_value, 2)),
              position = position_dodge(width = dodge_width, reverse = TRUE),
              vjust = -0.3, size = 3.5, color = "black"
            )
        }
        
        # Y-axis scale depends on diverging mode
        if (diverging) {
          plot <- plot +
            scale_y_continuous(
              breaks = seq(x_range[1] - center_value, x_range[2] - center_value, by = 1),
              labels = seq(x_range[1], x_range[2], by = 1),
              expand = c(0, 0)
            ) +
            coord_cartesian(ylim = c(x_range[1] - center_value - 0.3, x_range[2] - center_value + 0.3), clip = "off")
        } else {
          plot <- plot +
            scale_y_continuous(
              breaks = seq(x_range[1], x_range[2], by = 1),
              expand = c(0, 0)
            ) +
            coord_cartesian(ylim = c(x_range[1], x_range[2] + 0.5), clip = "on")
        }
        
        plot <- plot +
          scale_x_discrete(
            expand = expansion(add = c(0.5, 0.5))
          )
      }
      
      plot <- plot +
        scale_fill_manual(values = all_colors, name = "") +
        labs(
          title = wrapped_title,
          subtitle = combined_subtitle,
          x = x_label,
          y = y_label
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5, colour = "black"),
          plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "black"),
          legend.position = "top",
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 50, 10, 10)
        )
      
      # Add orientation-specific theme
      if (orientation == "horizontal") {
        plot <- plot +
          theme(
            axis.text.x = element_text(color = "black", size = 12),
            axis.text.y = if(show_y_labels) element_text(color = "black", size = 10) else element_blank(),
            panel.grid.major.y = element_blank()
          )
      } else {
        plot <- plot +
          theme(
            axis.text.x = element_text(
              color = "black", 
              size = 10,
              angle = if(angle_labels) 45 else 0,
              hjust = if(angle_labels) 1 else 0.5
            ),
            axis.text.y = element_text(color = "black", size = 12),
            panel.grid.major.x = element_blank()
          )
      }
      
      plots[[length(plots) + 1]] <- plot
    }
    
  } else {
    # SINGLE COMPARISON: Handle chunking
    summary_chunks <- group_split(summary_data, chunk)
    
    for (i in seq_along(summary_chunks)) {
      cur_chunk <- summary_chunks[[i]]
      
      # Create base plot based on orientation
      if (orientation == "horizontal") {
        # For horizontal, reverse comparison order so first statement is at top
        cur_chunk <- cur_chunk %>%
          mutate(comparison = factor(comparison, levels = rev(unique(comparison))))
        
        plot <- ggplot(cur_chunk, aes(x = plot_value, y = comparison, fill = version))
      } else {
        plot <- ggplot(cur_chunk, aes(x = comparison, y = plot_value, fill = version))
      }
      
      # Add center line if diverging
      if (diverging) {
        if (orientation == "horizontal") {
          plot <- plot + geom_vline(xintercept = 0, color = "black", linewidth = 0.8)
        } else {
          plot <- plot + geom_hline(yintercept = 0, color = "black", linewidth = 0.8)
        }
      }
      
      # Add bars and error bars based on diverging mode and orientation
      if (diverging) {
        # DIVERGING MODE
        if (orientation == "horizontal") {
          plot <- plot +
            geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
            geom_errorbar(
              aes(xmin = plot_ci_lower, xmax = plot_ci_upper),
              position = position_dodge(width = 0.8),
              width = 0.2, colour = "black", linewidth = 0.5
            ) +
            geom_text(
              aes(label = round(mean_value, 2),
                  hjust = ifelse(plot_value < 0, 1.2, -0.3)),
              position = position_dodge(width = 0.8),
              size = 3.5, color = "black"
            )
        } else {
          plot <- plot +
            geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
            geom_errorbar(
              aes(ymin = plot_ci_lower, ymax = plot_ci_upper),
              position = position_dodge(width = 0.8),
              width = 0.2, colour = "black", linewidth = 0.5
            ) +
            geom_text(
              aes(label = round(mean_value, 2),
                  vjust = ifelse(plot_value < 0, 1.2, -0.3)),
              position = position_dodge(width = 0.8),
              size = 3.5, color = "black"
            )
        }
      } else {
        # NON-DIVERGING MODE
        if (orientation == "horizontal") {
          plot <- plot +
            geom_col(
              aes(x = plot_value - x_range[1]),
              position = position_dodge(width = 0.8),
              width = 0.7
            ) +
            geom_errorbar(
              aes(xmin = pmax(plot_ci_lower - x_range[1], 0), 
                  xmax = pmin(plot_ci_upper - x_range[1], x_range[2] - x_range[1])),
              position = position_dodge(width = 0.8),
              width = 0.2, colour = "black", linewidth = 0.5
            ) +
            geom_text(
              aes(label = round(mean_value, 2), x = plot_value - x_range[1]),
              position = position_dodge(width = 0.8),
              hjust = -0.3, size = 3.5, color = "black"
            )
        } else {
          plot <- plot +
            geom_col(
              aes(y = plot_value - x_range[1]),
              position = position_dodge(width = 0.8),
              width = 0.7
            ) +
            geom_errorbar(
              aes(ymin = pmax(plot_ci_lower - x_range[1], 0), 
                  ymax = pmin(plot_ci_upper - x_range[1], x_range[2] - x_range[1])),
              position = position_dodge(width = 0.8),
              width = 0.2, colour = "black", linewidth = 0.5
            ) +
            geom_text(
              aes(label = round(mean_value, 2), y = plot_value - x_range[1]),
              position = position_dodge(width = 0.8),
              vjust = -0.3, size = 3.5, color = "black"
            )
        }
      }
      
      # Configure axis based on diverging or not and orientation
      if (diverging) {
        if (orientation == "horizontal") {
          plot <- plot +
            scale_x_continuous(
              breaks = seq(x_range[1] - center_value, x_range[2] - center_value, by = 1),
              labels = seq(x_range[1], x_range[2], by = 1),
              expand = c(0, 0)
            ) +
            coord_cartesian(xlim = c(x_range[1] - center_value - 0.3, x_range[2] - center_value + 0.3), clip = "off")
        } else {
          plot <- plot +
            scale_y_continuous(
              breaks = seq(x_range[1] - center_value, x_range[2] - center_value, by = 1),
              labels = seq(x_range[1], x_range[2], by = 1),
              expand = c(0, 0)
            ) +
            coord_cartesian(ylim = c(x_range[1] - center_value - 0.3, x_range[2] - center_value + 0.3), clip = "off")
        }
      } else {
        if (orientation == "horizontal") {
          plot <- plot +
            scale_x_continuous(
              breaks = seq(0, x_range[2] - x_range[1], by = 1),
              labels = seq(x_range[1], x_range[2], by = 1),
              expand = c(0, 0)
            ) +
            coord_cartesian(xlim = c(0, x_range[2] - x_range[1] + 0.5), clip = "off")
        } else {
          plot <- plot +
            scale_y_continuous(
              breaks = seq(0, x_range[2] - x_range[1], by = 1),
              labels = seq(x_range[1], x_range[2], by = 1),
              expand = c(0, 0)
            ) +
            coord_cartesian(ylim = c(0, x_range[2] - x_range[1] + 0.5), clip = "off")
        }
      }
      
      # Add remaining elements
      plot <- plot +
        scale_fill_manual(
          values = colors[1:length(unique(cur_chunk$version))],
          breaks = group_labels[1:length(unique(cur_chunk$version))]  # Keep legend in original order
        ) +
        guides(fill = guide_legend(reverse = FALSE)) +  # Don't reverse legend
        labs(
          title = if(length(summary_chunks) > 1) paste0(wrapped_title) else wrapped_title,
          subtitle = combined_subtitle,
          x = x_label,
          y = y_label,
          fill = ""
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5, colour = "black"),
          plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "black"),
          legend.position = "top",
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 50, 10, 10)
        )
      
      # Add orientation-specific theme
      if (orientation == "horizontal") {
        plot <- plot +
          theme(
            axis.text.x = element_text(color = "black", size = 12),
            axis.text.y = if(show_y_labels) element_text(color = "black", size = 10) else element_blank(),
            panel.grid.major.y = element_blank()
          )
      } else {
        plot <- plot +
          theme(
            axis.text.x = element_text(
              color = "black", 
              size = if(show_y_labels) 10 else 0,
              angle = if(angle_labels) 45 else 0,
              hjust = if(angle_labels) 1 else 0.5
            ),
            axis.text.y = element_text(color = "black", size = 12),
            panel.grid.major.x = element_blank()
          )
      }
      
      plots[[length(plots) + 1]] <- plot
    }
  }

  return(plots)
}