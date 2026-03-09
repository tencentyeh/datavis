#' @title Bar Plot Simple
#' @description Creates a simple bar chart from survey data.
#' @param data_source A data frame loaded from a Qualtrics CSV.
#' @param selected_variables Character vector of column names to plot.
#' @param title Plot title.
#' @param x_range Numeric vector of length 2 for the x-axis range.
#' @param chunk_size Number of variables per plot chunk.
#' @param title_width Integer for title wrapping width.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param subtitle Plot subtitle.
#' @param subtitle2 Second plot subtitle.
#' @param diverging Logical. Whether to use a diverging scale.
#' @param center_value Center value for diverging scale.
#' @param bar_color Color for bars when not comparing groups.
#' @param low_color Color for bars below center in diverging mode.
#' @param high_color Color for bars above center in diverging mode.
#' @param compare_rows Logical. Whether to compare row groups.
#' @param rows1 Row indices for group 1.
#' @param rows2 Row indices for group 2.
#' @param rows3 Row indices for group 3. Default NULL.
#' @param rows4 Row indices for group 4. Default NULL.
#' @param rows5 Row indices for group 5. Default NULL.
#' @param csv_rows Logical. Whether rows are original CSV row numbers.
#' @param filter_by_variable Variable name to filter by.
#' @param filter_values List of values to filter for each group.
#' @param group_labels Character vector of labels for each group.
#' @param colors Custom color palette.
#' @param orientation Either "horizontal" or "vertical".
#' @param angle_labels Logical. Whether to angle x-axis labels.
#' @param sort_by Either "original" or "value".
#' @param show_y_labels Logical. Whether to show y-axis labels.
#' @return A list of ggplot objects.
#' @export
bar_plot_simple <- function(data_source,  
                                selected_variables, 
                                title, 
                                x_range = c(1, 5), 
                                chunk_size = 5, 
                                title_width = 50, 
                                x_label = NULL, 
                                y_label = NULL, 
                                subtitle = NULL, 
                                subtitle2 = NULL,
                                diverging = FALSE,
                                center_value = 3,
                                bar_color = "#4f74dd",
                                low_color = "#4f74dd",
                                high_color = "#4f74dd",
                                compare_rows = FALSE,
                                rows1 = NULL,
                                rows2 = NULL,
                                rows3 = NULL,
                                rows4 = NULL,
                                rows5 = NULL,
                                csv_rows = TRUE,
                                # NEW: Automatic row filtering parameters
                                filter_by_variable = NULL,  # Variable name to filter by (e.g., "Q8")
                                filter_values = NULL,       # List of values to filter for each group (e.g., list(NULL, 1, 2))
                                group_labels = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"),  
                                colors = c("#4f74dd", "#efac51", "#d24d77", "#5db693", "#bd73b0"),
                                orientation = "horizontal",
                                angle_labels = TRUE,
                                sort_by = "original",
                                show_y_labels = TRUE) {
  
  # Validate orientation parameter
  orientation <- tolower(orientation)
  if (!orientation %in% c("horizontal", "vertical")) {
    stop("orientation must be either 'horizontal' or 'vertical'")
  }
  
  # Validate sort_by parameter
  sort_by <- tolower(sort_by)
  if (!sort_by %in% c("original", "value")) {
    stop("sort_by must be either 'original' (order in selected_variables) or 'value' (highest to lowest)")
  }
  
  # Process raw dataframe
  processed <- process_data_with_labels(data_source)
  data <- processed$data
  label_mapping <- processed$labels
  
  # If using original CSV row numbers, subtract 3 (for metadata rows)
  if (csv_rows) {
    if (!is.null(rows1)) rows1 <- rows1 - 3
    if (!is.null(rows2)) rows2 <- rows2 - 3
    if (!is.null(rows3)) rows3 <- rows3 - 3
    if (!is.null(rows4)) rows4 <- rows4 - 3
    if (!is.null(rows5)) rows5 <- rows5 - 3
  }
  
  # NEW: Automatic row filtering based on filter_by_variable
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
    if (length(filter_values) >= 1 && is.null(rows1)) {
      if (is.null(filter_values[[1]])) {
        rows1 <- 1:nrow(data)  # Use all rows
      } else {
        rows1 <- which(data[[filter_by_variable]] == filter_values[[1]])
      }
    }
    if (length(filter_values) >= 2 && is.null(rows2)) {
      if (is.null(filter_values[[2]])) {
        rows2 <- 1:nrow(data)
      } else {
        rows2 <- which(data[[filter_by_variable]] == filter_values[[2]])
      }
    }
    if (length(filter_values) >= 3 && is.null(rows3)) {
      if (is.null(filter_values[[3]])) {
        rows3 <- 1:nrow(data)
      } else {
        rows3 <- which(data[[filter_by_variable]] == filter_values[[3]])
      }
    }
    if (length(filter_values) >= 4 && is.null(rows4)) {
      if (is.null(filter_values[[4]])) {
        rows4 <- 1:nrow(data)
      } else {
        rows4 <- which(data[[filter_by_variable]] == filter_values[[4]])
      }
    }
    if (length(filter_values) >= 5 && is.null(rows5)) {
      if (is.null(filter_values[[5]])) {
        rows5 <- 1:nrow(data)
      } else {
        rows5 <- which(data[[filter_by_variable]] == filter_values[[5]])
      }
    }
    
    # Enable compare_rows mode if we have at least 2 groups
    if (!is.null(rows1) && !is.null(rows2)) {
      compare_rows <- TRUE
    }
  }
  
  ### Process Data --------------------------------------
  if (compare_rows && !is.null(rows1) && !is.null(rows2)) {
    
    process_group <- function(row_subset, group_label) {
      long_data <- data[row_subset, ] %>%
        select(all_of(selected_variables)) %>%
        pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
      
      long_data %>%
        group_by(variable) %>%
        summarise(
          mean_value = mean(value, na.rm = TRUE),
          se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
          ci_lower = mean_value - qt(0.975, df = sum(!is.na(value)) - 1) * se,
          ci_upper = mean_value + qt(0.975, df = sum(!is.na(value)) - 1) * se,
          .groups = 'drop'
        ) %>%
        mutate(group = group_label)
    }
    
    summary_data1 <- process_group(rows1, group_labels[1])
    summary_data2 <- process_group(rows2, group_labels[2])
    summary_data <- bind_rows(summary_data1, summary_data2)
    num_groups <- 2
    
    if (!is.null(rows3)) {
      summary_data <- bind_rows(summary_data, process_group(rows3, group_labels[3]))
      num_groups <- 3
    }
    if (!is.null(rows4)) {
      summary_data <- bind_rows(summary_data, process_group(rows4, group_labels[4]))
      num_groups <- 4
    }
    if (!is.null(rows5)) {
      summary_data <- bind_rows(summary_data, process_group(rows5, group_labels[5]))
      num_groups <- 5
    }
    
    summary_data <- summary_data %>%
      mutate(
        original_order = match(variable, selected_variables),
        variable = recode(variable, !!!label_mapping) %>%
                   str_wrap(width = if(diverging) 30 else 25),
        group = factor(group, levels = rev(group_labels[1:num_groups]))
      )
    
    if (sort_by == "value") {
      sort_order <- summary_data %>%
        group_by(variable, original_order) %>%
        summarise(avg_mean = mean(mean_value, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(avg_mean)) %>%
        mutate(sort_order = row_number())
      
      summary_data <- summary_data %>%
        left_join(sort_order %>% select(variable, sort_order), by = "variable")
      
      if (orientation == "horizontal") {
        summary_data <- summary_data %>%
          mutate(variable = factor(variable, levels = rev(sort_order$variable)))
      } else {
        summary_data <- summary_data %>%
          mutate(variable = factor(variable, levels = sort_order$variable))
      }
      
      # Chunk AFTER sorting
      summary_data <- summary_data %>%
        mutate(chunk = ceiling(sort_order / chunk_size))
    } else {
      if (orientation == "horizontal") {
        summary_data <- summary_data %>%
          arrange(desc(original_order)) %>%
          mutate(variable = factor(variable, levels = unique(variable)))
      } else {
        summary_data <- summary_data %>%
          arrange(original_order) %>%
          mutate(variable = factor(variable, levels = unique(variable)))
      }
      
      # Chunk based on original order
      summary_data <- summary_data %>%
        mutate(chunk = ceiling(original_order / chunk_size))
    }
    
  } else {
    long_data <- data %>%
      select(all_of(selected_variables)) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
    
    summary_data <- long_data %>%
      group_by(variable) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
        ci_lower = mean_value - qt(0.975, df = sum(!is.na(value)) - 1) * se,
        ci_upper = mean_value + qt(0.975, df = sum(!is.na(value)) - 1) * se,
        .groups = 'drop'
      ) %>%
      mutate(
        original_order = match(variable, selected_variables),
        variable = recode(variable, !!!label_mapping) %>%
                   str_wrap(width = if(diverging) 30 else 25)
      )
    
    if (sort_by == "value") {
      summary_data <- summary_data %>% 
        arrange(desc(mean_value)) %>%
        mutate(sort_order = row_number())
      
      if (orientation == "horizontal") {
        summary_data <- summary_data %>%
          mutate(variable = factor(variable, levels = rev(unique(variable))))
      } else {
        summary_data <- summary_data %>%
          mutate(variable = factor(variable, levels = unique(variable)))
      }
      
      # Chunk AFTER sorting
      summary_data <- summary_data %>%
        mutate(chunk = ceiling(sort_order / chunk_size))
    } else {
      if (orientation == "horizontal") {
        summary_data <- summary_data %>%
          arrange(desc(original_order)) %>%
          mutate(variable = factor(variable, levels = unique(variable)))
      } else {
        summary_data <- summary_data %>%
          arrange(original_order) %>%
          mutate(variable = factor(variable, levels = unique(variable)))
      }
      
      # Chunk based on original order
      summary_data <- summary_data %>%
        mutate(chunk = ceiling(original_order / chunk_size))
    }
    
    num_groups <- 1
  }
  
  if (diverging && !compare_rows) {
  summary_data <- summary_data %>%
    mutate(
      diverging_value = mean_value - center_value,
      bar_color = ifelse(mean_value < center_value, low_color, high_color)
    )
}
  
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
  if (compare_rows) {
    summary_chunks <- group_split(summary_data, chunk)
    
    plots <- lapply(summary_chunks, function(cur_chunk) {
      if (orientation == "horizontal") {
        plot <- ggplot(cur_chunk, aes(x = mean_value, y = variable, fill = group)) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
          geom_errorbar(
            aes(xmin = pmax(ci_lower, x_range[1]), xmax = pmin(ci_upper, x_range[2])),
            position = position_dodge(width = 0.8),
            width = 0.2, colour = "black", linewidth = 0.5
          ) +
          geom_text(
            aes(label = round(mean_value, 2)),
            position = position_dodge(width = 0.8),
            hjust = -0.3, size = 4, color = "black"
          ) +
          scale_x_continuous(breaks = seq(x_range[1], x_range[2], by = 1), expand = c(0, 0)) +
          coord_cartesian(xlim = c(x_range[1], x_range[2] + 0.5), clip = "on")
      } else {
        plot <- ggplot(cur_chunk, aes(x = variable, y = mean_value, fill = group)) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
          geom_errorbar(
            aes(ymin = pmax(ci_lower, x_range[1]), ymax = pmin(ci_upper, x_range[2])),
            position = position_dodge(width = 0.8),
            width = 0.2, colour = "black", linewidth = 0.5
          ) +
          geom_text(
            aes(label = round(mean_value, 2)),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 4, color = "black"
          ) +
          scale_y_continuous(breaks = seq(x_range[1], x_range[2], by = 1), expand = c(0, 0)) +
          coord_cartesian(ylim = c(x_range[1], x_range[2] + 0.5), clip = "on")
      }
      
      plot <- plot +
        scale_fill_manual(
          values = colors[1:num_groups],
          breaks = group_labels[1:num_groups]
        ) +
        guides(fill = guide_legend(reverse = FALSE)) +
        labs(
          title = wrapped_title,
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
          plot.margin = margin(10, 40, 10, 10)
        )
      
      if (orientation == "horizontal") {
        plot <- plot +
          theme(
            axis.text.x = element_text(color = "black", size = 12),
            axis.text.y = if(show_y_labels) element_text(color = "black", size = 10) else element_blank(),
            axis.ticks.y = if(show_y_labels) element_line() else element_blank(),
            panel.grid.major.y = element_blank()
          )
      } else {
        plot <- plot +
          theme(
            axis.text.x = if(show_y_labels) element_text(
              color = "black", size = 10,
              angle = if(angle_labels) 45 else 0,
              hjust = if(angle_labels) 1 else 0.5
            ) else element_blank(),
            axis.ticks.x = if(show_y_labels) element_line() else element_blank(),
            axis.text.y = element_text(color = "black", size = 12),
            panel.grid.major.x = element_blank()
          )
      }
      
      return(plot)
    })
    
  } else {
    summary_chunks <- group_split(summary_data, chunk)
    
    plots <- lapply(summary_chunks, function(cur_chunk) {
      if (diverging) {
        if (orientation == "horizontal") {
          plot <- ggplot(cur_chunk, aes(x = diverging_value, y = variable)) +
            geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
            geom_bar(aes(fill = bar_color), stat = "identity", width = 0.6, show.legend = FALSE) +
            geom_errorbar(
              aes(xmin = ci_lower - center_value, xmax = ci_upper - center_value), 
              width = 0.2, colour = "black", linewidth = 0.5
            ) +
            geom_text(
              aes(label = round(mean_value, 2), x = diverging_value,
                  hjust = ifelse(diverging_value < 0, 1.2, -0.2)),
              size = 4, color = "black"
            ) +
            scale_fill_identity() +
            scale_x_continuous(
              breaks = seq(x_range[1] - center_value, x_range[2] - center_value, by = 1),
              labels = seq(x_range[1], x_range[2], by = 1),
              expand = c(0, 0)
            ) +
            coord_cartesian(xlim = c(x_range[1] - center_value - 0.3, x_range[2] - center_value + 0.3), clip = "on")
        } else {
          plot <- ggplot(cur_chunk, aes(x = variable, y = diverging_value)) +
            geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
            geom_bar(aes(fill = bar_color), stat = "identity", width = 0.6, show.legend = FALSE) +
            geom_errorbar(
              aes(ymin = ci_lower - center_value, ymax = ci_upper - center_value), 
              width = 0.2, colour = "black", linewidth = 0.5
            ) +
            geom_text(
              aes(label = round(mean_value, 2), y = diverging_value,
                  vjust = ifelse(diverging_value < 0, 1.2, -0.2)),
              size = 4, color = "black"
            ) +
            scale_fill_identity() +
            scale_y_continuous(
              breaks = seq(x_range[1] - center_value, x_range[2] - center_value, by = 1),
              labels = seq(x_range[1], x_range[2], by = 1),
              expand = c(0, 0)
            ) +
            coord_cartesian(ylim = c(x_range[1] - center_value - 0.3, x_range[2] - center_value + 0.3), clip = "on")
        }
        
      } else {
        if (orientation == "horizontal") {
          plot <- ggplot(cur_chunk, aes(x = mean_value, y = variable)) +
            geom_bar(stat = "identity", fill = bar_color, width = 0.6) +
            geom_errorbar(
              aes(xmin = pmax(ci_lower, x_range[1]), xmax = pmin(ci_upper, x_range[2])), 
              width = 0.2, colour = "black", linewidth = 0.5
            ) +
            geom_text(
              aes(label = round(mean_value, 2), x = mean_value),  
              hjust = -0.3, size = 4, color = "black"
            ) +
            scale_x_continuous(breaks = seq(x_range[1], x_range[2], by = 1), expand = c(0, 0)) +
            coord_cartesian(xlim = c(x_range[1], x_range[2] + 0.5), clip = "on")
        } else {
          plot <- ggplot(cur_chunk, aes(x = variable, y = mean_value)) +
            geom_bar(stat = "identity", fill = bar_color, width = 0.6) +
            geom_errorbar(
              aes(ymin = pmax(ci_lower, x_range[1]), ymax = pmin(ci_upper, x_range[2])), 
              width = 0.2, colour = "black", linewidth = 0.5
            ) +
            geom_text(
              aes(label = round(mean_value, 2), y = mean_value),  
              vjust = -0.3, size = 4, color = "black"
            ) +
            scale_y_continuous(breaks = seq(x_range[1], x_range[2], by = 1), expand = c(0, 0)) +
            coord_cartesian(ylim = c(x_range[1], x_range[2] + 0.5), clip = "on")
        }
      }
      
      plot <- plot +
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
          plot.margin = margin(10, 40, 10, 10)
        )
      
      if (orientation == "horizontal") {
        plot <- plot +
          theme(
            axis.text.x = element_text(color = "black", size = 12),
            axis.text.y = if(show_y_labels) element_text(color = "black", size = if(diverging) 12 else 10) else element_blank(),
            axis.ticks.y = if(show_y_labels) element_line() else element_blank(),
            axis.title.y = if(show_y_labels) element_text() else element_blank(),
            panel.grid.major.y = element_blank()
          )
        if (!show_y_labels) {
          plot <- plot + scale_y_discrete(labels = NULL)
        }
      } else {
        plot <- plot +
          theme(
            axis.text.x = if(show_y_labels) element_text(
              color = "black",
              size = if(diverging) 12 else 10,
              angle = if(angle_labels) 45 else 0,
              hjust = if(angle_labels) 1 else 0.5
            ) else element_blank(),
            axis.ticks.x = if(show_y_labels) element_line() else element_blank(),
            axis.title.x = if(show_y_labels) element_text() else element_blank(),
            axis.text.y = element_text(color = "black", size = 12),
            panel.grid.major.x = element_blank()
          )
      }
      
      return(plot)
    })
  }
  
  return(plots)
}