#' @title Count Plot
#' @description Creates a frequency/count bar chart from survey data.
#' @param data_source A data frame loaded from a Qualtrics CSV.
#' @param selected_variable Unquoted column name to plot.
#' @param label_vec Character vector of labels for each response option.
#' @param title Plot title.
#' @param show_percentage Logical. Whether to show percentages instead of counts.
#' @param compare_rows List of row index vectors for group comparison.
#' @param group_labels Character vector of labels for each group.
#' @param csv_rows Logical. Whether rows are original CSV row numbers.
#' @param filter_by_variable Variable name to filter by.
#' @param filter_values List of values to filter for each group.
#' @param colors Custom color palette.
#' @param breaks Numeric interval between axis gridlines.
#' @param title_width Integer for title wrapping width.
#' @param title_font_size Font size for the title.
#' @param subtitle_font_size Font size for the subtitle.
#' @param axis_text_size Font size for axis text.
#' @param label_text_size Font size for bar labels.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param subtitle Plot subtitle.
#' @param orientation Either "horizontal" or "vertical".
#' @param angle_labels Logical. Whether to angle x-axis labels.
#' @param show_diagnostics Logical. Whether to show diagnostic output.
#' @return A ggplot object.
#' @export
count_plot <- function(data_source,
                        selected_variable,
                        label_vec,
                        title,
                        show_percentage = FALSE,
                        compare_rows = NULL,
                        group_labels = c("Group 1", "Group 2", "Group 3"),
                        csv_rows = TRUE,
                        filter_by_variable = NULL,
                        filter_values = NULL,
                        colors = c("#4f74dd", "#efac51", "#d24d77"),
                        breaks = 5,
                        title_width = 50,
                        title_font_size = 18,
                        subtitle_font_size = 10,
                        axis_text_size = 10,
                        label_text_size = 3.5,
                        x_label = NULL,
                        y_label = NULL,
                        subtitle = NULL,
                        orientation = "horizontal",
                        angle_labels = FALSE,
                        show_diagnostics = FALSE) {

  orientation <- tolower(orientation)

  col_name_str <- as.character(substitute(selected_variable))
  processed <- process_data_with_labels(data_source, multi_cols = col_name_str)
  data <- processed$data

  col_sym <- rlang::ensym(selected_variable)
  is_comparison <- !is.null(compare_rows) || (!is.null(filter_by_variable) && !is.null(filter_values))

  if (!is.null(filter_by_variable) && !is.null(filter_values) && is.null(compare_rows)) {
    compare_rows <- lapply(filter_values, function(v) {
      if (is.null(v)) 1:nrow(data) else which(data[[filter_by_variable]] == v)
    })
  }

  if (csv_rows && !is.null(filter_by_variable) == FALSE && !is.null(compare_rows)) {
    compare_rows <- lapply(compare_rows, function(rows) rows - 3)
  }

  num_groups <- if (is_comparison) length(compare_rows) else 1

  if (is_comparison) {
    process_group <- function(rows, label) {
      valid_subset <- data[rows, ] %>% filter(!is.na(!!col_sym) & !!col_sym != "" & !!col_sym != "NA")
      total <- nrow(valid_subset)
      group_data <- valid_subset %>%
        mutate(temp_col = str_split(as.character(!!col_sym), ",\\s*")) %>%
        unnest(temp_col) %>%
        mutate(temp_col = as.numeric(trimws(temp_col))) %>%
        filter(!is.na(temp_col)) %>%
        count(temp_col, name = "count") %>%
        mutate(group = label)
      list(data = group_data, total = total)
    }

    group_results <- lapply(seq_len(num_groups), function(i) process_group(compare_rows[[i]], group_labels[i]))
    combined_data <- bind_rows(lapply(group_results, `[[`, "data"))
    totals <- setNames(sapply(group_results, `[[`, "total"), group_labels[1:num_groups])

    final_df <- expand.grid(id = seq_along(label_vec), group = group_labels[1:num_groups], stringsAsFactors = FALSE) %>%
      left_join(combined_data, by = c("id" = "temp_col", "group")) %>%
      mutate(count = coalesce(count, 0L),
             full_label = str_wrap(label_vec[id], width = if(orientation == "vertical") 20 else 30),
             group = factor(group, levels = group_labels[1:num_groups]),
             value = if(show_percentage) (count / totals[group]) * 100 else count)
  } else {
    valid_rows <- data %>% filter(!is.na(!!col_sym) & !!col_sym != "" & !!col_sym != "NA")
    total_respondents <- nrow(valid_rows)
    counts <- valid_rows %>%
      mutate(temp_col = str_split(as.character(!!col_sym), ",\\s*")) %>%
      unnest(temp_col) %>%
      mutate(temp_col = as.numeric(trimws(temp_col))) %>%
      count(temp_col, name = "count")

    final_df <- tibble(id = seq_along(label_vec),
                       full_label = str_wrap(label_vec, width = if(orientation == "vertical") 20 else 30)) %>%
      left_join(counts, by = c("id" = "temp_col")) %>%
      mutate(count = coalesce(count, 0L),
             value = if(show_percentage) (count / total_respondents) * 100 else count)
  }

  # Add 20% headroom above max value so labels are never clipped
  max_limit <- if(show_percentage) 100 else ceiling(max(final_df$value) * 1.15)
  if (is.null(breaks)) breaks <- if(show_percentage) 10 else 1

  final_df$label_text <- if(show_percentage) paste0(round(final_df$value), "%") else as.character(final_df$value)

  dodge_width <- if (num_groups == 3) 0.85 else 0.8

  if (orientation == "horizontal") {
    if (is_comparison) {
      plot <- ggplot(final_df, aes(x = value, y = reorder(full_label, -id), fill = group)) +
        geom_bar(stat = "identity", position = position_dodge(width = dodge_width, reverse = TRUE), width = 0.7) +
        geom_text(aes(label = label_text, group = group),
                  position = position_dodge(width = dodge_width, reverse = TRUE), hjust = -0.3, size = label_text_size)
    } else {
      plot <- ggplot(final_df, aes(x = value, y = reorder(full_label, -id))) +
        geom_bar(stat = "identity", fill = colors[1], width = 0.7) +
        geom_text(aes(label = label_text), hjust = -0.3, size = label_text_size)
    }
  } else {
    if (is_comparison) {
      plot <- ggplot(final_df, aes(x = reorder(full_label, id), y = value, fill = group)) +
        geom_bar(stat = "identity", position = position_dodge(width = dodge_width, reverse = TRUE), width = 0.7) +
        geom_text(aes(label = label_text, group = group),
                  position = position_dodge(width = dodge_width, reverse = TRUE), vjust = -0.3, size = label_text_size)
    } else {
      plot <- ggplot(final_df, aes(x = reorder(full_label, id), y = value)) +
        geom_bar(stat = "identity", fill = colors[1], width = 0.7) +
        geom_text(aes(label = label_text), vjust = -0.3, size = label_text_size)
    }
  }

  plot <- plot +
    scale_fill_manual(values = colors, name = "") +
    labs(title = str_wrap(title, width = title_width), subtitle = subtitle, x = x_label, y = y_label) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = title_font_size, face = "bold", hjust = 0.5),
      legend.position = "top",
      plot.margin = margin(10, 40, 10, 10),
      panel.grid.minor = element_blank()
    )

  if (orientation == "vertical") {
    plot <- plot +
      scale_y_continuous(breaks = seq(0, max_limit, by = breaks), expand = c(0, 0)) +
      coord_cartesian(ylim = c(0, max_limit), clip = "off") +
      scale_x_discrete(guide = guide_axis(angle = 0)) +
      theme(
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(
          angle = if(angle_labels) 45 else 0,
          hjust = if(angle_labels) 1 else 0.5,
          vjust = if(angle_labels) 1 else 0.5,
          color = "black",
          size = axis_text_size
        )
      )
  } else {
    plot <- plot +
      scale_x_continuous(breaks = seq(0, max_limit, by = breaks), expand = c(0, 0)) +
      coord_cartesian(xlim = c(0, max_limit), clip = "off") +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "black", size = axis_text_size),
        axis.text.x = element_text(color = "black", size = axis_text_size)
      )
  }

  return(plot)
}