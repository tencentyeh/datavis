#' @title Bar Plot Multi Filter
#' @description Creates a bar chart where groups can come from different filter variables.
#'   This is an extension of bar_plot_simple that accepts a `groups` list, where each group
#'   defines its own optional filter variable and filter value. This allows you to combine
#'   segments from different questions (e.g. Q2 and Q3) into one chart.
#'
#' @param data_source A data frame loaded from a Qualtrics CSV.
#' @param selected_variables Character vector of column names to plot.
#' @param title Plot title.
#' @param groups A named list of group definitions. Each element is a list with:
#'   \describe{
#'     \item{label}{Display label for the group, e.g. "Overall (n=48)"}
#'     \item{filter_variable}{(optional) Variable name to filter on, e.g. "Q2". NULL = all rows.}
#'     \item{filter_value}{(optional) Value to filter for, e.g. 1. NULL = all rows.}
#'     \item{color}{(optional) Bar color for this group.}
#'   }
#'   Example:
#'   \code{
#'   groups = list(
#'     list(label = "Overall (n=48)",                filter_variable = NULL, filter_value = NULL),
#'     list(label = "Bayonetta Players (n=12)",      filter_variable = "Q3", filter_value = 1),
#'     list(label = "Non-Bayonetta Players (n=12)",  filter_variable = "Q3", filter_value = 2),
#'     list(label = "USA (n=24)",                    filter_variable = "Q2", filter_value = 1),
#'     list(label = "China (n=24)",                  filter_variable = "Q2", filter_value = 2)
#'   )
#'   }
#' @param x_range Numeric vector of length 2 for the x-axis range. Default c(1, 5).
#' @param chunk_size Number of variables per plot chunk. Default 5.
#' @param title_width Integer for title wrapping width. Default 50.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param subtitle Plot subtitle.
#' @param subtitle2 Second plot subtitle.
#' @param diverging Logical. Whether to use a diverging scale. Default FALSE.
#' @param center_value Center value for diverging scale. Default 3.
#' @param colors Default color palette (used when group does not specify its own color).
#' @param orientation Either "horizontal" or "vertical". Default "horizontal".
#' @param angle_labels Logical. Whether to angle x-axis labels. Default TRUE.
#' @param sort_by Either "original" or "value". Default "original".
#' @param show_y_labels Logical. Whether to show y-axis labels. Default TRUE.
#' @return A list of ggplot objects.
#' @export
bar_plot_multi_filter <- function(
    data_source,
    selected_variables,
    title,
    groups,                          # <-- the key new parameter
    x_range      = c(1, 5),
    chunk_size   = 5,
    title_width  = 50,
    x_label      = NULL,
    y_label      = NULL,
    subtitle     = NULL,
    subtitle2    = NULL,
    diverging    = FALSE,
    center_value = 3,
    colors       = c("#4f74dd", "#efac51", "#d24d77", "#5db693", "#bd73b0"),
    orientation  = "horizontal",
    angle_labels = TRUE,
    sort_by      = "original",
    show_y_labels = TRUE
) {
  # ── Validate inputs ──────────────────────────────────────────────────────────
  orientation <- tolower(orientation)
  if (!orientation %in% c("horizontal", "vertical"))
    stop("orientation must be 'horizontal' or 'vertical'")
  
  sort_by <- tolower(sort_by)
  if (!sort_by %in% c("original", "value"))
    stop("sort_by must be 'original' or 'value'")
  
  if (!is.list(groups) || length(groups) < 2)
    stop("groups must be a list with at least 2 group definitions")
  
  # ── Process raw Qualtrics dataframe ─────────────────────────────────────────
  processed     <- process_data_with_labels(data_source)
  data          <- processed$data
  label_mapping <- processed$labels
  
  num_groups <- length(groups)
  
  # ── Build per-group summary ──────────────────────────────────────────────────
  compute_group_summary <- function(group_def, idx) {
    label    <- group_def$label
    fvar     <- group_def$filter_variable
    fval     <- group_def$filter_value
    
    # Select rows
    if (is.null(fvar) || is.null(fval)) {
      row_subset <- seq_len(nrow(data))
    } else {
      if (!fvar %in% names(data))
        stop(paste("Filter variable", fvar, "not found in data"))
      row_subset <- which(data[[fvar]] == fval)
    }
    
    data[row_subset, ] %>%
      select(all_of(selected_variables)) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
      group_by(variable) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        se         = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
        ci_lower   = mean_value - qt(0.975, df = max(sum(!is.na(value)) - 1, 1)) * se,
        ci_upper   = mean_value + qt(0.975, df = max(sum(!is.na(value)) - 1, 1)) * se,
        .groups    = "drop"
      ) %>%
      mutate(group = label, group_idx = idx)
  }
  
  summary_data <- purrr::map2_dfr(groups, seq_along(groups), compute_group_summary)
  
  # ── Build ordered label vector for factor levels ────────────────────────────
  group_label_vec <- sapply(groups, function(g) g$label)
  
  # ── Build color vector ───────────────────────────────────────────────────────
  group_colors <- sapply(seq_along(groups), function(i) {
    grp_color <- groups[[i]]$color
    if (!is.null(grp_color)) grp_color else colors[i]
  })
  names(group_colors) <- group_label_vec
  
  # ── Add original order and recode variable labels ────────────────────────────
  summary_data <- summary_data %>%
    mutate(
      original_order = match(variable, selected_variables),
      variable       = recode(variable, !!!label_mapping) %>%
        str_wrap(width = if (diverging) 30 else 25),
      group          = factor(group, levels = rev(group_label_vec))
    )
  
  # ── Sorting & chunking ───────────────────────────────────────────────────────
  if (sort_by == "value") {
    sort_order <- summary_data %>%
      group_by(variable, original_order) %>%
      summarise(avg_mean = mean(mean_value, na.rm = TRUE), .groups = "drop") %>%
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
    summary_data <- summary_data %>%
      mutate(chunk = ceiling(original_order / chunk_size))
  }
  
  # ── Diverging adjustment ─────────────────────────────────────────────────────
  if (diverging) {
    summary_data <- summary_data %>%
      mutate(diverging_value = mean_value - center_value)
  }
  
  # ── Titles ───────────────────────────────────────────────────────────────────
  wrapped_title <- stringr::str_wrap(title, width = title_width)
  
  combined_subtitle <- NULL
  if (!is.null(subtitle) && !is.null(subtitle2)) {
    combined_subtitle <- paste0(subtitle, "\n", subtitle2)
  } else if (!is.null(subtitle)) {
    combined_subtitle <- subtitle
  } else if (!is.null(subtitle2)) {
    combined_subtitle <- subtitle2
  }
  
  # ── Build plots ──────────────────────────────────────────────────────────────
  summary_chunks <- group_split(summary_data, chunk)
  
  plots <- lapply(summary_chunks, function(cur_chunk) {
    
    if (diverging) {
      # ── Diverging + compare ──
      if (orientation == "horizontal") {
        plot <- ggplot(cur_chunk, aes(x = diverging_value, y = variable, fill = group)) +
          geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
          geom_errorbar(
            aes(xmin = ci_lower - center_value, xmax = ci_upper - center_value),
            position = position_dodge(width = 0.8),
            width = 0.2, colour = "black", linewidth = 0.5
          ) +
          geom_text(
            aes(label = round(mean_value, 2),
                hjust = ifelse(diverging_value < 0, 1.3, -0.3)),
            position = position_dodge(width = 0.8),
            size = 4, color = "black"
          ) +
          scale_x_continuous(
            breaks = seq(x_range[1] - center_value, x_range[2] - center_value, by = 1),
            labels = seq(x_range[1], x_range[2], by = 1),
            expand = c(0, 0)
          ) +
          coord_cartesian(
            xlim = c(x_range[1] - center_value - 0.5, x_range[2] - center_value + 0.7),
            clip = "on"
          )
      } else {
        plot <- ggplot(cur_chunk, aes(x = variable, y = diverging_value, fill = group)) +
          geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
          geom_errorbar(
            aes(ymin = ci_lower - center_value, ymax = ci_upper - center_value),
            position = position_dodge(width = 0.8),
            width = 0.2, colour = "black", linewidth = 0.5
          ) +
          geom_text(
            aes(label = round(mean_value, 2),
                vjust = ifelse(diverging_value < 0, 1.3, -0.3)),
            position = position_dodge(width = 0.8),
            size = 4, color = "black"
          ) +
          scale_y_continuous(
            breaks = seq(x_range[1] - center_value, x_range[2] - center_value, by = 1),
            labels = seq(x_range[1], x_range[2], by = 1),
            expand = c(0, 0)
          ) +
          coord_cartesian(
            ylim = c(x_range[1] - center_value - 0.5, x_range[2] - center_value + 0.7),
            clip = "on"
          )
      }
      
    } else {
      # ── Standard + compare ──
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
          scale_x_continuous(
            breaks = seq(x_range[1], x_range[2], by = 1), expand = c(0, 0)
          ) +
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
          scale_y_continuous(
            breaks = seq(x_range[1], x_range[2], by = 1), expand = c(0, 0)
          ) +
          coord_cartesian(ylim = c(x_range[1], x_range[2] + 0.5), clip = "on")
      }
    }
    
    # ── Shared styling ──────────────────────────────────────────────────────────
    plot <- plot +
      scale_fill_manual(
        values = group_colors,
        breaks = rev(group_label_vec)   # legend top-to-bottom matches bar order
      ) +
      guides(fill = guide_legend(reverse = TRUE)) +
      labs(
        title    = wrapped_title,
        subtitle = combined_subtitle,
        x        = x_label,
        y        = y_label,
        fill     = ""
      ) +
      theme_minimal() +
      theme(
        plot.title       = element_text(size = 18, face = "bold", hjust = 0.5, colour = "black"),
        plot.subtitle    = element_text(size = 10, hjust = 0.5, colour = "black"),
        legend.position  = "top",
        panel.grid.minor = element_blank(),
        plot.margin      = margin(10, 40, 10, 10)
      )
    
    if (orientation == "horizontal") {
      plot <- plot + theme(
        axis.text.x  = element_text(color = "black", size = 12),
        axis.text.y  = if (show_y_labels) element_text(color = "black", size = 10) else element_blank(),
        axis.ticks.y = if (show_y_labels) element_line() else element_blank(),
        panel.grid.major.y = element_blank()
      )
    } else {
      plot <- plot + theme(
        axis.text.x = if (show_y_labels) element_text(
          color = "black", size = 10,
          angle = if (angle_labels) 45 else 0,
          hjust = if (angle_labels) 1 else 0.5
        ) else element_blank(),
        axis.ticks.x = if (show_y_labels) element_line() else element_blank(),
        axis.text.y  = element_text(color = "black", size = 12),
        panel.grid.major.x = element_blank()
      )
    }
    
    return(plot)
  })
  
  return(plots)
}


# ══════════════════════════════════════════════════════════════════════════════
# USAGE EXAMPLE
# ══════════════════════════════════════════════════════════════════════════════
#
# plot_combined <- bar_plot_multi_filter(
#   data_source        = df,
#   selected_variables = c("Q9"),
#   title              = "What is your impression of the new game?",
#   subtitle           = "1 I really dislike it --> 5 I really like it",
#   subtitle2          = "Average out of 5",
#   show_y_labels      = FALSE,
#   diverging          = TRUE,
#   groups = list(
#     list(label = "Overall (n=48)",               filter_variable = NULL, filter_value = NULL,  color = "#4f74dd"),
#     list(label = "Bayonetta Players (n=12)",      filter_variable = "Q3", filter_value = 1,    color = "#5db693"),
#     list(label = "Non-Bayonetta Players (n=12)",  filter_variable = "Q3", filter_value = 2,    color = "#bd73b0"),
#     list(label = "USA (n=24)",                    filter_variable = "Q2", filter_value = 1,    color = "#efac51"),
#     list(label = "China (n=24)",                  filter_variable = "Q2", filter_value = 2,    color = "#d24d77")
#   )
# )
# print(plot_combined)