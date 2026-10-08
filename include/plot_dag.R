# Draws a DAG in dagitty.net's style: node colors (and a marker for exposure
# and outcome) show each variable's status, arrow colors show which paths
# between exposure and outcome are open given the variables marked
# [adjusted] in the DAG's model code. labels: a tibble with columns name and
# label; without it, nodes are labelled with their names.
plot_dag <- function(dag, labels = NULL) {
  if (is.null(labels))
    labels <- tibble(name = names(dag), label = names(dag))

  adjusted <- adjustedNodes(dag)

  open_paths <- dag |>
    paths(Z = adjusted) |>
    as_tibble() |>
    filter(open) |>
    mutate(path_type = if_else(str_detect(paths, "<-"), "Open biasing path", "Causal path"))

  # split each path ("a -> b <- c") into its single arrows
  path_edges <- open_paths |>
    mutate(
      path_id = row_number(),
      token   = str_split(paths, " ")
    ) |>
    unnest_longer(token) |>
    group_by(path_id) |>
    mutate(
      arrow     = lead(token),
      next_node = lead(token, 2)
    ) |>
    ungroup() |>
    filter(arrow %in% c("->", "<-")) |>
    mutate(
      name = if_else(arrow == "->", token, next_node),
      to   = if_else(arrow == "->", next_node, token)
    ) |>
    distinct(name, to, path_type)

  dag_data <- dag |>
    tidy_dagitty() |>
    pull_dag_data() |>
    left_join(labels, by = "name") |>
    left_join(path_edges, by = c("name", "to")) |>
    mutate(
      edge_type = replace_na(path_type, "Other relation"),
      status    = case_when(
        name %in% exposures(dag) ~ "Exposure",
        name %in% outcomes(dag)  ~ "Outcome",
        name %in% adjusted       ~ "Controlled for",
        name %in% latents(dag)   ~ "Unobserved",
        .default                 = "Not controlled for"
      )
    )

  status_limits <- c("Exposure", "Outcome", "Controlled for", "Not controlled for", "Unobserved")

  # exposure and outcome markers in the middle of the node
  markers <- dag_data |>
    distinct(name, x, y, status) |>
    filter(status %in% c("Exposure", "Outcome")) |>
    mutate(marker = if_else(status == "Exposure", "\u25B6", "\u2503"))

  dag_data |>
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    white_arrowheads(geom_dag_edges_link(
      aes(edge_colour = edge_type),
      edge_width  = 0.8,
      show.legend = TRUE
    )) +
    geom_dag_point(
      aes(fill = status, colour = status),
      shape       = 21,
      size        = 14,
      stroke      = 1,
      show.legend = c(fill = TRUE, colour = TRUE, edge_colour = FALSE)
    ) +
    geom_text(
      data        = markers,
      aes(x = x, y = y, label = marker),
      size        = 3.6,
      inherit.aes = FALSE
    ) +
    geom_dag_label(
      aes(label = label),
      fill      = "white",
      linewidth = 0,
      size      = 3.6,
      nudge_y   = -0.4
    ) +
    scale_y_reverse(expand = expansion(mult = 0.15)) +
    scale_x_continuous(expand = expansion(mult = 0.15)) +
    scale_fill_manual(
      limits = status_limits,
      values = c(
        "Exposure"           = "#d1e14e",
        "Outcome"            = "#4cbee9",
        "Controlled for"     = "#fafafa",
        "Not controlled for" = "#c3c3c3",
        "Unobserved"         = "#eeeeee"
      )
    ) +
    scale_colour_manual(
      limits = status_limits,
      values = c(
        "Exposure"           = "black",
        "Outcome"            = "black",
        "Controlled for"     = "black",
        "Not controlled for" = "#a8a8a8",
        "Unobserved"         = "#a7a7a7"
      )
    ) +
    ggraph::scale_edge_colour_manual(
      limits = c("Causal path", "Open biasing path", "Other relation"),
      values = c(
        "Causal path"       = "#4dac26",
        "Open biasing path" = "#d01c8b",
        "Other relation"    = "grey55"
      )
    ) +
    guides(
      fill        = guide_legend(nrow = 2, order = 1, override.aes = list(size = 7)),
      colour      = guide_legend(nrow = 2, order = 1),
      edge_colour = guide_legend(order = 2)
    ) +
    labs(
      fill        = NULL,
      colour      = NULL,
      edge_colour = NULL
    ) +
    theme_dag(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.box      = "vertical"
    )
}

# ggraph fills arrowheads (also in the legend) with the edge colour; dagitty.net leaves them white
white_arrowheads <- function(edge_layer) {
  parent <- edge_layer$geom

  set_fill <- function(grob) {
    if (!is.null(grob$gp))
      grob$gp$fill <- "white"
    if (inherits(grob, "gTree"))
      grob <- grid::setChildren(grob, do.call(grid::gList, map(grob$children, set_fill)))
    grob
  }

  edge_layer$geom <- ggproto(
    NULL, parent,
    draw_layer = function(self, data, params, layout, coord) {
      ggproto_parent(parent, self)$draw_layer(data, params, layout, coord) |>
        map(set_fill)
    },
    draw_key = function(data, params, size) {
      parent$draw_key(data, params, size) |>
        set_fill()
    }
  )
  edge_layer
}
