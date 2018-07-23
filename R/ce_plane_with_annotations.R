
#' ce_plane_with_annotations
#'
#' @return
#' @export
#'
#' @examples
ce_plane_with_annotations <- function() {

  # ceplane.plot(screen.bcea, pos = "bottomright")
  # contour(screen.bcea)
  # gg <- ceplane.plot(screen.bcea, graph = "ggplot2")
  # gg + scale_colour_manual(values = cbPalette)

  # with annotations

  # gg +
  #   # scale_color_brewer(palette = "Dark2") +
  #   # scale_colour_manual(values = cbPalette) +
  #   # xlim(0, 0.008) +
  #   scale_color_discrete(labels = SCENARIO_LABELS) +
  #   annotate("text",
  #            x = apply(screen.bcea$delta.e, 2, mean),
  #            y = apply(screen.bcea$delta.c, 2, mean),
  #            label = seq_along(SCENARIO_LABELS)) +
  #   theme(legend.position = c(1, 0.2))

  # with ICER values
  # gg + annotate("text",
  #            x = apply(screen.bcea$delta.e, 2, mean),
  #            y = apply(screen.bcea$delta.c, 2, mean),
  #            label = round(screen.bcea$ICER, 0))
}
