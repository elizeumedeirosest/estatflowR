# ─────────────────────────────────────────────────────────────────────────────
# MÓDULO VISUAL: TEMA APPLE & MOTOR GRÁFICO COM GRADE DUPLA EMBUTIDA
# ─────────────────────────────────────────────────────────────────────────────

#' Tema Minimalista Apple com Grade Dupla Nativa
#' @description Tema editorial limpo e elegante baseado no sistema de design Apple,
#'   com linhas de grade dupla (maior e menor) suaves embutidas por padrão.
#' @param base_size Tamanho base da fonte (padrão 11).
#' @param base_family Família da fonte (padrão Segoe UI ou sans-serif do sistema).
#' @return Objeto de tema ggplot2.
#' @noRd
.tema_apple <- function(base_size = 13, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Fundo branco puro
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", color = NA),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", color = NA),

      # Grade Suave de Alta Legibilidade
      panel.grid.major = ggplot2::element_line(color = "#E5E5EA", linewidth = 0.55),
      panel.grid.minor = ggplot2::element_line(color = "#F2F2F7", linewidth = 0.35, linetype = "dotted"),

      # Eixos limpos e com fontes nítidas
      axis.line = ggplot2::element_line(color = "#C7C7CC", linewidth = 0.6),
      axis.ticks = ggplot2::element_line(color = "#C7C7CC", linewidth = 0.5),
      axis.text = ggplot2::element_text(color = "#1D1D1F", size = 11),
      axis.title = ggplot2::element_text(color = "#1D1D1F", face = "bold", size = 12.5, margin = ggplot2::margin(t = 6, r = 6)),

      # Títulos e Subtítulos
      plot.title = ggplot2::element_text(color = "#1D1D1F", face = "bold", size = 14, margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(color = "#6E6E73", size = 11.5, margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(color = "#86868B", size = 9.5, margin = ggplot2::margin(t = 8)),

      # Legenda elegante no topo
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_text(color = "#1D1D1F", face = "bold", size = 11),
      legend.text = ggplot2::element_text(color = "#1D1D1F", size = 10.5),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),

      # Facetas
      strip.background = ggplot2::element_rect(fill = "#F8F9FA", color = "#E5E5EA", linewidth = 0.5),
      strip.text = ggplot2::element_text(color = "#1D1D1F", face = "bold", size = 11, margin = ggplot2::margin(t = 4, b = 4))
    )
}

#' Paleta de Cores Apple
#' @noRd
.cores_apple <- c(
  azul      = "#0071E3",
  verde     = "#34C759",
  laranja   = "#FF9500",
  vermelho  = "#FF3B30",
  roxo      = "#AF52DE",
  cinza_esc = "#1D1D1F",
  cinza_med = "#86868B",
  cinza_cla = "#E5E5EA"
)

#' Conversor Plotly com Tooltips Formatados e Barra Minimalista
#' @param p Objeto ggplot2
#' @param tooltip_var Nome da variável para tooltip
#' @return Objeto plotly
#' @noRd
.converter_plotly <- function(p, tooltip = "all") {
  if (!requireNamespace("plotly", quietly = TRUE)) return(p)

  ply <- tryCatch({
    plotly::ggplotly(p, tooltip = tooltip) |>
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "sendDataToCloud", "hoverCompareCartesian", "hoverClosestCartesian",
          "toggleSpikelines", "lasso2d", "select2d"
        ),
        toImageButtonOptions = list(
          format = "png",
          filename = "grafico_estatflowR",
          height = 600,
          width = 900,
          scale = 2
        )
      ) |>
      plotly::layout(
        font = list(family = "-apple-system, BlinkMacSystemFont, 'SF Pro Text', 'Inter', 'Segoe UI', sans-serif", size = 12.5),
        hoverlabel = list(
          bgcolor = "#1D1D1F",
          font = list(color = "#FFFFFF", family = "-apple-system, 'SF Pro Text', sans-serif", size = 12.5),
          bordercolor = "transparent"
        ),
        xaxis = list(tickfont = list(size = 11.5), titlefont = list(size = 12.5)),
        yaxis = list(tickfont = list(size = 11.5), titlefont = list(size = 12.5))
      )
  }, error = function(e) p)

  ply
}

#' Formatador numérico com vírgula decimal
#' @noRd
.fmt_num <- function(x, d = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  formatC(as.numeric(x), format = "f", digits = d, decimal.mark = ",")
}

#' Formatador percentual com vírgula decimal
#' @noRd
.fmt_pct <- function(x, d = 1) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  paste0(formatC(as.numeric(x), format = "f", digits = d, decimal.mark = ","), "%")
}
