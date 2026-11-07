# ─────────────────────────────────────────────────────────────────────────────
# MÓDULO: MATRIZ EXPLORATÓRIA MULTIVARIADA (PAIRS PLOT)
# Baseado na arquitetura do grafico_exploratorio() com grade dupla nativa
# ─────────────────────────────────────────────────────────────────────────────

#' Constrói a Matriz Exploratória Multivariada
#' @param data data.frame
#' @param variaveis Vetor de nomes de variáveis numéricas (2 a 6)
#' @param grupo Coluna de agrupamento opcional (fator ou categórica)
#' @return Objeto ggplot2 montado com patchwork
#' @noRd
.gerar_matriz_exploratoria <- function(data, variaveis, grupo = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    # Fallback para correlograma simples se patchwork não estiver disponível
    return(.gerar_correlograma(data, variaveis))
  }

  vars <- variaveis[variaveis %in% names(data)]
  vars <- vars[vapply(data[vars], is.numeric, logical(1))]
  n_vars <- length(vars)

  if (n_vars < 2) return(NULL)
  if (n_vars > 6) vars <- vars[1:6] # Limite para clareza visual

  tem_grupo <- !is.null(grupo) && grupo %in% names(data) && !all(is.na(data[[grupo]]))
  if (tem_grupo) {
    data[[grupo]] <- as.factor(data[[grupo]])
  }

  plots <- vector("list", n_vars * n_vars)

  for (i in seq_len(n_vars)) {
    for (j in seq_len(n_vars)) {
      idx <- (i - 1) * n_vars + j
      var_y <- vars[i]
      var_x <- vars[j]

      if (i == j) {
        # ── DIAGONAL: DENSIDADE ─────────────────────────────────────────────
        p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[var_x]]))
        if (tem_grupo) {
          p <- p +
            ggplot2::geom_density(ggplot2::aes(fill = .data[[grupo]], color = .data[[grupo]]),
                                  alpha = 0.35, linewidth = 0.8) +
            ggplot2::scale_fill_brewer(palette = "Set1") +
            ggplot2::scale_color_brewer(palette = "Set1")
        } else {
          p <- p +
            ggplot2::geom_density(fill = "#0071E3", color = "#0071E3", alpha = 0.25, linewidth = 0.9)
        }
        p <- p +
          ggplot2::labs(x = var_x, y = NULL) +
          .tema_apple(base_size = 9) +
          ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank(),
                         plot.title = ggplot2::element_blank())

      } else if (i > j) {
        # ── TRIÂNGULO INFERIOR: DISPERSÃO COM LINHA SUAVE ───────────────────
        p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[var_x]], y = .data[[var_y]]))
        if (tem_grupo) {
          p <- p +
            ggplot2::geom_point(ggplot2::aes(color = .data[[grupo]]), alpha = 0.65, size = 1.8) +
            ggplot2::scale_color_brewer(palette = "Set1")
        } else {
          p <- p +
            ggplot2::geom_point(color = "#0071E3", alpha = 0.6, size = 1.8) +
            ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#1D1D1F", linewidth = 0.7, formula = y ~ x)
        }
        p <- p +
          .tema_apple(base_size = 9) +
          ggplot2::labs(x = var_x, y = var_y)

      } else {
        # ── TRIÂNGULO SUPERIOR: COEFICIENTE DE CORRELAÇÃO ───────────────────
        v1 <- stats::na.omit(data[[var_x]])
        v2 <- stats::na.omit(data[[var_y]])
        ok <- !is.na(data[[var_x]]) & !is.na(data[[var_y]])
        r_val <- stats::cor(data[[var_x]][ok], data[[var_y]][ok])
        test_r <- stats::cor.test(data[[var_x]][ok], data[[var_y]][ok])
        p_val <- test_r$p.value
        ast <- if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else ""

        cor_texto <- if (abs(r_val) >= 0.7) "#0071E3" else if (abs(r_val) >= 0.4) "#34C759" else "#86868B"

        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.55,
                            label = paste0("r = ", .fmt_num(r_val)),
                            fontface = "bold", size = 4.2, color = cor_texto) +
          ggplot2::annotate("text", x = 0.5, y = 0.35,
                            label = paste0("p ", if (p_val < 0.001) "< 0,001" else paste0("= ", .fmt_num(p_val, 3)), " ", ast),
                            size = 3.2, color = "#86868B") +
          ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
          ggplot2::theme_void() +
          ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#FAFAFC", color = "#E5E5EA", linewidth = 0.4))
      }

      p <- p + ggplot2::theme(legend.position = "none")
      plots[[idx]] <- p
    }
  }

  patchwork::wrap_plots(plots, nrow = n_vars, ncol = n_vars)
}

#' Correlograma de Fallback
#' @noRd
.gerar_correlograma <- function(data, variaveis) {
  df_sub <- stats::na.omit(data[variaveis])
  mat_cor <- stats::cor(df_sub)
  df_c <- as.data.frame(as.table(mat_cor))
  names(df_c) <- c("Var1", "Var2", "r")

  ggplot2::ggplot(df_c, ggplot2::aes(x = Var1, y = Var2, fill = r)) +
    ggplot2::geom_tile(color = "#FFFFFF", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = .fmt_num(r)), color = "#1D1D1F", size = 3.8, fontface = "bold") +
    ggplot2::scale_fill_gradient2(low = "#FF3B30", mid = "#FFFFFF", high = "#0071E3", midpoint = 0, limits = c(-1, 1)) +
    .tema_apple() +
    ggplot2::labs(title = "Matriz de Correlação Linear (Pearson)", x = NULL, y = NULL, fill = "Correlação (r)")
}
