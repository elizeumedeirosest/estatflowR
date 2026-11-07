# ─────────────────────────────────────────────────────────────────────────────
# MÓDULO SERVER: LÓGICA REATIVA E DIAGNÓSTICOS DO ESTATFLOWR
# ─────────────────────────────────────────────────────────────────────────────

#' Cria o Servidor (Server) do estatflowR
#' @return Função server Shiny
#' @noRd
.criar_app_server <- function(dados_iniciais = NULL, nome_dataset = "dados") {

  function(input, output, session) {

    # ── ESTADO REATIVO DO DATASET ──────────────────────────────────────────
    dados_padrao <- if (!is.null(dados_iniciais)) {
      as.data.frame(dados_iniciais)
    } else {
      .obter_dados_exemplo("saude")
    }

    rv <- shiny::reactiveValues(
      dados        = dados_padrao,
      nome_dataset = if (!is.null(dados_iniciais)) nome_dataset else "dados_saude_exemplo"
    )

    # ── MODAL DE UPLOAD DE ARQUIVOS ────────────────────────────────────────
    shiny::observeEvent(input$btn_abrir_upload, {
      shiny::showModal(shiny::modalDialog(
        title = htmltools::tags$strong("Importar Arquivo de Dados"),
        htmltools::div(
          class = "text-center p-3",
          shiny::fileInput(
            "arq_upload", "Escolha um arquivo (.csv, .xlsx, .rds, .parquet):",
            multiple = FALSE,
            accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".rds", ".parquet", ".dta", ".sav"),
            buttonLabel = "Procurar...", placeholder = "Nenhum arquivo selecionado"
          ),
          htmltools::p(class = "apple-card-subtitle", "Detecção automática de separadores e codificação UTF-8.")
        ),
        footer = htmltools::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton("btn_confirmar_upload", "Carregar no App", class = "btn-apple")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    })

    shiny::observeEvent(input$btn_confirmar_upload, {
      shiny::req(input$arq_upload)
      caminho <- input$arq_upload$datapath
      nome_original <- tools::file_path_sans_ext(input$arq_upload$name)
      df_lido <- .ler_dados_universal(caminho)

      if (!is.null(df_lido) && nrow(df_lido) > 0) {
        rv$dados <- df_lido
        rv$nome_dataset <- nome_original
        shiny::removeModal()
      } else {
        shiny::showNotification("Não foi possível ler os dados do arquivo selecionado.", type = "error")
      }
    })

    # ── MODAL DE DATASETS DE EXEMPLO ───────────────────────────────────────
    shiny::observeEvent(input$btn_dados_demo, {
      shiny::showModal(shiny::modalDialog(
        title = htmltools::tags$strong("Carregar Dataset de Exemplo"),
        htmltools::div(
          class = "p-2",
          shiny::radioButtons(
            "sel_dados_demo", "Selecione o cenário:",
            choices = c(
              "Estudo de Saúde e Fatores Clínicos (120 pacientes)" = "saude",
              "Controle de Qualidade de Produção (90 peças / 18 lotes)" = "qualidade",
              "Vendas, Clientes e Faturamento (100 clientes)" = "vendas"
            ),
            selected = "saude"
          )
        ),
        footer = htmltools::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton("btn_confirmar_demo", "Carregar Dataset", class = "btn-apple")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    })

    shiny::observeEvent(input$btn_confirmar_demo, {
      nome_demo <- input$sel_dados_demo
      rv$dados <- .obter_dados_exemplo(nome_demo)
      rv$nome_dataset <- paste0("dados_", nome_demo)
      shiny::removeModal()
    })

    # ── ATUALIZAÇÃO DOS SELETORES DINÂMICOS ─────────────────────────────────
    shiny::observe({
      df <- rv$dados
      shiny::req(df)
      cols <- names(df)
      num_cols <- cols[vapply(df, is.numeric, logical(1))]
      cat_cols <- cols[!vapply(df, is.numeric, logical(1))]

      # Univariada
      shiny::updateSelectInput(session, "sel_var_univariada", choices = cols, selected = cols[1])

      # Bivariada
      shiny::updateSelectInput(session, "sel_biv_x", choices = cols, selected = cols[1])
      shiny::updateSelectInput(session, "sel_biv_y", choices = cols, selected = if (length(cols) > 1) cols[2] else cols[1])
      shiny::updateSelectInput(session, "sel_biv_grupo", choices = c("Nenhum" = "", cat_cols))

      # Matriz
      shiny::updateSelectizeInput(session, "sel_matriz_vars", choices = num_cols, selected = head(num_cols, 4))
      shiny::updateSelectInput(session, "sel_matriz_grupo", choices = c("Nenhum" = "", cat_cols))
    })

    # Badge do Dataset no Header
    output$ui_dataset_badge <- shiny::renderUI({
      htmltools::tags$span(
        class = "badge bg-light text-dark border",
        style = "font-size: 0.9rem; font-weight: 600; padding: 0.4rem 0.75rem;",
        rv$nome_dataset
      )
    })

    # ── METADADOS E TABELA DE VARIÁVEIS (ABA 1) ────────────────────────────
    diag_vars <- shiny::reactive({
      .diagnosticar_variaveis(rv$dados)
    })

    output$ui_kpis_dataset <- shiny::renderUI({
      df <- rv$dados
      shiny::req(df)
      n_lin <- nrow(df)
      n_col <- ncol(df)
      total_na <- sum(is.na(df))
      pct_na <- if (n_lin * n_col > 0) (total_na / (n_lin * n_col)) * 100 else 0
      n_num <- sum(vapply(df, is.numeric, logical(1)))
      n_cat <- n_col - n_num

      htmltools::div(
        class = "kpi-container",
        htmltools::div(
          class = "kpi-card",
          htmltools::div(class = "kpi-title", "Observações"),
          htmltools::div(class = "kpi-value", format(n_lin, big.mark = ".")),
          htmltools::div(class = "kpi-desc", "Total de linhas no dataset")
        ),
        htmltools::div(
          class = "kpi-card",
          htmltools::div(class = "kpi-title", "Variáveis"),
          htmltools::div(class = "kpi-value", n_col),
          htmltools::div(class = "kpi-desc", sprintf("%d numéricas | %d categóricas", n_num, n_cat))
        ),
        htmltools::div(
          class = "kpi-card",
          htmltools::div(class = "kpi-title", "Dados Ausentes (NAs)"),
          htmltools::div(
            class = "kpi-value",
            style = if (total_na > 0) "color: #FF9500;" else "color: #34C759;",
            .fmt_pct(pct_na, 1)
          ),
          htmltools::div(class = "kpi-desc", sprintf("%d células ausentes", total_na))
        ),
        htmltools::div(
          class = "kpi-card",
          htmltools::div(class = "kpi-title", "Completude Global"),
          htmltools::div(class = "kpi-value", .fmt_pct(100 - pct_na, 1)),
          htmltools::div(class = "kpi-desc", "Índice de dados válidos")
        )
      )
    })

    output$tab_diagnostico_vars <- DT::renderDT({
      tb <- diag_vars()
      shiny::req(tb)

      tb_formatada <- tb
      tb_formatada$Pct_Ausentes <- paste0(.fmt_num(tb$Pct_Ausentes, 1), "%")

      DT::datatable(
        tb_formatada[, c("Variavel", "Tipo", "Validos", "Ausentes", "Pct_Ausentes", "Resumo")],
        rownames = FALSE,
        colnames = c("Variável", "Tipo Detectado", "Válidos", "NAs", "% NAs", "Resumo Estatístico"),
        options = list(
          pageLength = 10,
          dom = "tip",
          language = list(
            search = "Filtrar variável:",
            paginate = list(previous = "Anterior", `next` = "Próximo"),
            info = "Mostrando _START_ a _END_ de _TOTAL_ colunas",
            infoEmpty = "Nenhuma variável encontrada"
          )
        )
      )
    })

    # ── ABA 2: ANÁLISE UNIVARIADA ──────────────────────────────────────────
    var_univariada_info <- shiny::reactive({
      col <- input$sel_var_univariada
      df <- rv$dados
      shiny::req(df, col, col %in% names(df))
      v <- df[[col]]
      list(nome = col, valores = v, is_num = is.numeric(v))
    })

    output$ui_controles_univariada <- shiny::renderUI({
      info <- var_univariada_info()
      if (is.null(info)) return(NULL)

      if (info$is_num) {
        shiny::radioButtons(
          "tipo_graf_num", "Estilo do Gráfico:",
          choices = c("Distribuição (Histograma + Densidade)" = "hist",
                      "Boxplot com Dispersão (Jitter)" = "box"),
          selected = "hist"
        )
      } else {
        htmltools::p(class = "apple-card-subtitle", "Exibindo gráfico de frequências relativas.")
      }
    })

    output$tit_grafico_univariada <- shiny::renderText({
      info <- var_univariada_info()
      if (is.null(info)) return("Visualização da Variável")
      sprintf("Distribuição: %s", info$nome)
    })

    output$ui_kpis_univariada <- shiny::renderUI({
      info <- var_univariada_info()
      if (is.null(info)) return(NULL)
      v <- stats::na.omit(info$valores)

      if (info$is_num && length(v) > 0) {
        m <- mean(v)
        md <- stats::median(v)
        dp <- stats::sd(v)
        iqr_val <- stats::IQR(v)

        htmltools::div(
          class = "kpi-container",
          htmltools::div(class = "kpi-card",
                         htmltools::div(class = "kpi-title", "Média"),
                         htmltools::div(class = "kpi-value", .fmt_num(m))),
          htmltools::div(class = "kpi-card",
                         htmltools::div(class = "kpi-title", "Mediana"),
                         htmltools::div(class = "kpi-value", .fmt_num(md))),
          htmltools::div(class = "kpi-card",
                         htmltools::div(class = "kpi-title", "Desvio-Padrão"),
                         htmltools::div(class = "kpi-value", .fmt_num(dp))),
          htmltools::div(class = "kpi-card",
                         htmltools::div(class = "kpi-title", "Amplitude Interquartil (IQR)"),
                         htmltools::div(class = "kpi-value", .fmt_num(iqr_val)))
        )
      } else if (length(v) > 0) {
        tb <- sort(table(v), decreasing = TRUE)
        moda_nome <- names(tb)[1]
        moda_pct <- (as.numeric(tb[1]) / length(v)) * 100

        htmltools::div(
          class = "kpi-container",
          htmltools::div(class = "kpi-card",
                         htmltools::div(class = "kpi-title", "Categorias Únicas"),
                         htmltools::div(class = "kpi-value", length(tb))),
          htmltools::div(class = "kpi-card",
                         htmltools::div(class = "kpi-title", "Moda (Mais Frequente)"),
                         htmltools::div(class = "kpi-value", style = "font-size: 1.35rem;", moda_nome)),
          htmltools::div(class = "kpi-card",
                         htmltools::div(class = "kpi-title", "Representatividade da Moda"),
                         htmltools::div(class = "kpi-value", .fmt_pct(moda_pct, 1))),
          htmltools::div(class = "kpi-card",
                         htmltools::div(class = "kpi-title", "Total Válido"),
                         htmltools::div(class = "kpi-value", length(v)))
        )
      }
    })

    output$plot_univariada <- plotly::renderPlotly({
      info <- var_univariada_info()
      shiny::req(info)
      df <- data.frame(val = stats::na.omit(info$valores))

      if (info$is_num) {
        tipo <- input$tipo_graf_num %||% "hist"
        if (tipo == "hist") {
          p <- ggplot2::ggplot(df, ggplot2::aes(x = val)) +
            ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                    fill = "#0071E3", color = "#FFFFFF", alpha = 0.85, bins = 20) +
            ggplot2::geom_density(color = "#1D1D1F", linewidth = 1.1) +
            .tema_apple() +
            ggplot2::labs(x = info$nome, y = "Densidade")
        } else {
          p <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = val)) +
            ggplot2::geom_boxplot(fill = "#E3F2FD", color = "#0071E3", outlier.shape = NA, width = 0.4, linewidth = 0.8) +
            ggplot2::geom_jitter(color = "#1D1D1F", alpha = 0.45, width = 0.15, size = 1.8) +
            .tema_apple() +
            ggplot2::labs(x = NULL, y = info$nome)
        }
      } else {
        # Categórica: Gráfico de barras horizontais
        tb_df <- as.data.frame(sort(table(df$val), decreasing = TRUE))
        names(tb_df) <- c("Categoria", "Contagem")
        tb_df$Percentual <- (tb_df$Contagem / sum(tb_df$Contagem)) * 100

        p <- ggplot2::ggplot(tb_df, ggplot2::aes(x = Percentual, y = stats::reorder(Categoria, Percentual))) +
          ggplot2::geom_col(fill = "#0071E3", width = 0.65) +
          .tema_apple() +
          ggplot2::labs(x = "Frequência Relativa (%)", y = info$nome)
      }

      .converter_plotly(p)
    })

    # Diagnóstico de Normalidade (Shapiro-Wilk + QQ-Plot) ou Tabela de Frequência
    output$ui_diagnostico_univariada <- shiny::renderUI({
      info <- var_univariada_info()
      shiny::req(info)
      v <- stats::na.omit(info$valores)

      if (info$is_num) {
        # Teste de Shapiro-Wilk (se 3 <= n <= 5000)
        n_obs <- length(v)
        res_shapiro <- if (n_obs >= 3 && n_obs <= 5000) {
          tryCatch(stats::shapiro.test(v), error = function(e) NULL)
        } else if (n_obs > 5000) {
          # Amostra de 5000
          tryCatch(stats::shapiro.test(sample(v, 5000)), error = function(e) NULL)
        } else NULL

        w_stat <- if (!is.null(res_shapiro)) .fmt_num(res_shapiro$statistic, 3) else "-"
        p_val  <- if (!is.null(res_shapiro)) res_shapiro$p.value else NA
        p_txt  <- if (is.na(p_val)) "-" else if (p_val < 0.001) "< 0,001" else .fmt_num(p_val, 3)
        normal <- !is.na(p_val) && p_val >= 0.05

        htmltools::div(
          class = "apple-card",
          htmltools::div(
            class = "apple-card-header",
            htmltools::h5(class = "apple-card-title", "Diagnóstico de Normalidade (Shapiro-Wilk & QQ-Plot)")
          ),
          htmltools::div(
            class = "row align-items-center",
            htmltools::div(
              class = "col-md-5",
              htmltools::div(
                style = "padding: 1rem; border-radius: 8px; background-color: #FAFAFC; border: 1px solid #E5E5EA;",
                htmltools::tags$strong("Teste de Shapiro-Wilk"),
                htmltools::p(style = "margin: 0.5rem 0;", sprintf("Estatística W: %s | p-valor: %s", w_stat, p_txt)),
                if (normal) {
                  htmltools::div(class = "badge bg-success", style = "font-size: 0.85rem;", "Distribuição compatível com a Normal (p ≥ 0,05)")
                } else {
                  htmltools::div(class = "badge bg-warning text-dark", style = "font-size: 0.85rem;", "Desvio significativo da Normalidade (p < 0,05)")
                },
                htmltools::p(class = "apple-card-subtitle", style = "margin-top: 0.75rem;",
                             if (normal) "Adequado para métodos paramétricos (teste t, ANOVA, regressão)."
                             else "Recomenda-se avaliar transformações ou métodos não-paramétricos.")
              )
            ),
            htmltools::div(
              class = "col-md-7",
              plotly::plotlyOutput("plot_qqplot_univariada", height = "280px")
            )
          )
        )
      } else {
        # Categórica: Tabela de Frequência
        htmltools::div(
          class = "apple-card",
          htmltools::div(
            class = "apple-card-header",
            htmltools::h5(class = "apple-card-title", "Tabela de Distribuição de Frequências")
          ),
          DT::DTOutput("tab_frequencias_univariada")
        )
      }
    })

    # QQ-Plot Interativo
    output$plot_qqplot_univariada <- plotly::renderPlotly({
      info <- var_univariada_info()
      shiny::req(info, info$is_num)
      df <- data.frame(val = stats::na.omit(info$valores))

      p <- ggplot2::ggplot(df, ggplot2::aes(sample = val)) +
        ggplot2::stat_qq(color = "#0071E3", alpha = 0.65, size = 2.0) +
        ggplot2::stat_qq_line(color = "#1D1D1F", linewidth = 0.8, linetype = "dashed") +
        .tema_apple() +
        ggplot2::labs(title = "QQ-Plot (Quantis Teóricos vs Amostrais)", x = "Quantis Teóricos", y = "Quantis Amostrais")

      .converter_plotly(p)
    })

    # Tabela de Frequências
    output$tab_frequencias_univariada <- DT::renderDT({
      info <- var_univariada_info()
      shiny::req(info, !info$is_num)
      v <- stats::na.omit(info$valores)
      tb <- as.data.frame(sort(table(v), decreasing = TRUE))
      names(tb) <- c("Categoria", "Frequencia_Absoluta")
      tb$Percentual <- (tb$Frequencia_Absoluta / sum(tb$Frequencia_Absoluta)) * 100
      tb$Percentual_Acumulado <- cumsum(tb$Percentual)

      tb$Percentual <- paste0(.fmt_num(tb$Percentual, 1), "%")
      tb$Percentual_Acumulado <- paste0(.fmt_num(tb$Percentual_Acumulado, 1), "%")

      DT::datatable(tb, rownames = FALSE, colnames = c("Categoria", "Frequência (N)", "% Relativo", "% Acumulado"),
                    options = list(dom = "t", pageLength = 8))
    })

    # ── ABA 3: RELAÇÕES & GRUPOS (BIVARIADA) ─────────────────────────────────
    biv_info <- shiny::reactive({
      df <- rv$dados
      x_col <- input$sel_biv_x
      y_col <- input$sel_biv_y
      grp_col <- input$sel_biv_grupo
      shiny::req(df, x_col, y_col, x_col %in% names(df), y_col %in% names(df))

      is_x_num <- is.numeric(df[[x_col]])
      is_y_num <- is.numeric(df[[y_col]])
      tem_grp  <- !is.null(grp_col) && nzchar(grp_col) && grp_col %in% names(df)

      list(
        df = df, x_col = x_col, y_col = y_col, grp_col = grp_col,
        is_x_num = is_x_num, is_y_num = is_y_num, tem_grp = tem_grp,
        tipo_rel = if (is_x_num && is_y_num) "num_num"
                   else if ((is_x_num && !is_y_num) || (!is_x_num && is_y_num)) "num_cat"
                   else "cat_cat"
      )
    })

    output$tit_grafico_bivariada <- shiny::renderText({
      info <- biv_info()
      if (is.null(info)) return("Relação Bivariada")
      sprintf("Relação: %s vs %s", info$x_col, info$y_col)
    })

    output$plot_bivariada <- plotly::renderPlotly({
      info <- biv_info()
      shiny::req(info)
      df <- info$df

      if (info$tipo_rel == "num_num") {
        # Dispersão com linha de tendência
        p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[info$x_col]], y = .data[[info$y_col]]))
        if (info$tem_grp) {
          p <- p + ggplot2::geom_point(ggplot2::aes(color = as.factor(.data[[info$grp_col]])), alpha = 0.7, size = 2.2) +
            ggplot2::scale_color_brewer(palette = "Set1", name = info$grp_col)
        } else {
          p <- p + ggplot2::geom_point(color = "#0071E3", alpha = 0.65, size = 2.2) +
            ggplot2::geom_smooth(method = "loess", color = "#1D1D1F", linewidth = 0.9, se = TRUE, formula = y ~ x)
        }
        p <- p + .tema_apple() + ggplot2::labs(x = info$x_col, y = info$y_col)

      } else if (info$tipo_rel == "num_cat") {
        # Numérica vs Categórica: Boxplot por grupo
        num_v <- if (info$is_x_num) info$x_col else info$y_col
        cat_v <- if (info$is_x_num) info$y_col else info$x_col

        p <- ggplot2::ggplot(df, ggplot2::aes(x = as.factor(.data[[cat_v]]), y = .data[[num_v]], fill = as.factor(.data[[cat_v]]))) +
          ggplot2::geom_boxplot(color = "#1D1D1F", alpha = 0.7, width = 0.45, linewidth = 0.7, outlier.shape = NA) +
          ggplot2::geom_jitter(color = "#1D1D1F", alpha = 0.35, width = 0.15, size = 1.6) +
          ggplot2::scale_fill_brewer(palette = "Set2") +
          .tema_apple() +
          ggplot2::theme(legend.position = "none") +
          ggplot2::labs(x = cat_v, y = num_v)

      } else {
        # Categórica vs Categórica: Barras agrupadas
        p <- ggplot2::ggplot(df, ggplot2::aes(x = as.factor(.data[[info$x_col]]), fill = as.factor(.data[[info$y_col]]))) +
          ggplot2::geom_bar(position = "dodge", alpha = 0.85) +
          ggplot2::scale_fill_brewer(palette = "Set1", name = info$y_col) +
          .tema_apple() +
          ggplot2::labs(x = info$x_col, y = "Contagem")
      }

      .converter_plotly(p)
    })

    output$ui_teste_bivariado <- shiny::renderUI({
      info <- biv_info()
      shiny::req(info)
      df <- info$df

      if (info$tipo_rel == "num_num") {
        ok <- !is.na(df[[info$x_col]]) & !is.na(df[[info$y_col]])
        vx <- df[[info$x_col]][ok]
        vy <- df[[info$y_col]][ok]

        cor_p <- stats::cor.test(vx, vy, method = "pearson")
        cor_s <- stats::cor.test(vx, vy, method = "spearman")

        htmltools::div(
          class = "row",
          htmltools::div(
            class = "col-md-6",
            htmltools::div(
              style = "padding: 1rem; border-radius: 8px; background: #FAFAFC; border: 1px solid #E5E5EA;",
              htmltools::tags$strong("Correlação Linear de Pearson"),
              htmltools::p(style = "margin: 0.4rem 0;", sprintf("r = %s  |  p-valor: %s", .fmt_num(cor_p$estimate, 3), if (cor_p$p.value < 0.001) "< 0,001" else .fmt_num(cor_p$p.value, 3))),
              htmltools::tags$small(class = "text-muted", sprintf("Intervalo de Confiança (95%%): [%s; %s]", .fmt_num(cor_p$conf.int[1], 3), .fmt_num(cor_p$conf.int[2], 3)))
            )
          ),
          htmltools::div(
            class = "col-md-6",
            htmltools::div(
              style = "padding: 1rem; border-radius: 8px; background: #FAFAFC; border: 1px solid #E5E5EA;",
              htmltools::tags$strong("Correlação Monotônica de Spearman"),
              htmltools::p(style = "margin: 0.4rem 0;", sprintf("rho = %s  |  p-valor: %s", .fmt_num(cor_s$estimate, 3), if (cor_s$p.value < 0.001) "< 0,001" else .fmt_num(cor_s$p.value, 3))),
              htmltools::tags$small(class = "text-muted", "Baseado em postos, robusto a outliers e não-linearidades.")
            )
          )
        )
      } else if (info$tipo_rel == "num_cat") {
        num_v <- if (info$is_x_num) info$x_col else info$y_col
        cat_v <- if (info$is_x_num) info$y_col else info$x_col

        ok <- !is.na(df[[num_v]]) & !is.na(df[[cat_v]])
        sub_df <- df[ok, ]
        grps <- as.factor(sub_df[[cat_v]])
        k_niveis <- nlevels(grps)

        if (k_niveis == 2) {
          # Teste t de Welch
          t_res <- stats::t.test(sub_df[[num_v]] ~ grps)
          p_val <- t_res$p.value
          sig <- p_val < 0.05

          htmltools::div(
            style = "padding: 1rem; border-radius: 8px; background: #FAFAFC; border: 1px solid #E5E5EA;",
            htmltools::tags$strong("Teste t de Welch (Comparação de 2 Médias Independentes)"),
            htmltools::p(style = "margin: 0.4rem 0;", sprintf("Estatística t = %s  |  Graus de Liberdade = %s  |  p-valor = %s",
                                                              .fmt_num(t_res$statistic, 2), .fmt_num(t_res$parameter, 1),
                                                              if (p_val < 0.001) "< 0,001" else .fmt_num(p_val, 3))),
            htmltools::div(
              class = if (sig) "badge bg-primary" else "badge bg-secondary",
              if (sig) "Diferença estatisticamente significativa entre as médias (p < 0,05)"
              else "Sem evidência de diferença significativa entre as médias (p ≥ 0,05)"
            )
          )
        } else if (k_niveis > 2) {
          # ANOVA One-Way
          aov_res <- stats::aov(sub_df[[num_v]] ~ grps)
          sm <- summary(aov_res)[[1]]
          f_val <- sm["grps", "F value"]
          p_val <- sm["grps", "Pr(>F)"]
          sig <- !is.na(p_val) && p_val < 0.05

          htmltools::div(
            style = "padding: 1rem; border-radius: 8px; background: #FAFAFC; border: 1px solid #E5E5EA;",
            htmltools::tags$strong(sprintf("Análise de Variância (ANOVA One-Way — %d Grupos)", k_niveis)),
            htmltools::p(style = "margin: 0.4rem 0;", sprintf("Estatística F = %s  |  p-valor = %s",
                                                              .fmt_num(f_val, 2),
                                                              if (p_val < 0.001) "< 0,001" else .fmt_num(p_val, 3))),
            htmltools::div(
              class = if (sig) "badge bg-primary" else "badge bg-secondary",
              if (sig) "Efeito de grupo significativo detectado (p < 0,05)"
              else "Médias dos grupos não apresentam diferença significativa (p ≥ 0,05)"
            )
          )
        } else {
          htmltools::p("O fator categórico possui menos de 2 níveis.")
        }
      } else {
        # Categórica vs Categórica: Teste Qui-Quadrado
        tab <- table(df[[info$x_col]], df[[info$y_col]])
        qui <- stats::chisq.test(tab)
        sig <- qui$p.value < 0.05

        htmltools::div(
          style = "padding: 1rem; border-radius: 8px; background: #FAFAFC; border: 1px solid #E5E5EA;",
          htmltools::tags$strong("Teste Qui-Quadrado de Independência"),
          htmltools::p(style = "margin: 0.4rem 0;", sprintf("Qui-Quadrado = %s (df = %d)  |  p-valor = %s",
                                                            .fmt_num(qui$statistic, 2), qui$parameter,
                                                            if (qui$p.value < 0.001) "< 0,001" else .fmt_num(qui$p.value, 3))),
          htmltools::div(
            class = if (sig) "badge bg-primary" else "badge bg-secondary",
            if (sig) "Associação estatisticamente significativa entre as variáveis (p < 0,05)"
            else "Variáveis independentes (sem evidência de associação, p ≥ 0,05)"
          )
        )
      }
    })

    # ── ABA 4: MATRIZ EXPLORATÓRIA (PAIRS PLOT) ──────────────────────────────
    output$plot_matriz_exploratoria <- shiny::renderPlot({
      input$btn_gerar_matriz
      vars <- shiny::isolate(input$sel_matriz_vars)
      grp  <- shiny::isolate(input$sel_matriz_grupo)
      df   <- rv$dados

      shiny::req(df, vars, length(vars) >= 2)
      grp_val <- if (!is.null(grp) && nzchar(grp)) grp else NULL

      p <- .gerar_matriz_exploratoria(df, vars, grupo = grp_val)
      if (!is.null(p)) print(p)
    })

    # ── MODAL: CÓDIGO R GERADO ───────────────────────────────────────────────
    shiny::observeEvent(input$btn_ver_codigo, {
      aba_ativa <- input$nav_principal %||% "Visão Geral"
      ds <- rv$nome_dataset

      codigo <- switch(
        aba_ativa,
        "Visão Geral" = paste(c(
          "# ==============================================================================",
          "# DIAGNÓSTICO ESTRUTURAL DOS DADOS - estatflowR",
          "# ==============================================================================",
          "",
          "# 1. Estrutura e tipos de variáveis",
          sprintf("str(%s)", ds),
          "",
          "# 2. Resumo descritivo geral",
          sprintf("summary(%s)", ds),
          "",
          "# 3. Diagnóstico de valores ausentes (NAs)",
          sprintf("colSums(is.na(%s))", ds)
        ), collapse = "\n"),

        "Variável Isolada" = {
          v <- input$sel_var_univariada %||% "var"
          paste(c(
            "# ==============================================================================",
            "# ANÁLISE UNIVARIADA - estatflowR",
            "# ==============================================================================",
            "library(ggplot2)",
            "",
            "# 1. Gráfico de distribuição com grade dupla",
            sprintf("ggplot(%s, aes(x = %s)) +", ds, v),
            "  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = '#0071E3', color = 'white', alpha = 0.75) +",
            "  geom_density(color = '#1D1D1F', linewidth = 0.9) +",
            "  theme_minimal() +",
            "  theme(panel.grid.major = element_line(color = 'grey85'),",
            "        panel.grid.minor = element_line(color = 'grey92', linetype = 'dotted'))",
            "",
            "# 2. Teste de normalidade (Shapiro-Wilk)",
            sprintf("shapiro.test(%s$%s)", ds, v)
          ), collapse = "\n")
        },

        "Relações & Grupos" = {
          x <- input$sel_biv_x %||% "x"
          y <- input$sel_biv_y %||% "y"
          paste(c(
            "# ==============================================================================",
            "# ANÁLISE BIVARIADA - estatflowR",
            "# ==============================================================================",
            "library(ggplot2)",
            "",
            "# 1. Gráfico de dispersão e tendência",
            sprintf("ggplot(%s, aes(x = %s, y = %s)) +", ds, x, y),
            "  geom_point(color = '#0071E3', alpha = 0.75, size = 2.5) +",
            "  geom_smooth(method = 'loess', color = '#FF9500', se = TRUE) +",
            "  theme_minimal() +",
            "  theme(panel.grid.major = element_line(color = 'grey85'),",
            "        panel.grid.minor = element_line(color = 'grey92', linetype = 'dotted'))",
            "",
            "# 2. Teste de correlação",
            sprintf("cor.test(%s$%s, %s$%s, method = 'pearson')", ds, x, ds, y)
          ), collapse = "\n")
        },

        "Matriz Exploratória" = {
          vars_vec <- input$sel_matriz_vars %||% c("var1", "var2")
          vars_str <- paste(sprintf("'%s'", vars_vec), collapse = ", ")
          paste(c(
            "# ==============================================================================",
            "# MATRIZ EXPLORATÓRIA MULTIVARIADA - estatflowR",
            "# ==============================================================================",
            "",
            "# 1. Diagrama de pares / dispersões",
            sprintf("pairs(%s[, c(%s)], pch = 19, col = '#0071E3')", ds, vars_str),
            "",
            "# 2. Matriz de correlações",
            sprintf("cor(%s[, c(%s)], use = 'complete.obs')", ds, vars_str)
          ), collapse = "\n")
        },

        sprintf("# Código R para %s\nhead(%s)", ds, ds)
      )

      shiny::showModal(shiny::modalDialog(
        title = htmltools::tags$strong("Código R Reproduzível"),
        htmltools::div(
          htmltools::p(class = "apple-card-subtitle", "Copie o script abaixo para reproduzir esta análise diretamente no seu console ou script R:"),
          htmltools::tags$pre(
            class = "code-modal-content",
            htmltools::tags$code(codigo)
          )
        ),
        footer = shiny::modalButton("Fechar"),
        easyClose = TRUE,
        size = "l"
      ))
    })

    # ── BOTÃO DE CONCLUIR / FECHAR APP ───────────────────────────────────────
    shiny::observeEvent(input$btn_fechar_app, {
      shiny::stopApp(rv$dados)
    })

  }
}
