# ─────────────────────────────────────────────────────────────────────────────
# MÓDULO SERVER: LÓGICA REATIVA E DIAGNÓSTICOS DO ESTATFLOWR
# ─────────────────────────────────────────────────────────────────────────────

# Helpers de cálculo de momentos estatísticos
.calc_skewness <- function(x) {
  x <- stats::na.omit(x)
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  if (s == 0) return(0)
  (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
}

.calc_kurtosis <- function(x) {
  x <- stats::na.omit(x)
  n <- length(x)
  if (n < 4) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  if (s == 0) return(0)
  n1 <- n - 1
  n2 <- n - 2
  n3 <- n - 3
  termo1 <- (n * (n + 1)) / (n1 * n2 * n3) * sum(((x - m) / s)^4)
  termo2 <- (3 * (n1^2)) / (n2 * n3)
  termo1 - termo2
}

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

    # Badge com o nome da base ativa
    output$ui_dataset_badge <- shiny::renderUI({
      htmltools::tags$span(
        class = "badge-apple badge-apple-info",
        style = "font-size: 0.95rem; font-weight: 600; padding: 0.35rem 0.75rem;",
        rv$nome_dataset
      )
    })

    # ── REATIVO DE DIAGNÓSTICO DO BANCO ATUAL ────────────────────────────────
    diagnostico_df <- shiny::reactive({
      .diagnosticar_variaveis(rv$dados)
    })

    # ── ABA 1: KPIS ESTRUTURAIS (DIMENSÕES, VARIÁVEIS, INTEGRIDADE) ──────────
    output$ui_kpis_dataset <- shiny::renderUI({
      df <- rv$dados
      n_lin <- nrow(df)
      n_col <- ncol(df)
      total_celulas <- n_lin * n_col
      total_nas <- sum(is.na(df))
      pct_nas <- if (total_celulas > 0) (total_nas / total_celulas) * 100 else 0
      completude <- 100 - pct_nas

      diag <- diagnostico_df()
      tipos <- table(diag$Tipo)
      n_num <- sum(diag$Tipo %in% c("Numérica Contínua", "Numérica Discreta"))
      pct_num <- if (n_col > 0) (n_num / n_col) * 100 else 0

      n_cat <- sum(diag$Tipo %in% c("Categórica / Fator", "Texto (Caractere)"))
      pct_cat <- if (n_col > 0) (n_cat / n_col) * 100 else 0

      n_txt <- sum(diag$Tipo == "Texto (Caractere)")
      pct_txt <- if (n_col > 0) (n_txt / n_col) * 100 else 0

      n_data <- sum(diag$Tipo == "Data / Hora")
      pct_data <- if (n_col > 0) (n_data / n_col) * 100 else 0

      htmltools::div(
        class = "kpi-row-apple",

        # Card 1: Dimensões
        htmltools::div(
          class = "kpi-box-apple",
          htmltools::div(class = "kpi-box-title", "Dimensões da Base"),
          htmltools::div(
            class = "kpi-box-value",
            sprintf("%s Linhas", format(n_lin, big.mark = "."))
          ),
          htmltools::div(
            class = "kpi-box-sub",
            sprintf("Colunas (Variáveis): %s", n_col)
          )
        ),

        # Card 2: Composição de Variáveis
        htmltools::div(
          class = "kpi-box-apple",
          htmltools::div(class = "kpi-box-title", "Composição de Variáveis"),
          htmltools::div(
            class = "kpi-box-value",
            sprintf("%s Numéricas (%s)", n_num, .fmt_pct(pct_num, 0))
          ),
          htmltools::div(
            class = "kpi-box-sub",
            paste(
              sprintf("Categóricas: %s (%s)", n_cat, .fmt_pct(pct_cat, 0)),
              if (n_txt > 0) sprintf("| Textos: %s (%s)", n_txt, .fmt_pct(pct_txt, 0)) else "",
              if (n_data > 0) sprintf("| Datas: %s (%s)", n_data, .fmt_pct(pct_data, 0)) else ""
            )
          )
        ),

        # Card 3: Integridade & Células Ausentes
        htmltools::div(
          class = "kpi-box-apple",
          htmltools::div(class = "kpi-box-title", "Integridade Global"),
          htmltools::div(
            class = "kpi-box-value",
            sprintf("%s Ausentes (%s)", format(total_nas, big.mark = "."), .fmt_pct(pct_nas, 1))
          ),
          htmltools::div(
            class = "kpi-box-sub",
            sprintf("Taxa de Completude: %s de dados válidos", .fmt_pct(completude, 1))
          )
        )
      )
    })

    # Tabela 1: Dicionário e Diagnóstico de Variáveis
    output$tab_diagnostico_vars <- DT::renderDT({
      diag <- diagnostico_df()
      tab_show <- diag[, c("Variavel", "Tipo", "Validos", "Ausentes", "Pct_Ausentes", "Status_NA", "Resumo")]
      names(tab_show) <- c("Variável", "Tipo Detectado", "Válidos", "NAs", "% NAs", "Status", "Resumo Paramétrico")

      DT::datatable(
        tab_show,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "ftip",
          language = list(
            search = "Buscar variável:",
            paginate = list(previous = "Anterior", `next` = "Próxima"),
            info = "Exibindo _START_ a _END_ de _TOTAL_ variáveis"
          )
        )
      ) |>
        DT::formatStyle(
          "Status",
          backgroundColor = DT::styleEqual(
            c("ok", "atencao", "critico"),
            c("#E8F5E9", "#FFF3E0", "#FFEBEE")
          ),
          color = DT::styleEqual(
            c("ok", "atencao", "critico"),
            c("#2E7D32", "#E65100", "#C62828")
          ),
          fontWeight = "600"
        )
    })

    # ── ABA 2: RESUMO DESCRITIVO COMPLETO (NUMÉRICO, CATEGÓRICO, NAS) ────────
    output$tab_descritiva_numerica <- DT::renderDT({
      df <- rv$dados
      vars_num <- names(df)[vapply(df, is.numeric, logical(1))]

      if (length(vars_num) == 0) {
        return(DT::datatable(data.frame(Mensagem = "Nenhuma variável numérica encontrada na base de dados."), rownames = FALSE))
      }

      res <- lapply(vars_num, function(v) {
        x <- df[[v]]
        xv <- stats::na.omit(x)
        n_val <- length(xv)
        if (n_val == 0) {
          return(data.frame(
            Variavel = v, N = 0, Media = "-", DP = "-", Mediana = "-",
            Q1 = "-", Q3 = "-", IQR = "-", Min = "-", Max = "-",
            Assimetria = "-", Curtose = "-", stringsAsFactors = FALSE
          ))
        }
        qs <- stats::quantile(xv, probs = c(0.25, 0.75), names = FALSE)
        data.frame(
          Variavel   = v,
          N          = n_val,
          Media      = .fmt_num(mean(xv)),
          DP         = .fmt_num(stats::sd(xv)),
          Mediana    = .fmt_num(stats::median(xv)),
          Q1         = .fmt_num(qs[1]),
          Q3         = .fmt_num(qs[2]),
          IQR        = .fmt_num(stats::IQR(xv)),
          Min        = .fmt_num(min(xv)),
          Max        = .fmt_num(max(xv)),
          Assimetria = .fmt_num(.calc_skewness(xv)),
          Curtose    = .fmt_num(.calc_kurtosis(xv)),
          stringsAsFactors = FALSE
        )
      })

      tab_num <- do.call(rbind, res)
      names(tab_num) <- c("Variável", "N Válido", "Média", "Desvio-Padrão", "Mediana", "Q1 (25%)", "Q3 (75%)", "IQR", "Mínimo", "Máximo", "Assimetria", "Curtose")

      DT::datatable(
        tab_num,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "ftip",
          language = list(
            search = "Buscar variável:",
            paginate = list(previous = "Anterior", `next` = "Próxima"),
            info = "Exibindo _START_ a _END_ de _TOTAL_ variáveis numéricas"
          )
        )
      )
    })

    output$ui_bloco_descritiva_categorica <- shiny::renderUI({
      df <- rv$dados
      vars_cat <- names(df)[!vapply(df, is.numeric, logical(1))]

      if (length(vars_cat) == 0) return(NULL)

      htmltools::div(
        class = "apple-card",
        htmltools::div(
          class = "apple-card-header",
          htmltools::div(
            htmltools::h5(class = "apple-card-title", "Estatísticas Descritivas — Variáveis Categóricas e Fatores"),
            htmltools::div(class = "apple-card-subtitle", "Níveis distintos, moda amostral e frequência relativa")
          )
        ),
        DT::DTOutput("tab_descritiva_categorica")
      )
    })

    output$tab_descritiva_categorica <- DT::renderDT({
      df <- rv$dados
      vars_cat <- names(df)[!vapply(df, is.numeric, logical(1))]
      if (length(vars_cat) == 0) return(NULL)

      res <- lapply(vars_cat, function(v) {
        x <- df[[v]]
        xv <- stats::na.omit(x)
        n_val <- length(xv)
        if (n_val == 0) {
          return(data.frame(
            Variavel = v, N = 0, Niveis = 0, Moda = "-", Freq_Moda = 0, Pct_Moda = "-", stringsAsFactors = FALSE
          ))
        }
        tb <- sort(table(as.character(xv)), decreasing = TRUE)
        moda_val <- names(tb)[1]
        freq_moda <- tb[1]
        pct_moda <- (freq_moda / n_val) * 100

        data.frame(
          Variavel  = v,
          N         = n_val,
          Niveis    = length(unique(xv)),
          Moda      = moda_val,
          Freq_Moda = freq_moda,
          Pct_Moda  = .fmt_pct(pct_moda, 1),
          stringsAsFactors = FALSE
        )
      })

      tab_cat <- do.call(rbind, res)
      names(tab_cat) <- c("Variável", "N Válido", "Níveis Únicos", "Moda (Mais Frequente)", "Frequência da Moda", "% da Moda")

      DT::datatable(
        tab_cat,
        rownames = FALSE,
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          dom = "ftip",
          language = list(
            search = "Buscar variável:",
            paginate = list(previous = "Anterior", `next` = "Próxima"),
            info = "Exibindo _START_ a _END_ de _TOTAL_ variáveis categóricas"
          )
        )
      )
    })

    # Tabela 3: Diagnóstico Detalhado de NAs
    output$tab_diagnostico_nas <- DT::renderDT({
      diag <- diagnostico_df()
      tb_na <- diag[, c("Variavel", "Tipo", "Validos", "Ausentes", "Pct_Ausentes", "Status_NA")]
      names(tb_na) <- c("Variável", "Tipo Detectado", "Válidos", "Ausentes (NAs)", "% NAs", "Classificação")

      tb_na$Classificação <- ifelse(
        tb_na$Classificação == "ok", "100% Válido",
        ifelse(tb_na$Classificação == "atencao", "Atenção (< 10%)", "Crítico (≥ 10%)")
      )

      DT::datatable(
        tb_na,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "ftip",
          language = list(
            search = "Buscar variável:",
            paginate = list(previous = "Anterior", `next` = "Próxima"),
            info = "Exibindo _START_ a _END_ de _TOTAL_ variáveis"
          )
        )
      ) |>
        DT::formatStyle(
          "Classificação",
          backgroundColor = DT::styleEqual(
            c("100% Válido", "Atenção (< 10%)", "Crítico (≥ 10%)"),
            c("#E8F5E9", "#FFF3E0", "#FFEBEE")
          ),
          color = DT::styleEqual(
            c("100% Válido", "Atenção (< 10%)", "Crítico (≥ 10%)"),
            c("#2E7D32", "#E65100", "#C62828")
          ),
          fontWeight = "600"
        )
    })

    # ── OBSERVERS PARA ATUALIZAR SELECTINPUTS AO MUDAR DATASET ───────────────
    shiny::observe({
      df <- rv$dados
      todas_vars <- names(df)
      vars_num <- todas_vars[vapply(df, is.numeric, logical(1))]
      vars_cat <- todas_vars[!vapply(df, is.numeric, logical(1))]

      # Univariada
      shiny::updateSelectInput(session, "sel_var_univariada", choices = todas_vars, selected = todas_vars[1])

      # Bivariada
      if (length(vars_num) >= 2) {
        shiny::updateSelectInput(session, "sel_biv_x", choices = todas_vars, selected = vars_num[1])
        shiny::updateSelectInput(session, "sel_biv_y", choices = todas_vars, selected = vars_num[2])
      } else if (length(todas_vars) >= 2) {
        shiny::updateSelectInput(session, "sel_biv_x", choices = todas_vars, selected = todas_vars[1])
        shiny::updateSelectInput(session, "sel_biv_y", choices = todas_vars, selected = todas_vars[2])
      }

      shiny::updateSelectInput(
        session, "sel_biv_grupo",
        choices = c("Nenhum" = "", vars_cat, vars_num),
        selected = ""
      )

      # Matriz Multivariada
      if (length(vars_num) >= 2) {
        sel_ini <- vars_num[1:min(4, length(vars_num))]
        shiny::updateSelectizeInput(session, "sel_matriz_vars", choices = vars_num, selected = sel_ini)
      }
      shiny::updateSelectInput(session, "sel_matriz_grupo", choices = c("Nenhum" = "", vars_cat), selected = "")
    })

    # ── ABA 3: ANÁLISE UNIVARIADA ────────────────────────────────────────────
    var_univariada_info <- shiny::reactive({
      v <- input$sel_var_univariada
      shiny::req(v, v %in% names(rv$dados))
      col_val <- rv$dados[[v]]
      list(
        nome = v,
        valores = col_val,
        is_num = is.numeric(col_val)
      )
    })

    output$ui_controles_univariada <- shiny::renderUI({
      info <- var_univariada_info()
      if (is.null(info)) return(NULL)

      if (info$is_num) {
        htmltools::div(
          style = "margin-top: 1rem;",
          shiny::radioButtons(
            "radio_tipo_graf_univariada", "Estilo do Gráfico:",
            choices = c("Distribuição (Histograma + Densidade)" = "hist_dens",
                        "Boxplot com Dispersão (Jitter)" = "boxplot"),
            selected = "hist_dens"
          )
        )
      } else {
        htmltools::div(
          style = "margin-top: 1rem;",
          htmltools::p(class = "apple-card-subtitle", "Variável categórica identificada. Exibição de frequências absolutas e relativas.")
        )
      }
    })

    # Tabela Horizontal de Estatísticas com Fundo Branco
    output$ui_tabela_horizontal_univariada <- shiny::renderUI({
      info <- var_univariada_info()
      if (is.null(info)) return(NULL)
      v <- stats::na.omit(info$valores)
      n_val <- length(v)

      if (info$is_num && n_val > 0) {
        qs <- stats::quantile(v, probs = c(0.25, 0.75), names = FALSE)
        htmltools::div(
          class = "tabela-descritiva-horizontal",
          htmltools::tags$table(
            htmltools::tags$thead(
              htmltools::tags$tr(
                htmltools::tags$th("Variável"),
                htmltools::tags$th("N Válido"),
                htmltools::tags$th("Média"),
                htmltools::tags$th("Desvio-Padrão"),
                htmltools::tags$th("Mediana"),
                htmltools::tags$th("Q1 (25%)"),
                htmltools::tags$th("Q3 (75%)"),
                htmltools::tags$th("IQR"),
                htmltools::tags$th("Mínimo"),
                htmltools::tags$th("Máximo"),
                htmltools::tags$th("Assimetria")
              )
            ),
            htmltools::tags$tbody(
              htmltools::tags$tr(
                htmltools::tags$td(htmltools::tags$strong(info$nome)),
                htmltools::tags$td(n_val),
                htmltools::tags$td(.fmt_num(mean(v))),
                htmltools::tags$td(.fmt_num(stats::sd(v))),
                htmltools::tags$td(.fmt_num(stats::median(v))),
                htmltools::tags$td(.fmt_num(qs[1])),
                htmltools::tags$td(.fmt_num(qs[2])),
                htmltools::tags$td(.fmt_num(stats::IQR(v))),
                htmltools::tags$td(.fmt_num(min(v))),
                htmltools::tags$td(.fmt_num(max(v))),
                htmltools::tags$td(.fmt_num(.calc_skewness(v)))
              )
            )
          )
        )
      } else if (!info$is_num && n_val > 0) {
        tb <- sort(table(as.character(v)), decreasing = TRUE)
        moda_val <- names(tb)[1]
        freq_m <- tb[1]
        pct_m <- (freq_m / n_val) * 100

        htmltools::div(
          class = "tabela-descritiva-horizontal",
          htmltools::tags$table(
            htmltools::tags$thead(
              htmltools::tags$tr(
                htmltools::tags$th("Variável"),
                htmltools::tags$th("N Válido"),
                htmltools::tags$th("Níveis Únicos"),
                htmltools::tags$th("Moda (Mais Frequente)"),
                htmltools::tags$th("Frequência da Moda"),
                htmltools::tags$th("% da Moda")
              )
            ),
            htmltools::tags$tbody(
              htmltools::tags$tr(
                htmltools::tags$td(htmltools::tags$strong(info$nome)),
                htmltools::tags$td(n_val),
                htmltools::tags$td(length(unique(v))),
                htmltools::tags$td(moda_val),
                htmltools::tags$td(freq_m),
                htmltools::tags$td(.fmt_pct(pct_m, 1))
              )
            )
          )
        )
      }
    })

    output$tit_grafico_univariada <- shiny::renderText({
      info <- var_univariada_info()
      if (is.null(info)) return("Visualização da Variável")
      sprintf("Distribuição: %s", info$nome)
    })

    # Gráfico Univariado Interativo
    output$plot_univariada <- plotly::renderPlotly({
      info <- var_univariada_info()
      shiny::req(info)
      df <- data.frame(val = stats::na.omit(info$valores))
      v_nome <- info$nome

      if (info$is_num) {
        estilo <- input$radio_tipo_graf_univariada %||% "hist_dens"

        if (estilo == "hist_dens") {
          p <- ggplot2::ggplot(df, ggplot2::aes(x = val)) +
            ggplot2::geom_histogram(
              ggplot2::aes(y = ggplot2::after_stat(density)),
              bins = 24, fill = "#0071E3", color = "#FFFFFF", alpha = 0.85
            ) +
            ggplot2::geom_density(color = "#1D1D1F", linewidth = 1.0) +
            .tema_apple() +
            ggplot2::labs(title = paste0("Histograma & Densidade: ", v_nome), x = v_nome, y = "Densidade Estimada")
        } else {
          p <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = val)) +
            ggplot2::geom_boxplot(fill = "#F2F2F7", color = "#1D1D1F", outlier.color = "#FF3B30", outlier.size = 2.5, width = 0.45) +
            ggplot2::geom_jitter(width = 0.12, color = "#0071E3", alpha = 0.65, size = 2.2) +
            .tema_apple() +
            ggplot2::labs(title = paste0("Boxplot & Dispersão: ", v_nome), x = "", y = v_nome)
        }
      } else {
        tb <- as.data.frame(table(df$val))
        names(tb) <- c("categoria", "contagem")
        tb <- tb[order(-tb$contagem), ]

        p <- ggplot2::ggplot(tb, ggplot2::aes(x = stats::reorder(categoria, contagem), y = contagem)) +
          ggplot2::geom_col(fill = "#0071E3", width = 0.6, alpha = 0.9) +
          ggplot2::coord_flip() +
          .tema_apple() +
          ggplot2::labs(title = paste0("Frequências: ", v_nome), x = "", y = "Contagem")
      }

      .converter_plotly(p)
    })

    # Diagnóstico de Normalidade (Shapiro-Wilk + QQ-Plot Analítico sem erro)
    output$ui_diagnostico_univariada <- shiny::renderUI({
      info <- var_univariada_info()
      shiny::req(info)

      if (info$is_num) {
        v <- stats::na.omit(info$valores)
        n_val <- length(v)
        if (n_val < 3) return(NULL)

        v_amostra <- if (n_val > 5000) sample(v, 5000) else v
        teste_sw <- tryCatch(stats::shapiro.test(v_amostra), error = function(e) NULL)

        card_sw_conteudo <- if (!is.null(teste_sw)) {
          w_val <- teste_sw$statistic
          p_val <- teste_sw$p.value
          is_normal <- p_val >= 0.05

          if (is_normal) {
            # CARD LARGURA TOTAL SE NORMAL
            htmltools::div(
              class = "apple-card",
              htmltools::div(class = "apple-card-title", "Teste de Shapiro-Wilk (Diagnóstico de Normalidade)"),
              htmltools::div(
                style = "margin: 0.6rem 0;",
                htmltools::tags$strong(sprintf("Estatística W: %s | p-valor: %s", .fmt_num(w_val, 3), if (p_val < 0.001) "< 0,001" else .fmt_num(p_val, 3)))
              ),
              htmltools::tags$span(class = "badge-apple badge-apple-ok", "Distribuição compatível com a Normal (p ≥ 0,05)"),
              htmltools::p(style = "margin-top: 0.5rem; font-size: 0.88rem; color: #1D1D1F;",
                           "Adequado para métodos paramétricos (teste t de Student, ANOVA, regressão linear e correlação de Pearson).")
            )
          } else {
            # DUAS COLUNAS LADO A LADO SE NÃO FOR NORMAL
            htmltools::div(
              class = "row",
              htmltools::div(
                class = "col-md-6",
                htmltools::div(
                  class = "apple-card",
                  htmltools::div(class = "apple-card-title", "Teste de Shapiro-Wilk"),
                  htmltools::div(
                    style = "margin: 0.6rem 0;",
                    htmltools::tags$strong(sprintf("Estatística W: %s | p-valor: %s", .fmt_num(w_val, 3), if (p_val < 0.001) "< 0,001" else .fmt_num(p_val, 3)))
                  ),
                  htmltools::tags$span(class = "badge-apple badge-apple-warn", "Distribuição difere da Normal (p < 0,05)"),
                  htmltools::p(style = "margin-top: 0.5rem; font-size: 0.88rem; color: #1D1D1F;",
                               "Evidência de assimetria ou caudas pesadas nos dados amostrais.")
                )
              ),
              htmltools::div(
                class = "col-md-6",
                htmltools::div(
                  class = "apple-card",
                  htmltools::div(class = "apple-card-title", "Recomendação Não-Paramétrica"),
                  htmltools::div(
                    style = "margin: 0.6rem 0;",
                    htmltools::tags$span(class = "badge-apple badge-apple-info", "Métodos Robustos Sugeridos")
                  ),
                  htmltools::p(style = "font-size: 0.88rem; color: #1D1D1F; margin-bottom: 0;",
                               "Priorizar Mediana e IQR para descrever a tendência central. Recomenda-se o uso de testes não-paramétricos (Wilcoxon/Mann-Whitney, Kruskal-Wallis) ou transformação de potência/Box-Cox.")
                )
              )
            )
          }
        } else {
          NULL
        }

        htmltools::div(
          style = "margin-top: 1rem;",
          card_sw_conteudo,
          htmltools::div(
            class = "apple-card",
            htmltools::div(
              class = "apple-card-header",
              htmltools::h5(class = "apple-card-title", "Gráfico Quantil-Quantil Normal (QQ-Plot)")
            ),
            plotly::plotlyOutput("plot_qqplot_univariada", height = "320px")
          )
        )
      } else {
        htmltools::div(
          class = "apple-card",
          style = "margin-top: 1rem;",
          htmltools::div(
            class = "apple-card-header",
            htmltools::h5(class = "apple-card-title", "Tabela de Frequências Detalhada")
          ),
          DT::DTOutput("tab_frequencias_univariada")
        )
      }
    })

    # QQ-Plot Analítico (sem erro de x não encontrado)
    output$plot_qqplot_univariada <- plotly::renderPlotly({
      info <- var_univariada_info()
      shiny::req(info, info$is_num)
      vals <- sort(stats::na.omit(info$valores))
      n <- length(vals)
      if (n < 3) return(NULL)

      # Cálculo exato dos quantis teóricos normais de Blom
      teorico <- stats::qnorm((seq_len(n) - 0.375) / (n + 0.25))
      df_qq <- data.frame(
        Quantil_Teorico = teorico,
        Quantil_Amostral = vals
      )

      # Linha de referência robusta pelos quartis
      y_q <- stats::quantile(vals, c(0.25, 0.75), names = FALSE)
      x_q <- stats::qnorm(c(0.25, 0.75))
      slope <- diff(y_q) / diff(x_q)
      intercept <- y_q[1] - slope * x_q[1]

      p <- ggplot2::ggplot(df_qq, ggplot2::aes(x = Quantil_Teorico, y = Quantil_Amostral)) +
        ggplot2::geom_abline(intercept = intercept, slope = slope, color = "#1D1D1F", linewidth = 0.8, linetype = "dashed") +
        ggplot2::geom_point(color = "#0071E3", alpha = 0.75, size = 2.2) +
        .tema_apple() +
        ggplot2::labs(
          title = "QQ-Plot Normal",
          x = "Quantis Teóricos Normais",
          y = paste0("Amostral (", info$nome, ")")
        )

      .converter_plotly(p)
    })

    # Tabela de Frequências Categórica
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

    # ── ABA 4: ANÁLISE BIVARIADA ─────────────────────────────────────────────
    output$ui_controles_bivariada <- shiny::renderUI({
      df <- rv$dados
      vx <- input$sel_biv_x
      vy <- input$sel_biv_y
      if (is.null(vx) || is.null(vy) || !vx %in% names(df) || !vy %in% names(df)) return(NULL)

      is_x_num <- is.numeric(df[[vx]])
      is_y_num <- is.numeric(df[[vy]])

      if (is_x_num && is_y_num) {
        shiny::checkboxInput("chk_linha_tendencia", "Adicionar Linha de Tendência LOESS", value = TRUE)
      } else {
        NULL
      }
    })

    output$tit_grafico_bivariada <- shiny::renderText({
      vx <- input$sel_biv_x
      vy <- input$sel_biv_y
      if (is.null(vx) || is.null(vy)) return("Relação Bivariada")
      sprintf("Relação: %s vs %s", vy, vx)
    })

    output$plot_bivariada <- plotly::renderPlotly({
      df <- rv$dados
      vx <- input$sel_biv_x
      vy <- input$sel_biv_y
      grp <- input$sel_biv_grupo

      shiny::req(vx, vy, vx %in% names(df), vy %in% names(df))

      df_sub <- df[, c(vx, vy, if (nzchar(grp) && grp %in% names(df)) grp else NULL), drop = FALSE]
      df_sub <- stats::na.omit(df_sub)
      if (nrow(df_sub) == 0) return(NULL)

      is_x_num <- is.numeric(df_sub[[vx]])
      is_y_num <- is.numeric(df_sub[[vy]])
      tem_grp <- nzchar(grp) && grp %in% names(df_sub)

      if (is_x_num && is_y_num) {
        # Scatter plot numérico vs numérico
        p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = .data[[vx]], y = .data[[vy]]))
        if (tem_grp) {
          p <- p +
            ggplot2::geom_point(ggplot2::aes(color = as.factor(.data[[grp]])), alpha = 0.8, size = 2.8) +
            ggplot2::scale_color_brewer(palette = "Set1", name = grp)
        } else {
          p <- p +
            ggplot2::geom_point(color = "#0071E3", alpha = 0.8, size = 2.8)
        }

        if (isTRUE(input$chk_linha_tendencia)) {
          p <- p + ggplot2::geom_smooth(method = "loess", color = "#1D1D1F", fill = "#8E8E93", alpha = 0.25, linewidth = 0.9)
        }

        p <- p + .tema_apple() + ggplot2::labs(x = vx, y = vy)

      } else if (!is_x_num && is_y_num) {
        # Boxplot comparativo categórica x numérica
        p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = as.factor(.data[[vx]]), y = .data[[vy]])) +
          ggplot2::geom_boxplot(fill = "#F2F2F7", color = "#1D1D1F", outlier.color = "#FF3B30", width = 0.5) +
          ggplot2::geom_jitter(width = 0.15, color = "#0071E3", alpha = 0.6, size = 2.2) +
          .tema_apple() +
          ggplot2::labs(x = vx, y = vy)

      } else if (is_x_num && !is_y_num) {
        # Boxplot invertido
        p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = as.factor(.data[[vy]]), y = .data[[vx]])) +
          ggplot2::geom_boxplot(fill = "#F2F2F7", color = "#1D1D1F", outlier.color = "#FF3B30", width = 0.5) +
          ggplot2::geom_jitter(width = 0.15, color = "#0071E3", alpha = 0.6, size = 2.2) +
          .tema_apple() +
          ggplot2::labs(x = vy, y = vx)

      } else {
        # Categórica vs Categórica (Barras agrupadas)
        p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = as.factor(.data[[vx]]), fill = as.factor(.data[[vy]]))) +
          ggplot2::geom_bar(position = "dodge", alpha = 0.9) +
          ggplot2::scale_fill_brewer(palette = "Set2", name = vy) +
          .tema_apple() +
          ggplot2::labs(x = vx, y = "Contagem")
      }

      .converter_plotly(p)
    })

    # Diagnóstico Estatístico da Análise Bivariada com Interpretação Semântica
    output$ui_teste_bivariado <- shiny::renderUI({
      df <- rv$dados
      vx <- input$sel_biv_x
      vy <- input$sel_biv_y
      shiny::req(vx, vy, vx %in% names(df), vy %in% names(df))

      df_sub <- stats::na.omit(df[, c(vx, vy)])
      if (nrow(df_sub) < 4) return(NULL)

      is_x_num <- is.numeric(df_sub[[vx]])
      is_y_num <- is.numeric(df_sub[[vy]])

      if (is_x_num && is_y_num) {
        # Correlação de Pearson & Spearman
        t_pearson <- tryCatch(stats::cor.test(df_sub[[vx]], df_sub[[vy]], method = "pearson"), error = function(e) NULL)
        t_spearman <- tryCatch(stats::cor.test(df_sub[[vx]], df_sub[[vy]], method = "spearman"), error = function(e) NULL)

        r_val <- if (!is.null(t_pearson)) t_pearson$estimate else NA
        p_pearson <- if (!is.null(t_pearson)) t_pearson$p.value else NA
        ic_pearson <- if (!is.null(t_pearson$conf.int)) t_pearson$conf.int else c(NA, NA)

        rho_val <- if (!is.null(t_spearman)) t_spearman$estimate else NA
        p_spearman <- if (!is.null(t_spearman)) t_spearman$p.value else NA

        # Classificação semântica da correlação de Pearson
        abs_r <- abs(r_val)
        dir_str <- if (r_val > 0) "positiva" else "negativa"
        mag_str <- if (abs_r >= 0.7) "Forte" else if (abs_r >= 0.4) "Moderada" else if (abs_r >= 0.2) "Fraca" else "Muito fraca / nula"
        sig_str <- if (!is.na(p_pearson) && p_pearson < 0.05) "significativa (p < 0,05)" else "não significativa (p ≥ 0,05)"

        badge_cor_cls <- if (!is.na(p_pearson) && p_pearson < 0.05) "badge-apple-ok" else "badge-apple-neutral"

        htmltools::div(
          class = "row",
          htmltools::div(
            class = "col-md-6",
            htmltools::tags$strong("Correlação Linear de Pearson (r)"),
            htmltools::div(
              style = "margin: 0.4rem 0;",
              sprintf("r = %s | p-valor: %s", .fmt_num(r_val, 3), if (!is.na(p_pearson) && p_pearson < 0.001) "< 0,001" else .fmt_num(p_pearson, 3))
            ),
            htmltools::div(
              style = "font-size: 0.84rem; color: #6E6E73; margin-bottom: 0.5rem;",
              sprintf("Intervalo de Confiança (95%%): [%s; %s]", .fmt_num(ic_pearson[1], 3), .fmt_num(ic_pearson[2], 3))
            ),
            htmltools::tags$span(class = paste("badge-apple", badge_cor_cls), sprintf("%s correlação %s %s", mag_str, dir_str, sig_str))
          ),
          htmltools::div(
            class = "col-md-6",
            htmltools::tags$strong("Correlação Monotônica de Spearman (ρ)"),
            htmltools::div(
              style = "margin: 0.4rem 0;",
              sprintf("ρ = %s | p-valor: %s", .fmt_num(rho_val, 3), if (!is.na(p_spearman) && p_spearman < 0.001) "< 0,001" else .fmt_num(p_spearman, 3))
            ),
            htmltools::div(
              style = "font-size: 0.84rem; color: #6E6E73; margin-bottom: 0.5rem;",
              "Baseado em postos ordinais, robusto a valores extremos."
            ),
            htmltools::tags$span(
              class = "badge-apple badge-apple-info",
              if (!is.na(p_spearman) && p_spearman < 0.05) "Associação monotônica significativa (p < 0,05)" else "Associação monotônica não significativa"
            )
          )
        )

      } else if ((!is_x_num && is_y_num) || (is_x_num && !is_y_num)) {
        # Comparação de Médias entre Grupos (Welch ou ANOVA)
        fator_var <- if (!is_x_num) vx else vy
        num_var   <- if (is_x_num) vx else vy

        df_sub[[fator_var]] <- as.factor(df_sub[[fator_var]])
        n_niveis <- length(levels(df_sub[[fator_var]]))

        if (n_niveis == 2) {
          t_welch <- tryCatch(stats::t.test(df_sub[[num_var]] ~ df_sub[[fator_var]]), error = function(e) NULL)
          if (!is.null(t_welch)) {
            p_val <- t_welch$p.value
            htmltools::div(
              htmltools::tags$strong("Teste t de Welch (Comparação de 2 Grupos)"),
              htmltools::div(
                style = "margin: 0.4rem 0;",
                sprintf("Estatística t = %s | Graus de Liberdade = %s | p-valor: %s",
                        .fmt_num(t_welch$statistic, 3), .fmt_num(t_welch$parameter, 1),
                        if (p_val < 0.001) "< 0,001" else .fmt_num(p_val, 3))
              ),
              htmltools::tags$span(
                class = paste("badge-apple", if (p_val < 0.05) "badge-apple-ok" else "badge-apple-neutral"),
                if (p_val < 0.05) "Diferença de médias estatisticamente significativa (p < 0,05)" else "Sem evidência de diferença de médias (p ≥ 0,05)"
              )
            )
          }
        } else if (n_niveis > 2) {
          fit_aov <- tryCatch(stats::aov(df_sub[[num_var]] ~ df_sub[[fator_var]]), error = function(e) NULL)
          if (!is.null(fit_aov)) {
            sm <- summary(fit_aov)[[1]]
            f_val <- sm["df_sub[[fator_var]]", "F value"]
            p_val <- sm["df_sub[[fator_var]]", "Pr(>F)"]
            htmltools::div(
              htmltools::tags$strong(sprintf("Análise de Variância — ANOVA (%s Níveis)", n_niveis)),
              htmltools::div(
                style = "margin: 0.4rem 0;",
                sprintf("Estatística F = %s | p-valor: %s", .fmt_num(f_val, 3), if (!is.na(p_val) && p_val < 0.001) "< 0,001" else .fmt_num(p_val, 3))
              ),
              htmltools::tags$span(
                class = paste("badge-apple", if (!is.na(p_val) && p_val < 0.05) "badge-apple-ok" else "badge-apple-neutral"),
                if (!is.na(p_val) && p_val < 0.05) "Pelo menos um dos grupos difere significativamente (p < 0,05)" else "Médias homogêneas entre os grupos (p ≥ 0,05)"
              )
            )
          }
        }
      } else {
        # Categórica vs Categórica (Qui-Quadrado)
        tb <- table(df_sub[[vx]], df_sub[[vy]])
        t_qui <- tryCatch(stats::chisq.test(tb), error = function(e) NULL)
        if (!is.null(t_qui)) {
          p_val <- t_qui$p.value
          htmltools::div(
            htmltools::tags$strong("Teste Qui-Quadrado de Independência de Pearson"),
            htmltools::div(
              style = "margin: 0.4rem 0;",
              sprintf("Estatística X² = %s | Graus de Liberdade = %s | p-valor: %s",
                      .fmt_num(t_qui$statistic, 3), t_qui$parameter,
                      if (p_val < 0.001) "< 0,001" else .fmt_num(p_val, 3))
            ),
            htmltools::tags$span(
              class = paste("badge-apple", if (p_val < 0.05) "badge-apple-ok" else "badge-apple-neutral"),
              if (p_val < 0.05) "Existe associação estatisticamente significativa entre as variáveis (p < 0,05)" else "Variáveis independentes (p ≥ 0,05)"
            )
          )
        }
      }
    })

    # ── ABA 5: ANÁLISE MULTIVARIADA ──────────────────────────────────────────
    output$plot_matriz_exploratoria <- shiny::renderPlot({
      # Depende do botão atualizar ou carrega inicial
      input$btn_gerar_matriz

      shiny::isolate({
        df <- rv$dados
        vars <- input$sel_matriz_vars
        grp <- input$sel_matriz_grupo
      })

      shiny::req(df, vars, length(vars) >= 2)
      grp_val <- if (!is.null(grp) && nzchar(grp)) grp else NULL

      p <- .gerar_matriz_exploratoria(df, vars, grupo = grp_val)
      if (!is.null(p)) print(p)
    })

    # ── MODAL: CÓDIGO R GERADO COM ETAPAS ENUMERADAS (1, 2, ...) ─────────────
    shiny::observeEvent(input$btn_ver_codigo, {
      aba_ativa <- input$nav_principal %||% "Visão Geral"
      ds <- rv$nome_dataset

      codigo <- switch(
        aba_ativa,
        "Visão Geral" = paste(c(
          "# Diagnóstico estrutural dos dados",
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

        "Resumo Descritivo" = paste(c(
          "# Resumo estatístico descritivo completo",
          "",
          "# 1. Estatísticas das variáveis numéricas",
          sprintf("sapply(%s[sapply(%s, is.numeric)], function(x) c(Media = mean(x, na.rm = TRUE), DP = sd(x, na.rm = TRUE), Mediana = median(x, na.rm = TRUE), IQR = IQR(x, na.rm = TRUE)))", ds, ds),
          "",
          "# 2. Frequências das variáveis categóricas",
          sprintf("lapply(%s[!sapply(%s, is.numeric)], table)", ds, ds)
        ), collapse = "\n"),

        "Análise Univariada" = {
          v <- input$sel_var_univariada %||% "var"
          paste(c(
            "# Análise univariada",
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

        "Análise Bivariada" = {
          x <- input$sel_biv_x %||% "x"
          y <- input$sel_biv_y %||% "y"
          paste(c(
            "# Análise bivariada",
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

        "Análise Multivariada" = {
          vars_vec <- input$sel_matriz_vars %||% c("var1", "var2")
          vars_str <- paste(sprintf("'%s'", vars_vec), collapse = ", ")
          paste(c(
            "# Análise multivariada",
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
