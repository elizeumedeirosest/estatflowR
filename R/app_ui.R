# ─────────────────────────────────────────────────────────────────────────────
# MÓDULO UI: INTERFACE GRÁFICA MINIMALISTA APPLE (ESTATFLOWR)
# ─────────────────────────────────────────────────────────────────────────────

#' Cria a Interface do Usuário (UI) do estatflowR
#' @return Objeto UI Shiny
#' @noRd
.criar_app_ui <- function(dados_iniciais = NULL, nome_dataset = "dados") {

  apple_theme <- bslib::bs_theme(
    version = 5,
    bg = "#F5F5F7",
    fg = "#1D1D1F",
    primary = "#0071E3",
    secondary = "#86868B",
    success = "#34C759",
    warning = "#FF9500",
    danger = "#FF3B30",
    base_font = bslib::font_google("Inter", local = FALSE),
    code_font = bslib::font_google("JetBrains Mono", local = FALSE)
  )

  bslib::page_navbar(
    theme = apple_theme,
    title = htmltools::tags$span(
      htmltools::tags$svg(
        style = "margin-right: 8px; vertical-align: -2px;",
        width = "20", height = "20", viewBox = "0 0 24 24", fill = "none",
        stroke = "#0071E3", strokeWidth = "2.2", strokeLinecap = "round", strokeLinejoin = "round",
        htmltools::tags$path(d = "M3 3v18h18"),
        htmltools::tags$path(d = "M18 17V9"),
        htmltools::tags$path(d = "M13 17V5"),
        htmltools::tags$path(d = "M8 17v-3")
      ),
      htmltools::tags$strong("estatflow", style = "letter-spacing: -0.02em; font-weight: 700;"),
      htmltools::tags$span("R", style = "color: #0071E3; font-weight: 800;")
    ),
    id = "nav_principal",
    navbar_options = if (utils::packageVersion("bslib") >= "0.9.0") bslib::navbar_options(underline = TRUE) else NULL,
    underline = if (utils::packageVersion("bslib") < "0.9.0") TRUE else NULL,

    header = htmltools::tags$head(
      htmltools::tags$link(rel = "stylesheet", href = "estatflowR/apple_style.css"),
      htmltools::tags$style(htmltools::HTML("
        .navbar { background: rgba(255, 255, 255, 0.9) !important; backdrop-filter: blur(16px); }
        .tab-content { padding-top: 1.25rem; }
      "))
    ),

    # ── ABA 1: VISÃO GERAL DO BANCO ──────────────────────────────────────────
    bslib::nav_panel(
      title = "Visão Geral",
      htmltools::div(
        class = "container-fluid",

        # Top Bar: Seletor de Dataset ou Upload
        htmltools::div(
          class = "apple-card",
          style = "padding: 1rem 1.5rem; margin-bottom: 1.25rem;",
          htmltools::div(
            class = "row align-items-center",
            htmltools::div(
              class = "col-md-5",
              htmltools::div(
                style = "display: flex; align-items: center; gap: 0.75rem;",
                htmltools::tags$strong("Banco de Dados:"),
                shiny::uiOutput("ui_dataset_badge")
              )
            ),
            htmltools::div(
              class = "col-md-7 text-end",
              shiny::actionButton(
                "btn_abrir_upload", "Carregar Outro Arquivo...",
                class = "btn-apple-secondary", icon = shiny::icon("upload")
              ),
              shiny::actionButton(
                "btn_dados_demo", "Dados de Exemplo",
                class = "btn-apple-secondary", icon = shiny::icon("database")
              )
            )
          )
        ),

        # KPIs Rápidos do Dataset
        shiny::uiOutput("ui_kpis_dataset"),

        # Tabela Inteligente de Variáveis
        htmltools::div(
          class = "apple-card",
          htmltools::div(
            class = "apple-card-header",
            htmltools::div(
              htmltools::h5(class = "apple-card-title", "Dicionário e Diagnóstico de Variáveis"),
              htmltools::div(class = "apple-card-subtitle", "Identificação automática de tipos, preenchimento e estatísticas rápidas")
            )
          ),
          DT::DTOutput("tab_diagnostico_vars")
        )
      )
    ),

    # ── ABA 2: VARIÁVEL ISOLADA (UNIVARIADA) ─────────────────────────────────
    bslib::nav_panel(
      title = "Variável Isolada",
      htmltools::div(
        class = "container-fluid",
        htmltools::div(
          class = "row",
          # Painel Lateral de Controle
          htmltools::div(
            class = "col-lg-3 col-md-4",
            htmltools::div(
              class = "apple-card",
              htmltools::h5(class = "apple-card-title", "Explorar Variável"),
              htmltools::p(class = "apple-card-subtitle", "Foco em uma coluna do dataset"),
              htmltools::hr(style = "margin: 0.75rem 0; border-color: #F0F0F2;"),
              shiny::selectInput("sel_var_univariada", "Selecione a Variável:", choices = NULL),
              shiny::uiOutput("ui_controles_univariada")
            )
          ),
          # Painel Central de Gráficos e Diagnósticos
          htmltools::div(
            class = "col-lg-9 col-md-8",
            # Cards KPI da variável
            shiny::uiOutput("ui_kpis_univariada"),
            # Gráfico Principal
            htmltools::div(
              class = "apple-card",
              htmltools::div(
                class = "apple-card-header",
                htmltools::h5(class = "apple-card-title", shiny::textOutput("tit_grafico_univariada")),
                shiny::uiOutput("ui_opcoes_graf_univariada")
              ),
              plotly::plotlyOutput("plot_univariada", height = "420px")
            ),
            # Diagnóstico de Normalidade (se numérica) ou Frequências (se categórica)
            shiny::uiOutput("ui_diagnostico_univariada")
          )
        )
      )
    ),

    # ── ABA 3: RELAÇÕES & GRUPOS (BIVARIADA) ─────────────────────────────────
    bslib::nav_panel(
      title = "Relações & Grupos",
      htmltools::div(
        class = "container-fluid",
        htmltools::div(
          class = "row",
          # Controles
          htmltools::div(
            class = "col-lg-3 col-md-4",
            htmltools::div(
              class = "apple-card",
              htmltools::h5(class = "apple-card-title", "Análise de Relações"),
              htmltools::p(class = "apple-card-subtitle", "Comparações bivariadas e por grupos"),
              htmltools::hr(style = "margin: 0.75rem 0; border-color: #F0F0F2;"),
              shiny::selectInput("sel_biv_x", "Variável X (Eixo Horizontal):", choices = NULL),
              shiny::selectInput("sel_biv_y", "Variável Y (Eixo Vertical):", choices = NULL),
              shiny::selectInput("sel_biv_grupo", "Grupo / Cor (Opcional):", choices = c("Nenhum" = "")),
              shiny::uiOutput("ui_controles_bivariada")
            )
          ),
          # Conteúdo
          htmltools::div(
            class = "col-lg-9 col-md-8",
            # Gráfico Bivariado
            htmltools::div(
              class = "apple-card",
              htmltools::div(
                class = "apple-card-header",
                htmltools::h5(class = "apple-card-title", shiny::textOutput("tit_grafico_bivariada")),
                shiny::uiOutput("ui_opcoes_graf_bivariada")
              ),
              plotly::plotlyOutput("plot_bivariada", height = "430px")
            ),
            # Card de Teste Estatístico Associado
            htmltools::div(
              class = "apple-card",
              htmltools::div(
                class = "apple-card-header",
                htmltools::h5(class = "apple-card-title", "Diagnóstico Estatístico Automático")
              ),
              shiny::uiOutput("ui_teste_bivariado")
            )
          )
        )
      )
    ),

    # ── ABA 4: MATRIZ EXPLORATÓRIA (PAIRS PLOT) ──────────────────────────────
    bslib::nav_panel(
      title = "Matriz Exploratória",
      htmltools::div(
        class = "container-fluid",
        htmltools::div(
          class = "row",
          htmltools::div(
            class = "col-lg-3 col-md-4",
            htmltools::div(
              class = "apple-card",
              htmltools::h5(class = "apple-card-title", "Matriz Multivariada"),
              htmltools::p(class = "apple-card-subtitle", "Pairs Plot com densidade, dispersão e correlação"),
              htmltools::hr(style = "margin: 0.75rem 0; border-color: #F0F0F2;"),
              shiny::selectizeInput(
                "sel_matriz_vars", "Selecione de 2 a 6 Variáveis:",
                choices = NULL, multiple = TRUE, options = list(maxItems = 6)
              ),
              shiny::selectInput("sel_matriz_grupo", "Agrupar por Fator (Opcional):", choices = c("Nenhum" = "")),
              shiny::actionButton("btn_gerar_matriz", "Atualizar Matriz", class = "btn-apple w-100", icon = shiny::icon("sync"))
            )
          ),
          htmltools::div(
            class = "col-lg-9 col-md-8",
            htmltools::div(
              class = "apple-card",
              htmltools::div(
                class = "apple-card-header",
                htmltools::h5(class = "apple-card-title", "Matriz de Correlações e Distribuições")
              ),
              shiny::plotOutput("plot_matriz_exploratoria", height = "650px")
            )
          )
        )
      )
    ),

    # ── BOTÃO DE AÇÃO NO CANTO SUPERIOR DIREITO ──────────────────────────────
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::actionButton(
        "btn_ver_codigo", "Código R",
        class = "btn-apple-secondary", icon = shiny::icon("code"),
        style = "margin-top: 0.35rem; margin-right: 0.5rem;"
      )
    ),
    bslib::nav_item(
      shiny::actionButton(
        "btn_fechar_app", "Concluir",
        class = "btn-apple", icon = shiny::icon("check"),
        style = "margin-top: 0.35rem;"
      )
    )
  )
}
