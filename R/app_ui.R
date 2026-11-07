# ─────────────────────────────────────────────────────────────────────────────
# MÓDULO UI: INTERFACE GRÁFICA MINIMALISTA APPLE (ESTATFLOWR)
# ─────────────────────────────────────────────────────────────────────────────

#' Cria a Interface do Usuário (UI) do estatflowR
#' @return Objeto UI Shiny
#' @noRd
.criar_app_ui <- function(dados_iniciais = NULL, nome_dataset = "dados") {

  apple_theme <- bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#1D1D1F",
    primary = "#0071E3",
    secondary = "#6E6E73",
    success = "#34C759",
    warning = "#FF9500",
    danger = "#FF3B30",
    base_font = bslib::font_google("Inter", local = FALSE),
    code_font = bslib::font_google("JetBrains Mono", local = FALSE)
  )

  bslib::page_navbar(
    theme = apple_theme,
    title = htmltools::tags$span(
      htmltools::tags$span("estat", style = "font-weight: 800; color: #1D1D1F; font-size: 1.25rem; letter-spacing: -0.5px;"),
      htmltools::tags$span("flow", style = "font-weight: 800; color: #6E6E73; font-size: 1.25rem; letter-spacing: -0.5px;"),
      htmltools::tags$span("R", style = "font-weight: 800; color: #0071E3; font-size: 1.25rem; letter-spacing: -0.5px;")
    ),
    id = "nav_principal",
    navbar_options = if (utils::packageVersion("bslib") >= "0.9.0") bslib::navbar_options(underline = TRUE) else NULL,
    underline = if (utils::packageVersion("bslib") < "0.9.0") TRUE else NULL,

    header = htmltools::tags$head(
      htmltools::tags$link(rel = "stylesheet", href = "estatflowR/apple_style.css"),
      htmltools::tags$style(htmltools::HTML("
        body { background-color: #FFFFFF !important; }
        .navbar { background: #FFFFFF !important; border-bottom: 1px solid #E5E5EA !important; }
        .tab-content { padding-top: 1.25rem; background-color: #FFFFFF !important; }
      "))
    ),

    # ── ABA 1: VISÃO GERAL ───────────────────────────────────────────────────
    bslib::nav_panel(
      title = "Visão Geral",
      htmltools::div(
        class = "container-fluid",

        # Top Bar: Identificação da Base e Upload
        htmltools::div(
          class = "apple-card",
          style = "padding: 0.85rem 1.25rem; margin-bottom: 1.25rem;",
          htmltools::div(
            class = "row align-items-center",
            htmltools::div(
              class = "col-md-6",
              htmltools::div(
                style = "display: flex; align-items: center; gap: 0.75rem;",
                htmltools::tags$strong("Banco de Dados:"),
                shiny::uiOutput("ui_dataset_badge")
              )
            ),
            htmltools::div(
              class = "col-md-6 text-end",
              shiny::actionButton(
                "btn_abrir_upload", "Carregar Outro Arquivo...",
                class = "btn-apple-secondary", icon = shiny::icon("upload")
              )
            )
          )
        ),

        # KPIs Estruturais da Base
        shiny::uiOutput("ui_kpis_dataset"),

        # Dicionário e Estrutura de Variáveis
        htmltools::div(
          class = "apple-card",
          htmltools::div(
            class = "apple-card-header",
            htmltools::div(
              htmltools::h5(class = "apple-card-title", "Dicionário e Estrutura de Variáveis"),
              htmltools::div(class = "apple-card-subtitle", "Identificação automática de tipos, dados ausentes e resumo estrutural")
            )
          ),
          DT::DTOutput("tab_diagnostico_vars")
        )
      )
    ),

    # ── ABA 2: RESUMO DESCRITIVO ─────────────────────────────────────────────
    bslib::nav_panel(
      title = "Resumo Descritivo",
      htmltools::div(
        class = "container-fluid",

        # Tabela 1: Estatísticas Descritivas Numéricas
        htmltools::div(
          class = "apple-card",
          htmltools::div(
            class = "apple-card-header",
            htmltools::div(
              htmltools::h5(class = "apple-card-title", "Estatísticas Descritivas — Variáveis Numéricas"),
              htmltools::div(class = "apple-card-subtitle", "Média, desvio padrão, mediana, quartis, dispersão, assimetria e curtose")
            )
          ),
          DT::DTOutput("tab_descritiva_numerica")
        ),

        # Tabela 2: Estatísticas Descritivas Categóricas (se houver)
        shiny::uiOutput("ui_bloco_descritiva_categorica"),

        # Tabela 3: Diagnóstico de Dados Ausentes (NAs)
        htmltools::div(
          class = "apple-card",
          htmltools::div(
            class = "apple-card-header",
            htmltools::div(
              htmltools::h5(class = "apple-card-title", "Diagnóstico de Dados Ausentes (NAs) por Variável"),
              htmltools::div(class = "apple-card-subtitle", "Contagem de válidos, ausentes, proporção e classificação de integridade")
            )
          ),
          DT::DTOutput("tab_diagnostico_nas")
        )
      )
    ),

    # ── ABA 3: ANÁLISE UNIVARIADA ────────────────────────────────────────────
    bslib::nav_panel(
      title = "Análise Univariada",
      htmltools::div(
        class = "container-fluid",
        htmltools::div(
          class = "row",
          # Painel Lateral de Controle
          htmltools::div(
            class = "col-lg-3 col-md-4",
            htmltools::div(
              class = "apple-card",
              htmltools::h5(class = "apple-card-title", "Análise Univariada"),
              htmltools::p(class = "apple-card-subtitle", "Inspeção detalhada de uma variável"),
              htmltools::hr(style = "margin: 0.75rem 0; border-color: #E5E5EA;"),
              shiny::selectInput("sel_var_univariada", "Selecione a Variável:", choices = NULL),
              shiny::uiOutput("ui_controles_univariada")
            )
          ),
          # Painel Central de Gráficos e Diagnósticos
          htmltools::div(
            class = "col-lg-9 col-md-8",
            # Tabela de Métricas Horizontais com Fundo Branco
            shiny::uiOutput("ui_tabela_horizontal_univariada"),
            # Gráfico Principal
            htmltools::div(
              class = "apple-card",
              htmltools::div(
                class = "apple-card-header",
                htmltools::h5(class = "apple-card-title", shiny::textOutput("tit_grafico_univariada")),
                shiny::uiOutput("ui_opcoes_graf_univariada")
              ),
              plotly::plotlyOutput("plot_univariada", height = "440px")
            ),
            # Diagnóstico de Normalidade (Shapiro-Wilk & QQ-Plot) ou Frequências
            shiny::uiOutput("ui_diagnostico_univariada")
          )
        )
      )
    ),

    # ── ABA 4: ANÁLISE BIVARIADA ─────────────────────────────────────────────
    bslib::nav_panel(
      title = "Análise Bivariada",
      htmltools::div(
        class = "container-fluid",
        htmltools::div(
          class = "row",
          # Controles
          htmltools::div(
            class = "col-lg-3 col-md-4",
            htmltools::div(
              class = "apple-card",
              htmltools::h5(class = "apple-card-title", "Análise Bivariada"),
              htmltools::p(class = "apple-card-subtitle", "Relações entre pares e comparações de grupos"),
              htmltools::hr(style = "margin: 0.75rem 0; border-color: #E5E5EA;"),
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
              plotly::plotlyOutput("plot_bivariada", height = "440px")
            ),
            # Card de Diagnóstico e Testes Estatísticos
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

    # ── ABA 5: ANÁLISE MULTIVARIADA ──────────────────────────────────────────
    bslib::nav_panel(
      title = "Análise Multivariada",
      htmltools::div(
        class = "container-fluid",
        htmltools::div(
          class = "row",
          htmltools::div(
            class = "col-lg-3 col-md-4",
            htmltools::div(
              class = "apple-card",
              htmltools::h5(class = "apple-card-title", "Análise Multivariada"),
              htmltools::p(class = "apple-card-subtitle", "Matriz Pairs Plot com densidade, dispersão e correlação"),
              htmltools::hr(style = "margin: 0.75rem 0; border-color: #E5E5EA;"),
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
