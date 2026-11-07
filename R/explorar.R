#' Explorar Dados Interativamente com Interface Estilo Apple
#'
#' Abre uma interface gráfica intuitiva, moderna e minimalista para análise
#' exploratória de dados, diagnóstico de tipos, valores ausentes, estatísticas
#' descritivas, gráficos univariados, bivariados, testes de hipóteses estatísticos
#' e matriz exploratória multivariada interativa com Plotly.
#'
#' @param dados Data frame opcional a ser explorado diretamente. Se \code{NULL},
#'   a interface é iniciada permitindo o upload multiformato (.csv, .xlsx, .rds, .parquet)
#'   ou a seleção de conjuntos de dados didáticos prontos.
#' @param viewer Modo de exibição da interface. Opções:
#'   \itemize{
#'     \item \code{"window"}: Janela de diálogo nativa dedicada (padrão se no RStudio, ou navegador).
#'     \item \code{"browser"}: Abre no navegador web padrão do sistema.
#'     \item \code{"dialog"}: Janela modal no RStudio Viewer.
#'     \item \code{"pane"}: Painel embutido no RStudio Viewer.
#'   }
#' @param largura Largura da janela (em pixels) quando usado em modo diálogo ou janela (padrão 1250).
#' @param altura Altura da janela (em pixels) quando usado em modo diálogo ou janela (padrão 820).
#'
#' @return Retorna de forma invisível o conjunto de dados carregado/explorado.
#'
#' @examples
#' \dontrun{
#' # Iniciar interface para carregar arquivo
#' explorar()
#'
#' # Explorar um data frame diretamente
#' explorar(mtcars)
#'
#' # Usando a sintaxe de pipe nativo do R
#' iris |> explorar()
#' }
#'
#' @export
explorar <- function(dados = NULL,
                     viewer = c("window", "browser", "dialog", "pane"),
                     largura = 1250,
                     altura = 820) {
  viewer <- match.arg(viewer)

  nome_dados <- "dados"
  if (!is.null(dados)) {
    nome_capturado <- deparse(substitute(dados))
    if (length(nome_capturado) == 1 && nzchar(nome_capturado) && !grepl("^[<(]", nome_capturado)) {
      nome_dados <- nome_capturado
    }

    if (!is.data.frame(dados)) {
      dados <- tryCatch(
        as.data.frame(dados),
        error = function(e) {
          stop("O argumento 'dados' deve ser um data frame ou conversivel em data frame.", call. = FALSE)
        }
      )
    }
  }

  # Definir viewer apropriado
  shiny_viewer <- switch(
    viewer,
    "window" = {
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        shiny::dialogViewer(
          dialogName = paste0("estatflowR \u2014 ", if (!is.null(dados)) nome_dados else "Explorador"),
          width = largura,
          height = altura
        )
      } else {
        shiny::browserViewer()
      }
    },
    "dialog" = {
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        shiny::dialogViewer(
          dialogName = paste0("estatflowR \u2014 ", if (!is.null(dados)) nome_dados else "Explorador"),
          width = largura,
          height = altura
        )
      } else {
        shiny::browserViewer()
      }
    },
    "pane" = {
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        shiny::paneViewer(min_height = "fill")
      } else {
        shiny::browserViewer()
      }
    },
    "browser" = shiny::browserViewer()
  )

  app <- shiny::shinyApp(
    ui = .criar_app_ui(dados_iniciais = dados, nome_dataset = nome_dados),
    server = .criar_app_server(dados_iniciais = dados, nome_dataset = nome_dados)
  )

  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::runGadget(app, viewer = shiny_viewer, stopOnCancel = TRUE)
  } else {
    stop("O pacote 'shiny' e necessario para executar 'explorar()'.", call. = FALSE)
  }

  invisible(dados)
}

#' Addin do RStudio para o estatflowR
#'
#' Abre o \code{estatflowR} a partir do menu Addins do RStudio. Se houver um nome
#' de objeto selecionado no editor de código, tenta abri-lo automaticamente.
#'
#' @export
explorar_addin <- function() {
  dados <- NULL
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    context <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
    if (!is.null(context)) {
      sel <- context$selection[[1]]$text
      sel <- trimws(sel)
      if (nzchar(sel) && exists(sel, envir = .GlobalEnv)) {
        obj <- get(sel, envir = .GlobalEnv)
        if (is.data.frame(obj)) {
          dados <- obj
        }
      }
    }
  }

  explorar(dados = dados, viewer = "window")
}
