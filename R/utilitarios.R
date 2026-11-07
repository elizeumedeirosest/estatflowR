# ─────────────────────────────────────────────────────────────────────────────
# MÓDULO: UTILITÁRIOS, LEITORES MULTIFORMATO E DIAGNÓSTICO DE TIPOS
# ─────────────────────────────────────────────────────────────────────────────

#' Leitor Universal de Arquivos de Dados
#' @param caminho Caminho do arquivo no disco
#' @return data.frame importado
#' @noRd
.ler_dados_universal <- function(caminho) {
  if (missing(caminho) || is.null(caminho) || !file.exists(caminho)) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(caminho))

  df <- tryCatch({
    switch(
      ext,
      "csv" = utils::read.csv(caminho, stringsAsFactors = FALSE, check.names = FALSE),
      "tsv" = utils::read.delim(caminho, stringsAsFactors = FALSE, check.names = FALSE),
      "txt" = utils::read.table(caminho, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE),
      "rds" = as.data.frame(readRDS(caminho)),
      "xlsx" = if (requireNamespace("readxl", quietly = TRUE)) as.data.frame(readxl::read_excel(caminho)) else utils::read.csv(caminho),
      "xls"  = if (requireNamespace("readxl", quietly = TRUE)) as.data.frame(readxl::read_excel(caminho)) else utils::read.csv(caminho),
      "parquet" = if (requireNamespace("arrow", quietly = TRUE)) as.data.frame(arrow::read_parquet(caminho)) else stop("Pacote 'arrow' necessário."),
      "dta"  = if (requireNamespace("haven", quietly = TRUE)) as.data.frame(haven::read_dta(caminho)) else stop("Pacote 'haven' necessário."),
      "sav"  = if (requireNamespace("haven", quietly = TRUE)) as.data.frame(haven::read_sav(caminho)) else stop("Pacote 'haven' necessário."),
      utils::read.csv(caminho, stringsAsFactors = FALSE)
    )
  }, error = function(e) {
    message("[estatflowR] Erro ao ler arquivo: ", e$message)
    NULL
  })

  as.data.frame(df)
}

#' Diagnóstico Inteligente de Variáveis
#' @param df Data frame
#' @return Tabela informativa de metadados
#' @noRd
.diagnosticar_variaveis <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(NULL)

  n_total <- nrow(df)
  cols <- names(df)

  info_list <- list()

  for (col in cols) {
    v <- df[[col]]
    n_na <- sum(is.na(v))
    pct_na <- if (n_total > 0) (n_na / n_total) * 100 else 0
    n_validos <- n_total - n_na
    n_unicos <- length(unique(stats::na.omit(v)))

    # Determinação do Tipo Amigável
    tipo_amigavel <- if (inherits(v, c("Date", "POSIXt"))) {
      "Data / Hora"
    } else if (is.numeric(v)) {
      if (is.integer(v) || (all(v %% 1 == 0, na.rm = TRUE) && n_unicos < 15)) {
        "Numérica Discreta"
      } else {
        "Numérica Contínua"
      }
    } else if (is.factor(v)) {
      if (is.ordered(v)) "Fator Ordinal" else "Fator Nominal"
    } else if (is.logical(v)) {
      "Booleano / Lógico"
    } else {
      if (n_unicos <= min(15, n_total / 2)) "Categórica / Texto" else "Texto Livre"
    }

    # Resumo descritivo simplificado
    resumo_texto <- if (is.numeric(v)) {
      v_clean <- stats::na.omit(v)
      if (length(v_clean) > 0) {
        sprintf("Média: %s | DP: %s | [%s; %s]",
                .fmt_num(mean(v_clean)), .fmt_num(stats::sd(v_clean)),
                .fmt_num(min(v_clean)), .fmt_num(max(v_clean)))
      } else "-"
    } else {
      v_clean <- stats::na.omit(v)
      if (length(v_clean) > 0) {
        tb <- sort(table(v_clean), decreasing = TRUE)
        moda_val <- names(tb)[1]
        sprintf("%d categorias | Moda: %s (%d)", length(tb), moda_val, as.integer(tb[1]))
      } else "-"
    }

    status_na <- if (pct_na == 0) "ok" else if (pct_na <= 5) "warn" else "crit"

    info_list[[length(info_list) + 1]] <- data.frame(
      Variavel       = col,
      Tipo           = tipo_amigavel,
      Validos        = n_validos,
      Ausentes       = n_na,
      Pct_Ausentes   = pct_na,
      Status_NA      = status_na,
      Resumo         = resumo_texto,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, info_list)
}

#' Datasets Didáticos Embutidos para Demonstração
#' @noRd
.obter_dados_exemplo <- function(nome = c("saude", "qualidade", "vendas")) {
  nome <- match.arg(nome)

  if (nome == "saude") {
    set.seed(42)
    n <- 120
    data.frame(
      Paciente_ID  = seq_len(n),
      Idade        = round(rnorm(n, 52, 12)),
      Pressao_Art  = round(rnorm(n, 128, 15)),
      Glicose      = round(rnorm(n, 102, 22)),
      Colesterol   = round(rnorm(n, 210, 35)),
      Grupo        = factor(sample(c("Controle", "Tratamento A", "Tratamento B"), n, replace = TRUE)),
      Desfecho     = factor(sample(c("Recuperado", "Não Recuperado"), n, replace = TRUE, prob = c(0.72, 0.28)))
    )
  } else if (nome == "qualidade") {
    set.seed(123)
    n <- 90
    data.frame(
      Lote         = factor(rep(1:18, each = 5)),
      Comprimento  = round(rnorm(n, 50.2, 1.4), 2),
      Espessura    = round(rnorm(n, 10.1, 0.35), 2),
      Peso         = round(rnorm(n, 125, 4.2), 1),
      Turno        = factor(sample(c("Manhã", "Tarde", "Noite"), n, replace = TRUE)),
      Status       = factor(sample(c("Aprovado", "Retrabalho"), n, replace = TRUE, prob = c(0.92, 0.08)))
    )
  } else {
    set.seed(789)
    n <- 100
    data.frame(
      Cliente_ID   = seq_len(n),
      Faturamento  = round(rnorm(n, 1500, 450), 2),
      Satisfacao   = round(pmin(10, pmax(1, rnorm(n, 7.8, 1.5))), 1),
      Visitas_Mes  = rpois(n, 4),
      Regiao       = factor(sample(c("Sul", "Sudeste", "Centro-Oeste", "Nordeste"), n, replace = TRUE)),
      Plano        = factor(sample(c("Básico", "Pro", "Premium"), n, replace = TRUE, prob = c(0.5, 0.35, 0.15)))
    )
  }
}
