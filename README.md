# estatflowR <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-success.svg?style=flat-square)](https://github.com/elizeumedeirosest/estatflowR/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-maturing-0071E3.svg?style=flat-square)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![CRAN status](https://www.r-pkg.org/badges/version/estatflowR?style=flat-square)](https://CRAN.R-project.org/package=estatflowR)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg?style=flat-square)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

> **A interface gráfica de análise exploratória e diagnóstico estatístico inspirada na filosofia de design da Apple.**  
> *Fluida. Intuitiva. Minimalista. Interativa.*

---

## ✦ Visão Geral

O **`estatflowR`** nasce para preencher uma lacuna fundamental no ecossistema R: proporcionar uma experiência de **análise exploratória de dados (EDA)** moderna, descomplicada e visualmente impecável. 

Combinando a robustez do `bslib`, a elegância editorial do `ggplot2` com **grade dupla nativa** e o poder interativo do `plotly`, o pacote permite inspecionar variáveis, detectar problemas estruturais e realizar testes inferenciais de hipótese através de uma única chamada de função:

```r
library(estatflowR)

# Abre o explorador com upload ou dados de exemplo:
explorar()

# Ou passe qualquer data frame diretamente:
explorar(mtcars)

# Compatível com o pipe nativo do R:
iris |> explorar()
```

---

##  Filosofia de Design: *Human Interface Guidelines*

Construído sob os princípios estéticos da Apple, o **`estatflowR`** se distancia das interfaces científicas poluídas ou datadas:

- **Espaçamento e Respiro:** Cards com cantos arredondados (`border-radius: 16px`), sombras dinâmicas suaves e tipografia moderna de sistema (`Inter`, `SF Pro Display`).
- **Paleta Editorial Sóbria:** Azul vibrante Apple (`#0071E3`), verde de confirmação (`#34C759`), cinza neutro e fundos translúcidos com *backdrop-filter blur*.
- **Grade Dupla Embutida por Padrão:** Todos os gráficos são gerados automaticamente com uma grade cartesiana dupla suave (linhas principais a 8% de opacidade e secundárias pontilhadas), proporcionando leitura de valores instantânea sem poluição visual.
- **Interatividade Fina:** Gráficos com tooltips em português do Brasil e formatação decimal com vírgula (`12,34`), permitindo *zoom*, *pan* e *hover* sem esforço.
- **Transparência e Reproduzibilidade:** A qualquer momento, clique no botão **"Ver Código R"** para visualizar e copiar o código exato em R base e `ggplot2` necessário para reproduzir o gráfico na sua sessão de análise.

---

## 🚀 Instalação

Você pode instalar a versão de desenvolvimento do GitHub:

```r
# Instalação via pak (recomendado):
# install.packages("pak")
pak::pak("elizeumedeirosest/estatflowR")

# Ou via remotes / devtools:
# install.packages("remotes")
remotes::install_github("elizeumedeirosest/estatflowR")
```

---

## 🧭 As 4 Dimensões de Exploração

A interface organiza a investigação estatística em quatro espaços de trabalho dedicados:

```
┌────────────────────────────────────────────────────────────────────────┐
│ estatflowR                                                             │
├──────────────┬──────────────────┬───────────────────┬──────────────────┤
│ 1. VISÃO     │ 2. VARIÁVEL      │ 3. RELAÇÕES &     │ 4. MATRIZ        │
│    GERAL     │    ISOLADA       │    GRUPOS         │    EXPLORATÓRIA  │
└──────────────┴──────────────────┴───────────────────┴──────────────────┘
```

### 1. Visão Geral & Diagnóstico Estrutural
- **KPI Cards no Topo:** Total de observações, número de variáveis, total e percentual de valores ausentes (NAs) e memória ocupada.
- **Tabela de Diagnóstico:** Identificação automática de tipos (Numérica Contínua, Numérica Discreta, Categórica / Fator, Data / Hora).
- **Status de Qualidade:** Diagnóstico instantâneo por coluna com badges semânticos de integridade (`100% Completo`, `Atenção (< 10%)`, `Crítico`).
- **Resumo Paramétrico:** Média, Desvio Padrão e Amplitude para números; contagem de níveis e moda para fatores.

### 2. Variável Isolada (Univariada)
- **Variáveis Numéricas:**
  - Histograma com Curva de Densidade KDE sobreposta.
  - Boxplot com pontos de dados reais (*jitter*) e destaque visual para *outliers*.
  - Gráfico QQ-Plot (*Quantile-Quantile Normal*) com envelope de referência.
  - Painel com resumo dos 5 números de Tukey e **Teste de Normalidade de Shapiro-Wilk** calculado em tempo real com interpretação do p-valor.
- **Variáveis Categóricas / Fatores:**
  - Gráfico de barras horizontais ordenadas por frequência com porcentagens calculadas.
  - Tabela com contagens absolutas e relativas acumuladas.

### 3. Relações & Grupos (Bivariada)
- **Numérica vs. Numérica:** Gráfico de dispersão (*scatter plot*) com linha de tendência linear / LOESS, cálculo do coeficiente de correlação de Pearson/Spearman e intervalo de confiança a 95%.
- **Numérica vs. Categórica:** Boxplots comparativos entre grupos com pontos transparentes, acompanhados de **Teste t de Welch** (2 níveis) ou **ANOVA / Kruskal-Wallis** (> 2 níveis) com indicação automática da significância estatística.
- **Categórica vs. Categórica:** Gráfico de barras agrupadas / empilhadas proporcionais e **Teste Qui-quadrado de Independência de Pearson**.

### 4. Matriz Exploratória Multivariada
- Inspirada na arquitetura `grafico_exploratorio()`:
  - **Diagonal:** Curvas de densidade univariadas com preenchimento translúcido.
  - **Triângulo Inferior:** Diagramas de dispersão com suavização linear.
  - **Triângulo Superior:** Coeficientes de correlação de Pearson com tamanho proporcional e coloração dinâmica.
  - Suporte a segmentação por variável de agrupamento (fator).

---

## 💡 Modos de Exibição (Viewer)

O `estatflowR` se adapta perfeitamente ao seu fluxo de trabalho:

```r
# Janela nativa dedicada no RStudio (padrão) ou navegador
explorar(mtcars, viewer = "window")

# Abrir no navegador padrão (Chrome, Safari, Edge, Firefox)
explorar(mtcars, viewer = "browser")

# Abrir no painel Viewer lateral do RStudio
explorar(mtcars, viewer = "pane")

# Janela modal redimensionável
explorar(mtcars, viewer = "dialog", largura = 1400, altura = 900)
```

Também está disponível diretamente através do menu **Addins** do RStudio:
> **RStudio Menu** -> **Addins** -> **ESTATFLOWR: Explorar Dados Interativamente**. Se você tiver selecionado o nome de um `data.frame` no script, ele será carregado automaticamente!

---

## 📦 Formatos Suportados na Interface

Caso abra a interface sem argumentos (`explorar()`), você pode carregar dados via arrastar e soltar ou explorador de arquivos:

| Formato | Extensões | Mecanismo |
|---|---|---|
| Arquivo de Texto | `.csv`, `.tsv`, `.txt` | Detecção automática de delimitador e encoding |
| Planilha Excel | `.xlsx`, `.xls` | Leitura multi-aba via `readxl` |
| R Serializado | `.rds`, `.RData`, `.rda` | Leitura nativa instantânea |
| Apache Parquet | `.parquet` | Leitura de alto desempenho via `arrow` |
| SPSS / Stata / SAS | `.sav`, `.dta`, `.sas7bdat` | Suporte via `haven` |

Ou escolha entre conjuntos de dados didáticos integrados (`mtcars`, `iris`, `diamonds`, `airquality`, `palmerpenguins`).

---

## 🛠️ Stack Tecnológica

O `estatflowR` foi projetado com dependências enxutas e de ponta:

- **[bslib](https://rstudio.github.io/bslib/):** Componentes Bootstrap 5 e CSS Moderno (CSS Custom Properties).
- **[ggplot2](https://ggplot2.tidyverse.org/):** Motor gráfico com tema editorial de grade dupla.
- **[plotly](https://plotly.com/r/):** Motor de renderização interativa WebGL/SVG.
- **[DT](https://rstudio.github.io/DT/):** Tabelas dinâmicas com paginação e busca instantânea.
- **[patchwork](https://patchwork.data-imaginist.com/):** Composição de matrizes multivariadas.

---

## 📄 Licença

Distribuído sob a licença **MIT**. Consulte o arquivo [LICENSE](LICENSE) para obter mais informações.

Desenvolvido por **[Elizeu Medeiros](https://github.com/elizeumedeirosest)** (2026).
