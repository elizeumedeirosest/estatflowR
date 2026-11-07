<div align="center">

<p align="center">
  <img src="man/figures/estatflowR_title.svg" alt="estatflowR" width="370" />
</p>

### Interface Visual e Exploração Estatística Interativa em R

[![Documentação Oficial](https://img.shields.io/badge/docs-oficial-0071e3?style=for-the-badge&logo=google-chrome&logoColor=white)](https://elizeumedeirosest.github.io/estatflowR) 
[![Licença MIT](https://img.shields.io/badge/licença-MIT-10b981?style=for-the-badge)](LICENSE) 
[![R >= 4.1](https://img.shields.io/badge/R-%3E%3D_4.1-38bdf8?style=for-the-badge&logo=R&logoColor=white)](https://cran.r-project.org/) 
[![UI Interativa](https://img.shields.io/badge/interface-Shiny%20%7C%20bslib%20%7C%20Plotly-0284c7?style=for-the-badge)](https://elizeumedeirosest.github.io/estatflowR) 
[![Código Reprodutível](https://img.shields.io/badge/código-ggplot2%20%7C%20stats-818cf8?style=for-the-badge)](https://elizeumedeirosest.github.io/estatflowR)

<p align="center">
  <b>Uma estação moderna e minimalista para diagnóstico, visualização e exploração de dados em R.</b><br>
  Design inspirado na filosofia Apple, gráficos com grade dupla nativa e interatividade suave.
</p>

</div>

---

## O que é o estatflowR?

O **estatflowR** fornece uma experiência completa de **Análise Exploratória de Dados (EDA)** através de uma interface gráfica única e intuitiva:

1. **Diagnóstico Estrutural Instantâneo:** Identificação automática de tipos (contínua, discreta, fator, data), contagem de valores ausentes (NAs) e resumo descritivo.
2. **Exploração Univariada:** Histogramas com densidade, boxplots com dispersão de pontos reais e teste de normalidade de Shapiro-Wilk calculado em tempo real.
3. **Relações Bivariadas & Grupos:** Diagramas de dispersão com tendência e correlação, além de comparações entre grupos com testes estatísticos automáticos (Welch, ANOVA, Qui-quadrado).
4. **Matriz Exploratória Multivariada:** Painel com densidades na diagonal, correlações no triângulo superior e dispersões no inferior, com opção de agrupamento.
5. **Código R Reprodutível:** Modal para visualização e cópia imediata do script R base e ggplot2 correspondente à análise em exibição.

---

## Instalação

```r
# Instalação via pak (recomendado):
# install.packages("pak")
pak::pak("elizeumedeirosest/estatflowR")

# Ou via remotes / devtools:
# install.packages("remotes")
remotes::install_github("elizeumedeirosest/estatflowR")
```

---

## Como Usar

### 1. Iniciar a Interface

```r
library(estatflowR)

# Iniciar interface para carregar arquivo (.csv, .xlsx, .rds, .parquet):
explorar()

# Ou explorar um data frame diretamente:
explorar(mtcars)

# Compatível com o pipe nativo do R:
iris |> explorar()
```

### 2. Modos de Visualização

```r
# Janela flutuante nativa (padrão se no RStudio):
explorar(mtcars, viewer = "window")

# No navegador padrão (Chrome, Edge, Safari):
explorar(mtcars, viewer = "browser")

# No painel Viewer do RStudio:
explorar(mtcars, viewer = "pane")
```

---

## Estrutura da Interface

| Aba | Foco da Análise | Principais Recursos |
|---|---|---|
| **Visão Geral** | Diagnóstico Estrutural | KPIs de integridade, detecção de tipos, diagnóstico de NAs e resumo numérico |
| **Variável Isolada** | Univariada | Histograma + KDE, Boxplot com jitter, QQ-Plot e teste de Shapiro-Wilk |
| **Relações & Grupos** | Bivariada | Dispersão + tendência, correlação de Pearson/Spearman, comparações por grupo |
| **Matriz Exploratória** | Multivariada | Pairs plot interativo com densidades, dispersões e correlações ponderadas |

---

## Licença

Distribuído sob a licença **MIT**. Consulte o arquivo [LICENSE](LICENSE) para obter mais informações.

Desenvolvido por **[Elizeu Medeiros](https://github.com/elizeumedeirosest)** (2026).
