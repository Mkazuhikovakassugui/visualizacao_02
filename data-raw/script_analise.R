#---------------------------------------------------------------------------------------------#
#                                        Análise da base ENEM
#                                    Curso- R - Visualização II
#                                     Professora: Beatriz Milz
#                                     Aluno: Marcio Vakassugui
#---------------------------------------------------------------------------------------------#

# PROCEDIMENTOS NORMAIS -----------------------------------------------------------------------
## Carregar da base ----------------------------------------------------------------------------
enem <- readr::read_csv("data/enem_analise_final.csv", show_col_types = FALSE)

## Carregar as funções -------------------------------------------------------------------------
source("R/funcao_tema_graficos.R")

# DADOS GERAIS DO RELATÓRIO -------------------------------------------------------------------
## Filtrar a base com os dados do município e o ano da prova ----------------------------------
### obs. para o arquivo .RMD, o município será informado como parâmetro (params no yaml)
enem_cidade <- enem |>
  dplyr::filter(nu_ano == 2020, no_municipio_prova == "Curitiba")

## Encontrar o total de alunos no município considerado ---------------------------------------
total_alunos_municipio <- enem_cidade |>
  dplyr::summarise(total_alunos = dplyr::n())


## Criar o mapa com o município em análise ----------------------------------------------------
library(geobr)
library(sf)

dados_geobr <- geobr::read_municipality("PR") # carregar os dados para o estado do PR

### Mapa01 - Mapa do município em análise -----------------------------------------------------
map1 <- dados_geobr |>
  dplyr::filter(name_muni == "Curitiba") |> # filtrar os dados para o município em análise
  ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = dados_geobr,
    fill = "#70A288",
    color = "#000000",
    size = .1,
    alpha = 0.7
  ) +
  ggplot2::geom_sf(ggplot2::aes(fill = name_muni)) +
  ggspatial::annotation_north_arrow(location = "tr") +
  ggplot2::theme_classic() +
  theme_enem() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_rect(
      fill = "#EBF9FF",
      colour = "#EBF9FF"
    )
  )

map1

# ANÁLISE DAS NOTAS DAS REDAÇÕES---------------------------------------------------------------
# Construir a visualização composta por dois gŕaficos por meio da biblioteca patchwork
# Boxplot para observarmos a média, notas mínimas e notas máximas
# Histogram para observarmos a distribuição das notas

## Distribuição das notas de redação ----------------------------------------------------------
### gráfico_01_1 - boxplot das notas -----------------------------------------------------------
### gráfico do boxplot das notas das redações (parte superior da visualização)
p1_1 <- enem_cidade |>
  dplyr::select(nu_nota_redacao) |>
  ggplot2::ggplot(ggplot2::aes(x = nu_nota_redacao)) +
  ggplot2::geom_boxplot(
    fill = "#11B5E4",
    alpha = 0.7
  ) +
  ggplot2::labs(
    title = "Distribuição das notas de redação ",
    x = "",
    y = ""
  ) +
  ggplot2::theme_classic() +
  theme_enem() +
  ggplot2::theme(
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank()
  )

p1_1

### gráfico_01_2 - Histograma das notas --------------------------------------------------------
### gráfico do histograma das notas das redações (parte inferior da visualização)
p1_2 <- enem_cidade |>
  dplyr::select(nu_nota_redacao) |>
  ggplot2::ggplot(ggplot2::aes(x = nu_nota_redacao)) +
  ggplot2::geom_histogram(
    fill = "#11B5E4",
    color = "#000000",
    alpha = 0.7
  ) +
  ggplot2::labs(
    x = "Notas",
    y = ""
  ) +
  ggplot2::theme_classic() +
  theme_enem()

p1_2

## gráfico_01
library(patchwork)

p1 <- p1_1/p1_2
p1

## Análise das 5 competências da redação do enem ----------------------------------------------
## A nota da redação do ENEM é composta por 5 competências. Cada competência pode receceber
## notas de 0 a 200 pontos. A soma das competências resulta na nota da Nota da Redação.

## ANÁLISE DAS COMPETÊNCIAS
## São as competências que compõem as notas da redação do ENEM
    # 1) Demonstrar domínio da modalidade escrita formal da Língua Portuguesa;;
    # 2) Compreender a proposta de redação e aplicar conceitos das várias áreas de conhecimento
    #    para desenvolver o tema, dentro dos limites estruturais do texto dissertativo-
    #    argumentativo em prosa;
    # 3) Selecionar, relacionar, organizar e interpretar informações, fatos, opiniões e
    #    argumentos em defesa de um ponto de vista;
    # 4) Demonstrar conhecimento dos mecanismos linguísticos necessários para a construção da
    #    argumentação;
    # 5) Elaborar proposta de intervenção para o problema abordado, respeitando os direitos
    #    humanos.


### dados estatísticos das notas ---------------------------------------------------------------

nota_max <- enem_cidade |> # nota máxima obtida
  dplyr::summarize(nota_max = max(nu_nota_redacao))

nota_max

nota_min <- enem_cidade |> # nota mínima obtida
  dplyr::summarise(nota_min = min(nu_nota_redacao))

nota_min

nota_media <- enem_cidade |> # nota média
  dplyr::summarise(nota_media = mean(nu_nota_redacao, na.rm = TRUE))

nota_media

zeros_redacao <- enem |> # quantidade de notas zero
  dplyr::filter(no_municipio_prova == "Curitiba" & nu_nota_redacao == 0 & nu_ano == 2020) |>
  dplyr:: summarise(qtde_zeros = dplyr::n())

zeros_redacao

perc_zeros_redacao <- enem_cidade |> # percentual de notas zero
  dplyr::summarise(perc_zeros_redacao = round((zeros_redacao / total_alunos_municipio)*100,3))

perc_zeros_redacao

### Distribuição das notas por intervalos de classes -------------------------------------------
### gráfico 02 - distribuição de notas por intervalos-------------------------------------------
### Diagrama de colunas deve mostrar a distribuição das notas de redação por intervalos de
### classes de notas, o amplitude de classe escolhido foi = 100

### base do gráfico 02

fabs_notas_redacao <- enem_cidade |>
  dplyr::select(nu_nota_redacao) |>
  table()

fabs_notas_redacao

### total de alunos com nota máxima
qtd_nota_max <- fabs_notas_redacao |>
  tail(n=1)

qtd_nota_max

### criar as classes de frequências das notas

intervalo_classes_notas <- seq(0, 1000, 100)
tab_classes_notas <- table(cut(
  enem_cidade$nu_nota_redacao,
  breaks = intervalo_classes_notas,
  right = FALSE
)) |>
  prop.table() |>
  data.frame() |>
  dplyr::mutate(Freq = Freq * 100)

## gráfico_02

p2 <- tab_classes_notas |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = Var1,
    y = Freq,
    fill = Var1,
    label = round(Freq, 3)
  ) +
  ggplot2::geom_col() +
  ggplot2::geom_label( # labels
    size = 4,
    color = "#576BC7",
    alpha = 0,
    nudge_y = 1
  ) +
  ggplot2::labs( # títulos dos eixos
    title = "Frequências relativas de notas por intervalos de classe",
    x = "Intervalo de notas",
    y = "Frequência (%)"
  ) +
  ggplot2::theme_classic() +
  theme_enem() +
  ggplot2::scale_color_viridis_d(option = "E") +
  ggplot2::theme(legend.position = "none")

p2

## Razões para o zero nas redações ------------------------------------------------------------
## O gráfico de apresentar os motivos que levaram à nota zero das redações do município
## considerado. O gráfico deve ter a formatação que atenda todos os municípios avaliados

### Principais razões para nota zero na redação -----------------------------------------------
#### gráfico 03 - Razões para nota zero --------------------------------------------------------
## base para o gráfico 03
redacoes_com_problemas <- enem_cidade |>
  dplyr::filter(nu_nota_redacao == 0) |>
  dplyr::group_by(tp_status_redacao) |>
  dplyr::summarise(qte = dplyr::n()) |>
  dplyr::arrange(desc(qte)) |>
  dplyr::mutate(tp_status_redacao = as.factor(tp_status_redacao))

## gráfico_03
p3 <- redacoes_com_problemas |>
  ggplot2::ggplot(ggplot2::aes(
    x = qte,
    y = tp_status_redacao,
    fill = tp_status_redacao,
    label = qte
  )) +
  ggplot2::geom_col(
    color = "#000000",
    alpha = 0.7
  ) +
  ggplot2::geom_label( # labels
    size = 5,
    color = "#000000",
    alpha = 0,
    hjust = -1
  ) +
  ggplot2::labs( # títulos dos eixos
    title = "Principais razões para eliminação das redações",
    x = "Quantidade de casos",
    y = "Razões"
  ) +
  ggplot2::scale_x_continuous(limits = c(0, max(redacoes_com_problemas$qte) + 6)) +
  ggplot2::scale_color_viridis_d(option = "E") +
  ggplot2::theme(legend.position = "none") +
  theme_enem()

p3

## Média das competências da redação ----------------------------------------------------------
### gráfico 04 - Média das notas que compõem a redação -----------------------------------------

### Cálculo das médias dos componentes da redação
### Cálculo das médias dos componentes da redação
medias_comp_redacao <- enem_cidade |>
  dplyr::summarise(dplyr::across(
    .cols = dplyr::starts_with("nu_nota_comp"),
    .fns = mean, na.rm = TRUE
  ))

medias_comp_redacao <- medias_comp_redacao |>                           # renomear as variáveis
  dplyr::rename(
    "comp1" = "nu_nota_comp1",
    "comp2" = "nu_nota_comp2",
    "comp3" = "nu_nota_comp3",
    "comp4" = "nu_nota_comp4",
    "comp5" = "nu_nota_comp5"
  )


### Pivotar a base para long
medias_comp_redacao <- medias_comp_redacao |>
  tidyr::pivot_longer(
    cols = c(
      "comp1",
      "comp2",
      "comp3",
      "comp4",
      "comp5"),
    names_to = "notas_comp_redacao",
    values_to = "medias") |>
  dplyr::mutate(notas_comp_redacao = as.factor(notas_comp_redacao))


# Gráfico 04
p4 <- medias_comp_redacao |> # base
  dplyr::mutate(
    notas_comp_redacao = forcats::fct_reorder(
      notas_comp_redacao,
      medias,
      .desc = TRUE
    )
  ) |>
  ggplot2::ggplot() + # gráfico
  ggplot2::aes(
    x = notas_comp_redacao,
    y = medias,
    fill = notas_comp_redacao,
    label = round(medias, 2)
  ) +
  ggplot2::geom_col(
    color = "#000000",
    alpha = 0.7
  ) +
  ggplot2::geom_label( # labels
    size = 5,
    color = "#000000",
    alpha = 0,
    nudge_y = 7
  ) +
  ggplot2::labs( # títulos dos eixos
    title = "Médias dos componentes da redação",
    subtitle = "A nota da redação é definida pela soma dos componentes",
    x = "Notas dos componentes",
    y = "Média das notas"
  ) + # tema
  ggplot2::theme(
    legend.position = "none"
  ) +
  theme_enem()+
  ggplot2::scale_fill_viridis_d(option = "E")

p4

## Distribuição das notas por competência -----------------------------------------------------
# Construir boxplot para cada competência

# pivotar a base
comp_redacao <- enem_cidade |>
  dplyr::select(nu_nota_comp1,
                nu_nota_comp2,
                nu_nota_comp3,
                nu_nota_comp4,
                nu_nota_comp5) |>
  dplyr::rename("comp1" = "nu_nota_comp1",
                "comp2" = "nu_nota_comp2",
                "comp3" = "nu_nota_comp3",
                "comp4" = "nu_nota_comp4",
                "comp5" = "nu_nota_comp5",)

comp_redacao_long <- comp_redacao |>
  tidyr::pivot_longer(
    cols = c(
      "comp1",
      "comp2",
      "comp3",
      "comp4",
      "comp5"
    ),
    names_to = "competencias",
    values_to = "notas_comp"
  ) |>
  dplyr::mutate(competencias = as.factor(competencias))

### gráfico_05 - Densidade de distribuiçao das notas das competencias --------------------------
p5 <- comp_redacao_long |>
  ggplot2::ggplot(ggplot2::aes(competencias, notas_comp, fill = competencias))+
  ggplot2::geom_violin(width = 0.6)+
  ggplot2::geom_boxplot(width = 0.1,
                        color = "black",
                        alpha = 0.2)+
  #ggplot2::geom_jitter(color="black", size=0.4, alpha=0.02) +
  ggplot2::labs(
    title = "Densidade de distribuição das notas das competências de redação",
    x = "Competências",
    y = "Notas"
  )+
  theme_enem()+
  ggplot2::theme(
    legend.position = "none"
  )

p5

## Evolução das notas de redação nos últimos anos ---------------------------------------------
## Construir gráficos que demonstrem a evolução das notas das redações nos últimos anos
## Como as melhores notas evoluíram
## Como as notas zero evoluíram
## Como a média das notas evoluíram

## filtrar a base para a cidade
enem_redacao_historico <- enem |>
  dplyr::filter(no_municipio_prova == "Curitiba") |>
  dplyr::select(nu_nota_redacao, nu_ano) |>
  dplyr::group_by(nu_ano) |>
  dplyr::summarise(media = mean(nu_nota_redacao, na.rm = TRUE))

## alterar tipo de ano para inteiro
enem_redacao_historico[["nu_ano"]] <- as.integer(enem_redacao_historico[["nu_ano"]])

enem_redacao_historico_notas900 <- enem |>
  dplyr::filter(no_municipio_prova == "Curitiba" & nu_nota_redacao >=900) |>
  dplyr::select(nu_nota_redacao, nu_ano) |>
  dplyr::group_by(nu_ano) |>
  dplyr::summarise(qtde = dplyr::n())

enem_notas_zero <- enem |>
  dplyr::filter(no_municipio_prova == "Curitiba" & nu_nota_redacao == 0) |>
  dplyr::select(nu_nota_redacao, nu_ano) |>
  dplyr::group_by(nu_ano) |>
  dplyr::summarise(qtde = dplyr::n())

### gráfico_06_1 - Média das notas -------------------------------------------------------------
p6_1 <- enem_redacao_historico |>
  ggplot2::ggplot()+
  ggplot2::aes(x = nu_ano,
               y = media,
               label = round(media,2))+
  ggplot2::geom_line(
    color = "grey"
  )+
  ggplot2::geom_point(
    shape = 21,
    color = "black",
    fill = "#70A288",
    size = 8
  )+
  ggplot2::geom_text(
    vjust = -2,
    size = 5
  )+
  theme_enem()+
  ggplot2::labs(
    title = "Médias das notas",
    x = "Ano",
    y = "Média"
  ) +
  ggplot2::scale_y_continuous(limits = c(min(enem_redacao_historico$media)-50,
                                         max(enem_redacao_historico$media)+50))

### gráfico_06_2 - Notas acima de 900 pontos --------------------------------------------------
p6_2 <- enem_redacao_historico_notas900 |>
  ggplot2::ggplot()+
  ggplot2::aes(x = nu_ano,
               y = qtde,
               label = qtde)+
  ggplot2::geom_line(
    color = "grey"
  )+
  ggplot2::geom_point(
    shape = 21,
    color = "black",
    fill = "#70A288",
    size = 6
  )+
  ggplot2::geom_text(
    hjust = -1,
    size = 5
  )+
  theme_enem()+
  ggplot2::labs(
    title = "Notas acima de 900",
    x = "Ano",
    y = "Quantidade"
  )+
  ggplot2::scale_x_continuous(limits = c(min(enem_redacao_historico_notas900$nu_ano),
                                         max(enem_redacao_historico_notas900$nu_ano)+0.8))

### gráfico_06_3 - gráfico de notas zero -------------------------------------------------------
p6_3 <- enem_notas_zero |>
  ggplot2::ggplot()+
  ggplot2::aes(x = nu_ano,
               y = qtde,
               label = qtde)+
  ggplot2::geom_line(
    color = "grey"
  )+
  ggplot2::geom_point(
    shape = 21,
    color = "black",
    fill = "#70A288",
    size = 6
  )+
  ggplot2::geom_text(
    hjust = -1,
    size = 5
  )+
  theme_enem()+
  ggplot2::labs(
    title = "Notas zero",
    x = "Ano",
    y = "Quantidade"
  )+
  ggplot2::scale_x_continuous(limits = c(min(enem_redacao_historico_notas900$nu_ano),
                                         max(enem_redacao_historico_notas900$nu_ano)+0.8))

library(patchwork)

p6 <-  (p6_3 + p6_2) / p6_1

p6

# ANÁLISE DAS NOTAS DE CIÊNCIAS DA NATUREZA ---------------------------------------------------
# Construir a visualização composta por dois gŕaficos por meio da biblioteca patchwork
# Boxplot para observarmos principalmente a média, notas mínimas e notas máximas
# Histogram para observarmos a distribuição das notas das redações

## Distribuição das notas de CN ---------------------------------------------------------------

### gráfico_07_1 - boxplot das notas ----------------------------------------------------------
p7_1 <- enem_cidade |>
  dplyr::select(nu_nota_cn) |>
  ggplot2::ggplot(ggplot2::aes(x = nu_nota_cn))+
  ggplot2::geom_boxplot(
    fill = "#11B5E4",
    alpha = 0.7)+
  ggplot2::labs(
    title = "Distribuição das notas de redação ",
    x = "",
    y = ""
  )+
  ggplot2::theme_classic()+
  theme_enem()+
  ggplot2::theme(
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank()
  )

### gráfico_07_2 - Histograma das notas -------------------------------------------------------
p7_2 <- enem_cidade |>
  dplyr::select(nu_nota_cn) |>
  ggplot2::ggplot(ggplot2::aes(x = nu_nota_cn))+
  ggplot2::geom_histogram(fill = "#11B5E4",
                          color = "#000000",
                          alpha = 0.7)+
  ggplot2::labs(
    x = "Notas",
    y = ""
  )+
  ggplot2::theme_classic()+
  theme_enem()

library(patchwork)                            # uso do pacote patchwork para união dos gráficos

# disposição dos gráficos
p7 <- p7_1/p7_2
p7


# qual a nota máxima na cidade analisada para Ciências Naturais
nota_max_cn <- enem_cidade |>
  dplyr::summarize(nota_max = max(nu_nota_cn))

# qual a nota mínima na cidade analisada para Ciências Naturais
nota_min_cn <- enem_cidade |>
  dplyr::summarise(nota_min = min(nu_nota_cn))

# qual a nota média de Ciências Naturais
nota_media_cn <- enem_cidade |>
  dplyr::summarise(nota_media_cn = mean(nu_nota_cn, na.rm = TRUE))

# qual a distribuição de frequência das notas de Ciências Naturais
fabs_notas_cn <- enem_cidade |>
  dplyr::select(nu_nota_cn) |>
  table()

# qual o total de alunos com nota máxima
qtd_nota_max_cn <- fabs_notas_cn |>
  tail(n = 1)

# criar as classes de frequências das notas
intervalo_classes_notas_cn <- seq(0, 1000, 100)

# tabela de classes
tab_classes_notas_cn <- table(
  cut(enem_cidade$nu_nota_cn,
      breaks = intervalo_classes_notas_cn,
      right = FALSE
  )) |>
  prop.table() |>
  data.frame() |>
  dplyr::mutate(Freq = Freq * 100)

# qual as duas classes com as maiores notas
maiores_classes_notas_cn <- tab_classes_notas_cn |>
  dplyr::arrange(desc(Freq)) |>
  dplyr::slice_max(order_by = Freq, n=2) |>
  tibble::tibble()

# qual as duas classes com a menor nota
menores_classes_notas_cn <- tab_classes_notas_cn |>
  dplyr::arrange(desc(Var1)) |>
  dplyr::filter(Freq != 0)|>
  dplyr::slice_tail() |>
  tibble::tibble()

### gráfico de frequência relativa por intervalo de notas --------------------------------------
p8 <- tab_classes_notas_cn |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = Var1,
    y = Freq,
    fill = Var1,
    label = round(Freq, 3)
  ) +
  ggplot2::geom_col(
    color = "#000000",
    alpha = 0.7
  ) +
  ggplot2::geom_label( # labels
    size = 5,
    color = "#000000",
    alpha = 0,
    nudge_y = 1.5
  ) +
  ggplot2::labs( # títulos dos eixos
    title = "Frequência relativa por intervalo de classes",
    x = "Intervalo de notas",
    y = "Frequência (%)"
  ) +
  ggplot2::scale_fill_viridis_d(option = "E") +
  ggplot2::theme(legend.position = "none") +
  theme_enem()

p8

### Gráfico notas por tipo de escolas ---------------------------------------------------------
# escola pública
notas_tipo_escola <- enem_cidade |>
  dplyr::select(tp_dependencia_adm_esc, nu_nota_cn) |>
  tidyr::drop_na() |>
  dplyr::group_by(tp_dependencia_adm_esc) |>
  dplyr::summarise(medias = mean(nu_nota_cn, na.rm = TRUE)) |>
  dplyr::arrange(desc(medias))

p9 <- notas_tipo_escola |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = forcats::fct_reorder(
      tp_dependencia_adm_esc,
      medias,
      .desc = TRUE
    ),
    y = medias,
    fill = tp_dependencia_adm_esc,
    label = round(medias, 2)
  ) +
  ggplot2::geom_col() +
  ggplot2::geom_label(
    size = 5,
    color = "#000000",
    alpha = 0,
    vjust = -0.5
  ) +
  ggplot2::labs(
    title = "Média das escolas por dependência administrativa",
    x = "Dependência administrativa",
    y = "Média"
  ) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_y_continuous(
    limits = c(0, 700),
    breaks = seq(0, 700, 100))+
  theme_enem()

p9



p9_1 <- enem_redacao_historico |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = nu_ano,
    y = media,
    label = round(media, 2)
  ) +
  ggplot2::geom_line(color = "grey") +
  ggplot2::geom_point(
    shape = 21,
    color = "black",
    fill = "#70A288",
    size = 8
  ) +
  ggplot2::geom_text(
    vjust = -2,
    size = 5
  ) +
  theme_enem() +
  ggplot2::labs(
    title = "Médias das notas",
    x = "Ano",
    y = "Média"
  ) +
  ggplot2::scale_y_continuous(limits = c(
    min(enem_redacao_historico$media) - 50,
    max(enem_redacao_historico$media) + 50
  ))

## Evolução das notas de redação nos últimos anos ---------------------------------------------

p10_2 <- enem_redacao_historico_notas900 |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = nu_ano,
    y = qtde,
    label = qtde
  ) +
  ggplot2::geom_line(color = "grey") +
  ggplot2::geom_point(
    shape = 21,
    color = "black",
    fill = "#70A288",
    size = 6
  ) +
  ggplot2::geom_text(
    hjust = -1,
    size = 5
  ) +
  theme_enem() +
  ggplot2::labs(
    title = "Notas acima de 900",
    x = "Ano",
    y = "Quantidade"
  ) +
  ggplot2::scale_x_continuous(limits = c(
    min(enem_redacao_historico_notas900$nu_ano),
    max(enem_redacao_historico_notas900$nu_ano) + 0.8
  ))

p6_3 <- enem_notas_zero |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = nu_ano,
    y = qtde,
    label = qtde
  ) +
  ggplot2::geom_line(
    color = "grey"
  ) +
  ggplot2::geom_point(
    shape = 21,
    color = "black",
    fill = "#70A288",
    size = 6
  ) +
  ggplot2::geom_text(
    hjust = -1,
    size = 5
  ) +
  theme_enem() +
  ggplot2::labs(
    title = "Notas zero",
    x = "Ano",
    y = "Quantidade"
  ) +
  ggplot2::scale_x_continuous(limits = c(
    min(enem_redacao_historico_notas900$nu_ano),
    max(enem_redacao_historico_notas900$nu_ano) + 0.8
  ))

library(patchwork)

p6 <-  (p6_3 + p6_2) / p6_1

p6

## Lingua estrangeira escolhida

p_7 <- enem_cidade |>
  dplyr::select(tp_lingua) |>
  table() |>
  as.data.frame() |>
  dplyr::mutate(tp_lingua = as.factor(tp_lingua))

# calcular as percentagens
p_7$fraction <- p_7$Freq / sum(p_7$Freq)

# calcular as percentagens acumuladas (topo de cada retângulo)
p_7$ymax <-  cumsum(p_7$fraction)

# calcular os valores mínimos de cada retângulo
p_7$ymin <- c(0, head(p_7$ymax, n = -1))

# calcular a posição do label
p_7$label_position <- (p_7$ymax + p_7$ymin)/2

# calcular o label
p_7$label <- paste0(p_7$tp_lingua, "\n value: ", p_7$Freq)



p_7 |>
  ggplot2::ggplot(ggplot2::aes(ymax = ymax,
                      ymin = ymin,
                      xmax = 4,
                      xmin = 3,
                      fill = tp_lingua))+
  ggplot2::geom_rect()+
  ggplot2::geom_text(x = 2, ggplot2::aes(y = label_position, label = label), size = 5)+
  ggplot2::scale_fill_brewer(palette = 1)+
  ggplot2::coord_polar(theta = 'y')+
  ggplot2::xlim(c(-1,4))+
  ggplot2::theme_void()+
  ggplot2::theme(
    legend.position = "none",
    panel.background = ggplot2::element_rect(fill = "#EBF9FF"),
    plot.background = ggplot2::element_rect(fill = "#FFFFFF"))
p_7


