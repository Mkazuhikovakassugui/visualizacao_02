#---------------------------------------------------------------------------------------------#
#                                        Análise da base ENEM
#                                    Curso- R - Visualização II
#                                     Professora: Beatriz Milz
#                                     Aluno: Marcio Vakassugui
#---------------------------------------------------------------------------------------------#

# CARREGAMENTO DAS BASE E FUNÇÕES -------------------------------------------------------------
## Carregar da base arrumada -------------------------------------------------------------------

enem <- readr::read_csv("data/enem_analise_final.csv", show_col_types = FALSE)

## Carregamento das funções--------------------------------------------------------------------
source("R/funcao_tema_graficos.R")

## Filtrar a base com os dados do município e o ano da prova-----------------------------------
enem_cidade <- enem |>
  dplyr::filter(nu_ano == 2020, no_municipio_prova == "Curitiba")

# ANÁLISE DAS NOTAS DAS REDAÇÕES---------------------------------------------------------------
# Construir a visualização composta por dois gŕaficos por meio da biblioteca patchwork
# Boxplot para observarmos principalmente a média, notas mínimas e notas máximas
# Histogram para observarmos a distribuição das notas das redações

## gráfico_01_1 - boxplot das notas -----------------------------------------------------------
p1_1 <- enem_cidade |>
  dplyr::select(nu_nota_redacao) |>
  ggplot2::ggplot(ggplot2::aes(x = nu_nota_redacao))+
  ggplot2::geom_boxplot()+
  ggplot2::labs(
    x = "",
    y = ""
  )+
  ggplot2::theme_classic()+
  theme_enem()

## gráfico_01_2 - Histograma das notas --------------------------------------------------------
p1_2 <- enem_cidade |>
  dplyr::select(nu_nota_redacao) |>
  ggplot2::ggplot(ggplot2::aes(x = nu_nota_redacao))+
  ggplot2::geom_histogram()+
  ggplot2::labs(
    x = "notas",
    y = ""
  )+
  ggplot2::theme_classic()+
  theme_enem()

## gráfico_01 ---------------------------------------------------------------------------------
library(patchwork)

p1 <- p1_1/p1_2


# ANÁLISE DAS 5 COMPETÊNCIAS DA REDAÇÃO DO ENEM -----------------------------------------------

# A nota da redação do ENEM é composta por 5 competências. Cada competência pode receceber
# notas de 0 a 200 pontos. A soma das competências resulta na nota da Nota da Redação.

# ANÁLISE DAS COMPETÊNCIAS
# São as competências que compõem as notas da redação do ENEM
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


# ANÁLISES DAS MÉDIAS DAS NOTAS QUE COMPÕEM AS NOTAS DA REDAÇÃO -------------------------------

# Cálculo das médias dos componentes da redação -----------------------------------------------
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

# VISUALIZAÇÃO DO RESULTADO -------------------------------------------------------------------

## Pivotar a base para long -------------------------------------------------------------------
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

## gráfico_02 de barras das médias dos componentes que compõem a nota da redação------
p2 <- medias_comp_redacao |>                                                             # base
  dplyr::mutate(
    notas_comp_redacao = forcats::fct_reorder(
      notas_comp_redacao,
      medias,
      .desc = TRUE
    )
  ) |>
  ggplot2::ggplot() +                                                                 # gráfico
  ggplot2::aes(
    x = notas_comp_redacao,
    y = medias,
    label = round(medias, 2)
  ) +
  ggplot2::geom_col(
    fill = "#576BC7"
  ) +
  ggplot2::geom_label(                                                                 # labels
    size = 4,
    color = "#D6F3FF",
    alpha = 0,
    nudge_y = -10
  ) +
  ggplot2::labs(                                                            # títulos dos eixos
    title = "Médias dos componentes da redação",
    subtitle = "A nota da redação é definida pela soma dos componentes",
    x = "notas dos componentes",
    y = "média das notas"
  ) +
  ggplot2::theme_classic() +                                                             # tema
  ggplot2::theme(
    legend.position = "none"
  ) +
  theme_enem()
p2

## maior nota entre os componentes da redação -------------------------------------------------
max_media <- medias_comp_redacao |>
    dplyr::slice_max(order_by = medias)

## menor nota entre os componentes da redação -------------------------------------------------
min_media <- medias_comp_redacao |>
    dplyr::slice_min(order_by = medias)


# CONSTRUIR GRÁFICO DE BOXPLOT  - UM BOXPLOT PARA CADA COMPETÊNCIA ----------------------------

## pivotar a base -----------------------------------------------------------------------------
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

## gráfico_03 - Densidade de distribuiçao das notas das competencias---------------------------
p3 <- comp_redacao_long |>
  ggplot2::ggplot(ggplot2::aes(competencias, notas_comp, fill = competencias))+
  ggplot2::geom_violin(width = 0.6)+
  ggplot2::geom_boxplot(width = 0.1, color = "black", alpha = 0.2)+
  ggplot2::geom_jitter(color="black", size=0.4, alpha=0.05) +
  ggplot2::labs(
    title = "Violin e boxplot das competências",
    x = "Competências",
    y = "notas"
  )+
  theme_enem()+
  ggplot2::theme(
    legend.position = "none"
  )

p3

# BASE PARA O GRÁFICO DE COLUNAS - INTERVALO DE NOTAS X FREQUENCIAS RELATIVAS -----------------

fabs_notas_redacao <- enem_cidade |>
  dplyr::select(nu_nota_redacao) |>
  table()

## total de alunos com nota máxima ------------------------------------------------------------

qtd_nota_max <- fabs_notas_redacao |>
  tail(n=1)

## criar as classes de frequências das notas --------------------------------------------------

intervalo_classes_notas <- seq(0, 1000, 100)
tab_classes_notas <- table(cut(enem_cidade$nu_nota_redacao,
                               breaks = intervalo_classes_notas,
                               right = FALSE)) |>
  prop.table() |>
  data.frame() |>
  dplyr::mutate(Freq = Freq*100)

## gráfico_04 - Médias dos componentes das redações -------------------------------------------

p4 <- tab_classes_notas |>
  ggplot2::ggplot()+
  ggplot2::aes(x= Var1,
               y = Freq,
               label = round(Freq, 3))+
  ggplot2::geom_col(fill = "#576BC7") +
  ggplot2::geom_label(                                                                 # labels
    size = 4,
    color = "#576BC7",
    alpha = 0,
    nudge_y = 1
  ) +
  ggplot2::labs(                                                            # títulos dos eixos
    title = "Frequências relativas de notas por intervalos de classe",
    x = "Intervalo de notas",
    y = "Frequência (%)"
  )+
  ggplot2::theme_classic()+
  theme_enem()

p4

# CONSTRUIR GRÁFICO REDACÕES COM PROBLEMAS ----------------------------------------------------

redacoes_com_problemas <- enem_cidade |>
  dplyr::filter(nu_nota_redacao == 0) |>
  dplyr::group_by(tp_status_redacao) |>
  dplyr::summarise(qte = dplyr::n()) |>
  dplyr::arrange(desc(qte)) |>
  dplyr::mutate(tp_status_redacao = as.factor(tp_status_redacao))

## gráfico_05 - Razões para nota zero nas redações --------------------------------------------
p5 <- redacoes_com_problemas |>
  ggplot2::ggplot(ggplot2::aes(x = qte,
                               y = tp_status_redacao,
                               label = qte))+
  ggplot2::geom_col(fill = "#576BC7")+
  ggplot2::geom_label(                                                                 # labels
    size = 4,
    color = "#576BC7",
    alpha = 0,
    nudge_x = 1
    ) +
  ggplot2::labs(                                                            # títulos dos eixos
    title = "Principais razões para eliminação das redações",
    x = "razões",
    y = "quantidades"
  ) +
  ggplot2::theme_classic()+
  theme_enem()

p5


# GRÁFICO DAS MÉDIAS DAS REDAÇÕES NOS ULTIMOS EXAMES -------------------------------------

## filtrar a base para a cidade
enem_redacao_historico <- enem |>
  dplyr::filter(no_municipio_prova == "Curitiba") |>
  dplyr::select(nu_nota_redacao, nu_ano) |>
  dplyr::group_by(nu_ano) |>
  dplyr::summarise(media = mean(nu_nota_redacao, na.rm = TRUE))

## alterar tipo de ano para inteiro
enem_redacao_historico[["nu_ano"]] <- as.integer(enem_redacao_historico[["nu_ano"]])

dplyr::glimpse(enem_redacao_historico)


p6_1 <- enem_redacao_historico |>
  ggplot2::ggplot()+
  ggplot2::aes(x = nu_ano,
               y = media)+
  ggplot2::geom_line()+
  ggplot2::geom_point()+
  ggplot2::theme_classic()+
  theme_enem()+
  ggplot2::labs(
    title = "Evolução das médias das redações"
  )

p6_1


# GRÁFICO QUANTIDADE DE REDAÇÕES COM NOTA ACIMA DE 900 NOS ÚLTIMOS ANOS ------------------

enem_redacao_historico_notas900 <- enem |>
  dplyr::filter(no_municipio_prova == "Curitiba" & nu_nota_redacao >=900) |>
  dplyr::select(nu_nota_redacao, nu_ano) |>
  dplyr::group_by(nu_ano) |>
  dplyr::summarise(qtde = dplyr::n())

p6_2 <- enem_redacao_historico_notas900 |>
  ggplot2::ggplot()+
  ggplot2::aes(x = nu_ano,
               y = qtde)+
  ggplot2::geom_line()+
  ggplot2::geom_point()+
  ggplot2::theme_classic()+
  theme_enem()+
  ggplot2::labs(
    title = "Quantidade de notas acima de 900"
  )

library(patchwork)


p6 <-  p6_1 + p6_2

p6
