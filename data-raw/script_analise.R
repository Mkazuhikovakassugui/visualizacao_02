#---------------------------------------------------------------------------------------------#
#                                        Análise da base ENEM
#                                    Curso- R - Visualização II
#                                     Professora: Beatriz Milz
#                                     Aluno: Marcio Vakassugui
#---------------------------------------------------------------------------------------------#


# Carregamento da base arrumada ---------------------------------------------------------------

enem <- readr::read_csv("data/enem_analise_final.csv", show_col_types = FALSE)

# Carregamento das funções
source("R/funcao_tema_graficos.R")

enem_cidade <- enem |>
  dplyr::filter(nu_ano == 2020, no_municipio_prova == "Santa Helena")


# ANO DE 2020
# ANÁLISE DAS 5 COMPETÊNCIAS DA REDAÇÃO DO ENEM -----------------------------------------------

# A nota da redação do ENEM é composta por 5 competências. Cada competência pode receceber
# notas de 0 a 200 pontos. A soma das competências resulta na nota da Nota da Redação.

# ANÁLISE DAS COMPETÊNCIAS
# São as competências que compõem as notas da redação do ENEM
    # dominío da norma padrão da Língua Portuguesa;
    # compreensão da proposta da redação;
    # emprego de mecanismos linguísticos alternativos;
    # elaboração de uma proposta de intervenção para o problema abordado.

# ANÁLISES DAS MÉDIAS DAS NOTAS QUE COMPÕEM AS NOTAS DA REDAÇÃO

# Cálculo das médias dos componentes da redação
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

# VISUALIZAÇÃO DO RESULTADO

# Pivotar a base para long
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

# Cosntruir o gráfico de barras das médias dos componentes que compõem a nota da redação
p1 <- medias_comp_redacao |>                                                             # base
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
p1

max_media <- medias_comp_redacao |>
    dplyr::slice_max(order_by = medias)

min_media <- medias_comp_redacao |>
    dplyr::slice_min(order_by = medias)


# CONSTRUIR GRÁFICO DE BOXPLOT  - UM BOXPLOT PARA CADA COMPETÊNCIA

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

p2 <- comp_redacao_long |>
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


p2


# BOXPLOT COMPETENCIAS E NOTAS -----------------------------------------------------------

enem_cidade |>
  dplyr::select(nu_nota_comp1, )
  ggplot2::ggplot()+
  aes(x = )



# GRÁFICO DE COLUNAS - INTERVALO DE NOTAS X FREQUENCIAS RELATIVAS ------------------------

fabs_notas_redacao <- enem_cidade |>
  dplyr::select(nu_nota_redacao) |>
  table()

# total de alunos com nota máxima

qtd_nota_max <- fabs_notas_redacao |>
  tail(n=1)

# criar as classes de frequências das notas

intervalo_classes_notas <- seq(0, 1000, 100)
tab_classes_notas <- table(cut(enem_cidade$nu_nota_redacao,
                               breaks = intervalo_classes_notas,
                               right = FALSE)) |>
  prop.table() |>
  data.frame() |>
  dplyr::mutate(Freq = Freq*100)


p2 <- tab_classes_notas |>
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

p2





´# Notas médias nas edições de 2018, 2019 e 2020 ------------------------------------------




