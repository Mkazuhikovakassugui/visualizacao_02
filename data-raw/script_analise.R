#------------------------------------------------------------------------------------------------------------#
#                                        Análise da base ENEM
#                                    Curso- R - Visualização II
#                                     Professora: Beatriz Milz
#                                     Aluno: Marcio Vakassugui
#------------------------------------------------------------------------------------------------------------#


# Carregamento da base arrumada -------------------------------------------------------------------------

enem <- readr::read_csv("data/enem_analise.csv", show_col_types = FALSE)


# ANO DE 2018
# ANÁLISE DAS 5 COMPETÊNCIAS DA REDAÇÃO DO ENEM ---------------------------------------------------------

# A nota da redação do ENEM é composta por 5 competências. Cada competência pode receber notas de 0 a 200
# pontos. A soma das competências resulta na nota da Nota da Redação.

# ANÁLISE DAS COMPETÊNCIAS
# São as competências que compõem as notas da redação do ENEM
    # dominío da norma padrão da Língua Portuguesa;
    # compreensão da proposta da redação;
    # emprego de mecanismos linguísticos alternativos;
    # elaboração de uma proposta de intervenção para o problema abordado.

# ANÁLISES DAS MÉDIAS DAS NOTAS QUE COMPÕEM AS NOTAS DA REDAÇÃO

# Cálculo das médias dos componentes da redação
medias_comp_redacao <- enem |>
  dplyr::filter(nu_ano == 2018, no_municipio_prova == "Curitiba") |>
  dplyr::summarise(dplyr::across(
    .cols = dplyr::starts_with("nu_nota_comp"),
    .fns = mean, na.rm = TRUE
  ))

medias_comp_redacao <- medias_comp_redacao |>                                          # renomear as variáveis
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
p1 <- medias_comp_redacao |>                                            # base
  dplyr::mutate(
    notas_comp_redacao = forcats::fct_reorder(
      notas_comp_redacao,
      medias,
      .desc = TRUE
    )
  ) |>
  ggplot2::ggplot() +                                                   # gráfico
  ggplot2::aes(
    x = notas_comp_redacao,
    y = medias,
    label = round(medias, 2)
  ) +
  ggplot2::geom_col(
    fill = "#576BC7"
  ) +
  ggplot2::geom_label(                                                  # labels
    size = 4,
    color = "#D6F3FF",
    alpha = 0,
    nudge_y = -10
  ) +
  ggplot2::labs(                                                        # títulos dos eixos
    title = "Médias dos componentes da redação",
    subtitle = "A nota da redação é definida pela soma dos componentes",
    x = "notas dos componentes",
    y = "média das notas"
  ) +
  ggplot2::theme_classic() +                                            # tema
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::theme(                                                       # customização do tema
    panel.background = ggplot2::element_rect(fill = "#D6F3FF"),                # cor do fundo do gráfico
    plot.background = ggplot2::element_rect(fill = "#D6F3FF"),                 # cor da grade externa
    plot.margin = ggplot2::unit(c(1, 2, 1, 1), "cm"),                          # distância das margens
    plot.title = ggplot2::element_text(                                        # título do gráfico.
      size = 18,                                                                     # tamanho da fonte
      face = "bold",                                                                # não negrito
      family = "",                                                                   # fonte do título
      hjust = 0,                                                                     # posição na horizontal
      margin = ggplot2::unit(c(0, 0, 0.5, 0.5), "cm")                                # margens do título
    ),
    plot.subtitle = ggplot2::element_text(                                     # subtítulo
      size = 11,                                                                     # tamanho da fonte
      family = "",                                                                   # fonte do subtítulo
      hjust = 0,                                                                     # posição na horizontl
    ),
    text = ggplot2::element_text(                                              # textos config. geral de texto
      family = "",                                                                   # fonte
      color = "#000E14",                                                             # cor
      size = 13,                                                                     # tamanho da fonte
      hjust = 0,                                                                     # posição na horizontal
      face = "bold"                                                                  # negrito
    ),
    axis.title = ggplot2::element_text(                                        # texto do título do eixo x e y
        face = "bold",                                                                 # negrito
        size = 13,                                                                     # tamanho
        hjust = 0.5,                                                                   # posição na horizontal
    ),
    axis.text.x = ggplot2::element_text(                                       # texto do eixo x
      color = "#000E14",                                                             # cor
      size = 11,                                                                    # tamanho
      face = "plain",                                                                 # negrito
      margin = ggplot2::unit(c(0.3, 0, 0.5, 0), "cm")                                # margens do texto
    ),
    axis.text.y = ggplot2::element_text(                                       # texto do eixo y
      color = "#000E14",                                                             # cor
      size = 11,                                                                    # tamanho
      face = "plain",                                                                 # negrito
      family = "",                                                                   # fonte
      margin = ggplot2::unit(c(0, 0.5, 0, 0.5), "cm")                                # margens do texto
    ),
    axis.ticks.x = ggplot2::element_line(color = "#1F1F1F"),                   # ticks do eixo x
    axis.line.x = ggplot2::element_line(color = "#1F1F1F"),                    # cor da linha do eixo x
    axis.ticks.y = ggplot2::element_line(color = "#1F1F1F"),                   # padrões dos ticks do eixo x
    axis.line.y = ggplot2::element_line(color = "#1F1F1F"),                    # cor da linha do eixo y
  )

p1


medias_comp_redacao |>
    dplyr::slice_max(order_by = medias)

medias_comp_redacao |>
    dplyr::slice_min(order_by = medias)





