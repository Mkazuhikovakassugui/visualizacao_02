#------------------------------------------------------------------------------------------------------------#
#                                        Análise da base ENEM
#                                    Curso- R - Visualização II
#                                     Professora: Beatriz Milz
#                                     Aluno: Marcio Vakassugui
#------------------------------------------------------------------------------------------------------------#


# CARREGA AS FUNÇÕES DA PASTA R -------------------------------------------------------------------------

source("R/funcao_importacao_dados.R")
source("R/funcao_preparacao_bases.R")

uf <- "PR"

# IMPORTAÇÃO DA BASE DE DADOS <2018><PR> ----------------------------------------------------------------
importa_microdados(2018)

# PREPARACAO DA BASE <2018> <PR> ------------------------------------------------------------------------
preparacao_base(2018, uf)

# IMPORTAÇÃO DA BASE DE DADOS <2019><PR> ----------------------------------------------------------------
importa_microdados(2019)

# PREPARACAO DA BASE <2018> <PR> ------------------------------------------------------------------------
preparacao_base(2019, uf)

# IMPORTAÇÃO DA BASE DE DADOS <2020><PR> ----------------------------------------------------------------
importa_microdados(2020)

# PREPARACAO DA BASE <2018> <PR> ------------------------------------------------------------------------
preparacao_base(2020, uf)


# ABRIR AS BASES UNINDO EM UMA ÚNICA TIBBLE -------------------------------------------------------------
arquivos <- fs::dir_ls(path = "data", glob = "*PR.csv$")              # vetor com os caminhos dos arquivos .rds.
enem <- purrr::map_dfr(arquivos, readr::read_csv)      # dataframe c/ três arquivos em uma estrutura de dados.

# SALVA O ARQUIVO COM A BASE PREPARADA ------------------------------------------------------------------
readr::write_csv(enem, "data/enem_analise.csv")

# CARREGAR A BASE GERADA --------------------------------------------------------------------------------
enem <- readr::read_csv("data/enem_analise_final.csv")


# ARRUMAÇÃO DA VARIÁVEL CIDADE (NOMES COM PROBLEMAS - ACENTUAÇÃO) ---------------------------------------
# á     ---> <e1>
# â     ---> <e2>
# ã     ---> <e3>
# é     ---> <e9>
# ê     ---> <ea>
# í     ---> <ed>
# ó     ---> <f3>
# ô     ---> <f4>
# ç     ---> <e7>

altera_caracter <- function(coluna, padrao) {
    dplyr::case_when(
        padrao == "<e1>" ~ stringr::str_replace_all(coluna, pattern = "<e1>", replacement = "á"),
        padrao == "<e2>" ~ stringr::str_replace_all(coluna, pattern = "<e2>", replacement = "â"),
        padrao == "<e3>" ~ stringr::str_replace_all(coluna, pattern = "<e3>", replacement = "ã"),
        padrao == "<e9>" ~ stringr::str_replace_all(coluna, pattern = "<e9>", replacement = "é"),
        padrao == "<ea>" ~ stringr::str_replace_all(coluna, pattern = "<ea>", replacement = "ê"),
        padrao == "<ed>" ~ stringr::str_replace_all(coluna, pattern = "<ed>", replacement = "í"),
        padrao == "<f3>" ~ stringr::str_replace_all(coluna, pattern = "<f3>", replacement = "ó"),
        padrao == "<f4>" ~ stringr::str_replace_all(coluna, pattern = "<f4>", replacement = "ô"),
        padrao == "<e7>" ~ stringr::str_replace_all(coluna, pattern = "<e7>", replacement = "ç"),
        padrao == "<c1>" ~ stringr::str_replace_all(coluna, pattern = "<c1>", replacement = "Á"),
        padrao == "<d3>" ~ stringr::str_replace_all(coluna, pattern = "<d3>", replacement = "Ó")
        )}


enem_novo <- enem |>
    dplyr::mutate(dplyr::across(
        .cols = dplyr::starts_with("no_municipio"),
        .fns = altera_caracter, "<c1>"
    ))

enem_novo <- enem_novo |>
    dplyr::mutate(dplyr::across(
        .cols = dplyr::starts_with("no_municipio"),
        .fns = altera_caracter, "<d3>"
    ))

enem_novo <- enem_novo |>
    dplyr::mutate(dplyr::across(
        .cols = dplyr::starts_with("no_municipio"),
        .fns = altera_caracter, "<e1>"
    ))

enem_novo <- enem_novo |>
    dplyr::mutate(dplyr::across(
        .cols = dplyr::starts_with("no_municipio"),
        .fns = altera_caracter, "<e2>"
    ))

enem_novo <- enem_novo |>
    dplyr::mutate(dplyr::across(
        .cols = dplyr::starts_with("no_municipio"),
        .fns = altera_caracter, "<e3>"
    ))

enem_novo <- enem_novo |>
    dplyr::mutate(dplyr::across(
        .cols = dplyr::starts_with("no_municipio"),
        .fns = altera_caracter, "<e9>"
    ))

enem_novo <- enem_novo |>
    dplyr::mutate(dplyr::across(
        .cols = dplyr::starts_with("no_municipio"),
        .fns = altera_caracter, "<ea>"
    ))

enem_novo <- enem_novo |>
    dplyr::mutate(dplyr::across(
        .cols = dplyr::starts_with("no_municipio"),
        .fns = altera_caracter, "<ed>"
    ))

enem_novo <- enem_novo |>
    dplyr::mutate(dplyr::across(
        .cols = dplyr::starts_with("no_municipio"),
        .fns = altera_caracter, "<f3>"
    ))

enem_novo <- enem_novo |>
    dplyr::mutate(dplyr::across(
        .cols = dplyr::starts_with("no_municipio"),
        .fns = altera_caracter, "<f4>"
    ))

enem_novo <- enem_novo |>
    dplyr::mutate(dplyr::across(
        .cols = dplyr::starts_with("no_municipio"),
        .fns = altera_caracter, "<e7>"
    ))


# ARRUMAÇÃO DA VARIÁVEL NU_NOTA_COMP1  ------------------------------------------------------------------

#Mudança do tipo de chr para dbl
enem[["nu_nota_comp1"]] <- as.double(enem[["nu_nota_comp1"]])



# SALVAR A BASE PREPARADA PARA O SCRIPT ANALISE ---------------------------------------------------------

readr::write_csv(enem_novo, "data/enem_analise_final.csv")
