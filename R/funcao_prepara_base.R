#---------------------------------------------------------------------------------------------#
#                                   Script Preparação da base ENEM <ANO>
#                                    Curso- R - Visualização II
#                                     Professora: Beatriz Milz
#                                     Aluno: Marcio Vakassugui
#---------------------------------------------------------------------------------------------#


# FUNÇÃO PARA PREPARAÇÃO DA BASE --------------------------------------------------------------

# As seguintes operações serão realizadas:
    ## executa "funcao_importacao_dados.R", a qual salva em "/data", a base enem<ANO>.rds;
    ## filtra os dados para o estado da federação passado como parâmetro para a função;
    ## seleciona as colunas de interesse para a análise;
    ## elimina os valores missing excluindo os alunos treineiros, os ausentes e os eliminados;
    ## salva em "/data/" a base preparada como "enem_<ANO>_<UF>.rds"

# parâmetros da função      : ano e uf
# retorno                   : base preparada sem os valores missings

preparacao_base <- function(ano,uf) {

    # IMPORTAÇÃO DOS DADOS
    endereco <- paste0("data/enem_",
                       ano, ".csv")
    enem <- readr::read_csv(endereco)                               # leitura da base enem_2018

    # TRABALHAR COM OS DADOS DO ESTADO DESEJADO
    enem_uf <- enem |>
        dplyr::filter(sg_uf_prova == uf)                             # filtro uf = parâmetro uf

    # SELECIONAR AS COLUNAS DE INTERESSE
    colunas_interesse<- c("nu_inscricao",                              # variáveis de interesse
                          "nu_ano",
                          "tp_faixa_etaria",
                          "tp_sexo",
                          "tp_estado_civil",
                          "tp_cor_raca",
                          "tp_nacionalidade",
                          "tp_st_conclusao",
                          "tp_ano_concluiu",
                          "tp_escola",
                          "tp_ensino",
                          "in_treineiro",
                          "co_municipio_esc",
                          "no_municipio_esc",
                          "sg_uf_esc",
                          "tp_dependencia_adm_esc",
                          "tp_localizacao_esc",
                          "no_municipio_prova",
                          "sg_uf_prova",
                          "tp_presenca_cn",
                          "tp_presenca_ch",
                          "tp_presenca_lc",
                          "tp_presenca_mt",
                          "nu_nota_cn",
                          "nu_nota_ch",
                          "nu_nota_lc",
                          "nu_nota_mt",
                          "tp_lingua",
                          "tp_status_redacao",
                          "nu_nota_comp1",
                          "nu_nota_comp2",
                          "nu_nota_comp3",
                          "nu_nota_comp4",
                          "nu_nota_comp5",
                          "nu_nota_redacao",
                          "q001",
                          "q002",
                          "q005",
                          "q006",
                          "q007",
                          "q008",
                          "q009",
                          "q022",
                          "q024",
                          "q025",
                          "q027"
    )

    enem_uf <- enem_uf |>                                      # base com os dados de interesse
        dplyr::select(dplyr::all_of(colunas_interesse))

    ## OPERAÇÕES PARA ELIMINAR OS VALORES MISSISNG

    ### EXCLUIR OS ALUNOS TREINEIROS

    enem_uf <- enem_uf |>
        dplyr::filter(in_treineiro == "Não") |>                 # filtrar alunos não treineiros
        dplyr::mutate(in_treineiro = NULL)

    ### EXCLUIR OS ALUNOS QUE FALTARAM ÀS PROVAS OU FORAM EXCLUÍDOS

    #### Vamos usar a seguinte lógica:
    # As provas foram realizadas em dois dias.
    # Cada dia teve duas provas
    # Os valores da variável presença assume para cada prova 0 se faltou, 1 se presente,
    # 2 se eliminado.
    # Somando os valores de presença, teremos:
    # zero = significa que o aluno faltou os dois dias (0+0+0+0);
    # dois = o aluno compareceu em um dos dias e fez a duas provas (1+1);
    # seis = o aluno compareceu em um dos dias (1+1) e foi eliminado nas duas provas do segundo
    # dia (2+2;
    # cinco = o aluno compareceu em um dos dias (1+1) e foi eliminado em uma prova do segundo
    # dia (1+2);
    # oito = o aluno compareceu nos dois dias, mas foi eliminado em todas as provas (2+2+2+2;
    # quatro = o aluno compareceu nos dois dias e não foi eliminado em nenhuma prova (1+1+1+1;


    #### Criar a variável presença com a soma das variáveis "tp_presenca_..."
    enem_uf <- enem_uf |>
        dplyr::mutate("presenca" = tp_presenca_ch + tp_presenca_cn + tp_presenca_lc + tp_presenca_mt)

    #### Selecionar apenas os alunos que compareceram todos os dias e não foram eliminados
    enem_uf <- enem_uf |>
        dplyr::filter(presenca == 4)

    # Caso o aluno tenha participado do primeiro dia (1+1) e no segundo dia tenha sido
    # eliminado em uma prova e não participado da segunda prova (2 + 0), também somaria 4.
    # Então, podemos eliminar estes valores.

    #### ELIMINAR OS REGISTROS DOS ALUNOS PRESENTE EM TRES PROVAS E ELIMINADO EM UMA PROVA
    enem_uf <- tidyr::drop_na(enem_uf, nu_nota_mt)

    # Missings eliminados

    # SALVAR OS DADOS EM DATA
    endereco_destino <- paste0("data/enem_", ano, "_", uf, ".csv")
    readr::write_csv(enem_uf, endereco_destino)

}


# PREPARAÇÃO DAS BASES -------------------------------------------------------------------

preparacao_base(2018, "PR")
preparacao_base(2018, "PR")
preparacao_base(2018, "PR")


