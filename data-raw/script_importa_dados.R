#---------------------------------------------------------------------------------------------#
#                                  Scritp Importação dos Dados ENEM <ano>
#                                    Curso- R - Visualização II
#                                     Professora: Beatriz Milz
#                                     Aluno: Marcio Vakassugui
#---------------------------------------------------------------------------------------------#


# IMPORTAÇÃO DOS DADOS ------------------------------------------------------------------------

# OBS. A rotina para a importação dos dados é fornecida pelo INEP juntamente com os demais
#      arquivos obtidos no momento do download do microdados.
#      Os pacotes necessários são "data.table" e "dplyr"

# CRIAR FUNÇÃO PARA A IMPORTAÇÃO DOS DADOS

# Esta função permite a importação dos dados do ENEM por ano de realização. O código para
# importação dos dados é fornecido juntamente com o arquivo zip dos microdados. Também realiza
# a primeira etapa de arrumação dos dados, alterando os tipos das variáveis e arrumando os
# nomes. A base resultante é arquivada na pasta "data" do projeto com o nome "enem_<ano>.rds",
# em que <ano> corresponde ao atributo fornecido para a função como parâmetro.

# parâmetros da função  : ano
# return                : arquivo salvo na pasta "data" do projeto com o nome "enem_<ano>.rds"


importa_microdados <- function(ano) {
    input_data <- paste0("data-raw/microdados_enem_",
                         ano,
                         "/DADOS/MICRODADOS_ENEM_",
                         ano,
                         ".csv")
    enem <- data.table::fread(input = input_data,
                              integer64 = "character",
                              skip = 0,                                         # ler do início
                              nrow = -1,                               # ler todos os registros
                              na.strings = "",
                              showProgress = TRUE
    )

    # TP_FAIXA_ETARIA:
    enem$TP_FAIXA_ETARIA <- factor(enem$TP_FAIXA_ETARIA,
                                   levels = c(1,2,3,4,5,6,7,8,9,10,11,12,
                                              13,14,15,16,17,18,19,20),
                                   labels = c('Menor de 17 anos','17 anos','18 anos','19 anos',
                                              '20 anos','21 anos','22 anos','23 anos',
                                              '24 anos','25 anos','Entre 26 e 30 anos',
                                              'Entre 31 e 35 anos', 'Entre 36 e 40 anos',
                                              'Entre 41 e 45 anos', 'Entre 46 e 50 anos',
                                              'Entre 51 e 55 anos', 'Entre 56 e 60 anos',
                                              'Entre 61 e 65 anos', 'Entre 66 e 70 anos',
                                              'Maior de 70 anos'))

    # IN_TREINEIRO
    enem$IN_TREINEIRO <- factor(enem$IN_TREINEIRO, levels = c(1,0),  labels=c('Sim','Não'))

    #TP_DEPENDENCIA_ADM_ESC
    enem$TP_DEPENDENCIA_ADM_ESC <- factor(enem$TP_DEPENDENCIA_ADM_ESC, levels = c(1,2,3,4),
                                          labels=c('Federal',
                                                   'Estadual',
                                                   'Municipal',
                                                   'Privada'))

    # TP_LOCALIZACAO_ESC
    enem$TP_LOCALIZACAO_ESC <- factor(enem$TP_LOCALIZACAO_ESC,
                                      levels = c(1,2),
                                      labels=c('Urbana',
                                               'Rural'))

    # TP_SIT_FUNC_ESC
    enem$TP_SIT_FUNC_ESC <- factor(enem$TP_SIT_FUNC_ESC, levels = c(1,2,3,4),
                                   labels=c('Em atividade',
                                            'Paralisada',
                                            'Extinta',
                                            'Escola extinta em anos anteriores'))

    # TP_SEXO
    enem$TP_SEXO <- factor(enem$TP_SEXO, levels = c('M','F'),
                           labels=c('Maculino',
                                    'Feminino'))

    # TP_ESTADO_CIVIL
    enem$TP_ESTADO_CIVIL <- factor(enem$TP_ESTADO_CIVIL, levels = c(0,1,2,3),
                                   labels=c('Solteiro(a)',
                                            'Casado(a)/Mora com um(a) companheiro(a)',
                                            'Divorciado(a)/Desquitado(a)/Separado(a)',
                                            'Viúvo(a)'))

    # TP_COR_RACA
    enem$TP_COR_RACA <- factor(enem$TP_COR_RACA, levels = c(0,1,2,3,4,5,6),
                               labels=c('Não declarado',
                                        'Branca','Preta',
                                        'Parda','Amarela',
                                        'Indígena',
                                        'Não dispõe da informação'))
    # TP_NACIONALIDADE
    enem$TP_NACIONALIDADE <- factor(enem$TP_NACIONALIDADE, levels = c(0,1,2,3,4),
                                    labels=c('Não informado',
                                             'Brasileiro(a)',
                                             'Brasileiro(a) Naturalizado(a)',
                                             'Estrangeiro(a)',
                                             'Brasileiro(a) Nato(a), nascido(a) no exterior'))


    # TP_ST_CONCLUSAO
    enem$TP_ST_CONCLUSAO <- factor(enem$TP_ST_CONCLUSAO,
                                   levels = c(1,2,3,4),
                                   labels=c('Já concluí o Ensino Médio',
                                            'Estou cursando e concluirei o Ensino Médio em 2018',
                                            'Estou cursando e concluirei o Ensino Médio após 2018',
                                            'Não concluí e Não estou cursando o Ensino Médio'))

    #_CONCLUIU
    enem$TP_ANO_CONCLUIU <- factor(enem$TP_ANO_CONCLUIU,
                                   levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                                   labels=c('Não informado','2017','2016',
                                            '2015','2014','2013',
                                            '2012','2011','2010',
                                            '2009','2008','2007',
                                            'Anterior a 2007'))

    # TP_ESCOLA
    enem$TP_ESCOLA <- factor(enem$TP_ESCOLA, levels = c(1,2,3,4),
                             labels=c('Não respondeu',
                                      'Pública',
                                      'Exterior',
                                      'Privada'))

    # TP_ENSINO
    enem$TP_ENSINO <- factor(enem$TP_ENSINO, levels = c(1,2,3),
                             labels=c('Ensino Regular',
                                      'Educação Especial - Modalidade Substitutiva',
                                      'Educação de Jovens e Adultos'))
    # TP_LINGUA
    enem$TP_LINGUA <- factor(enem$TP_LINGUA, levels = c(0,1),
                             labels=c('Inglês','Espanhol'))

    # TP_STATUS_REDACAO
    enem$TP_STATUS_REDACAO <- factor(enem$TP_STATUS_REDACAO, levels = c(1,2,3,4,5,6,7,8,9),
                                     labels=c('Sem problemas',
                                              'Anulada','Cópia Texto Motivador',
                                              'Em Branco','Fere Direitos Humanos',
                                              'Fuga ao tema',
                                              'Não atendimento ao tipo',
                                              'Texto insuficiente',
                                              'Parte desconectada'))

    #Q001
    enem$Q001 <- factor(enem$Q001,
                        levels = c('A','B','C','D','E','F','G','H'),
                        labels=c('Nunca estudou',
                                 'Não completou a 4ª série/5º ano do ensino fundamental',
                                 'Completou a 4ª série/5º ano, mas Não completou a 8ª série/9º ano do ensino fundamental',
                                 'Completou a 8ª série/9º ano do ensino fundamental, mas Não completou o Ensino Médio',
                                 'Completou o Ensino Médio, mas Não completou a Faculdade',
                                 'Completou a Faculdade, mas Não completou a Pós-graduação',
                                 'Completou a Pós-graduação','Não sei'))

    # Q002
    enem$Q002 <- factor(enem$Q002, levels = c('A','B','C','D','E','F','G','H'),
                        labels=c('Nunca estudou',
                                 'Não completou a 4ª série/5º ano do ensino fundamental',
                                 'Completou a 4ª série/5º ano, mas Não completou a 8ª série/9º ano do ensino fundamental',
                                 'Completou a 8ª série/9º ano do ensino fundamental, mas Não completou o Ensino Médio',
                                 'Completou o Ensino Médio, mas Não completou a Faculdade',
                                 'Completou a Faculdade, mas Não completou a Pós-graduação',
                                 'Completou a Pós-graduação','Não sei'))
    #Q005
    enem$Q005 <- factor(enem$Q005, levels = c(1,2,3,4,5,6,7,8,9,10,
                                              11,12,13,14,15,16,17,
                                              18,19,20),
                        labels=c('1','2','3','4','5','6','7','8','9','10',
                                 '11','12','13','14','15','16','17','18','19','20'))

    # Q006
    enem$Q006 <- factor(enem$Q006,levels =  c('A','B','C','D','E','F','G','H','I',
                                              'J','K','L','M','N','O','P','Q'),
                        labels=c('Nenhuma renda.',
                                 'até R$ 954,00',
                                 'De R$ 954,01 até R$ 1.431,00',
                                 'De R$ 1.431,01 até R$ 1.908,00',
                                 'De R$ 1.908,01 até R$ 2.385,00',
                                 'De R$ 2.385,01 até R$ 2.862,00',
                                 'De R$ 2.862,01 até R$ 3.816,00',
                                 'De R$ 3.816,01 até R$ 4.770,00',
                                 'De R$ 4.770,01 até R$ 5.724,00',
                                 'De R$ 5.724,01 até R$ 6.678,00',
                                 'De R$ 6.678,01 até R$ 7.632,00',
                                 'De R$ 7.632,01 até R$ 8.586,00',
                                 'De R$ 8.586,01 até R$ 9.540,00',
                                 'De R$ 9.540,01 até R$ 11.448,00',
                                 'De R$ 11.448,01 até R$ 14.310,00',
                                 'De R$ 14.310,01 até R$ 19.080,00',
                                 'Mais de R$ 19.080,00'))

    # Q007
    enem$Q007 <- factor(enem$Q007, levels = c('A','B','C','D'),
                        labels=c('Não','Sim, um ou dois dias por semana',
                                 'Sim, três ou quatro dias por semana',
                                 'Sim, pelo menos cinco dias por semana'))

    # Q008
    enem$Q008 <- factor(enem$Q008, levels = c('A','B','C','D','E'),
                        labels=c('Não',
                                 'Sim, um',
                                 'Sim, dois',
                                 'Sim, três',
                                 'Sim, quatro ou mais'))

    # Q009
    enem$Q009 <- factor(enem$Q009, levels = c('A','B','C','D','E'),
                        labels=c('Não',
                                 'Sim, um',
                                 'Sim, dois',
                                 'Sim, três',
                                 'Sim, quatro ou mais'))

    # Q024
    enem$Q024 <- factor(enem$Q024, levels = c('A','B','C','D','E'),
                        labels=c('Não',
                                 'Sim, um',
                                 'Sim, dois',
                                 'Sim, três',
                                 'Sim, quatro ou mais'))

    # Q025
    enem$Q025 <- factor(enem$Q025, levels = c('A','B'), labels=c('Não','Sim'))

    # Q027
    enem$Q027 <- factor(enem$Q027, levels = c('A','B','C','D','E','F'),
                        labels=c('Somente em escola pública',
                                 'Parte em escola pública e parte em escola privada SEM bolsa de estudo integral.',
                                 'Parte em escola pública e parte em escola privada COM bolsa de estudo integral.',
                                 'Somente em escola privada SEM bolsa de estudo integral.',
                                 'Somente em escola privada COM bolsa de estudo integral.',
                                 'Não frequentei a escola'))


    # RENOMEAR AS VARIÁVEIS -------------------------------------------------------------------

    enem <- janitor::clean_names(enem)

    # SALVAR A BASE  --------------------------------------------------------------------------
    enem_ano <- paste0("data/enem_", ano, ".csv")

    readr::write_csv(enem, enem_ano)
}


# IMPORTAR DADOS DOS ANOS  ---------------------------------------------------------------

importa_microdados(2018)
importa_microdados(2019)
importa_microdados(2020)
