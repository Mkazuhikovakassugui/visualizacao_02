#------------------------------------------------------------------------------------------------------------#
#                                   Cria Relatórios ENEM - Cidades
#                                    Curso- R - Visualização II
#                                     Professora: Beatriz Milz
#                                     Aluno: Marcio Vakassugui
#------------------------------------------------------------------------------------------------------------#



# Cria caminho global com here --------------------------------------------------------------------------

caminho_relatorio <- here::here("docs/trabalho_final.Rmd")



# Renderiza o arquivo - Relatório RMD -------------------------------------------------------------------

rmarkdown::render(caminho_relatorio, params = list(cidade = "Santa Helena", estado = "PR"))


# Renderiza o arquivo - Relatório RMD com novo nome do arquivo ------------------------------------------

rmarkdown::render(caminho_relatorio, params = list(cidade = "Toledo", estado = "PR"),
                  output_file = "toledo.html")


# Encontrar os municípios listados na base --------------------------------------------------------------

enem_base <- readr::read_csv("data/enem_analise.csv")

lista <- c("Almirante Tamandaré",
           "Alvorada do Sul",
           "Ampére",
           "Apucarana",
           "Arapongas",
           "Arapoti",
           "Araucária",
           "Assaí",
           "Assis Chateaubriand",
           "Astorga",
           "Bandeirantes",
           "Cafelândia",
           "Cambé",
           "Campina Grande do Sul",
           "Campo Largo",
           "Campo Magro",
           "Campo Mourão",
           "Capanema",
           "Cascavel",
           "Castro",
           "Chopinzinho",
           "Cianorte",
           "Clevelândia",
           "Colombo",
           "Colorado",
           "Cornélio Procópio",
           "Coronel Vivida",
           "Curitiba",
           "Dois Vizinhos",
           "Fazenda Rio Grande",
           "Foz do Iguaçu",
           "Francisco Beltrão",
           "Goioerê",
           "Guarapuava",
           "Guaratuba",
           "Guaíra",
           "Ibaiti",
           "Ibiporã",
           "Irati",
           "Ivaiporã",
           "IvaíJacarezinho",
           "Jaguariaíva",
           "Jandaia do Sul",
           "Lapa",
           "Laranjeiras do Sul",
           "Loanda",
           "Londrina",
           "Mandirituba",
           "Marechal Cândido Rondon",
           "Marialva",
           "Maringá",
           "Matinhos",
           "Mauá da Serra",
           "Medianeira",
           "Nova Esperança",
           "Paiçandu",
           "Palmas",
           "Palmeira",
           "Palotina",
           "Paranaguá",
           "Paranavaí",
           "Pato Branco",
           "Pinhais",
           "Pinhão",
           "Piraquara",
           "Pitanga",
           "Ponta Grossa",
           "Porecatu",
           "Prudentópolis",
           "Quatro Barras",
           "Quedas do Iguaçu",
           "Realeza",
           "Rio Branco do Sul",
           "Rio Negro",
           "Rolândia",
           "Santa Helena",
           "Santa Terezinha de Itaipu",
           "Santo Antônio da Platina",
           "Sarandi",
           "São José dos Pinhais",
           "São Mateus do Sul",
           "São Miguel do Iguaçu",
           "Telêmaco Borba",
           "Toledo",
           "Ubiratã",
           "Umuarama",
           "União da Vitória",
           "Wenceslau Braz"
           )
length(lista)
