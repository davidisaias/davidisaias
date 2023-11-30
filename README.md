' 
Universidade Federal do Rio Grande do Norte
Grupo de Pesquisa em planejamento territorial, Técnica e Desigualdades Socioespaciais - GEOPLAT/UFRN
Aprendizado de Máquina aplicado ao monitoramento da atuação do circuito superior da economia
Aluno: David Isaias de Souza 

Teste: Não sei qual
Objetivo: Analisar a correlação de váriaveis sociais, economicas e humanas na predição de áreas 
sucetiveis a desigualdade socioespacial 
Data: 27/11/2023
'

# Instalar os pacotes e carregar as bibliotecas necessárias ---------------

install.packages("tidyverse") #manipulação, visualização e análise de dados.
install.packages("tidymodels") #treinar, avaliar e ajustar modelos de aprendizado de máquina e estatísticas
install.packages("sf") #criar, manipular e visualizar dados geoespaciais
install.packages("tmap") #Plotar mapas
install.packages("spatialEco") #Realizar analises espaciais 
install.packages("ggalluvial")
install.packages("vip")
install.packages("ranger")

library(tidyverse)
library(tidymodels)
library(sf)
library(tmap)
library(ranger)
library(spatialEco)
library(factoextra)
library(FactoMineR)
library(aopdata)
library(cowplot)
library(cluster)
library(scales)
library(vip)
library(ggalluvial)


# Importar dados ----------------------------------------------------------


dados <- read_csv("D:/aprendizmaq/davdi_correcao.csv") %>%
  select(-...1) %>%
  mutate(Zona_adm_new = as.factor(Zona_adm_new))


# Parte 1 -  caracterizar as regioes --------------------------------------
#Nomes das variaveis para o eixo x do grafico
names_var <- select(dados, -Zona_adm_new) %>% colnames()

ggplot(data = dados,
       aes(axis1 = populacao_total, axis2 = renda_media_psm, axis3 = populacao_adulta_trabalho,
           axis4 = ubs_zona, axis5 = unidades_escolares, axis6 = taxa_alfabetizacao, axis7= unidades_segurança,
           axis8 = crimes_violentos, axis9 = rendimento_nm, axis10=idh, axis11= abs_agua_perc, axis12 = rede_esgoto,
           axis13= coleta_lixo, axis14=num_favelas, axis15=drenagem, axis16=paviementacao)) +
  labs(x= "variaveis", y = "", fill = "Regioes",
       caption = "Fonte= SEMURB, 2020.")+
  scale_x_discrete(limits = c(names_var)) +
  geom_alluvium(color= "black",aes(fill=Zona_adm_new)) +
  geom_stratum() +
  scale_fill_viridis_d(option = "C")+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),size = 3) +
  theme_void() +
  ggtitle("Caracterizacao das regioes administratrivas por variaveis demograficas",
          "estratificado por variaveis demograficas e socioeconomicas") +
  theme(title = ggplot2::element_text(size = 16, face = "bold"),
        axis.text.x = ggplot2::element_text(size = 9, angle = 90, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 13, face = "bold"),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 13),
        legend.title = ggplot2::element_text(size = 13, face = "bold"),
        legend.key = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(25, 25, 10, 25),
        plot.caption = ggplot2::element_text(color = "grey30", hjust = 1, size = 9)
  )


# Parte 2 - Calcular a importancia das variaveis ----------------------------

#Calcule a importancia das variaveis usando o random forest e o metodo de impurity de classificacao
training_model <- ranger(Zona_adm_new ~., data = dados,
                         importance = "impurity", num.trees = 10)

#Calcule a importancia das variaveis usando vip () e o modelo treinado
vip_calculo <- training_model %>%
  vip(num_features = length(colnames(.))) # a funcao vip () do pacote vip

# Convert o calculo par a data.frame e calcule a contribuicao acumulada em % para cada variavel
vip_data.frame <- vip_calculo$data %>%
  mutate(Importance = round(Importance / sum(Importance) * 100, digits = 2)) %>% #calculando a contribuicao acumulada
  arrange(desc(Importance)) %>% #colocando os valores em ordem decrescente
  set_names(c("variavel", "importance")) #renomeando as colunas

#Plot o grafico com o ggplot2
vip_data.frame %>%
  ggplot(aes(x=reorder(variavel, importance), y=importance, fill=importance))+
  geom_bar(stat="identity", position="dodge", show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 20))+
  coord_flip() +
  geom_text(aes(label = paste0(importance, "%")), hjust = 0, size = 5, fontface = "bold") + # checar o tamanho do valor % no grafico
  scale_fill_viridis_c(direction = -1,option = "C") + # ver F1 desta funcao que tem opcoes de cor entre A e E
  labs(title = "Variaveis determinantes para cada zona administrativa de Natal, RN",
       x = "Variavel",
       y = "Contribuicao acumulada de cada variavel(%)") +
  theme_minimal(base_size = 12) # checar outros temas, como o theme_bw()
