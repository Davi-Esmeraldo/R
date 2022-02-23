#_____Davi Esmeraldo_______# 
#_____Projeto Fantasma_____#

#Versão ----

version

# Obs: Foi utilizado R versão 4.0.3

#Exigências ----

#-> Série histórica do número de participantes nas olimpíadas de VERÃO para cada sexo
#-> Correlação entre número de medalhas e idade
#-> Quantidade de pódios por continente

#-> Distribuição dos IMCs por jogos de verão e inverno
#-> Top 5 de participantes considerando o ganho das medalhas, com pesos diferentes para cada uma
#-> Países com menor número de participações

#__ Carregando Pacotes ----

pacman::p_load(tidyverse, dplyr,tidyr,plotly,writexl,readxl,scales,ggplot2)

# Padronização Estat ----

cores_estat <- c(
  "#A11D21","#003366", "#CC9900", "#663333", "#FF6600",
  "#CC9966", "#999966", "#006606", "#008091", "#041835",
  "#666666"
)

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}
# Carregando Banco de dados ----

setwd('C:\\Users\\daviz\\Downloads\\Projeto Fantasma ESTAT 2021')
getwd()

dados = read.csv("athlete_events.csv.csv")

count_def =  read.csv("country_definitions.csv.csv") 

# Breve análise ----

View(dados)

glimpse(dados)

View(count_def)

glimpse(count_def)

# Dimensao da variavel ; linhas e colunas 

dim(dados) 

#  Classe das variaveis 

sapply(dados, class) 

# resumo

summary(dados)

### 1) ----

#Série histórica do número de participantes nas olimpíadas
#de VERÃO para cada sexo 

# Seleção das variáveis necessárias
# 'Split' da variável Games em Games e Ano
# filtrar jogos de verão

summer_games = dados %>%
  select(Name,Sex,Games) %>%
  separate(Games,c('Year','Games'),' ') %>%
  filter(Games == 'Summer')

summer_games = summer_games %>%    # desconsiderar múltiplas contagens 
  distinct()

summer_games = summer_games %>%
  group_by(Sex) %>%
  count(Year)

# 'limpar' dados . Converter tipos


class(summer_games$Year)
summer_games$Year = as.numeric(summer_games$Year)
class(summer_games$Year)

summary(summer_games$Year)

# Gráfico de linha bivariado, disposto por sexo

# Renomear

summer_games$Sex[summer_games$Sex =='F']<- "Feminino"
summer_games$Sex[summer_games$Sex =='M']<- "Masculino"
names(summer_games)[names(summer_games)== "Sex"]<- "Gênero"

# gráfico

ggplot(summer_games) +
  aes(x = Year, y = n,group = Gênero , colour  = Gênero)+
  geom_line(size =1) + geom_point(size =2)+
  scale_x_continuous(breaks = seq(1896,2016,10))+
  labs(x ="Ano", y ="Participantes")+
  theme_estat()
ggsave("Serie_histórica_PF.pdf", width =158, height =93, units ="mm")

ggplotly() # permite por interatividade visualizar observações em tempo real

# PORCENTAGEM PARTICIPAÇÃO MULHERES 
#(ANO,NUM_MULHERES)

#(1980,1123) 
round(1123/(1123+4129)*100,1)
#(1900,23) 
round(23/(1197 + 23)*100,1)
#(2016,5031) 
round(5031/(5031+6143)*100,1)

## Identificar pontos interresantes 
## 1 participação feminina 
# anos de cresmimentos mais perceptíveis 


### 2) ----

# Estudo das idade - frequencias observadas em tabela

idades = dados %>%
  select(Age)  %>%
  filter(Age != 'Na') %>%
  count(Age) %>%
  mutate(relativa = n/sum(n)*100)  # frequencias relatiavas

names(idades)[names(idades)== "n"]<- "Frequencia_idades"

summary(idades$Age)  # medidas resumo idade

# grafico 

ggplot(dados) +
  aes(x = Age) +
  geom_histogram(aes(y = 100 * (..count..)/ sum(..count..),),
  colour ="white",fill ="#A11D21",binwidth =6) +
  labs(x ="Idade", y ="Porcentagem") +
  xlim(0,60)+
  theme_estat()
ggsave("Dispersao_2_PF.pdf", width =158, height =93, units ="mm")


# Não levando em consideração as categorias das medalhas

# Variáveis quantitativas discreta e contínua  


# Seleção das variáveis necessárias
# AGRUPAR POR IDADE
# filtrar por diferentes de NA

medal_old = dados %>%
  select(Age,Medal)%>%
  group_by(Age) %>%
  filter(Medal != 'NA',Age != 'NA')


medal_old$Medal[medal_old$Medal!='NA']<- 1  # disconsidera tipo de medalha

medal_old = medal_old %>%      # contagem
count(Medal)   

medal_old$Medal = NULL


sum(medal_old$n) # 39051   # soma das frequencias observadas

medal_old = medal_old %>%
mutate( freq_r = n/39051*100)  # frequencia relativa

names(medal_old)[names(medal_old)== "n"]<- "Frequencia_medalhas"


idade_medalha_join <- inner_join(idades,medal_old,by="Age")

idade_medalha_join$relativa = NULL
idade_medalha_join$freq_r = NULL

idade_medalha_join = idade_medalha_join %>%
  mutate(porcentagem = Frequencia_medalhas/ Frequencia_idades * 100)

# dispersao idade e frequencia

ggplot(medal_old) +
  aes(x = Age, y = freq_r) +
  geom_point(colour ="#A11D21", size =3) +
  labs(x ="Idade",y ="Frequência relativa") +
  theme_estat()

# dispersao idade e freq relativa

idade_medalha_join_removed = idade_medalha_join[-c(1), ]  # remover ocorrência 100% 

ggplot(idade_medalha_join_removed) +
  aes(x = Age, y = porcentagem) +
  geom_point(colour ="#A11D21", size =3) +
  labs(x ="Idade",y ="Frequência relativa") +
  theme_estat()



ggplot(idade_medalha_join) +
  aes(x = Age, y = porcentagem) +
  geom_point(colour ="#A11D21", size =3) +
  labs(x ="Idade",y ="Frequência relativa") +
  theme_estat()
ggsave("Dispersao_relativa.pdf", width =158, height =93, units ="mm")

# #-> Correlação entre número de medalhas e idade


cor.test(idade_medalha_join$Age,idade_medalha_join$porcentagem,method = 'pearson')  # correlação negativa  
cor.test(idade_medalha_join_removed$Age,idade_medalha_join_removed$porcentagem,method = 'pearson')  # correlação negativa  


# Levando em consideração as categorias das medalhas

medal_old_2 = dados %>%
     select(Age,Medal)%>%
     filter(Medal != 'NA',Age != 'Na') %>%
     group_by(Medal) 

medal_old_2$Medal[medal_old_2$Medal == 'Gold']<- "Ouro"
medal_old_2$Medal[medal_old_2$Medal == 'Silver']<- "Prata"

medal_old_2$Medal <- factor(medal_old_2$Medal , levels=c("Bronze", "Prata", "Ouro"))

ggplot(medal_old_2) +
aes(x = Medal, y = Age) +
geom_boxplot(fill =c("#A11D21"),width =0.5) +
stat_summary(fun ="mean", geom ="point", shape =23, size =3, fill ="white") +
labs(x ="Medalhas", y ="Idade") +
theme_estat()

ggsave("Boxpolt_idade_medals_PF.pdf", width =158, height =93, units ="mm")

# ggplotly() visualizar mais rapidamente algumas observações

quadro_medal_old_2 = medal_old_2 %>%
  group_by(Medal) %>%
  summarize(media = mean(Age),
            desvio_padrao = sd(Age),
            min = min(Age),
            q25 = quantile(Age, probs = 0.25),
            mediana = quantile(Age, probs = 0.5),
            q75 = quantile(Age, probs = 0.75),
            max = max(Age))

view(quadro_medal_old_2)

## 3) ----

#-> Quantidade de pódios por continente

# ler arquivo externo com as correspondências continentais

continent_dataset = read.table("Continentes_r.txt", header=TRUE, sep=",")

head(continent_dataset)

# Substrings 

continent_dataset$Two_Letter_Country_Code = NULL
continent_dataset$Country_Number = NULL
continent_dataset$Country_Name = NULL

# Junção das Americas como apenas um continente

continent_dataset$Continent_Name[continent_dataset$Continent_Name  == 'North America']<- "America"
continent_dataset$Continent_Name[continent_dataset$Continent_Name  == 'South America']<- "America"
continent_dataset$Continent_Code[continent_dataset$Continent_Name  == 'America']<- "AM"

continent_dataset = continent_dataset[-c(125), ]   # remove Na

# Junção de tabelas

count_def = count_def  %>%
left_join(continent_dataset, by = c("NOC" = "Three_Letter_Country_Code")) 

# Coorelações

# A princípio foram tentadas por meio de um laço de repetição
# buscando autotização do processo 

count_def$Continent_Code[count_def$NOC == 'AHO' | count_def$NOC == 'ANT' | count_def$NOC == 'ARU' | count_def$NOC == 'BAR' | count_def$NOC == 'BER' | count_def$NOC == 'BIZ' | count_def$NOC == 'CAY' | count_def$NOC == 'CHI' | count_def$NOC == 'CRC' | count_def$NOC == 'ESA'] = 'AM' 
count_def$Continent_Code[count_def$NOC == 'ALG' | count_def$NOC == 'ANG' | count_def$NOC == 'BOT' | count_def$NOC == 'CGO' | count_def$NOC == 'BUR' | count_def$NOC == 'CHA' | count_def$NOC == 'EGY' | count_def$NOC == 'GAM']  <- 'AF'
count_def$Continent_Code[count_def$NOC == 'ANZ' | count_def$NOC == 'ASA' | count_def$NOC == 'BAH' | count_def$NOC == 'FIJ' | count_def$NOC == 'FSM']  <- 'OC'
count_def$Continent_Code[count_def$NOC == 'BAN' | count_def$NOC == 'BHU' | count_def$NOC == 'BRU' | count_def$NOC == 'CAM'] = 'AS'
count_def$Continent_Code[count_def$NOC == 'BOH' | count_def$NOC == 'BUL' | count_def$NOC == 'CRO' | count_def$NOC == 'CRT' | count_def$NOC == 'DEN' | count_def$NOC == 'ESP' | count_def$NOC == 'EUN' | count_def$NOC == 'FRG' | count_def$NOC == 'GBR'] <-'EU'

# Exportar tabela para visualização e ajustes finais em excel

write_xlsx(count_def,"C:\\Users\\daviz\\Downloads\\Projeto Fantasma ESTAT 2021\\people.xlsx"
)

# ler tabela pós ajustes finais
                         
count_def_NEW =  read_excel(choose.files())   # arquivo "people", se houver a necessidade posso manda-ló

# seleção das colunas necessarias

count_def_NEW = count_def_NEW %>%
  select(NOC  ,Continent_Code)

head(count_def_NEW)

# Junção com o banco de dados

dim(dados)

dados_continents = dados  %>%
  left_join(count_def_NEW, by = c("NOC" = "NOC")) 

dim(dados_continents)

# Limpeza

dados_continents %>%                    # DADOS NA
  select(Name,Medal,Continent_Code,NOC, ) %>%
  filter(Medal != 'NA', is.na(Continent_Code) ) 

# Renomer 

dados_continents$Continent_Code[dados_continents$Continent_Code == 'EU']<- "Europa"
dados_continents$Continent_Code[dados_continents$Continent_Code == 'AM']<- "América"
dados_continents$Continent_Code[dados_continents$Continent_Code == 'AS']<- "Ásia"
dados_continents$Continent_Code[dados_continents$Continent_Code == 'OC']<- "Oceânia"
dados_continents$Continent_Code[dados_continents$Continent_Code == 'AF']<- "Africa"
dados_continents$Continent_Code[dados_continents$Continent_Code == 'IND']<- "Outros"

# Frequências Padronizadas

dados_p_grafic = dados_continents %>%
  select(Name,Medal,Continent_Code ) %>%
  filter(Medal != 'NA') %>%
  count(Continent_Code) %>%
  mutate(prop = n/sum(n),
         freq = prop *100, 
         label = str_c(n," (", format(round(freq,2),nsmall = 2,dec=',')
,"%)")       )

sum(dados_p_grafic$prop) # 100 %

# Gráfico
 
ggplot(dados_p_grafic) +
  aes(x = fct_reorder(Continent_Code, n, .desc=T), y = n,label=label) +
  geom_bar(stat= "identity", fill ="#A11D21", width =0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5,
    size = 3
    )+
  scale_y_continuous(breaks = seq(0,25000,5000))+
  labs(x ="Continentes", y ="Quantidade de pódios") +
  theme_estat()
ggsave('colunas_uni_freq_OLIMP.pdf', width =158, height =93, units ="mm")  
  


## 4) ----

# -> Distribuição dos IMCs por jogos de verão e inverno

# IMC = PESO (Kg)/ ALTURA( m) ^2

# Seleção das variáveis necessárias
# 'Split' da variável Games em Games e Ano
# Adição da variável referente ao IMC
# Considerando NAs p/ gráfico de setores

dados_imc_c_NA = dados %>%
mutate (IMC = Weight / (Height/100)^2 ) %>%
separate(Games,c('Year','Games'),' ')

summary(dados_imc_c_NA$IMC)   # MEDIDAS RESUMO

dados_imc_c_NA %>% count(Games)  # Frequêncas Considerando NA´S

# TRADUÇÃO

dados_imc_c_NA$Games[dados_imc_c_NA$Games == 'Summer']<- "Verão"
dados_imc_c_NA$Games[dados_imc_c_NA$Games == 'Winter']<- "Inverno"


# Seleção das variáveis necessárias
# 'Split' da variável Games em Games e Ano
# Adição da variável referente ao IMC
# Frequêncas NÃO Considerando NA´S

dados_imc_S_NA = dados %>%
  mutate (IMC = Weight / (Height/100)^2 ) %>%
  separate(Games,c('Year','Games'),' ') %>%
  filter(IMC != 'NA') 
  
# TRADUÇÃO

dados_imc_S_NA$Games[dados_imc_S_NA$Games == 'Summer']<- "Verão"
dados_imc_S_NA$Games[dados_imc_S_NA$Games == 'Winter']<- "Inverno"


summary(dados_imc_S_NA$IMC)  # MEDIDAS RESUMO

dados_imc_S_NA %>% count(Games)  # Não Considera NA´S

# Quadro resumo by games 

quadro_dados_imc =  dados_imc_S_NA %>%
  group_by(Games) %>%
  summarize(media = mean(IMC),
            desvio_padrao = sd(IMC),
            min = min(IMC),
            q25 = quantile(IMC, probs = 0.25),
            mediana = quantile(IMC, probs = 0.5),
            q75 = quantile(IMC, probs = 0.75),
            max = max(IMC))


# Frequêcias de acordo com padronização

contagem <-  dados_imc_c_NA %>%group_by(Games) %>%
  summarise(Freq = n()) %>%
  mutate(Prop =round(100*(Freq/sum  (Freq)),2)) %>%
  arrange(desc(Games)) %>%
  mutate(posicao =cumsum(Prop) -0.5*Prop)

# Gráfico 1   # Alterar ordem das cores na padronização 

ggplot(contagem) +
  aes(x =factor(""), y = Prop , fill =factor(Games)) +
  geom_bar(width =1, stat= "identity") +
  coord_polar(theta ="y") +
  geom_text(aes(x =1.8, y = posicao, label =paste0(Prop,"%")),color ="black") +
  theme_void() +
  theme(legend.position ="top") +
  scale_fill_manual(values = cores_estat, name ='Jogos')
ggsave("setor_jogos.pdf", width =158, height =93, units ="mm")


# gráfico 2

ggplot(dados_imc_S_NA) +
  aes(x = Games, y = IMC) +
  geom_boxplot(fill =c("#A11D21"),width =0.5) +
  stat_summary(fun ="mean", geom ="point", shape =23, size =3, fill ="white") +
  labs(x ="Jogos", y ="Índice de Massa Corporal ") +
  theme_estat()
ggsave("box_bi_q_imc.pdf", width =158, height =93, units ="mm")


# Buscar entender, a título de curiosidade, se dentre os presentes outliers
# há um padrão. Se esses são atletas de um mesmo esporte, por exemplo.

ggplotly()

# OBSERVAÇÕES  canto inferior direito

dados_imc_S_NA %>%
select(Sport,IMC,Games) %>%        #  Gymnastics 
filter(Games == 'Verão') %>%
top_n(-10,IMC )             

# OBSERVAÇÕES  canto superior direito

dados_imc_S_NA %>%
  select(Sport,IMC,Games) %>%        # Weightlifting and  Judo 
  filter(Games == 'Verão') %>%
  top_n(9,IMC )  

# OBSERVAÇÕES  canto superior esquerdo

dados_imc_S_NA %>%
  select(Sport,IMC,Games) %>%        # Bobsleigh  
  filter(Games == 'Inverno') %>%
  top_n(9,IMC ) 

# OBSERVAÇÕES  canto inferior esquerdo


dados_imc_S_NA %>%
  select(Sport,IMC,Games) %>%        # Cross Country Skiing  
  filter(Games == 'Inverno') %>%
  top_n(-9,IMC ) 

# funcao interativa permite identificar pontos especificos, nesse caso imprimindo o esporte
boxplot(dados_imc_S_NA$IMC~dados_imc_S_NA$Games, las = 1)

identify(dados_imc_S_NA$IMC, y = dados_imc_S_NA$Games , labels = dados_imc_S_NA$Sport)

## 5) ----

#-> Top 5 de participantes considerando o ganho das medalhas, com pesos diferentes para cada uma


# Considerando o seguintes pesos:

# ouro = 3
# prata = 2
# bronze = 1

# selecão, filtragem e adição de pesos por medelha

medals_s_NA = dados %>%
  select(Name,Medal) %>%
  filter(Medal != 'NA') %>%
  mutate( Peso_medal = ifelse(Medal == 'Gold', 3, ifelse(Medal == "Silver", 2, 1 )))


# Tradução 

medals_s_NA$Medal[medals_s_NA$Medal == 'Gold']<- "Ouro"
medals_s_NA$Medal[medals_s_NA$Medal == 'Silver']<- "Prata"

# tabela com soma

data_sum_peso_medals = data.frame(tapply(medals_s_NA$Peso_medal, medals_s_NA$Name, FUN=sum))

names(data_sum_peso_medals)[names(data_sum_peso_medals)== "tapply.medals_s_NA.Peso_medal..medals_s_NA.Name..FUN...sum."]<- "SOMA"

head(data_sum_peso_medals)

# modifica 1 coluna para rown names

data_sum_peso_medals <- tibble::rownames_to_column(data_sum_peso_medals, "Name")


top_5 = top_n(data_sum_peso_medals,5)

# Gráfico de barras horizontais

ggplot(top_5) +
  aes(x = fct_reorder(Name, SOMA, .desc=F), y = SOMA, label = as.factor(SOMA)) +
  geom_bar(stat= "identity", fill ="#A11D21", width =0.7) +
  geom_text(position = position_dodge(width = .9),vjust = -0.2, hjust= -0.3,size =3) +
  labs(x ="Atletas", y ="Contagem das medalhas com peso") +
  coord_flip()+
  theme_estat()
ggsave("barrashorizon.pdf", width =158, height =93, units ="mm")


### 6) ----

# Países com menor número de participações


sapply(dados,function(x)length(unique(x))) # total de paises


count_by_reg = dados %>%    # participaçoes de todos países # NOC
  count(NOC)

summary(count_by_reg)  # medidas resumo

 

count_dados_noc = dados %>%   # NOC
  count(NOC) %>%         # contabila frequencias
  arrange(n) %>%            # crescente
  top_n(-10,n )   

count_dados_paises = count_dados_noc %>%     # JOIN NOC E PAISES DE ACORDO COM CORRESPONDENCIA
  inner_join(count_def)

count_paises = count_dados_paises %>%   # JUNTAR REGION E NOTES
  unite('region' , region: notes , remove = F)
  
count_paises$notes = NULL

count_paises = count_paises[-c(3), ]   # remove Unknown 


# TRADUÇOES 

count_paises$region[count_paises$region == 'Canada_Newfoundland']<- "Terra Nova e Labrador"
count_paises$region[count_paises$region == 'Malaysia_North Borneo']<- "Norte de Bornéu"
count_paises$region[count_paises$region == 'South Sudan_']<- "Sudão do Sul"
count_paises$region[count_paises$region == 'Yemen_South Yemen']<- "Iémen do Sul"
count_paises$region[count_paises$region == 'NA_Tuvalu']<- "Tuvalu"
count_paises$region[count_paises$region == 'Kosovo_']<- "Kosovo"
count_paises$region[count_paises$region == 'Timor-Leste_']<- "Timor-Leste"
count_paises$region[count_paises$region == 'Brunei_']<- "Brunei"
count_paises$region[count_paises$region == 'Greece_Crete']<- "Creta"
count_paises$region[count_paises$region == 'Kiribati_']<- "Quiribati"

count_paises = count_paises[c(1:10), ]   # selecionar apenas 10


# grafico

ggplot(count_paises) +
  aes(x = fct_reorder(region, n, .desc=F), y = n, label = as.factor(n)) +
  geom_bar(stat= "identity", fill ="#A11D21", width =0.7) +
  geom_text(position = position_dodge(width = .9),vjust = 0.3, hjust= -0.3,size =3) +
  labs(x ="País", y ="Frequência") +
  coord_flip()+
  theme_estat()
ggsave("paises_menos.pdf", width =158, height =93, units ="mm")


