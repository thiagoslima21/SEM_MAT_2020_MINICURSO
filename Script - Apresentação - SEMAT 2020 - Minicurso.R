library(readxl)
library(stringr)
library(lattice)
library(ggplot2)
library(dplyr)
library(gridExtra)



# Instalacao do Pacote -------------------------------------------------------------------------


install.packages("CFilt")


# Importar Banco de Dados -------------------------------------------------------------------------


CFilt_base <- read_excel("CFilt (respostas).xlsx", 
                               col_types = c("skip", "text", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric"))


###### CONSTRUINDO O DATA FRAME

## O Pacote aceita dois tipos de base de dados:

#1. A Matriz de Utilidade. (Com Linhas e Colunas nomeadas)
#2. Um dataframe com 3 colunas: Usuario, Item e Avaliacao. 
#Ou seja, cada linha é referente a uma avaliaçao realizada.

#Iremos aplicar uma função que reestrutura a base de dados disponibilizada 
#pelo Formulário Google para a forma de dataframe utilizável.

#Antes:

View(CFilt_base)

# Função:
estruturar<-function(dados){
  n_usuarios= nrow(dados)
  n_filmes= ncol(dados) - 1
  for(i in 1:n_filmes){
    colnames(dados)[i+1]<-str_sub(colnames(dados)[i+1],start=2,end=-2)
  }
  new_base<-data.frame(Id_Users=NA,Id_Items=NA,Ratings=NA)
  k=1
  for(i in 1:n_usuarios){
    for(j in 1:n_filmes){
      if(!is.na(dados[i,j+1])){
        new_base[k,1]<-as.character(dados[i,1])
        new_base[k,2]<-as.character(colnames(dados)[j+1])
        new_base[k,3]<-as.numeric(dados[i,j+1])
        k=k+1
      }
    }
  }
  return(new_base)
}

# Aplicação da Função:

CFilt_est<-estruturar(CFilt_base)

# Depois:
View(CFilt_est)


############################# PACOTE CFILT #################################################


# Construção de Objeto de Filtragem Colaborativa:

# sim_user:
# "cos", "pearson", "both" ou "none".
# sim_item:
# "cos", "adjcos", "both" ou "none".

library(CFilt)
SEMAT<-CFbuilder(CFilt_est, sim_user = "both", sim_item = "both")


# Calcularemos as 4 similaridades.

# Objeto CF ---------------------------------------------------------------

class(SEMAT)
View(SEMAT)

##################################### ANALISE DESCRITIVA #################################

n_us = nrow(SEMAT$MU) #Numero de Usuarios
n_fil= ncol(SEMAT$MU) #Numero de Filmes

# Matrizes ----------------------------------------------------------------


# Matriz de Utilidade -----------------------------------------------------

View(SEMAT$MU)
levelplot(t(SEMAT$MU[1:30,1:30][c(nrow(SEMAT$MU[1:30,1:30]):1),]),xlab="Movies",
          ylab="Users",col.regions=topo.colors(100),main="Matriz de Utilidade")


View(SEMAT$SU2)
##### Vetores de Medias ---------------------------------------------------


# Medias dos Usuários: averages_u -----------------------------------------


# Histograma:

hist(SEMAT$averages_u, main="Histograma Médias dos Usuarios",col="blue",
     ylab="Frequência", xlab="Medias")

#Graficos de Barras:
d1 = data.frame(filmes = names(sort(SEMAT$averages_u,decreasing = T)),
                medias = sort(SEMAT$averages_u,decreasing = T))
t_1 <- order(d1$medias, decreasing = FALSE)
d1$filmes <- factor(d1$filmes, levels=d1$filmes[t_1], ordered = T)


# Os 30 usuários mais otimistas
ggplot(d1[1:30,], aes(x=filmes,y=medias))  + geom_bar(stat= "identity",fill="blue",
  color="black")+ coord_flip() +xlab("Usuarios")+ylab("Médias")+
  ggtitle("Usuários mais Otimistas")+scale_y_continuous(limits=c(0,5), breaks=seq(0,5,.5))+
  geom_text(aes(label=round(medias,2) ), vjust = 0.4, hjust = -0.1)+
  theme_test()

# Os 10 Usuários mais Pessimistas 
ggplot(data=d1[(n_us-9):(n_us),],aes(x=filmes,y=medias)) + geom_bar(stat= "identity", fill="red", 
  color="black") + coord_flip() + ggtitle("Usuários mais Pessimistas") + scale_y_continuous(limits=c(0,5),
  breaks=seq(0,5,.5)) + geom_text(aes(label=round(medias,2)), vjust = 0.4,
  hjust = -0.1)+  theme_test() + xlab("Filmes")+ylab("Médias")



# Vetores das Médias dos Filmes:  averages_i ------------------------------


#Histograma:

hist(SEMAT$averages_i, main="Histograma Médias dos Filmes",
     col="yellow", ylab="Frequência",xlab="Médias", )


#Graficos de Barras

#Ordenando...
d2 = data.frame(filmes = names(sort(SEMAT$averages_i,decreasing = T)),
                medias = sort(SEMAT$averages_i,decreasing = T))
t_2 <- order(d2$medias, decreasing = FALSE)
d2$filmes <- factor(d2$filmes, levels=d2$filmes[t_2], ordered = T)

# Os 30 filmes mais bem avaliados
ggplot(data=d2[1:30,],aes(x=filmes,y=medias)) + geom_bar(stat= "identity",fill="blue", 
    color="black") + coord_flip() + ggtitle("TOP 30 - Filmes") + scale_y_continuous(limits=c(0,5),
      breaks=seq(0,5,.5)) + geom_text(aes(label=round(medias,2)), vjust = 0.4, 
      hjust = -0.1)+  theme_test() + xlab("Filmes")+ylab("Médias")

# Os 10 filmes mais mal avaliados

ggplot(data=d2[70:80,],aes(x=filmes,y=medias)) + geom_bar(stat= "identity", fill="red", 
      color="black") + coord_flip() + ggtitle("10 Piores Filmes") + scale_y_continuous(limits=c(0,
      5),breaks=seq(0,5,.5)) + geom_text(aes(label=round(medias,2)), vjust = 0.4,
      hjust = -0.1)+  theme_test() + xlab("Filmes")+ylab("Médias")


############## Vetores de Frequencias #################################

# Numero de Avaliacoes dos Usuarios:  n_aval_u ----------------------------

#Histograma:

hist(SEMAT$n_aval_u, col="green", main="Histograma - Frequência de Avaliacoes Realizadas", 
     xlab="Numero de Avaliacoes",ylab="Frequencia")

#Graficos de Barras:

# Ordenando...

d3 = data.frame(filmes = names(sort(SEMAT$n_aval_u,decreasing = T)),
                n_aval = sort(SEMAT$n_aval_u,decreasing = T))
t_3 <- order(d3$n_aval, decreasing = FALSE)
d3$filmes <- factor(d3$filmes, levels=d3$filmes[t_3], ordered = T)


# Os 30 mais cinéfilos:
ggplot(d3[1:30,],aes(x=filmes,y=n_aval))  + geom_bar(stat= "identity",fill="blue",
  color="black")+ coord_flip() +xlab("Filmes")+ylab("N de Avaliações")+
  ggtitle("TOP 30 - Os mais Cinéfilos")+scale_y_continuous(limits=c(0,80), breaks=seq(0,80,10))+
  geom_text(aes(label=round(n_aval,2) ), vjust = 0.4, hjust = -0.1)+
  theme_test()

# Os 10 menos cinéfilos:

ggplot(data=d3[(n_us-9):(n_us),], aes(x=filmes,y=n_aval)) + geom_bar(stat= "identity",fill="red",
    color="black")+ coord_flip()+ggtitle("TOP 10 - Os menos Cinéfilos ")+scale_y_continuous(limits=c(0,
    n_fil), breaks=seq(0,n_fil,10))+ geom_text(aes(label=n_aval ), vjust = 0.4,
    hjust = -0.1) + theme_test() + xlab("Usuários") + ylab("N de Avaliacoes")

# Numero de Avaliacoes dos Itens:  n_aval_i ------------------------------------------------


# Histograma:
hist(SEMAT$n_aval_i, col="cyan", main="Histograma - Frequência de Avaliações Recebidas", 
     xlab ="Numero de Avaliações ",ylab="Frequência")

#Graficos de Barras:

# Ordenando...
d4=data.frame(filmes = names(sort(SEMAT$n_aval_i,decreasing = T)),
              n_aval = sort(SEMAT$n_aval_i,decreasing = T))
t_4 <- order(d4$n_aval, decreasing = FALSE)
d4$filmes <- factor(d4$filmes, levels=d4$filmes[t_4], ordered = T)


# Gráfico com os 50 filmes mais populares

ggplot(data=d4[1:30,], aes(x=filmes,y=n_aval)) + geom_bar(stat= "identity",fill="blue",
  color="black")+ coord_flip() +ggtitle("Numero de Avaliacoes Recebidas")+scale_y_continuous(limits=c(0,n_us), 
  breaks=seq(0,n_us,10))+ geom_text(aes(label=round(n_aval,2) ), vjust = 0.4,
  hjust = -0.1)+theme_test()+xlab("Filmes")+ylab("N de Avaliacoes")


#Gráfico com os 10 filmes menos populares

ggplot(data=d4[70:80,], aes(x=filmes,y=n_aval)) + geom_bar(stat= "identity",fill="red",
    color="black")+ coord_flip()+ggtitle("Numero de Avaliacoes Recebidas")+scale_y_continuous(limits=c(0,
    n_us), breaks=seq(0,n_us,10))+ geom_text(aes(label=round(n_aval,2) ), vjust = 0.4, 
    hjust = -0.1)+theme_test() + xlab("Filmes")+ylab("N de Avaliacoes")


################################## RECOMENDACOES ##########################################################

#Recomendacoes:


# $estimaterating : Estimação das Notas das Avaliações -----------------------------------

## Esse método estima as avaliações tanto não realizadas como também 
# já realizadas (para ser usado para comparações) 

# Selecionar Participante:

(usuario_escolhido = rownames(SEMAT$MU)[10])

#Escolher um item que o usuario ja viu para fazermos comparações:

filmes_assistidos = names(which(!is.na(SEMAT$MU[10,])))
View(filmes_assistidos)
(filme_escolhido = filmes_assistidos[4])


# Nota Real:

#Acesso a nota na Matriz de Utilidade: 
SEMAT$MU["Thiago Lima","1917 (2020)"]


#Acessar a nota do usuário escolhido ao filme escolhido:
(nr = SEMAT$MU[usuario_escolhido,filme_escolhido])


#Prever a Nota do Participante para o Filme escolhido:
(n1=SEMAT$estimaterating(Id_u=usuario_escolhido,Id_i=filme_escolhido,
                      type = "user",neighbors = 5, similarity = "cos"))
(n2=SEMAT$estimaterating(Id_u=usuario_escolhido,Id_i=filme_escolhido,
                      type = "user",neighbors = 5, similarity = "pearson"))
(n3=SEMAT$estimaterating(Id_u=usuario_escolhido,Id_i=filme_escolhido,
                      type = "item",neighbors = 5, similarity = "cos"))
(n4=SEMAT$estimaterating(Id_u=usuario_escolhido,Id_i=filme_escolhido,
                      type = "item",neighbors = 5, similarity = "adjcos"))


#Data Frame com as comparacoes

dif = data.frame(type=c("user","user","item","item"),similarity=c("cos","pearson",
                "cos","adjcos"), Real=rep(nr,4), Estimado = c(n1,n2,n3,n4),
                diferenca = c(n1,n2,n3,n4) - nr)
View(dif)


# $recommend : decide se um filme é recomendável ou não -------------------


# Verificar se deve recomendar o filme selecionado para o usuario selecionado:

(rec_1=SEMAT$recommend(Id_u=usuario_escolhido,Id_i=filme_escolhido,type = "user", 
                       similarity= "cos",cuts = 3.5))
(rec_2=SEMAT$recommend(Id_u=usuario_escolhido,Id_i=filme_escolhido,type = "user", 
                       similarity= "pearson",cuts = 3.5))
(rec_3=SEMAT$recommend(Id_u=usuario_escolhido,Id_i=filme_escolhido,type = "item",
                       similarity= "cos",cuts = 3.5))
(rec_4=SEMAT$recommend(Id_u=usuario_escolhido,Id_i=filme_escolhido,type = "item",
                       similarity= "adjcos",cuts = 3.5))

#Nota-se que o processo é só verificar se a nota calculada é maior que "cuts" para realizar ou não a recomendacao.

dif$recomenda = c(rec_1,rec_2,rec_3,rec_4)
View(dif)


################ LISTAS DE RECOMENDACOES ############################################3


# $kclosestitems: os k itens mais similares ----------------------------------------


# Verificar quais 5 filmes sao considerados mais semelhantes ao filme "Shazam":

SEMAT$kclosestitems(Id_i="Shazam (2019)",k=5, similarity="cos")
SEMAT$kclosestitems(Id_i="Shazam (2019)",k=5, similarity="adjcos")


# $tokusers: os k usuários mais prováveis a gostar do item ----------------


# Verificar quais os 5 usuarios sao os mais provaveis a gostar do filme "Parasita":

SEMAT$topkusers(Id_i="Parasita (2019)", k=5, type="item", similarity = "adjcos")
SEMAT$topkusers(Id_i="Parasita (2019)", k=5, type="user", similarity = "pearson")


# $topkusers: os k itens que devem ser recomendados a um usuário ----------

# Verificar quais 6 filmes devem ser recomendados para alguns usuarios cadastrados:

(usuario2_escolhido = rownames(SEMAT$MU)[20])

SEMAT$topkitems(Id_u=usuario2_escolhido, type="user", k=6, similarity = "pearson",
                cuts=3.5, neighbors = 5)

SEMAT$topkitems(Id_u=usuario2_escolhido, type="item",k=6, similarity = "adjcos",
                cuts=3.5, neighbors = 5)


############################ Recomendacao Final para todos os usuarios!! ###############################################

#Em forma de Lista
recomendacoes<-list()

for(i in 1:n_us){
  recomendacoes[[i]]=c(rownames(SEMAT$MU)[i],
                       SEMAT$topkitems(Id_u=rownames(SEMAT$MU)[i],
                                        k=3,type="user", similarity = "pearson"))
}

#Em forma de Data Frame/Tabela:
tab_rec<-data.frame(Usuario=NA,Filme_1=NA,Filme_2=NA,Filme_3=NA)
for(i in 1:n_us){
  if(length(recomendacoes[[i]])>=1){tab_rec[i,1]=recomendacoes[[i]][[1]]}
  if(length(recomendacoes[[i]])>=2){tab_rec[i,2]=recomendacoes[[i]][[2]]}
  if(length(recomendacoes[[i]])>=3){tab_rec[i,3]=recomendacoes[[i]][[3]]}
  if(length(recomendacoes[[i]])>=4){tab_rec[i,4]=recomendacoes[[i]][[4]]}
}

#Visualizacao:
DT::datatable(tab_rec)

#Salvando tabela:
write.csv2(tab_rec,file="tab_rec.csv")


#Link para o acesso da tabela com as avaliacoes, dos dados tranformados, do script e da tabela em .csv
#para consultar as recomendacoes realizadas.

