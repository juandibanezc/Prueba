install.packages("NLP")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("SnowballC")
install.packages("tidytext")
install.packages("dplyr")
install.packages("stringr")  # Removing characters
install.packages("tm")         # Text mining cleaning
install.packages("qdapRegex")
install.packages("tidyverse")
library(NLP)
library(RColorBrewer)
library(tm)
library(wordcloud2)
library(SnowballC)
library(qdapRegex)     # Removing URLs 
library(wordcloud2)    # Creating the wordcloud
library(tidytext)
library(dplyr)
library(stringr)
library(tidyverse)
#Lectura de datos

data=readRDS(file = "Ipiales.Rda")

Ciudad=read.csv(file = "Ipiales.csv",sep = ";")


fct_count(Ciudad$Categoria.1)

Ciudad$Categoria.1<- fct_collapse(Ciudad$Categoria.1,deporte=c("deporte","deporte "," deporte"),noticia=c("noticia","noticia "," noticia"),personal=c("personal","personal "," personal"),cultura=c("cultura","cultura "," cultura"),educacion=c("educacion"," educacion","educacion "),migracion=c("migracion","migracion "," migracion"),ocurrencia=c("ocurrencia"," ocurrencia","ocurrencia "),empleo=c("empleo"," empleo","empleo "),opinion=c("opinion"," opinion","opinion "),politica=c("politica"," politica","politica "),salud=c("salud"," salud","salud "),seguridad=c("seguridad"," seguridad","seguridad "),solidaridad=c("solidaridad"," solidaridad","solidaridad "),xenofobia=c("xenofobia","xenofobia "," xenofobia"))

fct_count(Ciudad$Categoria.2)
Ciudad$Categoria.2<- fct_collapse(Ciudad$Categoria.2,deporte=c("deporte","deporte "," deporte"),noticia=c("noticia","noticia "," noticia"),personal=c("personal","personal "," personal"),cultura=c("cultura","cultura "," cultura"),educacion=c("educacion"," educacion","educacion "),migracion=c("migracion","migracion "," migracion"),ocurrencia=c("ocurrencia"," ocurrencia","ocurrencia "),empleo=c("empleo"," empleo","empleo "),opinion=c("opinion"," opinion","opinion "),politica=c("politica"," politica","politica "),salud=c("salud"," salud","salud "),seguridad=c("seguridad"," seguridad","seguridad "),solidaridad=c("solidaridad"," solidaridad","solidaridad "),xenofobia=c("xenofobia","xenofobia "," xenofobia"))
fct_count(Ciudad$Categoria.2)

#Todos los mensajes
AllData=data.frame(n=Ciudad$numero.de.tweet,text=data$text,categoria1=Ciudad$Categoria.1,categoria2=Ciudad$Categoria.2,Sentiment=Ciudad$sentiment)

AllData$Sentiment[AllData$Sentiment==1]="Positivo"
AllData$Sentiment[AllData$Sentiment==0]="Neutral"
AllData$Sentiment[AllData$Sentiment==2]="Mixto"
AllData$Sentiment[AllData$Sentiment==-1]="Negativo"
#Mensajes sin noticias
dataSinNoticia=AllData[!(AllData$categoria1=="noticia" | AllData$categoria2=="noticia"),]


#Mensajes correspondientes a noticias
dataNoticia=AllData[(AllData$categoria1=="noticia" | AllData$categoria2=="noticia"),]


#Nube con todos los mensajes

TCiudad<-str_c(AllData$text, collapse = "")


TCiudad <- 
  TCiudad %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("spanish")) %>%   # Remove common words (a, the, it etc.)
  removeWords(stopwords("english")) %>%
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("jaja","jajaja","..."," ....","venezolana","venezolano","venezolanos,","venezolanos","venezolanas","venezuela","venezuela","venezuela,","venezolanos ")) 

Nc=20

CCiudad=Corpus(VectorSource(TCiudad))

tdmCiudad=TermDocumentMatrix(CCiudad)
MCiudad=as.matrix(tdmCiudad)
VCiudad=sort(rowSums(MCiudad),decreasing=TRUE)
VCiudad=VCiudad[-1:-4]
ACiudad=data.frame(word=names(VCiudad),freq=VCiudad)
SCiudad = max((ACiudad[,2])/Nc)
ACiudad <- filter(ACiudad, freq > SCiudad)

IMA.ALL=ACiudad
IMA.ALL=IMA.ALL[1:300,]
IMA.ALL=IMA.ALL[c(-5,-16),]
wordcloud2(IMA.ALL, size=0.3,shape = 'star')

#Nube Sin Noticias

TCiudad<-str_c(dataSinNoticia$text, collapse = "")


TCiudad <- 
  TCiudad %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("spanish")) %>%   # Remove common words (a, the, it etc.)
  removeWords(stopwords("english")) %>%
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("jaja","jajaja","..."," ....","venezolana","venezolano","venezolanos,","venezolanos","venezolanas","venezuela","venezuela","venezuela,","venezolanos ","medellín")) 


CCiudad=Corpus(VectorSource(TCiudad))

tdmCiudad=TermDocumentMatrix(CCiudad)
MCiudad=as.matrix(tdmCiudad)
VCiudad=sort(rowSums(MCiudad),decreasing=TRUE)
VCCiudad=VCiudad[-1:-4]
ACiudad=data.frame(word=names(VCiudad),freq=VCiudad)
SCiudad = max((ACiudad[,2])/Nc)
ACiudad <- filter(ACiudad, freq > SCiudad)

IMA.SN=ACiudad
IMA.SN=IMA.SN[c(-2,-4,-204),]
wordcloud2(IMA.SN, size=0.5,shape = 'star')

#Extra, nube de solo noticias

TCiudad<-str_c(dataNoticia$text, collapse = "")


TCiudad <- 
  TCiudad %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("spanish")) %>%   # Remove common words (a, the, it etc.)
  removeWords(stopwords("english")) %>%
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("jaja","jajaja","..."," ....","venezolana","venezolano","venezolanos,","venezolanos","venezolanas","venezuela","venezuela","venezuela,","venezolanos "," venezolanos"))  


CCiudad=Corpus(VectorSource(TCiudad))

tdmCiudad=TermDocumentMatrix(CCiudad)
MCiudad=as.matrix(tdmCiudad)
VCiudad=sort(rowSums(MCiudad),decreasing=TRUE)
VCiudad=VCiudad[-1:-4]
ACiudad=data.frame(word=names(VCiudad),freq=VCiudad)
SCiudad = max((ACiudad[,2])/Nc)
ACiudad <- filter(ACiudad, freq > SCiudad)

IMA.CN=ACiudad
IMA.CN=IMA.CN[c(-1,-4,-11),]
wordcloud2(IMA.CN, size=0.5,shape = 'star')

#Tablas de frecuencia
#Temas mas hablados sobre los venezolanos en general

#Sin noticia
a1=data.frame(Categoria=dataSinNoticia$categoria1,Sentiment=dataSinNoticia$Sentiment)
b1=data.frame(Categoria=dataSinNoticia$categoria2,Sentiment=dataSinNoticia$Sentiment)
c1=rbind(a1,b1)
c1 = arrange(c1,Categoria)
c1 <- filter(c1, Categoria != '')
c1 <- filter(c1,Categoria == 'empleo'|Categoria == 'ocurrencia'|Categoria == 'opinion'|Categoria == 'personal'|Categoria == 'politica'|Categoria == 'salud'|Categoria == 'seguridad'| Categoria == 'xenofobia'| Categoria == 'migracion'| Categoria =='educacion'| Categoria =='solidaridad'| Categoria =='deporte' | Categoria =='cultura' | Categoria =='educacion '| Categoria =='migracion ')

#Re ordenando Niveles
c1$Sentiment=factor(c1$Sentiment, ordered = TRUE, levels = c("Negativo","Neutral","Positivo","Mixto"))
#Re ordenando categorias por frecuencia
c1$Categoria <- reorder(c1$Categoria, X=as.numeric(c1$Categoria), FUN=length)
#Grafica: Temas mas tocados sin noticias
attach(c1)
ggplot(c1)+
  geom_bar(mapping=aes(Categoria,fill=Sentiment))+
  coord_flip()

#con noticia
a2=data.frame(Categoria=dataNoticia$categoria1,Sentiment=dataNoticia$Sentiment)
b2=data.frame(Categoria=dataNoticia$categoria2,Sentiment=dataNoticia$Sentiment)
c2=rbind(a2,b2)
c2 = arrange(c2,Categoria)
c2 <- filter(c2, Categoria != '')
c2 <- filter(c2,Categoria == 'empleo'|Categoria == 'ocurrencia'|Categoria == 'opinion'|Categoria == 'personal'|Categoria == 'politica'|Categoria == 'salud'|Categoria == 'seguridad'| Categoria == 'xenofobia'| Categoria == 'migracion'| Categoria =='educacion'| Categoria =='solidaridad'| Categoria =='deporte' | Categoria =='cultura' | Categoria =='educacion '| Categoria =='migracion ')

#Re ordenando Niveles
c2$Sentiment=factor(c2$Sentiment, ordered = TRUE, levels = c("Negativo","Neutral","Positivo","Mixto"))
#Re ordenando categorias por frecuencia
c2$Categoria <- reorder(c2$Categoria, X=as.numeric(c2$Categoria), FUN=length)

#Grafica: Temas mas tocados sobre venezolanos en noticias.
attach(c2)
ggplot(c2)+
  geom_bar(mapping=aes(Categoria,fill=Sentiment))+
  coord_flip()

a3=data.frame(Categoria=AllData$categoria1,Sentiment=AllData$Sentiment)
b3=data.frame(Categoria=AllData$categoria2,Sentiment=AllData$Sentiment)
c3=rbind(a3,b3)
c3 = arrange(c3,Categoria)
c3 <- filter(c3, Categoria != '')
c3 <- filter(c3,Categoria == 'empleo'|Categoria == 'ocurrencia'|Categoria == 'opinion'|Categoria == 'personal'|Categoria == 'politica'|Categoria == 'salud'|Categoria == 'seguridad'| Categoria == 'xenofobia'| Categoria == 'migracion'| Categoria =='educacion'| Categoria =='solidaridad'| Categoria =='deporte' | Categoria =='cultura' | Categoria =='educacion '| Categoria =='migracion ')

#Re ordenando Niveles
c3$Sentiment=factor(c3$Sentiment, ordered = TRUE, levels = c("Negativo","Neutral","Positivo","Mixto"))
#Re ordenando categorias por frecuencia
c3$Categoria <- reorder(c3$Categoria, X=as.numeric(c3$Categoria), FUN=length)

#Grafica: Temas mas tocados en general
attach(c3)
ggplot(c3)+
  geom_bar(mapping=aes(Categoria,fill=Sentiment))+
  coord_flip()

