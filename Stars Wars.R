library(httr)
library(tidyverse)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(grid)
library(gridExtra)

#api
swfilms <- GET("https://swapi.dev/api/films")
filmscontent <- httr::content(swfilms)

char=map(filmscontent$results[[1]]$characters,toString)%>%map(GET)%>%map(content)
name=char%>%map_chr("name")
height=char%>%map("height")%>%as.numeric()
weight=char%>%map("mass")%>%as.numeric()
datos=data.frame(height=height,weight=weight,name=name)%>%na.omit()
row.names(datos)=datos$name

plot1=ggplot(data=datos,aes(height,weight))+geom_jitter(color="blue")+
geom_text_repel(aes(label=name))+ggtitle("Gráfico de dispersión",subtitle = "Star Wars")+
xlab("Altura")+ylab("Peso")

kmean=kmeans(x=datos[,1:2],centers = 3,nstart = 20)
clusteres=kmean$cluster

pca=PCA(datos[,1:2],scale.unit = TRUE)
pcaplot=fviz_pca_ind(pca,geom.ind = "point",col.ind=as.factor(clusteres))+geom_text_repel(aes(label=datos$name))

grid.arrange(plot1, pcaplot,
             ncol = 2)

select(datos,weight,height)

#base de datos
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
head(con)
copy_to(con,datos,name="Personajes")

personajes=tbl(con,"Personajes")

Planeta=char%>%map("homeworld")%>%map(GET)%>%map(content)%>%map_chr("name")

personajes=as_tibble(personajes)

personajes%>%select(name,weight)%>%filter(weight>96)
personajes=mutate(personajes,Planetas=Planeta[1:16])

ggplot(data = personajes,aes(weight,height,label=name))+geom_jitter(aes(colour=factor(Planetas)))+
geom_text_repel()+xlab("Peso en Kilogramos")+ylab("Altura en Centímetros")

