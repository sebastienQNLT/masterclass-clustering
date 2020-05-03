options(scipen=999)
options(stringsAsFactors  =FALSE)

library(dplyr)
library(factoextra)
library(FactoMineR)
library(caret)
library(skimr)
library(tidyr)
library(cluster)

#data selection
accidents<-read.csv2(file="./data/caracteristiques-2018.csv",sep=",")
accidents_for_clustering<-accidents %>% 
  select(lum,agg,int,atm,col) %>% mutate_all(as.character)

# documentation ---- 
# agg
# Localisation :
#   1 – Hors agglomération
# 2 – En agglomération
# 
# int
# Intersection :
#   1 – Hors intersection
# 2 – Intersection en X
# 3 – Intersection en T
# 4 – Intersection en Y
# 5 - Intersection à plus de 4 branches
# 6 - Giratoire
# 7 - Place
# 8 – Passage à niveau
# 9 – Autre intersection
# atm
# Conditions atmosphériques :
#   1 – Normale
# 2 – Pluie légère
# 3 – Pluie forte
# 4 – Neige - grêle
# 5 – Brouillard - fumée
# 6 – Vent fort - tempête
# 7 – Temps éblouissant
# 8 – Temps couvert
# 9 – Autre
# 5
# col
# Type de collision :
#   1 – Deux véhicules - frontale
# 2 – Deux véhicules – par l’arrière
# 3 – Deux véhicules – par le coté
# 4 – Trois véhicules et plus – en chaîne
# 5 – Trois véhicules et plus - collisions multiples
# 6 – Autre collision
# 7 – Sans collision





#data prep (one hot encoding) ----
dummy.model <- dummyVars(" ~ .", data = accidents_for_clustering)
accidents_for_clustering.OHE <- data.frame(predict(dummy.model, newdata = accidents_for_clustering))
#pas besoin de standardiser les données dans ce cas, uniquement des 0 et 1

#on supprime les NA's (attention pas une stratégie forcéement viable pour le projet)
skimr::skim(accidents_for_clustering.OHE)
data.clustering<-accidents_for_clustering.OHE %>% 
  drop_na()
skimr::skim(data.clustering)

#PCA ----
res.pca <- FactoMineR::PCA(data.clustering,  graph = FALSE,ncp=10)

#scree plot
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# graph des contributions des  variables
# Contributions of variables to PCx
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

#visualisation des axes 1 et 2 en limitant aux 10 variables qui contribuent le plus
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             select.var=list(contrib=10)
)


#kmeans ----
# on utilise les résultats de l'ACP sur les 10 premiers axes, cf ncp=10
#on teste 5 clusters
data<-as.data.frame(res.pca$ind$coord)
clusters <- kmeans(data, 4, nstart = 100)
clusters$tot.withinss  # variance intra
clusters$size          # taille des clusters

#on ajoute le clustering à l'ACP
data.clustered <-data %>%
  mutate(Cluster = clusters$cluster)
data.for.graph<-data.clustered %>% select(Dim.1,Dim.2,Dim.3,Cluster)
library(ggplot2)
data.for.graph %>% sample_n(1000) %>% ggplot(aes(x=Dim.1,y=Dim.2,color=as.factor(Cluster)))+geom_point()
data.for.graph %>% sample_n(1000) %>% ggplot(aes(x=Dim.1,y=Dim.3,color=as.factor(Cluster)))+geom_point()
