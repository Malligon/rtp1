# Mickaël ALLIGON le 25/09/18 TP1

#####################################################################################################################################################
#Définition de l'espace de travail
#####################################################################################################################################################

setwd("E:/R/TP1") # Répertoire de travail
x<-read.table("fev.txt",header=T) # Importation des données

#####################################################################################################################################################
# Vérification de l'importation
#####################################################################################################################################################

dim(x) # 654 Individus pour 6 variables
summary(x) # Pas de données manquantes

# Affichage de l'importation

head(x) # Sort les 6 premières lignes
x[1:10,] # Sort les 10 première lignes

str(x) # Nombre d'observations et de variables

#####################################################################################################################################################
# Analyse descriptive univariée
#####################################################################################################################################################

# Etude de la fev des patients

round(mean(x$FEV),2) # Sort la moyenne de la variable FEV arrondie à 2 décimales : 2.64L

attach(x) # Permet à R de reconnaitre les variables du fichier
round(mean(FEV),2) # Sort la moyenne de la variable FEV arrondie à 2 décimales : 2.64L

#---------------------------
# Etude de l'âge des enfants
#---------------------------

round (mean(Age),2) # Sort la moyenne de la variable Age arrondie à 2 décimales : 9.93 ans
round(sd(Age),2) # Sort l'écart type de l'âge des enfants : 2.95ans
quantile(Age,0.25) # 1er quantile : 8 ans
quantile(Age,0.5) # Médiane : 10 ans
quantile(Age,0.75) # 3ème quartile : 12 ans
min(Age) # Age minimum : 3 ans
max(Age) # Age maximum : 19 ans

# La base de données fev est constituée de 654 enfants agés de 3 à 19 ans. L'âge moyen de l'échantillon est de 10 ans
# (9.93) et l'âge médian est de 10 ans également. 1/4 des enfants ont un âge inférieur ou égal à 8 ans, et 3/4 des enfants
# ont au plus 12 ans.

#---------------------------------
#Représentation graphique de l'âge
#---------------------------------

# Représentation graphique de l'âge des enfants

hist(Age, main="Répartition des âges",xlab="Age en années",ylab="Nombre d'enfats",freq=F) # Histogramme de la répartition de l'âge des enfants. freq=F : proportion
# Distribution relativement unimodale (mode : 8-10ans) et symétrique avec une queue de distribution à droite.

boxplot(Age,main="Répartition des âges",ylab="Age en années") # Boîte à moustache de la répartition de l'âge des enfants.

resultats <- boxplot(Age,main="Répartition des âges",ylab="Age en années")
valext <- resultats$out # Stock les valeurs aberrantes de la boîte à moustache
valext # Les affiche

agetries <- sort(Age,decreasing=T) # Trie les ages par ordre décroissant
head(agetries) # Permet de vérifier si les valeurs extrèmes sont vraiment aberrantes
agetries[1:10] # Permet de vérifier si les valeurs extrèmes sont vraiment aberrantes


# Concentration très grande entre 8 et 12 ans, avec un étalement plutôt conséquent aux pôles (valeurs extrêmes mais non aberrantes : 3 enfants de 19ans).
# Les quartiles sont symétriques autour de la moyenne.

#------------------------------
#Etude de la taille des enfants
#------------------------------

taille <- Height*2.54 # Conversion de la taille en cm

round(mean(taille),2) # Moyenne de la taille des enfants : 155.3cm
round(sd(taille),2) # Ecart-type de la taille des enfants : 14.49cm
round(quantile(taille,0.25),2) # 1er quartile de la taille des enfants : 144.78cm
round(quantile(taille,0.5),2) # Médiane de la taille des enfants : 156.21cm
round(quantile(taille,0.75),2) # 3ème quartile de la taille des enfants : 166.37cm
min(taille) # Taille minimale des enfants : 116.84cm
max(taille) # Taille maximale des enfants : 187.96cm
# La base de données fev est constituée de 654 enfants d'une taille allant de 116.84cm à 187.96cm. La taille moyenne de l'échantillon est de 155.3cm
# et la taille médiane est de 156.21cm. 1/4 des enfants ont une taille inférieure ou égale 144.78cm, et 3/4 des enfants
# ont au plus 166.37cm.

#-------------------------------------
#Représentation graphique de la taille
#-------------------------------------

hist(taille, main="Répartition de la taille",xlab="Taille en cm",ylab="Effectifs",freq=T) # Histogramme de la répartition de la taille des enfants
# Distribution relativement bimodale (modes : 150-155cm et 160-165cm) et symétrique avec une queue de distribution à gauche
# et une légère excroissance entre 130 et 135cm. Présence d'une grande diminution inattendue de l'effectif des enfants entre 155 et 160cm.

boxplot(taille,main="Répartition de la taille",ylab="taille en cm") # Boîte à moustache de la répartition de la taille des enfants
# Concentration autour de la médiane très grande entre 145 et 165cm, pas de valeurs aberrantes. Les quartiles ne sont pas symétriques autour de la médiane,
#  plus grande concentration entre la médiane et le 3ème quartile. Ecart-type élevé mais attendu vu la différence d'âge des enfants.

#---------------------------
#Etude de la fev des enfants
#---------------------------

round(mean(FEV),2) # Moyenne de la fev des enfants : 2.64L
round(sd(FEV),2) # Ecart-type de la fev des enfants : 0.87L
round(quantile(FEV,0.25),2) # 1er quartile de la fev des enfants : 1.98L
round(quantile(FEV,0.5),2) # Médiane de la fev des enfants : 2.55L
round(quantile(FEV,0.75),2) # 3ème quartile de la fev des enfants : 3.12L
min(FEV) # Fev minimale des enfants : 0.791L
max(FEV) # Fev maximale des enfants : 5.793L
# La base de données fev est constituée de 654 enfants d'une fev allant de 0.791L à 5.793L. La fev moyenne de l'échantillon est de 2.64L
# et la fev médiane est de 2.55L. 1/4 des enfants ont une fev inférieure ou égale 1.98L, et 3/4 des enfants ont au plus 3.12L.

#----------------------------------
#Représentation graphique de la fev
#----------------------------------

hist(FEV, main="Répartition de la fev en litres",xlab="fev en litres",ylab="effectifs",freq=T) # Histogramme de la répartition de la fev
# Distribution relativement unimodale (modes : 2L-2.5L) et asymétrique avec une queue de distribution à droite.


boxplot(FEV,main="Répartition de la fev en litres",ylab="fev") # Boîte à moustache de la répartition de la fev

resultats2 <- boxplot(FEV,main="Répartition de la fev en litres",ylab="fev en litres")
valext2 <- resultats2$out # Stock les valeurs aberrantes de la boîte à moustache
valext2 # Affiche les valeurs aberrantes : 4.842L 5.224L 4.877L 5.083L 5.102L 5.793L 5.633L 5.638L 4.872L

fevtries <- sort(FEV,decreasing=T) # Trie les fev par ordre décroissant
head(fevtries) # Permet de vérifier si les valeurs extrèmes sont vraiment aberrantes
fevtries[1:10] # Permet de vérifier si les valeurs extrèmes sont vraiment aberrantes


# Concentration très grande autours de la médiane, avec un étalement plutôt conséquent aux pôles 
# (valeurs extrèmes mais non aberrantes : 4.842L 5.224L 4.877L 5.083L 5.102L 5.793L 5.633L 5.638L 4.872L). 
# Les quartiles sont symétriques autour de la moyenne.

#-------------------------
#Etude du sexe des enfants
#-------------------------

summary(Sex) # Nombre de filles et de garçons : F=318 M=336
table(Sex) # Nombre de filles et de garçons : F=318 M=336
prop.table(table(Sex)) # proportion de filles et de garçons : F=0.486 M=0.514
# Répartition du genre des enfants sensiblement égale.

#--------------------------------------------
#Représentation graphique du sexe des enfants
#--------------------------------------------

barplot(prop.table(table(Sex)),main="Répartition du sexe des enfants",xlab="Genre",ylab="Effectifs", col=c("pink","skyblue")) # Histogramme du genre des enfants
freq_sex <- as.numeric(table(Sex)) # Changement du type afin d'avoir une fréquence
text(x = barplot(main="Répartition du sexe des enfants",freq_sex,ylim=c(0,400),xlab="Genre",ylab="Effectifs", col=c("pink","skyblue"), names.arg=c("Femme","Homme")),y=freq_sex, label=freq_sex, pos = 3,cex = 0.8, col ="red") # Permet de rajouter les effectifs en rouge sur les colonnes
sexpie <- paste(round(prop.table(table(Sex)),4)*100,"%")
pie(prop.table(table(Sex)),main="Répartition du sexe des enfants",label=sexpie, col=c("pink","skyblue")) # Camembert des proportions garçon/fille"
legend("topright", c("Femme","Homme"),cex=0.8,fill=c("pink","skyblue"))

# Il y a légèrement plus de garçons que de filles. 


#-------------------------
#Etude fumeurs/non-Fumeurs
#-------------------------

summary(Smoker) # Nombre de fumeurs et non-fumeurs : F=65 NF=589
table(Smoker) # Nombre de fumeurs et non-fumeurs : F=65 NF=589
prop.table(table(Smoker)) # proportion de fumeurs et non-fumeurs : F=0.1 NF=0.9
# 10% des enfants fument régulièrement.

#------------------------------------------------
#Représentation graphique des fumeurs/non-Fumeurs
#------------------------------------------------

barplot(prop.table(table(Smoker)),main="Répartition des fumeurs et non-fumeurs",xlab="Statut",ylab="Effectifs") # Histogramme comparant fumeurs/non-fumeurs
freq_fum <- as.numeric(prop.table(table(Smoker))) # Changement du type afin d'avoir une fréquence
freq_fum2 <- paste(round(freq_fum*100,2),"%",sep="") # Va permettre d'afficher des pourcentages
text(x = barplot(main="Répartition des fumeurs et non-fumeurs",freq_fum,ylim=c(0,1),xlab="Statut",ylab="Effectifs",names.arg=c("Fumeurs","Non-fumeurs")),y=freq_fum, label=freq_fum2, pos = 3,cex = 0.8, col ="red") # Permet de rajouter les pourcentages en rouge sur les colonnes
smokpie <- paste(round(prop.table(table(Smoker)),4)*100,"%") # Va permettre d'afficher des pourcentages
pie(prop.table(table(Smoker)),main="Répartition des fumeurs et non-fumeurs",label=smokpie, col=c("darkgray","white")) # Camembert des proportions fumeurs/non-fumeurs
legend("topright", c("Femme","Homme"),cex=0.8,fill=c("darkgray","white")) # Rajoute une légende au camembert.
# Comme attendu puisque l'échantillon est composé d'enfants de 3 à 19 ans, il y a beaucoup plus de non-fumeurs que de fumeurs (10% de fumeurs).*

#####################################################################################################################################################
# Analyse descriptive bivariée entre variables qualitatives et quantitatives
#####################################################################################################################################################

#------------
#Age et genre
#------------

by(Age,Sex,summary) #Résumé des indices statistiques 
round(by(Age,Sex,mean),2) # La moyenne est légèrement plus haute chez les hommes : 10.01 ans pour 9.84 chez les femmes (faire un test de Student)
round(by(Age,Sex,sd),2) # L'écart type est légèrement plus haut ches les hommes : 2.96 ans pour 2.93 ans chez les femmes (faire un test de Levène)
by(Age,Sex,quantile,seq(0,1,0.25)) # L'âge médian n'est pas différent : 10 ans
# Les 1er et 3ème quartiles ne sont pas différents
# L'âge minimum n'est pas différent : 3 ans
# L'âge maximum n'est pas différent : 19 ans

#----------------------------------------------
#Représentation graphique de l'âge par le genre
#----------------------------------------------

hist(Age[Sex=="Female"], main="Répartition de l'âge des femmes",xlab="Age en années",ylab="Effectifs",freq=T) # Histogramme de la répartition des âges des femmes
# Répartition symétrique avec une queue de distribution à droite, unimodale (classe modale : 8 à 10 ans).
hist(Age[Sex=="Male"], main="Répartition de l'âge des hommes",xlab="Age en années",ylab="Effectifs",freq=T) # Histogramme de la répartition de l'âge des hommes
# Répartition relativement symétrique avec une queue de distribution à droite, unimodale (classe modale : 8 à 10 ans.)
boxplot(Age~Sex,main="Répartition de l'âge en fonction du genre",data=x,xlab="Genre",ylab="Age en années") # Boîtes à moustache de la répartition de l'âge par genre
#Les boîtes à moustache sont identiques, l'âge est donc bien réparti de manière équivalente entre les hommes et les femmes.

#-----------------------
#Age et statut de fumeur
#-----------------------

by(Age,Smoker,summary) #Résumé des indices statistiques 
round(by(Age,Smoker,mean),2) # La moyenne est significativement plus haute chez les fumeurs : 13.52 ans pour 9.53 chez les non-fumeurs (faire un test de Student)
round(by(Age,Smoker,sd),2) # L'écart type est légèrement plus haut ches les non-fumeurs : 2.74 ans pour 2.34 ans chez les fumeurs (faire un test de Levène)
by(Age,Smoker,quantile,seq(0,1,0.25)) # L'âge médian est fortement différent : 13 ans pour les fumeurs, 9 ans pour les non-fumeurs
# Le 1er quartile est fortement différent : 12 ans pour les fumeurs, contre 8 ans pour les non-fumeurs
# Le 3ème quartile est fortement différent : 15 ans pour les fumeurs contre 11 ans pour les non-fumeurs
# L'âge minimum est fortement différent : 9 ans pour les fumeurs, 3 ans pour les non-fumeurs
# L'âge maximum n'est pas différent : 19 ans

#---------------------------------------------------------
#Réprésentation graphique de l'âge par le statut de fumeur
#---------------------------------------------------------

hist(Age[Smoker=="Current"], main="Répartition de l'âge des fumeurs réguliers",xlab="Age en années",ylab="Effectifs",freq=T) # Histogramme de l'âge des fumeurs
# Répartition asymétrique, avec une queue de distribution à droite, unimodale (classe modale : 12 à 13 ans).
hist(Age[Smoker=="Non"], main="Répartition de l'âge des non-fumeurs",xlab="Age en années",ylab="Effectifs",freq=T) # Histogramme de l'âge des non-fumeurs
# Répartition symétrique avec une queue de distribution à droite, unimodale (classe modale : 8 à 10 ans).
# On remarque que les deux répartitions sont très différentes.
boxplot(Age~Smoker,main="Répartition de l'âge en fonction du statut de fumeur",data=x,xlab="Statut de fumeur",ylab="Age en années") # Boîtes à moustache de l'âge des enfants en fonction du statut de fumeur
# Les valeurs extrêmes ne sont pas pas aberrantes (vu plus haut). La variance semble relativement égale car les box sont identiques. Enorme écart entre les fumeurs
# et non-fumeurs, on voit que le 1er quartile des fumeurs est plus grand que le troisième des non-fumeurs.

#---------------
#Taille et genre
#---------------

by(taille,Sex,summary) #Résumé des indices statistiques 
round(by(taille,Sex,mean),2) # La moyenne est plus haute chez les hommes : 157,54cm pour 152,94cm chez les femmes (faire un test de Student)
round(by(taille,Sex,sd),2) # L'écart type est signicativement plus haut chez les hommes : 16.08cm pour 12.17cm chez les femmes (faire un test de Levène)
by(taille,Sex,quantile,seq(0,1,0.25)) # La taille médiane est différente : 157.48cm pour les hommes, 154.94cm pour les femmes
# Le 1er quartile est faiblement différent : 144,78cm pour les hommes, contre 146.05cm pour les femmes
# Le 3ème quartile est fortement différent : 171,45cm pour les hommes contre 161.29cm pour les femmes
# La taille minimale est différente : 119.38cm pour les hommes, 116.84cm pour les femmes
# La taille maximale est fortement différente : 187.96cm pour les hommes, 180.34cm pour les femmes

#--------------------------------------------------
#Représentation graphique de la taille par le genre
#--------------------------------------------------

hist(taille[Sex=="Female"], main="Répartition de la taille des femmes",xlab="Taille en cm",ylab="Effectifs",freq=T) # Histogrammme de la taille des femmes
# Répartition légèrement symétrique avec une grande queue de distribution à gauche, unimodale (classe modale : 160-165 cm).
hist(taille[Sex=="Male"], main="Répartition de la taille des hommes",xlab="Taille en cm",ylab="Effectifs",freq=T) # Histogramme de la taille des hommes
# Répartition complètement asymétrique, unimodale (170-175 cm).
boxplot(taille~Sex,main="Répartition de la taille en fonction du genre",data=x,xlab="Genre",ylab="Taille en cm") # Boîtes à moustache de la taille par le genre
# Variance beaucoup plus haute chez les hommes (box beaucoup plus grande). Les valeurs extrêmes ne sont pas aberrantes (vu plus haut). Concentration autour de la médiane
# plus importante chez les femmes.

#--------------------------
#Taille et statut de fumeur
#--------------------------

by(taille,Smoker,summary) #Résumé des indices statistiques 
round(by(taille,Smoker,mean),2) # La moyenne est signicativement plus haute chez les fumeurs : 167.52cm pour 153.96cm chez les non-fumeurs (faire un test de Student)
round(by(taille,Smoker,sd),2) # L'écart type est signicativement plus haut ches les fumeurs : 14.41cm pour 8.11cm chez les non-fumeurs (faire un test de Levène)
by(taille,Smoker,quantile,seq(0,1,0.25)) # La taille médiane est fortement différente : 167.64cm pour les fumeurs, 154.94cm pour les non-fumeurs
# Le 1er quartile sont fortement différent : 161.29cm pour les fumeurs, contre 144.78cm pour les non-fumeurs
# Le 3ème quartile sont fortement différent : 172.72cm pour les fumeurs contre 163.83cm pour les non-fumeurs
# La taille minimale est fortement différente : 147.32cm pour les fumeurs, 116.84cm pour les non-fumeurs
# La taille maximale est différente : 182.cm pour les fumeurs, 187.96cm pour les non-fumeurs

#-------------------------------------------------------------
#Représentation graphique de la taille par le statut de fumeur
#-------------------------------------------------------------

hist(taille[Smoker=="Current"], main="Répartition de la taille des fumeurs",xlab="Taille en cm",ylab="Effectifs",freq=T) # Histogramme de la taille des non-fumeurs
# Répartition symétrique avec une queue de distribution à gauche, unimodale (classe modale : 170-175cm)
hist(taille[Smoker=="Non"], main="Répartition de la taille des non-fumeurs",xlab="Taille en cm",ylab="Effectifs",freq=T) # Histogramme de la taille des fumeurs
# Répartition équivalente à la répartition de la taille de l'échantillon, même commentaires.
boxplot(taille~Smoker,main="Répartition de la taille en fonction du statut de fumeur",data=x,xlab="Statut de fumeur",ylab="Taille en cm") # Boîte à moustache de la taille par le statut de fumeur
# Variance beaucoup plus haute chez les non-fumeurs (attendu car présence d'enfants très jeunes). Concentration plus grande autour de la médiane chez les fumeurs que
# les non-fumeurs. Plus grands étalement aux pôles chez les non-fumeurs.

#------------
#FEV et genre
#------------

by(FEV,Sex,summary) #Résumé des indices statistiques 
round(by(FEV,Sex,mean),2) # La moyenne est plus haute chez les hommes : 2.81L pour 2.45L chez les femmes (faire un test de Student)
round(by(FEV,Sex,sd),2) # L'écart type est signicativement plus haut ches les hommes : 1L pour 0.65L chez les femmes (faire un test de Levène)
by(FEV,Sex,quantile,seq(0,1,0.25)) # La médiane est légèrement différente : 2.61L pour les hommes, 2.49L pour les femmes
# Le 1er quartile est très faiblement différent : 2.01L pour les hommes, contre 1.95L pour les femmes
# Le 3ème quartile est fortement différent : 3.53L pour les hommes contre 2.99L pour les femmes
# Le minimum est identique : 0.80L pour les hommes, 0.79L pour les femmes
# Le maximum est fortement différent : 5.79L pour les hommes, 3.835L pour les femmes

#-----------------------------------------------
#Représentation graphique de la fev par le genre
#-----------------------------------------------

hist(FEV[Sex=="Female"], main="Répartition de la fev des femmes",xlab="fev en L",ylab="Effectifs",freq=T) # Histogramme de la fev par le genre
# Répartition symétrique, avec une queue de distribution à droite, unimodale (2.5-3.0 L).
hist(FEV[Sex=="Male"], main="Répartition de la fev des hommes",xlab="fev en L",ylab="Effectifs",freq=T) # Histogramme de la fev par le genre
# Répartition asymétrique avec une queue de distribution à droite, unimodale (2-2.5 L).
# Répartition différente de la fev suivant le genre.
boxplot(FEV~Sex,main="Répartition de la fev en fonction du genre",data=x,xlab="Genre",ylab="fev en L") # Boîte à moustache de la fev en fonction du genre
# Variance plus élevée chez les hommes, et plus grand étalement chez les hommes aux pôles, pas de valeurs extrêmes. Concentration plus grande autour de la médiane 
# chez les femmes.

#-----------------------
#FEV et statut de fumeur
#-----------------------

by(FEV,Smoker,summary) #Résumé des indices statistiques 
round(by(FEV,Smoker,mean),2) # La moyenne est plus haute chez les fumeurs : 3.28L pour 2.57L chez les non-fumeurs (faire un test de Student)
round(by(FEV,Smoker,sd),2) # L'écart type est légèrement plus haut chez les non-fumeurs : 0.75L pour 0.85L chez les non-fumeurs (faire un test de Levène)
by(FEV,Smoker,quantile,seq(0,1,0.25)) # La médiane est légèrement différente : 3.17L pour les fumeurs, 2.46L pour les non-fumeurs
# Le 1er quartile est différent : 2.79L pour les fumeurs, contre 1.92L pour les non-fumeurs
# Le 3ème quartile est différent : 3.75L pour les fumeurs contre 3.05L pour les non-fumeurs
# Le minimum est extrêment différent : 1.69L pour les fumeurs, 0.79L pour les non-fumeurs
# Le maximum est fortement différent : 4.87L pour les fumeurs, 5.76L pour les non-fumeurs

#----------------------------------------------------------
#Représentation graphique de la fev par le statut de fumeur
#----------------------------------------------------------

hist(FEV[Smoker=="Current"], main="Répartition de la fev des fumeurs",xlab="fev en L",ylab="Effectifs",freq=T) # Histogramme de la FEV des fumeurs
# Répartition symétrique et unimodale (classe modale : [3.0;3.5]).
hist(FEV[Smoker=="Non"], main="Répartition de la fev des non-fumeurs",xlab="fev en L",ylab="Effectifs",freq=T) # Histogramme de la FEV des non-fumeurs
# Répartition symétrique avec une queue de distribution à droite, unimodale ( classe modale : 2-2.5 L).
# La classe modale est plus haute chez les fumeurs, ce qui n'est pas le résultat attendu.

boxplot(FEV~Smoker,main="Répartition de la fev en fonction du statut de fumeur",data=x,xlab="Statut de fumeur",ylab="fev en L") # Boîte à moustache de la FEV en fonction du statut de fumeur
# La fev des fumeurs est plus haute que celle des non-fumeurs, les valeurs extrêmes ne sont pas aberrantes.
# Résultats non conformes car l'âge est un facteur de confusion dans l'observation. Pour cela on peut effectuer une stratification sur l'âge,
# par exemple séparer les enfants de plus et de moins de douze ans.

fev_12inf <- x[Age<12,] # Crée un vecteur contenant tous les enfants de moins de 12 ans
fev_12sup <- x[Age>=12,] # Crée un vecteur contenant tous les enfants de 12 ans ou plus

dim (fev_12inf) # 480 enfants de moins de 12 ans
dim (fev_12sup) # 174 enfants de 12 ans ou plus
table(fev_12inf$Smoker) # 15 fumeurs de moins de 12 ans
prop.table(table(fev_12sup$Smoker)) #28.8% de fumeurs de 12 ans ou plus


#----------------------------------------------------------------------------------
#FEV et statut de fumeurs sur les enfants de moins de 12ans (même si peu pertinant)
#----------------------------------------------------------------------------------

by(fev_12inf$FEV,fev_12inf$Smoker,summary) #Résumé des indices statistiques 
round(by(fev_12inf$FEV,fev_12inf$Smoker,mean),2) # La moyenne est haute chez les fumeurs : 3.03L pour 2.31L chez les non-fumeurs (faire un test de Student)
round(by(fev_12inf$FEV,fev_12inf$Smoker,sd),2) # L'écart type est très légèrement plus haut chez les non-fumeurs : 0.68L pour 0.66L chez les non-fumeurs (faire un test de Levène)
by(fev_12inf$FEV,fev_12inf$Smoker,quantile,seq(0,1,0.25)) # La médiane est très différente : 3.10L pour les fumeurs, 2.25L pour les non-fumeurs
# Le 1er quartile est très différent : 2.96L pour les fumeurs, contre 1.79L pour les non-fumeurs
# Le 3ème quartile est très différent : 3.25L pour les fumeurs contre 2.72L pour les non-fumeurs
# Le minimum est très différent : 1.69L pour les fumeurs, 0.79L pour les non-fumeurs
# Le maximum est très légèrement différent : 4.64L pour les fumeurs, 4.59L pour les non-fumeurs

#----------------------------------------------------------------------------------------------
#Représentation graphique de la fev par le statut de fumeur pour les enfants de moins de 12 ans
#----------------------------------------------------------------------------------------------

hist(fev_12inf$FEV[fev_12inf$Smoker=="Current"], main="Répartition de la fev des fumeurs de moins de 12 ans",xlab="fev en L",ylab="Effectifs",freq=T) # Histogramme de la FEV des fumeurs de moins de 12 ans
# Répartition asymétrique et unimodale (classe modale : [3.0;3.5]). Peu précis car peu de fumeurs de moins de 12 ans.
hist(fev_12inf$FEV[fev_12inf$Smoker=="Non"], main="Répartition de la fev des non-fumeurs de moins de 12 ans", xlab="fev en L",ylab="Effectifs",freq=T) # Histogramme de la FEV des non-fumeurs de moins de 12 ans
# Répartition symétrique avec une queue de distribution à droite, unimodale ( classe modale : 2-2.5 L).
# La classe modale est plus haute chez les fumeurs, ce qui n'est pas le résultat attendu (mais normale car malgré le découpage en classe il y a une différence d'âge moyen).

boxplot(fev_12inf$FEV~fev_12inf$Smoker,main="Répartition de la fev en fonction du statut de fumeur des enfants de moins de 12 ans",data=x,xlab="Statut de fumeur",ylab="fev en L") # Boîte à moustache de la FEV des moins de 12 ans en fonction du statut de fumeur

resultats3 <- boxplot(fev_12inf$FEV[fev_12inf$Smoker=="Current"])
valext3 <- resultats3$out # Stock les valeurs aberrantes de la boîte à moustache des fumeurs
valext3 # Les affiche 1.953 1.694 2.387 4.637

resultats3 <- boxplot(fev_12inf$FEV[fev_12inf$Smoker=="Non"])
valext3 <- resultats3$out # Stock les valeurs aberrantes de la boîte à moustache des non-fumeurs
valext3 # Les affiche 4.593 4.591 4.324 4.130

fevfumtries <- sort(fev_12inf$FEV[fev_12inf$Smoker=="Current"],decreasing=T) # Trie les fev des fumeurs par ordre décroissant
head(fevfumtries) # Permet de vérifier si les valeurs extrèmes sont vraiment aberrantes : 4.637 semble être une valeur aberrante
fevfumtries <- sort(fev_12inf$FEV[fev_12inf$Smoker=="Current"],decreasing=F) # Trie les fev des fumeurs par ordre croissant
head(fevfumtries) # Permet de vérifier si les valeurs extrèmes sont vraiment aberrantes : les valeurs ne semblent pas aberrantes

fevfumtries <- sort(fev_12inf$FEV[fev_12inf$Smoker=="Non"],decreasing=T) # Trie les fev des non-fumeurs par ordre décroissant
head(fevfumtries) # Permet de vérifier si les valeurs extrèmes sont vraiment aberrantes : les valeurs ne semblent pas aberrantes

# Il y a une valeur aberrante chez les fumeurs : 4.637. Il y a trop peu de fumeurs pour que la boîte à moustache soit pertinente.

#----------------------------------------------------------
#FEV et statut de fumeurs sur les enfants de 12 ans et plus
#----------------------------------------------------------

by(fev_12sup$FEV,fev_12sup$Smoker,summary) #Résumé des indices statistiques 
round(by(fev_12sup$FEV,fev_12sup$Smoker,mean),2) # La moyenne est légèrement plus basse chez les fumeurs : 3.35L pour 3.53L chez les non-fumeurs (faire un test de Student)
round(by(fev_12sup$FEV,fev_12sup$Smoker,sd),2) # L'écart type est très légèrement plus haut chez les non-fumeurs : 0.76L pour les fumeurs 0.80L chez les non-fumeurs (faire un test de Levène)
by(fev_12sup$FEV,fev_12sup$Smoker,quantile,seq(0,1,0.25)) # La médiane est légèrement différente : 3.30L pour les fumeurs, 3.40L pour les non-fumeurs
# Le 1er quartile est légèrement différent : 2.77L pour les fumeurs, contre 2.91L pour les non-fumeurs
# Le 3ème quartile est légèrement différent : 3.83L pour les fumeurs contre 4.09L pour les non-fumeurs
# Le minimum est différent : 2.20L pour les fumeurs, 1.92L pour les non-fumeurs
# Le maximum est très différent : 4.87L pour les fumeurs, 5.79L pour les non-fumeurs

#----------------------------------------------------------------------------------------------
# Représentation graphique de la fev par le statut de fumeur pour les enfants de 12 ans et plus
#----------------------------------------------------------------------------------------------

hist(fev_12sup$FEV[fev_12sup$Smoker=="Current"], main="Répartition de la fev des fumeurs de 12 ans et plus",xlab="fev en L",ylab="Effectifs",freq=T) # Histogramme de la FEV des fumeurs de 12 ans et plus
# Répartition asymétrique avec une queue de distribution à droite, unimodale (classe modale : [3.0;3.5]).
hist(fev_12sup$FEV[fev_12sup$Smoker=="Non"], main="Répartition de la fev des non-fumeurs de 12 ans et plus", xlab="fev en L",ylab="Effectifs",freq=T) # Histogramme de la FEV des non-fumeurs de 12 ans et plus
# Répartition asymétrique avec une queue de distribution à droite, unimodale ( classe modale : 2.5-3 L).
# La classe modale est plus haute chez les fumeurs, ce qui n'est pas le résultat attendu (mais normale car malgré le découpage en classe il y a toujours une différence d'âge moyen).
# Répartition différente suivant le statut de fumeur.

boxplot(fev_12sup$FEV~fev_12sup$Smoker,main="Répartition de la fev en fonction du statut de fumeur des enfants de 12 ans et plus",data=x,xlab="Statut de fumeur",ylab="fev en L") # Boîte à moustache de la FEV des 12 ans et plus en fonction du statut de fumeur
# La box des non-fumeurs est légèrement surélevée par rapport à celle des fumeurs. Pas de valeurs aberrantes. Concentration autours des médianes
# relativement égale. Grande étendue vers le haut des non-fumeurs.

#####################################################################################################################################################
# Analyse descriptive bivariée entre variables qualitatives
#####################################################################################################################################################

#-------------------------
#Statut de fumeur et genre
#-------------------------

table(Sex,Smoker) # Affiche le tableau de contingence. Support pour le test du chi²
# On remarque qu'il y plus de femmes que d'hommes qui fument : 39 fumeuses pour 26 fumeurs.
100*prop.table(table(Sex,Smoker)) # Affiche les valeurs de la table en pourcentage

#----------------------------------------------
#Etude du genre en fonction du statut de fumeur
#----------------------------------------------

fumeur <-x[Smoker=="Current",] # Stock les individus fumeurs
nonfumeur <-x[Smoker=="Non",] # Stock les individus non-fumeurs
table(fumeur$Sex) # Affiche le tableau du genre des fumeurs
100*prop.table(table(fumeur$Sex)) # Affiche ce même tableau en pourcentage
# 60% des fumeurs de l'échantillon sont des femmes.
100*prop.table(table(nonfumeur$Sex)) # Affiche le tableau du genre des non-fumeurs
# 52.63% des non-fumeurs sont des hommes.

#----------------------------------------------
#Etude du statut de fumeur en fonction du genre
#----------------------------------------------
femmes <- x[Sex=="Female",] # Stock les femmes 
hommes <- x[Sex=="Male",] # Stock les hommes
100*prop.table(table(femmes$Smoker)) # Affiche le tableau du statut de fumeur des femmes en pourcentage
# 12.26% des femmes de l'échantillon fument.
100*prop.table(table(hommes$Smoker)) # Affiche le tableau du statut de fumeur des femmes en pourcentage
# 7.74% des hommes de l'échantillon fument.
# En conclusion, le genre semble avoir un effet sur le statut de fumeur (test du chi² pour vérifier).

#####################################################################################################################################################
# Analyse descriptive bivariée entre variables quantitatives
#####################################################################################################################################################

#-----------------------------------------------
# Représentation graphique de la taille et l'âge
#-----------------------------------------------

plot(taille,Age, main="Répartition de la taille en fonction de l'âge", xlab="Taille en cm", ylab="Age en années") # Nuage de point de la corrélation Taille/Age
# La corrélation semble linéaire, ce qui est attendu. On remarque bien que la taille a tendance à augmenter quand l'âge augmente.

#--------------
# Taille et âge
#--------------

# On étudie la corrélation des deux variables. Si elle est nulle il n'y a pas de corrélation. Si elle est positive les variables auront tendance 
# à évoluer dans le même sens. Si elle est négative, les varibales auront tendance à évoluer dans le sens inverse. La corrélation étant linéaire (cf graph)
# on utilise l'option pearson.
cor(taille,Age,method = "pearson") # Affiche la corrélation de la taille et l'âge : 0.79
# L'âge et la taille sont fortement corrélés, et évoluent dans le même sens, ce qui est attendu : un enfant plus âgé sera en moyenne plus grand.

#--------------------------------------------
# Représentation graphique de l'âge et la fev
#--------------------------------------------

plot(FEV,Age, main="Répartition de la fev en fonction de l'âge", xlab="fev en L", ylab="Age en années") # Nuage de point de la corrélation fev/Age
# La corrélation semble linéaire. On remarque bien que la fev a tendance à augmenter quand l'âge augmente.

#-----------
# Age et fev
#-----------

# La corrélation étant linéaire, on utilise l'option pearson qui est par défaut.
cor(FEV,Age) # Affiche la corrélation de la taille et l'âge : 0.76
# L'âge et la fev sont fortement corrélés, et évoluent dans le même sens, ce qui est attendu : un enfant plus âgé aura en moyenne une capacité
# pulmonaire plus grande.

#------------------------------------------------
# Représentation graphique de la taille et la fev
#------------------------------------------------

plot(FEV,taille, main="Répartition de la fev en fonction de la taille", xlab="fev en L", ylab="Taille en cm") # Nuage de point de la corrélation fev/Taille
# La corrélation semble linéaire. On remarque bien que la fev a tendance à augmenter fortement quand la taille augmente.


#--------------
# Taille et fev
#--------------

# La corrélation étant linéaire, on utilise l'option pearson qui est par défaut.
cor(FEV,taille) # Affiche la corrélation de la taille et l'âge : 0.86
# L'âge et la fev sont fortement corrélés, et évoluent dans le même sens, ce qui est attendu : un enfant plus grand aura en moyenne une capacité
# pulmonaire plus grande.

# On remarque que plusieurs variables ont l'air d'être associées, et on souhaite déterminer si cela est dû à la fluctuation d'échantillonage ou non.
# Pour cela on effectue des tests statistiques.

#####################################################################################################################################################
#Tests paramétriques entre variables quantitatives et variables qualitatives
#####################################################################################################################################################

# Chaque variable qualitative a deux modalités : homme et femme pour le sexe, fumeur et non-fumeur pour le statut de fumeur. Nous allons donc effectuer 
# des tests de comparaison de deux moyennes observées dans chacun des cas.

#---------------------------
# Test sur l'âge et le genre
#---------------------------

# Ici, chaque sous population a un effectif suffisamment grand pour appliquer le TLC. Donc on est dans le cas gaussien. On peut donc utiliser
# le test de Welch pour comparer les moyennes sans utiliser le test de Fisher pour comparer les variances.

t.test(Age~Sex,alternative="two.sided",conf.level=.95,var.equal=FALSE,data=x) # Test bilatéral de Welch à deux échantillons au niveau de confiance 95%
# La p-valeur est 46%, et est donc supérieure à 5%. A ce niveau de confiance, il n'y a pas de différences significatives entre 
# l'âge moyen des hommes et l'âge moyen des femmes.

#--------------------------------------
# Test sur l'âge et le statut de fumeur
#--------------------------------------

# Ici, chaque sous population a un effectif suffisamment grand pour appliquer le TLC. Donc on est dans le cas gaussien. On peut donc utiliser
# le test de Welch pour comparer les moyennes sans utiliser le test de Fisher pour comparer les variances. Au vu des distributions, on effectue un
# test unilatéral.

t.test(Age~Smoker,alternative="greater",conf.level=.95,var.equal=F,data=x) # Test unilatéral de Welch à deux échantillons au niveau de confiance 95%
# La p-valeur est inférieure à 2.2e-16, et donc inférieure à 5%. Au niveau 95%, et même 99%, l'âge des fumeurs est plus grand que l'âge des non-fumeurs.

#-------------------------------
# Test sur la taille et le genre
#-------------------------------

# Ici, chaque sous population a un effectif suffisamment grand pour appliquer le TLC. Donc on est dans le cas gaussien. On peut donc utiliser
# le test de Welch pour comparer les moyennes sans utiliser le test de Fisher pour comparer les variances. Au vu des distributions, on effectue un
# test unilatéral.

t.test(taille~Sex,alternative="less",conf.level=.95,var.equal=FALSE,data=x) # Test bilatéral de Welch à deux échantillons au niveau de confiance 95%
# La p-valeur est 1.94e-5 et est donc inférieure à 5%. Au niveau 5%, et même 1%, la taille moyenne des filles est plus petite que la taille moyenne
# des garçons.

#------------------------------------------
# Test sur la taille et le statut de fumeur
#------------------------------------------

# Ici, chaque sous population a un effectif suffisamment grand pour appliquer le TLC. Donc on est dans le cas gaussien. On peut donc utiliser
# le test de Welch pour comparer les moyennes sans utiliser le test de Fisher pour comparer les variances. Au vu des distributions, on effectue
# un test unilatéral.

t.test(taille~Smoker,alternative="greater",conf.level=.95,var.equal=FALSE,data=x) # Test bilatéral de Welch à deux échantillons au niveau de confiance 95%
# La p-valeur est inférieure à 2.2e-16, et donc inférieure à 5%. Au niveau 95%, et même 1%, la taille moyenne des fumeurs est plus grande que la taille
# moyenne des non-fumeurs.

#----------------------------
# Test sur la fev et le genre
#----------------------------

# Ici, chaque sous population a un effectif suffisamment grand pour appliquer le TLC. Donc on est dans le cas gaussien. On peut donc utiliser
# le test de Welch pour comparer les moyennes sans utiliser le test de Fisher pour comparer les variances. Au vu des distributions, on effectue
# un test unilatéral.

t.test(FEV~Sex,alternative="less",conf.level=.95,var.equal=FALSE,data=x) # Test Bilatéral de Welch à deux échantillons au niveau de confiance 95%
# La p-valeur est de 2.8e-8 et est donc inférieure à 5%. Au niveau 5%, et même 1%, les capacités pulmonaires moyennes sont plus petites chez 
# les femmes que chez les hommes.

#---------------------------------------
# Test sur la fev et le statut de fumeur
#---------------------------------------

# Ici, chaque sous population a un effectif suffisamment grand pour appliquer le TLC. Donc on est dans le cas gaussien. On peut donc utiliser
# le test de Welch pour comparer les moyennes sans utiliser le test de Fisher pour comparer les variances.

t.test(FEV~Smoker,alternative="two.sided",conf.level=.95,var.equal=FALSE,data=x) # Test bilatéral de Welch à deux échantillons au niveau de confiance 95%
# La p-valeur est de 3.1e-10 et est donc inférieure à 5%. Au niveau 5%, et même 1%, les capacités pulmonaires sont différentes suivant le statut
# de fumeur.

# On avait vu plus haut qu'étudier la distribution de la fev par rapport au statut de fumeur sur l'intégralité de l'échantillons était inadéquat car
# l'âge est un facteur de confusion trop important. On va donc effectuer le test uniquement pour les enfants de 12 ans ou plus. Au vu des distributions
# on va effectuer un test unilatéral.
# Ici, chaque sous population a toujours un effectif suffisamment grand pour appliquer le TLC. Donc on est dans le cas gaussien. On peut donc utiliser
# le test de Welch pour comparer les moyennes sans utiliser le test de Fisher pour comparer les variances.

t.test(fev_12sup$FEV~fev_12sup$Smoker,alternative="less",conf.level=.95,var.equal=FALSE,data=fev_12sup) # Test unilatéral de Welch à deux échantillons au niveau de confiance 95% 
# La p-valeur est de 8.8% et est donc supérieure à 5%. Donc au niveau 5%, on ne peut pas dire que les capacités pulmonaires sont différentes suivant
# le statut de fumeur.

#####################################################################################################################################################
# Tests non-paramétriques entre variables variables qualitatives
#####################################################################################################################################################

#-----------------------------------------
# Test sur le genre et le statut de fumeur
#-----------------------------------------

# Nous allons comparer deux variables qualitatives ayant chacune deux modalités. Nous allons donc effectuer un test du ch² d'indépendance.
table(Sex,Smoker) # Affiche le tableau de contingence
chisq.test(table(Sex,Smoker)) # Effectue le test du chi² sur les variables Sex et Smoker
# La p-valeur est 7.1% et est donc supérieure à 5%. Au niveau 5%, on ne peut pas dire qu'il y ait une différence de fréquence de fumeurs par rapport
# au genre des enfants.

#####################################################################################################################################################
# Tests paramétriques entre variables quantitatives
#####################################################################################################################################################

# Ici, nous allons effectuer des tests de corrélations linéaires afin de vérifier si les différences observées ne proviennent que de la 
# fluctuation d'échantillonage.

#------------------------------------------------------
# Test de corrélation linéaire entre l'âge et la taille
#------------------------------------------------------

# Au vu de la corrélation calculée préalablement, on effectue un test univarié.
cor.test(Age,taille,alternative="greater",method="pearson",conf.level=.95,data=x) # Test de corrélation entre l'âge et la taille au niveau de confiance 95%
# La p-valeur est inférieure à 2.2e-16 et donc inférieure à 5%. Au niveau 5%, et même 1%, l'âge et la taille évoluent dans le même sens et 
# sont corrélées linéairement.

#---------------------------------------------------
# Test de corrélation linéaire entre l'âge et la fev
#---------------------------------------------------

# Au vu de la corrélation calculée préalablement, on effectue un test univarié.
cor.test(Age,FEV,alternative="greater",method="pearson",conf.level=.95,data=x) # Test de corrélation entre l'âge et la fev au niveau de confiance 95%
# La p-valeur est inférieure à 2.2e-16 et donc inférieure à 5%. Au niveau 5%, et même 1%, l'âge et la capacité pulmonaire évoluent dans le même sens
# et sont corrélés linéairement.

#-------------------------------------------------------
# Test de corrélation linéaire entre la taille et la fev
#-------------------------------------------------------

# Au vu de la corrélation calculée préalablement, on effectue un test univarié.
cor.test(taille,FEV,alternative="greater",method="pearson",conf.level=.95,data=x) # Test de corrélation entre la taille et la fev au niveau de confiance 95%
# La p-valeur est inférieure à 2.2e-16 et donc inférieure à 5%. Au niveau 5%, et même 1%, la taille et la capacité pulmonaire évoluent dans le même sens
# et sont corrélées linéairement.

# Fin, le 03/10/2018.









