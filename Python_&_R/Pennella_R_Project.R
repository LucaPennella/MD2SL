rm(list=ls(all=TRUE))
setwd("C:\\Users\\matteo.stabile\\OneDrive - Accenture\\Desktop\\Esercizio_Master")
movielist <- read.csv(file = "top1000movies.csv")

head(movielist)
library(tidyr)
movielist_splitted_genres = separate_rows(movielist, Genres, convert = TRUE)

library(ggplot2)
P =ggplot(movielist_splitted_genres, aes(x=reorder(Genres, Genres, function(x)-length(x)))) +
geom_bar(fill='Green') +  labs(x='Genere del Film')
P + coord_flip()


install.packages('epiDisplay')
library(epiDisplay)
tab1(movielist_splitted_genres$Genres, sort.group = "decreasing", cum.percent = TRUE, main = '
Distribution of the movie genre')

question1 <- read.csv(file = "question_1.csv")
head(question1)

library(dplyr)
question1_filter = filter(movielist_splitted_genres, Title == "The Dark Knight Rises" |  Title == "King Kong" | Title == 'The Ten Commandments')
library(epiDisplay)
tab1(question1_filter$Genres, sort.group = "decreasing", cum.percent = TRUE, main = '
Distribution of the movie genre of Table1')




question2 <- read.csv(file = "question_2.csv")
head(question2)

library(ggplot2)
#Plot
ggplot(question2,aes(x=factor(Righe),y=Conteggio))+
  geom_col(color='black',fill='cyan3')+
  xlab('Number of Rows')+
  ggtitle('Distribution of question 2')+
  geom_text(aes(label = Film), nudge_y = 3)

question4 <- read.csv(file = "question_4.csv")
head(question4)

#Plot
ggplot(question4,aes(x=factor(Righe),y=Med.film.in.a.year))+
  geom_col(color='black',fill='cyan3')+
  xlab('Number of Rows')+
  ggtitle('Distribution of question 4')
  

question5 <- read.csv(file = "question_5.csv")
head(question5)


#Plot
ggplot(question5,aes(x=factor(Righe),y=Med.actor.in.a.film))+
  geom_col(color='black',fill='cyan3')+
  xlab('Number of Rows')+
  ggtitle('Distribution of question 5')


question3 <- read.csv(file = "question_3.csv")
head(question3)

actorlist <- read.csv(file = "df_actors_corretto.csv")
head(actorlist)

question3_filter_Flowers = filter(actorlist, Actor == 'Flowers, Bess')

question3_filter_Flowers_film = filter(movielist_splitted_genres, Title %in% question3_filter_Flowers$Title)

library(epiDisplay)
tab1(question3_filter_Flowers_film$Genres, sort.group = "decreasing", cum.percent = TRUE, main = '
Distribution of the movie genre of Table 3 for Flowers')

question3_filter_Deniro = filter(actorlist, Actor == 'De Niro, Robert')

question3_filter_Deniro_film = filter(movielist_splitted_genres, Title %in% question3_filter_Deniro$Title)


library(epiDisplay)
tab1(question3_filter_Deniro_film$Genres, sort.group = "decreasing", cum.percent = TRUE, main = '
Distribution of the movie genre of Table 3 for Flowers')


question3_filter_Ratzenberger = filter(actorlist, Actor == 'Ratzenberger, John')

question3_filter_Ratzenberger_film = filter(movielist_splitted_genres, Title %in% question3_filter_Ratzenberger$Title)


library(epiDisplay)
tab1(question3_filter_Ratzenberger_film$Genres, sort.group = "decreasing", cum.percent = TRUE, main = '
Distribution of the movie genre of Table 3 for Flowers')

question6 <- read.csv(file = "question_6.csv")
head(question6)


question6_filter_Freeman = filter(actorlist, Actor == 'Freeman, Morgan (I)')

question6_filter_Freeman_film = filter(movielist_splitted_genres, Title %in% question6_filter_Freeman$Title)


library(epiDisplay)
tab1(question6_filter_Freeman_film$Genres, sort.group = "decreasing", cum.percent = TRUE, main = '
Distribution of the movie genre of Table 3 for Flowers')


question6_filter_Oldman = filter(actorlist, Actor == 'Oldman, Gary')

question6_filter_Oldman_film = filter(movielist_splitted_genres, Title %in% question6_filter_Oldman$Title)


library(epiDisplay)
tab1(question6_filter_Oldman_film$Genres, sort.group = "decreasing", cum.percent = TRUE, main = '
Distribution of the movie genre of Table 3 for Flowers')
 




