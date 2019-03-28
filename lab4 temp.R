library(readxl)
library(tidyverse)
library(ggplot2)
defense <-read_excel('cyclonesFootball2018.xlsx',sheet ='Defensive')
offense <-read_excel('cyclonesFootball2018.xlsx',sheet ='Offensive')
bio <- read_excel('cyclonesFootball2018.xlsx',sheet ='Biography')

str(defense)
defense$Name <-as.factor(defense$Name)
defense$Opponent_Opponent <-as.factor(defense$Opponent_Opponent)
defenseClean<- defense %>% mutate_if(is.character,as.numeric)
str(defenseClean)

str(offense)
offense$Name <- as.factor(offense$Name)
offense$Opponent_Opponent <- as.factor(offense$Opponent_Opponent)
offenseClean <- offense %>% mutate_if(is.character,as.numeric)
str(offenseClean)

str(bio)
bio$Name <-as.factor(bio$Name)
bio$Weight <-as.numeric(bio$Weight)
str(bio)
bioClean <- bio %>% separate(Height, into = c("Feet","Inches") )%>% mutate_at(c("Feet","Inches"),as.numeric)
str(bioClean)

def<-defenseClean %>% gather(key =Statistics  ,value = Count ,3:11)                        
head(def)
ggplot(def,aes(x =Count))+geom_histogram(binwidth =1)+facet_wrap(~Statistics,scales = "free")

def <- def %>%filter(Opponent_Opponent =='West Virginia' | Opponent_Opponent =='Kansas State') %>%
  spread(key = Opponent_Opponent,value = Count) %>% 
  filter(Statistics == "Tackles_Solo") %>%
  group_by(Name)
def[is.na(def)] <- 0;

ggplot(def,aes(x = 'West Virginia',y='Kansas State'),group_by(name))+geom_point()



str(bioClean)
bioClean <- bioClean %>% separate(Hometown,into = c("City","State"),sep =', ')
str(bioClean)
head(bioClean)
bioClean_Count <- bioClean %>% group_by(State) %>% summarize(Num = n())
bioClean_Count
offenseJoin <- left_join(offenseClean,bioClean ,by = "Name")

recieve <- offenseJoin %>% filter(!is.na(offenseJoin$Receiving_YDS))
ggplot(recieve,aes(x =Weight,y = Receiving_YDS))+geom_line()
