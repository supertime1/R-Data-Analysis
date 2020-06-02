#Study subjects' demographics
library(ggplot2)

#age 
age <- c(31,32,56,56,63,64,31,32,41)
Age <- as.data.frame(age)

g<-ggplot(Age, aes(age))
A <- g + 
  geom_bar(fill = 'pink') + 
  labs(y="Counts")+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))
A
#gender
gender <- c('female','male','female','male',
            'female','male','female','male','male')
Gender <- as.data.frame(gender)

g<-ggplot(Gender, aes(gender))
G <- g + 
  geom_bar(fill = 'light green') + 
  labs(y="Counts")+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))
G
#race
race <- c('Asian','Asian','Asian','Asian','Asian','Asian',
          'Asian','Asian','White')
Race <- as.data.frame(race)

g<-ggplot(Race, aes(race))
R <- g + 
  geom_bar(fill = 'light blue') + 
  labs(y="Counts")+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))
R
