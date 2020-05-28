library(dplyr)
library(tidyr)
library(MLmetrics)#MAPE, MAE
library(ggplot2)
library(stringr)#String operations
library(psych)#ICC
library(formattable)#make a table

#extract data
setwd('C:/Users/57lzhang.US04WW4008/Desktop/Ear dome eva/All Color')
temp<-list.files(pattern = "^PTek*")

#Check number of files for each dome type
a = c("open","vented","power")
for (i in a){
  
  dome_type <- list.files(pattern = str_c("*",i,"*"))
  print(str_c("There are ", length(dome_type), " ", i, " dome files"))
}

#Combine all data into one big dataframe
df <- data.frame()
for (i in temp) {
  tt<-str_split(i,'-')[[1]][5]
  #extract activity type from each filename
  activity <- str_split(tt, ' ')[[1]][3]
  #extract dome color type from each filename
  color_csv <- str_split(tt, ' ')[[1]][8]
  color<-gsub(".csv", "", color_csv)
  #extact dome type from each filename
  type <- str_split(tt, ' ')[[1]][6]
  
  t <-read.csv(i, header = T)
  t1 <- t%>%mutate(Activity = activity)%>%
    mutate(Color = color)%>%
    mutate(Type = type)%>%select(-RRi,-RRi.1)
  #select the first 3mins data, with excluding the first 30s
  t1<-t1%>%slice(32:212)
  
  #combine all files into a big one
  df <-bind_rows(t1,df)
}

#Sanity check the joint dataframe 
##make sure there is no unexpected value
unique(df$Color)
unique(df$Activity)
unique(df$Type)

#this will create a summarized table
#pay attention to the order or HR, HR.1 in raw csv file, 
#..where the 2nd argument in MAPE
#..and MAE should be gold-standard

#ICC function takes a single argument - dataframe, 
#..where the order of HR,HR.1 doesn't matter
df_sum_act <- df %>% 
  drop_na(HR.1,HR)%>% 
  group_by(Color, Activity, Type) %>% 
  summarise(MAPE = round(MAPE(HR,HR.1)*100,1),
            MAE = round(MAE(HR,HR.1),1),
            Pearson_Corr = round(cor(HR,HR.1),2),
            ICC = round(ICC(data.frame(HR,HR.1))$results[2,2],2))

formattable(df_sum_act)

#turn off the Activity group
df_sum <- df %>% 
  drop_na(HR.1,HR)%>% 
  group_by(Color, Type) %>% 
  summarise(MAPE = round(MAPE(HR,HR.1)*100,1),
            MAE = round(MAE(HR,HR.1),1),
            Pearson_Corr = round(cor(HR,HR.1),2),
            ICC = round(ICC(data.frame(HR,HR.1))$results[2,2],2))

formattable(df_sum)

#make a bar plot
##MAPE
g<-ggplot(df_sum_act,aes(x=Type,y=MAPE))
MAPE <- g + 
  geom_bar(stat='identity', 
           position=position_dodge(),
           aes(fill=Color),
           color='black') + 
  facet_grid(Activity~.) +
  labs(y="MAPE(%)")
MAPE
##ICC
g<-ggplot(df_sum_act,aes(x=Type,y=ICC))
ICC <- g + 
  geom_bar(stat='identity', 
           position=position_dodge(),
           aes(fill=Color),
           color='black') + 
  facet_grid(Activity~.) +
  labs(y="ICC")+
  geom_hline(yintercept=0.75,
             linetype="dashed",
             color="red")
ICC

#Sunlight
#Plot individual figure
setwd('C:/Users/57lzhang.US04WW4008/Desktop/Ear dome eva/All Sunlight')
temp<-list.files(pattern = "^PTek*")
for (i in temp) {
  #string operations
  tt<-str_split(i,'-')[[1]][3]
  activity <- str_split(tt, ' ')[[1]][3]
  location <- str_split(tt, ' ')[[1]][4]
  title<-paste(activity,location)
  #data operations
  t <- read.csv(i, header = T)
  p <-ggplot(t,aes(x=as.numeric(row.names(t))))+
    geom_line(aes(y=HR,color="darkred"),size=1)+
    geom_line(aes(y=HR.1,color="steelblue"),size=1)+
    ggtitle(title)+
    ylab("Heart Rate (bpm)")+
    xlab("Seconds(s)")+
    scale_color_discrete(name = "Sensor", 
                         labels = c("BiometRIC", "Polar"))+
    theme(axis.text=element_text(size=12),
          title = element_text(size=16),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16),
          legend.title = element_text(size=16),
          legend.text = element_text(size=16))
  print(p)
}
