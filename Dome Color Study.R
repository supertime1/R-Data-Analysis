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
  #extract subject from each filename
  subject <- str_split(tt, ' ')[[1]][2]
  #extract dome color type from each filename
  color_csv <- str_split(tt, ' ')[[1]][8]
  color<-gsub(".csv", "", color_csv)
  #extact dome type from each filename
  type <- str_split(tt, ' ')[[1]][6]
  
  
  t <-read.csv(i, header = T)
  
  #find the correct HR column  
  
  Polar_HR_idx = grep("^Polar*",colnames(t))+1
  Bio_HR_idx = grep("^ENOS*",colnames(t))+1
  QoS_idx = grep("^ENOS*",colnames(t))+6
  
  #select polar and product HR column
  t0 <- t%>%select(polar_hr = colnames(t)[Polar_HR_idx],
                   b_hr = colnames(t)[Bio_HR_idx],
                   Timestamp,
                   QoS = colnames(t)[QoS_idx])
  
  t1 <- t0%>%
    mutate(Activity = activity)%>%
    mutate(Color = color)%>%
    mutate(Type = type)%>%
    mutate(Subject = toupper(subject))
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
unique(df$Subject)

#this will create a summarized table
#pay attention to the order or HR, HR.1 in raw csv file, 
#..where the 2nd argument in MAPE
#..and MAE should be gold-standard

#ICC function takes a single argument - dataframe, 
#..where the order of HR,HR.1 doesn't matter
df_sum <- df %>% 
  drop_na(b_hr,polar_hr)%>% 
  group_by(Color, Type, Activity,Subject) %>% 
  summarise(N_of_Points = n(),
            M_HR_P = round(mean(polar_hr),1),
            M_HR_B = round(mean(b_hr),1),
            M_QoS = round(mean(QoS),1),
            MAPE = round(MAPE(b_hr,polar_hr)*100,1),
            MAE = round(MAE(b_hr,polar_hr),1),
            Pearson_Corr = round(cor(b_hr,polar_hr),2),
            ICC = round(ICC(data.frame(b_hr,polar_hr))$results[2,2],2))

formattable(df_sum)


#make a barplot
#MAPE
g<-ggplot(df_sum,aes(x=Type,y=MAPE))
MAPE <- g + 
  geom_bar(stat='identity', 
           position=position_dodge(),
           aes(fill=Color),
           color='black') + 
  facet_grid(Activity~.) +
  labs(y="MAPE(%)")+
  geom_hline(yintercept=10,
             linetype="dashed",
             color="red")
MAPE


#ICC
g<-ggplot(df_sum,aes(x=Type,y=ICC))
MAPE <- g + 
  geom_bar(stat='identity', 
           position=position_dodge(),
           aes(fill=Color),
           color='black') + 
  facet_grid(Activity~.) +
  labs(y="ICC(%)")+
  geom_hline(yintercept=0.75,
             linetype="dashed",
             color="red")
MAPE


#make a boxplot
##MAPE
g<-ggplot(df_sum,aes(x=Type,y=MAPE))
MAPE <- g + 
  geom_boxplot(aes(fill=Color),
               color='black') + 
  facet_grid(Activity~.) +
  labs(y="MAPE(%)")+
  geom_hline(yintercept=10,
             linetype="dashed",
             color="red")
MAPE

##############################################
#Sunlight
##Make bar plot
sun_tb <-function(directory){
  
  setwd(directory)
  temp<-list.files(pattern = "^PTek*")
  df <- data.frame()
  
  for (i in temp) {
    #string operations
    tt<-str_split(i,'-')[[1]][3]
    activity <- str_split(tt, ' ')[[1]][3]
    location <- str_split(tt, ' ')[[1]][4]
    subject <- str_split(tt, ' ')[[1]][2]
    light <-str_split(tt, ' ')[[1]][5]
    device <- str_split(tt, ' ')[[1]][8]
    
    
    #data operations
    t <- read.csv(i, header = T)
    
    #find the correct HR column  
    
    Polar_HR_idx = grep("^Polar*",colnames(t))+1
    Bio_HR_idx = grep("^ENOS*",colnames(t))+1
    QoS_idx = grep("^ENOS*",colnames(t))+6
    
    
    #select polar and product HR column
    t0 <- t%>%select(polar_hr = colnames(t)[Polar_HR_idx],
                     b_hr = colnames(t)[Bio_HR_idx],
                     Timestamp,
                     QoS = colnames(t)[QoS_idx])
    
    t1 <- t0%>%
      mutate(Activity = activity)%>%
      mutate(Location = toupper(location))%>%
      mutate(Light = toupper(light))%>%
      mutate(Subject = subject)%>%
      mutate(Time = as.numeric(row.names(t0)))%>%
      mutate(Device = toupper(device))
    
    t1<-t1%>%slice(1:212)
    
    df <-bind_rows(t1,df)
  }
  
  return(df)

}

directory = 'C:/Users/57lzhang.US04WW4008/Desktop/Ear dome eva/All Sunlight'
df <-sun_tb(directory)

#sanity check
unique(df$Location)
unique(df$Light)
unique(df$Device)
unique(df$Subject)
unique(df$Activity)

#make a stats table
df_sum <- df %>% 
  drop_na(b_hr,polar_hr)%>% 
  group_by(Location, Light, Device, Subject) %>% 
  summarise(M_HR_P = round(mean(polar_hr),1),
            M_HR_B = round(mean(b_hr),1),
            M_QoS = round(mean(QoS),1),
            MAPE = round(MAPE(b_hr,polar_hr)*100,1),
            MAE = round(MAE(b_hr,polar_hr),1),
            Pearson_Corr = round(cor(b_hr,polar_hr),2),
            ICC = round(ICC(data.frame(b_hr,polar_hr))$results[2,2],2))
formattable(df_sum)


#make a bar plot
#MAPE
g<-ggplot(df_sum,aes(x=Device,y=MAPE))
MAPE <- g + 
  geom_bar(stat='identity', 
           position=position_dodge(),
           aes(fill=Light),
           color='black') + 
  facet_grid(Location~.) +
  labs(y="MAPE(%)")+
  geom_hline(yintercept=10,
             linetype="dashed",
             color="red")
MAPE

##ICC
g<-ggplot(df_sum,aes(x=Device,y=ICC))
ICC <- g + 
  geom_bar(stat='identity', 
           position=position_dodge(),
           aes(fill=Light),
           color='black') + 
  facet_grid(Location~.) +
  labs(y="ICC")+
  geom_hline(yintercept=0.75,
             linetype="dashed",
             color="red")
ICC


##QoS
g<-ggplot(df_sum,aes(x=Device,y=M_QoS))
QoS <- g + 
  geom_bar(stat='identity', 
           position=position_dodge(),
           aes(fill=Light),
           color='black') + 
  facet_grid(Location~.) +
  labs(y="QoS")
QoS


#make a boxplot
#MAPE
g<-ggplot(df_sum,aes(x=Device,y=MAPE))
MAPE <- g + 
  geom_boxplot(
           aes(fill=Light),
           color='black') + 
  facet_grid(Location~.) +
  labs(y="MAPE(%)")+
  geom_hline(yintercept=10,
             linetype="dashed",
             color="red")
MAPE

##ICC
g<-ggplot(df_sum,aes(x=Device,y=ICC))
ICC <- g + 
  geom_boxplot(
           aes(fill=Light),
           color='black') + 
  facet_grid(Location~.) +
  labs(y="ICC")+
  geom_hline(yintercept=0.75,
             linetype="dashed",
             color="red")
ICC


##QoS
g<-ggplot(df_sum,aes(x=Device,y=M_QoS))
QoS <- g + 
  geom_boxplot(
           aes(fill=Light),
           color='black') + 
  facet_grid(Location~.) +
  labs(y="QoS")
QoS

###########################################
#Plot individual figure
directory = 'C:/Users/57lzhang.US04WW4008/Desktop/Ear dome eva/All Sunlight/New folder'
df <-sun_tb(directory)

p <-ggplot(df,aes(x = Time))+
  geom_line(aes(y=b_hr,colour="BiometRic"),size=1)+
  geom_line(aes(y=polar_hr,colour="Polar"),size=1)+
  geom_line(aes(y=QoS, colour = "QoS"), size=1)+
  ggtitle("No Sunglass")+
  ylim(0,110)+
  ylab("Heart Rate (bpm) / QoS (%)")+
  xlab("Seconds(s)")+
  facet_grid(Subject~Location)+
  scale_color_discrete(name = "Device")+
  theme(axis.text=element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14))
print(p)



