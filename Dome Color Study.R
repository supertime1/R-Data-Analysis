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
  
  #find the correct HR column  
  
  Polar_HR_idx = grep("^Polar*",colnames(t))+1
  Bio_HR_idx = grep("^ENOS*",colnames(t))+1
  
  
  #select polar and product HR column
  t0 <- t%>%select(polar_hr = colnames(t)[Polar_HR_idx],
                   b_hr = colnames(t)[Bio_HR_idx],
                   Timestamp)
  
  t1 <- t0%>%
    mutate(Activity = activity)%>%
    mutate(Color = color)%>%
    mutate(Type = type)
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
  drop_na(b_hr,polar_hr)%>% 
  group_by(Color, Activity, Type) %>% 
  summarise(M_HR_P = round(mean(polar_hr),1),
            M_HR_B = round(mean(b_hr),1),
            MAPE = round(MAPE(b_hr,polar_hr)*100,1),
            MAE = round(MAE(b_hr,polar_hr),1),
            Pearson_Corr = round(cor(b_hr,polar_hr),2),
            ICC = round(ICC(data.frame(b_hr,polar_hr))$results[2,2],2))

formattable(df_sum_act)

#turn off the Activity group
df_sum <- df %>% 
  drop_na(b_hr,polar_hr)%>% 
  group_by(Color, Type) %>% 
  summarise(M_HR_P = round(mean(polar_hr),1),
            M_HR_B = round(mean(b_hr),1),
            MAPE = round(MAPE(b_hr,polar_hr)*100,1),
            MAE = round(MAE(b_hr,polar_hr),1),
            Pearson_Corr = round(cor(b_hr,polar_hr),2),
            ICC = round(ICC(data.frame(b_hr,polar_hr))$results[2,2],2))

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
##############################################
#Sunlight
##Make bar plot
setwd('C:/Users/57lzhang.US04WW4008/Desktop/Ear dome eva/All Sunlight')
temp<-list.files(pattern = "^PTek*")


#Combine all data into one big dataframe
df <- data.frame()
for (i in temp) {
  tt<-str_split(i,'-')[[1]][3]
  #extract activity type from each filename
  location <- str_split(tt, ' ')[[1]][4]
  #extract dome color type from each filename
  light <- str_split(tt, ' ')[[1]][5]
  
  device_csv <-str_split(tt, ' ')[[1]][8]
  device<-gsub(".csv", "", device_csv)
  
  
  t <-read.csv(i, header = T)
  
  #find the correct HR column  
  
  Polar_HR_idx = grep("^Polar*",colnames(t))+1
  Bio_HR_idx = grep("^ENOS*",colnames(t))+1
  
  
  #select polar and product HR column
  t0 <- t%>%select(polar_hr = colnames(t)[Polar_HR_idx],
                   b_hr = colnames(t)[Bio_HR_idx],
                   Timestamp)
  
  t1 <- t0%>%
    mutate(Location = toupper(location))%>%
    mutate(Light = toupper(light))%>%
    mutate(Device = toupper(device))
  #select the first 3mins data, with excluding the first 30s
  t1<-t1%>%slice(32:212)
  
  #combine all files into a big one
  df <-bind_rows(t1,df)
}

#sanity check
unique(df$Location)
unique(df$Light)
unique(df$Device)

#make a stats table
df_sum <- df %>% 
  drop_na(b_hr,polar_hr)%>% 
  group_by(Location, Light, Device) %>% 
  summarise(M_HR_P = round(mean(polar_hr),1),
            M_HR_B = round(mean(b_hr),1),
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
  labs(y="MAPE(%)")
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
###########################################
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
  
  #find the correct HR column  
  
  Polar_HR_idx = grep("^Polar*",colnames(t))+1
  Bio_HR_idx = grep("^ENOS*",colnames(t))+1
  
  
  #select polar and product HR column
  t0 <- t%>%select(polar_hr = colnames(t)[Polar_HR_idx],
                   b_hr = colnames(t)[Bio_HR_idx],
                   Timestamp)
  
  t1 <- t0%>%
    mutate(Activity = activity)%>%
    mutate(Color = color)%>%
    mutate(Type = type)
  
  p <-ggplot(t1,aes(x=as.numeric(row.names(t1))))+
    geom_line(aes(y=b_hr,color="darkred"),size=1)+
    geom_line(aes(y=polar_hr,color="steelblue"),size=1)+
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


