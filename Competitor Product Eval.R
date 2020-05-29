##This script analysis the data from competitor product:\
##Bose, xxx as compared to Polar H10 ECG chest belt


library(dplyr)
library(tidyr)
library(MLmetrics)#MAPE, MAE
library(ggplot2)
library(stringr)#String operations
library(psych)#ICC
library(formattable)#make a table

#extract data
setwd('C:/Users/57lzhang.US04WW4008/Desktop/Competitor Product Eval/Indoor')
temp<-list.files(pattern = "^PTek*")


#Check number of files for each dome type
a = c("Jabra","Philips","Bose")
for (i in a){
  
  dome_type <- list.files(pattern = str_c("*",i,"*"))
  print(str_c("There are ", length(dome_type), " ", i, " files"))
}

#Combine all data into one big dataframe
df <- data.frame()
for (i in temp) {
  tt<-str_split(i,'-')[[1]][3]
  #extract activity type from each filename
  activity <- str_split(tt, ' ')[[1]][3]
  #extract product type from each filename
  product <- str_split(tt, ' ')[[1]][5]
  
  t <-read.csv(i, header = T)

  #find the correct HR column  
  if(any(grepl("^HR.Bose*", colnames(t))) == TRUE){
    Polar_HR_idx = grep("^Polar*",colnames(t))+1
    Product_HR_idx = grep("^HR.Bose*",colnames(t))+1
  }
  
  if(any(grepl("^Jabra*", colnames(t))) == TRUE){
    Polar_HR_idx = grep("^Polar*",colnames(t))+1
    Product_HR_idx = grep("^Jabra*",colnames(t))+1
  }
  
  if(any(grepl("^Philips*", colnames(t))) == TRUE){
    Polar_HR_idx = grep("^Polar*",colnames(t))+1
    Product_HR_idx = grep("^Philips*",colnames(t))+1
  }
  
  #select polar and product HR column
  t0 <- t%>%select(polar_hr = colnames(t)[Polar_HR_idx],
                   p_hr = colnames(t)[Product_HR_idx],
                   Timestamp)
  
  #add two columns: activity and product names
  t1 <- t0%>%mutate(Activity = activity)%>%
    mutate(Product = product)
  #select the first 3mins data, with excluding the first 30s
  
  t1<-t1%>%slice(32:212)
  
  #combine all files into a big one
  df <-bind_rows(t1,df)
}

#Sanity check the joint dataframe 
##make sure there is no unexpected value
unique(df$Activity)
unique(df$Product)

#this will create a summarized table
#pay attention to the order or HR, HR.1 in raw csv file, 
#..where the 2nd argument in MAPE
#..and MAE should be gold-standard

#ICC function takes a single argument - dataframe, 
#..where the order of HR,HR.1 doesn't matter

df_sum_act <- df %>% 
  drop_na(polar_hr,p_hr)%>% 
  group_by(Product, Activity) %>% 
  summarise(MAPE = round(MAPE(p_hr,polar_hr)*100,1),
            MAE = round(MAE(p_hr,polar_hr),1),
            Pearson_Corr = round(cor(p_hr,polar_hr),2),
            ICC = round(ICC(data.frame(p_hr,polar_hr))$results[2,2],2))

formattable(df_sum_act)

#turn off the Activity group
df_sum <- df %>% 
  drop_na(polar_hr,p_hr)%>% 
  group_by(Product) %>% 
  summarise(MAPE = round(MAPE(p_hr,polar_hr)*100,1),
            MAE = round(MAE(p_hr,polar_hr),1),
            Pearson_Corr = round(cor(p_hr,polar_hr),2),
            ICC = round(ICC(data.frame(p_hr,polar_hr))$results[2,2],2))

formattable(df_sum)

#make a bar plot
##MAPE
g<-ggplot(df_sum_act,aes(x=Product,y=MAPE))
MAPE <- g + 
  geom_bar(stat='identity', 
           position=position_dodge(),
           aes(fill=Product),
           color='black') + 
  facet_grid(Activity~.) +
  labs(y="MAPE(%)")
MAPE
##ICC
g<-ggplot(df_sum_act,aes(x=Product,y=ICC))
ICC <- g + 
  geom_bar(stat='identity', 
           position=position_dodge(),
           aes(fill=Product),
           color='black') + 
  facet_grid(Activity~.) +
  labs(y="ICC")+
  geom_hline(yintercept=0.75,
             linetype="dashed",
             color="red")
ICC

