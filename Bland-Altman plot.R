library(BlandAltmanLeh)
library(tidyr)
library(stringr)#String operations
library(dplyr)
library(ggplot2)
library(ggExtra)

setwd('C:/Users/57lzhang.US04WW4008/Desktop/Floyer data/Gen2/')
temp<-list.files(pattern = "^PTek*")


#Check number of files for each activity
activity_ls = c("sitting","talking","chewing","drinking","housework",
      "walking","running","biking")
for (i in a){
  
  product_type <- list.files(pattern = str_c("*",i,"*"))
  print(str_c("There are ", length(product_type), " ", i, " files"))
}

#Combine all data into one big dataframe
df <- data.frame()
for (i in temp) {
  tt<-str_split(i,'-')[[1]][3]
  #extract subject from each filename
  subject <- str_split(tt, ' ')[[1]][2]
  #extract activity type from each filename
  activity <- str_split(tt, ' ')[[1]][3]

  
  t <-read.csv(i, header = T)
  
  #find the correct HR column  
  Polar_HR_idx = grep("^Polar*",colnames(t))+1
  Bio_HR_idx = grep("^ENOS*",colnames(t))+1
  
  #select polar and product HR column
  t0 <- t%>%select(polar_hr = colnames(t)[Polar_HR_idx],
                   bio_hr = colnames(t)[Bio_HR_idx],
                   Timestamp)
  
  #add two columns: activity and product names
  t1 <- t0%>%mutate(Activity = activity)%>%
    mutate(Subject= subject)%>%
    mutate(diff= bio_hr - polar_hr)
  #select the first 3mins data, with excluding the first 30s
  
  t1<-t1%>%slice(32:212)
  
  #combine all files into a big one
  df <-bind_rows(t1,df)
}

#sanity check of the joint dataframe
unique(df$Activity)
unique(df$Subject)


df<-df%>%drop_na(polar_hr,bio_hr)

#####################################
#####################################
#Plot BA plot for all subjects and activities
ba.stats<-bland.altman.stats(df$polar_hr, df$bio_hr)

#calculate how many percent points are out of two LOAs
higher <- df%>%filter(diff > ba.stats$lines[3])
lower <- df%>%filter(diff<ba.stats$lines[1])
outlier_per<- 100 * round((nrow(higher) + nrow(lower))/nrow(df),2)

#plot BA plot for all subjects and all activities
overall_plot<-ggplot(df, aes(polar_hr, diff, col = Activity))+
  geom_point() + 
  labs(x='Polar HR(bpm)', 
       y='Bio - Polar(bpm)',
       title = 'Bland-Altman Plot of all subjects and all activities',
       subtitle = 'BiometRIC Gen 2 vs Polar H10',
       caption = paste(paste('Mean difference: ',round(ba.stats$lines[2],1),'\n'),
                       paste('Lower LOA: ',round(ba.stats$lines[1],1),'\n'),
                       paste('Upper LOA: ',round(ba.stats$lines[3],1),'\n'),
                       paste('There are ', outlier_per, '% points outside of lower and upper LOA lines \n')))+
  geom_hline(yintercept = ba.stats$lines, lty=c(2,2,2), 
       col=c("blue","red","blue"), 
       lwd=c(1,1,1))+
  geom_count()+
  scale_size_area()+
  geom_hline(yintercept = ba.stats$CI.lines, 
             lty=c(1,1,1,1,1,1),
             col=c("lightblue","lightblue","pink","pink","lightblue","lightblue"),
             lwd=c(1,1,1,1,1,1))
print(overall_plot)
ggMarginal(overall_plot, type = 'histogram', fill ="transparent")

##########################################
###########################################
#Plot BA plot for all subjects per activities
for(i in unique(df$Activity)){
  df_i <- df%>%filter(Activity == i)
  ba.stats<-bland.altman.stats(df_i$polar_hr, df_i$bio_hr)
  
  #calculate how many percent points are out of two LOAs
  higher <- df_i%>%filter(diff > ba.stats$lines[3])
  lower <- df_i%>%filter(diff<ba.stats$lines[1])
  outlier_per<- 100 * round((nrow(higher) + nrow(lower))/nrow(df_i),2)
  
  #make ggplot
  activity_plot_i<-ggplot(df_i, aes(polar_hr, diff,col=Subject))+
    geom_point() + 
    labs(x='Polar HR(bpm)', y='Bio - Polar(bpm)')+
    labs(x='Polar HR(bpm)', 
         y='Bio - Polar(bpm)',
         title = paste('Bland-Altman Plot of',toupper(i)),
         subtitle = 'BiometRIC Gen 2 vs Polar H10, all subjects',
         caption = paste(paste('Mean difference: ',round(ba.stats$lines[2],1),'\n'),
                         paste('Lower LOA: ',round(ba.stats$lines[1],1),'\n'),
                         paste('Upper LOA: ',round(ba.stats$lines[3],1),'\n'),
                         paste('There are ', outlier_per, '% points outside of lower and upper LOA lines \n')))+
    geom_hline(yintercept = ba.stats$lines, lty=c(2,2,2), 
               col=c("blue","red","blue"), 
               lwd=c(1,1,1))+
    geom_count()+
    scale_size_area()+
    geom_hline(yintercept = ba.stats$CI.lines, 
               lty=c(1,1,1,1,1,1),
               col=c("lightblue","lightblue","pink","pink","lightblue","lightblue"),
               lwd=c(1,1,1,1,1,1))

  print(ggMarginal(activity_plot_i, type = 'histogram', fill ="transparent"))
}

#####################################
######################################
#Plot BA plot for all activities per subject
for(i in unique(df$Subject)){
  df_i <- df%>%filter(Subject == i)
  ba.stats<-bland.altman.stats(df_i$polar_hr, df_i$bio_hr)
  
  #calculate how many percent points are out of two LOAs
  higher <- df_i%>%filter(diff > ba.stats$lines[3])
  lower <- df_i%>%filter(diff<ba.stats$lines[1])
  outlier_per<- 100 * round((nrow(higher) + nrow(lower))/nrow(df_i),2)
  
  #make ggplot
  subject_plot_i<-ggplot(df_i, aes(polar_hr, diff, col=Activity))+
    geom_point() + 
    labs(x='Polar HR(bpm)', 
         y='Bio - Polar(bpm)',
         title = paste('Bland-Altman Plot of Subject',i),
         subtitle = 'BiometRIC Gen 2 vs Polar H10, all activities',
         caption = paste(paste('Mean difference: ',round(ba.stats$lines[2],1),'\n'),
                         paste('Lower LOA: ',round(ba.stats$lines[1],1),'\n'),
                         paste('Upper LOA: ',round(ba.stats$lines[3],1),'\n'),
                         paste('There are ', outlier_per, '% points outside of lower and upper LOA lines \n')))+
    geom_hline(yintercept = ba.stats$lines, lty=c(2,2,2), 
               col=c("blue","red","blue"), 
               lwd=c(1,1,1))+
    geom_count()+
    scale_size_area()+
    geom_hline(yintercept = ba.stats$CI.lines, 
               lty=c(1,1,1,1,1,1),
               col=c("lightblue","lightblue","pink","pink","lightblue","lightblue"),
               lwd=c(1,1,1,1,1,1))
  
  print(ggMarginal(subject_plot_i, type = 'histogram', fill ="transparent"))
}