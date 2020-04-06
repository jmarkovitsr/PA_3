library(tidyverse)
df1 = read_csv("data/vowel_data.csv")
#anipulate the dataframe as necessary so that you can calculate average F1/F2 centroids and trajectory length (include SD).

#media de las variables
df1 %>%
  summarise(media_f1 = mean(f1_cent), media_f2 = mean(f2_cent), meadia_tl = mean(tl)) 

#Create the following plots:
  



#See below for extra challenges

#trajectory length as a function of vowel and language
ggplot(df1, aes(fill= language, y=tl, x=vowel)) + 
  geom_bar(position="dodge", stat="identity")

#F1 as a function of vowel and language

ggplot(df1, aes(fill= language, y=f1_cent, x=vowel)) + 
  geom_bar(position="dodge", stat="identity")

#F2 as a function of vowel and language
ggplot(df1, aes(fill= language, y=f2_cent, x=vowel)) + 
  geom_bar(position="dodge", stat="identity")



#Plot trajectory length in F1 and F2 vowel space

ggplot(df1, aes(fill= f2_cent, y=f1_cent, x=tl)) + 
  geom_area ()

#Plot spectral centroids in F1/F2 vowel space

