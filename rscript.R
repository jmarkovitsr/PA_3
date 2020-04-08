library(tidyverse)
vowel = read_csv("data/vowel_data.csv")
#anipulate the dataframe as necessary so that you can calculate average F1/F2 centroids and trajectory length (include SD).


vowel_means = vowel %>% 
  group_by(vowel, language) %>% 
  summarize(f1_cent = mean(f1_cent), f2_cent = mean(f2_cent)) %>% 
  ungroup() %>% 
  mutate(order = case_when(vowel == "i" ~ 1, vowel == "a" ~ 2, TRUE ~ 3), 
         vowel = forcats::fct_reorder2(vowel, vowel, order)) %>% 
  arrange(order)

vowel %>% 
  mutate(vowel = forcats::fct_relevel(vowel, "u", "a", "i")) %>% 
  ggplot(., aes(x = f2_cent, y = f1_cent, color = language, label = vowel)) + 
  geom_text(size = 3.5, alpha = 0.6, show.legend = F) + 
  geom_path(data = vowel_means, aes(group = language, lty = language), 
            color = "grey") + 
  geom_text(data = vowel_means, show.legend = F, size = 7) + 
  scale_y_reverse() + 
  scale_x_reverse() + 
  scale_color_brewer(palette = "Set1") + 
  labs(title = "Vowel space comparison", 
       subtitle = "Spectral centroids of English/Spanish cardinal vowels", 
       y = "F1 (hz)", x = "F2 (hz)") + 
  theme_minimal(base_size = 16)




#media de las variables
df1 %>%
  summarise(media_f1 = mean(f1_cent), media_f2 = mean(f2_cent), meadia_tl = mean(tl)) 

#Create the following plots:
  



#See below for extra challenges

#trajectory length as a function of vowel and language
group_by(vowel) %>%
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

vowel%>% 
  filter(vowel == "L2SPANISH" | GROUP == "L2HEB", Frequent == "f") %>% 
  ggplot( aes(x= EPT, y=MINT, fill = factor(EPT))) + 
  geom_boxplot() +
  coord_flip()

vowels %>% 
  mutate(vowel = forcats::fct_relevel(vowel, "u", "a", "i")) %>% 
  ggplot(., aes(x = f2_cent, y = f1_cent, color = language, label = vowel)) + 
  geom_text(size = 3.5, alpha = 0.6, show.legend = F) + 
  geom_path(data = vowel_means, aes(group = language, lty = language), 
            color = "grey") + 
  geom_text(data = vowel_means, show.legend = F, size = 7) + 
  scale_y_reverse() + 
  scale_x_reverse() + 
  scale_color_brewer(palette = "Set1") + 
  labs(title = "Vowel space comparison", 
       subtitle = "Spectral centroids of English/Spanish cardinal vowels", 
       y = "F1 (hz)", x = "F2 (hz)") + 
  theme_minimal(base_size = 16)

vowel %>% 
  mutate(vowel = forcats::fct_relevel(vowel, "u", "a", "i")) %>%
  ggplot(df1, aes(fill= language, y=vowel, x = vowel)) + 
  geom_bar(position="dodge", stat="identity")

vowel_f1 = separate(data = vowel, col = vowel, into = c("f1_cent", "f1_20", "f1_35", "f1_50", "f1_65", "f1_80"), sep = "_") 
vowel_df1  
vowel %>% 
  mutate(vowel = forcats::fct_relevel(vowel, "f1_cent", "f1_20", "f1_35", "f1_50", "f1_80")) %>% 
  ggplot(df1, aes(fill= language, y=f1_cent, x = vowel)) + 
  geom_bar(position="dodge", stat="identity")

# get traj legth for f1
mutate(vowel, traj_length_f1 = (f1_20) + (f1_35) + (f1_50) + (f1_65) + (f1_80))

# make it an object
traj_length_f1 <- mutate(vowel, traj_length_f1 = (f1_20) + (f1_35) + (f1_50) + (f1_65) + (f1_80))

# get traj legth for f2
mutate(vowel, traj_length_f2 = (f2_20) + (f2_35) + (f2_50) + (f2_65) + (f2_80))

# make it an object
traj_length_f2 <- mutate(vowel, traj_length_f2 = (f2_20) + (f2_35) + (f2_50) + (f2_65) + (f2_80))

# plot for f1 as a function of vowel and language
vowel %>%  
  ggplot(., aes(x = language, y = f1_cent)) +
  geom_boxplot() +
  facet_grid(. ~ vowel)
 
  ggplot(., aes(x = language, y = traj_length_f1)) +
  geom_boxplot() +
  facet_grid(. ~ vowel)
  
  traj_length_f1 %>% 
  ggplot(., aes(fill= language, y=traj_length_f1, x=vowel)) + 
  geom_bar(position="dodge", stat="identity")

  traj_length_f1 %>% 
    ggplot(., aes(fill= language, y=traj_length_f1, x=vowel)) +
  geom_boxplot() +
    facet_grid(. ~ vowel)
  