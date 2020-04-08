

df1 %>% 
select(vowel, language, f1_cent, f1_20, f1_35, f1_50, f1_65, f1_80 ) %>% 
gather("formant", "f1" , - vowel, - language) %>% 
ggplot(aes(fill= formant, y=f1, x=vowel)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ language)
