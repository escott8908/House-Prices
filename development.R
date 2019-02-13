
# play with forcats -------------------------------------------------------

a = char_cols[1]
ggplot(impdata, aes(x = get(a) %>% fct_infreq())) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

impdata %>% mutate_if(is.factor, fct_infreq())
