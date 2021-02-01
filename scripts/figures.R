# Theme -------------------------------------------------------------------

source(here("scripts", "theme_jonas.R"))
theme_set(theme_jonas())

# Figures -----------------------------------------------------------------

melanoma %>% 
  ggplot(aes(x = ulcer, fill = status)) + 
  geom_bar(position = "fill") +
  theme(legend.title = element_blank()) + 
  labs(y = "Proportion")

ggsave(here("figures", "ulcer_deaths.jpg")) #saving the figure

melanoma %>% 
  ggplot(aes(x = sex, fill = status)) + 
  geom_bar(position = "fill") +
  theme(legend.title = element_blank()) + 
  labs(x = "", y = "Proportion")

ggsave(here("figures", "sex_deaths.jpg")) #saving the figure


p1 <- melanoma %>% 
  ggplot(aes(x = ulcer, fill = dead_melanoma_5yr)) + 
  geom_bar() + 
  theme(legend.position = "none")

p2 <- melanoma %>% 
  ggplot(aes(x = ulcer, fill = dead_melanoma_5yr)) + 
  geom_bar(position = "fill") + 
  ylab("proportion")

p1 + p2

ggsave(here("figures", "dead_melanoma_5yr.jpg"), width = 8, height = 6) #saving the figure
