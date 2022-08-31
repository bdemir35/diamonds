library(tidyverse)
library(skimr)
View(diamonds)
str(diamonds)
head(diamonds)

avg_price <- diamonds %>%
  summarize(price = mean(price))

avg_price_by_color <- diamonds %>%
  group_by(color) %>%
  summarize(price = mean(price),
            count = n()) %>%
  arrange(price)

avg_price_by_cut <- diamonds %>%
  group_by(cut) %>%
  summarize(price = mean(price),
            count = n()) %>%
  arrange(price)

avg_price_by_clarity <- diamonds %>%
  group_by(clarity) %>%
  summarize(price = mean(price),
            count =n()) %>%
  arrange(price)

# Avg_price_by_clarity plot

  diamonds %>%
  group_by(clarity) %>%
  summarize(price = mean(price)) %>%
  arrange(desc(price)) %>% 
  ggplot()+
  geom_point(aes(x = clarity, y = price, color = clarity), size = 6)+
  labs(x = "Clarity", y= "",title = "Avg Price by Clarity")+
  theme(plot.title.position = "plot")
  
  
# Avg_price_by_color plot
  
  diamonds %>% 
    group_by(color) %>% 
    summarize(avg_price = mean(price)) %>%
    arrange(desc(avg_price)) %>%
    ggplot() +
    geom_col(aes(x = color, y = avg_price, fill = color)) +
    labs(x = "Color", y= "Average Price", title = "Average Price by Diamond Color")+
    theme(plot.title.position = "plot")
  
  ggsave("avg_price_by_color.jpg", plot = last_plot())
  
# Avg_price_by_cut plot
  
  diamonds %>% 
    group_by(cut) %>% 
    summarize(avg_price = mean(price)) %>% 
    sort(avg_price, decreasing = TRUE) %>% 
    ggplot() +
    geom_col(aes(x = cut, y = avg_price, fill = cut), position = "stack", show.legend = FALSE)+
    labs(x = "Cut Style", y = "Average Price", title = "Average Price by Cut Style")+
    theme(plot.title.position = "plot")
  
  ggsave("avg_price_by_cut.jpg", plot = last_plot())
  
  
  
  # Looking distribution of the carat

  diamonds %>% 
    ggplot(mapping = aes(x = carat, y = price, color = cut))+
    geom_point()+
    geom_smooth()+
    labs(x = "Carat", y= "Price", title = "Carat vs Price Distrubition")+
    theme(plot.title.position = "plot")
 
  
  # Finding the Top 5 and Lowest price 5 diamonds in the dataset
  
  
top5price <- diamonds %>% 
  
  select(carat, cut, color, price) %>%
  arrange(desc(price)) %>% 
  top_n(5,) 

top5price %>% 
  ggplot(mapping = aes(x = carat, y = price, color =  color))+
  geom_point(size = 10)+
  labs(x = "Carat", y= "Price", title = "Top 5 Diamonds Price by Carat and Color")+
  theme(plot.title.position = "plot")

min5price <- diamonds %>% 
  select(carat, cut, color, price) %>% 
  arrange(price) %>% 
  head(5,)

min5price %>% 
  ggplot(mapping = aes(x = carat, y = price, color =  color))+
  geom_point(size = 10)+
  labs(x = "Carat", y= "Price", title = "Lowest 5 Diamonds Price by Carat and Color")+
  theme(plot.title.position = "plot")





  
 


