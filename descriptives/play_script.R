hmcare_alldata %>%
  group_by(LAcode, AgeGRP) %>%  
  summarise(count = n()) %>%
  mutate(pct = count/sum(count)*100) %>%
  arrange(AgeGRP == "85+", -pct) %>%
  ggplot(aes(x = LAcode, y = pct, fill = AgeGRP)) +
  geom_col(position = "stack") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1, vjust = 1)) 


facet_plot <- function(df, x, y){
  
  plotstats <-                   
    df %>%
    count_(y, x) %>%         
    mutate_(pct = ~n/(sum(n)) * 100) %>%
    mutate_(tot = ~sum(n))
  
  plot <-            
    ggplot(df,aes_(x = x)) +
    geom_bar() +
    facet_wrap(y, scales = "free") +
    geom_text(data = plotstats, aes(label=percent(pct),
                                    y=pct), size=3.5, vjust = -1, 
              colour = "sky blue") +
    geom_text(data = plotstats,
              aes(label=paste0("Total number = ",yeartot), x = Inf, y= Inf),
              hjust = 1, vjust = 1, colour = "black") +
    theme_economist()
  plot
  
}

play <-
  hmcare_alldata %>%
  count(year, AgeGRP) %>%
  mutate(pct = n/(sum(n)) * 100) %>%
  mutate(tot = sum(n))

