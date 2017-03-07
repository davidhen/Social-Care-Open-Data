stack_plot <- function(df, LAcode, stackvar, start, finish){
  plotstats <-                   
  df %>%
  group_by_(LAcode, stackvar) %>%         
  summarise_(count = ~n()) %>%
  mutate_(pct = ~count/sum(count)*100) %>%
  arrange_(LAcode, -pct)
  
ordered_LAs <- plotstats$y[start:finish] 

plot <-
  ggplot(plotstats, aes_(x = LAcode, y = pct, fill = stackvar)) +
  geom_col(position = "stack") +
  scale_x_discrete(limits = ordered_LAs) +   
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1)) 

plot
}


facet_plot <- function(df, x, y){
  
  plotstats <-                   
    df %>%
    group_by_(y, x) %>%         
    summarise_(count = ~n()) %>%
    mutate_(pct = ~count/sum(count)*100) %>%
    mutate_(yeartot = ~sum(count))
  
  plot <-            
    ggplot(df,aes_(x = x)) +
    geom_bar() +
    facet_wrap(y, scales = "free") +
    geom_text(data = plotstats, aes(label=paste0(round(pct,1),"%"),
                                    y=pct), size=3.5, vjust = -1, 
              colour = "sky blue") +
    geom_text(data = plotstats,
              aes(label=paste0("Total number = ",yeartot), x = Inf, y= Inf),
              hjust = 1, vjust = 1, colour = "black")
  plot
  
}

```{r stacked_bandHRSTT_byLA, fig.width=10}
stackstats <-                   
  hmcare_alldata_simplified %>%
  group_by(LAcode, bandHRSTT) %>%         
  summarise(count = n()) %>%
  mutate(pct = count/sum(count)*100)

ordered_bandHRSTT <-                   #Create a vector with LAs ordered by <4hrs
  stackstats %>%
  arrange(bandHRSTT, -pct)
ordered_LAs <- ordered_bandHRSTT$LAcode[1:32] # Order by <4

bandHRSTT_stacked_LA <-
  ggplot(stackstats, aes(x = LAcode, y = pct, fill = bandHRSTT)) +
  geom_col(position = "stack") +
  scale_x_discrete(limits = ordered_LAs) +   #using above vector
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1)) +
  ggtitle("Total Home Care Hours 2010-2012, ordered by proportion of <4hrs") +
  xlab("Local Authority") +
  ylab("Perecentage receiving homecare")
bandHRSTT_stacked_LA
```