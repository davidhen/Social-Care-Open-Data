library(dplyr)
library(ggplot2)

ID <- sample(1:32, 128, replace = TRUE)
AgeGrp <- sample(c("18-65", "65-75", "75-85", "85+"), 128, replace = TRUE)
ID <- factor(ID)
AgeGrp <- factor(AgeGrp)
data <- data_frame(ID, AgeGrp)
data
#################################################################################
plotstats <-                     
  data %>%                    
  group_by(AgeGrp) %>%                
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)  

age_plot <-                 
  ggplot(data,aes(x = AgeGrp)) +
  geom_bar() + 
  geom_text(data = plotstats, aes(label=paste0(round(pct,1),"%"), #Add labels from above
                                      y=pct), size=3.5, vjust = -1, colour = "sky blue") +
  ggtitle("Count of Age Group")
age_plot

###############################################################################
basic_plot <- function(df, x){
  
  plotstats <-
    df %>%
    group_by_(x) %>%               
    summarise_(count = ~n()) %>%
    mutate_(pct = ~count/sum(count)*100)
  
  plot <-                   
    ggplot(df,aes_(x = x)) +
    geom_bar() + 
    geom_text(data = plotstats, aes(label=paste0(round(pct,1),"%"),
                                        y=pct), size=3.5, vjust = -1, colour = "sky blue")
  
  plot

}

facet_plot(hmcare2010, quote(AgeGRP), quote(HCclient))


basic_plot(data, quote(AgeGrp))
###############################################################################
facet_plot <- function(df, x, y){
  
  plotstats <-                   
    df %>%
    group_by_(y, x) %>%         
    summarise_(count = ~n()) %>%
    mutate_(pct = ~count/sum(count)*100)
  
  plot <-            
    ggplot(df,aes_(x = x)) +
    geom_bar() + 
    facet_wrap(y) +
    geom_text(data = plotstats, aes(label=paste0(round(pct,1),"%"),
                                    y=pct), size=3.5, vjust = -1, colour = "sky blue")
  plot
  
}

facet_plot(hmcare2010, quote(AgeGRP), quote(HCclient))
###################################################################################

#Need to think about this one. 

order <-                               
  AgeGRP10stats_byLA %>%
  arrange(AgeGRP, -pct)
ordered_LAs <- ordered_age$LAcode[97:128]  

age_plot_stacked_age_LA <-
  ggplot(ordered_age, aes(x = LAcode, y = pct, fill = AgeGRP)) +
  geom_col(position = "stack") +
  scale_x_discrete(limits = ordered_LAs) +   
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1)) +
  ggtitle("Stacked proportions AgeGRP 2010 ordered by proportion of 85+") +
  xlab("Local Authority") +
  ylab("Perecentage receiving homecare")
age_plot_stacked_age_LA

#################################################################################

###NSE
basic_plot <- function(df, x){
  
  plotstats <-
    df %>%
    group_by(x) %>%               
    summarise(count = n()) %>%
    mutate(pct = count/sum(count)*100)
  
  plot <-                   
    ggplot(df,aes(x = x)) +
    geom_bar() + 
    geom_text(data = plotstats, aes(label=paste0(round(pct,1),"%"),
                                    y=pct), size=3.5, vjust = -1, colour = "sky blue")
  
  plot
  
}

basic_plot(data, quote(AgeGrp))