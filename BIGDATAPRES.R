library(tidyverse)

# 1) Open and clean the file -------------------------------------------------

fifa <- read.csv("FIFA.csv")

### Let's remove unnecessary variables
### We can remove Wage; Special; International.Reputation; Contract.Valid.Until;
### Height, Weight; 

fifa <- fifa %>% select(!c(Wage, Special, International.Reputation,
                           Contract.Valid.Until, Height, Weight))
### With the "!! operator we are basically keeping all the variables except the
### ones we wrote in the code

### Now let's check for NA's

fifa %>% summarise(count = sum(is.na(fifa)))
### There are many NA's, let's drop them

fifa <- drop_na(fifa) 
### We lost some observations which were players that were without some stats.
### We can't fill the up with average values, or other kind of values, because
### each player has its own unique stats


# 2) Visualise the data ---------------------------------------------------

### I would like to check the distribution of overalls depending on the positions
### First, let's order the positions from defensive to offensive

### We have to create a vector with the desired order

order <- c("GK", "CB", "RCB", "LCB", "RB", "LB", "CDM", 
           "RDM", "LDM", "LWB", "RWB", "RCM", "LCM",
           "CM", "RM", "LM", "CAM", "RAM", "LAM", "RW", 
           "LW", "RF", "LF", "CF", "ST", "RS", "LS")

### Then we match it with our Position column in the data frame

fifa_ordered <- fifa %>% mutate(
  Position =  factor(Position, levels = order)) %>%
  arrange(Position)

### Now that they are ordered, let's check if offensive and defensive positions
### have different average overalls

fifa_ordered %>% group_by(Position) %>% 
  summarise(Overall = mean(Overall)) %>% 
  ggplot(mapping = aes(Overall, Position)) +
  geom_point()

### It does indeed seem like there is are higher average overalls
### for offensive positions rather than defensive


