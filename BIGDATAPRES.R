library(tidyverse)
library(corrplot)
library(car)
library(gamlss)

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

order <- c("GK", "CB", "RCB", "LCB", "RB", "LB", "LWB",
           "RWB", "CDM", "RDM", "LDM", "RCM", "LCM",
           "CM", "RM", "LM", "CAM", "RAM", "LAM", "RW", 
           "LW", "RF", "LF", "CF", "ST", "RS", "LS")

### Then we match it with our Position column in the data frame

fifa_ordered <- fifa %>% mutate(
  Position =  factor(Position, levels = order)) %>%
  arrange(Position) 
 
fifa_ordered <- fifa_ordered[-c(1:1900),] ### Let's drop all rows dedicated to goalkeepers

### Now that they are ordered, let's check if offensive and defensive positions
### have different average overalls

fifa_ordered %>% group_by(Position) %>% 
  summarise(Overall = mean(Overall)) %>% 
  ggplot(mapping = aes(Overall, Position)) +
  geom_point()

### It does indeed seem like there is are higher average overalls
### for offensive positions rather than defensive

### Let's add a new column where we group all of the positions in:
### "Defender", "Midfielder", "Striker"

fifa_ordered2 <- fifa_ordered %>% mutate(Position = case_when(
  Position %in% c("CB", "RCB", "LCB", "RB", "LB", "LWB", "RWB") ~ "Defender",
  Position %in% c("CDM", "RDM", "LDM", "RCM", "LCM", "CM", "RM", "LM", "CAM", "RAM", "LAM") ~ "Midfielder",
  Position %in% c("RW", "LW", "RF", "LF", "CF", "ST", "RS", "LS") ~ "Striker"
))

### Now we can check the distribution of overalls for each role

ggplot(fifa_ordered2, aes(x = Position, y = Overall, fill = Position)) +
  geom_boxplot() +
  labs(title = "Overall distribution per position",
       x = "Position",
       y = "Overall") +
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# 3) Correlations and model building --------------------------------------


cor_matrix <- cor(fifa_ordered2[, c("Overall", "Crossing", "Finishing", "HeadingAccuracy", "ShortPassing", "Volleys", 
                            "Dribbling", "Curve", "FKAccuracy", "LongPassing", "BallControl", "Acceleration", 
                            "SprintSpeed", "Agility", "Reactions", "Balance", "ShotPower", "Jumping", "Stamina", 
                            "Strength", "LongShots", "Aggression", "Interceptions", "Positioning", "Vision", 
                            "Penalties", "Composure", "Marking", "StandingTackle", "SlidingTackle")], 
                  use = "complete.obs")

corrplot(cor_matrix, method = "circle")

### Some variables have very high correlation and are conceptually very similar. We can combine them.
### Variables to combine: Sliding and standing tackle (Tackle); Long passing, short passing, crossing and vision (passing)
### Acceleration and sprint speed (speed); Ball control, dribbling (Technique); Finishing, Volleys, shot power, long shots (Shooting)
### Agility and balance (Equilibrium); Aggression, Interceptions and marking (Ball recovery)

### Since these variables are highly positevily correlated, we can say that, on average,
### If one is high, the other is high aswell, and viceversa. So, we can just combine them
### by doing an average

fifa_ordered3 <- fifa_ordered2 %>% 
  mutate(Tackle = (SlidingTackle + StandingTackle)/2) %>%
  mutate(Passing = (LongPassing + ShortPassing + Crossing + Vision)/4) %>%
  mutate(Speed = (Acceleration + SprintSpeed)/2) %>%
  mutate(Technique = (BallControl + Dribbling)/2) %>%
  mutate(Shooting = (Finishing + Volleys + ShotPower + LongShots)/4) %>%
  mutate(Equilibrium = (Agility + Balance)/2) %>%
  mutate(BallRecovery = (Aggression + Interceptions + Marking)/3) %>%
  select(!c(SlidingTackle, StandingTackle, LongPassing, ShortPassing, 
             Crossing, Vision, Acceleration , SprintSpeed, BallControl, 
             Dribbling, Finishing, Volleys, ShotPower, LongShots, Agility,
             Balance, Aggression, Interceptions, Marking,
             GKDiving, GKHandling, GKKicking, GKPositioning,
             GKReflexes, Release.Clause)) ### We erased the variables we combined, and removed other unnecessary ones

### Now, let's check for correlations again (only of the new variables we used)

cor_matrix2 <- cor(fifa_ordered3[, c("Tackle", "Passing", "Speed",
                                     "Technique", "Shooting", "Equilibrium",
                                     "BallRecovery")], 
                  use = "complete.obs")

corrplot(cor_matrix2, method = "circle")

### There is still very high correlation between many of the covariates, 
### let's see if we can improve things

fifa_ordered3 <- fifa_ordered2 %>% 
  mutate(Aggressiveness = (SlidingTackle + StandingTackle + 
                             Aggression + Interceptions + Marking)/5) %>%
  mutate(Speed = (Acceleration + SprintSpeed + Agility + Balance)/4) %>%
  mutate(Technique = (BallControl + Dribbling + LongPassing + 
                        ShortPassing + Crossing + Vision)/6) %>%
  mutate(Shooting = (Finishing + Volleys + ShotPower + LongShots)/4) %>%
  select(!c(SlidingTackle, StandingTackle, LongPassing, ShortPassing, 
            Crossing, Vision, Acceleration , SprintSpeed, BallControl, 
            Dribbling, Finishing, Volleys, ShotPower, LongShots, Agility,
            Balance, Aggression, Interceptions, Marking,
            GKDiving, GKHandling, GKKicking, GKPositioning,
            GKReflexes, Release.Clause))

cor_matrix2 <- cor(fifa_ordered3[, c("Aggressiveness", "Speed",
                                     "Technique", "Shooting")], 
                   use = "complete.obs")

corrplot(cor_matrix2, method = "circle")

### There is still high correlation between some of the covariates but it looks like
### an improvement. 