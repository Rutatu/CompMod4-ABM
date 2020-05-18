Agent based model of the role of social support in the development of
Major Depressive Disorder
================
Ruta Slivkaite

``` r
#Setting directory and libraries

setwd("~/Rutos SmuTkes/Cognitive Science/4th Semester/Social and cultural dynamics/ABM models")


library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.0     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   0.8.5
    ## v tidyr   1.0.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(ggplot2)
```

Step 1: Creating groups of agents with their properties

``` r
#creating 1st agent in low vulnerability group
Agent_Low <- data.frame(AgentNo = 1,
                     Perceived_Support = runif(1,0,10),
                     Vulnerability_Group = "L",
                     Distress = 0,
                     MDD_onset ="",
                     Appropriate_Received_support_count = 0,
                     Inappropriate_Received_support_count = 0,
                     stringsAsFactors = FALSE)

#creating 1st agent in high vulnerability group
Agent_High<- data.frame(AgentNo = 1,
                     Perceived_Support = runif(1,0,10),
                     Vulnerability_Group = "H",
                     Distress = 0,
                     MDD_onset ="",
                     Appropriate_Received_support_count = 0,
                     Inappropriate_Received_support_count = 0,
                     stringsAsFactors = FALSE)


#determining the size of the populations
nPop1 <- 150
nPop2 <- 150


#creating a population of agents in a low vulnerability group
for( i in 2:nPop1){
  Agent1 <- data.frame(AgentNo = i,
                       Perceived_Support = runif(1,0,10),
                       Vulnerability_Group = "L",
                       Distress = 0,
                       MDD_onset ="",
                       Appropriate_Received_support_count = 0,
                       Inappropriate_Received_support_count = 0,
                       stringsAsFactors = FALSE)
  Agent_Low <- rbind(Agent_Low, Agent1)
}
  

#creating a population of agents in a high vulnerability group
for( i in 2:nPop2){
  Agent1 <- data.frame(AgentNo = i,
                       Perceived_Support = runif(1,0,10),
                       Vulnerability_Group = "H",
                       Distress = 0,
                       MDD_onset ="",
                       Appropriate_Received_support_count = 0,
                       Inappropriate_Received_support_count = 0,
                       stringsAsFactors = FALSE)
  Agent_High <- rbind(Agent_High, Agent1)
}
  



#fixing the AgentNo so it is from 1 to 300
Agent_High$AgentNo[Agent_High$AgentNo == 1:150] <- 151:300

#merging the two populations
AgentPop <- rbind(Agent_Low, Agent_High)


#saving as csv
write.csv(AgentPop, "Agent_Population.csv")




AgentPop$Appropriate_Received_support_count <- as.numeric(AgentPop$Appropriate_Received_support_count)
class(AgentPop$Appropriate_Received_support_count)
```

    ## [1] "numeric"

``` r
AgentPop$Inappropriate_Received_support_count <- as.numeric(AgentPop$Inappropriate_Received_support_count)
class(AgentPop$Inappropriate_Received_support_count)
```

    ## [1] "numeric"

Step 2: Creating the function generating stressful events with diferrent
valence which will happen to the agents

``` r
#creating a function for generating events 
  #E is extremely negative events
  #N is quite negative events 
  #M is moderately negative events
  #L is low negativity events which could also be interpreted as daily hasles
  
  
 Events_f <- function(E, N, M, L) {
   
   extremely_negative <- data.frame(Valence = runif(E,-13,-12))
   
   negative <- data.frame(Valence = runif(N,-12,-8))
   
   moderate <- data.frame(Valence = runif(M,-8,-4))
   
   low <- data.frame(Valence = runif(L,-4,0))
   
   Events <- rbind(extremely_negative, negative, moderate, low)
   
   #adding a column called "impact" which indicates how much stress every event elicits
   Events <- Events %>% mutate(Impact ="")
   Events$Impact[Events$Valence >= -13 & Events$Valence <= -12] <- 14
   Events$Impact[Events$Valence > -12 & Events$Valence <= -8] <- 7
   Events$Impact[Events$Valence > -8 & Events$Valence <= -4] <- 3
   Events$Impact[Events$Valence > -4 & Events$Valence <= 0] <- 1
     
return(data.frame(Events)) 
 }
```

Step 3: Creating the whole simulations Defining the behavior of agents:
what happens to their properties when certain event
happens

``` r
#with the function created above I generate a dataset with desired number of events with desired valence
Happened_Events <- Events_f(1, 2, 4, 8)

Happened_Events$Impact <- as.numeric(Happened_Events$Impact)
class(Happened_Events$Impact)
```

    ## [1] "numeric"

``` r
write.csv(Happened_Events, "Happened_Events.csv")



AgentPop <- read.csv(file = "~/Rutos SmuTkes/Cognitive Science/4th Semester/Social and cultural dynamics/ABM models/Agent_Population.csv")

                                                ######## Simulation Starts ########


#creating a for loop to go through all the events
for (k in 1:nrow(Happened_Events)){

  #creating a for loop to go through all the agents in a populations
  for (i in 1:nrow(AgentPop)){

    
#STRESSFUL EVENT INCREASE DISTRESS LEVEL
    
  #determine which event with what valence happens
  Event <- Happened_Events$Valence[k]
   

  #events increase the distress level of agents
  ifelse(Event >= -13 & Event <= -12 ,AgentPop$Distress[AgentPop$AgentNo[i]] <- AgentPop$Distress[AgentPop$AgentNo[i]] + 14, ifelse(Event > -12 & Event <= -8 ,AgentPop$Distress[AgentPop$AgentNo[i]] <- AgentPop$Distress[AgentPop$AgentNo[i]] + 7, ifelse(Event > -8 & Event <= -4 ,AgentPop$Distress[AgentPop$AgentNo[i]] <- AgentPop$Distress[AgentPop$AgentNo[i]] + 3, ifelse(Event > -4 & Event <= 0 ,AgentPop$Distress[AgentPop$AgentNo[i]] <- AgentPop$Distress[AgentPop$AgentNo[i]] + 1))))

  
#RECEIVED and PERCEIVED SUPPORT INTERACTION
   
    #received support happens randomly to each agent during each event. There is a 33.33% chance for each scenario to happen: a)  agent receives appropriate support (1), b) agent receives inappropriate support (2), c) agent does not receive any support (3)
   

  #determining which received support scenario (a-c) happens to the agent
  Received_support <- data.frame(Received_Support = 1:3)
  Received_support_sample <- sample_n(Received_support, 1, replace = TRUE)
  
  
  
  #keeping track of how many times each agent received support
  
  ifelse(Received_support_sample == 1, AgentPop$Appropriate_Received_support_count[AgentPop$AgentNo[i]] <-  AgentPop$Appropriate_Received_support_count[AgentPop$AgentNo[i]] + 1, AgentPop$Appropriate_Received_support_count[AgentPop$AgentNo[i]] <- AgentPop$Appropriate_Received_support_count[AgentPop$AgentNo[i]]) 
  
  ifelse(Received_support_sample == 2, AgentPop$Inappropriate_Received_support_count[AgentPop$AgentNo[i]] <-  AgentPop$Inappropriate_Received_support_count[AgentPop$AgentNo[i]] + 1, AgentPop$Inappropriate_Received_support_count[AgentPop$AgentNo[i]] <- AgentPop$Inappropriate_Received_support_count[AgentPop$AgentNo[i]])
 
 
  
  

  #received support increases/decreases perceived support level depending on its appropriateness

      
  ifelse(Received_support_sample == 1, AgentPop$Perceived_Support[AgentPop$AgentNo[i]] <- AgentPop$Perceived_Support[AgentPop$AgentNo[i]] + 0.7, AgentPop$Perceived_Support[AgentPop$AgentNo[i]] <- AgentPop$Perceived_Support[AgentPop$AgentNo[i]])
  
  ifelse(Received_support_sample == 2, AgentPop$Perceived_Support[AgentPop$AgentNo[i]] <- AgentPop$Perceived_Support[AgentPop$AgentNo[i]] - 0.7, AgentPop$Perceived_Support[AgentPop$AgentNo[i]] <- AgentPop$Perceived_Support[AgentPop$AgentNo[i]])

  #making sure that the value of perceived support stays in a range from 0 to 10
  
  ifelse(AgentPop$Perceived_Support[AgentPop$AgentNo[i]] >= 10 ,AgentPop$Perceived_Support[AgentPop$AgentNo[i]] <- 10, ifelse(AgentPop$Perceived_Support[AgentPop$AgentNo[i]] <= 0, AgentPop$Perceived_Support[AgentPop$AgentNo[i]] <- 0,  AgentPop$Perceived_Support[AgentPop$AgentNo[i]] <- AgentPop$Perceived_Support[AgentPop$AgentNo[i]]))
      

  #finding out the changed perceived support level
  Perceived_Support_update <- AgentPop$Perceived_Support[AgentPop$AgentNo[i]]
  
  
  
#PERCEIVED SUPPORT BUFFERS AGAINST DISTRESS  

  #calculating the individual buffer (%), which is determined by the perceived support level, and indicates by how much perceived support buffers against distress 
  individual_buffer <- Perceived_Support_update / 11

  #individual buffer together with the set impact value of the event determines the amount of distress that is going to be subtracted from the distress level after the event has happened

  AgentPop$Distress[AgentPop$AgentNo[i]] <- AgentPop$Distress[AgentPop$AgentNo[i]] - (Happened_Events$Impact[k] * individual_buffer)

  

#LIKELIHOOD OF MAJOR DEPRESSIVE DISORDER ONSET
  
  #setting different distress thresholds for Major Depressive Disorder onset in low and high vulnerability groups
  
  #I grab those in low vulnerability group with distress level bigger than 40
  Low_Vulnerability <- (1:nrow(AgentPop))[AgentPop$Vulnerability_Group == "L" & AgentPop$Distress >= 40]
  #I set their MDD onset to 1 (meaning it is very likely that Major Depressive Disorder might have started)  
  AgentPop$MDD_onset[Low_Vulnerability] <- 1
 
 
  #I grab those in high vulnerability group with distress level bigger than 13.33 (there is a threefold increased risk of MDD onset in a high vulnerability group)
  High_Vulnerability <- (1:nrow(AgentPop))[AgentPop$Vulnerability_Group == "H" & AgentPop$Distress >= 13.33]
  #I set their MDD onset to 1 (meaning it is very likely that Major Depressive Disorder might have started)  
  AgentPop$MDD_onset[High_Vulnerability] <- 1
 
 
  }

}
 

                                                ######## Simulation Ends ########
```

Step 4: Analysing and summarising the
data

``` r
                                                ############# ANALYSIS #############

AgentPop$MDD_onset <- as.numeric(AgentPop$MDD_onset)
class(AgentPop$MDD_onset)
```

    ## [1] "numeric"

``` r
#1)  with both appropriate and inappropriate received support


AgentPop1 <- read.csv(file = "~/Rutos SmuTkes/Cognitive Science/4th Semester/Social and cultural dynamics/ABM models/1.csv")

#count of MDD onset in Low and High Vulnerability groups
AgentPop1 %>% group_by(Vulnerability_Group) %>% count(MDD_onset) %>% na.omit()
```

    ## # A tibble: 2 x 3
    ## # Groups:   Vulnerability_Group [2]
    ##   Vulnerability_Group MDD_onset     n
    ##   <chr>                   <int> <int>
    ## 1 H                           1   120
    ## 2 L                           1    24

``` r
#visualise relationship between distress and perceived support in both vulnerability groups

ggplot(AgentPop1, aes(x = Perceived_Support, y = Distress, colour = Vulnerability_Group)) +
    geom_smooth(method = lm) + geom_hline(yintercept = 13.33, color='coral', linetype = "dashed")+ geom_hline(yintercept = 40, color='#00AFBB', linetype = "dashed") + geom_point() + ggtitle("Relationship between Perceived support and Distress level", "Received appropriate and inappropriate support") + theme_classic()
```

    ## `geom_smooth()` using formula 'y ~ x'

![](My-ABM_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#visualize how many of MDD onset in total are in L and H groups
ggplot(AgentPop1, aes(x=Vulnerability_Group, y=MDD_onset)) + geom_bar(stat="identity", fill = "#00AFBB") + ggtitle("Count of Major Depressive Disorder onset in Low and High Vulnerability groups") + ggtitle("Count of Major Depressive Disorder onset in Low and High Vulnerability groups", "Received appropriate and inappropriate support") + theme_classic()
```

    ## Warning: Removed 156 rows containing missing values (position_stack).

![](My-ABM_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
#summarising the output
AgentPop1 %>% group_by(Vulnerability_Group) %>% summarise(mean = mean(Perceived_Support), sd = sd(Perceived_Support))
```

    ## # A tibble: 2 x 3
    ##   Vulnerability_Group  mean    sd
    ##   <chr>               <dbl> <dbl>
    ## 1 H                    4.88  2.98
    ## 2 L                    5.10  3.05

``` r
AgentPop1 %>% group_by(Vulnerability_Group) %>% summarise(mean = mean(Distress), sd = sd(Distress), max = max(Distress), min = min(Distress))
```

    ## # A tibble: 2 x 5
    ##   Vulnerability_Group  mean    sd   max   min
    ##   <chr>               <dbl> <dbl> <dbl> <dbl>
    ## 1 H                    27.1  12.6  47.9  4.68
    ## 2 L                    26.1  11.9  47.5  4.55

``` r
AgentPop1 %>% group_by(Vulnerability_Group) %>% summarise(sum(Appropriate_Received_support_count))
```

    ## # A tibble: 2 x 2
    ##   Vulnerability_Group `sum(Appropriate_Received_support_count)`
    ##   <chr>                                                   <int>
    ## 1 H                                                         765
    ## 2 L                                                         759

``` r
AgentPop1 %>% group_by(Vulnerability_Group) %>% summarise(sum(Inappropriate_Received_support_count))
```

    ## # A tibble: 2 x 2
    ##   Vulnerability_Group `sum(Inappropriate_Received_support_count)`
    ##   <chr>                                                     <int>
    ## 1 H                                                           739
    ## 2 L                                                           736

``` r
#2)  Only appropriate received support

AgentPop2 <- read.csv(file = "~/Rutos SmuTkes/Cognitive Science/4th Semester/Social and cultural dynamics/ABM models/2.csv")



AgentPop2 %>% group_by(Vulnerability_Group) %>% count(MDD_onset) %>% na.omit()
```

    ## # A tibble: 2 x 3
    ## # Groups:   Vulnerability_Group [2]
    ##   Vulnerability_Group MDD_onset     n
    ##   <chr>                   <int> <int>
    ## 1 H                           1   110
    ## 2 L                           1    16

``` r
#visualise relationship between distress and perceived support in both vulnerability groups

ggplot(AgentPop2, aes(x = Perceived_Support, y = Distress, colour = Vulnerability_Group)) +
    geom_smooth(method = lm) + geom_hline(yintercept = 13.33, color='coral', linetype = "dashed")+ geom_hline(yintercept = 40, color='#00AFBB', linetype = "dashed") + geom_point() + ggtitle("Relationship between Perceived support and Distress level", "Received appropriate support") + theme_classic()
```

    ## `geom_smooth()` using formula 'y ~ x'

![](My-ABM_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
#visualize how many of MDD onset in total are in L and H groups
ggplot(AgentPop2, aes(x=Vulnerability_Group, y=MDD_onset)) + geom_bar(stat="identity", fill = "#00AFBB") + ggtitle("Count of Major Depressive Disorder onset in Low and High Vulnerability groups") + ggtitle("Count of Major Depressive Disorder onset in Low and High Vulnerability groups", "Received appropriate support") + theme_classic()
```

    ## Warning: Removed 174 rows containing missing values (position_stack).

![](My-ABM_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

``` r
#summarising the output
AgentPop2 %>% group_by(Vulnerability_Group) %>% summarise(mean = mean(Perceived_Support), sd = sd(Perceived_Support))
```

    ## # A tibble: 2 x 3
    ##   Vulnerability_Group  mean    sd
    ##   <chr>               <dbl> <dbl>
    ## 1 H                    7.60  2.35
    ## 2 L                    7.84  2.40

``` r
AgentPop2 %>% group_by(Vulnerability_Group) %>% summarise(mean = mean(Distress), sd = sd(Distress), max = max(Distress), min = min(Distress))
```

    ## # A tibble: 2 x 5
    ##   Vulnerability_Group  mean    sd   max   min
    ##   <chr>               <dbl> <dbl> <dbl> <dbl>
    ## 1 H                    23.6  12.4  46.4  4.36
    ## 2 L                    22.3  12.2  46.1  4.36

``` r
AgentPop2 %>% group_by(Vulnerability_Group) %>% summarise(sum(Appropriate_Received_support_count))
```

    ## # A tibble: 2 x 2
    ##   Vulnerability_Group `sum(Appropriate_Received_support_count)`
    ##   <chr>                                                   <int>
    ## 1 H                                                         749
    ## 2 L                                                         754

``` r
#3)  with only inappropriate received support


AgentPop3 <- read.csv(file = "~/Rutos SmuTkes/Cognitive Science/4th Semester/Social and cultural dynamics/ABM models/3.csv")


#Count of MDD onset in Low and High Vulnerability groups
AgentPop3 %>% group_by(Vulnerability_Group) %>% count(MDD_onset) %>% na.omit()
```

    ## # A tibble: 2 x 3
    ## # Groups:   Vulnerability_Group [2]
    ##   Vulnerability_Group MDD_onset     n
    ##   <chr>                   <int> <int>
    ## 1 H                           1   135
    ## 2 L                           1    41

``` r
#visualise relationship between distress and perceived support in both vulnerability groups

ggplot(AgentPop3, aes(x = Perceived_Support, y = Distress, colour = Vulnerability_Group)) +
    geom_smooth(method = lm) + geom_hline(yintercept = 13.33, color='coral', linetype = "dashed")+ geom_hline(yintercept = 40, color='#00AFBB', linetype = "dashed") + geom_point() + ggtitle("Relationship between Perceived support and Distress level", "Received inappropriate support") + theme_classic()
```

    ## `geom_smooth()` using formula 'y ~ x'

![](My-ABM_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->

``` r
#visualize how many of MDD onset in total are in L and H groups
ggplot(AgentPop3, aes(x=Vulnerability_Group, y=MDD_onset)) + geom_bar(stat="identity", fill = "#00AFBB") + ggtitle("Count of Major Depressive Disorder onset in Low and High Vulnerability groups") + ggtitle("Count of Major Depressive Disorder onset in Low and High Vulnerability groups", "Received inappropriate support") + theme_classic()
```

    ## Warning: Removed 124 rows containing missing values (position_stack).

![](My-ABM_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->

``` r
#summarising the output

AgentPop3 %>% group_by(Vulnerability_Group) %>% summarise(mean = mean(Perceived_Support), sd = sd(Perceived_Support))
```

    ## # A tibble: 2 x 3
    ##   Vulnerability_Group  mean    sd
    ##   <chr>               <dbl> <dbl>
    ## 1 H                    1.92  2.32
    ## 2 L                    2.29  2.26

``` r
AgentPop3 %>% group_by(Vulnerability_Group) %>% summarise(mean = mean(Distress), sd = sd(Distress), max = max(Distress), min = min(Distress))
```

    ## # A tibble: 2 x 5
    ##   Vulnerability_Group  mean    sd   max   min
    ##   <chr>               <dbl> <dbl> <dbl> <dbl>
    ## 1 H                    31.5  12.2    48  6.71
    ## 2 L                    30.1  11.8    48  8.35

``` r
AgentPop3 %>% group_by(Vulnerability_Group) %>% summarise(sum(Inappropriate_Received_support_count))
```

    ## # A tibble: 2 x 2
    ##   Vulnerability_Group `sum(Inappropriate_Received_support_count)`
    ##   <chr>                                                     <int>
    ## 1 H                                                           765
    ## 2 L                                                           697

``` r
#4)  with no received support

AgentPop4 <- read.csv(file = "~/Rutos SmuTkes/Cognitive Science/4th Semester/Social and cultural dynamics/ABM models/4.csv")


#Count of MDD onset in Low and High Vulnerability groups
AgentPop4 %>% group_by(Vulnerability_Group) %>% count(MDD_onset) %>% na.omit()
```

    ## # A tibble: 2 x 3
    ## # Groups:   Vulnerability_Group [2]
    ##   Vulnerability_Group MDD_onset     n
    ##   <chr>                   <int> <int>
    ## 1 H                           1   120
    ## 2 L                           1    31

``` r
#visualise relationship between distress and perceived support in both vulnerability groups

ggplot(AgentPop4, aes(x = Perceived_Support, y = Distress, colour = Vulnerability_Group)) +
    geom_smooth(method = lm) + geom_hline(yintercept = 13.33, color='coral', linetype = "dashed")+ geom_hline(yintercept = 40, color='#00AFBB', linetype = "dashed") + geom_point() + ggtitle("Relationship between Perceived support and Distress level", "No Received support") + theme_classic()
```

    ## `geom_smooth()` using formula 'y ~ x'

![](My-ABM_files/figure-gfm/unnamed-chunk-5-7.png)<!-- -->

``` r
#visualize how many of MDD onset in total are in L and H groups
ggplot(AgentPop4, aes(x=Vulnerability_Group, y=MDD_onset)) + geom_bar(stat="identity", fill = "#00AFBB") + ggtitle("Count of Major Depressive Disorder onset in Low and High Vulnerability groups") + ggtitle("Count of Major Depressive Disorder onset in Low and High Vulnerability groups", "No Received support") + theme_classic()
```

    ## Warning: Removed 149 rows containing missing values (position_stack).

![](My-ABM_files/figure-gfm/unnamed-chunk-5-8.png)<!-- -->

``` r
#summarising the output

AgentPop4 %>% group_by(Vulnerability_Group) %>% summarise(mean = mean(Perceived_Support), sd = sd(Perceived_Support))
```

    ## # A tibble: 2 x 3
    ##   Vulnerability_Group  mean    sd
    ##   <chr>               <dbl> <dbl>
    ## 1 H                    4.72  2.97
    ## 2 L                    4.99  2.87

``` r
AgentPop4 %>% group_by(Vulnerability_Group) %>% summarise(mean = mean(Distress), sd = sd(Distress), max = max(Distress), min = min(Distress))
```

    ## # A tibble: 2 x 5
    ##   Vulnerability_Group  mean    sd   max   min
    ##   <chr>               <dbl> <dbl> <dbl> <dbl>
    ## 1 H                    27.4  13.0  47.8  4.87
    ## 2 L                    26.2  12.5  48.0  4.47

``` r
AgentPop4 %>% group_by(Vulnerability_Group) %>% summarise(sum(Appropriate_Received_support_count))
```

    ## # A tibble: 2 x 2
    ##   Vulnerability_Group `sum(Appropriate_Received_support_count)`
    ##   <chr>                                                   <int>
    ## 1 H                                                           0
    ## 2 L                                                           0
