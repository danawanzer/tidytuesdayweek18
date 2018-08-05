Tidy Tuesday - Week 18
================

``` r
#Load packages
library(ggplot2)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v tibble  1.4.2     v purrr   0.2.5
    ## v tidyr   0.8.1     v dplyr   0.7.6
    ## v readr   1.1.1     v stringr 1.3.1
    ## v tibble  1.4.2     v forcats 0.3.0

    ## -- Conflicts ---------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(grid)
library(summarytools)
library(janitor)
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
#Read data
dat <- read_excel("week18_dallas_animals.xlsx")

view(dfSummary(dat))
```

    ## Method 'viewer' only valid within RStudio. Switching method to 'browser'.

    ## Output file written: C:\Users\DANAAN~1\AppData\Local\Temp\RtmpK26dp1\file57187bf31577.html

Bar Chart of Animal Type
========================

``` r
#Code for doing multicolor text; add the new title via arrangeGrob at the very bottom
grobs <- grobTree( 
  gp = gpar(fontsize = 14, fontface = "bold"),
  textGrob(label = "Dogs ", 
           name = "title1",
           x = unit(.2, "lines"), y = unit(.2, "lines"),
           hjust = 0, vjust = 0, gp = gpar(col = "darkred")),
  textGrob(label = "are the most common type of animal rescued by the Dallas Animal Services",
           name = "title2",
           x = grobWidth("title1") + unit(.2, "lines"), y = unit(.2, "lines"),
           hjust = 0, vjust = 0, gp = gpar(col = "black"))
)

p <- dat %>%
  group_by(animal_type) %>% #Group by animal type
  filter(outcome_type == "ADOPTION") %>% #Only examine animals that were adopted
  summarize(n = n()) %>% #Summarize into a datframe with just the n of each animal type adopted
  arrange(n) %>% #sort ascending
  mutate(max = if_else(order(n, decreasing = TRUE) == 1, "Top", "Other")) %>% #Figure out biggest group
  ggplot(aes(x = reorder(animal_type, -n), y = n, fill = max)) + #ggplot aesthetics
  geom_col() + #Create a column chart
  scale_fill_manual(values = c("Other" = "grey", "Top" = "darkred")) + #Use the mutate above to color 
  theme_minimal() + #make things pretty
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank()) +
  geom_text(aes(label = n), position = position_dodge(.9), vjust = -1) + #Fix the labels not showing
  ylim(c(0, 8500))

gg <- arrangeGrob(p, top = grobs, padding = unit(2.6, "line")) # add the title from above
grid.newpage() #use this code to test your figures
grid.draw(gg)

ggsave(filename = "animaltype.png", #use this code to finalize your figures into a png saved on your comp
       plot = grid.draw(gg),
       device = "png",
       dpi = "retina",
       width = 8, height = 5)
```

Dot plot of Animal Type by Outcome Type
=======================================

Goal is a dot plot where:
\* each line is an outcome, only examining the top four outcomes for animals \* we look at just cats and dogs to compare outcome across the two \* the x-axis is a percentage from 0-100%

``` r
#Get the data we need
dat2 <- dat %>%
  filter(outcome_type %in% c("ADOPTION", "EUTHANIZED", "TRANSFER", "RETURNED TO OWNER")) %>%
  filter(animal_type == "DOG" | animal_type == "CAT") %>% #Only look at dogs and cats
  group_by(animal_type, outcome_type) %>% #group by animal and outcome types
  summarize(n = n()) %>% #determine the n for each outcome by each animal
  spread(animal_type, n) %>% #spread the data into wide format; MAY NEED TO REMOVE THIS FOR DOT PLOT
  adorn_percentages("col") %>% #change n to column percentages instead of n from the janitor package
  mutate(diff = CAT - DOG) %>%
  gather(animal_type, n, -outcome_type, -diff) %>%
  mutate(n = n * 100)

#Make a dot plot

catdogcol <- c("CAT" = "#7b3294", "DOG" = "#008837")

p <- ggplot(dat2, aes(x = n, y = reorder(outcome_type, diff), label = paste(round(n, 0), "%", sep = ""))) + 
  geom_point(shape = 19, size = 14, aes(col = animal_type)) +
  geom_label(aes(fill = animal_type, fontface = "bold"), color = "#FFFFFF", label.size = 0) +
  scale_color_manual(values = catdogcol) +
  scale_fill_manual(values = catdogcol) + 
  theme_minimal() + #make things pretty
  theme(axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.y = element_text(face = "bold")) +
  scale_x_continuous(limits = c(0,40),labels = function(x) paste0(x, "%", sep = ""))

#Make the title
gs <- grobTree( 
  gp = gpar(fontsize = 14, fontface = "bold"),
  textGrob(label = "Cats ", 
           name = "title1",
           x = unit(.2, "lines"), 
           y = unit(1.4, "lines"),
           hjust = 0, vjust = 0, gp = gpar(col = "#7b3294")),
 
   textGrob(label = "are more likely to be transferred or euthanized.",
           name = "title2",
           x = grobWidth("title1") + unit(.2, "lines"), 
           y = unit(1.4, "lines"),
           hjust = 0, vjust = 0, gp = gpar(col = "black")),

  textGrob(label = "Dogs ",
           name = "title3",
           x = unit(.2, "lines"),
           y = unit(.2, "lines"),
           hjust = 0, vjust = 0, gp = gpar(col = "#008837")),

  textGrob(label = "are more likely to be adopted or returned to the owner.",
           name = "title4",
           x = grobWidth("title3") + unit(.2, "lines"),
           y = unit(.2, "lines"),
           hjust = 0, vjust = 0, gp = gpar(col = "black"))
)

gg <- arrangeGrob(p, top = gs, padding = unit(2.6, "line"))
grid.newpage() #use this code to test your figures
grid.draw(gg)

ggsave(filename = "outcometype_catdog.png", #use this code to finalize your figures into a png saved on your comp
       plot = grid.draw(gg),
       device = "png",
       dpi = "retina",
       width = 8, height = 5)
```
