library(tidyverse)  #voor(%>%)
library(desc)       #Get information about the authors
library(bookdown)   #For creating a website
library(readxl)     #For reading excel files
library(toolboxr)   #For rotating the axes in R
library(nlme)       #For running multi-level statistical models
library(ggsignif)   
library(tools)
library(RColorBrewer) #For more colours in a graph
library(kableExtra)   #For more complex table options
#Exercise 1.2

#A

url <- "http://genesdev.cshlp.org/content/suppl/2009/06/11/gad.1806309.DC1/FancySuppTable2.xls"
download.file(url = url, destfile = "fancysupptable2.xls")
fancysupptable2 <- read_xls("fancysupptable2.xls")
getwd()


download.file(url="http://genesdev.cshlp.org/content/suppl/2009/06/11/gad.1806309.DC1/FancySuppTable2.xls", destfile = "fancysupptable2_1.xls")
fancysupptable2 <- read_xls("fancysupptable2_1.xls")

#B
#Error: 
#  filepath: C:\Users\Jari\Documents\Dauer2\DSFB2\dsfb2_workflows_exercises\fancysupptable2.xls
#libxls error: Unable to open file

#C
supptable <- read_excel("FancySuppTable2.xls")
View(supptable)

#D
#Je benoemt niet de stappen hoe je de data heb verkregen

#G
#Example gene name conversion = 	1/3/1900
#H
X <- read_excel("CE.LIQ.FLOW.062_Tidydata.xlsx")
View(X)

ergoStool %>% as_tibble()
#
result <- ergo_model %>% summary() 
result$tTable %>% as.data.frame() %>% knitr::kable()

ergoStool %>%
  ggplot(aes(x = reorder(Type, effort), y = effort)) + 
  geom_boxplot(colour = "darkgreen", outlier.shape = NA) + 
  geom_jitter(aes(colour = reorder(Subject, -effort)), 
              width = 0.2, size = 3) +
  scale_colour_manual(
    values = c(
      "red","blue", 
      "green", "darkblue", 
      "darkgreen", "purple", 
      "grey", "black", "darkgrey")
  ) +
  ylab("Effort (Borg scale score)") +
  xlab("Chair type") + 
  guides(colour=guide_legend(title="Subject id")) +
  theme_bw()


#Exercise 1.3
#A
#The variability of the effort in person blue is ascending for chair 1, 4, 3, 2
#B
#Mister green has to do more effort to get out of a chair than mister black
#C
#type 2
#D
#Than you will lose the ascending order on the xas

ergo_model <- lme(
  data = ergoStool, # the data to be used for the model
  fixed = effort ~ Type, # the dependent and fixed effects variables
  random = ~1 | Subject # random intercepts for Subject variable
)


#
library(ggsignif)
p_values <- result$tTable %>% as.data.frame()
annotation_df <- data.frame(Type=c("T1", "T2"), 
                            start=c("T1", "T1"), 
                            end=c("T2", "T3"),
                            y=c(16, 14),
                            label=
                              paste("p-value:",
                                    c(
                                      formatC(
                                        p_values$`p-value`[2], digits = 3),
                                      formatC(
                                        p_values$`p-value`[3], digits = 3)
                                    )
                              )
)

set.seed(123)
ergoStool %>%
  ggplot(aes(x = reorder(Type, effort), 
             y = effort)) + 
  geom_boxplot(colour = "darkgreen", 
               outlier.shape = NA) + 
  geom_jitter(aes(
    colour = reorder(Subject, -effort)), 
    width = 0.2, 
    size = 3) +
  scale_colour_manual(
    values = c(
      "red", "blue","green", 
      "darkblue", "darkgreen", 
      "purple", "grey", "black", 
      "darkgrey")) +
  ylab("Effort (Borg scale score)") +
  xlab("Chair type") + 
  guides(colour=guide_legend(title="Subject id")) +
  ylim(c(6,20)) +
  geom_signif(
    data=annotation_df,
    aes(xmin=start, 
        xmax=end, 
        annotations=label, 
        y_position=y),
    textsize = 5, vjust = -0.2,
    manual=TRUE) +
  theme_bw() -> plot_ergo
plot_ergo

knitr::include_graphics(
  here::here(
    "images",
    "cos-shield.png")
)

###############################
###########lesson2#############
###############################

#Exercise 2.1
#The file names are too long and are not descriptive enough. The datasheet uses spaces instead of _

#Exercise 2.2

#library(tools)

myDir <- here::here(
  "data",
  "md5_examples")

fileNames <- list.files(myDir, recursive = TRUE)

tools::md5sum(file.path(myDir, fileNames)) %>% enframe() -> md5sums_all
md5sums_all$filename <- fileNames
md5sums_all %>% select(filename,value)

md_5_1 <- tools::md5sum("MD5_exampledata_1.txt")
md_5_1 %>% enframe()
getwd()



cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

library(palmerpenguins)

mass_hist <- ggplot(data = penguins, aes(x = body_mass_g)) +
  geom_histogram(aes(fill = species),
                 alpha = 0.8,
                 position = "identity") +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  theme_minimal() +
  labs(x = "Body mass (g)",
       y = "Frequency",
       title = "Penguin body mass")

mass_hist + scale_fill_manual(values = cbp1)
mass_hist
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

#
data_for_table <- mtcars[1:5, 1:6]
kbl(data_for_table) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                position = "left")
data_for_table %>%
  kbl() %>%
  kable_classic_2(full_width = F) %>%
  row_spec(0, angle = -45) %>%
  column_spec(2, color = spec_color(mtcars$mpg[1:5]),
              link = "https://www.wikipedia.com") %>%
  column_spec(5, color = "white",
              background = spec_color(mtcars$hp[1:5], end = 0.7),
              popover = paste("am:", mtcars$am[1:5]))


#
dinosaur <- c("Triceratops", "Ankylosaurus", "Spinosaurus", "Tyrannosaurus rex", "Velociraptor")
description <- c("An herbivore dinosaur with two long horns for protection", "An armoured herbivore dinosaur", "A giant theropod, the largest of all predatory dinosaurs", "A large theropod, that relied on his smell and sight for hunting", "A small, bird-like theropod. With large claws to kill his preys")
dataframe <- data.frame(dinosaur = dinosaur, description = description)
dataframe %>% kbl() %>% kable_classic_2(full_width = F) %>% 
  column_spec(1, color = c("red", "green", "brown", "orange", "blue"))

rating <- c(8, 7, 10, 9, 8)
dataframe_rating <- dataframe %>% mutate(rating = rating)
View(dataframe_rating)
#Make a graph
rating <- c(8.5, 6.5, 10, 9, 8)
dataframe_rating <- dataframe %>% mutate(rating = rating)

dataframe_rating %>% ggplot(aes(dinosaur, rating, fill = dinosaur)) +
  geom_col() +
  labs(title = "Rating my top 5 dinosaurs on a scale from 1 to 10", 
       y = " ",
       x = " ") +
  theme_classic() +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "none")
           