library(tidyverse)  #voor(%>%)
library(desc)       #Get information about the authors
library(bookdown)   #For creating a website
library(readxl)     #For reading excel files
library(toolboxr)   #For rotating the axes in R
library(nlme)       #For running multi-level statistical models
library(ggsignif)   
library(tools)
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
fileNames <- list.files(recursive = TRUE)
View(fileNames)


md_5_excel <- tools::md5sum("toxrefdb_nel_lel_noael_loael_summary_AUG2014_FOR_PUBLIC_RELEASE.csv")
md_5_excel %>% enframe() -> md5
md5 %>% readr::write_csv("toxrefdb_nel_lel_noael_loael_summary_AUG2014_FOR_PUBLIC_RELEASE.csv")
View(md5)
fs::dir_tree(here::here("wrong_structure"))

knitr::include_graphics(
  file.path(
    image_dir,
    "one_ring.jpg")
)

data(package = "mlbench", dataset = "BostonHousing")
BostonHousing %>% as_tibble()


salmonella <- read_excel("lesson2/salmonella CFU kinetics OD600 in LB van ipecs 8okt2020 kleur.xlsx", skip = 1, sheet = "All Cycles")
View(salmonella)
#TIDY MAKEN

data_platereader <- salmonella %>%
  rename(sample = Time, well = ...1) %>%
  janitor::clean_names()
View(data_platereader)
unique(data_platereader$well)
## create sample table
sample_names <- data_platereader$sample

mv_utr_tx100 <- rep(c("mv", "mv", "mv", "mv", 
                      "untr", "untr", "untr", "untr", "untr",
                      "tx100", "tx100", "tx100"), times = 8)

salmonella <- read_xlsx("lesson2/salmonella CFU kinetics OD600 in LB van ipecs 8okt2020 kleur.xlsx"
  , sheet = "layout", range = "C5:N13"
) %>%
  janitor::clean_names() 

# cheack data types
map(
  .x = salmonella,
  typeof
)
#####################
salmonella <- salmonella %>%
  pivot_longer(ul_sal_1:ul_sal_12,
               names_to = "plate_column", 
               values_to = "microliters_bacteria")

## synthesize to sample table

samples <- tibble(
  well = data_platereader$well,  
  sample = sample_names,
  condition = mv_utr_tx100,
  ul_salmonella = salmonella$microliters_bacteria
)

## join sample table with data
data_join <- left_join(samples, data_platereader)

## create tidy version
data_tidy <- data_join %>%
  pivot_longer(
    x0_h:x24_h_5_min,
    names_to = "time",
    values_to = "value"
  )

## fix time variable
data_tidy_time <- data_tidy %>%
  mutate(time_var =
           str_replace_all(
             string = time,
             pattern = "x",
             replacement = ""
           )) %>%
  mutate(time_var =
           str_replace_all(
             string = time_var,
             pattern = "_",
             replacement = ""
           )) %>%
  mutate(time_var =
           str_replace_all(
             string = time_var,
             pattern = "h",
             replacement = ":"
           )) %>%
  mutate(time_var =
           str_replace_all(
             string = time_var,
             pattern = "min",
             replacement = ""
           )) %>%
  separate(
    col = time_var,
    into = c("hours", "minutes"),
    remove = FALSE
  ) %>%
  mutate(
    minutes = ifelse(minutes == "", "0", minutes)
  ) %>%
  mutate(minutes_passed = 60*as.numeric(hours) + as.numeric(minutes))

## misingness
data_tidy %>%
  naniar::vis_miss()
## graphs
data_tidy_time %>%
  group_by(condition, ul_salmonella, minutes_passed) %>%
  summarise(mean_value = mean(value)) %>%
  mutate(ul_salmonella = round(as.numeric(ul_salmonella), 2)) %>%
  ggplot(aes(x = minutes_passed, y = mean_value)) +
  geom_line(aes(colour = condition), show.legend = FALSE) +
  facet_grid(condition ~ ul_salmonella) +
  xlab("Time passed (minutes)") +
  ylab("Mean AU")


library(palmerpenguins)
data_penguins <- palmerpenguins::penguins_raw 
data_penguins
# simulating inconsistent data entry
penguinswrong <- penguins
penguinswrong
levels(penguinswrong$species) <- c(levels(penguinswrong$species), "adelie")
penguinswrong$species[1:5]<-"adelie"

# make a box plot of flipper length showing a/Adelie as separate species
flipper_box <- ggplot(data = penguinswrong, aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
  geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE, position = position_jitter(width = 0.2, seed = 0)) +
  scale_color_manual(values = c("darkorange","purple","cyan4","red")) +
  theme_minimal() +
  labs(x = "Species",
       y = "Flipper length (mm)")

flipper_box
library(ggplot2)


data_penguins %>%
  ggplot(aes(x = Sex, y = `Flipper Length (mm)`)) +
  geom_point(aes(colour = Species), position = "jitter", show.legend = FALSE)

######
# generate some dummy data for the example
measured1 <- rbinom(100, size = 2, prob = 0.3)
measured2 <- rnorm(100, mean = 5.3, sd = 0.1)
measured3 <- rnbinom(100, size = 10, prob = 0.1)
concentration <- rep(1:10, 10)

# put it in a tibble
data <- tibble::tibble(
  `concentration (mMol/l)` = concentration,
  `measured 1 (pg/ml)` = measured1,
  `measured 2 (ng/ml)` = measured2,
  `measured 3 (ng/ml)` = measured3
)
data
var_names <- names(data)
metadata <- tibble::tibble(
  varnames = var_names
)
metadata

metadata %>%
  mutate(
    varnames = str_replace_all(
      varnames,
      pattern = " ",
      replacement = "")) %>%
  separate(
    varnames,
    into = c("varnames", "units"), sep = "\\(", remove = FALSE) %>%
  mutate(
    units = str_replace_all(
      units,
      pattern = "\\)",
      replacement = "")) -> metadata_clean
metadata_clean
