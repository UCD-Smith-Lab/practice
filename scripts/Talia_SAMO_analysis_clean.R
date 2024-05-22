
# 01_Talia_SAMO_analysis_clean --------------------------------------------

# Created: Spring 2021
# Modified for Smith Lab: Spring 2024

# General Info: Analysis of how detections of deer and coyotes change over time since fire in different habitat types. The analysis uses camera data and zero-inflated poisson models to estimate detections of each species over the months following the Springs Fire in three habitats: 1) burned areas within the Springs Fire perimeter, 2) control (unburned) areas outside the Springs Fire perimeter, 3) unburned habitat patches within the Springs Fire perimeter.

# Data info
# Site: camera site
# occasion: detection data descritized in 30 day occasions
# deer/coyote_count: number of animals detected during an occaision, detections were independent photos 60 minutes apart
# distance: distance from camera site to nearest development
# habitat type: burn, control, large/small patch (unburned within fire perimeter)
# o.date: start date for each occasion
# monthfire: number of months since the fire happened
# effort: number of nights camera was operational during that occasion

# -------------------------------------------------------------------------





#### set-up ####
# load in libraries 
library(tidyverse)
library(coefplot)
library(MuMIn)
library(ggplot2)
library(msm)
library(lubridate)
library(glmmTMB)
library(DHARMa)
library(bbmle)
library(dotwhisker)
library(dplyr)
library(broom)
library(broom.mixed)

# set working directory
library(here)

# read in data 
fulldata<- read_csv("data/Talia_fulldata_30days.csv")
view(fulldata)

# seperate the data by habitat type 
burn<- fulldata %>% filter(Habitat_Type == "Burned")
control<- fulldata %>% filter(Habitat_Type == "Control")
patch<- fulldata %>% filter(Habitat_Type == "Large Patch" | Habitat_Type== "Small Patch")





#### models of detects over time since fire, deer ####
# deer, burn
dburntime_zip<- glmmTMB(Deer_Count~monthfire
                        + (1|Site),
                        ziformula = ~1,
                        dispformula = ~1,
                        data = burn,
                        na.action = na.omit,
                        family = poisson)
summary(dburntime_zip)

# deer, control
dcontroltime_zip<- glmmTMB(Deer_Count~monthfire
                           + (1|Site),
                           ziformula = ~1,
                           dispformula = ~1,
                           data = control,
                           na.action = na.omit,
                           family = poisson)
summary(dcontroltime_zip)

# deer, patch
dpatchtime_zip<- glmmTMB(Deer_Count~monthfire
                         + (1|Site),
                         ziformula = ~1,
                         dispformula = ~1,
                         data = patch,
                         na.action = na.omit,
                         family = poisson)
summary(dpatchtime_zip)

#### models of dets over time since fire, coyote ####
# coyote, burn
cburntime_zip<- glmmTMB(Coyote_Count~monthfire
                        + (1|Site),
                        ziformula = ~1,
                        dispformula = ~1,
                        data = burn,
                        na.action = na.omit,
                        family = poisson)
summary(cburntime_zip)

# coyote, control
ccontroltime_zip<- glmmTMB(Coyote_Count~monthfire
                           + (1|Site),
                           ziformula = ~1,
                           dispformula = ~1,
                           data = control,
                           na.action = na.omit,
                           family = poisson)
summary(ccontroltime_zip)

# coyote, patch
cpatchtime_zip<- glmmTMB(Coyote_Count~monthfire
                         + (1|Site),
                         ziformula = ~1,
                         dispformula = ~1,
                         data = patch,
                         na.action = na.omit,
                         family = poisson)
summary(cpatchtime_zip)

#### plots time since fire, deer ####

# first create data frames for burn, control, & patch
dburn_monthfire_df<- tidy(dburntime_zip) %>% filter(term == "monthfire")  %>% mutate(model = "burn")

dcontrol_monthfire_df<- tidy(dcontroltime_zip) %>% filter(term == "monthfire")  %>% mutate(model = "control")

dpatch_monthfire_df<- tidy(dpatchtime_zip) %>% filter(term == "monthfire")  %>% mutate(model = "patch")

#bind df's together
monthfire_df<- bind_rows(dpatch_monthfire_df, dcontrol_monthfire_df, dburn_monthfire_df)

#relabel predictors
monthfire_df<- monthfire_df %>% 
  dotwhisker::relabel_predictors(monthfire = "Habitat type" )

#assign levels to decide order (defaults alphabetically)
monthfire_df$model<- factor(monthfire_df$model, levels = c("burn", "control", "patch"))
levels(monthfire_df$model)

#dwplot
dwplot((monthfire_df),vline = geom_vline(xintercept = 0, colour = "grey2", linetype = 2),
       dot_args = list(aes(color = model), size = 3), 
       whisker_args = list(size = 1)) +
  theme_classic() +
  xlab("") +
  ylab("Habitat Types") +
  ggtitle("Deer detections with time since fire") +
  theme(text = element_text(size=15,color = "black"),
        plot.title = element_text(hjust = 0.5, size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "black", size = 12.5),
        axis.ticks.x = element_line(size = 1),  
        axis.text = element_text(size = 12, color = "black"), 
        axis.ticks.y = element_line(size = .001, color = "black"), 
        axis.line = element_line(size = 1.2),
        legend.position = "none") +
  scale_color_brewer(palette = "Dark2",
                     breaks = c("burn", "patch", "control"),
                     labels = c("burn", "patch", "control"))

#### plots time since fire, coyote ####

#first create data frames for burn, control, & patch
cburn_monthfire_df<- tidy(cburntime_zip) %>% filter(term == "monthfire")  %>% mutate(model = "burn")

ccontrol_monthfire_df<- tidy(ccontroltime_zip) %>% filter(term == "monthfire")  %>% mutate(model = "control")

cpatch_monthfire_df<- tidy(cpatchtime_zip) %>% filter(term == "monthfire")  %>% mutate(model = "patch")

#bind df's together
coymonthfire_df<- bind_rows(cburn_monthfire_df, cpatch_monthfire_df, ccontrol_monthfire_df)

#relabel predictors
coymonthfire_df<- coymonthfire_df %>% 
  dotwhisker::relabel_predictors(monthfire = "Habitat type" )

#assign levels to decide order (defaults alphabetically)
coymonthfire_df$model<- factor(coymonthfire_df$model, levels = c("burn", "patch", "control"))
levels(coymonthfire_df$model)

#dwplot
dwplot((coymonthfire_df),vline = geom_vline(xintercept = 0, colour = "grey2", linetype = 2),
       dot_args = list(aes(color = model), size = 3), 
       whisker_args = list(size = 1)) +
  theme_classic() +
  xlab("Coefficient estimate") +
  ylab("Habitat Types") +
  ggtitle("Coyote detections with time since fire") +
  theme(text = element_text(size=15,color = "black"),
        plot.title = element_text(hjust = 0.5, size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "black", size = 12.5),
        axis.ticks.x = element_line(size = 1),  
        axis.text = element_text(size = 12, color = "black"), 
        axis.line = element_line(size = 1.2),
        legend.position = "none") +
  scale_color_brewer(palette = "Dark2",
                     breaks = c("burn", "patch", "control"),
                     labels = c("burn", "patch", "control"))

