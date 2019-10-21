#====load packages====
library(devtools)

# #This didn't work
# devtools::install_github("USGS-R/glmtools", ref = 'ggplot_overhaul')
# #This worked for me
# #This is an old version though...
# install.packages('glmtools',repos=c('https://owi.usgs.gov/R'))
# devtools::install_github("GLEON/GLM3r")


library(glmtools)
library(GLM3r)
library(tidyverse)
library(lubridate)


#====TR========

#===try GLM3====


sim_folder <- paste0(getwd(),"/TR")
run_glm(sim_folder = sim_folder)

#===load output====
out_folder = paste0(sim_folder, "/outputs")
list.files(out_folder)
out_file <- paste0(out_folder, "/output.nc")
get_var(out_file, "temp")

plot_var(out_file, "temp")

routine = read_csv("Data/TR_routine.csv")


routine = routine %>%
  filter(year(datetime)==1996) %>%
  mutate(index = interaction(datetime, depth)) %>%
  filter(!duplicated(index)) %>%
  select(-index)

write_csv(routine, "Data/routine1996.csv")



plot_var_compare(out_file, field_file = "Data/routine1996.csv", "temp")


#RMSE
temp_rmse <- compare_to_field(out_file, field_file = "Data/routine1996.csv",
                              metric = 'water.temperature', as_value = FALSE)
print(paste(round(temp_rmse,2), "deg C RMSE"))


#====CR========



#===try GLM3====


sim_folder <- paste0(getwd(),"/CR")
run_glm(sim_folder = sim_folder)

#===load output====
out_folder = paste0(sim_folder, "/outputs")
list.files(out_folder)
out_file <- paste0(out_folder, "/output.nc")
get_var(out_file, "temp")

plot_var(out_file, "temp")

routine = read_csv("Data/CR_routine.csv")


# routine = routine %>% 
#   filter(year(datetime)==1996) %>% 
#   mutate(index = interaction(datetime, depth)) %>% 
#   filter(!duplicated(index)) %>% 
#   select(-index)
# 
# write_csv(routine, "Data/routine1996.csv")



plot_var_compare(out_file, field_file = "Data/routine1996.csv", "temp")


#RMSE
temp_rmse <- compare_to_field(out_file, field_file = "Data/routine1996.csv",
                              metric = 'water.temperature', as_value = FALSE)
print(paste(round(temp_rmse,2), "deg C RMSE"))
