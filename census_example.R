library(tidyverse)
library(tidycensus)
library(openxlsx)


# load in search tables

#2019 acs data tables
v19 <- load_variables(2019, "acs5", cache = TRUE)

#2019 acs subject tables
v19_sub <- load_variables(2019, dataset = "acs5/subject") 

#2019 data profiles
v19_prof <- load_variables(2019, dataset = "acs5/profile") 

#2020 decennial data
v20 <- load_variables(2020, "pl")


#DP03_0128P = Percent!!PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL!!All people

counties <- get_acs(geography = "county", 
                    variables = c('poverty' = 'DP03_0128P'),
                    year = 2019,
                    geometry = FALSE) %>%
  separate(NAME, c("County_Name", "State"), sep = "([.,:])") %>%
  mutate(State = trimws(State))


rural_des <- read.xlsx("C:/Users/jfain/OneDrive - Save the Children Federation Inc/USP/Projects/Rural Definition/Archive (USE FILES ON DATA MGMT SHAREPOINT)/data/ruralurbancodes2013.xlsx") %>%
   separate(Description, c("Designation", "NA")) %>%
   select(State, County_Name, Designation) 

rural_des$State <- state.name[match(rural_des$State, state.abb)]

#join 
  
rural_counties <-left_join(counties, rural_des)

head(rural_counties)

# West Virginia rural poverty


wv_rural_pov <- rural_counties %>%
  filter(State == 'West Virginia',
         Designation == 'Nonmetro') %>%
  arrange(estimate)

head(wv_rural_pov)


# visualize 

wv_rural_pov %>%
  ggplot(aes(x = estimate, y = reorder(County_Name, estimate))) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Percent of People in Poverty by County in WV",
       subtitle = "2015-2019 American Community Survey",
       y = "",
       x = "") +
  geom_text(aes(label=estimate), position=position_dodge(width=0.9), vjust=0.3, hjust = .03)


# map

library(mapview)

wv_img <- get_acs(geography = "county", 
                  state = "West Virginia",
                  variables = c('poverty' = 'DP03_0128P'),
                  year = 2019,
                  geometry = TRUE) %>%
  separate(NAME, c("County_Name", "State"), sep = "([.,:])") %>%
  mutate(State = trimws(State)) %>%
  left_join(rural_des) %>%
  filter(Designation == 'Nonmetro')


mapview(wv_img, zcol = "estimate")
