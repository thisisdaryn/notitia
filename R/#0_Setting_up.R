library(readr)
library(dplyr)

unemp <- read_csv("../data/US_unemp.csv")
chi_emps <- read_csv("../data/ChicagoEmployees.csv")
countries <- read_csv("../data/countries")


set.seed(1731)
s1 <- sample(1:10, 2, replace = FALSE)
s2 <- sample(1:10, 3, replace = FALSE)

populations <- read_csv("../data/country_populations.csv") %>%
  slice(-s1) %>% arrange(desc(pop)) %>%
  mutate(pop = pop%/%1000000)
areas <- read_csv("../data/country_area.csv") %>%
  slice(-s2) %>% arrange(desc(area)) %>%
  mutate(area = area%/%1000)


populations_full <- read_csv("../data/country_populations.csv")
areas_full <- read_csv("../data/country_area.csv")



lebron <- read_csv("../data/lebron.csv") %>%
  select(-c(4,11,14:18,21,24))

carmelo <- read_csv("../data/carmelo.csv") %>%
  select(-c(4,11,14:18,21,24))

penny <- read_csv("../data/penny.csv") %>%
  select(-c(4,11,14:18,21,24))

kemp <- read_csv("../data/kemp.csv") %>%
  select(-c(4,11,14:18,21,24))



capitals <- read_csv("../data/country_capitals.csv")

nyc_ed <- read_csv("../data/2012_SAT_Results.csv")

apple_prod <- read_csv("../data/apple_prod.csv") %>%
  arrange(Year, Quarter, Product) %>% filter(Year >= 2015)

apple_prod <- unique(apple_prod)

flight_data <- read_csv("../data/FlightData.csv")

save(unemp, chi_emps, populations, areas, populations_full, areas_full,
     capitals, lebron, carmelo, penny, kemp, nyc_ed, apple_prod,flight_data,
     file = "data/notitia.RData")

