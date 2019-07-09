library(readr)
library(dplyr)
library(stringr)
library(tidyr)


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


complete_populations <- read_csv("../data/country_populations.csv")
complete_areas <- read_csv("../data/country_area.csv")




jordan <- read_csv("../data/basketball/jordan.csv") %>%
  select(-c(4,11,14:18,21,24))
lebron <- read_csv("../data/basketball/lebron.csv") %>%
  select(-c(4,11,14:18,21,24))









capitals <- read_csv("../data/country_capitals.csv")

nyc_sat12 <- read_csv("../data/2012_SAT_Results.csv")
nyc_sat10 <- read_csv("../data/2010_SAT_Results.csv")

apple_prod <- read_csv("../data/apple_prod.csv") %>%
  arrange(Year, Quarter, Product) %>% filter(Year >= 2015)

apple_prod <- unique(apple_prod)

flight_data <- read_csv("../data/FlightData.csv")



######## tennis
rafa_novak <- read_tsv("../data/NadalDjokovic.txt") %>%
  mutate(Loser = ifelse(Winner == "Rafael Nadal", "Novak Djokovic", "Rafael Nadal"))
novak_roger <- read_tsv("../data/FedererDjokovic.txt") %>%
  mutate(Loser = ifelse(Winner == "Novak Djokovic", "Roger Federer", "Novak Djokovic"))
rafa_roger <- read_tsv("../data/NadalFederer.txt") %>%
  mutate(Loser = ifelse(Winner == "Rafael Nadal", "Roger Federer", "Rafael Nadal"))



### Lara batting statistics

lara_odi <- read_csv("../data/lara_odi.csv")
lara_tests <- read_csv("../data/Lara_tests.csv")
lara_tests$Wkts <- ifelse(lara_tests$Wkts == "-", NA, lara_tests$Wkts)
lara_tests$Conc <- ifelse(lara_tests$Conc == "-", NA, lara_tests$Conc)
lara_tests$Bat1 <- ifelse(lara_tests$Bat1 == "-", NA, lara_tests$Bat1)
lara_tests$Bat2 <- ifelse(lara_tests$Bat2 == "-", NA, lara_tests$Bat2)
lara_tests$Runs <- ifelse(lara_tests$Runs == "-", NA, lara_tests$Runs)
lara_tests$Ct <- ifelse(lara_tests$Ct == "-", NA, lara_tests$Ct)
lara_tests$St <- ifelse(lara_tests$St == "-", NA, lara_tests$St)






lara_tests <- gather(lara_tests, key = "Inning", value = Bat1, 1:2) %>%
  arrange(Match) %>%  mutate(Notout = str_detect(Bat1, "\\*"),
                             DNB = if_else(Bat1 %in% c("DNB", "TDNB"), TRUE, FALSE)) %>%
  separate(Bat1, into = c("Bat","extra"), sep = "\\*") %>%
  mutate(Bat1 = as.numeric(Bat)) %>% separate(Opposition, into = c("extra2", "Opp"), sep = "v " ) %>%
  select(Bat1, Inning, Notout, DNB, Opp, Ground, Match, `Start Date`) %>%
  rename(Runs = Bat1)
#Wkts, Conc, Ct, St,
#Wkts, Conc, Ct, St,


lara_tests$Inning <- ifelse(lara_tests$Inning == "Bat1", 1, 2)

lara_odi <- mutate(lara_odi, Notout = str_detect(Bat1, "\\*"),
                   DNB = if_else(Bat1 %in% c("DNB", "TDNB"), TRUE, FALSE)) %>%
  separate(Bat1, into = c("Bat","extra"), sep = "\\*") %>%
  mutate(Bat1 = as.numeric(Bat), Inning = 1) %>% separate(Opposition, into = c("extra2", "Opp"), sep = "v " ) %>%
  rename(Runs = Bat1) %>%
  select(Runs, Inning, Notout, DNB, Opp, Ground, `Start Date`, Match)

lara <- bind_rows(lara_odi, lara_tests) %>% mutate(Date = `Start Date`) %>%
  separate(`Date`, into = c("Day", "Month", "Year"), sep = "-")
lara$Year <- as.numeric(lara$Year)
lara$Year <- ifelse(lara$Year > 10, 1900 + lara$Year, 2000 + lara$Year)


lara$Month <- factor(lara$Month, levels = c("Jan", "Feb", "Mar", "Apr",
                                            "May", "Jun", "Jul", "Aug",
                                            "Sep", "Oct", "Nov", "Dec"))

lara <- lara %>% arrange(Year, Month, Day) %>%
  select(-Year, -Day, -Month)

lara$Runs <- as.integer(lara$Runs)
lara$Inning <- as.factor(lara$Inning)

lara <- as.data.frame(lara)


write_csv(lara, "../data/lara.csv")

## world electricity consumption

electricity <- read_csv("../data/electricity.csv")


## ESS media and social values survey

ESS_MedSoc <- read_csv("../data/ESS_MedSoc.csv") %>%
    select( -cproddat, -name, -edition, -cedition, -cname)


save(unemp, chi_emps, populations, areas, complete_populations, complete_areas,
     capitals, lebron, jordan, nyc_sat10, nyc_sat12, apple_prod,flight_data, rafa_novak,
     lara, electricity, ESS_MedSoc,
     file = "data/notitia.RData")



#
#carmelo <- read_csv("../data/carmelo.csv") %>%
#  select(-c(4,11,14:18,21,24))

#penny <- read_csv("../data/penny.csv") %>%
#  select(-c(4,11,14:18,21,24))

#kemp <- read_csv("../data/kemp.csv") %>%
#  select(-c(4,11,14:18,21,24))

#bird <- read_csv("../data/basketball/bird.csv") %>%
#  select(-c(4,11,14:18,21,24))
#isiah <- read_csv("../data/basketball/isiah.csv") %>%
#  select(-c(4,11,14:18,21,24))
#kobe <- read_csv("../data/basketball/kobe.csv") %>%
#  select(-c(4,11,14:18,21,24))
#magic <- read_csv("../data/basketball/magic.csv") %>%
#  select(-c(4,11,14:18,21,24))
#timmy <- read_csv("../data/basketball/timmy.csv") %>%
#  select(-c(4,11,14:18,21,24))
#hakeem <- read_csv("../data/basketball/hakeem.csv") %>%
#  select(-c(4,11,14:18,21,24))
#shaq <- read_csv("../data/basketball/shaq.csv") %>%
#  select(-c(4,11,14:18,21,24))

