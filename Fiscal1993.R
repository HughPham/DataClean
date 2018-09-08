library(readr)
library(tidyverse)
Fiscal1993 <- 
  read_csv("~/Data/Fiscal1993.csv", skip = 1) %>%
  filter(!str_detect(name, "上海|北京|天津|省$|自治区$"))  %>%
  mutate(city = ifelse(!is.na(city), name, city) %>%
           str_remove_all(., "[小|合|总]计$")) %>%
  mutate(county_type = ifelse(is.na(county_type),4,county_type))
for(i in 1:nrow(Fiscal1993)) {
  if (is.na(Fiscal1993$province)[i]) {
    Fiscal1993$province[i] <- Fiscal1993$province[i - 1]
  }
  if (is.na(Fiscal1993$city)[i]) {
    Fiscal1993$city[i] <- Fiscal1993$city[i - 1]
  }
}

Prov_Fiscal_1993 <-
  Fiscal1993 %>%
  filter(county_type != 3) %>%
  filter(str_detect(city, "(县级)$|(盟市)$")) %>%
  mutate(county_type = rep(c("A", "B", "C"), 30)) %>%
  mutate(city = province)

write_csv(Prov_Fiscal_1993, "Prov_Fiscal_1993.csv")


Fiscal1993 <-
  anti_join(
    Fiscal1993, Prov_Fiscal_1993 %>% select(id1993)
  ) 

City_Fiscal_1993 <-
  Fiscal1993 %>%
  filter(str_detect(name, "计$|级$")) 

County_Fiscal1993 <-
  Fiscal1993 %>%
  filter(!str_detect(name, "计$|级$"))  %>%
  mutate(province = str_sub(province, 1, 2)) %>%
  rename(prov2 = province)  %>%
  mutate(city = ifelse(str_detect("北京|上海|天津", prov2),
                       paste0(prov2, "市"),
                       city)) 

library(readr)
code <- read_csv("~/Documents/GitHub/province-managing-county/Results/code.csv")

code_1993 <-
  code %>%
  filter(year == 1993)

prov_code <-
  code %>% select(province, prov_code) %>%
  unique.data.frame()

code_1993_dup <-
  code_1993 %>%
  filter(code_1993 %>%
           select(county) %>% duplicated())

code_1993_sng <-
  code_1993 %>%
  filter(!code_1993 %>%
           select(county) %>% duplicated())

code_1993_auto <-
  code_1993_sng %>%
  filter(str_detect(county,"自治县"))

code_1993_qi <-
  code_1993_sng %>%
  filter( str_detect(county, "旗"))

County_Fiscal1993 <-
  inner_join(prov_code %>% mutate(prov2 = str_sub(province, 1, 2)), 
             County_Fiscal1993) %>%
  select(-prov2)

anti_join(
  County_Fiscal1993 %>% rename(county = name),
  code_1993_sng ,
  by = c("province", "prov_code",  "county")
) %>% View(.)

inner_join(
  County_Fiscal1993 %>% rename(county = name),
  code_1993_dup ,
  by = c("province", "prov_code", "city", "county")
)
