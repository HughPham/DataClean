library(readr)
library(tidyverse)
library(readxl)
rm(list = ls())
fyear <- 2005
fiscal_url <- paste0("~/Data/Fiscal", fyear , ".csv")
Fiscal <-
  read_csv(fiscal_url,
           locale = locale(encoding = "GB2312")) %>%
  mutate(id_fiscal = 1:n()) %>%
  mutate(name = str_remove_all(name, "[^\u4e00-\u9fa5]")) %>%
  mutate(city = ifelse(is.na(city), city,
                       ifelse(
                         str_detect(name, "^[\u4e00-\u9fa5]{2,}[市(地区)州盟]"),
                         str_extract(name, "^[\u4e00-\u9fa5]{2,}[市(地区)州盟]"),
                         city
                       )))


for (i in 1:nrow(Fiscal)) {
  if (is.na(Fiscal$province)[i]) {
    Fiscal$province[i] <- Fiscal$province[i - 1]
  }
  if (is.na(Fiscal$city)[i]) {
    Fiscal$city[i] <- Fiscal$city[i - 1]
  }
}

# Prov_Fiscal <-
#   Fiscal %>%
#   filter(province == city) %>%
#   filter(is.na(county_type))
# 
# 
# Fiscal <-
#   anti_join(Fiscal ,
#             Prov_Fiscal %>% select(id_fiscal)) %>%
#   mutate(county_type = ifelse(is.na(county_type), 9 , county_type))
# 
# 
City_Fiscal <-
  Fiscal %>%
  filter(!county_type %in% c(3, 4))
# 
# County_Fiscal  <-
#   Fiscal %>%
#   filter(county_type %in% c(3, 4)) %>%
#   mutate(province = str_sub(province, 1, 2)) %>%
#   rename(prov2 = province)


Change <- read_excel("~/Data/Change.xlsx")

County_Fiscal <-
  Fiscal %>%
  filter(!is.na(county_type)) %>%
  right_join(Change, ., ) %>%
  mutate(county = ifelse(is.na(county), name, county)) %>%
  filter(county_type == 3 | county_type == 4)


library(readr)
code <-
  read_csv("~/Data/code_id.csv")  %>%
  filter(year == fyear)

# Adding Province Name
prov_code <-
  code %>% select(province, prov_code) %>%
  unique.data.frame() %>%
  mutate(prov2 = str_sub(province, 1, 2))
County_Fiscal <-
  inner_join(prov_code,
             County_Fiscal %>%
               mutate(prov2 = str_sub(province,1,2)) %>%
               select( - province)) %>%
  select(-prov2) 

# Clean Municipality
County_Fiscal_Municipality <-
  County_Fiscal %>%
  filter(str_detect(province, "北京|上海|天津|重庆")) %>%
  mutate(county2 = str_remove_all(county, "[市县区]$")) %>%
  select(-city, - county) 
code_Municipality <-
  code %>%
  filter(str_detect(province, "北京|上海|天津|重庆")) %>%
  mutate(county2 = str_remove_all(county, "[市县区]$"))

temp0 <-
  right_join(code_Municipality, County_Fiscal_Municipality) %>%
  select(-county2)

# temp0 %>% arrange(county_type) %>%
# View()


# Non-Municipality Data
County_Fiscal <-
  County_Fiscal %>%
  filter(!str_detect(province, "北京|上海|天津|重庆"))
code <-
  code %>%
  filter(!str_detect(province, "北京|上海|天津|重庆")) 

# Auto_county
code_auto <-
  code %>%
  filter(str_detect(county, "自治县$"))
temp1 <-
  left_join(
    code_auto %>%
      mutate(county2 = str_sub(county, 1, 2)),
    County_Fiscal %>%
      mutate(county2 = str_sub(name, 1, 2))  %>% select(-city, - county),
    by = c("province", "prov_code", "county2")
  )
# temp1 %>% arrange(county_type) %>%
#   View()

# Qi
County_Fiscal <-
  County_Fiscal %>%
  anti_join(., temp1 %>% select(id_fiscal))
code_qi <-
  code %>%
  filter(str_detect(county, "旗$"))
temp2 <-
  left_join(
    code_qi ,
    County_Fiscal %>% mutate(county = name) %>% select(-city),
    by = c("province", "prov_code", "county")
  )

# temp2 %>% arrange(county_type) %>%
# View()


# Duplicated County Name
County_Fiscal <-
  County_Fiscal %>%
  anti_join(., temp2 %>% select(id_fiscal))
code <-
  code %>%
  filter(!str_detect(county, "自治县$|旗$"))
code_dup <-
  code %>%
  group_by(province, county) %>%
  filter(n() > 1) %>%
  ungroup()
temp3 <- 
  left_join(code_dup,
            County_Fiscal %>%
              mutate(county = name))

# Single Name
County_Fiscal <-
  County_Fiscal %>%
  anti_join(., temp3 %>% select(id_fiscal))
code <-
  code %>%
  group_by(province, county) %>%
  filter(n() == 1) %>%
  ungroup()

# Same Name within City
code_same <-
  code %>%
  group_by(province, str_sub(city, 1, 2),
           str_sub(county, 1, 2)) %>%
  filter(n() > 1) %>%
  ungroup()
temp4 <-
  left_join(code_same %>%
              mutate(city2 = str_sub(city, 1, 2)),
            County_Fiscal %>% 
              mutate(county = name,
                     city2 = str_sub(city, 1, 2)) %>%
              select( - city)) 

# Others
code <-
  code %>%
  group_by(province, str_sub(city, 1, 2),
           str_sub(county, 1, 2)) %>%
  filter(n() == 1) %>%
  ungroup()
County_Fiscal <-
  County_Fiscal %>%
  anti_join(., temp4 %>% select(id_fiscal))
temp5 <-
  inner_join(code %>% mutate(city2 = str_sub(city, 1, 2)),
             County_Fiscal %>% 
               mutate(city2 = str_sub(city, 1, 2)) %>%
               select( - city))


# Need to Clean
temp6 <-
  County_Fiscal %>%
  anti_join(., temp5 %>% select(id_fiscal))  %>%
  right_join(code, ., by = c("province", "prov_code","city", "county"),
             suffix = c("","_fiscal"))

County_Fiscal <- 
  bind_rows(temp0,
            temp1,
            temp2,
            temp3,
            temp4,
            temp5,
            temp6) 
County_Fiscal %>%
  group_by(id_fiscal) %>% 
  filter(n() > 1) %>% View(.)

prov_url <- paste0("~/Data/prov_fiscal",fyear,".csv")
write_csv(Prov_Fiscal,prov_url )
city_url <- paste0("~/Data/city_fiscal",fyear,".csv")
write_csv(City_Fiscal,city_url )
county_url <- paste0("~/Data/county_fiscal",fyear,".csv")
write_csv(County_Fiscal,county_url )

