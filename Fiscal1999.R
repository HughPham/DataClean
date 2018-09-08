library(readr)
rm(list = ls())
fyear <- 2003
fiscal_url <- paste0("~/Data/Fiscal", fyear , ".csv")
Fiscal <-
  read_csv(fiscal_url,
           locale = locale(encoding = "GB2312")) %>%
  filter(!str_detect(name, "上海|北京|天津|重庆|省$|自治区$")) %>%
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

Prov_Fiscal <-
  Fiscal %>%
  filter(province == city) %>%
  filter(is.na(county_type))


Fiscal <-
  anti_join(Fiscal ,
            Prov_Fiscal %>% select(id_fiscal)) %>%
  mutate(county_type = ifelse(is.na(county_type), 9 , county_type))


City_Fiscal <-
  Fiscal %>%
  filter(!county_type %in% c(3, 4))

County_Fiscal  <-
  Fiscal %>%
  filter(county_type %in% c(3, 4)) %>%
  mutate(province = str_sub(province, 1, 2)) %>%
  rename(prov2 = province)


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
             County_Fiscal) %>%
  select(-prov2)

# Clean Municipality
County_Fiscal_Municipality <-
  County_Fiscal %>%
  filter(str_detect(province, "北京|上海|天津|重庆")) %>%
  mutate(county2 = str_remove_all(name, "[市县区]$")) %>%
  select(-city)
code_Municipality <-
  code %>%
  filter(str_detect(province, "北京|上海|天津|重庆")) %>%
  mutate(county2 = str_remove_all(county, "[市县区]$"))

temp0 <-
  right_join(code_Municipality, County_Fiscal_Municipality) %>%
  select(-county2)



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
      mutate(county2 = str_sub(name, 1, 2))  %>% select(-city),
    by = c("province", "prov_code", "county2")
  )
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
             mutate(county = name,
                    city2 = str_sub(city, 1, 2)) %>%
             select( - city))


# Need to Clean
Change <- read_excel("~/Data/Change.xlsx")
temp6 <-
  County_Fiscal %>%
  anti_join(., temp5 %>% select(id_fiscal))  %>%
  right_join(Change, .)  %>%
  mutate(county = ifelse(is.na(county) , name, county)) %>% 
  right_join(code, ., by = c("province", "prov_code", "county"),
             suffix = c("","_fiscal")) %>%
  mutate(city = ifelse(is.na(city), city_fiscal, city))

County_Fiscal <- 
bind_rows(temp0,
          temp1,
          temp2,
          temp3,
          temp4,
          temp5,
          temp6) 
County_Fiscal %>%
  group_by(code) %>%
  filter(n() > 1) %>% View(.)

prov_url <- paste0("~/Data/prov_fiscal",fyear,".csv")
write_csv(Prov_Fiscal,prov_url )
city_url <- paste0("~/Data/city_fiscal",fyear,".csv")
write_csv(City_Fiscal,city_url )
county_url <- paste0("~/Data/county_fiscal",fyear,".csv")
write_csv(County_Fiscal,county_url )

