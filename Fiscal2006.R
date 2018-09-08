library(readr)
rm(list = ls())

Fiscal2006 <- 
  read_csv("~/Data/Fiscal2006.csv", 
                       locale = locale(encoding = "GB2312"))

Fiscal2006_part1 <-
  Fiscal2006 %>%
  filter(str_detect(name3, "本级|[总合小]计"))


Fiscal2006 <-
  Fiscal2006 %>%
  filter(!str_detect(name3, "本级|[总合小]计"))


code <-
  read_csv("~/Data/code_id.csv")  %>%
  filter(year == 2006)

left_join(code,Fiscal2006) %>% 
  write_csv(.,"~/Data/county_fiscal2006.csv")