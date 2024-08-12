library(tidyverse)
library(readxl)

mydata = read_excel(file.choose()) %>% 
  mutate(Software = factor(Software)) %>% 
  mutate(Date = factor(Date))

mydata2 = mydata %>% 
  filter(Value <2) %>% 
  filter(Value > -2)

cort_test = aov(Value ~ Software * Date,data = mydata)
summary(cort_test)
emmeans::emmeans(cort_test, list(pairwise ~ Software + Date),adjust = "tukey")

cort_test2 = aov(Value ~ Software * Date,data = mydata2)
summary(cort_test2)
emmeans::emmeans(cort_test2, list(pairwise ~ Software + Date),adjust = "tukey")

thudata = read_excel(file.choose(),
                     sheet = "filo") %>% 
  filter(Value >0) %>% 
  mutate(Plasmid = factor(Plasmid)) %>% 
  mutate(Crop = factor(Crop))

crop_test = aov(Value ~ Crop * Plasmid, data = thudata)
summary(crop_test)
emmeans::emmeans(crop_test, list(pairwise ~ Crop * Plasmid), adjust = "tukey")
