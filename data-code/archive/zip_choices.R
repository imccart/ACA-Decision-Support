# Clean zipcode choice data -----------------------------------------------

zip3.choices <- choice.data %>%
  rename(year=Year,
         region=Region,
         county=County,
         zip=Zip,
         BlueShield_EPO=Blue_Shield_EPO,
         BlueShield_HMO=Blue_Shield_HMO,
         BlueShield_PPO=Blue_Shield_PPO,
         ChineseCommunity=Chinese_Community,
         ContraCosta=Contra_Costa,
         HealthNet_HMO=Health_Net_HMO,
         HealthNet_HSP=Health_Net_HSP,
         HealthNet_EPO=Health_Net_EPO,
         HealthNet_PPO=Health_Net_PPO,
         LACare=LA_Care) %>%
  mutate(Sharp = case_when(
    Sharp1 == 1 ~ 1,
    Sharp2 == 1 ~ 1,
    TRUE ~ NA_real_
  )) %>%
  select(-c(Sharp1, Sharp2))

zip3.choices <- pivot_longer(zip3.choices,
                             cols=-c("year","region","county","zip"),
                             names_to="product",
                             values_to="offered") %>%
  mutate(zip3=as.numeric(str_sub(zip, 1, 3)),
         offered=ifelse(is.na(offered),0,offered)) %>%
  group_by(year, region, zip3, product) %>%
  summarize(offered = ifelse(sum(offered, na.rm=TRUE)>0,1,0)) %>%
  filter(offered==1) %>%
  select(year, region, zip3, product) %>%
  mutate(product2=product) %>%
  separate(product2, sep="_", into=c("insurer","plan_network_type","MSP")) %>%
  mutate(MSP = ifelse(MSP=="MSP",1,0))
  
  

