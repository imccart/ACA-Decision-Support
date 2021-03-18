# Meta --------------------------------------------------------------------

## Date Created:  6/5/2020
## Date Edited:   3/18/2021
## Description:   Summarize results from discrete choice models


# Average treatment effect on the treated ---------------------------------

pred.purchase <- all.prob %>% ungroup() %>%
  mutate(plan_name=str_replace(plan_name, "G.*","G"),
         plan_name=str_replace(plan_name, "P.*","P"),
         plan_name=str_replace(plan_name, "CAT.*","CAT"),
         plan_name=str_replace(plan_name, "BR.*","BR"))

plan.summary <- pred.purchase %>%
  group_by(plan_name) %>%
  summarize(obs_purchase=sum(obs_purchase, na.rm=TRUE), 
            pred_purchase=sum(pred_purchase, na.rm=TRUE))


## by metal 
metal.summary <- pred.purchase %>%
  separate(plan_name, c("insurer","metal"), sep="_") %>%
  mutate(metal=if_else(is.na(metal),"Uninsured",metal)) %>%
  group_by(metal) %>%
  summarize(obs_purchase=sum(obs_purchase, na.rm=TRUE), 
            pred_purchase=sum(pred_purchase, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(e_y1 = obs_purchase/sum(obs_purchase),
         e_y0 = pred_purchase/sum(pred_purchase),
         att = e_y1 - e_y0)

## by insurer
ins.summary <- pred.purchase %>%
  separate(plan_name, c("insurer","metal"), sep="_") %>%
  group_by(insurer) %>%
  summarize(obs_purchase=sum(obs_purchase, na.rm=TRUE), 
            pred_purchase=sum(pred_purchase, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(e_y1 = obs_purchase/sum(obs_purchase),
         e_y0 = pred_purchase/sum(pred_purchase),
         att = e_y1 - e_y0)




# 95% confidence intervals ------------------------------------------------

bs.pred.purchase <- bs.pred %>% ungroup() %>%
  mutate(plan_name=str_replace(plan_name, "G.*","G"),
         plan_name=str_replace(plan_name, "P.*","P"),
         plan_name=str_replace(plan_name, "CAT.*","CAT"),
         plan_name=str_replace(plan_name, "BR.*","BR"))

bs.metal.boot <- bs.pred.purchase %>%   
  separate(plan_name, c("insurer","metal"), sep="_") %>%
  mutate(metal=if_else(is.na(metal),"Uninsured",metal)) %>%
  group_by(metal, boot) %>%
  summarize(obs_purchase=sum(obs_purchase, na.rm=TRUE), 
            pred_purchase=sum(pred_purchase, na.rm=TRUE)) %>%
  group_by(boot) %>%
  mutate(e_y1 = obs_purchase/sum(obs_purchase),
         e_y0 = pred_purchase/sum(pred_purchase),
         att = e_y1 - e_y0)

bs.metal.summary <- bs.metal.boot %>% ungroup() %>%
  group_by(metal) %>%
  summarize(p01=quantile(att, 0.01),
            p05=quantile(att, 0.05),
            p95=quantile(att, 0.95),
            p99=quantile(att, 0.99))


bs.ins.boot <- bs.pred.purchase %>%   
  separate(plan_name, c("insurer","metal"), sep="_") %>%
  group_by(insurer, boot) %>%
  summarize(obs_purchase=sum(obs_purchase, na.rm=TRUE), 
            pred_purchase=sum(pred_purchase, na.rm=TRUE)) %>%
  group_by(boot) %>%
  mutate(e_y1 = obs_purchase/sum(obs_purchase),
         e_y0 = pred_purchase/sum(pred_purchase),
         att = e_y1 - e_y0)

bs.ins.summary <- bs.ins.boot %>% ungroup() %>%
  group_by(insurer) %>%
  summarize(p01=quantile(att, 0.01),
            p05=quantile(att, 0.05),
            p95=quantile(att, 0.95),
            p99=quantile(att, 0.99))


metal.final <- metal.summary %>%
  left_join(bs.metal.summary, by="metal")

ins.final <- ins.summary %>%
  left_join(bs.ins.summary, by="insurer")

# Final summary results ---------------------------------------------------

choice.metals <- metal.final %>% filter(metal!="Uninsured") %>%
  mutate(metal = case_when(
    metal=="G" ~ "Gold",
    metal=="BR" ~ "Bronze",
    metal=="P" ~ "Platinum",
    metal=="SIL" ~ "Silver",
    metal=="CAT" ~ "Catastrophic"
  )) %>%
  mutate(metal = factor(metal, levels=c("Platinum","Gold","Silver","Bronze","Catastrophic"))) %>%
  ggplot(aes(x=as.factor(metal),y=att)) +
  geom_hline(aes(yintercept=0),linetype="dashed") +
  geom_errorbar(aes(ymin=p05, ymax=p95),
                lwd=1, width=0, position=position_dodge(width=0.5)) +
  labs(
    y="Estimate and \n95% Confidence Interval",
    x="Metal Level") +
  geom_point(size=2,position=position_dodge(width=0.5)) +
  theme_bw() + ggsave("figures/choice_metals.png")


choice.insurer <- ins.final %>%
  mutate(insurer = case_when(
    insurer=="ANT" ~ "Anthem",
    insurer=="BS" ~ "BCBS",
    insurer=="HN" ~ "HealthNet",
    insurer=="KA" ~ "Kaiser",
    insurer=="Small" ~ "Other",
    insurer=="Uninsured" ~ "Uninsured"
  )) %>%
  mutate(insurer = factor(insurer, levels=c("Anthem","BCBS","HealthNet","Kaiser","Other","Uninsured"))) %>%
  ggplot(aes(x=as.factor(insurer),y=att)) +
  geom_hline(aes(yintercept=0),linetype="dashed") +
  geom_errorbar(aes(ymin=p05, ymax=p95),
                lwd=1, width=0, position=position_dodge(width=0.5)) +
  labs(
    y="Estimate and \n95% Confidence Interval",
    x="Insurer") +
  geom_point(size=2,position=position_dodge(width=0.5)) +
  theme_bw() + ggsave("figures/choice_insurer.png")
