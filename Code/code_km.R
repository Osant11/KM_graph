library(haven)
library(ggplot2)
library(plyr)
library(dplyr)
library(survminer)
library(survival)
library(tidyr)
library(broom)
library(stringr)
library(gridExtra)
library(grid)
library(ggpubr)


### Load data_km ### 
  

##### Kaplan-Meier estimates at each year from Coxph ####
res.cox <- coxph(Surv(TimeTrt, censor) ~ TRT, data = data_km)
summary(res.cox)

fit <- survfit(Surv(TimeTrt, censor) ~ TRT, data = data_km)

cs <- tidy(fit)

#Algorithm to get te closest estimates at each year
df <- cs[NULL,]
for (i in 1:9){
  df_i <- cs %>%
    group_by(strata) %>%
    mutate(
      delta = abs(time - i),
      year = i
    ) %>%
    filter(delta == min(delta)) %>%
    select(-delta)
  
  df <- bind_rows(df, df_i)
}

df <- df %>% 
  mutate(TRT = as.factor(str_remove(strata, "TRT="))) %>% 
  dplyr::rename(surv = estimate, 
         high = conf.high, 
         low = conf.low) %>% 
  select(time, TRT, high, low, year)


#### KM Plot ####
res <- ggsurvplot(fit,
                  data = data_km,
                  break.time.by = 1, 
                  surv.scale = "percent",
                  legend.title = "",
                  legend.labs = c("Treatment A", "Treatment B", "Treatment C"),
                  legend = c(0.8, 0.92),
                  #pval = "Hazard-Ratio vs 20mg = 0.93    Hazard-Ratio vs 40mg = 1.14 \n95% CI: (0.68, 1.25)                   95% CI: (0.83, 1.55 ) \np = 0.62                                      p = 0.42",
                  pval.size = 4,
                  pval.coord = c(0.4, 0.15),
                  risk.table = TRUE, 
                  censor = FALSE, 
                  ncensor.plot = FALSE, 
                  palette = c("RoyalBlue3", "tomato3", "SeaGreen"), 
                  conf.int = FALSE, 
                  conf.int.style = "step",
                  conf.int.alpha = 0.15,
                  surv.plot.height = 0.95, 
                  fontsize = 4,
                  tables.col = "strata",
                  risk.table.height = 0.20, 
                  xlab = "Years since active treatment start", 
                  font.x = 12,
                  ylab = "Percent event free \n (Kaplan-Meier)", 
                  font.y = 12
)


# Changing theme of table
res$table <- res$table + theme(axis.line = element_blank(), 
                               axis.text.x=element_blank(),
                               axis.title.x=element_blank(), 
                               axis.text.y = element_text(face = "bold"), 
                               axis.ticks=element_blank(), 
                               legend.position = "none",
                               plot.title = element_text(size = 13)) + 
  geom_text(aes(fontface=2, col = TRT))


# Addind CI's at each year
res$data.survplot <- res$data.survplot %>% 
  left_join(df, by = c("TRT", "time")) %>% 
  filter(!is.na(high)) %>% 
  arrange(year)


# Adding geom_errobar
res$plot <- res$plot + 
  geom_errorbar(data = res$data.survplot, aes(x = year, ymin = lower, ymax = upper, col = TRT, group = TRT), 
                position = position_dodge(width = 0.4), width=0.25) +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(vjust = -0.25),
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(vjust = -12.5))


#Final plot
grid.arrange(ggarrange(res$plot + theme(plot.margin=unit(c(0.2,1,0.2,-0.8),"cm")), 
                       res$table + theme(plot.margin=unit(c(0,0,0,-0.8),"cm")), 
                       ncol = 1, align = "v", heights = c(40,9)), 
             top = textGrob(paste("Time to first event", 
                                  "\nAnalysis Set: xxx", sep = ""), x = 0.0071, 
                            hjust = 0, gp = gpar(fontface = 3L, fontsize = 11)),
             bottom = textGrob("Kaplan-Meier estimates are presented. Bars on graph display pointwise 95% confidence limits of the estimate.", x = 0.001, 
                               hjust = 0, gp = gpar(fontface = 3L, fontsize = 11))
)
