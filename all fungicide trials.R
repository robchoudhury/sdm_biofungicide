library(tidyverse)
library(Cairo)
library(gridExtra)
library(viridis)
library(lsmeans)


#you can skip this bit, unless you want to look at raw data
fungicide=read_csv(file=('all2.csv')) %>%
  mutate(Treat = as.factor(toupper(Treat)))
#fungicide$Treat=revalue(fungicide$Treat, c("AGBIOME"="HOWLER","BMJ"="LIFEGARD","SERANADE OPTIMUM"="SERENADE OPTIMUM"))
keeps=c("Actinovate", "Oxidate", "Cueva", "Serenade", "Taegro", "Untreated", "Zampro")
keeps=toupper(keeps)

fungicide.keep=fungicide %>%
  filter(., Treat %in% keeps)

ggplot(fungicide, aes(Treat, Incidence))+
  geom_jitter(alpha=0.2)+geom_boxplot(outlier.shape = NA, alpha=0.7)+
  stat_summary(fun.y="mean", geom = "point", shape=19, size=3)+
  facet_grid(Trial~., scales="free")+
  theme_bw()+ 
  xlab("Fungicide Treatment")+ ylab("Disease Incidence (%)")+
  #ylim(-5,110)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face="bold", size=22),
        strip.text = element_text( face="bold",size=20),
        axis.text = element_text( size=14),
    panel.background = element_rect(fill="transparent", color=NA))#,plot.background = element_rect(fill="transparent", color=NA)

ggsave(filename = "fungicidecairo.png",width =13, height=7, units = "in",dpi = 300,type="cairo-png", bg="transparent" )  #

ggplot(fungicide, aes(Treat, Incidence))+
  geom_jitter(alpha=0.4, aes(color=Trial))+geom_boxplot(outlier.shape = NA, alpha=0.7)+
  #stat_summary(fun.y="weighted.mean", geom = "point", shape=19, size=3)+
  stat_summary(fun.y="mean",  geom = "point", shape=19, size=3)+
    #facet_grid(Trial~., scales="free")+
  theme_bw()+ 
  xlab("Fungicide Treatment")+ ylab("Disease Incidence (%)")+
  #ylim(-5,110)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(face="bold", size=22),
        strip.text = element_text( face="bold",size=20),
        axis.text = element_text( face="bold",size=14),
        panel.background = element_rect(fill="transparent", color=NA),
        legend.position = c(0.4, 0.8),
        legend.title = element_text(face="bold", size=16),
        legend.text = element_text(face="bold", size=14))#,plot.background = element_rect(fill="transparent", color=NA)

ggsave(filename = "fungicidecairo_colored.png",width =13, height=7, units = "in",dpi = 300,type="cairo-png", bg="transparent" )  #

fungicide1 = df[order(df$mean),]


ggplot(fungicide, aes(reorder(Treat, scaled.incidence, FUN = mean), scaled.incidence))+
  geom_jitter(alpha=0.4, size=3,width = 0.2, aes(color=Trial))+
  geom_boxplot(outlier.shape = NA, alpha=0.7)+
  stat_summary(fun.y="mean", geom = "point", shape=19, size=3)+
  theme_bw()+ 
  xlab("Fungicide Treatment")+ ylab("Scaled Disease Incidence")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(face="bold", size=22),
        strip.text = element_text( face="bold",size=20),
        axis.text = element_text( face="bold",size=18),
        panel.background = element_rect(fill="transparent", color=NA),
        legend.position = c(.1,.8),
        legend.title = element_text(face="bold", size=24),
        legend.text = element_text(face="bold", size=20))#,plot.background = element_rect(fill="transparent", color=NA)

ggsave(filename = "fungicidecairo_colored_sorted_scaled.png",width =13, height=7, units = "in",dpi = 300,type="cairo-png", bg="transparent" )  #


