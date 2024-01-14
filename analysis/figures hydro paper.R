library(here)
library(tidyverse)
library(zoo)
library(gridExtra)


fr<- read.csv(here("analysis", "figures hydro paper.csv"))

together_3_hydro<- read.csv(here("analysis", "together_hydro_Sep.22.csv"))

together_1 <- read.csv(here("analysis", "together_1.csv"))

organic4 <- read.csv(here("analysis", "organic3.csv"))


plot_sed_time_2 <- together_3_hydro %>%
        select(median, sedrate.mm) %>%
        na.omit() %>%
        unique()%>%
        ggplot() +
        geom_path(aes(x = sedrate.mm, y = (median/1000)))+
        xlab(expression(atop("Sed rate", paste ("(mm/yr)")))) +
        ylab("Age (ka)")+
        theme_minimal(base_size = 20) +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        scale_x_continuous(limits=c(0,2),breaks = seq(0, 2, by = 1))+
        theme(axis.title.x=element_text(size=14),axis.text.x=element_text(size=12),axis.title.y=element_text(size=14))+
        theme(
              plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.border = element_blank())

plot_sed_time_3<- plot_sed_time_2 +scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) + 
        geom_hline(yintercept=c(9.7, 18.2,29.1))
#theme(axis.title=element_text(size=22), axis.text=element_text(size=16))

plot_05_time <- fr %>%
        select(median, d.0.5) %>%
        na.omit() %>%
        ggplot() +
        geom_point(aes(x = d.0.5, y = median/1000), size = 1, alpha = 0.75) + geom_path(aes(x = d.0.5, y = median/1000), size = 1, alpha = 0.75)+
        xlab(expression(atop("Median grain",paste("size (\u03BCm)")))) +
        theme_minimal(base_size = 20)+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(axis.title.x=element_text(size=14),axis.text.x=element_text(size=12),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())+
        theme(
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank())
plot_05_time2<- plot_05_time +scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) + 
        geom_hline(yintercept=c(9.7, 18.2,29.1))

plot_frac2 <- fr %>%
        select(median, Fraction,Percentage) %>%
        na.omit() %>%
        unique()%>%
        ggplot() +
        geom_point(aes(x = Percentage , y = median/1000, color=Fraction))+ geom_path(aes(x = Percentage, y = median/1000, color=Fraction))+
        xlab(expression(atop("Percentage", paste("(%)")))) +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme_minimal(base_size = 20)+
        theme(axis.title.x=element_text(size=14),axis.text.x=element_text(size=12),
              axis.title.y = element_blank(), axis.text.y = element_blank(),
               legend.position="none")+ annotate("text",x=65,y=0,label="Silt",size=6,color="black")+
        annotate("text",x=20,y=0,label="Coarse sand",size=5,color="green")+
        annotate("text",x=30,y=25,label="Clay",size=6,color="red")+
        annotate("text",x=35,y=31,label="Sand",size=6,color="turquoise")+
        theme(
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank())

plot_frac <- fr %>%
        select(median, Fraction,Percentage) %>%
        na.omit() %>%
        unique()%>%
        ggplot() +
        geom_point(aes(x = Percentage , y = median/1000, color=Fraction))+ geom_path(aes(x = Percentage, y = median/1000, color=Fraction))+
        scale_color_manual(values=c("blue", "red", "green", "black" ))+
        xlab(expression(atop("Percentage", paste("(%)")))) +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme_minimal(base_size = 20)+
        theme(axis.title.x=element_text(size=14),axis.text.x=element_text(size=12),
              axis.title.y = element_blank(), axis.text.y = element_blank(),
              legend.position="none")+ annotate("text",x=65,y=0,label="Silt",size=6,color="black")+
        annotate("text",x=20,y=0,label="Coarse sand",size=5,color="red")+
        annotate("text",x=30,y=25,label="Clay",size=6,color="blue")+
        annotate("text",x=35,y=31,label="Sand",size=6,color="green")+
        theme(
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank())
plot_frac3 <- plot_frac +scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) + 
        geom_hline(yintercept=c(9.7, 18.2,29.1))
#fr<-ages_final_ITRAX2%>%
 #       select(Depth, 'median')%>%
  #      merge(sizes4,all = TRUE)%>%
   #     na.omit()
#plot_frac

##################### fig 4
#a <- grid.arrange(plot_sed_time_2,plot_05_time,plot_frac,ncol=3)
#?grid.arrange
#write.csv(fr, file = here("analysis", "figures hydro paper.csv"))
#write.csv(together_3, file = here("analysis", "together_hydro_Sep.22.csv"))
#write.csv(together_1, file = here("analysis", "together_1.csv"))
##########################

plot_4_21 <- prcurve_ITRAX5 %>%
        select(NorK,NorCa, median)%>%
        #  select(1:7,9)%>%
        gather(Element,counts,1:2,-(3))%>%
        na.omit() %>%
        mutate(facet_label=fct_recode(
                Element,
                "Ca" = "NorCa",
                "K" = "NorK"))%>%
        ggplot(aes(y = median/1000, x = counts)) +
        geom_point()+
        geom_path()+
        xlab("") +
        theme_bw(base_size = 20) +
        facet_wrap(~facet_label, scales = "fixed",labeller=label_parsed, ncol=7)+
        scale_y_reverse(limits=c(33,4),breaks = seq(4, 33, by = 2)) +
        # scale_x_continuous(labels = scales::number_format(accuracy = 0.1))%>%
        labs(x = "Normalized ratio", y = "Age (ka)")
#  theme(axis.title.x=element_text(size=22),axis.text.x=element_text(size=16),
print(plot_4_21)

plot_4_21+scale_x_continuous(breaks=c(0.05,0.1,0.2),
                             labels = scales::number_format(accuracy = 0.05)) + 
        geom_hline(yintercept=c(9.7, 18.2,29.1))


si_al_2<-together_3%>% 
        select(median,roll_Si_Al)%>%
        na.omit()%>%
        ggplot(aes(x = roll_Si_Al, y = median/1000)) +
        # geom_path() +
        geom_point() +
        labs(x="Si:Al",y="Age (ka)")+
        theme_bw(base_size = 20)+
        #   geom_smooth(method = "gam")+
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))
si_al_2

sea_level_all<-read.csv(here("data","all_sea_level.csv"))

level_graph<- sea_level_all%>% ggplot() +
        geom_point(aes(x = level, y = age), size = 1, alpha = 0.75) + 
        xlab("Sea level (m)") +
        ylab("Age (ka)")+
        theme_bw(base_size = 20) +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        theme(
                axis.title.y = element_blank(), axis.ticks.y= element_blank(),
                axis.text.y = element_blank()) 
#   geom_hline(yintercept=29, color="red")+
# annotate("rect", xmin=c(-150),xmax=c(Inf),ymin=c(29) , ymax=c(33), alpha=0.2, color="blue", fill="blue")

#scale_x_continuous(limits=c(0,2),breaks = seq(0, 2, by = 1))+
#theme(axis.title=element_text(size=22), axis.text=element_text(size=16))+ 
# geom_hline(yintercept=29, color="red")

level_graph_2<- sea_level_all%>% ggplot() +
        geom_point(aes(x = age, y = level), size = 1, alpha = 0.75) + 
        xlab("Age (ka)") +
        ylab("Sea level (m)")+
        theme_bw(base_size = 20) +
        scale_y_continuous(limits=c(-150,2),breaks = seq(-150, 2, by = 25))+
        theme(axis.title=element_text(face="bold"))
level_graph_2

png(
        "other/Hydro/figure 7.png", 
        width = 8, 
        height = 8, 
        res = 300,
        units = "in"
)


print(level_graph_2)

dev.off()

together_all<- together_1%>%
        select(median, NorFe, NorSi, PrC, NorAl, Si_Ti)%>%
        mutate(NorFe_roll=rollmean(NorFe,k=10, fill = TRUE))%>%
        mutate(NorSi_roll=rollmean(NorSi,k=10, fill = TRUE))%>%
        mutate(NorPrC_roll=rollmean(PrC,k=10, fill = TRUE))%>%
        mutate(NorAl_roll=rollmean(NorAl,k=10, fill = TRUE))%>%
        mutate(NorSi_Ti_roll=rollmean(Si_Ti,k=10, fill = TRUE))%>%
        filter(median>4600 & median<32000)%>%
        left_join(organic4)%>%
        select(median,NorSi_Ti_roll, NorMoroll)%>%
        gather(Element, Value, -median)%>%
        drop_na()

together<- together_1%>%
        select(median, NorFe, NorSi, PrC, NorAl, Si_Ti)%>%
        mutate(NorFe_roll=rollmean(NorFe,k=10, fill = TRUE))%>%
        mutate(NorSi_roll=rollmean(NorSi,k=10, fill = TRUE))%>%
        mutate(NorPrC_roll=rollmean(PrC,k=10, fill = TRUE))%>%
        mutate(NorAl_roll=rollmean(NorAl,k=10, fill = TRUE))%>%
        mutate(NorSi_Ti_roll=rollmean(Si_Ti,k=10, fill = TRUE))%>%
        filter(median>4600 & median<32000)%>%
        left_join(organic3)%>%
        select(median,NorSi_Ti_roll, NorMoroll)%>%
        drop_na()

plot_ <- together_z_1 %>%
        select(median, Value,Element) %>%
        na.omit() %>%
        mutate(facet_label=fct_recode(
                Element,
                "Si:Ti"= "NorSi_Ti_roll",
          
                "Inc/Coh" = "NorMoroll"
        ))%>%
        ggplot(aes(y = median/1000, x = Value)) +
        geom_point()+
        geom_path()+
        xlab("") +
        theme_bw(base_size = 20) +
        facet_wrap(~facet_label,scales = "free_x", labeller=label_parsed, ncol=5)+
        scale_y_reverse(limits=c(33,4),breaks = seq(4, 33, by = 2)) +
        labs(x = "Normalized ratio", y = "Age (ka)")

plot_




plot2 <- together %>%
        na.omit() %>%
        ggplot(aes(y = median/1000, x = NorSi_Ti_roll)) +
        geom_point()+
        geom_path()+
        xlab("") +
        xlab(expression(atop("Si:Ti", paste ("(mm/yr)")))) +
        ylab("Age (ka)")+
        theme_minimal(base_size = 20) +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        scale_x_continuous(limits=c(0,1),breaks = seq(0, 1, by = 0.5))+
        theme(axis.title.x=element_text(size=14),axis.text.x=element_text(size=12),axis.title.y=element_text(size=14))+
        theme(
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank())

plot3 <- together %>%
        na.omit() %>%
        ggplot(aes(y = median/1000, x = NorMoroll)) +
        geom_point()+
        geom_path()+
        xlab("") +
        xlab(expression(atop("Inc:Coh", paste ("")))) +
        ylab("Age (ka)")+
        theme_minimal(base_size = 20) +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        scale_x_continuous(limits=c(4,7),breaks = seq(4, 7, by = 1))+
        theme(axis.title.x=element_text(size=14),axis.text.x=element_text(size=12),
              axis.title.y = element_blank(), axis.text.y = element_blank())+
        theme(
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank())

plot4 <- together %>%
        na.omit() %>%
        ggplot(aes(y = median/1000, x = NorSi_Ti_roll)) +
        geom_point()+
        geom_path()+
        xlab("") +
        xlab(expression(atop("Si:Ti", paste ("")))) +
        ylab("Age (ka)")+
        theme_minimal(base_size = 20) +
        scale_y_reverse(limits=c(33,0),breaks = seq(0, 33, by = 2))+
        scale_x_continuous(limits=c(0,1),breaks = seq(0, 1, by = 0.5))+
        theme(axis.title.x=element_text(size=14),axis.text.x=element_text(size=12),
              axis.title.y = element_blank(), axis.text.y = element_blank())+
        theme(
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank())

range(together$NorSi_Ti_roll)
grid.arrange(a, plot3,ncol=2)
a <- grid.arrange(plot_sed_time_2,plot_05_time,plot_frac,ncol=3)


png(
        "other/Hydro/Figure 4_2.png", 
        width = 12, 
        height = 8, 
        res = 300,
        units = "in"
)


grid.arrange(plot_sed_time_2,plot_05_time,plot_frac,ncol=3)

dev.off()




geo<- together_1%>%
        select(median, NorFe, NorSi, NorAl, Si_Ti, NorTi, Si_Al)%>%
        mutate(NorFe_roll=rollmean(NorFe,k=10, fill = TRUE))%>%
        mutate(NorSi_roll=rollmean(NorSi,k=10, fill = TRUE))%>%
        mutate(NorAl_roll=rollmean(NorAl,k=10, fill = TRUE))%>%
        mutate(NorTi_roll=rollmean(NorTi,k=10, fill = TRUE))%>%
        mutate(NorSi_Ti_roll=rollmean(Si_Ti,k=10, fill = TRUE))%>%
        mutate(NorSi_Al_roll=rollmean(NorSi/NorAl,k=5, fill = TRUE))%>%
        mutate(NorSi_Ti= NorSi/NorTi)%>%
       # mutate(NorSi_Al= NorSi_roll/NorAl_roll)%>%
       # mutate(NorSi_Al_roll= NorSi/NorAl)%>%
        filter(median>4600 & median<32000)%>%
      drop_na(NorSi_Al_roll)%>%
        left_join(organic4)%>%
        select(median, NorAl, NorFe, NorSi, NorTi, NorSi_Ti, NorMoroll, NorSi_Al_roll)%>%
        gather(Element, Value, -median)#%>%
       # drop_na()
pp<- geo%>%
        select(median, Value,Element) %>%
        na.omit() %>%
        mutate(facet_label=fct_recode(
                Element,
                "Al" = 'NorAl',
                "Fe" = "NorFe",
                "Si" = "NorSi",
                "Ti" = "NorTi",
                "Si:Ti"= "NorSi_Ti",
                "Inc/Coh" = "NorMoroll",
                "Si:Al" = "NorSi_Al_roll"
        ))%>%
        ggplot(aes(y = median/1000, x = Value)) +
        geom_point()+
     #   geom_path()+
        xlab("") +
        theme_bw(base_size = 18) +
        facet_wrap(~facet_label,scales = "free_x", labeller=label_parsed, ncol=7)+
        scale_y_reverse(limits=c(32,4),breaks = seq(4, 32, by = 2)) +
        labs(x = "Normalized ratio", y = "Age (ka)")
pp2<-pp+scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) + 
        geom_hline(yintercept=c(9.7, 18.2,29.1))
#write.csv(organic3, here("analysis", "organic3.csv"))

png(
        "other/Hydro/Figure 6_f.png", 
        width = 12, 
        height = 8, 
        res = 300,
        units = "in"
)


print(pp2)

dev.off()



all<- read.csv(here("together.csv"))



together<- all2%>%
        select(median, NorFe, NorSi, NorAl)%>%
        mutate(NorFe_roll=rollmean(NorFe,k=10, fill = TRUE))%>%
        mutate(NorSi_roll=rollmean(NorSi,k=10, fill = TRUE))%>%
        #mutate(NorPrC_roll=rollmean(PrC,k=10, fill = TRUE))%>%
        mutate(NorAl_roll=rollmean(NorAl,k=10, fill = TRUE))%>%
        #mutate(NorSi_Ti_roll=rollmean(Si_Ti,k=10, fill = TRUE))%>%
        filter(median>4600 & median<32000)%>%
        select(median,NorSi)%>%
        gather(Element, Value, -median)%>%
        drop_na()
pp<- together%>%
        select(median, Value,Element) %>%
        na.omit() %>%
        mutate(facet_label=fct_recode(
                Element,
                 "Fe" = "NorSi"

        ))%>%
        ggplot(aes(y = median/1000, x = Value)) +
        geom_point()+
        #geom_path()+
        xlab("") +
        theme_bw(base_size = 15) +
        facet_wrap(~facet_label,scales = "fixed", labeller=label_parsed, ncol=6)+
        scale_y_reverse(limits=c(33,4),breaks = seq(4, 33, by = 2)) +  scale_x_continuous(limits=c(0,1))+
        labs(x = "Normalized ratio", y = "Age (ka)")
pp


all2<-read.csv(here("san_final_model10.csv"))

fff<-fr%>%
        spread(Fraction, Percentage)


geo22<- together_1%>%
        select(median, NorFe, NorSi, NorAl, Si_Ti, NorTi, Si_Al)%>%
        mutate(NorFe_roll=rollmean(NorFe,k=10, fill = TRUE))%>%
        mutate(NorSi_roll=rollmean(NorSi,k=10, fill = TRUE))%>%
        mutate(NorAl_roll=rollmean(NorAl,k=10, fill = TRUE))%>%
        mutate(NorTi_roll=rollmean(NorTi,k=10, fill = TRUE))%>%
        mutate(NorSi_Ti_roll=rollmean(Si_Ti,k=10, fill = TRUE))%>%
        mutate(NorSi_Al_roll=rollmean((NorSi/NorAl)/10,k=5, fill = TRUE))%>%
        mutate(NorSi_Ti= NorSi/NorTi)%>%
        # mutate(NorSi_Al= NorSi_roll/NorAl_roll)%>%
        # mutate(NorSi_Al_roll= NorSi/NorAl)%>%
        filter(median>4600 & median<32000)%>%
        drop_na(NorSi_Al_roll)%>%
        left_join(organic4)%>%
        select(median, NorAl_roll, NorFe_roll, NorSi_roll, NorTi_roll, NorSi_Ti_roll, NorMoroll, NorSi_Al_roll)%>%
        gather(Element, Value, -median)#%>%
# drop_na()
pp22<- geo22%>%
        select(median, Value,Element) %>%
        na.omit() %>%
        mutate(facet_label=fct_recode(
                Element,
                "Al" = 'NorAl_roll',
                "Fe" = "NorFe_roll",
                "Si" = "NorSi_roll",
                "Ti" = "NorTi_roll",
                "Si:Ti"= "NorSi_Ti_roll",
                "Inc/Coh" = "NorMoroll",
                "Si:Al" = "NorSi_Al_roll"
        ))%>%
        ggplot(aes(y = median/1000, x = Value)) +
        geom_point()+
        #   geom_path()+
        xlab("") +
        theme_bw(base_size = 18) +
        facet_wrap(~facet_label,scales = "free_x", labeller=label_parsed, ncol=7)+
        scale_y_reverse(limits=c(32,4),breaks = seq(4, 32, by = 2)) +
        labs(x = "Normalized ratio", y = "Age (ka)")
pp222<-pp22+scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) + 
        geom_hline(yintercept=c(9.7, 18.2,29.1))
pp222

png(
        "other/Hydro/figure 6.png", 
        width = 14, 
        height = 8, 
        res = 300,
        units = "in"
)


print(pp222)

dev.off()
