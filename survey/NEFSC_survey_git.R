# summarize survey data
# liz brooks
# january 2021
# last update: feb 2021
# Notes:  look for notes by searching " !!NOTE:  "



rm(list=ls(all=TRUE))      # Remove all variables, etc. from the session memory
graphics.off()  # close any open graphics windows


library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library(ggridges)
library(readr)
library(stringr)
library(gridExtra)

#=====================================================

fd <- "C:/Users/liz.brooks/liz/Docs/haddock/2021_benchmark/TOR3_Surveys/saga/"  #input csv files should be here
od <- "C:/Users/liz.brooks/liz/Docs/haddock/2021_benchmark/TOR3_Surveys/saga/plots/"   #all output will be saved here

stock <- "GB"   #the stock i'm interested in plotting

#specify landing, length, and age files here:
 # the tow.station files are from survan *_tow.DAT
f.fall.tow.station <- paste0(fd,"GBfall.csv")
f.spring.tow.station <- paste0(fd, "GBspring.csv")

# get ADIOS stratified means

gb.mean <- as_tibble(read.csv("C:/Users/liz.brooks/liz/Docs/haddock/2021_benchmark/TOR3_Surveys/ADIOS/GB_mean.csv", header=T) )
gb.fall.b <- gb.mean %>%
  filter(SEASON=="FALL", INDEX_TYPE=="Biomass (kg/tow)") %>%
  mutate(Year=YEAR) %>%
 select(Year, INDEX)  


gb.spring.b <- gb.mean %>%
  filter(SEASON=="SPRING", INDEX_TYPE=="Biomass (kg/tow)") %>%
  mutate(Year=YEAR) %>%
  select(Year, INDEX)  

disc.ci <- disc.ci %>%
  mutate( ci_lower_trunc = case_when(
    ci_lower>=0 ~ci_lower,
    ci_lower< 0 ~0
  ))

index.annual.plot <- ggplot(gb.mean, aes(x=YEAR, y=INDEX)) + 
  facet_grid(INDEX_TYPE ~ SEASON, scales = "free_y") +
  geom_line(aes(y=INDEX), colour="blue") + 
  geom_ribbon(aes(ymin=LOWER_90_CI, ymax=UPPER_90_CI), fill="blue", alpha=0.15)+
  ylab(label="NEFSC Index") 


#=====================================================
#=====================================================


####  --- fall length data ====

fall.len <- as.data.frame(read.csv(f.fall.tow.station, header=T))
spring.len <- as.data.frame(read.csv(f.spring.tow.station, header=T))

fall <- as_tibble(fall.len) %>%
  select(Year, Strata, Tow, Area, Temp, Depth, catch_number, catch_weight, starts_with("Bin"))

fall.index <- fall %>%
  select(Year, Strata) %>%
  left_join(gb.fall.b) %>%
  mutate(Length=93) %>%
  mutate(INDEX=round(INDEX,0)) %>%
  mutate(INDEX = ifelse(Strata==1130, INDEX, " " )) %>%
  mutate(Strata = factor(Strata))

bin.cols <- which(substr(colnames(fall), 1,3) =="Bin")
len.min <- min(substr(colnames(fall[bin.cols]) , 5,8) )
len.max <- max(substr(colnames(fall[bin.cols]) , 5,8) )    

fall2 <- fall %>% 
  pivot_longer(cols = starts_with("Bin"), names_to="Length", values_to="count", values_drop_na = TRUE) %>%
  mutate(Length=as.numeric(substr(Length, 5,8))) %>%
  mutate(Lengths = round(count,0)) %>%
  mutate(Temp.bin = ceiling(Temp)) %>%
  mutate(Depth.bin = 25*ceiling(Depth/25))

fall3 <- fall2 %>%
  filter(catch_number>0) %>%
  mutate(Year=as.factor(Year), Strata=as.factor(Strata)) %>%
  uncount(Lengths) 

fall.tow.num.str <- fall %>%
  group_by(Year, Strata, Tow) %>%
  select(Year, Strata, Tow, catch_number) %>%
  mutate(Year = factor(Year))

fall.ntows <- fall.tow.num.str %>%
  add_count(Tow) %>%
  group_by(Year) %>%
  mutate(pos = ifelse(catch_number>0, 1, 0)) %>%
  summarise(Ntow.yr = sum(n), 
            prop.pos = sum(pos)/Ntow.yr) %>%
  mutate(Season="FALL")

fall.tow.hist.63to90 <- ggplot(fall.tow.num.str[fall.tow.num.str$Year %in% seq(1963, 1990),], aes(x = catch_number )    ) +   
  facet_wrap(~Year, scales = "free")+
  geom_histogram(fill='#6633BB') +
  scale_fill_manual(values='#6633BB') +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=9)) + 
  ggtitle("Fall Histogram of Numbers per Tow") 

fall.tow.hist.91to19 <- ggplot(fall.tow.num.str[fall.tow.num.str$Year %in% seq(1991, 2019),], aes(x = catch_number )    ) +   
  facet_wrap(~Year, scales = "free")+
  geom_histogram(fill='#6633BB') +
  scale_fill_manual(values='#6633BB') +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=9)) + 
  ggtitle("Fall Histogram of Numbers per Tow") 


 fall.strata.counts <- fall3 %>%
   group_by(Year, Strata)  %>%
   summarize( str.sum=sum(count, na.rm=T)) %>%
   mutate(str.sum = round(str.sum, 0)) %>%
   mutate(percentage=str.sum/sum(str.sum)*100) %>%
   mutate(pct.txt = ifelse(percentage<0.5, '*', ' '))
 
 
 fall.strata.mean.DT <- fall3 %>%
   group_by(Strata)  %>%
   mutate(Temp = ifelse(Temp==-999, NA, Temp)) %>%
   summarize( str.mean.depth=mean(Depth, na.rm=T), str.mean.temp = mean(Temp, na.rm=T)) 
 
 
 
 fall4 <- fall3 %>%
   left_join(fall.strata.counts) %>%
   mutate(Depth.bin = factor(Depth.bin), Temp.bin=factor(Temp.bin))
 
 
 n.strata <- length(c(seq(1130, 1250, by=10), 1290, 1300))
 strata.color.set <- matrix(NA, nrow=1, ncol=n.strata)
                             #13         14         15         16          17        18         19          20
strata.color.set[1,] <- c("#55DDDD", "#5599DD", "#1111DD", "#FF99AA" , "#FF1188", "#DD0055", "#CCCCEE", "#CCaaCC",
                     # 21        22          23         24        25          29         30
                   "#BB77DD", "#AA00BB", "#55BB00", "#117733", "#99DD00", "#551199", "#000088")
colnames(strata.color.set) <- c(seq(1130, 1250, by=10), 1290, 1300)


tmp.strata <- as.vector(sort(unique(fall3$Strata[fall3$Year %in% seq(1963, 1990)]) ))
match.strata <- which(colnames(strata.color.set) %in% tmp.strata)
strata.color <- strata.color.set[1, match.strata]

fall.yr.strata.63to90 <- ggplot(fall3[fall3$Year %in% seq(1963, 1990),], aes(x = Length, y=Strata,   fill=Strata) ) +   #after_stat(density),
  facet_wrap(~Year)+
  geom_density_ridges(bandwidth = 3) +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(y="Stratum")+ 
  ggtitle("Fall Length Frequency by stratum")  +
  scale_fill_manual(values=strata.color) +
  geom_text(data=fall.strata.counts[fall.strata.counts$Year %in% seq(1963, 1990),], aes(y=Strata, x=rep(80,dim(fall.strata.counts[fall.strata.counts$Year %in% seq(1963, 1990),])[[1]]), label=pct.txt), size=3.5, position=position_nudge(y= .25) ) +
  geom_text( data=fall.index[fall.index$Year %in% seq(1963, 1990),] , aes(x=Length, y=Strata, label=INDEX),  size=3.5 , inherit.aes = FALSE, position=position_nudge(y= .4), fontface="plain" ) +
  labs(caption = str_wrap("Figure : Annual Kg/tow is in bottom right of each facet panel.  Strata with <0.5% of total numbers in a given year are indicated by * in each facet panel.")) +
  theme(plot.caption = element_text(hjust = 0)) +
  guides(fill=guide_legend(reverse=T))

tmp.strata <- as.vector(sort(unique(fall3$Strata[fall3$Year %in% seq(1991, 2019)]) ))
match.strata <- which(colnames(strata.color.set) %in% tmp.strata)
strata.color <- strata.color.set[1, match.strata]

fall.yr.strata.91to19 <- ggplot(fall3[fall3$Year %in% seq(1991, 2019),], aes(x = Length, y=Strata,   fill=Strata) ) +  
  geom_text(data=fall.strata.counts[fall.strata.counts$Year %in% seq(1991, 2019),], aes(y=Strata, x=rep(80,dim(fall.strata.counts[fall.strata.counts$Year %in% seq(1991, 2019),])[[1]]), label=pct.txt), size=3.5, position=position_nudge(y= .25) ) +
  geom_text( data=fall.index[fall.index$Year %in% seq(1991, 2019),] , aes(x=Length, y=Strata, label=INDEX),  size=3.5 , inherit.aes = FALSE, position=position_nudge(y= .4), fontface="plain" ) +
  geom_density_ridges(bandwidth = 3)+
  facet_wrap(~Year) +
  theme_ridges() + 
  xlim(0,110) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(y="Stratum")+ 
  ggtitle("Fall Length Frequency by stratum")  +
  scale_fill_manual(values=strata.color) +
  labs(caption = str_wrap("Figure : Annual Kg/tow is in bottom right of each facet panel.  Strata with <0.5% of total numbers in a given year are indicated by * in each facet panel.")) +
  theme(plot.caption = element_text(hjust = 0)) +
  guides(fill=guide_legend(reverse=T))




  
  
ndepth <- length(sort(unique(fall4$Depth.bin))  )
depth.color <- colorRampPalette(c("#99FFFF", '#0000AA')) (ndepth)
fall.depth <- ggplot(fall4, aes(x = Length, y=Depth.bin,   fill=Depth.bin) ) +   #after_stat(density),
  facet_wrap(~Year)+
  geom_density_ridges(bandwidth = 3) +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(y="Depth")+ 
  ggtitle("Fall Length Frequency by Depth Bin")+
  scale_fill_manual(values=depth.color)+
  labs(caption = str_wrap("Figure : In broad terms, smaller fish tend to be found at shallower depths, while larger fish (when present) tend to be in deeper waters.")) +
  theme(plot.caption = element_text(hjust = 0)) +
  guides(fill=guide_legend(reverse=T))

ntemp <- length(sort(unique(fall4$Temp.bin)) [-1] )
temp.color <- colorRampPalette(c("blue", 'red')) (ntemp)
fall.temp <- ggplot(fall4[fall4$Temp.bin %in% seq(0,25),], aes(x = Length, y=Temp.bin,   fill=Temp.bin) ) +   #after_stat(density),
  facet_wrap(~Year)+
  geom_density_ridges(bandwidth = 3) +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(y="Temperature")+ 
  ggtitle("Fall Length Frequency by Temperature Bin (Celcius)")+
  scale_fill_manual(values=temp.color) +
  labs(caption = str_wrap("Figure : In broad terms, smaller fish tend to be found at shallower depths, while larger fish (when present) tend to be in deeper waters.")) +
  theme(plot.caption = element_text(hjust = 0)) +
  guides(fill=guide_legend(reverse=T))




####  --- spring length data ====

spring.len <- as.data.frame(read.csv(f.spring.tow.station, header=T))

spring <- as_tibble(spring.len) %>%
  select(Year, Strata, Tow, Area, Temp, Depth, catch_number, catch_weight, starts_with("Bin"))

bin.cols <- which(substr(colnames(spring), 1,3) =="Bin")
len.min <- min(substr(colnames(spring[bin.cols]) , 5,8) )
len.max <- max(substr(colnames(spring[bin.cols]) , 5,8) )  

spring.index <- spring %>%
  select(Year, Strata) %>%
  left_join(gb.spring.b) %>%
  mutate(Length=93) %>%
  mutate(INDEX=round(INDEX,0)) %>%
  mutate(INDEX = ifelse(Strata==1130, INDEX, " " )) %>%
  mutate(Strata = factor(Strata))

spring2 <- spring %>% 
  pivot_longer(cols = starts_with("Bin"), names_to="Length", values_to="count", values_drop_na = TRUE) %>%
  mutate(Length=as.numeric(substr(Length, 5,8))) %>%
  mutate(Lengths = round(count,0)) %>%
  mutate(Temp.bin = ceiling(Temp)) %>%
  mutate(Depth.bin = 25*ceiling(Depth/25))

spring3 <- spring2 %>%
  filter(catch_number>0) %>%
  mutate(Year=as.factor(Year), Strata=as.factor(Strata)) %>%
  uncount(Lengths)

spring.tow.num.str <- spring %>%
  group_by(Year, Strata, Tow) %>%
  select(Year, Strata, Tow, catch_number) %>%
  mutate(Year = factor(Year))

spring.ntows <- spring.tow.num.str %>%
  add_count(Tow) %>%
  group_by(Year) %>%
  mutate(pos = ifelse(catch_number>0, 1, 0)) %>%
  summarise(Ntow.yr = sum(n), 
            prop.pos = sum(pos)/Ntow.yr) %>%
  mutate(Season = "SPRING")

spring.tow.hist.68to90 <- ggplot(spring.tow.num.str[spring.tow.num.str$Year %in% seq(1968, 1990),], aes(x = catch_number )    ) +   
  facet_wrap(~Year, scales = "free")+
  geom_histogram(fill='#6633BB') +
  scale_fill_manual(values='#6633BB') +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=9)) + 
  ggtitle("Spring Histogram of Numbers per Tow") 

spring.tow.hist.91to19 <- ggplot(spring.tow.num.str[spring.tow.num.str$Year %in% seq(1991, 2019),], aes(x = catch_number )    ) +   
  facet_wrap(~Year, scales = "free")+
  geom_histogram(fill='#6633BB') +
  scale_fill_manual(values='#6633BB') +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=9)) + 
  ggtitle("Spring Histogram of Numbers per Tow") 

spring.strata.counts <- spring3 %>%
  group_by(Year, Strata)  %>%
  summarize( str.sum=sum(count, na.rm=T)) %>%
  mutate(str.sum = round(str.sum, 0)) %>%
  mutate(percentage=str.sum/sum(str.sum)*100) %>%
  mutate(pct.txt = ifelse(percentage<0.5, '*', ' '))


spring.strata.mean.DT <- spring3 %>%
  group_by(Strata)  %>%
  mutate(Temp = ifelse(Temp==-999, NA, Temp)) %>%
  summarize( str.mean.depth=mean(Depth, na.rm=T), str.mean.temp = mean(Temp, na.rm=T)) 



spring4 <- spring3 %>%
  left_join(spring.strata.counts) %>%
  mutate(Depth.bin = factor(Depth.bin), Temp.bin=factor(Temp.bin))




n.strata <- length(c(seq(1130, 1250, by=10), 1290, 1300))
strata.color.set <- matrix(NA, nrow=1, ncol=n.strata)
#13         14         15         16          17        18         19          20
strata.color.set[1,] <- c("#55DDDD", "#5599DD", "#1111DD", "#FF99AA" , "#FF1188", "#DD0055", "#CCCCEE", "#CCaaCC",
                          # 21        22          23         24        25          29         30
                          "#BB77DD", "#AA00BB", "#55BB00", "#117733", "#99DD00", "#551199", "#000088")
colnames(strata.color.set) <- c(seq(1130, 1250, by=10), 1290, 1300)




tmp.strata <- as.vector(sort(unique(spring3$Strata[spring3$Year %in% seq(1968, 1990)]) ))
match.strata <- which(colnames(strata.color.set) %in% tmp.strata)
strata.color <- strata.color.set[1, match.strata]

spring.yr.strata.68to90 <- ggplot(spring3[spring3$Year %in% seq(1968, 1990),], aes(x = Length, y=Strata,   fill=Strata) ) +   
  facet_wrap(~Year)+
  geom_density_ridges(bandwidth = 3) +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(y="Stratum")+ 
  ggtitle("Spring Length Frequency by stratum")  +
  scale_fill_manual(values=strata.color) +
  geom_text(data=spring.strata.counts[spring.strata.counts$Year %in% seq(1968, 1990),], aes(y=Strata, x=rep(80,dim(spring.strata.counts[spring.strata.counts$Year %in% seq(1968, 1990),])[[1]]), label=pct.txt), size=3.5, position=position_nudge(y= .25) ) +
  geom_text( data=spring.index[spring.index$Year %in% seq(1968, 1990),] , aes(x=Length, y=Strata, label=INDEX),  size=4 , inherit.aes = FALSE, position=position_nudge(y= .4), fontface="plain" ) +
  labs(caption = str_wrap("Figure : Annual Kg/tow is in bottom right of each facet panel.  Strata with <0.5% of total numbers in a given year are indicated by * in each facet panel.")) +
  theme(plot.caption = element_text(hjust = 0)) +
  guides(fill=guide_legend(reverse=T))


tmp.strata <- as.vector(sort(unique(spring3$Strata[spring3$Year %in% seq(1991, 2019)]) ))
match.strata <- which(colnames(strata.color.set) %in% tmp.strata)
strata.color <- strata.color.set[1, match.strata]

spring.yr.strata.91to19 <- ggplot(spring3[spring3$Year %in% seq(1991, 2019),], aes(x = Length, y=Strata,   fill=Strata) ) +   
  facet_wrap(~Year)+
  geom_density_ridges(bandwidth = 3) +
  theme_ridges() + 
  xlim(0,100) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(y="Stratum")+ 
  ggtitle("Spring Length Frequency by stratum")  +
  scale_fill_manual(values=strata.color) +
  geom_text(data=spring.strata.counts[spring.strata.counts$Year %in% seq(1991, 2019),], aes(y=Strata, x=rep(80,dim(spring.strata.counts[spring.strata.counts$Year %in% seq(1991, 2019),])[[1]]), label=pct.txt), size=3.5, position=position_nudge(y= .25) ) +
  geom_text( data=spring.index[spring.index$Year %in% seq(1991, 2019),] , aes(x=Length, y=Strata, label=INDEX),  size=4 , inherit.aes = FALSE, position=position_nudge(y= .4), fontface="plain" ) +
  labs(caption = str_wrap("Figure : Annual Kg/tow is in bottom right of each facet panel.  Strata with <0.5% of total numbers in a given year are indicated by * in each facet panel.")) +
  theme(plot.caption = element_text(hjust = 0)) +
  guides(fill=guide_legend(reverse=T))



ndepth <- length(sort(unique(spring4$Depth.bin))  )
depth.color <- colorRampPalette(c("#99FFFF", '#0000AA')) (ndepth)
spring.depth <- ggplot(spring4, aes(x = Length, y=Depth.bin,   fill=Depth.bin) ) +  
  facet_wrap(~Year)+
  geom_density_ridges(bandwidth = 3) +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(y="Depth")+ 
  ggtitle("Spring Length Frequency by Depth Bin")+
  scale_fill_manual(values=depth.color)+
  labs(caption = str_wrap("Figure : In broad terms, smaller fish tend to be found at shallower depths, while larger fish (when present) tend to be in deeper waters.")) +
  theme(plot.caption = element_text(hjust = 0)) +
  guides(fill=guide_legend(reverse=T))

ntemp <- length(sort(unique(spring4$Temp.bin)) [-1] )
temp.color <- colorRampPalette(c("blue", 'red')) (ntemp)
spring.temp <- ggplot(spring4[spring4$Temp.bin %in% seq(0,25),], aes(x = Length, y=Temp.bin,   fill=Temp.bin) ) +   
  facet_wrap(~Year)+
  geom_density_ridges(bandwidth = 3) +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(y="Temperature")+ 
  ggtitle("Spring Length Frequency by Temperature Bin (Celcius)")+
  scale_fill_manual(values=temp.color)+
  labs(caption = str_wrap("Figure : In broad terms, smaller fish tend to be found in warmer waters, while larger fish (when present) tend to be in cooler waters.")) +
  theme(plot.caption = element_text(hjust = 0)) +
  guides(fill=guide_legend(reverse=T))


fall.yr <- fall3 %>%
  group_by(Year, Length) %>%
  select(Year, Length) %>%
  mutate(Season='Fall') %>%
  ungroup()


spring.yr <- spring3 %>%
  group_by(Year, Length) %>%
  select(Year, Length) %>%
  mutate(Season='Spring') %>%
  ungroup()

spring.fall.yr <- fall.yr %>%
  add_row(spring.yr)

spring.fall <- spring.fall.yr %>%
  group_by(Length ) %>%
  select(Length, Season) %>%
  ungroup()
  
spring.fall.lenfreq.yr <- ggplot(spring.fall.yr, aes(x = Length, y=Season,   fill=Season) ) +   
  facet_wrap(~Year)+
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(y="Season")+ 
  ggtitle("Spring vs Fall Length Frequency")+
  scale_fill_manual(values=c( "#CC663344","#22CC6644"))  +
  guides(fill=guide_legend(reverse=T))
  


spring.fall.lenfreq <- ggplot(spring.fall, aes(x = Length, y=Season,   fill=Season) ) +   
  geom_density_ridges() +
  stat_density_ridges(quantile_lines = TRUE)+
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(y="Season")+ 
  ggtitle("Spring vs Fall Length Frequency")+
  scale_fill_manual(values=c( "#CC663344","#22CC6644"))   +
  guides(fill=guide_legend(reverse=T))


spring.fall.ntows <- fall.ntows %>%
  add_row(spring.ntows)

Ntow.plot <- ggplot(spring.fall.ntows, aes(x=Year, y=Ntow.yr)) + 
  facet_wrap( ~ Season) +
  geom_point( colour="#5566FF") + 
   ylab(label="Number of Tows") +
  ylim(0, max(spring.fall.ntows$Ntow.yr)) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=9),
        axis.text.y  = element_text(size=9))+
  scale_x_discrete(breaks = seq(1963, 2019, by=2))

Prop.pos.plot <- ggplot(spring.fall.ntows, aes(x=Year, y=prop.pos)) + 
  facet_wrap( ~ Season) +
  geom_point( colour="#992266") + 
  ylab(label="Proportion of Positive Tows")+
  ylim(0,1) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=9),
        axis.text.y  = element_text(size=9)) +
  scale_x_discrete(breaks = seq(1963, 2019, by=2))
       
     
 Tow.prop.pos.plot <- grid.arrange(Ntow.plot, Prop.pos.plot, #heights = c(4, 4),, align = "v"
                                ncol = 1, nrow = 2)      
   ggsave(Tow.prop.pos.plot, file=paste0(od, "GBHaddock_Tow.prop.pos.plot.pdf"), width=8, height=10.5, pointsize=10)

pdf(file=paste0(od, "GBHaddock_TOR3_Survey_plots.pdf"), width=8, height=10.5, pointsize=10)
index.annual.plot
fall.tow.hist.63to90
fall.tow.hist.91to19
fall.yr.strata.63to90
fall.yr.strata.91to19 
fall.depth
fall.temp
spring.tow.hist.68to90
spring.tow.hist.91to19
spring.yr.strata.68to90
spring.yr.strata.91to19
spring.depth
spring.temp
spring.fall.lenfreq.yr
spring.fall.lenfreq
dev.off()



