# summarize landings, lengths, ages
# liz brooks
# january 2020
# last update:
# Notes:  look for notes by searching " !!NOTE:  "

library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library(ggridges)
library(hrbrthemes)

#=====================================================

fd <- "C:/Users/liz.brooks/liz/Docs/haddock/2021_benchmark/TOR2_Catch/SAS/output/"  #input csv files should be here
od <- "C:/Users/liz.brooks/liz/Docs/haddock/2021_benchmark/TOR2_Catch/plots/"   #all output will be saved here

stock <- "GB"   #the stock i'm interested in plotting

#specify landing, length, and age files here:
f.cat.land <- paste0(fd,"comland_summary_147_1964_2019.csv")
f.cat.len <- paste0(fd, "comlen_summary_147_1969_2019.csv")
f.cat.age <- paste0(fd, "comage_summary_147_1965_2019.csv")



#=====================================================
#=====================================================

# # color vector for NEGEAR !!NOTE: not sure best way to handle this globally, there are so many NEGEAR types;
# #    for now, i'm just assigning separately within length, age, and landings sections
# gear.mat <- matrix(NA, nrow=9, ncol=10)
# rownames(gear.mat) <- seq(10,90, by=10)
# colnames(gear.mat) <- seq(0,9)
# gear.mat[1,] <- c( )  # blacks
# gear.mat[2,] <- c( )  # greens
# gear.mat[3,] <- c( )  # purples
# gear.mat[4,] <- c( )  # oranges
# gear.mat[5,] <- c( )  # blues
# gear.mat[6,] <- c( )  # reds
# gear.mat[7,] <- c( )  #  
# gear.mat[8] <- c( )  #  
# gear.mat[9,] <- c( )  #  



# generic color matrix for commercial statistical area  
# i think i have assigned colors to all named stat areas but could use a double check
# colors come from colorbrewer:  https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3

sa.mat <- matrix(NA, nrow=15, ncol=10)
rownames(sa.mat) <- c(40, 45, 46, 
                       50, 51, 52, 53, 54, 55, 56, 
                       60, 61, 62, 63, 64)
colnames(sa.mat) <- seq(0,9)
sa.mat[,1] <- c('grey45', rep('grey75',2), 'grey35', rep('grey65',6), 'grey25', rep('grey55',4))  #40
sa.mat[2,2:10] <- c('#fcfbfd','#efedf5','#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')  # 45* purple
sa.mat[3,2:10] <- c('#fcfbfd','#efedf5','#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')  # 46* purple
#sa.mat[4,2:10] <- (keep as NA) #50
sa.mat[5,2:6] <- c('#f1eef6','#d7b5d8','#df65b0','#dd1c77','#980043')  # 51* magenta  [11-15]
sa.mat[6,2:7] <- c('#edf8e9','#c7e9c0','#a1d99b','#74c476','#31a354','#006d2c')  # 52* green  [21-26]
sa.mat[7,c(4:5, 8:10)] <- c('#fee391','#fec44f','#fe9929','#d95f0e','#993404')  # 53* yellow/brown [33-34; 37-39]
sa.mat[8,2:4] <- c('#e0f3db','#a8ddb5','#43a2ca')  # 54* green/blue [41-43]
sa.mat[9,2:3] <- c('#9ecae1','#3182bd')  # 55* blue [61-62]
sa.mat[10,2:3] <- c('#9ecae1','#3182bd')  # 56* blue [61-62]
#sa.mat[11,] <0 (keep as NA)  #60
sa.mat[12,2:7] <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#de2d26','#a50f15')  # 61* red [11-16]
sa.mat[13,2:10] <- c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')  # 62* red [21-29]
sa.mat[14,2:10] <- c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')  # 63* red [31-39]
sa.mat[15,2:10] <- rev(c('#ffffff','#f0f0f0','#d9d9d9','#bdbdbd','#969696','#737373','#525252','#252525','#000000') )  # 64* black [41-49]


# function to extract col.mat cells corresponding to unique SA in data ====
# using this function ensures that the same color is always associated with the same SA across all plots
assign.col <- function(x, x.mat) {
  #x is the factor
  #x.mat is the color matrix
  
  tmp.color <- c()
  for (c in 1:length(x)){
    
    tmp.2 <- substr(x[c], 1,2)
    tmp.row <- which(rownames(x.mat)==(tmp.2))
    tmp.3 <- substr(x[c], 3,3)
    tmp.col <- which(colnames(x.mat)==(tmp.3))
    tmp.color[c] <- x.mat[tmp.row, tmp.col]
      }#end loop over c 
    return(tmp.color)
    
} #end function assign.col

#=====================================================
#=====================================================



####  --- length data ====

cat.len <- as.data.frame(read.csv(f.cat.len, header=T))

#subset for user-specified stock only
my.len <- cat.len[cat.len$STOCK==stock, ]

#reassign stat areas that were renamed after hague line
my.len$SA[which(my.len$SA==523)] <- 561
my.len$SA[which(my.len$SA==524)] <- 562
# make SA and NEGEAR and MK factors
my.len$SA <- as.factor(my.len$SA)
my.len$NEGEAR <- as.factor(my.len$NEGEAR)
my.len$MK <- as.factor(my.len$MK)

############my.len.sa <- unique(my.len$SA)

my.tibble <- as_tibble(my.len)
my.tibble.len <- my.tibble
my.tibble <- my.tibble %>%
  mutate(PORT_NAME2 = substr(PORT_NAME,1,7))

ports.sampled.length <- unique(my.tibble$PORT_NAME2)

#expand rows based on LENGTHS (so each LENGTH will have nrows=LENGTHS)
my.tibble.long <- my.tibble %>%
  uncount(LENGTHS)

#subset for quarterly summaries
my.q1 <- filter(my.tibble.long, QTR==1)
my.q2 <- filter(my.tibble.long, QTR==2)
my.q3 <- filter(my.tibble.long, QTR==3)
my.q4 <- filter(my.tibble.long, QTR==4)


 # Summarize Length Sampling Numbers  ====

 # ---- by statistical area ----
 len.col <- assign.col( x=sort(unique(my.tibble.long$SA)),  x.mat=sa.mat )
comlen.annual.n.sa <- ggplot(my.tibble.long, aes(YEAR, fill=SA)) +
  geom_histogram(binwidth=1)+
  ggtitle("US Annual Length Samples - All Gears")+
  scale_fill_manual(values=len.col)

comlen.annual.prop.sa <- ggplot(my.tibble.long, aes(YEAR, fill=SA)) +
  geom_histogram(binwidth=1, position="fill")+
  ggtitle("US Annual Length Samples - All Gears")+
  scale_fill_manual(values=len.col)+
  labs(y = "Proportion")


# ---- by  market ----
#(4 unique for gbhaddock)
len.mk <- sort(unique(my.tibble$MK))
n.len.mk <- length(len.mk)
mk.col.mat <- matrix(NA, nrow=1, ncol=n.len.mk)
colnames(mk.col.mat) <- len.mk 
rownames(mk.col.mat) <- ""
mk.col.mat[1,] <- rev(c('#edb9da','#c994c7','#e7298a','#980043') ) # !!NOTE:  i manually made sure the number of colors=n.len.mk



mk.col <- mk.col.mat[1,which(colnames(mk.col.mat) ==  sort(unique(my.tibble.long$MK))  )]
comlen.annual.n.mk <- ggplot(my.tibble.long, aes(YEAR, fill=MK)) +
  geom_histogram(binwidth=1)+
  ggtitle("US Annual Length Samples By Market Category")+
  scale_fill_manual(values=mk.col)

comlen.annual.prop.mk <- ggplot(my.tibble.long, aes(YEAR, fill=MK)) +
  geom_histogram(binwidth=1, position="fill")+
  ggtitle("US Annual Length Samples By Market Category")+
  scale_fill_manual(values=mk.col)+
  labs(y = "Proportion")

# ---- by  gear ----
#(8 unique for gbhaddock)
len.gear <- sort(unique(my.tibble$NEGEAR))
n.len.gear <- length(len.gear)
gear.col.mat <- matrix(NA, nrow=1, ncol=n.len.gear)
colnames(gear.col.mat) <- len.gear  #c(as.character(len.gear))
rownames(gear.col.mat) <- ""
gear.col.mat[1,] <- rev(c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#0c2c84') ) # !!NOTE:  i manually made sure the number of colors=n.len.gear



  gear.col <- gear.col.mat[1,which(colnames(gear.col.mat) ==  sort(unique(my.tibble.long$NEGEAR))  )]
comlen.annual.n.gear <- ggplot(my.tibble.long, aes(YEAR, fill=NEGEAR)) +
  geom_histogram(binwidth=1)+
  ggtitle("US Annual Length Samples By Gear")+
 scale_fill_manual(values=gear.col)

comlen.annual.prop.gear <- ggplot(my.tibble.long, aes(YEAR, fill=NEGEAR)) +
  geom_histogram(binwidth=1, position="fill")+
  ggtitle("US Annual Length Samples By Gear")+
  scale_fill_manual(values=gear.col)+
  labs(y = "Proportion")



# ---- by  port  ----
# (12 unique for gb haddock)
len.port <- sort(unique(my.tibble$PORT_NAME2))
n.len.port <- length(len.port)
port.col.mat <- matrix(NA, nrow=1, ncol=n.len.port)
colnames(port.col.mat) <- len.port  #c(as.character(len.gear))
rownames(port.col.mat) <- ""
port.col.mat[1,] <- c(rev(c('#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#4a1486') ), rev( c('#fdd0a2','#fdae6b','#fd8d3c','#f16913','#d94801','#8c2d04')  )   ) # !!NOTE:  i manually made sure the number of colors=n.len.port

port.col <- port.col.mat[1,which(colnames(port.col.mat) ==  sort(unique(my.tibble.long$PORT_NAME2))  )]
comlen.annual.n.port <- ggplot(my.tibble.long, aes(YEAR, fill=PORT_NAME2)) +
  geom_histogram(binwidth=1)+
  labs(fill="Port")+
  ggtitle("US Annual Length Samples By Port")+
  scale_fill_manual(values=port.col)

comlen.annual.prop.port <- ggplot(my.tibble.long, aes(YEAR, fill=PORT_NAME2)) +
  geom_histogram(binwidth=1, position="fill")+
  labs(fill="Port")+  
  ggtitle("US Annual Length Samples By Port")+
  scale_fill_manual(values=port.col)+
  labs(y = "Proportion")






# Summarize Length Frequency  ====
# ---- by year ----
my.tibble.annual <- my.tibble.long %>%
  mutate(YEAR=factor(YEAR), 
         YEAR=factor(YEAR, levels=rev(levels(YEAR))))

comlen.annual <- ggplot(my.tibble.annual, aes(x = LENGTH, y=YEAR,  after_stat(density), fill=YEAR) ) +
  geom_density_ridges() +
  theme_ridges() + 
  #scale_y_discrete(limits=rev(levels(YEAR2)) )+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(y="Year")+ 
  ggtitle("US Annual LenFreq - All Gears")+
  theme(legend.position = "none")


# ---- by statistical area ----


len.col <- assign.col( x=sort(unique(my.tibble.long$SA)), x.mat=sa.mat  )
comlen.annual.sa <- ggplot(my.tibble.long, aes(x = LENGTH, y=SA,  after_stat(density), fill=SA) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=9))+
  ggtitle("US Annual LenFreq - All Gears")+
  scale_fill_manual(values=len.col)



  len.col <- assign.col( x=sort(unique(my.q1$SA)), x.mat=sa.mat   )
comlen.q1.sa <- ggplot(my.q1, aes(x = LENGTH, y=SA,  after_stat(density), fill=SA) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  #theme(legend.position = "none")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=9))+
  ggtitle("Q1 LenFreq - All Gears")+
  scale_fill_manual(values=len.col)

  len.col <- assign.col( x=sort(unique(my.q2$SA)) , x.mat=sa.mat  )
comlen.q2.sa <- ggplot(my.q2, aes(x = LENGTH, y=SA,  after_stat(density), fill=SA) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
 # theme(legend.position = "none")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=9))+
  ggtitle("Q2 LenFreq - All Gears")+
  scale_fill_manual(values=len.col)

  len.col <- assign.col( x=sort(unique(my.q3$SA)) , x.mat=sa.mat  )
comlen.q3.sa <- ggplot(my.q3, aes(x = LENGTH, y=SA,  after_stat(density), fill=SA) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  #theme(legend.position = "none")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=9))+
  ggtitle("Q3 LenFreq - All Gears")+
  scale_fill_manual(values=len.col)

len.col <- assign.col( x=sort(unique(my.q4$SA)), x.mat=sa.mat  )
comlen.q4.sa <- ggplot(my.q4, aes(x = LENGTH, y=SA,  after_stat(density), fill=SA) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  #theme(legend.position = "none")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=9))+
  ggtitle("Q4 LenFreq - All Gears")+
  scale_fill_manual(values=len.col)



# ---- by statistical market ----
mk.col <- mk.col.mat[1,which(colnames(mk.col.mat) ==  sort(unique(my.tibble.long$MK))  )]
comlen.annual.mk <- ggplot(my.tibble.long, aes(x = LENGTH, y=MK,  after_stat(density), fill=MK) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  #theme(legend.position = "none")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=9))+
  ggtitle("US Annual LenFreq By Market")+
  scale_fill_manual(values=mk.col)






# ---- by statistical gear ----

gear.col <- gear.col.mat[1,which(colnames(gear.col.mat) ==  sort(unique(my.tibble.long$NEGEAR))  )]
comlen.annual.gear <- ggplot(my.tibble.long, aes(x = LENGTH, y=NEGEAR,  after_stat(density), fill=NEGEAR) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=9))+
  ggtitle("US Annual LenFreq By Gear")+
  scale_fill_manual(values=gear.col)


# ---- by statistical port ----

port.col <- port.col.mat[1,which(colnames(port.col.mat) ==  sort(unique(my.tibble.long$PORT_NAME2))  )]
comlen.annual.port <- ggplot(my.tibble.long, aes(x = LENGTH, y=PORT_NAME2,  after_stat(density), fill=PORT_NAME2) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),
        axis.text.y  = element_text(size=6))+
  labs(fill="Port")+
  ggtitle("US Annual LenFreq By Port")+
  scale_fill_manual(values=port.col)+
  labs(y = "Port")


# make pdf for lengths  ====
pdf(file=paste0(od,"comm.len.sampling.pdf"), width=8, height=10.5, pointsize=10)

comlen.annual.n.sa
comlen.annual.prop.sa
comlen.annual.n.mk
comlen.annual.prop.mk
comlen.annual.n.gear
comlen.annual.prop.gear
comlen.annual.n.port 
comlen.annual.prop.port
comlen.annual 
comlen.annual.sa
comlen.q1.sa
comlen.q2.sa
comlen.q3.sa
comlen.q4.sa
comlen.annual.mk
comlen.annual.gear
comlen.annual.port

dev.off()



##-------------  ages ====

cat.age <- as.data.frame(read.csv(f.cat.age, header=T))

#subset for my stock only
my.age <- cat.age[cat.age$STOCK==stock, ]

#reassign stat areas that were renamed after hague line
my.age$SA[which(my.age$SA==523)] <- 561
my.age$SA[which(my.age$SA==524)] <- 562
my.age$SA <- as.factor(my.age$SA)
my.age$NEGEAR <- as.factor(my.age$NEGEAR)
my.age$MK <- as.factor(my.age$MK)



my.tibble <- as_tibble(my.age)
my.tibble.age <- my.tibble
my.tibble <- my.tibble %>%
  mutate(PORT_NAME2 = substr(PORT_NAME,1,7))

#expand rows based on LENGTHS (so each AGE will have nrows=AGES)
my.tibble.long <- my.tibble %>%
  uncount(AGES)

my.q1 <- filter(my.tibble.long, QTR==1)
my.q2 <- filter(my.tibble.long, QTR==2)
my.q3 <- filter(my.tibble.long, QTR==3)
my.q4 <- filter(my.tibble.long, QTR==4)



# Summarize Age Sampling Numbers  ====

# ---- by statistical area ----
age.col <- assign.col( x=sort(unique(my.tibble.long$SA)),  x.mat=sa.mat )
comage.annual.n.sa <- ggplot(my.tibble.long, aes(YEAR, fill=SA)) +
  geom_histogram(binwidth=1)+
  ggtitle("US Annual Age Samples - All Gears")+
  scale_fill_manual(values=age.col)

comage.annual.prop.sa <- ggplot(my.tibble.long, aes(YEAR, fill=SA)) +
  geom_histogram(binwidth=1, position="fill")+
  ggtitle("US Annual Age Samples - All Gears")+
  scale_fill_manual(values=age.col)+
  labs(y = "Proportion")


# ---- by  market ----
#(4 unique for gbhaddock)
age.mk <- sort(unique(my.tibble$MK))
n.age.mk <- length(age.mk)
mk.col.mat <- matrix(NA, nrow=1, ncol=n.age.mk)
colnames(mk.col.mat) <- age.mk 
rownames(mk.col.mat) <- ""
mk.col.mat[1,] <- rev(c('#edb9da','#c994c7','#e7298a','#980043', '#ffffff') ) # !!NOTE:  i manually made sure the number of colors=n.age.mk

mk.col <- mk.col.mat[1,which(colnames(mk.col.mat) ==  sort(unique(my.tibble.long$MK))  )]
comage.annual.n.mk <- ggplot(my.tibble.long, aes(YEAR, fill=MK)) +
  geom_histogram(binwidth=1)+
  ggtitle("US Annual Age Samples By Market Category")+
  scale_fill_manual(values=mk.col)

comage.annual.prop.mk <- ggplot(my.tibble.long, aes(YEAR, fill=MK)) +
  geom_histogram(binwidth=1, position="fill")+
  ggtitle("US Annual Age Samples By Market Category")+
  scale_fill_manual(values=mk.col)+
  labs(y = "Proportion")

# ---- by  gear ----
#(8 unique for gbhaddock)
age.gear <- sort(unique(my.tibble$NEGEAR))
n.age.gear <- length(age.gear)
gear.col.mat <- matrix(NA, nrow=1, ncol=n.age.gear)
colnames(gear.col.mat) <- age.gear  #c(as.character(age.gear))
rownames(gear.col.mat) <- ""
gear.col.mat[1,] <- rev(c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#0c2c84') ) # !!NOTE:  i manually made sure the number of colors=n.age.gear



gear.col <- gear.col.mat[1,which(colnames(gear.col.mat) ==  sort(unique(my.tibble.long$NEGEAR))  )]
comage.annual.n.gear <- ggplot(my.tibble.long, aes(YEAR, fill=NEGEAR)) +
  geom_histogram(binwidth=1)+
  ggtitle("US Annual Age Samples By Gear")+
  scale_fill_manual(values=gear.col)

comage.annual.prop.gear <- ggplot(my.tibble.long, aes(YEAR, fill=NEGEAR)) +
  geom_histogram(binwidth=1, position="fill")+
  ggtitle("US Annual Age Samples By Gear")+
  scale_fill_manual(values=gear.col)+
  labs(y = "Proportion")



# ---- by  port  ----
# (12 unique for gb haddock)
age.port <- sort(unique(my.tibble$PORT_NAME2))
n.age.port <- length(age.port)
port.col.mat <- matrix(NA, nrow=1, ncol=n.age.port)
colnames(port.col.mat) <- age.port  #c(as.character(age.gear))
rownames(port.col.mat) <- ""
port.col.mat[1,] <- c(rev(c('#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#4a1486') ), rev( c('#fdd0a2','#fdae6b','#fd8d3c','#f16913','#d94801','#8c2d04')  ),'#000000'   ) # !!NOTE:  i manually made sure the number of colors=n.age.port

port.col <- port.col.mat[1,which(colnames(port.col.mat) ==  sort(unique(my.tibble.long$PORT_NAME2))  )]
comage.annual.n.port <- ggplot(my.tibble.long, aes(YEAR, fill=PORT_NAME2)) +
  geom_histogram(binwidth=1)+
  labs(fill="Port")+
  ggtitle("US Annual Length Samples By Port")+
  scale_fill_manual(values=port.col)

comage.annual.prop.port <- ggplot(my.tibble.long, aes(YEAR, fill=PORT_NAME2)) +
  geom_histogram(binwidth=1, position="fill")+
  labs(fill="Port")+  
  ggtitle("US Annual Length Samples By Port")+
  scale_fill_manual(values=port.col)+
  labs(y = "Proportion")







# Summarize Age Frequency  of Samples ====
# specify max.age
max.age <- 14
# specify tick mark spacing on x-axis
age.tick <- 2
# specify font size for x-axis
x.font <- 9
# specify font size for y-axis
y.font <- 7



# ---- by year ----
my.tibble.annual <- my.tibble.long %>%
  mutate(YEAR=factor(YEAR), 
         YEAR=factor(YEAR, levels=rev(levels(YEAR))))

comage.annual <- ggplot(my.tibble.annual, aes(x = AGE, y=YEAR,  after_stat(density), fill=YEAR) ) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=x.font),
        axis.text.y  = element_text(size=y.font))+
  scale_x_continuous(breaks=seq(0,max.age,age.tick))+
  coord_cartesian(xlim=c(0, max.age))+
  ggtitle("US Annual AgeFreq - All Gears")+
  theme(legend.position = "none")


# ---- by statistical area ----

 age.col <- assign.col( x=sort(unique(my.tibble.long$SA)), x.mat=sa.mat  )
comage.annual.sa <- ggplot(my.tibble.long, aes(x = AGE, y=SA,  after_stat(density), fill=SA) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=x.font),
        axis.text.y  = element_text(size=y.font))+
  scale_x_continuous(breaks=seq(0,max.age,age.tick))+
  coord_cartesian(xlim=c(0, max.age))+
  ggtitle("US Annual ageFreq - All Gears")+
  scale_fill_manual(values=age.col)


# ---- by statistical area*qtr ----
  age.col <- assign.col( x=sort(unique(my.q1$SA)), x.mat=sa.mat  )
comage.q1.sa <- ggplot(my.q1, aes(x = AGE, y=SA,  after_stat(density), fill=SA) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=x.font),
        axis.text.y  = element_text(size=y.font))+
  scale_x_continuous(breaks=seq(0,max.age,age.tick))+
  coord_cartesian(xlim=c(0, max.age))+
  ggtitle("Q1 AgeFreq - All Gears")+
  scale_fill_manual(values=age.col)


  age.col <- assign.col( x=sort(unique(my.q2$SA)), x.mat=sa.mat  )
comage.q2.sa <- ggplot(my.q2, aes(x = AGE, y=SA,  after_stat(density), fill=SA) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=x.font),
        axis.text.y  = element_text(size=y.font))+
  scale_x_continuous(breaks=seq(0,max.age,age.tick))+
  coord_cartesian(xlim=c(0, max.age))+
  ggtitle("Q2 AgeFreq - All Gears")+
  scale_fill_manual(values=age.col)


  age.col <- assign.col( x=sort(unique(my.q3$SA)), x.mat=sa.mat  )
comage.q3.sa <- ggplot(my.q3, aes(x = AGE, y=SA,  after_stat(density), fill=SA) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=x.font),
        axis.text.y  = element_text(size=y.font))+
  scale_x_continuous(breaks=seq(0,max.age,age.tick))+
  coord_cartesian(xlim=c(0, max.age))+
  ggtitle("Q3 AgeFreq - All Gears")+
  scale_fill_manual(values=age.col)

  age.col <- assign.col( x=sort(unique(my.q4$SA)), x.mat=sa.mat  )
comage.q4.sa <- ggplot(my.q4, aes(x = AGE, y=SA,  after_stat(density), fill=SA) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=x.font),
        axis.text.y  = element_text(size=y.font))+
  scale_x_continuous(breaks=seq(0,max.age,age.tick))+
  coord_cartesian(xlim=c(0, max.age))+
  ggtitle("Q4 AgeFreq - All Gears")+
  scale_fill_manual(values=age.col)



# ---- by market ----
mk.col <- mk.col.mat[1,which(colnames(mk.col.mat) ==  sort(unique(my.tibble.long$MK))  )]
comage.annual.mk <- ggplot(my.tibble.long, aes(x = AGE, y=MK,  after_stat(density), fill=MK) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=x.font),
        axis.text.y  = element_text(size=y.font))+
  scale_x_continuous(breaks=seq(0,max.age,age.tick))+
  coord_cartesian(xlim=c(0, max.age))+
  ggtitle("US Annual AgenFreq By Market")+
  scale_fill_manual(values=mk.col)




# ---- by  gear ----

gear.col <- gear.col.mat[1,which(colnames(gear.col.mat) ==  sort(unique(my.tibble.long$NEGEAR))  )]
comage.annual.gear <- ggplot(my.tibble.long, aes(x = AGE, y=NEGEAR,  after_stat(density), fill=NEGEAR) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=x.font),
        axis.text.y  = element_text(size=y.font))+
  scale_x_continuous(breaks=seq(0,max.age,age.tick))+
  coord_cartesian(xlim=c(0, max.age))+
  ggtitle("US Annual AgeFreq By Gear")+
  scale_fill_manual(values=gear.col)



# ---- by  port ----

port.col <- port.col.mat[1,which(colnames(port.col.mat) ==  sort(unique(my.tibble.long$PORT_NAME2))  )]
comage.annual.port <- ggplot(my.tibble.long, aes(x = AGE, y=PORT_NAME2,  after_stat(density), fill=PORT_NAME2) ) +
  facet_wrap(~YEAR)+
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=x.font),
        axis.text.y  = element_text(size=y.font))+
  scale_x_continuous(breaks=seq(0,max.age,age.tick))+
  coord_cartesian(xlim=c(0, max.age))+
  ggtitle("US Annual AgeFreq By Port")+
  scale_fill_manual(values=port.col)+
  labs(y = "Port")



# make pdf for ages  ====
pdf(file=paste0(od,"comm.age.sampling.pdf"), width=8, height=10.5, pointsize=10)

comage.annual.n.sa
comage.annual.prop.sa
comage.annual.n.mk
comage.annual.prop.mk
comage.annual.n.gear
comage.annual.prop.gear
comage.annual.n.port 
comage.annual.prop.port
comage.annual 
comage.annual.sa
comage.q1.sa
comage.q2.sa
comage.q3.sa
comage.q4.sa
comage.annual.mk
comage.annual.gear
comage.annual.port


dev.off()




##-------------  Summarize landings  ====


cat.land <- as.data.frame(read.csv(f.cat.land, header=T))

#subset for user-specified stock only
my.land <- cat.land[cat.land$STOCK==stock, ]

#reassign stat areas that were renamed after hague line
my.land$SA[which(my.land$SA==523)] <- 561
my.land$SA[which(my.land$SA==524)] <- 562
# make SA and NEGEAR and MK and YEAR factors
my.land$SA <- as.factor(my.land$SA)
my.land$NEGEAR <- as.factor(my.land$NEGEAR)
my.land$MK <- as.factor(my.land$MK)
my.land$MG <- as.factor(my.land$MG)
my.land$TC <- as.factor(my.land$TC)
my.land$ALEVEL <- as.factor(my.land$ALEVEL)


my.tibble.land <- as_tibble(my.land)
my.tibble.land <- mutate(my.tibble.land, MT=LANDINGS_KG_ADJ/1000 )
my.tibble.land <- my.tibble.land %>%
  mutate(PORT_NAME2 = substr(PORT_NAME,1,7))

# ---- by year ----

land.by.yr <- my.tibble.land %>%
  group_by(YEAR) %>%
  summarize(MTsum.yr = sum(MT)) 


# ---- by year*statistical area ----

land.by.yr.sa <- my.tibble.land %>%
  group_by(YEAR, SA) %>%
  summarize(MTsum = sum(MT, na.rm=T)) %>%
  mutate(percentage=MTsum/sum(MTsum)*100) 
  
# --------------plot

land.col <- assign.col( x=sort(unique(land.by.yr.sa$SA)),  x.mat=sa.mat )
comland.annual.sa <- ggplot( land.by.yr.sa, aes(x=YEAR, y=MTsum, fill=SA, text=SA)) +
  geom_area( ) +
ggtitle("US Landings by Statistical Area")+
  scale_fill_manual(values=land.col)

comland.annual.prop.sa <- ggplot( land.by.yr.sa, aes(x=YEAR, y=percentage, fill=SA, text=SA)) +
  geom_area( ) +
  ggtitle("US Landings by Statistical Area")+
  scale_fill_manual(values=land.col)

# # testing bar plots  here ====
# land.by.yr.sa <- my.tibble.land %>%
#   group_by(YEAR, SA) %>%
#   summarize(MTsum = sum(MT, na.rm=T)) %>%
#   mutate(percentage=MTsum/sum(MTsum)*100) %>%
#   right_join(land.by.yr)
# 
# comland.annual.sa.bar <- ggplot( land.by.yr.sa, aes(x=YEAR,  fill=SA)) +
#   geom_histogram(aes(weight=MTsum.yr), binwidth=1)+
#   ggtitle("US Landings by Statistical Area")+
#   scale_fill_manual(values=land.col)+
#   labs(y="MT Landings")
# 
# # testing bar plots  here (proportional bar plot is messed up!) ====
# comland.annual.sa.prop.bar <- ggplot( land.by.yr.sa, aes(x=YEAR,  fill=SA)) +
#   geom_histogram(aes(weight=MTsum.yr), binwidth=1, position="fill")+
#   ggtitle("US Landings by Statistical Area")+
#   scale_fill_manual(values=land.col)+
#   labs(y="MT Landings")
# 



# ---- by year*gear ----

land.by.yr.gear <- my.tibble.land %>%
  group_by(YEAR, GG) %>%
  summarize(MTsum = sum(MT, na.rm=T)) %>%
  mutate(percentage=MTsum/sum(MTsum)*100)
# --------------plot
land.gear <- sort(unique(land.by.yr.gear$GG))
n.land.gear <- length(land.gear)
gear.col.mat <- matrix(NA, nrow=1, ncol=n.land.gear)
colnames(gear.col.mat) <- land.gear  #c(as.character(age.gear))
rownames(gear.col.mat) <- ""
gear.col.mat[1,] <- (c('#a1dab4','#41b6c4','#ffffcc','#253494') ) # !!NOTE:  i manually made sure the number of colors=n.age.gear



gear.col <- gear.col.mat[1,which(colnames(gear.col.mat) ==  sort(unique(land.by.yr.gear$GG))  )]
comland.annual.gear <- ggplot( land.by.yr.gear, aes(x=YEAR, y=MTsum, fill=GG, text=GG)) +
  geom_area( ) +
  ggtitle("US Landings by GEAR")+
  scale_fill_manual(values=gear.col)+
  labs(fill="Gear Group")

comland.annual.prop.gear <- ggplot( land.by.yr.gear, aes(x=YEAR, y=percentage, fill=GG, text=GG)) +
  geom_area( ) +
  ggtitle("US Landings by Gear")+
  scale_fill_manual(values=gear.col)+
  labs(fill="Gear Group")



# ---- by year*statistical area * gear ----

land.by.yr.sa.gear <- my.tibble.land %>%
  select(YEAR, SA, GG, MT) %>%
  group_by(YEAR, SA, GG) %>%
  summarize( MTsum = sum(MT, na.rm=T)) %>%
  mutate(percentage=MTsum/sum(MTsum)*100) 

# --------------plot

land.col <- assign.col( x=sort(unique(land.by.yr.sa.gear$SA)),  x.mat=sa.mat )
comland.annual.sa.gear <- ggplot( land.by.yr.sa.gear, aes(x=YEAR, y=MTsum, fill=SA, text=SA)) +
  geom_area( ) +
  facet_wrap(~GG) +
  ggtitle("US Landings by Statistical Area")+
  scale_fill_manual(values=land.col)

# !!NOTE: something isn't right with the percentages in this plot; probably an issue with group/summarize above

# comland.annual.prop.sa.gear <- ggplot( land.by.yr.sa.gear, aes(x=YEAR, y=percentage, fill=SA, text=SA)) +
#   geom_area( ) +
#   facet_wrap(~GG) +
#   ggtitle("US Landings by Statistical Area")+
#   scale_fill_manual(values=land.col)


# ---- by year*market ----

land.by.yr.mk <- my.tibble.land %>%
  filter(MK %in% c(1470, 1475, 1476, 1479)) %>%
  group_by(YEAR, MK) %>%
  summarize(MTsum = sum(MT, na.rm=T)) %>%
  mutate(percentage=MTsum/sum(MTsum)*100) 

# --------------plot
land.yr.mk <- sort(unique(land.by.yr.mk$MK))
n.land.mk <- length(land.yr.mk)
mk.col.mat <- matrix(NA, nrow=1, ncol=n.land.mk)
colnames(mk.col.mat) <- land.yr.mk  #c(as.character(age.gear))
rownames(mk.col.mat) <- ""
mk.col.mat[1,] <- c("#980043" ,"#e7298a", "#c994c7", "#edb9da" ) # !!NOTE:  i manually made sure the number of colors=n.land.mk


mk.col <- mk.col.mat[1,which(colnames(mk.col.mat) ==  sort(unique(land.by.yr.mk$MK))  )]
comland.annual.mk <- ggplot( land.by.yr.mk, aes(x=YEAR, y=MTsum, fill=MK, text=MK)) +
  geom_area( ) +
  ggtitle("US Landings by Market")+
  scale_fill_manual(values=mk.col)+
  labs(fill="Market")

comland.annual.prop.mk <- ggplot( land.by.yr.mk, aes(x=YEAR, y=percentage, fill=MK, text=MK)) +
  geom_area( ) +
  ggtitle("US Landings by Market")+
  scale_fill_manual(values=mk.col)+
  labs(fill="Market")







# ---- by year*qtr ----
land.by.yr.qtr <- my.tibble.land %>%
  group_by(YEAR, MG) %>%
  summarize(MTsum = sum(MT, na.rm=T))  %>%
  mutate(percentage=MTsum/sum(MTsum)*100)

# --------------plot
land.yr.qtr <- sort(unique(land.by.yr.qtr$MG))
n.land.qtr <- length(land.yr.qtr)
qtr.col.mat <- matrix(NA, nrow=1, ncol=n.land.qtr)
colnames(qtr.col.mat) <- land.yr.qtr  #c(as.character(age.gear))
rownames(qtr.col.mat) <- ""
qtr.col.mat[1,] <- c('#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15' ) # !!NOTE:  i manually made sure the number of colors=n.land.qtr


qtr.col <- qtr.col.mat[1,which(colnames(qtr.col.mat) ==  sort(unique(land.by.yr.qtr$MG))  )]
comland.annual.qtr <- ggplot( land.by.yr.qtr, aes(x=YEAR, y=MTsum, fill=MG, text=MG)) +
  geom_area( ) +
  ggtitle("US Landings by Quarter")+
  scale_fill_manual(values=qtr.col)+
  labs(fill="Quarter")

comland.annual.prop.qtr <- ggplot( land.by.yr.qtr, aes(x=YEAR, y=percentage, fill=MG, text=MG)) +
  geom_area( ) +
  ggtitle("US Landings by Quarter")+
  scale_fill_manual(values=qtr.col)+
  labs(fill="Quarter")





# ---- by year*tc ---- 
land.by.yr.tc <- my.tibble.land %>%
  group_by(YEAR, TC) %>%
  summarize(MTsum = sum(MT, na.rm=T))   %>%
  mutate(percentage=MTsum/sum(MTsum)*100)


# --------------plot
land.yr.tc <- sort(unique(land.by.yr.tc$TC))
n.land.tc <- length(land.yr.tc)
tc.col.mat <- matrix(NA, nrow=1, ncol=n.land.tc)
colnames(tc.col.mat) <- land.yr.tc  #c(as.character(age.gear))
rownames(tc.col.mat) <- ""
tc.col.mat[1,] <- c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e' ) # !!NOTE:  i manually made sure the number of colors=n.land.tc


tc.col <- tc.col.mat[1,which(colnames(tc.col.mat) ==  sort(unique(land.by.yr.tc$TC))  )]
comland.annual.tc <- ggplot( land.by.yr.tc, aes(x=YEAR, y=MTsum, fill=TC, text=TC)) +
  geom_area( ) +
  ggtitle("US Landings by Tonnage Class")+
  scale_fill_manual(values=tc.col)+
  labs(fill="Tonnage Class")

comland.annual.prop.tc <- ggplot( land.by.yr.tc, aes(x=YEAR, y=percentage, fill=TC, text=TC)) +
  geom_area( ) +
  ggtitle("US Landings by Tonnage Class")+
  scale_fill_manual(values=tc.col)+
  labs(fill="Tonnage Class")




# ---- by year*ALEVEL ---- 
land.by.yr.al <- my.tibble.land %>%
  group_by(YEAR, ALEVEL) %>%
  summarize(MTsum = sum(MT, na.rm=T))   %>%
  mutate(percentage=MTsum/sum(MTsum)*100)

# --------------plot
land.yr.al <- sort(unique(land.by.yr.al$ALEVEL))
n.land.al <- length(land.yr.al)
al.col.mat <- matrix(NA, nrow=1, ncol=n.land.al)
colnames(al.col.mat) <- land.yr.al  #c(as.character(age.gear))
rownames(al.col.mat) <- ""
al.col.mat[1,] <- c('#7b3294','#c2a5cf','#f7f7f7','#a6dba0','#008837' ) # !!NOTE:  i manually made sure the number of colors=n.land.al


al.col <- al.col.mat[1,which(colnames(al.col.mat) ==  sort(unique(land.by.yr.al$ALEVEL))  )]
comland.annual.al <- ggplot( land.by.yr.al, aes(x=YEAR, y=MTsum, fill=ALEVEL, text=ALEVEL)) +
  geom_area( ) +
  ggtitle("US Landings by ALEVEL")+
  scale_fill_manual(values=al.col)+
  labs(fill="ALEVEL")

comland.annual.prop.al <- ggplot( land.by.yr.al, aes(x=YEAR, y=percentage, fill=ALEVEL, text=ALEVEL)) +
  geom_area( ) +
  ggtitle("US Landings by ALEVEL")+
  scale_fill_manual(values=al.col)+
  labs(fill="ALEVEL")




# ---- by year*port ---- 
ports.sampled.length <- ports.sampled.length[nchar(ports.sampled.length)>1]
land.by.yr.port <- my.tibble.land %>%
  mutate(PN=ifelse(PORT_NAME2 %in% ports.sampled.length, PORT_NAME2, "OTHER") ) %>%
  group_by(YEAR, PN) %>%
  summarize(MTsum = sum(MT, na.rm=T))   %>%
  mutate(percentage=MTsum/sum(MTsum)*100)

# --------------plot
land.yr.port <- sort(unique(land.by.yr.port$PN))
n.land.port <- length(land.yr.port)
port.col.mat <- matrix(NA, nrow=1, ncol=n.land.port)
colnames(port.col.mat) <- land.yr.port  #c(as.character(age.gear))
rownames(port.col.mat) <- ""
port.col.mat[1,] <- c(rev(c('#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#4a1486') ), rev( c('#fdd0a2','#fdae6b','#fd8d3c','#f16913','#d94801','#8c2d04')  )   ) # !!NOTE:  i manually made sure the number of colors=n.len.port


port.col <- port.col.mat[1,which(colnames(port.col.mat) ==  sort(unique(land.by.yr.port$PN))  )]
comland.annual.port <- ggplot( land.by.yr.port, aes(x=YEAR, y=MTsum, fill=PN, text=PN)) +
  geom_area( ) +
  ggtitle("US Landings by Port")+
  scale_fill_manual(values=port.col)+
  labs(fill="Port")

comland.annual.prop.port <- ggplot( land.by.yr.port, aes(x=YEAR, y=percentage, fill=PN, text=PN)) +
  geom_area( ) +
  ggtitle("US Landings by Port")+
  scale_fill_manual(values=port.col)+
  labs(fill="Port")







# ---- by year*month ---- 
land.by.yr.mo <- my.tibble.land %>%
  group_by(YEAR, MONTH) %>%
  summarize(MTsum = sum(MT, na.rm=T))   %>%
  mutate(percentage=MTsum/sum(MTsum)*100) %>%
  mutate(MONTH=as.factor(MONTH))

# --------------plot
land.yr.mo <- sort(unique(land.by.yr.mo$MONTH))
n.land.mo <- length(land.yr.mo)
mo.col.mat <- matrix(NA, nrow=1, ncol=n.land.mo)
colnames(mo.col.mat) <- land.yr.mo  #c(as.character(age.gear))
rownames(mo.col.mat) <- ""
mo.col.mat[1,] <- c('#000000', '#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695' , '#eeeeee' ) # !!NOTE:  i manually made sure the number of colors=n.len.mo


mo.col <- mo.col.mat[1,which(colnames(mo.col.mat) ==  sort(unique(land.by.yr.mo$MONTH))  )]
comland.annual.mo <- ggplot( land.by.yr.mo, aes(x=YEAR, y=MTsum, fill=MONTH, text=MONTH)) +
  geom_area( ) +
  ggtitle("US Landings by Month")+
  scale_fill_manual(values=mo.col)+
  labs(fill="Month")

comland.annual.prop.mo <- ggplot( land.by.yr.mo, aes(x=YEAR, y=percentage, fill=MONTH, text=MONTH)) +
  geom_area( ) +
  ggtitle("US Landings by Month")+
  scale_fill_manual(values=mo.col)+
  labs(fill="Month")



write.csv(land.by.yr, file=paste0(od,"land.by.yr.csv") )
write.csv(land.by.yr.gear, file=paste0(od,"land.by.yr.gear.csv") )
write.csv(land.by.yr.mk, file=paste0(od,"land.by.yr.mk.csv") )
write.csv(land.by.yr.sa, file=paste0(od,"land.by.yr.sa.csv") )
write.csv(land.by.yr.sa.gear, file=paste0(od,"land.by.yr.sa.gear.csv") )
write.csv(land.by.yr.tc, file=paste0(od,"land.by.yr.tc.csv") )
write.csv(land.by.yr.mo, file=paste0(od,"land.by.yr.mo.csv") )
write.csv(land.by.yr.qtr, file=paste0(od,"land.by.yr.qtr.csv") )
write.csv(land.by.yr.port, file=paste0(od,"land.by.yr.port.csv") )
write.csv(land.by.yr.al, file=paste0(od,"land.by.yr.al.csv") )


# make pdf for landings  ====
pdf(file=paste0(od,"comm.land.pdf"), width=8, height=10.5, pointsize=10)

comland.annual.mo
comland.annual.prop.mo
comland.annual.qtr
comland.annual.prop.qtr
comland.annual.mk 
comland.annual.prop.mk 
comland.annual.sa
comland.annual.prop.sa
comland.annual.gear
comland.annual.prop.gear
comland.annual.sa.gear
comland.annual.tc
comland.annual.prop.tc
comland.annual.al
comland.annual.prop.al
comland.annual.port
comland.annual.prop.port


dev.off()


# === Make Tables for report ====
# https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/
# gt https://gt.rstudio.com/
# flextable:  https://ardata-fr.github.io/flextable-book/index.html
#  
# Length Samples ====
len.mk.sa <- my.tibble.len %>%
  mutate(Market =  case_when(
    MK == 1470 ~ "LARGE",
     MK == 1475 ~ "SCROD",
     MK == 1476 ~ "SNAPPER",
     MK == 1479 ~ "UNKNOWN"
     )
   )  %>%
  mutate(Area = case_when(
    (SA %in% c(551, 552, 561, 562) ) ~"EGB",
   ( !(SA %in% c(551, 552, 561, 562))) ~ "WGB"
     )
         ) %>% 
  mutate(Half = case_when(
    (QTR %in% c(1,2) ) ~"1",
    ( QTR %in% c(3, 4)) ~ "2"
  )
  ) %>% 
  select(YEAR, Market, Area, Half, LENGTHS) %>%
  group_by(YEAR, Market, Area, Half) %>%
  summarize(n.len.samples = sum(LENGTHS, na.rm=T))

  
  
  len.mk.area.table <- len.mk.sa %>% 
    pivot_wider(names_from = c(Market, Area, Half), 
                names_sep = ".",
               # names_prefix = "Len.",
                values_from=n.len.samples) 
  
  
  write.csv(len.mk.area.table, file=paste0(od, "Lengths_Sampled_by_Mkt_Area.csv"), row.names = F)
  
  
  # Age Samples ====
  age.mk.area <- my.tibble.age %>%
    filter(MONTH>0, MK %in% c(1470, 1475, 1476, 1479)) %>%
    mutate(Market =  case_when(
      MK == 1470 ~ "LARGE",
      MK == 1475 ~ "SCROD",
      MK == 1476 ~ "SNAPPER",
      MK == 1479 ~ "UNKNOWN"
    )
    )  %>%
    mutate(Area = case_when(
      (SA %in% c(551, 552, 561, 562) ) ~"EGB",
      ( !(SA %in% c(551, 552, 561, 562))) ~ "WGB"
    )
    ) %>% 
    mutate(Half = case_when(
      (QTR %in% c(1,2) ) ~"1",
      ( QTR %in% c(3, 4)) ~ "2"
    )
    ) %>% 
    select(YEAR, Market, Area, Half, AGES) %>%
    group_by(YEAR, Market, Area, Half) %>%
    summarize(n.age.samples = sum(AGES, na.rm=T))
  
  
  
  age.mk.area.table <- age.mk.area %>% 
    pivot_wider(names_from = c(Market, Area, Half), 
                names_sep = ".",
                #names_prefix = "Age.",
                values_from=n.age.samples)
  
  
  
  
  write.csv(len.mk.area.table, file=paste0(od, "Ages_Sampled_by_Mkt_Area.csv"), row.names = F)
  
  
  
  # Landings ====
  land.mk.area <- my.tibble.land %>%
    filter(MONTH>0, MK %in% c(1470, 1475, 1476, 1479)) %>%
    mutate(Market =  case_when(
      MK == 1470 ~ "LARGE",
      MK == 1475 ~ "SCROD",
      MK == 1476 ~ "SNAPPER",
      MK == 1479 ~ "UNKNOWN"
    )
    )  %>%
    mutate(Area = case_when(
      (SA %in% c(551, 552, 561, 562) ) ~"EGB",
      ( !(SA %in% c(551, 552, 561, 562))) ~ "WGB"
    )
    ) %>% 
    mutate(Half = case_when(
      (MONTH %in% seq(1,6) ) ~"1",
      ( MONTH %in% seq(7,12)) ~ "2"
    )
    ) %>% 
    select(YEAR, Market, Area, Half, MT) %>%
    group_by(YEAR, Market, Area, Half) %>%
    summarize(MT.sum = sum(MT, na.rm=T))
  
  
  
  land.mk.area.table <- land.mk.area %>% 
    pivot_wider(names_from = c(Market, Area, Half), 
                names_sep = ".",
                #names_prefix = "Land.",
                values_from=MT.sum)
  
  
  
  
 write.csv(land.mk.area.table, file=paste0(od, "Landings_by_Mkt_Area.csv"), row.names = F)
  
  



 land.len.table <- land.mk.area.table %>%
   left_join(len.mk.area.table, by="YEAR", suffix = c(".land", ".len") )
 land.len.age.table <- land.len.table %>%
   left_join(age.mk.area.table, by="YEAR", suffix = c(".x", ".len")) 

  large <- land.len.age.table %>% 
   select(starts_with("LARGE")) %>%
   mutate(
     LARGE.EGB.1.Lengths.per.MT = (LARGE.EGB.1.len)/(LARGE.EGB.1.land),
     LARGE.EGB.1.Ages.per.MT = (LARGE.EGB.1)/(LARGE.EGB.1.land),
     LARGE.EGB.2.Lengths.per.MT = (LARGE.EGB.2.len)/(LARGE.EGB.2.land),
     LARGE.EGB.2.Ages.per.MT = (LARGE.EGB.2)/(LARGE.EGB.2.land),
     LARGE.WGB.1.Lengths.per.MT = (LARGE.WGB.1.len)/(LARGE.WGB.1.land),
     LARGE.WGB.1.Ages.per.MT = (LARGE.WGB.1)/(LARGE.WGB.1.land),
     LARGE.WGB.2.Lengths.per.MT = (LARGE.WGB.2.len)/(LARGE.WGB.2.land),
     LARGE.WGB.2.Ages.per.MT = (LARGE.WGB.2)/(LARGE.WGB.2.land)
   ) %>%
   replace(is.na(.), 0) %>% 
    select(YEAR, ends_with(c(".land","per.MT" )) ) %>%
    mutate(across(where(is.double), round, 1)) 
  
  
  

  
  

 

 
 scrod <- land.len.age.table %>% 
   select(starts_with("SCROD"))
 
 scrod <- land.len.age.table %>% 
   select(starts_with("SCROD")) %>%
   mutate(
     SCROD.EGB.1.Lengths.per.MT = (SCROD.EGB.1.len)/(SCROD.EGB.1.land),
     SCROD.EGB.1.Ages.per.MT = (SCROD.EGB.1)/(SCROD.EGB.1.land),
     SCROD.EGB.2.Lengths.per.MT = (SCROD.EGB.2.len)/(SCROD.EGB.2.land),
     SCROD.EGB.2.Ages.per.MT = (SCROD.EGB.2)/(SCROD.EGB.2.land),
     SCROD.WGB.1.Lengths.per.MT = (SCROD.WGB.1.len)/(SCROD.WGB.1.land),
     SCROD.WGB.1.Ages.per.MT = (SCROD.WGB.1)/(SCROD.WGB.1.land),
     SCROD.WGB.2.Lengths.per.MT = (SCROD.WGB.2.len)/(SCROD.WGB.2.land),
     SCROD.WGB.2.Ages.per.MT = (SCROD.WGB.2)/(SCROD.WGB.2.land)
   ) %>%
   replace(is.na(.), 0) %>% 
   select(YEAR, ends_with(c(".land","per.MT" )) ) %>%
   mutate(across(where(is.double), round, 1)) 
   
 
 
 
 snapper <- land.len.age.table %>% 
   select(starts_with("SNAPPER"))
 
 snapper <- land.len.age.table %>% 
   #filter(YEAR>2000) %>%
   select(starts_with("SNAPPER")) %>%
   mutate(
     SNAPPER.EGB.1.Lengths.per.MT = (SNAPPER.EGB.1.len)/(SNAPPER.EGB.1.land),
     SNAPPER.EGB.1.Ages.per.MT = (SNAPPER.EGB.1)/(SNAPPER.EGB.1.land),
     SNAPPER.EGB.2.Lengths.per.MT = (SNAPPER.EGB.2.len)/(SNAPPER.EGB.2.land),
     SNAPPER.EGB.2.Ages.per.MT = (SNAPPER.EGB.2)/(SNAPPER.EGB.2.land),
     SNAPPER.WGB.1.Lengths.per.MT = (SNAPPER.WGB.1.len)/(SNAPPER.WGB.1.land),
     SNAPPER.WGB.1.Ages.per.MT = (SNAPPER.WGB.1)/(SNAPPER.WGB.1.land),
     SNAPPER.WGB.2.Lengths.per.MT = (SNAPPER.WGB.2.len)/(SNAPPER.WGB.2.land),
     SNAPPER.WGB.2.Ages.per.MT = (SNAPPER.WGB.2)/(SNAPPER.WGB.2.land)
   ) %>%
   replace(is.na(.), 0) %>% 
   select(YEAR, ends_with(c(".land","per.MT" )) ) %>%
   mutate(across(where(is.double), round, 1)) 
 
 
 
 write.csv(large, file=paste0(od, "Landings_Len_Age_LARGE.csv"), row.names = F)  #large[, order(colnames(large))]
 write.csv(scrod, file=paste0(od, "Landings_Len_Age_SCROD.csv"), row.names = F)
 write.csv(snapper, file=paste0(od, "Landings_Len_Age_SNAPPER.csv"), row.names = F)
 
 
 
 
 # ==== Plot Samples per MT ====
 
 
 # --------------plot
 large.yr.samp <- large %>%
   select(YEAR, ends_with("per.MT")) %>%
   filter(YEAR>1969) %>%
   pivot_longer(!YEAR, names_to = "Sampling", values_to = "per_MT") 
 
 scrod.yr.samp <- scrod %>%
   select(YEAR, ends_with("per.MT")) %>%
   filter(YEAR>1969) %>%
   pivot_longer(!YEAR, names_to = "Sampling", values_to = "per_MT") 

 snapper.yr.samp <- snapper %>%
   select(YEAR, ends_with("per.MT")) %>%
   filter(YEAR>1969) %>%
   pivot_longer(!YEAR, names_to = "Sampling", values_to = "per_MT") 
 
 
 
 large.annual.samp <- ggplot( large.yr.samp, aes(x=per_MT, y=YEAR)) +
   geom_vline(xintercept = c(0.2) , col='red', lty=1) +
   geom_segment(aes(x=0 ,xend=per_MT, y=YEAR, yend=YEAR), color="blue" ) +
   facet_wrap(~Sampling) +
   ggtitle("Sampling per Metric Ton - LARGE Market") +
   labs(caption = str_wrap("Figure 1: Number of lengths or ages sampled per MT (1000 kg) of landed catch in market category 'Large'.  The vertical red line is at 0.2, reflecting the ICNAF/NAFO recommendation of 200 lengths per 1000 tons as a minimum requirement (NAFO SCS Doc. 80/VI/20).  No numeric threshold for age samples was recommended.")) +
   theme(plot.caption = element_text(hjust = 0))
   
 scrod.annual.samp <- ggplot( scrod.yr.samp, aes(x=per_MT, y=YEAR)) +
   geom_vline(xintercept = c(0.2) , col='red') +
   geom_segment(aes(x=0 ,xend=per_MT, y=YEAR, yend=YEAR), color="blue" ) +
   facet_wrap(~Sampling) +
   ggtitle("Sampling per Metric Ton - SCROD Market") +
   labs(caption = str_wrap("Figure 2: Number of lengths or ages sampled per MT (1000 kg) of landed catch in market category 'Scrod'.  The vertical red line is at 0.2, reflecting the ICNAF/NAFO recommendation of 200 lengths per 1000 tons as a minimum requirement (NAFO SCS Doc. 80/VI/20).  No numeric threshold for age samples was recommended.")) +
   theme(plot.caption = element_text(hjust = 0))
 
 snapper.annual.samp <- ggplot( snapper.yr.samp, aes(x=per_MT, y=YEAR)) +
   geom_vline(xintercept = c(0.2) , col='red') +
   geom_segment(aes(x=0 ,xend=per_MT, y=YEAR, yend=YEAR), color="blue" ) +
   facet_wrap(~Sampling) +
   ggtitle("Sampling per Metric Ton - SNAPPER Market") +
   labs(caption = str_wrap("Figure 3: Number of lengths or ages sampled per MT (1000 kg) of landed catch in market category 'Snapper'.  The vertical red line is at 0.2, reflecting the ICNAF/NAFO recommendation of 200 lengths per 1000 tons as a minimum requirement (NAFO SCS Doc. 80/VI/20).  No numeric threshold for age samples was recommended.")) +
   theme(plot.caption = element_text(hjust = 0))
   
   
   
   
 # make pdf for sampling of landings  ====
 pdf(file=paste0(od,"Sampling.per.MT.pdf"), width=8, height=10.5, pointsize=10)
 
large.annual.samp
scrod.annual.samp
snapper.annual.samp
 
 dev.off()
 