#order of operations
#Import trait data from excel
#visualize data
#Subset data
#copy + paste  stacked barplot from Download packages
#Install ggplot2
#install tidyverse
#Run code

#===load libraries======
library(ggplot2)
library(tidyverse)
library(vegan)
library(forcats)
library(scales)
library(ggpubr)

#=====import data======
#relative pathname
plt <- file.path(".", "Data", "full_data_cleaned.csv")

##Load data
cwm<-read_csv(plt)

#Sub-setting data to run a stacked barplot

#High resource Low disturbance sub-setting select for any new columns

HRLD<-
  cwm%>%
  select(1, 3:5)%>%
  gather(Categories, Papers, 2:4)

# High resource Low disturbance Stack barplot 
group.colors <- c(Positive_HRLD = "red", Mixed_HRLD = "gray", Negative_HRLD = "blue")

PlotHRLD <- HRLD%>%
  mutate(Traits = fct_reorder(Trait, desc(Trait))) %>%
  ggplot(aes (y=Papers, x=Traits, fill = Categories)) + 
  geom_bar(position="stack", stat="identity")+
  xlab(NULL)+ 
  ylab("Paired studies")+
  scale_fill_manual(values = group.colors) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,10.5), breaks = breaks_pretty()) +
  coord_flip()+
  theme(axis.text.x = element_text(colour= "black", face = "bold", size = 15),
        axis.text.y = element_text(colour= "black", face = "bold", size = 15),
        axis.line = element_line(colour = "black", size = .3),
        axis.line.x = element_line(colour = "black", size =.3),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =.3),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size = 20, color = 'black', face = "bold"),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey", size = .1),
        panel.grid.minor.x = element_line(color = "grey", size = .1),
        panel.background = element_rect(fill= "white"),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))

#===============================================
#High resource High  disturbance subsetting

HRHD<-
  cwm%>%
  select(1,7:9)%>%
  gather(Categories, Papers, 2:4)

# High resource high disturbance Stack barplot 
group.colors <- c(Positive_HRHD = "red", Mixed_HRHD = "gray", Negative_HRHD = "blue")

PlotHRHD <- HRHD%>%
  mutate(Traits = fct_reorder(Trait, desc(Trait))) %>%
  ggplot(aes (y=Papers, x=Traits, fill = Categories)) + 
  geom_bar(position="stack", stat="identity")+
  xlab(NULL)+ 
  ylab("Paired studies")+
  scale_fill_manual(values = group.colors) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,10.5), breaks = breaks_pretty()) +
  coord_flip()+
  theme(axis.text.x = element_text(colour= "black", face = "bold", size = 15),
        axis.text.y = element_text(colour= "black", face = "bold", size = 15),
        axis.line = element_line(colour = "black", size = .3),
        axis.line.x = element_line(colour = "black", size =.3),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =.3),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size = 20, color = 'black', face = "bold"),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey", size = .1),
        panel.grid.minor.x = element_line(color = "grey", size = .1),
        panel.background = element_rect(fill= "white"),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))

#==============================================
#Low resource Low disturbance subsetting

LRLD<-
  cwm%>%
  select(1,11:13)%>%
  gather(Categories, Papers, 2:4)

# Low resource and low disturbance Stack barplot 
group.colors <- c(Positive_LRLD = "red", Mixed_LRLD = "gray", Negative_LRLD = "blue")

PlotLRLD <- LRLD%>%
  mutate(Traits = fct_reorder(Trait, desc(Trait))) %>%
  ggplot(aes (y=Papers, x=Traits, fill = Categories)) + 
  geom_bar(position="stack", stat="identity")+
  xlab(NULL)+ 
  ylab("Paired studies")+
  scale_fill_manual(values = group.colors) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,10.5), breaks = breaks_pretty()) +
  coord_flip()+
  theme(axis.text.x = element_text(colour= "black", face = "bold", size = 15),
        axis.text.y = element_text(colour= "black", face = "bold", size = 15),
        axis.line = element_line(colour = "black", size = .3),
        axis.line.x = element_line(colour = "black", size =.3),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =.3),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size = 20, color = 'black', face = "bold"),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey", size = .1),
        panel.grid.minor.x = element_line(color = "grey", size = .1),
        panel.background = element_rect(fill= "white"),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))

#==============================================
#Low resource and High disturbance subsetting

LRHD<-
  cwm%>%
  select(1,15:17)%>%
  gather(Categories, Papers, 2:4)

# Low resource and high disturbance Stack barplot 
group.colors <- c(Positive_LRHD = "red", Mixed_LRHD = "gray", Negative_LRHD = "blue")

PlotLRHD <- LRHD%>%
  mutate(Traits = fct_reorder(Trait, desc(Trait))) %>%
  ggplot(aes (y=Papers, x=Traits, fill = Categories)) + 
  geom_bar(position="stack", stat="identity")+
  xlab(NULL)+ 
  ylab("Paired studies")+
  scale_fill_manual(values = group.colors) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,10.5), breaks = breaks_pretty()) +
  coord_flip()+
  theme(axis.text.x = element_text(colour= "black", face = "bold", size = 15),
        axis.text.y = element_text(colour= "black", face = "bold", size = 15),
        axis.line = element_line(colour = "black", size = .3),
        axis.line.x = element_line(colour = "black", size =.3),
        axis.ticks.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size =.3),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size = 20, color = 'black', face = "bold"),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey", size = .1),
        panel.grid.minor.x = element_line(color = "grey", size = .1),
        panel.background = element_rect(fill= "white"),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))

## plot all graphs
figure <- ggarrange(PlotHRLD,
                    PlotHRHD,
                    PlotLRLD,
                    PlotLRHD,
                    labels = c("(a)", "(b)", "(c)", "(d)"),
                    font.label=list(color="black",size=20),
                    ncol = 2, nrow = 2)
figure