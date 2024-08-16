#This script creates summaries for Figure 2b & 2c

setwd("/Users/alexaputillo/Library/CloudStorage/OneDrive-FloridaStateUniversity/PhD/Projects/PhD Chapters/Bimini Grazing/Figure 2")

install.packages(readxl)
library(readxl)



#Data
meta<-read_excel(file.choose("Metadata_Bimini_Grazing_Reformatted Final.xlsx"))


install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)

#Response Variable Counts 
df<-meta %>%
  # count number of rows for each combination of response category and paper
  group_by(Response.Cat, Reference) %>%
  tally() %>%
  # pivot the protocol names over the columns
  pivot_wider(names_from=Response.Cat, values_from=n) %>%
  # replace NA values in all columns with 0
  mutate(across(everything(), .fns=~replace_na(., 0))) %>%
  print(n=Inf) 

view(df)
write.csv(df, "Response_Cat_Summary.csv", row.names = FALSE)

#Seagrass Genera Counts 
df1<-meta %>%
  # count number of rows for each combination of seagrass genera and paper
  group_by(Focal.Genera, Reference) %>%
  tally() %>%
  # pivot the protocol names over the columns
  pivot_wider(names_from=Focal.Genera, values_from=n) %>%
  # replace NA values in all columns with 0
  mutate(across(everything(), .fns=~replace_na(., 0)))

view(df1)
write.csv(df1, "Focal_Genera_Summary.csv", row.names = FALSE)

#Figure 2 Development 
response<-read_excel(file.choose())
genera<-read_excel(file.choose())
response
genera


r<-ggplot(data=response, aes(x=reorder(responsevar,-percentage), y=percentage)) +
  geom_bar(stat="identity", width=0.75, color="Dim Gray", fill="Light Gray") + 
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, by=20), expand=c(0, 0))
res<-r +labs(x ="Seagrass Response Variable", y = "Percentage of Studies (%)") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, color = "black", family = "sans")) +
  theme(plot.margin = margin(10, 10, 10, 20)) +
  theme(axis.text.x = element_text(size = 10, color = "black", family = "sans")) +
  theme(axis.text.y = element_text(size = 10, color = "black", family = "sans"))
res

g<-ggplot(data=genera, aes(x=reorder(genera,-percentage), y=percentage)) +
  geom_bar(stat="identity", width=0.5, color="Dim Gray", fill="Light Gray") + 
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, by=20), expand=c(0, 0))
gen<-g +labs(x ="Seagrass Genera", y = "Percentage of Studies (%)") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(plot.margin = margin(10, 10, 10, 15))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.text.y = element_text(size = 10))
gen


fig<- plot_grid(
  res + theme(plot.margin = margin(10, 5, 10, 30)),
  gen + theme(plot.margin = margin(10, 5, 10, 5)),
  align = "h",
  rel_widths = c(1, 0.8))

ggsave("/Users/AlexaPutillo/Library/CloudStorage/OneDrive-FloridaStateUniversity/PhD/Projects/Bimini Grazing/Figure 2/Figure 2.pdf", width = 18, height = 10, units = "cm")

