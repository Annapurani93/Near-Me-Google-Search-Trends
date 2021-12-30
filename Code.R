library(tidyverse)
library(readxl)
library(reshape2)
library(ggtext)
library(cowplot)
library(png)

read_excel("Google search trends.xlsx")->data

data%>%
  type.convert()->data

data%>%
  gather("Trend","Value",2:11)%>%
  mutate(Week=as.Date(Week))->data

data
data%>%
  mutate(Trend=fct_relevel(Trend,"COVID vaccine","COVID test","Food delivery",
                           "Oxygen cylinder","Covid hospital","Tiffin service",
                           "CT scan","Takeout restaurants","Fastag","Driving school"))->data1


ggplot(data1,aes(x=Week,y=Value))+
  geom_area(fill="gray30")+
  geom_line(colour="#fe8a71")+
  facet_wrap(~Trend,nrow=10,scales = "free")+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(colour="#fe8a71",face = "bold"),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="#fe8a71",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=14,face="bold",color="#fe8a71",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="#fe8a71",margin=margin(b=21)),
        plot.caption = element_markdown(size=10, colour="#fe8a71",hjust=0,margin=margin(t=25)))+  
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="WHAT DID INDIANS SEARCH FOR NEAR THEM IN 2021?",
       subtitle = str_wrap("India has the second highest number of Corona virus infections in the world. The second wave of infections that hit the country in March-April-May, proved to be extremely lethal claiming the lives of millions. That being the case, it is understandable why 5 of the top 10 things that were searched for in the NEAR ME category, on Google, were related to COVID in 2021. With the Omicron variant spreading fast across States currently, and with cases beginning to rise again, here's a look at top 10 things that people looked for near them, in 2021, on the web",108),
       caption="<span style='color:#fe8a71'>Numbers represent search interest relative to the highest point on the chart for the given region and time.<br> A value of 100 is the peak popularity for the term. A value of 50 means that the term is half as popular.<br> A score of 0 means there was not enough data for this term.<br> <br>Data: Google Trends| Design and Analysis: @annapurani93</span>")->plot

readPNG("C:/Users/Annapurani/Desktop/coronavirus.png")->logo

ggdraw(plot) +
  draw_image(logo, x = 0.42, y =0.45, scale = .10)->plot2

ggsave("searchtrendsnearme3.png",plot2,width = 10,height=16)
