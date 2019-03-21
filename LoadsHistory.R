library(dplyr)
library(ggplot2)
library(reshape2)
library(quantreg)
library(plotly)
library(gridExtra)
library(plotly)
library(tidyverse)
library(wesanderson)
library(ggrepel)
library(ggpubr)
library(ggiraph)
library(gsubfn)
library(ggalt)
library(hrbrthemes)


options(dplyr.print_max = 250)
options(scipen=10000)


#______________________________LOADS__________________________________________________________


loads211=read.csv("LoadsDetail_211.csv",h=T)

#manually add modified sector column in excel, manually change 'v7 and 'v14 to 2018' and 2019'

loads221=read.csv("loadsFinal.csv",h=T)

loads227=read.csv("finalloads2.csv",h=T)

loads36=read.csv("loads_36.csv",h=T)





long_loads227 <- loads36 %>% gather(Title,Value ,VA.Draft.WIP.3v7_Amount:X1985.Progress_SLoadEOT) %>% 
separate(Title, c("Year", "Progress","Type"), sep = "\\.{1}")  %>% mutate(Year=gsub('X','',Year))%>%
unite_( "Type", c("Progress","Type"))%>%
filter(Value > 0 & str_detect(Type, 'PLoadEOT') | str_detect(Type, 'NLoadEOT')) %>% 
mutate(Type2=ifelse(str_detect(Type, 'NLoadEOT'),"Nitrogen","Phosphorus")) %>%
select(everything(),-contains("Amount"))%>%
select(everything(),-contains("SLoad"))%>%
              
  
  mutate(Geography=case_when(
    str_detect(Geography,"James") ~ "James",
    str_detect(Geography,"Eastern")~ "Eastern Shore",
    str_detect(Geography,"York") ~"York",
    str_detect(Geography,"Rappahannock") ~"Rappahannock",
    str_detect(Geography,"Potomac") ~"Potomac",
    TRUE  ~ as.character(Geography)
  )) %>% 
  mutate(Wip_Target=as.numeric(case_when(
    Type2=="Phosphorus" & Geography=="Eastern Shore" ~ "0.164",
    Type2=="Phosphorus" & Geography=="James" ~ "2.758",                        
    Type2=="Phosphorus" & Geography=="Potomac" ~ "1.867",                             
    Type2=="Phosphorus" & Geography=="Rappahannock" ~ "0.84",                              
    Type2=="Phosphorus" & Geography=="York" ~ "0.557",
    Type2=="Nitrogen" & Geography=="Eastern Shore" ~ "1.42",
    Type2=="Nitrogen" & Geography=="James" ~ "26.01",                        
    Type2=="Nitrogen" & Geography=="Potomac" ~ "15.98",                             
    Type2=="Nitrogen" & Geography=="Rappahannock" ~ "6.86",                              
    Type2=="Nitrogen" & Geography=="York" ~ "5.54",
    TRUE                      ~ "other" 
  )
  ),Climate=as.numeric(case_when(
    Type2=="Phosphorus" & Geography=="Eastern Shore" ~ "5000",
    Type2=="Phosphorus" & Geography=="James" ~ "59000",                        
    Type2=="Phosphorus" & Geography=="Potomac" ~ "82000",                             
    Type2=="Phosphorus" & Geography=="Rappahannock" ~ "27000",                              
    Type2=="Phosphorus" & Geography=="York" ~ "14000",
    Type2=="Nitrogen" & Geography=="Eastern Shore" ~ "110000",
    Type2=="Nitrogen" & Geography=="James" ~ "480000",                        
    Type2=="Nitrogen" & Geography=="Potomac" ~ "620000",                             
    Type2=="Nitrogen" & Geography=="Rappahannock" ~ "310000",                              
    Type2=="Nitrogen" & Geography=="York" ~ "200000",
    TRUE                      ~ "other" 
  ) ),Wip_Target=Wip_Target *1000000,Climate2= Wip_Target - Climate)


long_loads227_gapclose=long_loads227 %>% 
mutate(Value2=ifelse(Year==2019 & Value > 0,case_when(
    Type2=="Nitrogen" & Sector=="Wastewater" & Geography=="Eastern Shore" ~ Value-3000,
    Type2=="Nitrogen" & Sector=="Wastewater" & Geography=="York" ~ Value-46000,
    Type2=="Nitrogen" & Sector=="Wastewater" & Geography=="Rappahannock" ~ Value-39000,
    Type2=="Nitrogen" & Sector=="Wastewater" & Geography=="James" ~  Value -3520000,
    Type2=="Nitrogen" & Sector=="Wastewater" & Geography=="Potomac" ~ Value-105000,
    Type2=="Nitrogen" & Sector=="Natural" & Geography=="Eastern Shore" ~ Value - 0,
    Type2=="Nitrogen" & Sector=="Natural" & Geography=="York" ~ Value-20000,
    Type2=="Nitrogen" & Sector=="Natural" & Geography=="Rappahannock" ~ Value-20000,
    Type2=="Nitrogen" & Sector=="Natural" & Geography=="James" ~ Value -60000,
    Type2=="Nitrogen" & Sector=="Natural" & Geography=="Potomac" ~ Value-100000,
    Type2=="Phosphorus" & Sector=="Wastewater" & Geography=="Eastern Shore" ~ Value-400,
    Type2=="Phosphorus" & Sector=="Wastewater" & Geography=="York" ~ Value-5300,
    Type2=="Phosphorus" & Sector=="Wastewater" & Geography=="Rappahannock" ~ Value-2200,
    Type2=="Phosphorus" & Sector=="Wastewater" & Geography=="James" ~ Value -236500,
    Type2=="Phosphorus" & Sector=="Wastewater" & Geography=="Potomac" ~ Value-8200,
    TRUE                      ~ Value-0 
),Value),Diff=Value-Value2,Value2=ifelse(Value2 > 0,Value2,0))



##

long_loads227_gapclose=long_loads227_gapclose %>% 
add_row(Geography="James",Climate2=0,Climate=0,Wip_Target=0,Year=2019,Value2=300000.01,Type2="Phosphorus",Modified.Sector="Wastewater") %>%
add_row(Geography="James",Climate2=0,Climate=0,Wip_Target=0,Year=2018,Value2=300000.02,Type2="Phosphorus",Modified.Sector="Wastewater") %>%
add_row(Geography="James",Climate2=0,Climate=0,Wip_Target=0,Year=2019,Value2=3000000.03,Type2="Nitrogen",Modified.Sector="Wastewater")%>%
add_row(Geography="James",Climate2=0,Climate=0,Wip_Target=0,Year=2018,Value2=3000000.04,Type2="Nitrogen",Modified.Sector="Wastewater")

###
  
extrabar=long_loads227_gapclose %>%
filter(Year==2019 | Year==2018) %>%
group_by(Type2,Year) %>%
summarise(Climate3=sum(Value2)+sum(unique(Climate)))


#n=long_loads227_gapclose %>%
#filter(Geography=="York")

#n1 <- nPlot(Value2 ~ Year, group = "Modified.Sector", data = n, type = "multiBarChart")

#p8 = nPlot(Value2 ~ Year, group = "Modified.Sector", data = n, type = 'stackedAreaChart', id = 'chart')
#p8$chart(useInteractiveGuideline=TRUE)
#p8$facet(var = 'Type2', type = 'wrap', rows = 2)
#p8$print(include_assets=T)

#j=long_loads227_gapclose %>%
#filter(Year==2019 | Year==2018) %>%
#group_by(Modified.Sector,Year)%>%
#streamgraph("Modified.Sector", "Value2", "Year",interpolate="cardinal")

#g=j %>% filter(Year %in% 2010:2019 & Value2 > 0) %>% mutate(Year=as.numeric(Year),Value3=round(Value2,0)) %>%
#arrange(Year)%>%  
#select("Modified.Sector", "Value3", "Year")%>%
#gather(key=Modified.Sector,value=Value2,-Year)
#streamgraph("Modified.Sector", value ="Value3", date="Year") %>%

#sg_axis_x(1, "Year", "%Y") %>%
#offset = "expand", interpolate = "cardinal", interactive = TRUE,
#scale = "date", top = 20, right = 40, bottom = 30, left = 50)




All_Loads_Baywide_Phos_318=long_loads227_gapclose %>% 
filter(!Type2=="Nitrogen")%>%
ggplot() +
annotate("rect", xmin="14", xmax=Inf, ymin=0, ymax=Inf,alpha=0.1, fill="black",hjust = 0.2)+
### didn't need ##geom_text(alpha=0.5,hjust =0.35,vjust =1.5,colour="grey75",size=8,aes(x = "2018"),y=Inf, label = "WIP III")+
#geom_bar(fill="grey45",width=0.7,data=extrabar %>% filter(Type2=="Nitrogen"),stat="identity",aes(colour="Climate Change",x=Year,y=Climate3))+
geom_bar(fill="grey45",width=0.7,data=extrabar %>% filter(!Type2=="Nitrogen"),stat="identity",aes(colour="Climate Change",x=Year,y=Climate3))+
geom_bar(aes(x=Year,y=Value2,fill=Modified.Sector),stat="identity",width=0.7)+
scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
#geom_hline(data=.%>% filter(Type2=="Nitrogen"),aes(yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
geom_hline(data=.%>% filter(!Type2=="Nitrogen"),aes(yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
scale_color_manual(values=c("Climate Change"="grey65"))+
theme_minimal()+
theme_ipsum_rc(grid="Y")+
theme(legend.title=element_blank())+
theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
theme(legend.key.size =  unit(0.25, "in"))+
#facet_wrap(~Type2,scales = "free",nrow=2)+
theme(legend.text=element_text(size=12),legend.position="right")+
scale_fill_manual(name = 'WIP III Target', 
values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
"Wastewater"="#d73027","Septic"="#d8b365"))+
scale_x_discrete(limits=c("1985","1111","2009","2010","2011","2012","2013","2014","2015","2016","2017","14","2018","2019"),
labels=c("1985","","2009","2010","2011","2012","2013","2014","2015","2016","2017","","WIP III Initial","WIP III Draft"))+
labs(x=NULL, y=NULL, title="Phosphorus")+
theme(strip.background =element_rect(fill="black"))+
theme(strip.text.x = element_text(colour = 'white',size=15,face="bold"))+
theme(axis.line = element_line())+
scale_linetype_manual(values=c("WIP III Target"="solid", "Climate Change"="dotted"))+
theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
theme(strip.background = element_rect(fill="black"),
strip.text = element_text(size=15, colour="white"))
  

#ORIGINAL# ggsave("All_Loads_Baywide_Nitro_318.png", width = 25, height = 25, units = "cm",dpi = 600)

ggsave("All_Loads_Baywide_Nitro_318.png", width = 27, height = 20, units = "cm",dpi = 600)
ggsave("All_Loads_Baywide_Phos_318.png", width = 27, height = 20, units = "cm",dpi = 600)



grid.arrange(All_Loads_Baywide_36.1,All_Loads_Baywide_36.3,All_Loads_Baywide_36.2,ncol=3)




write.csv(long_loads2,"Loads_modified_221.csv")
write.csv(long_loads2,"Loads_modified_222.csv")
write.csv(long_loads227,"Loads_modified_227.csv")



text <- paste("Basin:", long_loads227_gapclose$Geography, "<br>",
              "Value:", long_loads227_gapclose$Value2, "<br>",
              "Year:", long_loads227_gapclose$Year)


ggplotly(All_Loads_Baywide_38,tooltip=paste("Basin:", long_loads227_gapclose$Geography, "<br>",
                                             "Value:", long_loads227_gapclose$Value2, "<br>",
                                             "Year:", long_loads227_gapclose$Year))


gg$x$data[[2]]$text <- paste("Basin:", long_loads227_gapclose$Geography, "<br>",
                             "Value:", long_loads227_gapclose$Value2, "<br>",
                             "Year:", long_loads227_gapclose$Year)

 
 #____for each basin~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

extrabar2=long_loads227_gapclose %>%
  filter(Year==2019 | Year==2018) %>%
  group_by(Type2,Year,Geography) %>%
  summarise(Climate3=sum(Value2)+sum(unique(Climate)))


loads.graph <- function(long_loads227_gapclose, na.rm = TRUE, ...){
  # create list of counties in data to loop over 
  basin_list <- unique(long_loads227_gapclose$Geography)
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(basin_list)) { 

 plot=ggplot(subset(long_loads227_gapclose,long_loads227_gapclose$Geography==basin_list[i]),aes(x=Year,y=Value2,fill=Modified.Sector)) +
  annotate("rect", xmin="14", xmax=Inf, ymin=0, ymax=Inf,alpha=0.1, fill="black",hjust = 0.2)+
  #geom_rect(mapping=aes(xmin="2018", xmax="2019", ymin=0, ymax=Value+1000), color="black", alpha=0.5)+ 
  #geom_area(stat="identity")
  scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
  geom_bar(fill="grey45",width=0.7,data=extrabar2 %>% filter(Geography %in% basin_list[i] & Type2=="Nitrogen" & Year==2019) ,stat="identity",aes(colour="Climate Change",x=Year,y=Climate3))+
  geom_bar(fill="grey45",width=0.7,data=extrabar2 %>% filter(Geography %in% basin_list[i] & !Type2=="Nitrogen" & Year==2019) ,stat="identity",aes(colour="Climate Change",x=Year,y=Climate3))+
  geom_bar(fill="grey45",width=0.7,data=extrabar2 %>% filter(Geography %in% basin_list[i] & Type2=="Nitrogen" & Year==2018) ,stat="identity",aes(colour="Climate Change",x=Year,y=Climate3))+
  geom_bar(fill="grey45",width=0.7,data=extrabar2 %>% filter(Geography %in% basin_list[i] &!Type2=="Nitrogen" & Year==2018) ,stat="identity",aes(colour="Climate Change",x=Year,y=Climate3))+
  geom_bar(stat="identity",width=0.7)+
  #use#geom_text(alpha=0.5,hjust =0.35,vjust =1.5,colour="grey75",size=8,aes(x = "2018"),y=Inf, label = "WIP III")+
   #geom_text(aes(0, Wip_Target, label=scales::comma(Wip_Target), vjust=-1,hjust=-0.25),family="Times")+
  geom_hline(data=.%>% filter(Type2=="Nitrogen"),aes(yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
  #geom_hline(data=.%>% filter(Type2=="Nitrogen"),aes(yintercept=sum(unique(Climate2)),linetype="Climate Change"),color="black", size=1.5)+
  geom_hline(data=.%>% filter(!Type2=="Nitrogen"),aes(yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
  #geom_hline(data=.%>% filter(!Type2=="Nitrogen"),aes(yintercept=sum(unique(Climate2)),linetype="Climate Change"),color="black", size=1.5)+
  facet_wrap(~Type2, scales = "free",ncol=1)+
  scale_color_manual(values=c("Climate Change"="grey65"))+
  theme_minimal()+
  theme_ipsum_rc(grid="Y")+
  theme(legend.title=element_blank())+
  #scale_fill_brewer(type = 'div', palette = "Spectral",direction=-1)+
  #theme(strip.text.x = element_text(colour = "white", face = "bold",size = rel(1.5)))+
  theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
  #theme(legend.key = element_rect(size = 5, fill = 'white'))+
  theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
  theme(legend.key.size =  unit(0.25, "in"))+
  #theme(panel.background = element_rect(fill = "grey60"), panel.grid = element_blank())+
  #facet_wrap(~Geography,scales = "free")+
  theme(legend.text=element_text(size=12),legend.position="right")+
   scale_fill_manual(name = 'WIP 3 Progress', 
                     values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
                               "Wastewater"="#d73027","Septic"="#d8b365"))+
   scale_x_discrete(limits=c("1985","1111","2009","2010","2011","2012","2013","2014","2015","2016","2017","14","2018","2019"),
                    labels=c("1985","","2009","2010","2011","2012","2013","2014","2015","2016","2017","","WIP III Initial","WIP III Draft"))+
   labs(x=NULL, y=NULL, title="")+
   theme(strip.background =element_rect(fill="black"))+
  theme(strip.text.x = element_text(colour = 'white',size=15,face="bold"))+
  theme(axis.line = element_line())+
  scale_linetype_manual(values=c("WIP III Target"="solid", "Climate Change"="dotted"))

 
 ggsave(plot,file=paste("38_NormalLabel_FinalLoads",basin_list[i],".png"), width = 25, height = 25, units = "cm",dpi = 600)
 
 
    #print(plot)
  }
}


loads.graph(long_loads227_gapclose)

#______________________PIES________________________________________________~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



pie1= read.csv("2017_LU.csv",h=T)
pie2= read.csv("2025_LU.csv",h=T)

pies= full_join(pie1, pie2)

pies4=pies %>%
mutate(Modified3=ifelse(LoadSource=='Regulated Construction',"Developed(Non MS4)", as.character(Modified))

)

pies5=pies %>% group_by(LoadSource) %>% 
  fill(Modified) %>%
  ungroup()

Potomac_pies=pies5 %>% 
filter(str_detect(StateBasin,"Potomac")) %>% 
group_by(Modified,Year)%>%
summarise(Total=sum(PreBMPAcres))%>%
ungroup() %>%
group_by(Year)%>%
mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 1), "%"))

York_pies=pies5 %>% 
  filter(str_detect(StateBasin,"York")) %>% 
  group_by(Modified,Year)%>%
  summarise(Total=sum(PreBMPAcres))%>%
  ungroup() %>%
  group_by(Year)%>%
  mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 1), "%"))

Rappahannock_pies=pies5 %>% 
  filter(str_detect(StateBasin,"Rappahannock")) %>% 
  group_by(Modified,Year)%>%
  summarise(Total=sum(PreBMPAcres))%>%
  ungroup() %>%
  group_by(Year)%>%
  mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 1), "%"))

ES_pies=pies5 %>% 
  filter(str_detect(StateBasin,"Eastern")) %>% 
  group_by(Modified,Year)%>%
  summarise(Total=sum(PreBMPAcres))%>%
  ungroup() %>%
  group_by(Year)%>%
  mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 1), "%"))

James_pies=pies5 %>% 
  filter(str_detect(StateBasin,"James")) %>% 
  group_by(Modified,Year)%>%
  summarise(Total=sum(PreBMPAcres))%>%
  ungroup() %>%
  group_by(Year)%>%
  mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 1), "%"))

All_pies=pies %>% 
  #filter(str_detect(StateBasin,"James")) %>% 
  group_by(Modified,Year,StateBasin)%>%
  summarise(Total=sum(PreBMPAcres))%>%
  ungroup() %>%
  mutate(StateBasin=case_when(
    str_detect(StateBasin,"James") ~ "James",
    str_detect(StateBasin,"Eastern")~ "Eastern Shore",
    str_detect(StateBasin,"York") ~"York",
    str_detect(StateBasin,"Rappahannock") ~"Rappahannock",
    str_detect(StateBasin,"Potomac") ~"Potomac",
    TRUE  ~ as.character(StateBasin)
  ))%>%
  
  group_by(Year,StateBasin)%>%
  mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 1), "%")) 

#########################################################################

  james=James_pies %>% 
  ggplot(aes(x="", y=P, fill=Modified)) + 
  geom_bar(stat="identity", width=4,size = 1.5, color = "white")+
  coord_polar("y", start=0) + 
  #geom_text(size=5,aes(label = paste0(round(Percent*1), "%")), position = position_stack(vjust = 0.5))+
  geom_label(show.legend = FALSE,size=4,aes(label = Percent), position = position_stack(vjust = 0.5))+
  #geom_label(size=3,aes(label = Modified),position = position_stack(vjust = 0.3))+
  #scale_fill_manual(values=c("#3288BD", "#FDAE61","#FFFFBF","#D53E4F", "#F26419","#FFFFBF"))+
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  theme_classic() + 
  facet_wrap(~Year) +
  scale_fill_manual(name = 'Sector', 
                    values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
                              "Wastewater"="#d73027","Septic"="#d8b365"))+
   theme(strip.text.x = element_text(colour = "white", face = "bold",size = 16))+
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text.x = element_text(colour = 'white',size=15,face="bold"))+
  theme(axis.line = element_blank(),
        axis.text.x=element_text(colour='black'),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))+
  #theme(legend.key.size =  unit(0.25, "in"),legend.text=element_text(size=12),legend.position="left")+
  theme(legend.text=element_text(size=12),legend.position="right")






ggsave("potomac.png", width = 38, height = 20, units = "cm",dpi = 600)
ggsave("ES.png", width = 38, height = 20, units = "cm",dpi = 600)
ggsave("york.png", width = 38, height = 20, units = "cm",dpi = 600)
ggsave("rapp.png", width = 38, height = 20, units = "cm",dpi = 600)
ggsave("james.png", width = 38, height = 20, units = "cm",dpi = 600)



ggsave("York_pie_Plot.png", width = 38, height = 20, units = "cm",dpi = 600)
ggsave("Rappahannock_pie_Plot.png", width = 38, height = 20, units = "cm",dpi = 600)
ggsave("ES_pie_Plot.png", width = 38, height = 20, units = "cm",dpi = 600)
ggsave("James_pie_Plot.png", width = 38, height = 20, units = "cm",dpi = 600)

ggsave("All_pie_Plot.png", width = 38, height = 35, units = "cm",dpi = 600)

##########################################################################################################################




scale_y_continuous(breaks=cumsum(Potomac_pies$P) - Potomac_pies$P / 2, labels= Potomac_pies$P)








#________________________________________
All_pies=pies %>% 
  group_by(Modified,Year,StateBasin)%>%
  summarise(Total=sum(PreBMPAcres))%>%
  ungroup() %>%
  group_by(Year)%>%
  mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 0), "%"))


#__________________________________________________________________________
library(waffle)

p17=Potomac_pies %>% 
  filter(str_detect(Year,"2017"))
p2025=Potomac_pies %>% 
  filter(str_detect(Year,"2025"))


waff17 <- c(
  `Agriculture` = 23, `Developed (MS4)` = 6,
  `Developed (Non MS4)` = 13, `Natural` = 58
)

waff2025 <- c(
  `Agriculture` = 22.3, `Developed (MS4)` = 6,
  `Developed (Non MS4)` = 13.3, `Natural` = 58.3
)


o=waffle(title = "2017",waff17, rows = 5, size = 0.5,colors = c('#4575b4',"#fee090",'#fc8d59',"#7fbf7b"))
p=waffle(title = "2025",waff17, rows = 5, size = 0.5,colors = c('#4575b4',"#fee090",'#fc8d59',"#7fbf7b"))
  

iron(o,p)+
  theme(legend.position="none")

#______________________________________________________________________________________________________________________________________




  scale_fill_manual(name = 'WIP 3 Progress', 
                    values =c('Agriculture'='#3288BD','Natural'='#99D594',
                              "Developed (MS4)"="#FC8D59","Developed (Non MS4)"="#D53E4F","Wastewater"="#eacb5b","Septic"="#8f63e8"))



#8c510a
#d8b365
#f6e8c3
#f5f5f5
#c7eae5
#5ab4ac
#01665e
  
  
  
 #______________________BMPs_____________________________________________________________________________________________________________
  
  
  #WIP3v14BMPDetail_Manure <- read.csv("WIP3v14BMPDetail_Manure.csv",h=TRUE)
  #WIP3v14BMPDetail_Manure=WIP3v14BMPDetail_Manure %>% select(Geography,Agency,BMP,Unit,Sector,AmountCredited)
  #Missing Geography
  
  #~~~~~~~~~~~~~~~~~~Animal~~~~~~~~~~~~~~~~~~~~~~~~~
  W2017BMPDetail_Animal <- read.csv("2017BMPDetail_Animal.csv",h=T)%>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)
  
  
  Animal_Initial <- read.csv("Animal_Initial.csv",h=T)
  Animal_Initial=Animal_Initial %>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)
  
  Animal_April <- read.csv("Animal_April.csv",h=T)
  Animal_April=Animal_April %>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)
  
  Animal_May <- read.csv("Aland.csv",h=T)
  Animal_May=Animal_May %>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~Land~~~~~~~~~~~~~~~~~~~~~~~~
  W2017BMPDetail_Land <- read.csv("2017BMPDetail_Land.csv",h=T)%>%
  filter(str_detect(Agency,"Non-Federal") & !str_detect(FromLoadSource, paste(c("Ms4","CSS"),collapse="|")))%>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)
  
  Land_Initial <- read.csv("Land_Initial.csv",h=T)
  Land_Initial=Land_Initial %>%
  filter(str_detect(Agency,"Non-Federal") & !str_detect(FromLoadSource, paste(c("Ms4","CSS"),collapse="|")))%>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)
  
  Land_April <- read.csv("Land_April.csv",h=T)
  Land_April=Land_April %>% 
  filter(str_detect(Agency,"Non-Federal") & !str_detect(FromLoadSource, paste(c("Ms4","CSS"),collapse="|")))%>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)
  
  Land_May <- read.csv("Fland.csv",h=T)
  Land_May=Land_May %>% 
  filter(str_detect(Agency,"Non-Federal") & !str_detect(FromLoadSource, paste(c("Ms4","CSS"),collapse="|")))%>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)
  
  
#  WIP3v14BMPDetail_Animal <- read.csv("WIP3v14BMPDetail_Animal.csv",h=T)
#  WIP3v14BMPDetail_Animal=WIP3v14BMPDetail_Animal %>% 
#  select(Geography,BMP,Unit,Sector,AmountCredited)
  
  
# WIP3v14BMPDetail_Land <- read.csv("WIP3v14BMPDetail_Land.csv",h=T)%>%
#  filter(str_detect(Agency,"Non-Federal") & !str_detect(FromLoadSource, paste(c("Ms4","CSS"),collapse="|")))%>% 
# select(Geography,BMP,Unit,Sector,AmountCredited)
  
#~~~~~~~~~~~~~~~~~~~

#WIP3v7BMPDetail_Land <- read.csv("WIP3v7BMPDetail_Land.csv",h=T)%>%
#  filter(str_detect(Agency,"Non-Federal") & !str_detect(FromLoadSource, paste(c("Ms4","CSS"),collapse="|")))%>% 
#select(Geography,BMP,Unit,Sector,AmountCredited)

#WIP3v7BMPDetail_Animal <- read.csv("WIP3v7BMPDetail_Animal.csv",h=T)%>% 
#select(Geography,BMP,Unit,Sector,AmountCredited)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#WIP_14=rbind(WIP3v14BMPDetail_Animal,WIP3v14BMPDetail_Land)
#WIP_14=WIP_14 %>% mutate(Progress="WIP3v14")

#WIP_7=rbind(WIP3v7BMPDetail_Animal,WIP3v7BMPDetail_Land)
#WIP_7=WIP_7 %>% mutate(Progress="WIP3v7")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
WIP_2017=rbind(W2017BMPDetail_Animal,W2017BMPDetail_Land)
WIP_2017=WIP_2017 %>% mutate(Progress="V2017")

Initial=rbind(Land_Initial,Animal_Initial)
Initial=Initial %>% mutate(Progress="Initial")

April=rbind(Land_April,Animal_April)
April=April %>% mutate(Progress="April")

May=rbind(Land_May,Animal_May)
May=May %>% mutate(Progress="May")



WIP_BMPs=rbind(May,April,Initial,WIP_2017)
### Outliers



####-----------------------------------------------------------------------------
#> nrow(WIP_BMPs)
#[1] 8971

WIP_no_covercrop=WIP_BMPs %>%
filter(!str_detect(BMP,"Cover Crop"))
# nrow(WIP_no_covercrop) 8197


TRY=WIP_BMPs %>%filter(str_detect(BMP, "Cover Crop"))
#nrow=774 

wip_covercrop= TRY %>%  mutate(BMP = case_when(
    str_detect(BMP, "Late") ~ "Cover Crop (Late)",
    str_detect(BMP, "Early") ~ "Cover Crop (Early)",
    str_detect(BMP, "Commodity") ~ "Cover Crop (Commodity)",
    TRUE                   ~ "Cover Crop Traditional"
  )) 

WIP_MOd=rbind(wip_covercrop,WIP_no_covercrop)
#nrow 8971

#For pasture Fence, lets combine the rotational grazing, alternate water and pasture fence into one practice, call it 
#"Livestock Stream Exclusion and Pasture Management".  That should bump it up the list
#
#

WIP_MOd_sum2=WIP_MOd %>%
filter(!str_detect(BMP, paste(c("Upland Acres","Nutrient Management Core P","Nutrient Management P Rate","Nutrient Management P Timing",
                                "Nutrient Management N Rate","Nutrient Management N Timing","Nutrient Management N Placement"),collapse="|")))%>%

mutate(BMP=ifelse(Sector=="Agriculture" & str_detect(BMP,"Nutrient Management Core N"),"Agriculture Nutrient Management Core", BMP))%>%
mutate(BMP=ifelse(Sector=="Agriculture" & str_detect(BMP,"Nutrient Management P Placement"),"Agriculture Nutrient Management Enhanced", BMP))%>%
mutate(BMP=ifelse(!str_detect(BMP,"Upland Acres") & str_detect(BMP,paste(c("Exclusion Fencing","Off Stream Watering","Rotational/Perscribed Grazing"),
collapse="|")),"Livestock Stream Exclusion and Pasture Management",BMP))%>%
mutate(BMP=ifelse(Sector=="Agriculture" & str_detect(BMP,"Tillage Management"),"Tillage Management", BMP))%>%
mutate(BMP=ifelse(str_detect(BMP,"Stream Restoration"),"Stream Restoration", BMP))%>%
mutate(BMP=ifelse(str_detect(BMP,"Shoreline Management"),"Shoreline Management", BMP))%>%
mutate(BMP=ifelse(str_detect(BMP,"Shoreline Erosion Control"),"Shoreline Management", BMP))%>%
mutate(BMP=ifelse(str_detect(BMP,"Nutrient Management Plan"),"Urban Nutrient Management Plan", BMP))%>%
mutate(Sector,Sector=plyr::revalue(Sector,c("Developed" ="Developed(Non-MS4)")))%>%

#mutate(Sector=ifelse(Sector=="Developed" & str_detect(Sector,"Developed"),"Developed(Non-MS4)", Sector))%>%
group_by(BMP,Geography,Progress)%>%
mutate(AmountCredited=sum(AmountCredited))%>%
distinct()
#distinct(AmountCredited, .keep_all = TRUE)


 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~QA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
WIP_BMPs_Raw=WIP_BMPs%>%
  group_by(BMP,Geography,Progress)%>%
  mutate(AmountCredited=sum(AmountCredited))%>%
  distinct() %>%
  spread(Progress,AmountCredited,fill=0)%>%
  mutate(V2017=sum(V2017),May=sum(May),Initial=sum(Initial))%>%
  mutate(Flag=ifelse(V2017 > May | V2017 > Initial,"Flag",""))%>%
  mutate(Flag=ifelse(Initial > May,"Flag 2",Flag))%>%  
  distinct(V2017,Initial,May,.keep_all = TRUE)

WIP_BMP_Modified=WIP_MOd_sum2 %>%
  #filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  group_by(BMP) %>%spread(Progress,AmountCredited,fill=0)%>%
  mutate(V2017=sum(V2017),May=sum(May),Initial=sum(Initial))%>%
  mutate(Flag=ifelse(V2017 > May | V2017 > Initial,"Flag",""))%>%
  mutate(Flag=ifelse(Initial > May,"Flag 2",Flag))%>%  
  distinct(V2017,Initial,May,.keep_all = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#  #         #             #         #              #                  #                           #

#

#       #         #     BMP          #                #                       #                           #

#



###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Bay wide~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# WIP_MOd_sum 

WIP_BMP_baywide=WIP_MOd_sum2 %>%
#filter(!Progress=="April") %>%
filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
spread(Progress,AmountCredited,fill=0)%>%
group_by(BMP) %>%
mutate(V2017=sum(V2017),May=sum(May),Initial=sum(Initial))%>%
distinct(May,V2017,Initial,.keep_all = TRUE)

#labelbay=WIP_BMP_baywide %>% filter(BMP=="Agriculture Nutrient Management Core")
labelbay=WIP_BMP_baywide %>% filter(BMP=="Stream Restoration")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$




All_BMP36_after_Shore= WIP_BMP_baywide %>%
filter(str_detect(BMP,"Shoreline Management"))%>%
filter(May > 85000) %>%
 ggplot()+ 
geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
               x=0,size=1,color='grey20',alpha=0.9)+
   geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
 geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
 ###geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+78000,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+378000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult =c(0.02,0.11)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1, "cm"))+
theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
theme(legend.position = "none")+
theme(plot.margin = unit(c(0.25, 0.25, 0, 1), "cm"))
  


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!most BMPS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(lemon)
library(cowplot)

All_BMP36_after= WIP_BMP_baywide %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(May > 85000) %>%
  ggplot()+ 
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
               x=0,size=1,color='grey20',alpha=0.9)+
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+78000,y=reorder(BMP,May,sum)))+
  #geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+100000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult = c(0.02,0.11)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.99)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1.3, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
  theme(legend.title=element_blank(),
        legend.margin=margin(c(2,6,2,6)))+
  theme(legend.position="top",
        legend.justification="left")+
 # theme(legend.position = "none")+
  theme(plot.margin = unit(c(0, 0.25, 1, 0.25), "cm"))+
 geom_label_repel(show.legend = FALSE,color="black",data=labelbay,size=6,aes(fill=NULL,x=V2017, y=BMP,label="2017"),
 nudge_y=0.9,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
 geom_label_repel(show.legend = FALSE,color="black",data=labelbay,size=6,aes(fill=NULL,x=Initial, y=BMP,label="WIP III Initial"),
 nudge_y=0.95,nudge_x=-220000,direction = "both",angle= 0,segment.size = 0.8)+
 geom_label_repel(show.legend = FALSE,color="black",data=labelbay,size=6,aes(fill=NULL,x=May, y=BMP,label="WIP III Draft"),
nudge_y=1,nudge_x=30000,angle= 0,segment.size = 0.8)+
 #scale_y_discrete(position="top",expand =expand_scale(mult=(c(0.05,0.18))))
scale_y_discrete(expand =expand_scale(add=c(0.5,1.1)))


a=plot_grid(All_BMP36_after_Shore, reposition_legend(All_BMP36_after+ theme(legend.position='bottom'),"bottom",offset=c(-0.015,0.03)),
align = "v",nrow=2,rel_heights = c(0.9, 5),rel_widths = 1.1)

#margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")


ggsave("a.png", width = 41, height = 27, units = "cm",dpi = 600)



#  #         #             #         #              #                  #                           #

#

#       #         #     Each BAsin          #                #                       #                           #

##  #         #             #         #              #                  #                           #

#

#       #         #     Each BAsin          #                #                       #                           #

##  #         #             #         #              #                  #                           #

#

#       #         #     Each BAsin          #                #                       #                           #

##  #         #             #         #              #                  #                           #

#

#       #         #     Each BAsin          #                #                       #                           #

#

#Rapp~~~~~~~~~~~~~~~  

labelRap=WIP_MOd_sum2 %>% 
  filter(BMP=="Soil Conservation and Water Quality Plans" & str_detect(Geography, 'Rappahannock'))


rapp38_no= WIP_MOd_sum2 %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  spread(Progress,AmountCredited,fill=0)%>%
  filter(May > 15200) %>%
  filter(str_detect(Geography, 'Rappahannock')) %>%
  filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  mutate(Min=pmin(Initial,May,V2017)) %>%
  ggplot()+
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Min),
               x=0,size=1,color='grey20',alpha=0.9)+
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+15000,y=reorder(BMP,May,sum)))+
  #geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+100000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult = c(0.02,0.11)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.99)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1.3, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
  theme(legend.title=element_blank(),
        legend.margin=margin(c(2,6,2,6)))+
  theme(legend.position="top",
        legend.justification="left")+
  # theme(legend.position = "none")+
  ##theme(plot.margin = unit(c(0, 0.25, 1, 0.25), "cm"))+
  geom_label_repel(show.legend = FALSE,color="black",data=labelRap[3,],size=6,aes(fill=NULL,x=0, y=BMP,label="2017"),
                   nudge_y=0.3,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labelRap[3,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Initial"),
                   nudge_y=0.3,nudge_x=-30000,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labelRap[1,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Draft"),
                   nudge_y=0.8,nudge_x=100,angle= 0,segment.size = 0.8)+
  #scale_y_discrete(position="top",expand =expand_scale(mult=(c(0.05,0.18))))
  scale_y_discrete(expand =expand_scale(add=c(0.5,1)))

####rapp shoreline

rapp_Shore= WIP_MOd_sum2 %>%
  filter(str_detect(BMP,"Shoreline Management"))%>%
  filter(str_detect(Geography, 'Rappahannock') & str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  spread(Progress,AmountCredited,fill=NA)%>%
  ggplot()+ 
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
               x=0,size=1,color='grey20',alpha=0.9)+
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  ###geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+78000,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+55000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult =c(0.02,0.11)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0.25, 0.25, 0, 1), "cm"))




rapp=plot_grid(rapp_Shore, reposition_legend(rapp38_no + theme(legend.position='bottom'),"bottom",offset=c(-0.015,0.03)),
            align = "v",nrow=2,rel_heights = c(0.9, 5),rel_widths = 1.1)

#margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")


ggsave("rapp_38.png", width = 41, height = 27, units = "cm",dpi = 600)





###########POTO ##############


labelpoto=WIP_MOd_sum2 %>% 
  filter(BMP=="Stream Restoration" & str_detect(Geography, 'Potomac'))


poto38_no= WIP_MOd_sum2 %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  spread(Progress,AmountCredited,fill=0)%>%
  filter(May > 52000) %>%
  filter(str_detect(Geography, 'Potomac')) %>%
  filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  mutate(Min=pmin(Initial,May,V2017)) %>%
  ggplot()+
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Min),
               x=0,size=1,color='grey20',alpha=0.9)+
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+15000,y=reorder(BMP,May,sum)))+
  #geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+100000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult = c(0.02,0.09)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.99)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1.3, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
  theme(legend.title=element_blank(),
        legend.margin=margin(c(2,6,2,6)))+
  theme(legend.position="top",
        legend.justification="left")+
  # theme(legend.position = "none")+
  ##theme(plot.margin = unit(c(0, 0.25, 1, 0.25), "cm"))+
  geom_label_repel(show.legend = FALSE,color="black",data=labelpoto[4,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
                   nudge_y=0.7,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labelpoto[3,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Initial"),
                   nudge_y=0.4,nudge_x=-35000,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labelpoto[1,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Draft"),
                   nudge_y=0.9,nudge_x=20000,angle= 0,segment.size = 0.8)+
  #scale_y_discrete(position="top",expand =expand_scale(mult=(c(0.05,0.18))))
  scale_y_discrete(expand =expand_scale(add=c(0.5,1.2)))



#########

poto_Shore= WIP_MOd_sum2 %>%
  filter(str_detect(BMP,"Shoreline Management"))%>%
  spread(Progress,AmountCredited,fill=0)%>%
  filter(May > 50000) %>%
  filter(str_detect(Geography, 'Potomac')) %>%
  filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  mutate(Min=pmin(Initial,May,V2017)) %>%
  ggplot()+
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
               x=0,size=1,color='grey20',alpha=0.9)+
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  ###geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+78000,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+55000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult =c(0.02,0.09)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0.25, 0.25, 0, 1), "cm"))




poto=plot_grid(poto_Shore, reposition_legend(poto38_no + theme(legend.position='bottom'),"bottom",offset=c(-0.015,0.03)),
               align = "v",nrow=2,rel_heights = c(0.9, 5),rel_widths = 1.1)

#margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")


ggsave("Poto_38.png", width = 41, height = 27, units = "cm",dpi = 600)





################################################ York#######################



labelyork=WIP_MOd_sum2 %>% 
  filter(BMP=="Soil Conservation and Water Quality Plans" & str_detect(Geography, 'York'))


york38_no= WIP_MOd_sum2 %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  spread(Progress,AmountCredited,fill=0)%>%
  filter(May > 16900) %>%
  filter(str_detect(Geography, 'York')) %>%
  filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  mutate(Min=pmin(Initial,May,V2017)) %>%
  ggplot()+
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Min),
               x=0,size=1,color='grey20',alpha=0.9)+
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+8000,y=reorder(BMP,May,sum)))+
  #geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+100000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult = c(0.02,0.1)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.99)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1.3, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
  theme(legend.title=element_blank(),
        legend.margin=margin(c(2,6,2,6)))+
  theme(legend.position="top",
        legend.justification="left")+
  # theme(legend.position = "none")+
  ##theme(plot.margin = unit(c(0, 0.25, 1, 0.25), "cm"))+
  geom_label_repel(show.legend = FALSE,color="black",data=labelyork[3,],size=6,aes(fill=NULL,x=0, y=BMP,label="2017"),
                   nudge_y=0.4,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labelyork[3,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Initial"),
                   nudge_y=1.1,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labelyork[1,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Draft"),
                   nudge_y=0.38,nudge_x=0,angle= 0,segment.size = 0.8)+
  #scale_y_discrete(position="top",expand =expand_scale(mult=(c(0.05,0.18))))
  scale_y_discrete(expand =expand_scale(add=c(0.5,1.2)))

####rapp shoreline

york_Shore= WIP_MOd_sum2 %>%
  filter(str_detect(BMP,"Shoreline Management"))%>%
  spread(Progress,AmountCredited,fill=0)%>%
  filter(May > 15000) %>%
  filter(str_detect(Geography, 'York')) %>%
  filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  mutate(Min=pmin(Initial,May,V2017)) %>%
  ggplot()+
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
               x=0,size=1,color='grey20',alpha=0.9)+
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  ###geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+78000,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+100000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult =c(0.02,0.08)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0.25, 0.25, 0, 1), "cm"))




york=plot_grid(york_Shore, reposition_legend(york38_no + theme(legend.position='bottom'),"bottom",offset=c(-0.015,0.03)),
               align = "v",nrow=2,rel_heights = c(0.9, 5),rel_widths = 1.1)

ggsave("York_38.png", width = 41, height = 27, units = "cm",dpi = 600)







############## James###################





labeljames=WIP_MOd_sum2 %>% 
  filter(BMP=="Stream Restoration" & str_detect(Geography, 'James'))


james38_no= WIP_MOd_sum2 %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  spread(Progress,AmountCredited,fill=0)%>%
  filter(May > 63000) %>%
  filter(str_detect(Geography, 'James')) %>%
  filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  mutate(Min=pmin(Initial,May,V2017)) %>%
  ggplot()+
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Min),
               x=0,size=1,color='grey20',alpha=0.9)+
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+15000,y=reorder(BMP,May,sum)))+
  #geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+100000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult = c(0.02,0.09)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.99)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1.3, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
  theme(legend.title=element_blank(),
        legend.margin=margin(c(2,6,2,6)))+
  theme(legend.position="top",
        legend.justification="left")+
  # theme(legend.position = "none")+
  ##theme(plot.margin = unit(c(0, 0.25, 1, 0.25), "cm"))+
  geom_label_repel(show.legend = FALSE,color="black",data=labeljames[4,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
                   nudge_y=0.7,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labeljames[3,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Initial"),
                   nudge_y=0.4,nudge_x=-45000,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labeljames[1,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Draft"),
                   nudge_y=0.9,nudge_x=25000,angle= 0,segment.size = 0.8)+
  #scale_y_discrete(position="top",expand =expand_scale(mult=(c(0.05,0.18))))
  scale_y_discrete(expand =expand_scale(add=c(0.5,1.2)))

####rapp shoreline

james_Shore= WIP_MOd_sum2 %>%
  filter(str_detect(BMP,"Shoreline Management"))%>%
  spread(Progress,AmountCredited,fill=0)%>%
  filter(May > 15000) %>%
  filter(str_detect(Geography, 'James')) %>%
  filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  mutate(Min=pmin(Initial,May,V2017)) %>%
  ggplot()+
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
               x=0,size=1,color='grey20',alpha=0.9)+
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  ###geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+78000,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+55000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult =c(0.02,0.09)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0.25, 0.25, 0, 1), "cm"))




james=plot_grid(james_Shore, reposition_legend(james38_no + theme(legend.position='bottom'),"bottom",offset=c(-0.015,0.03)),
                align = "v",nrow=2,rel_heights = c(0.9, 5),rel_widths = 1.1)

ggsave("James_38.png", width = 41, height = 27, units = "cm",dpi = 600)




###################### Eastern Shore

labeles=WIP_MOd_sum2 %>% 
  filter(BMP=="Agriculture Nutrient Management Core" & str_detect(Geography, 'Eastern Shore'))


es38_no= WIP_MOd_sum2 %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  spread(Progress,AmountCredited,fill=0)%>%
  filter(May > 3371) %>%
  filter(str_detect(Geography, 'Eastern Shore')) %>%
  filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  mutate(Min=pmin(Initial,May,V2017)) %>%
  ggplot()+
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Min),
               x=0,size=1,color='grey20',alpha=0.9)+
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+15000,y=reorder(BMP,May,sum)))+
  #geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+100000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult = c(0.02,0.09)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.99)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1.3, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
  theme(legend.title=element_blank(),
        legend.margin=margin(c(2,6,2,6)))+
  theme(legend.position="top",
        legend.justification="left")+
  # theme(legend.position = "none")+
  ##theme(plot.margin = unit(c(0, 0.25, 1, 0.25), "cm"))+
  geom_label_repel(show.legend = FALSE,color="black",data=labeles[4,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
                   nudge_y=0.7,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labeles[3,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Initial"),
                   nudge_y=0.4,nudge_x=5000,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labeles[1,],size=6,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Draft"),
                   nudge_y=0.5,nudge_x=0,angle= 0,segment.size = 0.8)+
  #scale_y_discrete(position="top",expand =expand_scale(mult=(c(0.05,0.18))))
  scale_y_discrete(expand =expand_scale(add=c(0.5,1.2)))

####rapp shoreline

es_Shore= WIP_MOd_sum2 %>%
  filter(str_detect(BMP,"Shoreline Management"))%>%
  spread(Progress,AmountCredited,fill=0)%>%
  filter(May > 15000) %>%
  filter(str_detect(Geography, 'Eastern Shore')) %>%
  filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  mutate(Min=pmin(Initial,May,V2017)) %>%
  ggplot()+
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
               x=0,size=1,color='grey20',alpha=0.9)+
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  ###geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+78000,y=reorder(BMP,May,sum)))+
  geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+55000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult =c(0.02,0.09)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.5)))+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0.25, 0.25, 0, 1), "cm"))




es=plot_grid(es_Shore, reposition_legend(es38_no + theme(legend.position='bottom'),"bottom",offset=c(-0.015,0.03)),
                align = "v",nrow=2,rel_heights = c(0.9, 5),rel_widths = 1.1)

ggsave("EasternShore_38.png", width = 41, height = 27, units = "cm",dpi = 600)









#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~EXTRA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~EXTRA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~EXTRA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~EXTRA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~EXTRA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~EXTRA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~EXTRA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~EXTRA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~EXTRA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




labelRap=WIP_MOd_sum2 %>% 
filter(BMP=="Agriculture Nutrient Management" & str_detect(Geography, 'Rappahannock'))
  

WIP.Rappahannock_36=WIP_MOd_sum2 %>% 
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(str_detect(Geography, 'Rappahannock') & str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  spread(Progress,AmountCredited,fill=NA)%>%
  #group_by(BMPs) %>%
  #mutate(WIP3=sum(WIP3),Progress_2017=sum(Progress_2017),Unit=tolower(Unit))%>%
  #distinct(WIP3, .keep_all = TRUE)%>%
  #ungroup()%>%
  filter(May > 18000) %>%
  #top_n(10,wt=May)%>%
  #arrange(desc(WIP3))%>%
  #Potomac_2017 %>% 
  ggplot()+ 
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
  x=0,size=1,color='grey20',alpha=0.9)+
 
 geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
 geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  #geom_point(pch=124,size=7,color="black",aes(x=V2017,y=reorder(BMP,May,sum)))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  #pch=3,color="black",
  
  geom_text(color="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+22000,y=reorder(BMP,May,sum)))+
  #geom_linerange(size=5,aes(colour=alt_Sector),position = position_dodge(.5))+ 
  #geom_point(size=6,aes(x=Progress_2017,y=reorder(alt_BMPs,WIP3)))
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult = c(0.02,0.2)),labels = scales::comma)+
  #scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  #facet_wrap(~Sector, scales = "free",ncol=2)+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
  scale_colour_manual(name = 'Sector', 
                      values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  #scale_shape_manual(values=124) + 
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1, "cm"))+
  #theme(legend.position = "top")+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.3)),
        axis.text.y = element_text(size=rel(1.3)))+
    geom_label_repel(show.legend = FALSE,color="black",data=labelRap[1,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Draft"),
                     nudge_y=0.4,nudge_x=13000,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labelRap[3,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP III Initial"),
                   nudge_y=1.4,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labelRap[4,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
                   nudge_y=0.3,nudge_x=-18000,direction = "both",angle= 0,segment.size = 0.8)+
  scale_y_discrete(position="top",expand = expand_scale(mult=(c(0.05,0.11))))  
  
  

  ggsave("WIP.Rappahannock_36.png", width = 40, height = 20, units = "cm",dpi = 600)


#______________________Potomac
  
  labelPoto=WIP_MOd_sum2 %>% 
  filter(BMP=="Agriculture Nutrient Management" & str_detect(Geography, 'Potomac'))
  
  
  WIP.Potomac_36=WIP_MOd_sum2 %>% 
    
    filter(str_detect(Geography, 'Potomac') & str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
    spread(Progress,AmountCredited,fill=NA)%>%
    #group_by(BMPs) %>%
    #mutate(WIP3=sum(WIP3),Progress_2017=sum(Progress_2017),Unit=tolower(Unit))%>%
    #distinct(WIP3, .keep_all = TRUE)%>%
    #ungroup()%>%
    filter(May > 50000) %>%
    #arrange(desc(May)) %>%
    #filter(row_number() <= 10L) %>%
    #arrange(desc(WIP3))%>%
    #Potomac_2017 %>% 
    ggplot()+ 
    geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
                 x=0,size=1,color='grey20',alpha=0.9)+
    
    geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
    geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
    #geom_point(pch=124,size=7,color="black",aes(x=V2017,y=reorder(BMP,May,sum)))+
    geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
    geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
    #pch=3,color="black",
    
    geom_text(color="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+22000,y=reorder(BMP,May,sum)))+
    #geom_linerange(size=5,aes(colour=alt_Sector),position = position_dodge(.5))+ 
    #geom_point(size=6,aes(x=Progress_2017,y=reorder(alt_BMPs,WIP3)))
    labs(x=NULL, y=NULL, title="") +
    theme_ipsum_rc(grid="X") +
    scale_x_continuous(expand =expand_scale(mult = c(0.02,0.2)),labels = scales::comma)+
    #scale_x_continuous(labels = scales::comma)+
    theme(panel.grid.major.x=element_line(size=0.7))+
    #facet_wrap(~Sector, scales = "free",ncol=2)+
    theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
    scale_colour_manual(name = 'Sector', 
                        values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                  "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
    #scale_shape_manual(values=124) + 
    theme(legend.position = c(0.60, 0.42))+
    theme(legend.key.size = unit(1, "cm"))+
    #theme(legend.position = "top")+
    theme(legend.text = element_text(colour="black", size=12))+
    theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
    theme(axis.text.x = element_text(size=rel(1.3)),
          axis.text.y = element_text(size=rel(1.3)))+
    geom_label_repel(show.legend = FALSE,color="black",data=labelPoto[1,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP3 April"),
                     nudge_y=0.5,nudge_x=10000,direction = "both",angle= 0,segment.size = 0.8)+
    geom_label_repel(show.legend = FALSE,color="black",data=labelPoto[3,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP3 Initial"),
                     nudge_y=0.8,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
    geom_label_repel(show.legend = FALSE,color="black",data=labelPoto[4,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
                     nudge_y=0.4,nudge_x=-40000,direction = "both",angle= 0,segment.size = 0.8)+
    scale_y_discrete(position="top",expand =expand_scale(mult=(c(0.05,0.07))))
  
  
  
  ggsave("WIP.Potomac_36.png", width = 35, height = 25, units = "cm",dpi = 600)

################################################################################
  
#_______________________York
  
###########################  
  
  
  labelYork=WIP_MOd_sum2 %>% 
  filter(BMP=="Agriculture Nutrient Management" & str_detect(Geography, 'York'))
  
  
  WIP.York_36=WIP_MOd_sum2 %>% 
    
    filter(str_detect(Geography, 'York') & str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
    spread(Progress,AmountCredited,fill=NA)%>%
    #group_by(BMPs) %>%
    #mutate(WIP3=sum(WIP3),Progress_2017=sum(Progress_2017),Unit=tolower(Unit))%>%
    #distinct(WIP3, .keep_all = TRUE)%>%
    #ungroup()%>%
    filter(May > 17200) %>%
    #arrange(desc(May)) %>%
    #filter(row_number() <= 10L) %>%
    #arrange(desc(WIP3))%>%
    #Potomac_2017 %>% 
    ggplot()+ 
    geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
                 x=0,size=1,color='grey20',alpha=0.9)+
    
    geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
    geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
    #geom_point(pch=124,size=7,color="black",aes(x=V2017,y=reorder(BMP,May,sum)))+
    geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
    geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
    #pch=3,color="black",
    
    geom_text(color="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(Initial)+22000,y=reorder(BMP,May,sum)))+
    #geom_linerange(size=5,aes(colour=alt_Sector),position = position_dodge(.5))+ 
    #geom_point(size=6,aes(x=Progress_2017,y=reorder(alt_BMPs,WIP3)))
    labs(x=NULL, y=NULL, title="") +
    theme_ipsum_rc(grid="X") +
    scale_x_continuous(expand =expand_scale(mult = c(0.02,0.05)),labels = scales::comma)+
    #scale_x_continuous(labels = scales::comma)+
    theme(panel.grid.major.x=element_line(size=0.7))+
    #facet_wrap(~Sector, scales = "free",ncol=2)+
    theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
    scale_colour_manual(name = 'Sector', 
                        values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                  "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
    #scale_shape_manual(values=124) + 
    theme(legend.position = c(0.60, 0.42))+
    theme(legend.key.size = unit(1, "cm"))+
    #theme(legend.position = "top")+
    theme(legend.text = element_text(colour="black", size=12))+
    theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
    theme(axis.text.x = element_text(size=rel(1.3)),
          axis.text.y = element_text(size=rel(1.3)))+
    geom_label_repel(show.legend = FALSE,color="black",data=labelYork[3,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP3 Initial"),
                     nudge_y=0.5,nudge_x=10000,direction = "both",angle= 0,segment.size = 0.8)+
    geom_label_repel(show.legend = FALSE,color="black",data=labelYork[1,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP3 May"),
                     nudge_y=0.8,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
    geom_label_repel(show.legend = FALSE,color="black",data=labelYork[4,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
                     nudge_y=0.4,nudge_x=-10000,direction = "both",angle= 0,segment.size = 0.8)+
    scale_y_discrete(position="top",expand = expand_scale(mult=(c(0.05,0.07))))
  
  
  
  ggsave("WIP.York_36.png", width = 35, height = 25, units = "cm",dpi = 600)


  ################################################################################
  
  #_______________________York
  
  ###########################  
  
  
  labelJames=WIP_MOd_sum2 %>% 
    filter(BMP=="Agriculture Nutrient Management" & str_detect(Geography, 'James'))
  
  
  WIP.James_31=WIP_MOd_sum2 %>% 
    
    filter(str_detect(Geography, 'James') & str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
    spread(Progress,AmountCredited,fill=NA)%>%
    #group_by(BMPs) %>%
    #mutate(WIP3=sum(WIP3),Progress_2017=sum(Progress_2017),Unit=tolower(Unit))%>%
    #distinct(WIP3, .keep_all = TRUE)%>%
    #ungroup()%>%
    filter(May > 62000) %>%
    #arrange(desc(May)) %>%
    #filter(row_number() <= 10L) %>%
    #arrange(desc(WIP3))%>%
    #Potomac_2017 %>% 
    ggplot()+ 
    geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
                 x=0,size=1,color='grey20',alpha=0.9)+
    
    geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
    geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
    #geom_point(pch=124,size=7,color="black",aes(x=V2017,y=reorder(BMP,May,sum)))+
    geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
    geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
    #pch=3,color="black",
    
    geom_text(color="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(Initial)+29000,y=reorder(BMP,May,sum)))+
    #geom_linerange(size=5,aes(colour=alt_Sector),position = position_dodge(.5))+ 
    #geom_point(size=6,aes(x=Progress_2017,y=reorder(alt_BMPs,WIP3)))
    labs(x=NULL, y=NULL, title="") +
    theme_ipsum_rc(grid="X") +
    scale_x_continuous(expand =expand_scale(mult = c(0.02,0.09)),labels = scales::comma)+
    #scale_x_continuous(labels = scales::comma)+
    theme(panel.grid.major.x=element_line(size=0.7))+
    #facet_wrap(~Sector, scales = "free",ncol=2)+
    theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
    scale_colour_manual(name = 'Sector', 
                        values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                  "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
    #scale_shape_manual(values=124) + 
    theme(legend.position = c(0.60, 0.42))+
    theme(legend.key.size = unit(1, "cm"))+
    #theme(legend.position = "top")+
    theme(legend.text = element_text(colour="black", size=12))+
    theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
    theme(axis.text.x = element_text(size=rel(1.3)),
          axis.text.y = element_text(size=rel(1.3)))+
    geom_label_repel(show.legend = FALSE,color="black",data=labelJames[3,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP3 Initial"),
                     nudge_y=0.5,nudge_x=10000,direction = "both",angle= 0,segment.size = 0.8)+
    geom_label_repel(show.legend = FALSE,color="black",data=labelJames[1,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP3 April"),
                     nudge_y=0.4,nudge_x=-60000,direction = "both",angle= 0,segment.size = 0.8)+
    geom_label_repel(show.legend = FALSE,color="black",data=labelJames[4,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
                     nudge_y=0.4,nudge_x=-10000,direction = "both",angle= 0,segment.size = 0.8)+
    scale_y_discrete(position="top",expand = expand_scale(mult=(c(0.05,0.07))))
  
  
  
  ggsave("WIP.James_31.png", width = 35, height = 25, units = "cm",dpi = 600)



#####################    #############ES
  
  labelES=WIP_MOd_sum2 %>% 
    filter(BMP=="Agriculture Nutrient Management" & str_detect(Geography, 'Eastern Shore'))
  
  
  WIP.ES_31=WIP_MOd_sum2 %>% 
    
    filter(str_detect(Geography, 'Eastern Shore') & str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
    spread(Progress,AmountCredited,fill=NA)%>%
    #group_by(BMPs) %>%
    #mutate(WIP3=sum(WIP3),Progress_2017=sum(Progress_2017),Unit=tolower(Unit))%>%
    #distinct(WIP3, .keep_all = TRUE)%>%
    #ungroup()%>%
    filter(May > 3840) %>%
    #arrange(desc(May)) %>%
    #filter(row_number() <= 10L) %>%
    #arrange(desc(WIP3))%>%
    #Potomac_2017 %>% 
    ggplot()+ 
    geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
                 x=0,size=1,color='grey20',alpha=0.9)+
    
    geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
    geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
    #geom_point(pch=124,size=7,color="black",aes(x=V2017,y=reorder(BMP,May,sum)))+
    geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
    geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
    #pch=3,color="black",
    
    geom_text(color="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(Initial)+7000,y=reorder(BMP,May,sum)))+
    #geom_linerange(size=5,aes(colour=alt_Sector),position = position_dodge(.5))+ 
    #geom_point(size=6,aes(x=Progress_2017,y=reorder(alt_BMPs,WIP3)))
    labs(x=NULL, y=NULL, title="") +
    theme_ipsum_rc(grid="X") +
    scale_x_continuous(expand =expand_scale(mult = c(0.02,0.07)),labels = scales::comma)+
    #scale_x_continuous(labels = scales::comma)+
    theme(panel.grid.major.x=element_line(size=0.7))+
    #facet_wrap(~Sector, scales = "free",ncol=2)+
    theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
    scale_colour_manual(name = 'Sector', 
                        values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                  "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
    #scale_shape_manual(values=124) + 
    theme(legend.position = c(0.60, 0.42))+
    theme(legend.key.size = unit(1, "cm"))+
    #theme(legend.position = "top")+
    theme(legend.text = element_text(colour="black", size=12))+
    theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
    theme(axis.text.x = element_text(size=rel(1.3)),
          axis.text.y = element_text(size=rel(1.3)))+
    geom_label_repel(show.legend = FALSE,color="black",data=labelES[3,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP3 Initial"),
                     nudge_y=0.5,nudge_x=5000,direction = "both",angle= 0,segment.size = 0.8)+
    geom_label_repel(show.legend = FALSE,color="black",data=labelES[1,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="WIP3 April"),
                     nudge_y=0.8,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
    geom_label_repel(show.legend = FALSE,color="black",data=labelES[4,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
                     nudge_y=0.4,nudge_x=-5000,direction = "both",angle= 0,segment.size = 0.8)+
    scale_y_discrete(position="top",expand = expand_scale(mult=(c(0.05,0.07))))
  
  
  
  ggsave("WIP.ES_31.png", width = 35, height = 25, units = "cm",dpi = 600)
  







  
  
  
ggsave("WIP_BMP_Potomac.png", width = 35, height = 25, units = "cm",dpi = 600)

ggsave("WIP_BMP_York.png", width = 35, height = 25, units = "cm",dpi = 600)

          geom_label_repel(force=10,show.legend = FALSE,color="black",data=label[3,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
nudge_y=0.3,nudge_x=-17000,direction = "both",angle= 0, vjust =0.08,segment.size = 0.8)+
geom_label_repel(force=10,show.legend = FALSE,color="black",data=label[1,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="Initial WIP3"),
nudge_x=25,nudge_y=1,direction = "both",angle= 0, vjust =0.8,segment.size = 0.8)+
geom_label_repel(force=10,show.legend = FALSE,color="black",data=label[2,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="April WIP3"),
nudge_y=0.3,direction = "both",angle= 0, vjust =0.08,segment.size = 0.8)+
#####York

label=WIP_MOd_sum %>% filter(str_detect(Geography, 'York') & BMP=="Agriculture Nutrient Management")


WIP_BMP_York=WIP_MOd_sum %>% 
  filter(str_detect(Geography, 'York') & str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  
  spread(Progress,AmountCredited,fill=NA)%>%
  #group_by(BMPs) %>%
  #mutate(WIP3=sum(WIP3),Progress_2017=sum(Progress_2017),Unit=tolower(Unit))%>%
  #distinct(WIP3, .keep_all = TRUE)%>%
  #ungroup()%>%
  filter(WIP3v14 > 35000) %>%
  #arrange(desc(WIP3))%>%
  #Potomac_2017 %>% 
  ggplot()+ 
  geom_segment(linetype="dotted",aes(y=reorder(BMP,WIP3v14,sum), yend=reorder(BMP,WIP3v14,sum),xend=WIP3v7),
               x=0,size=1,color='grey20',alpha=0.9)+
  
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,WIP3v14,sum),yend=reorder(BMP,WIP3v14,sum),x= WIP3v7,xend =WIP3v14 ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,WIP3v14,sum),yend=reorder(BMP,WIP3v14,sum),x= V2017,xend = WIP3v7))+
  #geom_point(pch=124,size=7,color="black",aes(x=V2017,y=reorder(BMP,WIP3v14,sum)))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=WIP3v14,y=reorder(BMP,WIP3v14,sum)))+
  geom_point(size=8,aes(color=Sector,x=WIP3v7,y=reorder(BMP,WIP3v14,sum)))+
  #pch=3,color="black",
  
  geom_text(show.legend = FALSE,alpha=0.7,size=4,aes(color=Sector,label=Unit,x=max(WIP3v7)+22000,y=reorder(BMP,WIP3v14,sum)))+
  #geom_linerange(size=5,aes(colour=alt_Sector),position = position_dodge(.5))+ 
  #geom_point(size=6,aes(x=Progress_2017,y=reorder(alt_BMPs,WIP3)))
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult = c(0.02,0.2)),labels = scales::comma)+
  #scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  #facet_wrap(~Sector, scales = "free",ncol=2)+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
  scale_colour_manual(name = 'Sector', 
                      values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  #scale_shape_manual(values=124) + 
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1, "cm"))+
  #theme(legend.position = "top")+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.3)),
        axis.text.y = element_text(size=rel(1.3)))+
  
  geom_label_repel(force=10,show.legend = FALSE,color="black",data=label[3,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
                   nudge_y=0.3,nudge_x=-17000,direction = "both",angle= 0, vjust =0.08,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=label[1,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="Initial WIP3"),
                   nudge_x=0,nudge_y=1,direction = "both",angle= 0, vjust =0.8,segment.size = 0.8)+
  geom_label_repel(force=10,show.legend = FALSE,color="black",data=label[2,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="April WIP3"),
                   nudge_y=0.3,direction = "both",angle= 0, vjust =0.08,segment.size = 0.8)+
  scale_y_discrete(expand = expand_scale(add = 0.5))



#################################################

ggsave("WIP_BMP_James.png", width = 35, height = 25, units = "cm",dpi = 600)

#####York

label=WIP_MOd_sum %>% filter(str_detect(Geography, 'James') & BMP=="Agriculture Nutrient Management")


WIP_BMP_James=WIP_MOd_sum %>% 
  filter(str_detect(Geography, 'James') & str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  
  spread(Progress,AmountCredited,fill=NA)%>%
  #group_by(BMPs) %>%
  #mutate(WIP3=sum(WIP3),Progress_2017=sum(Progress_2017),Unit=tolower(Unit))%>%
  #distinct(WIP3, .keep_all = TRUE)%>%
  #ungroup()%>%
  filter(WIP3v14 > 35000) %>%
  #arrange(desc(WIP3))%>%
  #Potomac_2017 %>% 
  ggplot()+ 
  geom_segment(linetype="dotted",aes(y=reorder(BMP,WIP3v14,sum), yend=reorder(BMP,WIP3v14,sum),xend=WIP3v7),
               x=0,size=1,color='grey20',alpha=0.9)+
  
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,WIP3v14,sum),yend=reorder(BMP,WIP3v14,sum),x= WIP3v7,xend =WIP3v14 ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,WIP3v14,sum),yend=reorder(BMP,WIP3v14,sum),x= V2017,xend = WIP3v7))+
  #geom_point(pch=124,size=7,color="black",aes(x=V2017,y=reorder(BMP,WIP3v14,sum)))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=WIP3v14,y=reorder(BMP,WIP3v14,sum)))+
  geom_point(size=8,aes(color=Sector,x=WIP3v7,y=reorder(BMP,WIP3v14,sum)))+
  #pch=3,color="black",
  
  geom_text(show.legend = FALSE,alpha=0.7,size=4,aes(color=Sector,label=Unit,x=max(WIP3v7)+22000,y=reorder(BMP,WIP3v14,sum)))+
  #geom_linerange(size=5,aes(colour=alt_Sector),position = position_dodge(.5))+ 
  #geom_point(size=6,aes(x=Progress_2017,y=reorder(alt_BMPs,WIP3)))
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult = c(0.02,0.2)),labels = scales::comma)+
  #scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  #facet_wrap(~Sector, scales = "free",ncol=2)+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
  scale_colour_manual(name = 'Sector', 
                      values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  #scale_shape_manual(values=124) + 
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1, "cm"))+
  #theme(legend.position = "top")+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.3)),
        axis.text.y = element_text(size=rel(1.3)))+
  
  geom_label_repel(force=10,show.legend = FALSE,color="black",data=label[3,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
                   nudge_y=0.3,nudge_x=-17000,direction = "both",angle= 0, vjust =0.08,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=label[1,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="Initial WIP3"),
                   nudge_x=0,nudge_y=1,direction = "both",angle= 0, vjust =0.8,segment.size = 0.8)+
  geom_label_repel(force=10,show.legend = FALSE,color="black",data=label[2,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="April WIP3"),
                   nudge_y=0.3,direction = "both",angle= 0, vjust =0.08,segment.size = 0.8)+
  scale_y_discrete(expand = expand_scale(add = 0.5))



###################
ggsave("WIP_BMP_ES.png", width = 35, height = 25, units = "cm",dpi = 600)

label=WIP_MOd_sum %>% filter(str_detect(Geography, 'Eastern') & BMP=="Agriculture Nutrient Management")


WIP_BMP_ES=WIP_MOd_sum %>% 
  filter(str_detect(Geography, 'Eastern') & str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  
  spread(Progress,AmountCredited,fill=NA)%>%
  #group_by(BMPs) %>%
  #mutate(WIP3=sum(WIP3),Progress_2017=sum(Progress_2017),Unit=tolower(Unit))%>%
  #distinct(WIP3, .keep_all = TRUE)%>%
  #ungroup()%>%
  filter(WIP3v14 > 5000) %>%
  #arrange(desc(WIP3))%>%
  #Potomac_2017 %>% 
  ggplot()+ 
  geom_segment(linetype="dotted",aes(y=reorder(BMP,WIP3v14,sum), yend=reorder(BMP,WIP3v14,sum),xend=WIP3v7),
               x=0,size=1,color='grey20',alpha=0.9)+
  
  geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,WIP3v14,sum),yend=reorder(BMP,WIP3v14,sum),x= WIP3v7,xend =WIP3v14 ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,WIP3v14,sum),yend=reorder(BMP,WIP3v14,sum),x= V2017,xend = WIP3v7))+
  #geom_point(pch=124,size=7,color="black",aes(x=V2017,y=reorder(BMP,WIP3v14,sum)))+
  geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=WIP3v14,y=reorder(BMP,WIP3v14,sum)))+
  geom_point(size=8,aes(color=Sector,x=WIP3v7,y=reorder(BMP,WIP3v14,sum)))+
  #pch=3,color="black",
  
  geom_text(show.legend = FALSE,alpha=0.7,size=4,aes(color=Sector,label=Unit,x=max(WIP3v7)+22000,y=reorder(BMP,WIP3v14,sum)))+
  #geom_linerange(size=5,aes(colour=alt_Sector),position = position_dodge(.5))+ 
  #geom_point(size=6,aes(x=Progress_2017,y=reorder(alt_BMPs,WIP3)))
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult = c(0.02,0.2)),labels = scales::comma)+
  #scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  #facet_wrap(~Sector, scales = "free",ncol=2)+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
  scale_colour_manual(name = 'Sector', 
                      values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  #scale_shape_manual(values=124) + 
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1, "cm"))+
  #theme(legend.position = "top")+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(size=rel(1.3)),
        axis.text.y = element_text(size=rel(1.3)))+
  
  geom_label_repel(force=10,show.legend = FALSE,color="black",data=label[3,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="2017"),
                   nudge_y=0.3,nudge_x=-17000,direction = "both",angle= 0, vjust =0.08,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=label[1,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="Initial WIP3"),
                   nudge_x=0,nudge_y=1,direction = "both",angle= 0, vjust =0.8,segment.size = 0.8)+
  geom_label_repel(force=10,show.legend = FALSE,color="black",data=label[2,],size=5,aes(fill=NULL,x=AmountCredited, y=BMP,label="April WIP3"),
                   nudge_y=0.3,direction = "both",angle= 0, vjust =0.08,segment.size = 0.8)+
  scale_y_discrete(expand = expand_scale(add = 1.2))













+
  scale_linetype_manual(NULL,values =c("2017"=3))




ggsave("WIP_BMP_Potomac.png", width = 35, height = 15, units = "cm",dpi = 600)



    
    #geom_text(aes(label=scales::comma(round(WIP3)),y=reorder(alt_BMPs,WIP3), x=(WIP3 + Progress_2017)/2), size = 3,color = "black",show.legend = FALSE)
  geom_text(aes(label=scales::comma(round(WIP3)),y=reorder(BMP,AmountCredited,sum), x=(WIP3 + 20000)), size = 3,color = "black",show.legend = FALSE)



 











#Phos
p=long_loads %>% filter(Value > 1 & str_detect(Type, 'PLoadEOT')) %>% 
group_by(Geography,Sector,Year) %>%
summarise(Number= sum(Value)) 

phos_plot= p%>% mutate(Phos_Target=as.numeric(case_when(
  Geography=="Eastern Shore" ~ "0.164",
  Geography=="James" ~ "2.731",                        
  Geography=="Potomac" ~ "1.892",                             
  Geography=="Rappahannock" ~ "0.849",                              
  Geography=="York" ~ "0.556",
  TRUE                      ~ "other" 
  )
  ) 
) %>% mutate(Phos_Target=Phos_Target*1000000, Climate=Phos_Target - 38600 ) %>% 

ggplot(aes(x=Year,y=Number,fill=Sector)) + #geom_area(stat="identity")
geom_bar(stat="identity",width=0.7,alpha=0.9)+
scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
geom_text(aes(0, Phos_Target, label=scales::comma(Phos_Target), vjust=-1,hjust=-0.25),family="Times")+
geom_hline(aes(yintercept=Phos_Target,color = "WIP Target"), linetype="solid", size=1.5)+
geom_hline(aes(yintercept=Climate,color = "Climate Change Target"), linetype="dotted", size=1.5)+
#facet_wrap(~Geography, scales = "free_y",ncol=1)
scale_color_manual(values=c("red","black"))+
theme_dark()+
theme(legend.title=element_blank())+
scale_fill_brewer(type = 'div', palette = "Spectral",direction=-1)+
theme(strip.text.x = element_text(colour = "white", face = "bold",size = rel(1.5)))+
theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
theme(legend.key = element_rect(size = 5, fill = 'white'))+
theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
labs(title = "Phosphorus", x = "", y = "Pounds")+
theme(legend.key.size =  unit(0.25, "in"))+
#theme(panel.background = element_rect(fill = "grey60"), panel.grid = element_blank())+
facet_wrap(~Geography,scales = "free_y",ncol=3)+
theme(legend.position="bottom")

ggsave("phos_plot.png", width = 30, height = 20, units = "cm",dpi = 600)


#Nitrogen



n=long_loads %>% filter(Value > 0 & str_detect(Type, 'NLoadEOT')) %>% 
  group_by(Geography,Sector,Year) %>%
  summarise(Number= sum(Value)) 

nitro_plot= n%>% mutate(N_Target=as.numeric(case_when(
  Geography=="Eastern Shore" ~ "1.43",
  Geography=="James" ~ "25.92",                        
  Geography=="Potomac" ~ "16",                             
  Geography=="Rappahannock" ~ "6.85",                              
  Geography=="York" ~ "5.52",
  TRUE                      ~ "other" 
)
) 
) %>% mutate(N_Target=N_Target*1000000, Climate=N_Target - 344400 ) %>%
  

  ggplot(aes(x=Year,y=Number,fill=Sector)) + #geom_area(stat="identity")
  geom_bar(stat="identity",width=0.7,alpha=0.9)+
  scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
  geom_text(aes(0, N_Target, label=scales::comma(N_Target), vjust=-1,hjust=-0.25),family="Times")+
  geom_hline(aes(yintercept=N_Target,color = "WIP Target"), linetype="solid", size=1.5)+
  geom_hline(aes(yintercept=Climate,color = "Climate Change Target"), linetype="dotted", size=1.5)+
  #facet_wrap(~Geography, scales = "free_y",ncol=1)
  scale_color_manual(values=c("red","black"))+
  theme_dark()+
  theme(legend.title=element_blank())+
  scale_fill_brewer(type = 'div', palette = "Spectral",direction=-1)+
  theme(strip.text.x = element_text(colour = "white", face = "bold",size = rel(1.5)))+
  theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
  theme(legend.key = element_rect(size = 5, fill = 'white'))+
  theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
  labs(title = "Nitrogen", x = "", y = "Pounds")+
  theme(legend.key.size =  unit(0.25, "in"))+
  #theme(panel.background = element_rect(fill = "grey60"), panel.grid = element_blank())+
  facet_wrap(~Geography,scales = "free_y",ncol=3)+
  theme(legend.position="bottom")

ggsave("nitrogen_plot.png", width = 30, height = 20, units = "cm",dpi = 600)



####################
##########################
#################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############################


long_loads <- loads %>% gather(Title,Value ,X2018.Progress.V5_Amount:X1985.Progress_SLoadEOT) %>% 
  separate(Title, c("Year", "Progress","Type"), sep = "\\.{1}")  %>% mutate(Year=gsub('X','',Year)) %>% 
  unite_( "Type", c("Progress","Type")) %>% filter(Value > 0 & str_detect(Type, 'PLoadEOT') | str_detect(Type, 'NLoadEOT')) 

long_loads2=long_loads %>% mutate(Type2=ifelse(str_detect(Type, 'NLoadEOT'),"Nitrogen","Phosphorus")) %>%

mutate(Wip_Target=as.numeric(case_when(
  Type2=="Phosphorus" & Geography=="Eastern Shore" ~ "0.164",
  Type2=="Phosphorus" & Geography=="James" ~ "2.731",                        
  Type2=="Phosphorus" & Geography=="Potomac" ~ "1.892",                             
  Type2=="Phosphorus" & Geography=="Rappahannock" ~ "0.849",                              
  Type2=="Phosphorus" & Geography=="York" ~ "0.556",
  Type2=="Nitrogen" & Geography=="Eastern Shore" ~ "1.43",
  Type2=="Nitrogen" & Geography=="James" ~ "25.92",                        
  Type2=="Nitrogen" & Geography=="Potomac" ~ "16.00",                             
  Type2=="Nitrogen" & Geography=="Rappahannock" ~ "6.85",                              
  Type2=="Nitrogen" & Geography=="York" ~ "5.52",
  TRUE                      ~ "other" 
)
),Climate=as.numeric(case_when(
  Type2=="Phosphorus" & Geography=="Eastern Shore" ~ "0.159",
  Type2=="Phosphorus" & Geography=="James" ~ "2.699",                        
  Type2=="Phosphorus" & Geography=="Potomac" ~ "1.785",                             
  Type2=="Phosphorus" & Geography=="Rappahannock" ~ "0.813",                              
  Type2=="Phosphorus" & Geography=="York" ~ "0.543",
  Type2=="Nitrogen" & Geography=="Eastern Shore" ~ "1.31",
  Type2=="Nitrogen" & Geography=="James" ~ "25.53",                        
  Type2=="Nitrogen" & Geography=="Potomac" ~ "14.578",                             
  Type2=="Nitrogen" & Geography=="Rappahannock" ~ "6.55",                              
  Type2=="Nitrogen" & Geography=="York" ~ "5.34",
  TRUE                      ~ "other" 
) ),Wip_Target=Wip_Target *1000000,Climate=Climate *1000000 )

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


updated_phos_plot=long_loads2 %>% filter(Type2=="Phosphorus") %>% 
  ggplot(aes(x=Year,y=Value,fill=Sector)) + #geom_area(stat="identity")
  geom_bar(stat="identity",width=0.7,alpha=0.9)+
  scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
  geom_text(aes(0, Wip_Target, label=scales::comma(Wip_Target), vjust=-1,hjust=-0.25),family="Times")+
  geom_hline(aes(yintercept=Wip_Target,color = "WIP Target"), linetype="solid", size=1.5)+
  geom_hline(aes(yintercept=Climate,color = "Climate Change Target"), linetype="dotted", size=1.5)+
  #facet_wrap(~Geography, scales = "free_y",ncol=1)
  scale_color_manual(values=c("red","black"))+
  theme_dark()+
  theme(legend.title=element_blank())+
  scale_fill_brewer(type = 'div', palette = "Spectral",direction=-1)+
  theme(strip.text.x = element_text(colour = "white", face = "bold",size = rel(1.5)))+
  theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
  theme(legend.key = element_rect(size = 5, fill = 'white'))+
  theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
  labs(title = "Phosphorus", x = "", y = "Pounds")+
  theme(legend.key.size =  unit(0.25, "in"))+
  #theme(panel.background = element_rect(fill = "grey60"), panel.grid = element_blank())+
  facet_wrap(~Geography,scales = "free_y",ncol=3)+
  theme(legend.text=element_text(size=12),legend.position="bottom")

ggsave("updated_phos_plot.png", width = 30, height = 20, units = "cm",dpi = 600)





#########################NITRO#####################################




updated_nitro_plot=long_loads2 %>% filter(Type2=="Nitrogen") %>% ggplot(aes(x=Year,y=Value,fill=Sector)) + #geom_area(stat="identity")
  geom_bar(stat="identity",width=0.7,alpha=0.9)+
  scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
  geom_text(aes(0, Wip_Target, label=scales::comma(Wip_Target), vjust=-1,hjust=-0.25),family="Times")+
  geom_hline(aes(yintercept=Wip_Target,color = "WIP Target"), linetype="solid", size=1.5)+
  geom_hline(aes(yintercept=Climate,color = "Climate Change Target"), linetype="dotted", size=1.5)+
  #facet_wrap(~Geography, scales = "free_y",ncol=1)
  scale_color_manual(values=c("red","black"))+
  theme_dark()+
  theme(legend.title=element_blank())+
  scale_fill_brewer(type = 'div', palette = "Spectral",direction=-1)+
  theme(strip.text.x = element_text(colour = "white", face = "bold",size = rel(1.5)))+
  theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
  theme(legend.key = element_rect(size = 5, fill = 'white'))+
  theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
  labs(title = "Nitrogen", x = "", y = "Pounds")+
  theme(legend.key.size =  unit(0.25, "in"))+
  #theme(panel.background = element_rect(fill = "grey60"), panel.grid = element_blank())+
  facet_wrap(~Geography,scales = "free_y",ncol=3)+
  theme(legend.text=element_text(size=12),legend.position="bottom")

ggsave("updated_nitro_plot.png", width = 30, height = 20, units = "cm",dpi = 600)


#######################################################################################
#POTOMAC

Potomac_plot=long_loads2 %>% filter(Geography=="Potomac") %>% ggplot(aes(x=Year,y=Value,fill=Sector)) + #geom_area(stat="identity")
  geom_bar(stat="identity",width=0.7,alpha=0.9)+
  scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
  geom_text(aes(0, Wip_Target, label=scales::comma(Wip_Target), vjust=-1,hjust=-0.25),family="Times")+
  geom_hline(aes(yintercept=Wip_Target,color = "WIP Target"), linetype="solid", size=1.5)+
  geom_hline(aes(yintercept=Climate,color = "Climate Change Target"), linetype="dotted", size=1.5)+
  #facet_wrap(~Geography, scales = "free_y",ncol=1)
  scale_color_manual(values=c("red","black"))+
  theme_dark()+
  theme(legend.title=element_blank())+
  scale_fill_brewer(type = 'div', palette = "Spectral",direction=-1)+
  theme(strip.text.x = element_text(colour = "white", face = "bold",size = rel(1.5)))+
  theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
  theme(legend.key = element_rect(size = 5, fill = 'white'))+
  theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
  labs(title = "Potomac River Basin", x = "", y = "Pounds")+
  theme(legend.key.size =  unit(0.25, "in"))+
  #theme(panel.background = element_rect(fill = "grey60"), panel.grid = element_blank())+
  facet_wrap(~Type2,scales = "free_y",ncol=3)+
  theme(legend.text=element_text(size=12),legend.position="right")


  

############################ GEOM AREA####################


L3=long_loads2




 Potomac_Area=L3 %>%   mutate(Year=replace(Year, Year==1985, 2009)) %>% 
   filter(Geography=="Potomac") %>% 
   group_by(Sector,Year,Type2,Wip_Target,Climate) %>%
  summarise(n = sum(Value)) %>%
  ggplot(aes(x=as.numeric(as.character(factor(Year))), y = n, fill=Sector)) +
  geom_area(position = "stack",alpha=0.9) +
  scale_x_continuous("Year",expand =expand_scale(mult = c(0, 0)))+
  labs(y="Pounds",title = "Potomac River Basin")+
  facet_wrap(~Type2, nrow=2,scales="free_y")+
  theme_minimal()+
  theme(legend.title=element_blank())+
  scale_fill_brewer(type = 'div', palette = "Spectral",direction=-1)+
  theme(strip.text.x = element_text(colour = "black", face = "bold",size = rel(1.5)))+
  theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
  #(legend.key = element_rect(size = 5, fill = 'white'))+
  theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
  scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
   geom_text(aes(2010, Wip_Target, label=scales::comma(Wip_Target),vjust=-1))+
   geom_hline(aes(yintercept=Wip_Target,color = "WIP Target"), linetype="solid", size=1.5)+
   geom_hline(aes(yintercept=Climate,color = "Climate Change Target"), linetype="dotted", size=1.5)+
   #facet_wrap(~Geography, scales = "free_y",ncol=1)
   scale_color_manual(values=c("red","black"))
   
 
 
 ggsave("Potomac_Area.png", width = 35, height = 20, units = "cm",dpi = 600)
 
   #+
  #geom_hline(aes(yintercept=Climate,color = "Climate Change Target"), linetype="dotted", size=1.5)+
  
 
