library(extrafont)
#font_import(paths = c("C:/Users/Janeth/AppData/Local/Microsoft/Windows/Fonts", prompt = F))
loadfonts(device = "win")
#asdfghgasdfghsasdf
library(grid)
library(xlsx)
library(ggplot2)
library(ggtext)
library(directlabels)
library(cowplot)
library(gridExtra)
library(rstudioapi)
library(dplyr)
library(ggrepel)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#Teams <- c("XL","S04","FNC","G2","MAD","MSF","OG","RGE","SK","VIT")
teamstats <- read.xlsx("EGR and MLR progression.xlsx", 1, header=TRUE)
###############################
teamstatsp1 <- teamstats[c(1:10,21:30,41:50),]
teamstatsp1$Statistics <- ifelse(teamstatsp1$Statistics == "EGR_1", 0,
                                 (ifelse(teamstatsp1$Statistics == "EGR_2",2,4)))
teamstatsp2 <- teamstats[c(11:20,31:40,51:60),]
teamstatsp2$Statistics <- ifelse(teamstatsp2$Statistics == "MLR_1", 0,
                                 (ifelse(teamstatsp2$Statistics == "MLR_2",2,4)))
teamstats2 <- cbind(teamstatsp1,teamstatsp2)
teamstats2 <- teamstats2[,-(3:4)]
colnames(teamstats2) <- c("Split","EGR","MLR","Team")

teamstats2 <- teamstats2 %>% filter(!Team %in% c("Hanwha Life Esports","KT Rolster","SANDBOX Gaming","SeolHaeOne Prince","Team Dynamics"))
plot <- ggplot(teamstats2, aes(x = Split, y = EGR, group = Team, family = "Roboto")) +
  geom_line(aes(color = Team), size = 1.25) + guides(color = FALSE) +
  scale_color_manual(values = c('#00BFFF','#9ebbc6','#003366','#FFB447','#FE6B64')) +
  geom_dl(aes(label = Team, colour = Team), method = list("last.bumpup",cex = 1.5, dl.combine("last.points"))) +
  geom_line(aes(y = 50), color='#72BF44', size = 1.25, linetype=2) +
  scale_x_continuous(labels = c("10.11-10.12","","10.13-10.14","","10.15-10.16",""),
                     limits=c(0,5), n.breaks = 5) +
  scale_y_continuous(limits=c(15,85), breaks = c(20,35,50,65,80)) +
  annotate("rect",xmin = 0, xmax = 4, ymin = 65, ymax = 85, fill = '#BFEFBB', alpha = 0.2) +
  labs(title = "<span style='font-size:22pt'>EGR of LCK Teams Summer 2020
         </span>") +
  #geom_text_repel(label=teamstats2$Team, size = 5, family = "Roboto") + 
  #scale_x_continuous(limits=c(35,65), n.breaks = 6) +
  #geom_hline(yintercept=0, size = 0.5, linetype=2, color='#72BF44') + geom_vline(xintercept=50, size = 0.5, linetype=2, color='#72BF44') +
  #annotate("rect",xmin = 35, xmax = 65, ymin = 0, ymax = 15, fill = '#d9184b', alpha = 0.2) +
  #annotate("rect",xmin = 35, xmax = 65, ymin = 15, ymax = 30, fill = '#04F585', alpha = 0.2) +
  theme(axis.line = element_blank(),
        plot.margin = unit(c(0,0,0,5),"cm"),
        plot.title = element_markdown(family = "Roboto", hjust = 0.37),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 14, hjust = 1, vjust=0.5, margin=margin(0,0,0,0), family = "Roboto"),
        axis.text.x = element_text(size = 14, vjust = 5, margin=margin(0,0,0,0), family = "Roboto"))

save_plot("EGR of LCK Teams Summer 2020.png", plot, base_height = 8, base_aspect_ratio = 1.58)

##############################
plot2 <- ggplot(teamstats2, aes(x = Split, y = MLR, group = Team, family = "Roboto")) +
  geom_line(aes(color = Team), size = 1.25) + guides(color = FALSE) +
  scale_color_manual(values = c('#00BFFF','#9ebbc6','#003366','#FFB447','#FE6B64')) +
  geom_dl(aes(label = Team, colour = Team), method = list("last.bumpup",cex = 1.4, dl.combine("last.points"))) +
  geom_line(aes(y = 0), color='#72BF44', size = 1.25, linetype=2) +
  scale_x_continuous(labels = c("10.11 - 10.12","","10.13 - 10.14","","10.15 - 10.16",""),
                     limits=c(0,5), n.breaks = 5) +
  scale_y_continuous(limits=c(-5,25), n.breaks = 5) +
  annotate("rect",xmin = 0, xmax = 4, ymin = 10, ymax = 25, fill = '#BFEFBB', alpha = 0.2) +
  labs(title = "<span style='font-size:22pt'>MLR of LCK Teams Summer 2020
         </span>") +
  #geom_text_repel(label=teamstats2$Team, size = 5, family = "Roboto") + 
  #scale_x_continuous(limits=c(35,65), n.breaks = 6) +
  #geom_hline(yintercept=0, size = 0.5, linetype=2, color='#72BF44') + geom_vline(xintercept=50, size = 0.5, linetype=2, color='#72BF44') +
  #annotate("rect",xmin = 35, xmax = 65, ymin = 0, ymax = 15, fill = '#d9184b', alpha = 0.2) +
  #annotate("rect",xmin = 35, xmax = 65, ymin = 15, ymax = 30, fill = '#04F585', alpha = 0.2) +
  theme(axis.line = element_blank(),
        plot.margin = unit(c(0,0,0,5),"cm"),
        plot.title = element_markdown(family = "Roboto", hjust = 0.37),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 14, hjust = 1, vjust=0.5, margin=margin(0,0,0,0), family = "Roboto"),
        axis.text.x = element_text(size = 14, vjust = 5, margin=margin(0,0,0,0), family = "Roboto"))

save_plot("MLR of LCK Teams Summer 2020.png", plot2, base_height = 8, base_aspect_ratio = 1.58)

##################################

teamstatsplit <- read.xlsx("EGR and MLR progression split.xlsx", 1, header=TRUE)

plot3 <- ggplot(EGRdata, aes(x= Split, y = EGR, group = Team, family = "Roboto")) + 
  geom_line(aes(color = Team), size = 1.25) + 
  scale_color_manual(values = c('#D6D6D6','#AAC5E2','#FFB447','#FE6B64','#b27526',
                                '#D6D6D6','#D6D6D6','#769ECB','#403233','#D6D6D6')) +
  geom_dl(aes(label = Team, colour = Team), method = list(cex = 1.5, dl.combine("last.points"))) + 
  geom_line(aes(y = 50), color='#72BF44', size = 1.25, linetype=2) +
  annotate("text", x = "EGR_2", y = 49, label = "50", color = '#72BF44', size = 8) +
  annotate("rect",xmin = "EGR_1", xmax = "EGR_2", ymin = 50, ymax = 65, fill = '#BFEFBB', alpha = 0.2) +
  #geom_dl(aes(y = 50, label = "50%", colour = '#72BF44'), method = list(cex = 1.5, dl.combine("last.points"))) + 
  #geom_text(aes(y = 50, label = "50%", colour = '#72BF44', vjust = -1)) +
  guides(color = FALSE) +
  labs(title = "<span style='font-size:22pt'>LEC Summer 2020: Early Game Rating</span>") +
  scale_y_continuous(limits=c(35,65),
                     n.breaks = 6) +
  scale_x_discrete(labels = c("First Half", "Second Half")) +
  theme(axis.line = element_blank(),
        plot.title = element_markdown(family = "Roboto", hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16, vjust=10, margin=margin(-15,0,0,0), family = "Roboto"))

