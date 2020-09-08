library(extrafont)
#font_import(paths = c("C:/Users/Janeth/AppData/Local/Microsoft/Windows/Fonts", prompt = F))
#font_import(paths = c("C:/Windows/Fonts", prompt = F))
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
library(tidyverse)
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
plot <- ggplot(teamstats2, aes(x = Split, y = EGR, group = Team, family = "Franklin Gothic Demi Cond")) +
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
  #geom_text_repel(label=teamstats2$Team, size = 5, family = "Franklin Gothic Demi Cond") + 
  #scale_x_continuous(limits=c(35,65), n.breaks = 6) +
  #geom_hline(yintercept=0, size = 0.5, linetype=2, color='#72BF44') + geom_vline(xintercept=50, size = 0.5, linetype=2, color='#72BF44') +
  #annotate("rect",xmin = 35, xmax = 65, ymin = 0, ymax = 15, fill = '#d9184b', alpha = 0.2) +
  #annotate("rect",xmin = 35, xmax = 65, ymin = 15, ymax = 30, fill = '#04F585', alpha = 0.2) +
  theme(axis.line = element_blank(),
        plot.margin = unit(c(0,0,0,5),"cm"),
        plot.title = element_markdown(family = "Franklin Gothic Demi Cond", hjust = 0.37),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 14, hjust = 1, vjust=0.5, margin=margin(0,0,0,0), family = "Franklin Gothic Demi Cond"),
        axis.text.x = element_text(size = 14, vjust = 5, margin=margin(0,0,0,0), family = "Franklin Gothic Demi Cond"))

save_plot("EGR of LCK Teams Summer 2020.png", plot, base_height = 8, base_aspect_ratio = 1.58)

##############################
plot2 <- ggplot(teamstats2, aes(x = Split, y = MLR, group = Team, family = "Franklin Gothic Demi Cond")) +
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
  #geom_text_repel(label=teamstats2$Team, size = 5, family = "Franklin Gothic Demi Cond") + 
  #scale_x_continuous(limits=c(35,65), n.breaks = 6) +
  #geom_hline(yintercept=0, size = 0.5, linetype=2, color='#72BF44') + geom_vline(xintercept=50, size = 0.5, linetype=2, color='#72BF44') +
  #annotate("rect",xmin = 35, xmax = 65, ymin = 0, ymax = 15, fill = '#d9184b', alpha = 0.2) +
  #annotate("rect",xmin = 35, xmax = 65, ymin = 15, ymax = 30, fill = '#04F585', alpha = 0.2) +
  theme(axis.line = element_blank(),
        plot.margin = unit(c(0,0,0,5),"cm"),
        plot.title = element_markdown(family = "Franklin Gothic Demi Cond", hjust = 0.37),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 14, hjust = 1, vjust=0.5, margin=margin(0,0,0,0), family = "Franklin Gothic Demi Cond"),
        axis.text.x = element_text(size = 14, vjust = 5, margin=margin(0,0,0,0), family = "Franklin Gothic Demi Cond"))

save_plot("MLR of LCK Teams Summer 2020.png", plot2, base_height = 8, base_aspect_ratio = 1.58)

##################################

teamstatsplit <- read.xlsx("EGR and MLR progression split.xlsx", 1, header=TRUE)

plot3 <- ggplot(EGRdata, aes(x= Split, y = EGR, group = Team, family = "Franklin Gothic Demi Cond")) + 
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
        plot.title = element_markdown(family = "Franklin Gothic Demi Cond", hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16, vjust=10, margin=margin(-15,0,0,0), family = "Franklin Gothic Demi Cond"))

######################################
#scatterplot
teamstats <- read.xlsx("EGR and MLR progression split.xlsx", 1, header=TRUE)
teamstats$Split <- ifelse(teamstats$Split == 0, "first half of split","second half of split")

teamstats <- teamstats %>% filter(!Team %in% c("Hanwha Life Esports","KT Rolster","SANDBOX Gaming","SeolHaeOne Prince","Team Dynamics"))
plot4 <- 
  ggplot(teamstats, aes(x = EGR, y = MLR, shape = Split, fill = Team, color = Team, family = "Franklin Gothic Demi Cond")) +
  geom_point(size = 5, alpha = 0.75) + 
  guides(color = FALSE, fill = FALSE, shape = guide_legend(override.aes = list(color = '#ffffff', fill = '#ffffff'))) +
  scale_color_manual(values = c('#0143b3','#67a2a8','#5a8eff','#ab8c2b','#e50027')) +
  scale_fill_manual(values = c('#0143b3','#67a2a8','#5a8eff','#ab8c2b','#e50027')) +
  labs(title = "EGR and MLR  of LCK Teams") +
  geom_text_repel(label=teamstats$Team, size = 7, family = "Franklin Gothic Demi Cond") + 
  scale_x_continuous(limits=c(20,80), breaks = c(20, 30, 40, 50, 60, 70, 80)) +
  scale_y_continuous(limits=c(-15,15), n.breaks = 6) +
  geom_hline(yintercept=0, size = 0.5, linetype=2, color='#ffffff') + geom_vline(xintercept=50, size = 0.5, linetype=2, color='#ffffff') +
  #annotate("rect",xmin = 35, xmax = 65, ymin = 0, ymax = 15, fill = '#d9184b', alpha = 0.2) +
  annotate("rect",xmin = 50, xmax = 80, ymin = 10, ymax = 15, fill = '#04F585', alpha = 0.2) +
  theme(axis.line = element_blank(),
        plot.background=element_rect(fill = '#1f1f1f'),
        panel.background = element_rect(fill = '#1f1f1f'),
        legend.title = element_text(family = "Franklin Gothic Demi Cond", size = 16, color = '#ffffff'),
        legend.background = element_rect(fill = '#1f1f1f', color = '#1f1f1f'),
        legend.key = element_rect(fill = '#1f1f1f'),
        legend.position = c(0.90,0.15),
        legend.text = element_text(family = "Franklin Gothic Demi Cond",size = 14, color = '#ffffff'),
        axis.text = element_text(color = '#ffffff', family = "Franklin Gothic Demi Cond",size = 16),
        axis.title = element_text(family = "Franklin Gothic Demi Cond",size = 16, color = '#ffffff'),
        panel.grid = element_blank(),
        plot.margin = margin(1, 2, 1, 2, "cm"),
        plot.title = element_markdown(family = "Franklin Gothic Demi Cond", hjust = 0.5, vjust = 0, 
                                      face = "bold", size = 32, color = '#ffffff'))

save_plot("EGR and MLR of LCK Teams.png", plot4, base_height = 8, base_aspect_ratio = 1.58)
########################################
#animated scatter plot
library(devtools)
#install_github("dgrtwo/gganimate")
library(gganimate)
library(av)
library(glue)
library(gifski)

playerstats <- read.xlsx("Playerstats tabulated.xlsx", 1, header=TRUE)
carries <- playerstats %>% filter(!Pos %in% c("Support","Jungle"), GP >= 5) 
Patches <- ifelse(carries$Patch == 1, "Patches 10.11-10.12",ifelse(carries$Patch == 2, "Patches 10.13-10.14", "Patches 10.15-10.16"))
carries <- cbind(carries,Patches)
g <- ggplot(carries, aes(GD10, DPM, size = KDA, frame = Patch, family = "Franklin Gothic Demi Cond")) +
  geom_point(aes(color = Team),alpha = 0.75) + 
  guides(color = FALSE, size = guide_legend(override.aes = list(color = '#ffffff', fill = '#ffffff'))) +
  scale_x_continuous(limits=c(-485,485), n.breaks = 8) +
  geom_text(aes(color=Team, label=Player, size = 7), family = "Franklin Gothic Demi Cond", position = position_dodge(width = 1),
            vjust = -1,show.legend = FALSE) + 
  scale_color_manual(values = c('#0143b3','#67a2a8','#5a8eff','#ab8c2b','#e50027')) +
  # scale_fill_manual(values = c('#0143b3','#67a2a8','#5a8eff','#ab8c2b','#e50027')) +
  scale_size_continuous(range = c(2,20)) +
  scale_y_continuous(limits=c(300,700), n.breaks = 6) +
  geom_hline(yintercept=500, size = 0.5, linetype=2, color='#ffffff') + geom_vline(xintercept=0, size = 0.5, linetype=2, color='#ffffff') +
  annotate("rect",xmin = 350, xmax = 485, ymin = 500, ymax = 672, fill = '#04F585', alpha = 0.2) +
  labs(title = "Carry Players' Performances of Top LCK Teams in {closest_state}") +
  theme(axis.line = element_blank(),
        plot.title = element_markdown(family = "Franklin Gothic Demi Cond", hjust = 0, vjust = 1, face = "bold", size = 34, color = '#ffffff'),
        plot.background=element_rect(fill = '#1f1f1f'),
        panel.background = element_rect(fill = '#1f1f1f'),
        legend.title = element_text(family = "Franklin Gothic Demi Cond", size = 20, color = '#ffffff'),
        legend.background = element_rect(fill = '#1f1f1f', color = '#1f1f1f'),
        legend.key = element_rect(fill = '#1f1f1f'),
        legend.position = c(0.90,0.15),
        #legend.box = "horizontal",
        #legend.key.size = unit(2, "cm"),
        legend.text = element_text(family = "Franklin Gothic Demi Cond",size = 18, color = '#ffffff'),
        axis.text = element_text(family = "Franklin Gothic Demi Cond",size = 20, color = '#ffffff'),
        axis.title = element_text(family = "Franklin Gothic Demi Cond",size = 20, color = '#ffffff'),
        panel.grid = element_blank(),
        plot.margin = margin(2, 2, 2, 2, "cm")) + 
  transition_states(Patches, transition_length = 4, state_length = 4)

#w <- 1040
animate(g, 200, fps = 20, renderer = gifski_renderer(), width = 1200, height = 1000)
anim_save("carries.gif")
#################################
playerstats <- read.xlsx("Playerstats tabulated.xlsx", 1, header=TRUE)
supps <- playerstats %>% filter(Pos %in% c("Support","Jungle"), GP >= 5) 
supps <- supps[-c(7,8),]
Patches <- ifelse(supps$Patch == 1, "Patches 10.11-10.12",ifelse(supps$Patch == 2, "Patches 10.13-10.14", "Patches 10.15-10.16"))
supps <- cbind(supps,Patches)
h <- ggplot(supps, aes(GD10, KP, size = KDA, frame = Patch, family = "Franklin Gothic Demi Cond")) +
  geom_point(aes(color = Team),alpha = 0.75) + 
  guides(color = FALSE, size = guide_legend(override.aes = list(color = '#ffffff', fill = '#ffffff'))) +  scale_color_manual(values = c('#0143b3','#67a2a8','#5a8eff','#ab8c2b','#e50027')) +
  scale_fill_manual(values = c('#0143b3','#67a2a8','#5a8eff','#ab8c2b','#e50027')) +
  geom_text(aes(color=Team, label=Player, size = 6), family = "Franklin Gothic Demi Cond", position = position_dodge(width = 1),
            vjust = -1,show.legend = FALSE) + 
  scale_x_continuous(limits=c(-600,600), n.breaks = 8) +
  scale_y_continuous(limits=c(0.50,0.80), n.breaks = 6) +
  scale_size_continuous(range = c(2,20)) +
  geom_hline(yintercept=0.65, size = 0.5, linetype=2, color='#ffffff') + geom_vline(xintercept=0, size = 0.5, linetype=2, color='#ffffff') +
  annotate("rect",xmin = 350, xmax = 600, ymin = 0.65, ymax = 0.80, fill = '#04F585', alpha = 0.2) +
  labs(title = "Jungle and Support Performances of Top LCK Teams in {closest_state}") +
  theme(axis.line = element_blank(),
        plot.title = element_markdown(family = "Franklin Gothic Demi Cond", hjust = 0, vjust = 1, face = "bold", size = 34, color = '#ffffff'),
        plot.background=element_rect(fill = '#1f1f1f'),
        panel.background = element_rect(fill = '#1f1f1f'),
        legend.title = element_text(family = "Franklin Gothic Demi Cond", size = 20, color = '#ffffff'),
        legend.background = element_rect(fill = '#1f1f1f', color = '#1f1f1f'),
        legend.key = element_rect(fill = '#1f1f1f'),
        legend.position = c(0.90,0.15),
        #legend.box = "horizontal",
        #legend.key.size = unit(2, "cm"),
        legend.text = element_text(family = "Franklin Gothic Demi Cond",size = 18, color = '#ffffff'),
        axis.text = element_text(family = "Franklin Gothic Demi Cond",size = 20, color = '#ffffff'),
        axis.title = element_text(family = "Franklin Gothic Demi Cond",size = 20, color = '#ffffff'),
        panel.grid = element_blank(),
        plot.margin = margin(2, 3, 2, 2, "cm")) + 
  transition_states(Patches, transition_length = 4, state_length = 4)

w <- 1040
animate(h, 200, fps = 20, renderer = gifski_renderer(), width = 1200, height = 1000)
#magick::image_write(h_gif, path="supps.gif")
anim_save("supps.gif")

###################################################
#animated bar chart
champstats <- read.xlsx("Champstats.xlsx", 1, header=TRUE)
champstats <- champstats %>%
  group_by(Patch) %>%
  mutate(rank = rank(-W., ties.method = "random") * 1) %>%
  group_by(Champion) %>% 
  ungroup()

staticplot <- ggplot(champstats, 
                     aes(rank, group = Champion, fill = as.factor(Pos), 
                         color = as.factor(Pos))) +
  geom_tile(aes(y = W./2, height = W., width = 0.9), alpha = 0.8) +
  geom_text(aes(y = 0, label = paste(Champion, " "), color = Pos),
            vjust = 0.2, hjust = 1, size = 9, family = "Franklin Gothic Demi Cond") +
  geom_text(aes(y=W., label = paste(" ", W.*100,"%"), color = Pos,
                hjust=0), size = 6, family = "Franklin Gothic Demi Cond") +
  scale_color_manual(values = c('#ff6026','#02e2bf','#ffd321','#145263','#8f225d'),
                     breaks=c("Top","Jungle","Middle","ADC","Support")) +
  scale_fill_manual(values = c('#ff6026','#02e2bf','#ffd321','#145263','#8f225d'),
                    breaks=c("Top","Jungle","Middle","ADC","Support")) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill=guide_legend(title="Position")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_text(family = "Franklin Gothic Demi Cond", size = 18, color = '#ffffff'),
        legend.background = element_rect(fill = '#1f1f1f', color = '#1f1f1f'),
        legend.key = element_rect(fill = '#1f1f1f', color = '#1f1f1f'),
        legend.position = c(0.90,0.15),
        legend.text = element_text(family = "Franklin Gothic Demi Cond",size = 18, color = '#ffffff'),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title=element_markdown(size = 36, vjust= 0, hjust=0.5, face="bold", family = "Franklin Gothic Demi Cond", color = '#ffffff'),
        plot.subtitle=element_markdown(size=18, hjust=0.5, vjust = 0.5, face="italic", family = "Franklin Gothic Demi Cond", color = '#ffffff'),
        plot.background=element_rect(fill = '#1f1f1f'),
        panel.background = element_rect(fill = '#1f1f1f'),
        plot.margin = margin(2, 2, 2, 6, "cm"))

anim <- staticplot + transition_states(Patch, transition_length = 5, state_length = 11) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'LCK: Win rates in Patch {closest_state}',  
       subtitle  =  "Top 10 Most Played Champions")
# For GIF
animate(anim, 300, fps = 20, width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))
anim_save("champs.gif")
