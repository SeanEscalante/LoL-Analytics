library(extrafont)
extrafont::font_import()
loadfonts(device = "win")
library(grid)
library(xlsx)
library(ggplot2)
library(ggrepel)
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

playerstats <- read.xlsx("Player Stats.xlsx", 1, header=TRUE)
playerstats2 <- playerstats[-c(11,21,32,39,42),]

plot <- ggplot(playerstats2, aes(x = CTR., y = GD10, family = "CM Sans")) 

plot1 <- plot + geom_point(aes(color = W.), size = 5, alpha = 0.5) + 
  labs(title = "<span style='font-size:18pt'> <span style='color:#00FF00'>Player </span><span style='color:#00FF00'>W</span><span style='color:#00FF00'>i</span><span style='color:#00FF00'>n </span><span style='color:#00FF00'>R</span><span style='color:#00FF00'>a</span><span style='color:#00FF00'>t</span><span style='color:#00FF00'>e</span><span style='color:#00FF00'>s</span> by GD10 and CPR of LEC Summer 2020
         </span>") +
  geom_text_repel(label=playerstats2$Player, size = 5, family = "CM Sans") + 
  geom_hline(yintercept=0.5, size = 0.5) + geom_vline(xintercept=0.5, size = 0.5) +
  scale_colour_gradient(low = "red", high = "green", aesthetics = c("colour","fill")) +
  scale_x_continuous(name="Counterpick Rate", limits=c(0, 1), n.breaks = 10) +
  scale_y_continuous(name="Gold Diff @10mins", 
                     limits=c((-1*max(abs(min(playerstats2$GD10)),abs(max(playerstats2$GD10)))), 
                              max(abs(min(playerstats2$GD10)),abs(max(playerstats2$GD10)))),
                     n.breaks = 10) +
  theme(axis.line = element_blank(),
        plot.title = element_markdown(family = "CM Sans", hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.95,0.85),
        legend.title = element_blank(),
        axis.title.x = element_text(family = "CM Sans",size = 16),
        axis.title.y = element_text(family = "CM Sans",size = 16))

save_plot("player win rates by GD10 and CPR.png", plot1, base_height = 8, base_aspect_ratio = 1.58)

############################################

plot2 <- ggplot(playerstats2, aes(x = CTR., y = XPD10, family = "CM Sans")) +
  geom_point(aes(color = W.), size = 5, alpha = 0.5) + 
  labs(title = "<span style='font-size:18pt'>Player 
       <span style='color:#00FF00'>W</span><span style='color:#47FF00'>i</span><span style='color:#8DFF00'>n </span><span style='color:#D4FF00'>R</span><span style='color:#FFD300'>a</span><span style='color:#FF8C00'>t</span><span style='color:#FF4600'>e</span><span style='color:#FF0000'>s</span> by XPD10 and CPR of LEC Summer 2020
         </span>") +
  geom_text_repel(label=playerstats2$Player, size = 5, family = "CM Sans") + 
  geom_hline(yintercept=0.5, size = 0.5) + geom_vline(xintercept=0.5, size = 0.5) +
  scale_colour_gradient(low = "red", high = "green", aesthetics = c("colour","fill")) +
  scale_x_continuous(name="Counterpick Rate", limits=c(0, 1), n.breaks = 10) +
  scale_y_continuous(name="Exp Diff @10mins", 
                     limits=c((-1*max(abs(min(playerstats2$XPD10)),abs(max(playerstats2$XPD10)))), 
                              max(abs(min(playerstats2$XPD10)),abs(max(playerstats2$XPD10)))),
                     n.breaks = 10) +
  theme(axis.line = element_blank(),
        plot.title = element_markdown(family = "CM Sans", hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.95,0.85),
        legend.title = element_blank(),
        axis.title.x = element_text(family = "CM Sans",size = 16),
        axis.title.y = element_text(family = "CM Sans",size = 16))

save_plot("player win rates by XPD10 and CPR.png", plot2, base_height = 8, base_aspect_ratio = 1.58)
############################################

plot3 <- ggplot(playerstats2, aes(x = CTR., y = CSD10, family = "CM Sans")) +
  geom_point(aes(color = W.), size = 5, alpha = 0.5) + 
  labs(title = "<span style='font-size:18pt'>Player 
       <span style='color:#00FF00'>W</span><span style='color:#47FF00'>i</span><span style='color:#8DFF00'>n </span><span style='color:#D4FF00'>R</span><span style='color:#FFD300'>a</span><span style='color:#FF8C00'>t</span><span style='color:#FF4600'>e</span><span style='color:#FF0000'>s</span> by CSD10 and CPR of LEC Summer 2020
         </span>") +
  geom_text_repel(label=playerstats2$Player, size = 5, family = "CM Sans") + 
  geom_hline(yintercept=0.5, size = 0.5) + geom_vline(xintercept=0.5, size = 0.5) +
  scale_colour_gradient(low = "red", high = "green", aesthetics = c("colour","fill")) +
  scale_x_continuous(name="Counterpick Rate", limits=c(0, 1), n.breaks = 10) +
  scale_y_continuous(name="CS Diff @10mins", 
                     limits=c((-1*max(abs(min(playerstats2$CSD10)),abs(max(playerstats2$CSD10)))), 
                              max(abs(min(playerstats2$CSD10)),abs(max(playerstats2$CSD10)))),
                     n.breaks = 10) +
  theme(axis.line = element_blank(),
        plot.title = element_markdown(family = "CM Sans", hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.95,0.85),
        legend.title = element_blank(),
        axis.title.x = element_text(family = "CM Sans",size = 16),
        axis.title.y = element_text(family = "CM Sans",size = 16))

save_plot("player win rates by CSD10 and CPR.png", plot3, base_height = 8, base_aspect_ratio = 1.58)

cor.test(playerstats2$CTR., playerstats2$W.)
