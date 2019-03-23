setwd("~/Documents/GitHub/rworkshop2019/")

pacman::p_load(tidyverse)
pacman::p_load_gh("crsh/papaja", "thomasp85/patchwork")

DD <- read.csv("demodata_ANTPRM.csv", header = T)

length(unique(DD$SID)) # N = 36
DD$SID <- factor(DD$SID)

glimpse(DD)

table(DD$SID)
table(DD$StudyResp, DD$SID)
table(DD$Visibility, DD$SID)

D2 <- na.omit(DD)
dim(DD)
dim(D2)
glimpse(D2)



# visibility 
vD2slong <- D2 %>% group_by(SID, Visibility) %>% 
  summarise(Accuracy = mean(Correct)*100) %>% 
  ungroup()

vD2slong$Visibility = factor(vD2slong$Visibility, ordered = TRUE)

vD2g <- vD2slong %>% group_by(Visibility) %>%
  summarise(M = mean(Accuracy)) %>% 
  ungroup() 

vD2g$ci <- wsci(data = vD2slong, id = "SID", # the order of conditions must be checked.
                factors = "Visibility", dv = "Accuracy")$Accuracy


# Recognition level
rD2slong <- D2 %>% group_by(SID, StudyResp) %>% 
  summarise(Accuracy = mean(Correct)*100) %>% 
  ungroup()

rD2slong$StudyResp = factor(rD2slong$StudyResp, ordered = TRUE)

rD2g <- rD2slong %>% group_by(StudyResp) %>%
  summarise(M = mean(Accuracy)) %>% 
  ungroup() 

rD2g$ci <- wsci(data = rD2slong, id = "SID", # the order of conditions must be checked.
             factors = "StudyResp", dv = "Accuracy")$Accuracy



# plot for visibility level
P1 <- ggplot(vD2g, mapping=aes(x = Visibility, y=M, group=1)) + 
  geom_ribbon(aes(ymin=M-ci, ymax=M+ci), fill="darkred", alpha=0.3) +
  geom_line(colour="darkred", size = 1) +
  geom_line(vD2slong, alpha = 0.3, show.legend = F,
            mapping=aes(x=Visibility, y=Accuracy, group=SID, color=SID)) +
  # geom_pointrange(rD2g, inherit.aes=FALSE,
  #                 mapping=aes(x = Visibility, y=Accuracy, 
  #                             ymin = Accuracy - ci, ymax = Accuracy + ci), 
  #                 colour="darkred", size = 1) +
  coord_cartesian(ylim = c(0, 100), clip = "on") +
  labs(x = "Visibility", 
       y = "Identification Accuracy (%)") +
  # scale_x_discrete(labels = c("1" = "1(Guessed)","4"="4(Recognized)")) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") 


# plot for recognition level
P2 <- ggplot(rD2g, mapping=aes(x = StudyResp, y=M, group=1)) + 
  geom_ribbon(aes(ymin=M-ci, ymax=M+ci), fill="darkred", alpha=0.3) +
  geom_line(colour="darkred", size = 1) +
  geom_line(rD2slong, alpha = 0.3, show.legend = F,
            mapping=aes(x=StudyResp, y=Accuracy, group=SID, color=SID)) +
  # geom_pointrange(rD2g, inherit.aes=FALSE,
  #                 mapping=aes(x = StudyResp, y=Accuracy, 
  #                             ymin = Accuracy - ci, ymax = Accuracy + ci), 
  #                 colour="darkred", size = 1) +
  coord_cartesian(ylim = c(0, 100), clip = "on") +
  labs(x = "Recognition Level", 
       y = "Identification Accuracy (%)") +
  scale_x_discrete(labels = c("1" = "Guessed","2" = "", "3" = "",
                              "4"="Recognized")) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") 
    

P1 + P2
