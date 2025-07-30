######
# Extension figures
#  for Huseth only with NC data
##

CEW

ggplot((CEW %>% filter(Week >= 20)),aes(x=Year,y=(mean_count))) +
  geom_smooth(method="gam",se=FALSE) +
  scale_color_viridis(discrete=T) +
  #geom_vline(xintercept= 23,linetype = "dashed")+
  #geom_vline(xintercept= 36,linetype = "dashed")+
  theme_pubr()+ theme(legend.title = element_blank())  +
  coord_cartesian(ylim=c(0,NA))

WOY_Year_logged <-
  ggplot((CEW %>% filter(Week >= 20)),aes(x=Week,y=log(mean_count+1),color=as.factor(Year))) +
  geom_smooth(method="gam",se=FALSE) +
  scale_color_viridis(discrete=T) +
  geom_vline(xintercept= 23,linetype = "dashed")+
  geom_vline(xintercept= 36,linetype = "dashed")+
  theme_pubr()+ theme(legend.title = element_blank()) 

WOY_Year_2001_onward <-
  ggplot((CEW %>% filter(Year >= 2001)),aes(x=Week,y=(mean_count),color=as.factor(Year))) +
  geom_smooth(method="gam",se=FALSE) +
  scale_color_viridis(discrete=T) +
  geom_vline(xintercept= 23,linetype = "dashed")+
  geom_vline(xintercept= 36,linetype = "dashed")+
  theme_pubr()+ theme(legend.title = element_blank()) 

ggsave(WOY_Year_logged,file="WOY_Year_logged.pdf")
ggsave(WOY_Year_2001_onward,file="WOY_Year_2001_onward.pdf")
