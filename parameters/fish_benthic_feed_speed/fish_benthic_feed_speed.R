################################################################################
# Estimate swimming speed of Green Sturgeon
# Objective: Estimate the swimming speed when the GS is feeding.
# (mostly benthic feeding)
# Method: Use fish movement data in the Delta region where it normally feeds.
# Assumption 1) All the data collected from the delta region are involved
# in feeding activity (except for extreme outliars).
# Assumption 2) Even when moving between sites this behavior also involves
# feeding activity.
# Assumption 3) All fish are directly moving towards two points with some
# activity of preying and searching without resting.
# NOTE 1) Feeding activity involves both searching for food and the time it
# spends actually feeding the prey. Therefore, the total speed includes both the
# swim speed and when it is holding at one spot. Therefore, the feeding speed
# should be close to zero if the fish actually captures more prey.
# Note 2) Kelly et al. papers (Kelly  et al. 2007 and Kelly and Klimley 2012)
# have actively tracked the fish's movement to acquire their velocity. 
# However, the results reported in their studies are an average of the entire SF
# bay and the delta. Plus, the size were sub-adults and one adult.
# Note 3) Thomas et al. 2002 used a juvenile GS but an acoustic tag with
# installed receivers. Therefore, we acquired their data and tried to analyse
# the swim speed in the delta zone.
################################################################################

##### Options ##################################################################
# Set the cutoff for benthic feeding movement
# 1) Based on Kelly et al. 2007 the max swimming speed during
# directional movements is 2.20m/s (average 0.56m/s) n=378
# non-directional movements is 1.94m/s (average 0.21m/s) n=635
# Based on Kelly et al. 2012:
# Benthic: 0.6 (SD 0.5; 0.0-2.1)
# Surface: 0.9 (SD 0.4; 0.1-1.9)
max_nd_speed = 1.94
max_dir_speed = 2.2

# Max days between observations
max_gap_days = 2

# Kelly et al. 2012 says 0.6 m/s is maxo feeding mode speed
max_feed_speed = 0.6

# 5.1 Add Kelly et al. Hansen et al. 2012 data 
Kelly2007Fsize<-mean(c(105,105,101,102,153,106))
Kelly2007Fspeed<-0.21
Kelly2012Fsize<-mean(c(105,105,101,153,106))
Kelly2012Fspeed<-0.6
Hansen2022size<-mean(c(14.1,29.4)) 
Hansen2022speed<-mean(c(0.0027,0.094))

################################################################################

##### Load libraries ###########################################################
source("general_scripts/load_libraries.R")

##### Read in data and clean ###################################################
kelly_et_al = read.csv(here("parameters",
                            "fish_benthic_feed_speed",
                            "inputs",
                            "fish_benthic_feed_speed_kelly _and_hasen.csv"),
                       header=TRUE)

thomas_2022 = read.csv(here("parameters",
                               "fish_benthic_feed_speed",
                               "inputs",
                               "fish_benthic_feed_speed_thomas.csv"),
                               header=TRUE) %>%
  select(No:Activity) %>%
  filter(Dist_km != "skipped a station") %>%
  mutate_at(c("Length","Dist_km","DeltaTime","DeltaTimeMin","Distperhour","Distperday"),
            as.numeric) %>%
  mutate_at(c("Fish","Activity","DLocationGen","ALocationGen"), as.factor) %>%
  mutate(MeterPerSec=Distperhour/3600*1000) %>%
  mutate(Activity=case_when(Activity=="Delta"~"Delta",
                            Activity=="Migrate"~"Bay")) %>%
  select(-c("DeltaTime")) %>% 
  filter((MeterPerSec < max_nd_speed & Activity=="Delta") |
           (MeterPerSec < max_dir_speed & Activity=="Bay")) %>% 
  filter(DeltaTimeMin<60*24*max_gap_days)


##### Plot the data ############################################################
speed_histogram = thomas_2022 %>%
    ggplot(aes(x=MeterPerSec, fill=Activity, color=Activity)) + 
    theme_classic(base_size = 25)+
    geom_histogram(binwidth=0.1, color="black", fill="white")+
    xlab("Meters per seconds")+
    ylab("Count")+
    facet_wrap(~Activity) 

ggsave(filename = here("parameters",
                       "fish_benthic_feed_speed",
                       "outputs",
                       "fish_benthic_feed_speed.png"),
       plot = speed_histogram,
       device = "png",
       dpi = 300,
       height = 5,
       width = 5)

##### Calculate the parameters #################################################
combined_data = thomas_2022 %>%
  select(Length,MeterPerSec) %>%
  mutate(Activity=case_when(MeterPerSec <= max_feed_speed ~ "Feed", 
                            MeterPerSec > max_feed_speed ~ "Move")) %>%
  mutate(Author = "Thomas2022") %>%
  bind_rows(kelly_et_al) %>% 
  # Divide by length and convert speed to cm/s
  mutate(LengthPerSecond = MeterPerSec*100/Length) 


# Only Thomas et al. 2021
combined_data %>% 
  filter(Author == "Thomas2022")%>%
  group_by(Activity)%>%
  summarise(AverageLengthPerSecond=mean(LengthPerSecond),
            ALPS.sd=sd(LengthPerSecond), 
            Samplesize=length(LengthPerSecond) )

# Only Kelly et al. 2007 and Hansen et al. 2022
average_speed = combined_data %>% 
  #group_by(Activity)%>%
  filter(Author == "Kelly2007" | Author == "Hansen2022") %>%
  summarise(AverageLengthPerSecond=mean(LengthPerSecond),
            ALPS.sd=sd(LengthPerSecond), 
            Samplesize=length(LengthPerSecond)) %>% 
  pull(AverageLengthPerSecond)

message(">>>>> The average speed is: ", average_speed, " body lenghts per second.")

################################################################################
# END
################################################################################


##### Analysis code kwan used to look at the thomas data #######################
# ##### Histograms of Delta ####################################################
# 
# # 1.1 Total
# 
#     # update the data
#     GS2013_summary1.2<-GS2013_summary %>%
#       filter(Activity=="Delta")
# 
#     # 2) If there are extreme gaps of days between sites. 
#     GS2013_summary1.2 %>%
#       filter(DeltaTimeMin>60*24*2)%>%
#       select(DeltaTimeMin)%>%
#       summary() 
#     # Mean time is 16939. Therefore this would be equivalent to 16939/60/24 11 days.
#     # And the minimum would be 2982/60/24 2.07 days
#     # Max days are 105525/60/24 73 days.
#     GS2013_summary1.2 %>%
#       filter(DeltaTimeMin>60*24*2)%>%
#       ggplot(aes(x=Distperday))+
#       geom_histogram()
#     
#     # update the data
#     GS2013_summaryD<-GS2013_summary1.2 %>%
#       filter(DeltaTimeMin<60*24*2)
#     
#     GS2013_summaryD %>%
#       ggplot(aes(x=Distperday))+
#       geom_histogram()
#     
#     GS2013_summaryD %>%
#       ggplot(aes(x=ALocationGen, y=Distperday))+
#       geom_boxplot()
#   
#     str(GS2013_summaryD)
# 
# #####################################
# #### 2. Histograms of Migration ####
# GS2013_summary %>%
#   filter(Activity=="Bay")%>%
#       # filter(MeterPerSec < 2.2 )%>% 
#       filter(DeltaTimeMin < 60*24*2)%>%
#   ggplot(aes(x=MeterPerSec))+
#   geom_histogram()
# 
#     GS2013_summaryM<-GS2013_summary %>%
#       filter(Activity=="Bay")%>%
#       filter(MeterPerSec < 2.2 )%>% 
#       filter(DeltaTimeMin < 60*24*2)
# 
#     GS2013_summaryM %>%
#       select(MeterPerSec) %>%
#       summary()
#     
# #############################################
# ######## 3. Statistical analyses ############     
#     GS2013_combined<-rbind(GS2013_summaryD, GS2013_summaryM)
#     str(GS2013_combined)
#     GS2013_combined %>%
#       group_by(Activity)%>%
#       summarise(averageMPS=mean(MeterPerSec))
# 
#     # To see whether the Migration vs. the delta fish have different speed
#     m1.1<-glmer(MeterPerSec~1      +(1|ALocationGen)+(1|Fish),data=GS2013_combined,family=Gamma)
#     m1.2<-glmer(MeterPerSec~Activity+Length+Dist_km+(1|ALocationGen)+(1|Fish),data=GS2013_combined,family=Gamma)
#     m1.3<-glmer(MeterPerSec~Activity+      +Dist_km+(1|ALocationGen)+(1|Fish),data=GS2013_combined,family=Gamma)
#     m1.4<-glmer(MeterPerSec~Activity+Length+       +(1|ALocationGen)+(1|Fish),data=GS2013_combined,family=Gamma)
#     m1.5<-glmer(MeterPerSec~Activity+               (1|ALocationGen)+(1|Fish),data=GS2013_combined,family=Gamma)
# 
#     m2.1<-glmer(MeterPerSec~1      +(1|Fish),data=GS2013_combined,family=Gamma)
#     m2.2<-glmer(MeterPerSec~Activity+Length+Dist_km+ALocationGen+(1|Fish),data=GS2013_combined,family=Gamma)
#     m2.3<-glmer(MeterPerSec~Activity+Length+       +ALocationGen+(1|Fish),data=GS2013_combined,family=Gamma)
#     m2.4<-glmer(MeterPerSec~Activity+      +Dist_km+ALocationGen+(1|Fish),data=GS2013_combined,family=Gamma)
#     m2.5<-glmer(MeterPerSec~Activity+Length+Dist_km+            +(1|Fish),data=GS2013_combined,family=Gamma)
#     m2.6<-glmer(MeterPerSec~Activity+              +ALocationGen+(1|Fish),data=GS2013_combined,family=Gamma)
#     m2.7<-glmer(MeterPerSec~Activity+       +Dist_km+           +(1|Fish),data=GS2013_combined,family=Gamma)
#     m2.8<-glmer(MeterPerSec~Activity+Length                     +(1|Fish),data=GS2013_combined,family=Gamma)
# 
#     AIC(m1.1,m1.2,m1.3,m1.4,m1.5,m2.1,m2.2,m2.3,m2.4,m2.5,m2.6,m2.7,m2.8)
#     
#     # There's not really a difference in speed between the Migrating ones and the delta ones.
#     # THerefore, I will just combine them together.
#       
# ########################################################
# ######## 4. Combine the two dataset + Histogram######### 
#     GS2013_combined<-rbind(GS2013_summaryD, GS2013_summaryM)
#     str(GS2013_combined)
#     
#     GS2013_combined %>%
#       ggplot(aes(x=Fish, y=MeterPerSec, color=Activity))+
#       geom_boxplot(aes(color=Activity))
# 
#     
#     (HistogramMeterPerSec<-GS2013_combined %>%
#       ggplot(aes(x=MeterPerSec, fill=Activity,color=Activity)) + #, color=Activity))+ # color=Activity
#       theme_classic(base_size = 25)+
#       geom_histogram(binwidth=0.1, color="black",fill="white")+
#       xlab("Meters per seconds")+
#       ylab("Count")+
#       facet_wrap(~Activity) )
#     
#       ggsave(filename = here("HistogramMeterPerSec.png"),
#            plot = HistogramMeterPerSec,
#            device = "png",
#            dpi = 300,
#            height = 5,
#            width = 5)
#       
#       # Hitogram without the facet wrap
#       (GS2013_combined %>%
#           #filter(Activity=="Delta")%>%
#           ggplot(aes(x=MeterPerSec, fill=Activity,color=Activity)) + #, color=Activity))+ # color=Activity
#           theme_classic(base_size = 25)+
#           #theme(legend.position = c(0.8, 0.8)) +
#           #scale_color_manual(values = cbPalette, name = "Activity")+
#           geom_histogram(binwidth=0.1, color="black",fill="white")+
#           xlab("Meters per seconds")+
#           ylab("Count") )
#          
# 
#     GS2013_combined %>%
#       ggplot(aes(x=ALocationGen, y=MeterPerSec))+
#       geom_boxplot()
#    
#     GS2013_combined %>%
#       select(Dist_km) %>%
#       summary()
#     
#         # It's really not clear whether the speed is different across various areas.
# 
# ##############################################################
# ############# 5. Calculate Speed per body length #############     
# 
# # 5.1 Add Kelly et al. Hansen et al. 2012 data 
#  Kelly2007Fsize<-mean(c(105,105,101,102,153,106))
#     Kelly2007Fspeed<-0.21
#  Kelly2012Fsize<-mean(c(105,105,101,153,106))
#     Kelly2012Fspeed<-0.6
#  Hansen2022size<-mean(c(14.1,29.4)) 
#     Hansen2022speed<-mean(c(0.0027,0.094))
#   
#     
# GS2013_combined2<-GS2013_combined %>%
#   select(Length,MeterPerSec) %>%
#   mutate(Activity=case_when(MeterPerSec<=0.60~ "Feed", # since Kelly et al. 2012 says 0.6 is also feeding mode..
#                             MeterPerSec>0.60~ "Move"))%>%
#   mutate(Author="Thomas2022")%>%
#   add_row(Length=Kelly2007Fsize, MeterPerSec=Kelly2007Fspeed,Activity="Feed",Author="Kelly2007") %>%
#   add_row(Length=Kelly2012Fsize, MeterPerSec=Kelly2012Fspeed,Activity="Feed",Author="Kelly2012") %>%
#   add_row(Length=Hansen2022size, MeterPerSec=Hansen2022speed,Activity="Feed",Author="Hansen2022") %>%
#   mutate(LengthPerSecond=MeterPerSec*100/Length) #<_ LengthPerSecond MULTIPLY  100 since MeterPErSec is in cm unit
#   
# 
# # 5.2 Calculate Length per second for each activity mode (feed vs travel)
# 
# # 5.2.1 Only THomas et al. 2021
# GS2013_combined2 %>% 
#   filter(Author == "Thomas2022")%>%
#   group_by(Activity)%>%
#   summarise(AverageLengthPerSecond=mean(LengthPerSecond),
#             ALPS.sd=sd(LengthPerSecond), 
#             Samplesize=length(LengthPerSecond) )
# 
# # Result: 
# # 0.541 /s vs 2.29/s 
# 
# # 5.2.1 Only Kelly et al. 2007 and Hansen et al. 2022
#   GS2013_combined2 %>% 
#     #group_by(Activity)%>%
#     filter(Author == "Kelly2007" | Author == "Hansen2022") %>%
#   summarise(AverageLengthPerSecond=mean(LengthPerSecond),
#             ALPS.sd=sd(LengthPerSecond), 
#             Samplesize=length(LengthPerSecond) )
#   
#   # Result: 
#   # 0.2048994
# 
# 
# 
# # 5.3 Calculate average MeterPerSec for each activity (since we reclassified between 0.6)
# GS2013_combined2 %>% 
#   group_by(Activity)%>%
#   summarise(AverageMeterPerSec=mean(MeterPerSec),
#             AMPS.sd=sd(MeterPerSec), 
#             Samplesize=length(MeterPerSec) )
# 
# 
# 
# # 5.4 etc. Plot Length vs MeterPerSec
# GS2013_combined2 %>%
#   filter(Activity=="Feed")%>% # Change mode to "Travel" is needed
#   group_by(Length) %>%
#   summarise(MeanSpeed=mean(MeterPerSec))%>%
#   ggplot(aes(x=Length, y=MeanSpeed))+
#   geom_point()+
#   ylab("MeterPerSec")


