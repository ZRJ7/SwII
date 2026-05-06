# clean env----
rm(list = ls())

# Packages----
library(ggplot2)
library(dplyr)
library(ggthemes)
library(survival)
library(survminer)
library(extrafont)
library(lmodel2)
library(cowplot)
loadfonts()

# input longevity
longevity <- read.csv("Fig5AB_Longevity_Final.csv") 

View(longevity)

str(longevity)

longevity$Sex <- factor(longevity$Sex)
longevity$Phenotype <- factor(longevity$Phenotype)
longevity$Box <- factor(longevity$Box)
longevity$Life_span <- as.numeric(longevity$Life_span)
longevity$Weight_Adu <- as.numeric(longevity$Weight_Adu)

str(longevity)

#longevity <- longevity %>% filter(Life_span > 10) # 

# longevity by sex -----
longevity %>% filter(Sex == "F") -> female
longevity %>% filter(Sex == "M") -> male

m.pro <- male %>% mutate(Ave_pro = (Pro_Adu1 + Pro_Adu2 + Pro_Adu3)/3)
f.pro <- female %>% mutate(Ave_pro = (Pro_Adu1 + Pro_Adu2 + Pro_Adu3)/3)

# Survival analysis ----
# Female ----
# Calculate SMI ----
lmodel2(log(Weight_Adu)~log(Ave_pro), data = f.pro)

# non-removal
fbSMA = 2.259970

# 3. Estimate the mean length
fx0 = mean(f.pro$Ave_pro)

fx0 #3.749273     

# 4. Calculate SMI for Female
fSMI = f.pro$Weight_Adu*(fx0/f.pro$Ave_pro)^fbSMA * 1000

f.pro$SMI <- fSMI 

# construct survival status death = 1; survival = 0
f.pro$status <- 1  

surv_obj.f <- Surv(time = f.pro$Life_span, event = f.pro$status)


# coxph_model.f.normal <- coxph(
#                         surv_obj.f ~ Phenotype + SMI,
#                         data = f.pro
#                               )
# 
# summary(coxph_model.f.normal)

coxph_model.f.cluster <- coxph(
                              surv_obj.f ~ Phenotype + SMI + cluster(Box),
                              data = f.pro
                              )

summary(coxph_model.f.cluster)

# male ----
# all data ----
# Calculate SMI ----
lmodel2(log(Weight_Adu)~log(Ave_pro), data = m.pro)

mbSMA = 2.292413

# 3. Estimate the mean length
mx0 = mean(m.pro$Ave_pro)

mx0 #3.856518 

# 4. Calculate SMI for Male
mSMI = m.pro$Weight_Adu*(mx0/m.pro$Ave_pro)^mbSMA *1000

m.pro$SMI <- mSMI #

# construct survival status death = 1; survival = 0
m.pro$status <- 1  

surv_obj.m <- Surv(time = m.pro$Life_span, event = m.pro$status)

# coxph_model.m.normal <- coxph(
#                               surv_obj.m ~ Phenotype + SMI,
#                               data = m.pro
#                              )
# 
# summary(coxph_model.m.normal)

coxph_model.m.cluster <- coxph(
                              surv_obj.m ~ Phenotype + SMI + cluster(Box),
                              data = m.pro
                              )

summary(coxph_model.m.cluster)


# plots
# Male
fit.male <- survfit(Surv(Life_span) ~ Phenotype, data = m.pro)

p.male <- ggsurvplot(fit.male, conf.int = T,
                     conf.int.alpha = 0.15,
                     xlim = c(0, 150),
                     font.main = 16,
                     expand = c(0, 0.05),
                     longevity = m.pro,
                     legend.labs = c("Lw", "Sw"),
                     ggtheme = theme_classic(),
                     xlab = "Time (days)",
                     legend.title = "",
                     pval = FALSE,
                     risk.table = T,
                     tables.height =  0.25,
                     tables.theme = theme_few(),
                     tables.col ="strata",
                     font.family = "Arial",
                     # risk.table.title = "Risk table",
                     risk.table.pos = "out",
                     # color palettes. 
                     palette = c("#e97a42", "#afbde0"),
                     font.x = c(14, "bold.italic", "red"),  #"red" "darkred"
                     font.y = c(14, "bold.italic", "red"),
                     font.tickslab = c(12, "plain", "black"),  
                     break.time.by = 30
) 

p.male

pdf("KM_male_2026Apr09.pdf", width = 5, height = 5)
print(p.male, newpage = FALSE)
dev.off()

# Female
fit.female <- survfit(Surv(Life_span) ~ Phenotype, data = f.pro)

pfemale <- ggsurvplot(fit.female, conf.int = T,
                      conf.int.alpha = 0.15,
                      legend.labs = c("Lw", "Sw"),
                      ggtheme = theme_classic(),
                      legend.title = "",
                      xlab = "Time (days)",
                      pval = FALSE,
                      # add risk table 
                      risk.table = TRUE,
                      tables.height =  0.25,
                      tables.theme = theme_few(),
                      tables.col ="strata",
                      font.family = "Arial",
                      # color palettes. 
                      palette = c("#9cc37b", "#106ab3"),
                      font.x = c(14, "bold.italic", "red"),  #"red" #darkred
                      font.y = c(14, "bold.italic", "red"),
                      font.tickslab = c(12, "plain", "black"),  
                      break.time.by = 30
                      )

pfemale

pdf("KM_female_2026Apr09.pdf", width = 5, height = 5)
print(pfemale, newpage = FALSE)
dev.off()

fit.male <- survfit(Surv(Life_span) ~ Phenotype, data = m.pro)

p.male <- ggsurvplot(fit.male, conf.int = F,
                     xlim = c(0, 150),
                     font.main = 16,
                     expand = c(0, 0.05),
                     longevity = m.pro,
                     legend.labs = c("Lw", "Sw"),
                     ggtheme = theme_classic(),
                     xlab = "Time (days)",
                     legend.title = "",
                     pval = FALSE,
                     risk.table = T,
                     tables.height =  0.25,
                     tables.theme = theme_few(),
                     tables.col ="strata",
                     font.family = "Arial",
                     # risk.table.title = "Risk table",
                     risk.table.pos = "out",
                     # color palettes. 
                     palette = c("#e97a42", "#afbde0"),
                     font.x = c(14, "bold.italic", "red"),
                     font.y = c(14, "bold.italic", "darkred"),
                     font.tickslab = c(12, "plain", "black"),  
                     break.time.by = 30
                     ) 

p.male

pdf("male4_23Mar2025.pdf", width = 5, height = 5)
print(p.male, newpage = FALSE)
dev.off()

# Body mass analysis ----
# read data ----
weight_change <- read.csv("Fig5DE_Bodymass_summary.csv")

str(weight_change)

weight_change$Phenotype <- factor(weight_change$Phenotype)

weight_change$Time <- factor(weight_change$Time,
                             levels = c("W0",
                                        "W2",
                                        "W4",
                                        "W6",
                                        "W8",
                                        "W10"))



std_mean <- function(x) sd(x)/sqrt(length(x))

F.w_change <-
  weight_change %>% 
  filter(Phenotype!= "Sw_M", Phenotype!= "Lw_M") %>%
  ggplot(aes(x = Time, y = Mean*1000, color = Phenotype, group=Phenotype)) +
  geom_point(size = 2) +
  geom_line(linetype = "dashed") + 
  scale_color_manual(values = c("Lw_F"="#9cc37b",
                                "Lw_M"="#094c8b",
                                "Sw_F"="#106ab3",
                                "Sw_M"="#f27c79")) +
  geom_errorbar(aes(ymin = (Mean-Se)*1000,
                    ymax = (Mean+Se)*1000),
                width = 0.15) +
  theme_classic() +
  xlab("Weeks post-eclosion") +
  ylab("Average body mass (mg)") +
  theme(text = element_text("Arial",size = 16),
        legend.position = "none") +
  ylim(400, 1000) +
  annotate(geom = "text", x = "W0", y = 530,
           label = "ns", size = 4) +
  annotate(geom = "text", x = "W2", y = 750,
           label = "****", size = 6) +
  annotate(geom = "text", x = "W4", y = 830,
           label = "****", size = 6) +
  annotate(geom = "text", x = "W6", y = 880,
           label = "***", size = 6) +
  annotate(geom = "text", x = "W8", y = 940,
           label = "****", size = 6) +
  annotate(geom = "text", x = "W10", y = 990,
           label = "****", size = 6)

F.w_change

M.w_change <-
  weight_change %>% 
  filter(Phenotype!= "Sw_F", Phenotype!= "Lw_F") %>%
  ggplot(aes(x = Time, y = Mean*1000, color = Phenotype, group=Phenotype)) +
  geom_point(size = 2) +
  geom_line(linetype = "dashed") +
  scale_color_manual(values = c("Lw_F"="#9cc37b",
                                "Lw_M"="#e97a42",
                                "Sw_F"="#106ab3",
                                "Sw_M"="#afbde0")) +
  geom_errorbar(aes(ymin = (Mean-Se)*1000,
                    ymax = (Mean+Se)*1000),
                width = 0.15) +
  theme_classic() +
  xlab("Weeks post-eclosion") +
  ylab("Average body mass (mg)") +
  theme(text = element_text("Arial",size = 16),
        legend.position = "none") +
  ylim(400, 1000) +
  annotate(geom = "text", x = "W0", y = 580,
           label = "ns", size = 4) +
  annotate(geom = "text", x = "W2", y = 620,
           label = "ns", size = 4) +
  annotate(geom = "text", x = "W4", y = 610,
           label = "ns", size = 4) +
  annotate(geom = "text", x = "W6", y = 600,
           label = "ns", size = 4) +
  annotate(geom = "text", x = "W8", y = 590,
           label = "ns", size = 4) +
  annotate(geom = "text", x = "W10", y = 580,
           label = "ns", size = 4)

M.w_change 

# Plot each age ----
weight2 <- read.csv("Fig5DE_Bodymass_Full record.csv")
str(weight2)

weight2$Sex <- as.factor(weight2$Sex)

weight2$Phenotype <- as.factor(weight2$Phenotype)

weight2$Time_of_weight <- as.factor(weight2$Time_of_weight)

weight2$Phenotype_sex <- paste(weight2$Phenotype, weight2$Sex, sep = "_")

str(weight2)

# Male ----

# W0
weight2 %>% filter(Time_of_weight == "W0") %>% filter(Sex == "M") -> male.w0

male.w0 %>% filter(Phenotype == "Lw") ->w0.m.Lw
male.w0 %>% filter(Phenotype == "Sw") ->w0.m.Sw

shapiro.test(w0.m.Lw$Weight)
shapiro.test(w0.m.Sw$Weight)

var.test(w0.m.Lw$Weight, w0.m.Sw$Weight)

t.test(Weight ~ Phenotype, data = male.w0, var.equal = TRUE) #p>0.05

m.w0 <- 
  male.w0 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(300,800) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 800,
           label = "ns", size = 8) +
  theme_test() +
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 0") 

m.w0

# W2
weight2 %>% filter(Time_of_weight == "W2") %>% filter(Sex == "M") -> male.w2

male.w2 %>% filter(Phenotype == "Lw") ->w2.m.Lw
male.w2 %>% filter(Phenotype == "Sw") ->w2.m.Sw

shapiro.test(w2.m.Lw$Weight)
shapiro.test(w2.m.Sw$Weight) # p<0.05

wilcox.test(Weight ~ Phenotype, data = male.w2)  # p > 0.05

m.w2 <- male.w2 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(300,800) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 800,
           label = "ns", size = 8) +
  theme_test() +
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 2") 

m.w2

# W4
weight2 %>% filter(Time_of_weight == "W4") %>% filter(Sex == "M") -> male.w4

male.w4 %>% filter(Phenotype == "Lw") ->w4.m.Lw
male.w4 %>% filter(Phenotype == "Sw") ->w4.m.Sw

shapiro.test(w4.m.Lw$Weight)
shapiro.test(w4.m.Sw$Weight) 

var.test(w4.m.Lw$Weight, w4.m.Sw$Weight)

t.test(Weight ~ Phenotype, data = male.w4, var.equal = TRUE) #p>0.05

m.w4 <- 
  male.w4 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(300,800) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 800,
           label = "ns", size = 8) +
  theme_test() +
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 4") 

m.w4

# W6
weight2 %>% filter(Time_of_weight == "W6") %>% filter(Sex == "M") -> male.w6

male.w6 %>% filter(Phenotype == "Lw") ->w6.m.Lw
male.w6 %>% filter(Phenotype == "Sw") ->w6.m.Sw

shapiro.test(w6.m.Lw$Weight)
shapiro.test(w6.m.Sw$Weight)  # p < 0.05

wilcox.test(Weight ~ Phenotype, data = male.w6)  # p > 0.05

m.w6 <-
  male.w6 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(300,800) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 800,
           label = "ns", size = 8) +
  theme_test() +
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 6") 

m.w6

# W8
weight2 %>% filter(Time_of_weight == "W8") %>% filter(Sex == "M") -> male.w8

male.w8 %>% filter(Phenotype == "Lw") ->w8.m.Lw
male.w8 %>% filter(Phenotype == "Sw") ->w8.m.Sw

shapiro.test(w8.m.Lw$Weight)
shapiro.test(w8.m.Sw$Weight) 

var.test(w8.m.Lw$Weight, w8.m.Sw$Weight)

t.test(Weight ~ Phenotype, data = male.w8, var.equal = TRUE) #p>0.05

m.w8 <-
  male.w8 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(300,800) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 800,
           label = "ns", size = 8) +
  theme_test() +
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 8") 
m.w8

# W10
weight2 %>% filter(Time_of_weight == "W10") %>% filter(Sex == "M") -> male.w10

male.w10 %>% filter(Phenotype == "Lw") ->w10.m.Lw
male.w10 %>% filter(Phenotype == "Sw") ->w10.m.Sw

shapiro.test(w10.m.Lw$Weight)
shapiro.test(w10.m.Sw$Weight) 

var.test(w10.m.Lw$Weight, w10.m.Sw$Weight)

t.test(Weight ~ Phenotype, data = male.w10, var.equal = TRUE) #p>0.05

m.w10 <-
  male.w10 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(300,800) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 800,
           label = "ns", size = 8) +
  theme_test() +
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 10") 

m.w10

# plot all ----
m.all <-
  plot_grid(
    m.w0, m.w2, m.w4,
    m.w6, m.w8, m.w10,
    ncol = 3,
    nrow = 2,
    labels = c("A", "B", "C", "D", "E", "F"),
    label_size = 16)

m.all

ggsave("male.all.time.2026Apr09.pdf", m.all, height = 10, width = 12, dpi = 600)

# Female ----
# W0
weight2 %>% filter(Time_of_weight == "W0") %>% filter(Sex == "F") -> female.w0

female.w0 %>% filter(Phenotype == "Lw") ->w0.f.Lw
female.w0 %>% filter(Phenotype == "Sw") ->w0.f.Sw

shapiro.test(w0.f.Lw$Weight)
shapiro.test(w0.f.Sw$Weight)

var.test(w0.f.Lw$Weight, w0.f.Sw$Weight)

t.test(Weight ~ Phenotype, data = female.w0, var.equal = TRUE) #p>0.05

f.w0 <-
  female.w0 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(400,1200) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 1200,
           label = "ns", size = 8) +
  theme_test() +
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 0") 

f.w0

# W2
weight2 %>% filter(Time_of_weight == "W2") %>% filter(Sex == "F") -> female.w2

female.w2 %>% filter(Phenotype == "Lw") ->w2.f.Lw
female.w2 %>% filter(Phenotype == "Sw") ->w2.f.Sw

shapiro.test(w2.f.Lw$Weight)
shapiro.test(w2.f.Sw$Weight) 


var.test(w2.f.Lw$Weight, w2.f.Sw$Weight)

t.test(Weight ~ Phenotype, data = female.w2, var.equal = TRUE) #p<0.0001 2.698e-06

f.w2 <-
  female.w2 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(400,1200) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 1200,
           label = "P < 0.0001", size = 6, fontface = "italic") +
  theme_test()+
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 2") 


f.w2

# W4
weight2 %>% filter(Time_of_weight == "W4") %>% filter(Sex == "F") -> female.w4

female.w4 %>% filter(Phenotype == "Lw") ->w4.f.Lw
female.w4 %>% filter(Phenotype == "Sw") ->w4.f.Sw

shapiro.test(w4.f.Lw$Weight)
shapiro.test(w4.f.Sw$Weight) 

var.test(w4.f.Lw$Weight, w4.f.Sw$Weight)

t.test(Weight ~ Phenotype, data = female.w4, var.equal = TRUE) #p<0.0001 p-value = 6.047e-05

f.w4 <-
  female.w4 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(400,1200) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 1200,
           label = "P < 0.0001", size = 6, fontface = "italic") +
  theme_test()+
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 4") 

f.w4

# W6
weight2 %>% filter(Time_of_weight == "W6") %>% filter(Sex == "F") -> female.w6

female.w6 %>% filter(Phenotype == "Lw") ->w6.f.Lw
female.w6 %>% filter(Phenotype == "Sw") ->w6.f.Sw

shapiro.test(w6.f.Lw$Weight)
shapiro.test(w6.f.Sw$Weight) 

var.test(w6.f.Lw$Weight, w6.f.Sw$Weight)

t.test(Weight ~ Phenotype, data = female.w6, var.equal = TRUE) #p<0.001 0.0002843

f.w6 <-
  female.w6 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(400,1200) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 1200,
           label = "P < 0.001", size = 6, fontface = "italic") +
  theme_test()+
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 6") 


f.w6

# W8
weight2 %>% filter(Time_of_weight == "W8") %>% filter(Sex == "F") -> female.w8

female.w8 %>% filter(Phenotype == "Lw") ->w8.f.Lw
female.w8 %>% filter(Phenotype == "Sw") ->w8.f.Sw

shapiro.test(w8.f.Lw$Weight)
shapiro.test(w8.f.Sw$Weight) 

var.test(w8.f.Lw$Weight, w8.f.Sw$Weight)

t.test(Weight ~ Phenotype, data = female.w8, var.equal = TRUE) #p<0.0001 4.147e-05

f.w8 <-
  female.w8 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(400,1200) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 1200,
           label = "P < 0.0001", size = 6, fontface = "italic") +
  theme_test() +
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 8") 

f.w8

# W10
weight2 %>% filter(Time_of_weight == "W10") %>% filter(Sex == "F") -> female.w10

male.w10 %>% filter(Phenotype == "Lw") ->w10.f.Lw
male.w10 %>% filter(Phenotype == "Sw") ->w10.f.Sw

shapiro.test(w10.f.Lw$Weight)
shapiro.test(w10.f.Sw$Weight) 

var.test(w10.f.Lw$Weight, w10.f.Sw$Weight)

t.test(Weight ~ Phenotype, data = female.w10, var.equal = TRUE) #p<0.0001 4.913e-05

f.w10 <-
  female.w10 %>%
  ggplot(aes(x = Phenotype_sex, y = Weight*1000, fill = Phenotype)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2) +
  ylim(400,1200) +
  ylab("Body mass (mg)") +
  annotate(geom = "text", 
           x = 1.5, 
           y = 1200,
           label = "P < 0.0001", size = 6, fontface = "italic") +
  theme_test() +
  theme(text = element_text("Arial",size = 16),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(title = "Week 10") 

f.w10

# plot all ----
f.all <-
  plot_grid(
    f.w0, f.w2, f.w4,
    f.w6, f.w8, f.w10,
    ncol = 3,
    nrow = 2,
    labels = c("A", "B", "C", "D", "E", "F"),
    label_size = 16)

f.all

ggsave("female.all.time.2026Apr09.pdf", f.all, height = 10, width = 12, dpi = 600)

sessionInfo()
# R version 4.3.0 (2023-04-21)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS 26.4
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: Asia/Shanghai
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] cowplot_1.1.3   ggthemes_5.1.0  survminer_0.4.9 ggpubr_0.6.0    lmodel2_1.7-3  
# [6] extrafont_0.19  readxl_1.4.3    survival_3.6-4  dplyr_1.1.4     ggplot2_3.5.1  
# 
# loaded via a namespace (and not attached):
# [1] utf8_1.2.4        generics_0.1.3    tidyr_1.3.1       xml2_1.3.6        rstatix_0.7.2    
# [6] stringi_1.8.4     lattice_0.22-6    hms_1.1.3         extrafontdb_1.0   magrittr_2.0.3   
# [11] grid_4.3.0        cellranger_1.1.0  Matrix_1.6-5      backports_1.4.1   ggtext_0.1.2     
# [16] gridExtra_2.3     purrr_1.0.2       fansi_1.0.6       scales_1.3.0      abind_1.4-5      
# [21] cli_3.6.3         KMsurv_0.1-5      rlang_1.1.4       commonmark_1.9.1  munsell_0.5.1    
# [26] splines_4.3.0     withr_3.0.1       tools_4.3.0       tzdb_0.4.0        ggsignif_0.6.4   
# [31] colorspace_2.1-1  km.ci_0.5-6       broom_1.0.5       vctrs_0.6.5       R6_2.5.1         
# [36] zoo_1.8-12        lifecycle_1.0.4   stringr_1.5.1     car_3.1-2         pkgconfig_2.0.3  
# [41] pillar_1.9.0      gtable_0.3.5      Rcpp_1.0.13       glue_1.7.0        data.table_1.17.8
# [46] xfun_0.43         tibble_3.2.1      tidyselect_1.2.1  rstudioapi_0.16.0 knitr_1.45       
# [51] farver_2.1.2      xtable_1.8-4      survMisc_0.5.6    labeling_0.4.3    carData_3.0-5    
# [56] readr_2.1.5       Rttf2pt1_1.3.12   compiler_4.3.0    markdown_1.12     gridtext_0.1.5 