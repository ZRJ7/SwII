#clean env -----
rm(list = ls())


# load r packages ----
library(ggplot2)
library(dplyr)
library(readr)
library(extrafont)
loadfonts()

# load data -----
r.size <- read_csv("Fig2C_Resonator size_Final.csv") 

str(r.size)

r.size$Phenotype <- factor(r.size$Phenotype, levels = c("Sw", "Lw"))

r.size %>%
  group_by(Phenotype) %>%
  summarise(
    mean = mean(Mean_Harp_Size),
    se = sd(Mean_Harp_Size) / sqrt(n())
  ) 


#  Phenotype  mean se
#1 Sw         5.64 0.0856
#2 Lw         8.35 0.112 

# Fig 2C
  r.size %>%
  ggplot(aes(x = Phenotype, y = Mean_Harp_Size, color = Phenotype)) +
  geom_boxplot(outlier.alpha = 0, size = 0.5, linewidth = 0.15, width = 0.35,) +
  geom_boxplot(linetype = "dashed", outlier.alpha = 0, size = 0.5, width = 0.35) +
  stat_boxplot(aes(ymin = after_stat(lower), ymax = after_stat(upper)),
               width = 0.35, outlier.alpha = 0) +
  stat_boxplot(geom = "errorbar",
               aes(ymin = after_stat(max)),
               width = 0.15) +
  stat_boxplot(geom = "errorbar",
               aes(ymax = after_stat(min)),
               width = 0.15) +
  geom_jitter(shape=16,position = position_jitter(0.15), 
              alpha = 0.5, size = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylim(3,12) +
  ylab("Resonator size (mm2)")+
  xlab("") +
  scale_color_manual(name = "Phenotype",
                     values = c("#998EC3","#F1A340")) +
  theme(text = element_text(size = 16, family = "Arial"),
        legend.position = "none")


# Statistical test
 r.size %>%
  group_by(Phenotype) %>%
  summarise(
    p = shapiro.test(Mean_Harp_Size)$p.value,
  ) 

var.test(Mean_Harp_Size ~ Phenotype, data = r.size)

t.test(r.size$Mean_Harp_Size~ r.size$Phenotype, var.equal = T)

sessionInfo()
R version 4.3.0 (2023-04-21)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS 26.4

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Asia/Shanghai
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
[1] compiler_4.3.0    tools_4.3.0       rstudioapi_0.16.0