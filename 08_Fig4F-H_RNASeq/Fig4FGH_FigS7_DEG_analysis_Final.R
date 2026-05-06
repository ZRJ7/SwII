# CLean Env ----
rm(list = ls())

# Load packages -----
library("DESeq2")
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("kableExtra")
library("ggrepel") 
library("plotrix")
library(extrafont)
font_import()
loadfonts()

################################################

################################################
# DE analysis -----

# load expression data 
counts_data <- read.csv("Fig4F_gene_count_matrix.csv",
                        row.names = "gene_id")
head(counts_data)
str(counts_data)
colnames(counts_data)


# load sample info

colData <- read.csv("Fig4F_Phenotype.csv", 
                    row.names = "ID")
colData$Phe <- as.factor(colData$Phe)
head(colData)
str(colData)
rownames(colData)

# Making sure the row names in colData matches to column names in counts_data
# Are they in same order?
all(colnames(counts_data) == rownames(colData)) 

# Step 2: Construct a DESqeDataSet object -----

dds <- DESeqDataSetFromMatrix(countData = counts_data,
                              colData = colData,
                              design = ~Phe)

dds


# pre-filtering: removing rows with low gene counts

# keeping rows that have at least 8 reads in total 
keep <- rowSums(counts(dds)) >= 8  

dds.keep <- dds[keep,]

dds.keep # only 17073 genes left

# set the factor level 
# long-wing as background

dds.keep$Phe <- relevel(dds$Phe, ref = "Lw")

# Step 3: Run DESeq -----
dds.keep <- DESeq(dds.keep)

res <- results(dds.keep)

res

# explore results -----
summary(res)

# set the padj as < 0.05
res0.05 <- results(dds.keep, alpha = 0.05)

summary(res0.05)

sum(res0.05$padj <0.05, na.rm =TRUE)

# contrasts
resultsNames(dds.keep)

# PCA
vsd <- vst(dds.keep, blind = FALSE)
plotPCA(vsd, intgroup="Phe")

# LogFC Correction 
contrast <- c("Phe", "Lw", "Sw")
ddlfc <- lfcShrink(dds.keep, contrast =contrast, res=res, type="ashr")
plotMA(ddlfc, ylim=c(-5,5))

# export result of DEGs
DEG <- ddlfc %>%
  as.data.frame() %>%
  rownames_to_column("gene_id")

# filter genes with lfc >1 and p<0.05 --- 1,233 genes
DEG_filtered_1_0.05 <- DEG %>%  
  dplyr::filter((log2FoldChange>=1 | log2FoldChange <= (-1)) & padj <0.05)

# Export all filtered genes ----
DEG.na.omit <- na.omit(DEG) # 16078 genes in total

# export 
# write_csv(DEG.na.omit, "DE analysis_all fitered genes.csv")

gene.name.1 <- read.csv("Fig4F_Volcano plot annotated.csv") # annotate ApoD and cuticle-realted DEGs

gene.name.1$Gene_Name <- as.factor(gene.name.1$Gene_Name)

# plot ----
fig.volcano <-
  ggplot(gene.name.1, aes(log2FoldChange, -log(padj,10))) +
  geom_point(aes(color= Gene_Name), size =2, alpha = 1) +
  ylab(expression("-log"[10]*"(adjusted p-value)")) +
  xlab(expression("log"[2]*"(fold-change)")) +
  scale_color_manual(labels = c("others", "ApoD", "Cuticle-related"),
                     values = c('gray70', "#E41A1C", "royalblue")) + 
  guides(colour = guide_legend(override.aes = list(size=1.5))) +
  xlim(-11,11)+
  ylim(0,150) +
  geom_vline(xintercept=c(-1,1), linetype="dashed") + 
  geom_hline(yintercept=1.3, linetype="dashed") + 
  theme_test() +
  theme(text = element_text(family = "Arial", size = 16)
        ,legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "none") +
  # Annotation of cuticle-development related DEGs
  geom_point(data = subset(gene.name.1, Gene_Name == "Cu"),
             aes(color = Gene_Name), size = 2) 

fig.volcano

ggsave("volcano_plot.pdf", fig.volcano, height = 6, width = 6, dpi = 600)


# Enrichment analysis 
# Plot ----
# GO
selected5.go <- read.csv("Fig4GH_Final_SelectedGO.csv")

# order the factors
selected5.go <- selected5.go %>% filter(Selected == "Y") %>%
  arrange(-padj_BH, .by_group = T)


selected5.go$GO_Name <-
  factor(selected5.go$GO_Name, levels = selected5.go$GO_Name)


GO <-
  selected5.go  %>%
  ggplot(aes(
    x = -log10(padj_BH),
    y = GO_Name,
    fill = Class
  )) +
  scale_fill_manual(values = c("#88E0C2", "#CBC3E3")) +
  geom_col(width = 0.8) +
  geom_text(aes(
    x = 0.1,
    y = GO_Name,
    label = GO_Name,
    size = 3.5,
    hjust = 0
  )) +
  theme_classic(base_size = 16) +
  ylab("") +
  xlab("-log10(P)") +
  scale_x_continuous(breaks = c(0, 5, 10, 15), limits = c(0, 15)) +
  facet_wrap( ~ Class, ncol = 1, scales = "free_y") +
  theme(
    text = element_text("Arial", size = 18),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold.italic"),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y  = element_blank(),
    axis.line.x  = element_blank(),
    legend.position = "none"
  )

GO

################################################################################
# KEGG

KEGG <- read_csv("Fig4GH_Final_Top10_KEGG.csv")
KEGG <- KEGG[order(KEGG$padj_BH, decreasing = T),]
KEGG$`Term Name` <-
  factor(KEGG$`Term Name`, levels = KEGG$`Term Name`)

View(KEGG)
head(KEGG)


kegg <-
  ggplot(KEGG, aes(x = -log10(padj_BH), y = `Term Name`)) +
  geom_bar(stat = "identity",
           width = 0.8,
           fill = 'lightblue') +
  geom_text(aes(
    x = 0.1,
    y = `Term Name`,
    label = `Term Name`,
    size = 3.5,
    hjust = 0
  )) +
  theme_classic() +
  theme(
    text = element_text("Arial", size = 18),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold.italic"),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y  = element_blank(),
    axis.line.x  = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "KEGG Enrichment",
       size = 24,
       fontface = 'bold')

kegg

ggsave(
  "kegg.05Apr2026.pdf",
  kegg,
  dpi = 600,
  height = 5,
  width = 5
)

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
# [1] stats4    stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] extrafont_0.19              plotrix_3.8-4               kableExtra_1.4.0           
# [4] ggrepel_0.9.5               lubridate_1.9.3             forcats_1.0.0              
# [7] stringr_1.5.1               dplyr_1.1.4                 purrr_1.0.2                
# [10] readr_2.1.5                 tidyr_1.3.1                 tibble_3.2.1               
# [13] ggplot2_3.5.1               tidyverse_2.0.0             DESeq2_1.40.2              
# [16] SummarizedExperiment_1.30.2 Biobase_2.62.0              MatrixGenerics_1.12.3      
# [19] matrixStats_1.3.0           GenomicRanges_1.52.1        GenomeInfoDb_1.38.5        
# [22] IRanges_2.36.0              S4Vectors_0.40.2            BiocGenerics_0.48.1        
# 
# loaded via a namespace (and not attached):
# [1] tidyselect_1.2.1        viridisLite_0.4.2       farver_2.1.2           
# [4] bitops_1.0-8            fastmap_1.1.1           RCurl_1.98-1.14        
# [7] digest_0.6.37           timechange_0.3.0        lifecycle_1.0.4        
# [10] magrittr_2.0.3          compiler_4.3.0          rlang_1.1.4            
# [13] tools_4.3.0             utf8_1.2.4              knitr_1.45             
# [16] labeling_0.4.3          S4Arrays_1.0.6          bit_4.0.5              
# [19] DelayedArray_0.26.7     xml2_1.3.6              abind_1.4-5            
# [22] BiocParallel_1.36.0     withr_3.0.1             grid_4.3.0             
# [25] fansi_1.0.6             colorspace_2.1-1        extrafontdb_1.0        
# [28] scales_1.3.0            cli_3.6.3               rmarkdown_2.26         
# [31] crayon_1.5.2            generics_0.1.3          rstudioapi_0.16.0      
# [34] tzdb_0.4.0              zlibbioc_1.48.0         parallel_4.3.0         
# [37] XVector_0.42.0          vctrs_0.6.5             Matrix_1.6-5           
# [40] hms_1.1.3               bit64_4.5.2             systemfonts_1.0.6      
# [43] locfit_1.5-9.9          glue_1.7.0              codetools_0.2-20       
# [46] stringi_1.8.4           gtable_0.3.5            munsell_0.5.1          
# [49] pillar_1.9.0            htmltools_0.5.8.1       GenomeInfoDbData_1.2.11
# [52] R6_2.5.1                vroom_1.6.5             evaluate_0.23          
# [55] lattice_0.22-6          Rcpp_1.0.13             svglite_2.1.3          
# [58] Rttf2pt1_1.3.12         xfun_0.43               pkgconfig_2.0.3        