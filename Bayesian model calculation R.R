Bikas<-read.csv("E:/NARC2/Bayesian model/Combine 77-78.csv", header = TRUE)
Bikas
dim(Bikas)
colnames(Bikas)
library(ProbBreed)
?ProbBreed
?bayes_met
Bikas$ENV=as.factor(Bikas$ENV)
Bikas$YEAR=as.factor(Bikas$YEAR)
Bikas$REP=as.factor(Bikas$REP)
Bikas$GEN=as.factor(Bikas$GEN)
mean_results <- aggregate(Bikas$Y.ha, by=list(GEN=Bikas$GEN), FUN=mean)
mean_results
mean_resultssd<- aggregate(Bikas$Y.ha, by=list(GEN=Bikas$GEN), FUN=sd)
mean_resultssd
combined_results <- merge(mean_results, mean_resultssd, by="GEN", suffixes = c("_mean", "_sd"))
combined_results
library(writexl)
getwd()
write_xlsx(combined_results, "mean_and_sd_results.xlsx")
##########to create the circular violooin plot 
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Ensure GEN is a factor
Bikas$GEN <- as.factor(Bikas$GEN)

# Create a custom color palette for 45 genotypes
num_genotypes <- length(unique(Bikas$GEN))
color_palette <- colorRampPalette(brewer.pal(n = 12, name = "Set1"))(num_genotypes)

# Calculate mean values for each GEN and arrange in ascending order
mean_values <- Bikas %>%
  group_by(GEN) %>%
  summarise(mean_Y = mean(Y.ha), .groups = 'drop') %>%
  arrange(mean_Y)

# Reorder GEN based on mean values
Bikas$GEN <- factor(Bikas$GEN, levels = mean_values$GEN)

# Create a circular box plot
Yield<-ggplot(Bikas, aes(x = GEN, y = Y.ha, fill = GEN)) +
  geom_boxplot() +
  coord_polar() +  # Convert to polar coordinates
  theme_minimal() +
  labs(title = "Box Plot of Yield/ha by GEN using DMRT test result", x = "Hybrids", y = "Yield/ha") +
  scale_fill_manual(values = color_palette) +  # Use custom color palette
  theme(axis.text.x = element_text(angle = 42, hjust = 1)) +  # Adjust text angle for better visibility
  geom_text(data = mean_values, aes(x = GEN, y = mean_Y, label = round(mean_Y, 1)), 
            position = position_nudge(y = 0.5),  # Adjust position for better visibility
            color = "black")
ggsave("Yield circular plot.jpg", width = 12, height = 7, dpi = 700)

####fitting the data sets on Bayesian model #########
mod = bayes_met(data = Bikas, 
                gen = "GEN", 
                loc = "ENV",
                repl = "REP",
                reg = NULL,
                year = "YEAR",
                res.het = TRUE,
                trait = "Y.ha",
                iter = 2000, cores = 0.0005, chains = 1)

outs = extr_outs(model = mod,
                 probs = c(0.05, 0.95), 
                 verbose = TRUE)
write.table(outs)

outs$variances
outs$ppcheck
outs$ppcheck
require(ggplot2)
getwd()
P1<-plot(outs, category = "density")
P1
ggsave("Density plot of maize datasets.png", plot = P1, width = 15, height = 15, dpi = 600)

P2<-plot(outs, category = "histogram")
P2
plot(outs)
ggsave("Histogram plot of maize datasets.png", plot = P2, width = 18, height = 18, dpi = 600)
library(gridExtra)
Combined<-grid.arrange(P1, P2, ncol = 2)
ggsave("Combined Density and Histogram plot.jpg", plot = Combined, width = 18, height = 18, dpi = 600)

P3<-plot(outs)
P3
hist<-plot(outs, category = "histogram")
hist
CP3nhist<-grid.arrange(P3, hist, ncol = 2)
ggsave("Emperical vs sample density plot.jpg", plot = P3, width = 18, height = 18, dpi = 600)
?extr_outs

results = prob_sup(extr = outs, 
                   int = .2,
                   increase = TRUE, 
                   save.df = FALSE, 
                   verbose = FALSE)
head(results$across$g_hpd)
results$across$g_hpd
plot(results, category = "hpd")
ggsave("HPD plot.jpg", plot = HPD, width = 15, height = 15, dpi = 600)
head(results$across$perfo)
results$across$perfo
PSP<-plot(results)
PSP
ggsave("Probability of the superir performance.jpg", plot = PSP, width = 15, height = 15, dpi = 600)
Combined<-grid.arrange(HPD, PSP, ncol= 2)
plot(Combined)
ggsave("Combined plot of HPD and Prob of superior performance.jpg", plot = Combined, width = 20, height = 18, dpi = 600)
PPSP<-plot(results, category = "pair_perfo")
PPSP
results$category = "pair_perfo"

ggsave("Pairwise probability of the superior performance.jpg", plot = PPSP, width = 15, height = 15, dpi = 600)

results$across$pair_perfo[1:5, 1:5]
head(results$across$stabi$gl)
head(results$across$stabi$gm)
PSS<-plot(results, category = "stabi")
results$across$stabi$gl
PSS
results$across$stabi$gl
results$across$stabi
ggsave("Probability of superior stablity.jpg", plot = PSS, width = 15, height = 15, dpi = 600)
Combined2<-grid.arrange(PPSP, PSS, ncol = 2)
Combined2
ggsave("Pairwise probability of the superior performance and prob stablity combined.jpg", plot = Combined2, width = 20, height = 20, dpi = 600)

results$across$pair_stabi$gl[1:5, 1:5]
plot(results, category = "pair_stabi")
results$across$pair_stabi$gl
PPSS<-plot(results, category = "pair_stabi")
PPSS
ggsave("Pairwise probability of superior stability.jpg", plot = PPSS, width = 15, height = 15, dpi = 600)

head(results$across$joint)
results$across$joint
JPPSPS<-plot(results, category = "joint")
JPPSPS
ggsave("Joint probability of superior performance and superior stability.jpg", plot = JPPSPS, width = 15, height = 15, dpi = 600)

head(results$within$perfo$gl)
results$within$perfo$gl

PSPWE<-plot(results, category = "perfo", level = "within")
PSPWE
ggsave("Probability of superior performance within the Env.jpg", plot = PSPWE, width = 15, height = 15, dpi = 600)

head(results$within$perfo$gl)
pairsupwithin <- plot(results, category = "pair_perfo", level = "within")
pairsupwithin$gl$
pairsupwithin$gl$Nepalgunj 
pairsupwithin$gl$Parwanipur 
pairsupwithin$gl$Tarahara 
pairsupwithin <- plot(results, category = "pair_perfo", level = "within")
str(pairsupwithin)



pairsupwithin$
pairsupwithin$gm$TB
getwd()
