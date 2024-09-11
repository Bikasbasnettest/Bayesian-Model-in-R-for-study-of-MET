Bikas<-read.csv("E:/NARC2/Bayesian model/Combine 77-78.csv", header = TRUE)
Bikas
dim(Bikas)
colnames(Bikas)
library(ProbBreed)
?ProbBreed
mod = bayes_met(data = Bikas, 
                gen = "GEN", 
                loc = "ENV",
                repl = "REP",
                reg = NULL,
                year = "YEAR",
                res.het = FALSE,
                trait = "Y.ha",
                iter = 2000, cores = 0.005, chains = 1)

outs = extr_outs(model = mod,
                 probs = c(0.05, 0.95), 
                 verbose = TRUE)
plot(outs, category = "density")
plot(outs, category = "histogram")
plot(outs)
results = prob_sup(extr = outs, 
                   int = .2,
                   increase = TRUE, 
                   save.df = FALSE, 
                   verbose = FALSE)
plot(results, category = "hpd")
head(results$across$perfo)
plot(results)
results$across$pair_perfo[1:5, 1:5]
plot(results, category = "pair_perfo")
results$across$pair_perfo[1:5, 1:5]
head(results$across$stabi$gl)
head(results$across$stabi$gm)
plot(results, category = "stabi")
results$across$pair_stabi$gl[1:5, 1:5]
plot(results, category = "pair_stabi")
plot(results, category = "pair_stabi")
head(results$across$joint)
plot(results, category = "joint")
head(results$within$perfo$gl)
plot(results, category = "perfo", level = "within")
head(results$within$perfo$gl)
pairsupwithin = plot(results, category = "pair_perfo", level = "within")
pairsupwithin$gl$E13
pairsupwithin$gm$TB
