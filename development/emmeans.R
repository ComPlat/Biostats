library(emmeans)
CO2$conc_f <- as.factor(CO2$conc)
model <- glm(uptake ~ conc_f, data = CO2, family = stats::gaussian())
summary(model)
emm <- emmeans(model, c("conc_f"))
print(emm)
res <- pairs(emm, adjust = "tukey")
res
