library(fitdistrplus)
library(actuar)
library(ggplot2)
library(ggpubr)

# Density plots ####
d1<-ggplot(sum_stats, aes(x = shape.yule)) + geom_histogram(aes(y = ..density..), colour = "darkblue", fill = "white") + 
  geom_density(alpha = .2, fill = "#FF6666") + labs(title="", x = "Shape (Yule)", y = "Density") + 
  theme_tufte(base_family = "Helvetica") + geom_rangeframe(data=data.frame(x = c(0, 30),
                                                                           y = c(0, .25)), aes(x, y)) 
d2<-ggplot(sum_stats, aes(x = shape.pda)) + geom_histogram(aes(y = ..density..), colour = "darkblue", fill = "white") + 
  geom_density(alpha = .2, fill = "#FF6666") + labs(title="", x = "Shape (PDA)", y = "Density") + 
  theme_tufte(base_family = "Helvetica") + geom_rangeframe(data=data.frame(x = c(-30, 10),
                                                                           y = c(0, .3)), aes(x, y)) 

ggarrange(d1, 2,   
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

# Distribution fitting ####
plotdist(subset(sum_stats, !is.na(shape.yule))$shape.yule, histo = TRUE, demp = TRUE)
plotdist(subset(sum_stats, !is.na(shape.pda))$shape.pda, histo = TRUE, demp = TRUE)
plotdist(subset(sum_stats, !is.na(gamma.stat))$gamma.stat, histo = TRUE, demp = TRUE)

descdist(subset(sum_stats, !is.na(shape.yule))$shape.yule, discrete = F, boot = 1000)
descdist(subset(sum_stats, !is.na(shape.pda))$shape.pda, discrete = F, boot = 1000)
descdist(subset(sum_stats, !is.na(gamma.stat))$gamma.stat, discrete = F, boot = 1000)

#z-score standarization [0,1]
x<-subset(sum_stats, !is.na(shape.pda))$shape.pda
x_scaled<-((x-mean(x))/(sd(x)))
x_scaled[x_scaled>2]<-(2)
x_scaled[x_scaled<(-2)]<-(-2)
x_scaled<-(x_scaled/5)
x_scaled<-(x_scaled+.5)

#
fit_b<-fitdist(x_scaled, "beta")
fit_w<-fitdist(x_scaled, "weibull")
fit_g<-fitdist(x_scaled, "gamma")
fit_ln<-fitdist(x_scaled, "lnorm")
fit_ll<-fitdist(x_scaled, "llogis", start = list(shape = 1, scale = 500))
fit_P<-fitdist(x_scaled, "pareto", start = list(shape = 1, scale = 500))
fit_B<-fitdist(x_scaled, "burr", lower = c(0,0,0), start = list(shape1 = 1, shape2 = 1, rate = 1))

gofstat(list(fit_b,fit_w, fit_g, fit_ln, fit_ll, fit_P, fit_B), fitnames = c("Beta", "Weibull", "Gamma", "lnorm", "llogis", "Pareto", "Burr"))

par(mfrow = c(1,1))
cdfcomp(list(fit_b, fit_g, fit_B), xlogscale = TRUE, ylogscale = TRUE,
        legendtext = c("Beta", "Gamma", "Burr"), lwd = 2)
plot.legend<-c("Beta", "Gamma", "Burr")
dcomp<-denscomp(list(fit_b, fit_g, fit_B), legendtext = plot.legend, xlab = "Shape (Yule)", ylab = "Count", main = "", plotstyle = "ggplot")
dcomp + ggplot2::theme_minimal()

par(mfrow = c(2,2))
plot.legend<-c("Beta", "Gamma", "Burr")
denscomp(list(fit_b, fit_g, fit_B), legendtext = plot.legend, xlab = "Shape (Yule)", ylab = "Count", main = "")
cdfcomp(list(fit_b, fit_g, fit_B), legendtext = plot.legend)
qqcomp(list(fit_b, fit_g, fit_B), legendtext = plot.legend)
ppcomp(list(fit_b, fit_g, fit_B), legendtext = plot.legend)

