################################################################################
# EXAMPLE OF DLNMS WITH A MULTINOMIAL MODEL
################################################################################

# load the packages
library(nnet)
library(dlnm)
library(splines)

# mock birth data
birth <- read.csv("data/birth_mock data.csv")

# define the cross-basis
cb1.inv <- crossbasis(birth$max_inv_pm, 
  lag = 20, 
  argvar = list(fun = "ns", knots = 3), 
  arglag = list(fun = "ns", knots = 3))
summary(cb1.inv)

# logistic, two categories — works
m_bin <- glm(factor(early_preterm_status) ~ cb1.inv,
  family = "binomial", birth)
summary(m_bin)

# prediction and plot
pred_bin <- crosspred(cb1.inv, m_bin, at=1, cumul=TRUE, cen=0)
plot(pred_bin, var=1)

birth$preterm_status <- relevel(as.factor(birth$preterm_status), ref = "term") # edit BK

# multinomial, multiple categories — error with crosspred
m_multi <- multinom(preterm_status ~ cb1.inv, birth)
summary(m_multi)

# extract coef/vcov for various categories
cat <- c("early preterm", "late preterm", "preterm") # edit BK
parlist <- lapply(seq(cat), function(i) {
  coef <- coef(m_multi)[i,][-1]
  ind <- (i-1)*7+seq(6)+1
  vcov <- vcov(m_multi)[ind,ind]
  list(coef=coef, vcov=vcov)
})
names(parlist) <- cat

# prediction
cplist <- lapply(cat, function(x)
  crosspred(cb1.inv, coef=parlist[[x]]$coef, vcov=parlist[[x]]$vcov, at=1, 
    cumul=TRUE, cen=0)
)
  
# plot
plot(cplist[[1]], var=1, ci="l", col=2)
lines(cplist[[2]], var=1, ci="l", col=3)
lines(cplist[[3]], var=1, ci="l", col=4)
legend("top", legend=cat, col=2:4, bty="n", ncol=1, lty=1, cex=0.7)

# edit BK
plot(cplist[[1]]$, var = 1)
