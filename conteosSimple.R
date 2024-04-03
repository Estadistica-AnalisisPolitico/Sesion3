rm(list = ls())

censo2007_wide_Totales=read.csv("censo2007_wide_Totales.csv")
## A nivel distrital, la cantidad de personas con algún seguro de salud 
## está afectada por el nivel analfabetismo.
h1=formula(conSeguro_Total~analfa_Total)

rp1=glm(h1, data = censo2007_wide_Totales, 
        offset=log(poblacion_Total), #exposure 
        family = poisson(link = "log"))

summary(rp1)

cbind(exp(coef(rp1)),exp(confint(rp1)))

## > A nivel distrital, la cantidad de personas con algun seguro de salud 
## está afectada por el nivel analfabetismo y 
## por la presencia de trabajadores independientes.

h2=formula(conSeguro_Total~analfa_Total + indep_Total)

rp2=glm(h2, data = censo2007_wide_Totales, 
        offset=log(poblacion_Total),
        family = poisson(link = "log"))

summary(rp2)

cbind(exp(coef(rp2)),exp(confint(rp2)))

# equidispersion

AER::dispersiontest(rp2,alternative='greater')$ p.value<0.05 # overdispersion?
AER::dispersiontest(rp2,alternative='less')$ p.value<0.05# underdispersion?

rqp = glm(h2, data = censo2007_wide_Totales,
          offset=log(poblacion_Total),
          family = quasipoisson(link = "log"))
summary(rqp)

#mismos coeff?
cbind(coef_Poi=coef(rp2),coef_QuasiPoi=coef(rqp))
#mismos errores tipicos?
library(arm)
cbind(se_Poi=se.coef(rp2),se_QuasiPoi=se.coef(rqp))
#nuevos ic:
cbind(exp(coef(rqp)),exp(confint(rqp)))

# bin
h2off=formula(conSeguro_Total ~ analfa_Total + indep_Total + offset(log(poblacion_Total)))

rbn=MASS::glm.nb(h2off,data=censo2007_wide_Totales)
summary(rbn)

# comparacion no anidada
data.frame(Model = c("poisson", "quasipoisson", "negative-binomial"),
           AIC = c(AIC(rp2), AIC(rqp), AIC(rbn)),
           BIC = c(BIC(rp2), BIC(rqp), BIC(rbn)),stringsAsFactors = FALSE
)

# poisson case
performance::check_overdispersion(rp2)
# quasipoisson case
performance::check_overdispersion(rqp)
# negative binomial case
performance::check_overdispersion(rbn)


library(ggplot2)
dotwhisker::dwplot(list(Poisson=rp2,CuasiPoisso=rqp,BinomialNegativa=rbn),exp=T) + scale_y_discrete(labels=c("trabajo\nindependiente","analfabetismo")) + scale_color_discrete(name="Modelos para:\nCantidad de Asegurados") + geom_vline(
    xintercept = 1,
    colour = "grey60",
    linetype = 2
)


sdVD=sd(censo2007_wide_Totales$conSeguro_Total)
sdVIs=apply(censo2007_wide_Totales[,c("analfa_Total","indep_Total")],2,sd)
DF=list(Poisson=sdVIs*coef(rp2)[c(2,3)]/sdVD,
        CuasiPoisson=sdVIs*coef(rqp)[c(2,3)]/sdVD,
        BinomNegativa=sdVIs*coef(rbn)[c(2,3)]/sdVD)%>%
    data.frame()
DF