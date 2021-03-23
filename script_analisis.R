library(readxl)
library(readxl)
library(tidyverse)
library(cowplot)

data_cts <- read_excel("Desktop/UNIVERSIDAD_DEL_ROSARIO/PROYECTOS/ANALISIS_CT_CARGA_VIRAL/Base_Cts_SARS.xlsx", 
                       col_types = c("text", "text", "numeric", 
                                     "text", "numeric", "skip", "skip", 
                                     "skip", "skip", "skip", "skip", "skip", 
                                     "skip", "skip", "skip", "skip", "skip", 
                                     "skip", "skip", "skip", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text"))


Base_Cts <- read.csv2("~/Desktop/UNIVERSIDAD_DEL_ROSARIO/PROYECTOS/ANALISIS_CT_CARGA_VIRAL/Base_Cts.csv")

Base_Cts <- drop_na(Base_Cts)

numSummary(Base_Cts_SARS[,c("Ct", "Edad"), drop=FALSE], 
           statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))


#cálculo normalidad valores de Cts por grupos 
normalityTest(Ct ~ Asintomatico, test="ad.test", data=Base_Cts_SARS)

# medianas por group
Tapply(Ct ~ Obesidad, median, na.action=na.omit, data=Base_Cts_SARS) 

# comparación de medianas por grupo Wilcoxon test
wilcox.test(Ct ~ Asintomatico, alternative="two.sided", data=Base_Cts_SARS)

# comparación de medianas por grupos de carga viral
Tapply(Ct ~ Carga.Viral..Rev., median, na.action=na.omit, data=Base_Cts_SARS) 

# comparación de medianas por grupos de carga viral Kruskal wallis test
kruskal.test(Ct ~ Carga.Viral..Rev., data=Base_Cts_SARS)


# correlación Pearson Edad vs Cts
with(Base_Cts_SARS, cor.test(Ct, Edad, alternative="two.sided", method="pearson"))



#boxplot para comparar Cts entre grupos en los que se encontró dif significativas.

p1 <- Base_Cts_SARS %>% filter (HOSP. == "NO") %>% 
  ggplot( aes(x= HOSP., y=Ct, fill=HOSP.)) +
  geom_boxplot(fill = rgb(0.1,0.1,0.7,0.5)) +
  theme(legend.position="none", 
        plot.title = element_text(size=11)) +
  #scale_fill_brewer(palette="Set2") +
  theme_cowplot() +
  ggtitle("CT values NO-HOSP") +
  xlab("No-HOSP") +
  ylab("Ct Value")


p2 <- Base_Cts_SARS %>% filter (HOSP. == "SI") %>% 
  ggplot( aes(x= HOSP. , y=Ct, fill=HOSP.)) +
  geom_boxplot(fill = rgb(0.8,0.1,0.3,0.6)) +
  theme(legend.position="none", 
        plot.title = element_text(size=11)) +
  #scale_fill_brewer(colours(rgb(0.1,0.1,0.7,0.5))) +
  theme_cowplot() +
  ggtitle("CT values HOSP") +
  xlab("HOSP") +
  ylab("Ct Value")



merge_fig <- plot_grid(p1, p2,
                       nrow = 1,
                       labels = c("A.", "B."),
                       label_size = 10,
                       label_fontfamily = "sans") 
merge_fig



#Análisis de correlación a partir de la construcción de modelos de regresión lineal para determinar el valor de R2


cts_edad <- lm(Ct~Edad, data=Base_Cts_SARS)
summary(cts_edad ) 

ggplot(Base_Cts_SARS, aes(x=Ct, y=Edad)) +
  geom_point() +    # genera circulos en el grafico
  geom_smooth(method=lm, color="black", fill="#6699FF", se=TRUE) # adjunta la linea de regresion por defecto es al 95% de confianza

#tablas de contingencia y chi cuadrado 

local({.Table <- xtabs(~Sexo+Carga.Viral..Rev., data=Base_Cts_SARS)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})


#regresiones lineales

#sintomas

lm(formula = Ct ~ Asintomatico + Cefalea + Conjuntivitis + Diarrea + 
     Dificultad.Respiratoria + Fatiga + Fiebre + Odinofagia + 
     Rinorrea + Tos, data = Base_Cts_SARS)



#antecedentes

lm(formula = Ct ~ Asma + Cáncer + Desnutrición + Diabetes + 
     Enfermedad.Cardiaca + EPOC + Fumador + Inmunosupresores + 
     Insuficiencia.Renal + Ningún.Antecedente + Obesidad + Tuberculosis + 
     VIH, data = Base_Cts_SARS)

