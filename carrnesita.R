library(readxl)
library(ggplot2)
setwd("C:/algo/AprenderR/meta-analisis")



carnesita <- read_excel('carnesita.xlsx',
                     sheet = 'Españita',
                     col_names = TRUE)


df_carnseita <- data.frame(Año<-c(carnesita$Año),
                           Consumo<-c(carnesita$kg_aper),
                           limiteInferior<-c(carnesita$`lim inf`),
                           limiteSuperior<-c(carnesita$`lim sup`),
                           minimoRecomendado<-c(carnesita$Minimo_Recomendado),
                           maximoRecomendado<-c(carnesita$Máximo_Recomendado))


colnames(df_carnseita)<-c("Año","Consumo","LimiteInferior","LimiteSuperior",
                          "MinimoRecomendado","MaximoRecomendado")

###graficando que es gerundio

ggplot(df_carnseita,aes(x=Año,group = 1))+
  geom_line(aes(y=Consumo,colour="Consumo"))+
  geom_line(aes(y=minimoRecomendado,colour="min"))+
  geom_line(aes(y=maximoRecomendado,colour="max"))+
  geom_ribbon(aes(ymin=limiteInferior,ymax=limiteSuperior),alpha=0.2)+
  theme(panel.background = element_rect(color = "black", # Border color
                                        size = 1, fill = "#FFFFFF"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "mono", face = "bold"),
        plot.subtitle = element_text(family = "mono"),
        plot.title.position = "plot" )+
  geom_point(aes(y = Consumo, colour = "Consumo"))+
  labs(title = "Evolución del consumo cárnico al año por persona con IC 95% y dosis mínima y máxima recomendada", 
       y="kg/año/persona",
       subtitle = "Autor: Manuel Alén con datos de Our World in Data")+
  scale_color_manual("Item",
                     values= c("Consumo" = "blue","min" = "green", 
                               "max"="red"))+
  scale_x_continuous(n.breaks=length(carnesita$Año)/5)+
  geom_label(aes(x = 1990, y = 36, label = "Consumo máximo por persona y por año recomendado por la OMS"),
             fill = "lightgreen", label.size = NA, size = 4)+
  geom_curve(x = 1989, y = 33,xend = 1991, yend = 26,color = 2,arrow = arrow(),curvature = -0.3, 
             angle = 90,
             size = 1.9)
