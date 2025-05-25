library(tidyverse)
library(plotly)
library(dplyr)

salaries <- read.csv("./Salaries.csv")

n1 <- salaries %>% group_by(Expertise.Level, Company.Location)
promedios <- n1 %>% summarize(Promedio_Salario = mean(Salary.in.USD))
promedios$Promedio_Salario <- round(promedios$Promedio_Salario)

costes <- salaries %>% group_by(Company.Location) %>% summarize(cost_live = mean(cost))
costes$cost_live <- round(costes$cost_live)


p2 <- promedios %>% spread((Expertise.Level),Promedio_Salario)

p3 <- merge(x = p2, y = costes, by = "Company.Location")



promedios_total <- salaries %>% group_by(Company.Location) %>% summarize(Promedio_Salario = mean(Salary.in.USD))
promedios_total$Promedio_Salario <- round(promedios_total$Promedio_Salario)

pt <- merge(x = promedios_total, y = costes, by = "Company.Location")

pt$cost_live = pt$cost_live * 12

pt2 <- pt %>% gather("Referente","Coste",2:3)

gplotpromedios <- ggplot(pt2, aes(x=reorder(Company.Location,Coste), y=Coste, fill=Referente)) +
  geom_col(position='dodge') + coord_flip()
ggplotly(gplotpromedios)



pt3 <- pt %>% mutate(Ratio = Promedio_Salario/cost_live)

gplotpt3 <- ggplot(pt3, aes(x=reorder(Company.Location, Ratio), y=Ratio)) + 
  ylim (0,12) + coord_flip() + geom_col(fill="lightblue") + xlab('País') + ylab('Ratio (Salari Promig / Cost de vida)') +
  scale_fill_discrete(name = 'País')
ggplotly(gplotpt3)



pt4 <- pt %>% mutate(Diff = Promedio_Salario - cost_live) %>% select(Company.Location, Diff, cost_live)
pt5 <- pt4 %>% gather("Coso","Valor",2:3)

#pt5 <- pt4 %>% gather("Coso","Valor",2:3) %>% arrange(desc(Coso))

gplotpt5 <- ggplot(pt5, aes(x=reorder(Company.Location,Valor), y=Valor, fill=Coso)) + coord_flip() + geom_col()
ggplotly(gplotpt5)

pt5$Coso2 <- factor(pt5$Coso, levels = c('Diff', 'cost_live'))

gplotpt5 <- ggplot(pt5, aes(x=reorder(Company.Location,Valor), y=Valor, fill=Coso2)) + coord_flip() + geom_col() +
  scale_fill_manual(name = "", values=c("#800080", "#E69F00"))
ggplotly(gplotpt5)

elplot <- ggplot(promedios_total, aes(x=reorder(Company.Location,Promedio_Salario), y=Promedio_Salario)) + 
  coord_flip() + geom_col(fill = 'lightblue')
ggplotly(elplot)


