library(tidyverse)
library(plotly)
salaries <- read.csv("./Data_Science_Salaries.csv")
n1 <- salaries %>% group_by(Expertise.Level, Company.Location)
promedios <- n1 %>% summarize(Promedio_Salario = mean(Salary.in.USD))
promedios$Promedio_Salario <- round(promedios$Promedio_Salario)

#Plot barres amb tots els nivels d'expertise
gplotpromedios <- ggplot(promedios, aes(x=Company.Location, y=Promedio_Salario, fill=Expertise.Level)) + 
  geom_col(position='dodge') + coord_flip()
ggplotly(gplotpromedios)

#Salaris promig de Junior ordenats en gràfic de barres
junior <- promedios %>% filter(Expertise.Level=='Junior')
gplotjunior <- ggplot(junior, aes(x=reorder(Company.Location,Promedio_Salario), y=Promedio_Salario, fill=reorder(Company.Location,Promedio_Salario))) +
  geom_col() + coord_flip() + xlab("Salari promig (Junior)") + ylab("País empresa")
ggplotly(gplotjunior)

#Salaris promig Junior en format mapamundi
mapajunior <- plot_ly(
  data = junior,
  locations = ~Company.Location,
  locationmode = "country names",
  z = ~Promedio_Salario,
  type = "choropleth",
  colorscale = "inferno"
) %>% layout(title = "Salari promig (Junior)")
mapajunior

#Si vols que només es mostrin països amb dades, fes
# %>% layout(title = "Salari promig (Intermediate)",geo = list(showframe = FALSE, showcoastlines = FALSE))



#Salaris promig de Intermediates ordenats en gràfic de barres
intermediate <- promedios %>% filter(Expertise.Level=='Intermediate')
gplotinter <- ggplot(intermediate, aes(x=reorder(Company.Location,Promedio_Salario), y=Promedio_Salario, fill=reorder(Company.Location,Promedio_Salario))) +
  geom_col() + coord_flip() + xlab("Salari promig (Intermediate)") + ylab("País empresa")
ggplotly(gplotinter)

#Salaris promig Intermediate en format mapamundi
mapainter <- plot_ly(
  data = intermediate,
  locations = ~Company.Location,
  locationmode = "country names",
  z = ~Promedio_Salario,
  type = "choropleth",
  colorscale = "inferno"
) %>% layout(title = "Salari promig (Intermediate)")
mapainter



#Salaris promig de Experts ordenats en gràfic de barres
expert <- promedios %>% filter(Expertise.Level=='Expert')
gplotexpert <- ggplot(expert, aes(x=reorder(Company.Location,Promedio_Salario), y=Promedio_Salario, fill=reorder(Company.Location,Promedio_Salario))) + 
  geom_col() + coord_flip() + xlab("Salari promig (Expert)") + ylab("País empresa")
ggplotly(gplotexpert)

#Salaris promig Experts en format mapamundi
mapaexpert <- plot_ly(
  data = expert,
  locations = ~Company.Location,
  locationmode = "country names",
  z = ~Promedio_Salario,
  type = "choropleth",
  colorscale = "inferno"
) %>% layout(title = "Salari promig (Expert)")
mapaexpert



#Salaris promig de Directors ordenats en gràfic de barres
director <- promedios %>% filter(Expertise.Level=='Director')
gplotdirector <- ggplot(director,aes(x=reorder(Company.Location,Promedio_Salario), y=Promedio_Salario, fill=reorder(Company.Location,Promedio_Salario))) + 
  geom_col() + coord_flip() + xlab("Salari promig (Director)") + ylab("País empresa")
ggplotly(gplotdirector)

#Salaris promig Directors en format mapamundi
mapadirector <- plot_ly(
  data = director,
  locations = ~Company.Location,
  locationmode = "country names",
  z = ~Promedio_Salario,
  type = "choropleth",
  colorscale = "inferno"
) %>% layout(title = "Salari promig (Director)")
mapadirector

#----------------------------------------------------------------------
#Mapamundi amb promig total, si poses el ratoí damunt et diu els promigs de cada nivell (si hi han dades)
p2 <- promedios %>% spread(Expertise.Level,Promedio_Salario)

p3 <- p2 %>% mutate(Promedio = rowMeans(p2[,c('Director','Expert','Intermediate','Junior')],na.rm=TRUE))
p3$hover <- p3 %>% with(paste(" Director:", Director, "<br>",
                              "Expert:", Expert, "<br>",
                              "Intermediate:", Intermediate, "<br>",
                              "Junior:", Junior))

mp3 <- plot_ly(
  data = p3,
  locations = ~Company.Location,
  locationmode = "country names",
  z = ~Promedio,
  text = ~hover,
  type = "choropleth",
  colorscale = "inferno"
) %>% layout(title = "Salari promig")
mp3
