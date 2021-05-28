library(ggplot2)
library(gridExtra)
library(dplyr)
library(gapminder)

#windowsFonts()
#library(extrafont)
#font_import()
#loadfonts(device = "win")

theme_m <- function(base_size = 16,
                      base_family = "Georgia",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170){
  
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    
    theme(plot.title = element_text(       
        family = base_family,            
        size = 20,                
        face = 'bold',            
        hjust = 0,
        vjust = 4),
      
      plot.subtitle = element_text(      
        family = base_family,            
        size = 14,
        hjust = 0,                
        vjust = 2),               
      
      plot.caption = element_text(
        family = base_family,     
        size = 10,                
        hjust = 1),               
      
      axis.title = element_text(  
        family = base_family,     
        size = 14,
        face="bold"),               
      
      axis.text = element_text(    
        family = base_family,      
        size = 12),                
      
      axis.text.x = element_text(           
        margin=margin(5, b = 10)),
      
      axis.line = element_line(colour = "grey50", size = 1),
      
      panel.grid.major.y = element_line(colour = "grey90", size = .5),
      panel.grid.minor.y = element_blank(),   
      panel.grid.major.x = element_line(colour = "grey90", size = .5),
      panel.grid.minor.x = element_blank(),   
      
      plot.margin=unit(c(2,2,2.5,2.2),"cm"),
      
      strip.text = element_text(size=14, face="bold", family = base_family)
      
    )
}

## Ejemplo
# Cargo y limpio data de gapminder
hist_df <- gapminder %>%
  filter(year == 2007)

# Grafico con tema por defecto
ggplot(hist_df, aes(lifeExp)) +
  geom_histogram(binwidth = 5, colour = "white", fill = "seagreen") +
  scale_x_continuous(limits = c(35, 95),
                     breaks = seq(40, 90, by = 10),
                     labels = c("40", "50", "60", "70", "80", "90 años")) +
  labs(title = "Variación en esperanza de vida en el mundo",
       subtitle = "Distribución de la esperanza de vida en 2007",
       caption = "Fuente: Gapminder \n Adaptación de BBC Visual and Data Journalism",
       y = "Número de países", x = "Expectativa de vida")

# Grafico con tema customizado
ggplot(hist_df, aes(lifeExp)) +
  geom_histogram(binwidth = 5, colour = "white", fill = "seagreen") +
  scale_x_continuous(limits = c(35, 95),
                     breaks = seq(40, 90, by = 10),
                     labels = c("40", "50", "60", "70", "80", "90 años")) +
  labs(title = "Variación en esperanza de vida en el mundo",
       subtitle = "Distribución de la esperanza de vida en 2007",
       caption = "Fuente: Gapminder \n Adaptación de BBC Visual and Data Journalism",
       y = "Número de países", x = "Expectativa de vida") + 
  theme_m() 


