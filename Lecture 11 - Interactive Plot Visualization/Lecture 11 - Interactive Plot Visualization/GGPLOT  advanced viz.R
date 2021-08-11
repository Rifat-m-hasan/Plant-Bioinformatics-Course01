library(tidyverse)

df <- iris

summary(df)

# Histogram plot -------------------------------------------------------

df %>%
 filter(Sepal.Length >= 4.7 & Sepal.Length <= 7.9) %>%
 filter(Sepal.Width >= 2 & Sepal.Width <= 3.98) %>%
 filter(!(Species %in% "versicolor")) %>%
 ggplot() +
 aes(x = Sepal.Length) +
 geom_histogram(bins = 50L, fill = "#2CF0C3") +
 labs(x = "count", 
      y = "value", 
      title = "Histogram plot", 
      subtitle = "lets make it", 
      caption = "its going to be ok") +
 coord_flip() +
 theme_minimal()



# Scatter Plot -------------------------------------------------------

df %>%
 filter(Sepal.Width >= 2 & Sepal.Width <= 3.98) %>%
 ggplot() +
 aes(x = Sepal.Length, 
     y = Sepal.Width, 
     colour = Petal.Length, 
     size = Petal.Width) + 
 geom_point(shape = "square open") +
 scale_color_gradient(low = "red", high = "green") +
 scale_y_continuous(trans = "log") +
 labs(x = "Sepal.Length", 
      y = "Sepal.Width", 
      title = "Scatter plot", 
      subtitle = "lets make it", 
      caption = "its going to be ok") +
 ggthemes::theme_solarized() +
 theme(legend.position = "bottom") +
 facet_wrap(vars(Species))



ggplot(df) +
   aes(x = Sepal.Length,
       y = Sepal.Width,
       colour = Petal.Length,
       size = Petal.Width ) +
   geom_point(shape = "asterisk") +
   facet_wrap(vars(Species), 
              scales = "free") +
   scale_color_distiller(palette = "ggplot2", 
                         direction = 1) +   #reserve
   scale_x_continuous(trans = "log10") +
   scale_y_continuous(trans = "exp") +
   labs(y = "Sepal width",
        color = "Petal Length",
        size = "Petal width" ) +
   ggthemes::theme_par() +
   theme(legend.position = "bottom") +
   xlim(2L, 9L) +
   ylim(0L, 5L)

# Box plot ------------------------------------------

df1 <- as.data.frame(Titanic)

ggplot(df1) +
 aes(x = Class, 
     y = Freq, 
     fill = Survived, 
     group = Age) +
 geom_boxplot(shape = "circle") +
 scale_fill_viridis_d(option = "set1", 
                      direction = 1) +
 labs(x = "X axix label", 
      y = "Y axix label", 
      title = "Titanic Survival", 
      subtitle = "It is a subtitle", 
      caption = "Here you can put your caption", 
      fill = "Fill label") +
 theme_classic() +
 theme(legend.position = "bottom", 
       plot.title = element_text(size = 20L, 
                                 hjust = 0.5,
                                 family = "mono",
                                 colour = "red",
                                 face = "bold"), 
       plot.subtitle = element_text(size = 10L, 
                                    face = "bold"), 
       plot.caption = element_text(size = 10L, 
                                   face = "italic",
                                   hjust = 0), 
       axis.title.y = element_text(size = 10L, 
                                   face = "bold", 
                                   hjust = 0),
       axis.title.x = element_text(size = 10L)) +
 facet_wrap(vars(Sex), 
            scales = "free_x", 
            ncol = 2L, 
            nrow = 1L)


# Some Extra Example


data <- data.frame(x = 1:10,       # Create example data
                   y = 1:10)


ggp <- ggplot(data, aes(x, y)) +  # ggplot2 plot with default font
   geom_point()

ggp 


windowsFonts(A = windowsFont("Arial"))  

ggp +                           
   theme(text = element_text(family = "A"))























