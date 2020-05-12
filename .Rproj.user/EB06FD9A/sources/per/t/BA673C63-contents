#ggplot2 .... art??? first try
#from https://github.com/marcusvolz/mathart
library(mathart)
library(tidyverse)
library(gganimate)
set.seed(2)
base <- lissajous(a = runif(1, 0, 2), 
                b = runif(1, 0, 2), 
                A = runif(1, 0, 2), 
                B = runif(1, 0, 2), 
                d = 200) %>%
  sample_n(1001)

df <- map_dfr(2:30, function(x){
  message(x)
  k_nearest_neighbour_graph(base, x) %>% mutate(k = !!x)
})

lissajous <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
  geom_segment(size = 0.03) +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0) + 
  transition_states(
    k, 
    transition_length = 1, 
    state_length = 1
  )

anim_save("lissajous.gif", lissajous, 
          res = 600, 
          width = 25, height = 25, units = "cm")
