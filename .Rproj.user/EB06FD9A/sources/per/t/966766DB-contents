#Write a function that counts matches of 3 or more-in-a-row cells of the same value, for example as in "Candy Crush" or "Puzzle and Dragons." 
#Full definition:
#Given a 5x6 matrix, where each cell can take a value from 1 to 6, a "match" is defined as 

count_matches <- function(mat){
  
  
}

# if i,j, i,j+1, i,j+2
# if i,

mod1 <- lm(Petal.Width~ Petal.Length*Sepal.Width, iris)
iris <- iris %>% 
  mutate(newpwidth= predict(mod1), 
         plength_factor = as.factor(floor(Petal.Length)))
ggplot(iris, aes(y = newpwidth, x = Sepal.Width,
                 group = plength_factor, color = plength_factor)) + 
  geom_smooth() + 
  geom_point() + 
  labs(color = "Floored Petal Length", y = "Predicted Petal Width")

mod1 <- lm(dep_delay ~ distance*air_time, flights) 
flights %>% drop_na(distance,air_time) %>%  
  mutate(pred_delay = predict(mod1)) %>% 
  ggplot(aes(y = pred_delay, x =  distance, color = air_time)) + 
  geom_point()

ggplot(iris, aes(newpwidth, Sepal.Width, fill = Petal.Length)) + 
  geom_raster()
iris$new.Petal.Width  <- predict(mod1)
ggplot(iris, aes( y = new.Petal.Width, x = Petal.Length,color = Sepal.Width)) + 
  geom_line() 

mod1 <- lm(mpg~ disp+hp*wt, mtcars) 
mtcars$newmpg <- predict(mod1)
mtcars <- mtcars %>% 
  mutate(newmpg = predict(mod1), 
         wt_factor = as.factor(floor(wt)))
ggplot(mtcars, aes(y = newmpg, x = hp, group = wt_factor)) + 
  geom_smooth() 
