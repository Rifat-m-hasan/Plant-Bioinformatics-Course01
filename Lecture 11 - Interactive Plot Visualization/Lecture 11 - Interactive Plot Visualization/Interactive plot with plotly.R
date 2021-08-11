# install.packages("plotly")

library(plotly)

# General Syntax plot_ly() ---------------------------------------------

# 1. Plot_ly functions creates a plotly object and produces interactive visualization based on data and arguments supplied

# 2. plot_ly(data, x, y , type, mode, color, size,....)

plot_ly()

# 3. data = a dataframe

# 4. x and y axis values

# 5. type = specifies the type of plot or trace such as: 'histogram', 'scatter', 'bar', 'box', 'heatmap', 'histogram', 'histogram2d', 'histogram2dcontour', 'pie', 'contour', 'scatter3d', 'surface', 'mesh3d', 'scattergeo', 'choropleth', 'scattergl', 'scatterternary', 'scattermapbox', 'area'

# 6. mode = specifies the mode, such as 'line', 'points', 'markers' 

# 7. color = specifies the color of data points usually a function of I() to avoid scaling

# 8. colors = colourbrewer pallete, vector of color in hexa format

# 9. size = name or expression that defines the size of the data points

# 10. We can use other functions such as layout() for axis formating

# 11. and other functions that comes with plotly package


plot_ly(data = mtcars, 
        x = ~ wt, 
        y = ~ mpg)


summary(mtcars)


## Define the plot type exclusively as "scatter" --------------------------
# & mode as "markers" 

plot_ly(data = mtcars, 
        x = ~ wt, 
        y = ~ mpg,
        type = "scatter",
        mode = "markers" )




## Change color of scatter data points ----------------------------------
plot_ly(data=mtcars, 
            x=~wt, 
            y=~mpg,
            type = "scatter",
            mode = "markers",
            color = I("red"))



## Change color of data points using marker argument ---------------------

plot_ly(data=mtcars, 
            x=~wt, 
            y=~mpg,
            type = "scatter",
            mode = "markers",
            marker = list(color = "green", size=10))




## Styled Marker ----------------------------------
plot_ly(data=mtcars, 
            x=~wt, 
            y=~mpg,
            type = "scatter",
            mode = "markers",
            marker = list(size = 7,
                          color = 'rgba(255, 182, 193, .9)',
                          line = list(color = 'rgba(152, 0, 0, .8)',
                                      width = 2)))


## Set the color scale based on a factor variable -------------
plot_ly(data=mtcars, 
            x=~wt, 
            y=~mpg,
            type = "scatter",
            mode = "markers",
            color = ~as.factor(cyl))





## Use color Brewer pallets for data point colors ---------------

plot_ly(data = mtcars, 
        x = ~ wt, 
        y = ~ mpg,
        type = "scatter",
        mode = "markers",
        color = ~ as.factor(cyl),
        colors = "Set1")



## Use customized pallet for data point colors -----------------------
pal <- c("red", "blue", "green")

plot_ly(data=mtcars, 
            x=~wt, 
            y=~mpg,
            type = "scatter",
            mode = "markers",
            color = ~as.factor(cyl),
            colors = pal)





## Change data point shape based on factor variable -----------

plot_ly(data=mtcars, 
            x=~wt, 
            y=~mpg,
            type = "scatter",
            mode = "markers",
            symbol = ~ as.factor(cyl),
            symbols =  c('circle','x','o'),
            marker = list(size = 10))






## Set the color scale based on a continuous variable ----------
plot_ly(data=mtcars, 
           x=~wt, 
           y=~mpg, 
           mode="markers", 
           color = ~disp)




## Scatter plot with different data point size based on a continuous variable -----------------

plot_ly(data=mtcars, 
            x=~wt, 
            y=~mpg, 
            type="scatter",
            mode="markers", 
            color = ~as.factor(cyl),
            size = ~hp)







## Hide the legend --------------------------------------------
plot_ly(data=mtcars, 
             x=~wt, 
             y=~mpg,
             type = "scatter",
             mode = "markers",
             symbol = ~as.factor(cyl),
             symbols =  c('circle','x','o'),
             marker = list(size = 5)) %>%
  layout(showlegend = FALSE)





## Change the orientation of the legend (below the plot) -----------------
plot_ly(data=mtcars, 
             x=~wt, 
             y=~mpg,
             type = "scatter",
             mode = "markers",
             symbol = ~as.factor(cyl),
             symbols =  c('circle','x','o'),
             marker = list(size = 5)) %>%
  layout(legend = list(orientation = 'h'))





## Change the orientation of the legend (inside the plot) ----------------
plot_ly(data=mtcars, 
             x=~wt, 
             y=~mpg,
             type = "scatter",
             mode = "markers",
             symbol = ~as.factor(cyl),
             symbols =  c('circle','x','o'),
             marker = list(size = 5)) %>%
  layout(legend = list(x = 0.8, y = 0.9))





## Change the orientation of the legend (outside the plot) -------------
plot_ly(data=mtcars, 
             x=~wt, 
             y=~mpg,
             type = "scatter",
             mode = "markers",
             symbol = ~as.factor(cyl),
             symbols =  c('circle','x','o'),
             marker = list(size = 5)) %>%
  layout(legend = list(x = 1, y = 0.5))







## Add title to the legend --------------------------------
plot_ly(data=mtcars, 
             x=~wt, 
             y=~mpg,
             type = "scatter",
             mode = "markers",
             symbol = ~as.factor(cyl),
             symbols =  c('circle','x','o'),
             marker = list(size = 5)) %>%
  layout(legend = list(x = 1, y = 0.5))







## Adding chart title and axis labels -------------------------------

plot_ly(data=mtcars, 
             x=~wt, 
             y=~mpg,
             type = "scatter",
             mode = "markers",
             symbol = ~as.factor(cyl),
             symbols =  c('circle','x','o'),
             marker = list(size = 5)) %>%
  layout(title = "Scatter plot using R Plotly",
         xaxis = list(title="Weight", showgrid = F),
         yaxis = list(title="MPG", showgrid = T))





## Customizing mouse hover text -----------------------------------

plot_ly(data = mtcars, 
            x=~mpg, 
            y=~wt, 
            type = "scatter", 
            mode="markers",
            hoverinfo = "text", 
            text = paste("Miles per gallon: ", mtcars$mpg, 
                         "<br>", 
                         "Weight: ", mtcars$wt))





## Add annotations to the plot - add_annotations() function syntax -------
# add_annotations() function is used to add annotations to the plot along with the following arguments

# x = set annotations x coordinate position (x axis value based on the dataset)

# y = set annotations y coordinate position (y axis value based on the dataset)

# text = text associated with annotation

# showarrow = 0 or 1 depending whether to show arrow or not

# xref = "paper" telling plotly to set x reference to the plot (in which case x=0 means the left most, x=1 means right most)

# yref = "paper" telling plotly to set y reference to the plot (in which case y=0 means the bottom, y=1 means top)

# xref = "x" (non paper x coordinates)

# yref = "y" (non paper y coordinates)

# arrowhead, arrowsize, font, ax, ay 





## Example of annotations on a single data points --------------------
## display annotations for good mileage
plot_ly(data = mtcars, 
        x=~mpg, 
        y=~wt, 
        type = "scatter", 
        mode="markers") %>% 
  add_annotations(
    x = mtcars$mpg[which.max(mtcars$mpg)],
    y = mtcars$wt[which.max(mtcars$mpg)],
    text = "Good mileage",
    showarrow = F)




## Example of annotations - placing text at a desired location on the plot ------------------------
## Display Data Source
## Demo of x/y ref as "paper"

plot_ly(data = mtcars, 
        x=~mpg, 
        y=~wt, 
        type = "scatter", 
        mode="markers") %>% 
  add_annotations(
    xref = "paper",
    yref = "paper",
    x = 1,
    y = 0.9,
    text = "Data Source : mtcars",
    showarrow= F)



## Example #1 of annotations on multiple data points ------------------
## display annotation for low and high mileage ##
plot_ly(data = mtcars, 
        x=~mpg, 
        y=~wt, 
        type = "scatter", 
        mode="markers") %>% 
  add_annotations(x=mtcars$mpg[which.max(mtcars$mpg)],
                  y=mtcars$wt[which.max(mtcars$mpg)],
                  text="high mileage") %>% 
  add_annotations(x=mtcars$mpg[which.min(mtcars$mpg)],
                  y=mtcars$wt[which.min(mtcars$mpg)],
                  text="low mileage")



## Example #2 of annotations on multiple data points ---------
# display annotations for automatic transission cars

plot_ly(data = mtcars, 
        x=~mpg, 
        y=~wt, 
        type = "scatter", 
        mode="markers") %>% 
  add_annotations(
    x = mtcars$mpg[mtcars$am==0],
    y= mtcars$wt[mtcars$am==0],
    text="auto",
    showarrow=F)





## Styling annotations -----------------------------------------
# using font argument

plot_ly(data = mtcars, 
        x = ~mpg, 
        y = ~wt, 
        type = "scatter", 
        mode="markers") %>% 
  add_annotations(x = mtcars$mpg[which.max(mtcars$mpg)],
                  y = mtcars$wt[which.max(mtcars$mpg)],
                  text = "Good mileage",
                  font = list(color = "green", 
                              family = "sans serif", 
                              size = 20))










