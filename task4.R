# Preprocessing

library(plotly)
setwd("C:/UT/2nd_semester/dm/project/submit")

food.df <- read.csv("epi_r.csv",header=T,sep=',', stringsAsFactors=FALSE)
food.df <- food.df[complete.cases(food.df),]
food.subset <- food.df[food.df$rating == 5,]

food.subset.veg <- food.subset[food.subset$vegetarian == 1,]
food.subset.nonveg <- food.subset[food.subset$vegetarian == 0,]

#Vegetarian and non-vegetarian recipes in 5 start rating
df.composition <- data.frame("count"=c(nrow(food.subset.veg),nrow(food.subset.nonveg)), 
                             "catagory"=c("Vegetarian","Non-vegetarian"))

chart <- plot_ly(df.composition, labels = ~catagory, values = ~count, type = 'pie',
                 
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF', size = 24),
                 hoverinfo = 'text',
                 text = ~paste('', count, ''),
                 marker = list(colors = c('rgb(211,94,96)', 'rgb(128,133,133)'),
                               line = list(color = '#FFFFFF', width = 2)),
                 showlegend = FALSE) %>%
  layout(title = 'Vegetarian vs Non-vegetarian 5 Star Recipes')

chart

#vegetarian and non-vegetarian nutrients
recipe <- c("Calories", "Protein", "Fat", "Sodium")

food.subset.nonveg.norm <- food.subset.nonveg[sample(nrow(food.subset.veg)),]
nutrients.sum.veg = colSums(food.subset.veg[ sapply(food.subset.veg, is.numeric)] )
nutrients.veg <- c(nutrients.sum.veg[[2]], nutrients.sum.veg[[3]], nutrients.sum.veg[[4]], nutrients.sum.veg[[5]])

nutrients.sum.nonveg = colSums(food.subset.nonveg[sapply(food.subset.nonveg.norm, is.numeric)] )
nutrients.nonveg <- c(nutrients.sum.nonveg[[2]], nutrients.sum.nonveg[[3]], nutrients.sum.nonveg[[4]], 
                      nutrients.sum.nonveg[[5]])

data <- data.frame(recipe, nutrients.veg, nutrients.nonveg)

p <- plot_ly(data, x = ~recipe, y = ~log(nutrients.veg), type = 'bar', name = 'Vegetarian Recipes') %>%
  add_trace(y = ~log(nutrients.nonveg), name = 'Non-vegetarian Recipes') %>%
  layout(title = 'Nutritional Value of Vegetarian and Non-vegetarian for 5 Star Recipes',
         yaxis = list(title = 'Amount'), xaxis = list(title = 'Nutrients'), barmode = 'group')
p
#### Non-vegetarian Recipes Fat vs Sodium
a <- list(
  text = "Protein vs Sodium in Non-vegetarian Recipes ",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

b <- list(
  text = "Protein vs Sodium in Vegetarian Recipes",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
p1 <- plot_ly(food.subset.nonveg.norm, x = ~log(protein), y = ~log(sodium), type = 'scatter', mode = 'markers', 
              marker = list(size = ~log(protein + sodium)+5, opacity = 0.5, color = 'rgb(255, 65, 54)')) %>%
  layout(annotations = a, yaxis = list(title = 'Sodium'), xaxis = list(title = 'Protein'))

p2 <- plot_ly(food.subset.veg, x = ~log(protein), y = ~log(sodium), type = 'scatter', mode = 'markers', 
              marker = list(size = ~log(protein + sodium)+5, opacity = 0.5, color = 'rgb(255, 65, 54)')) %>%
  layout(annotations = b, yaxis = list(title = 'Sodium'), xaxis = list(title = 'Protein'))
p <- subplot(p1, p2, titleX = TRUE, titleY = TRUE) %>%
  layout(showlegend = FALSE)
p
###

c <- list(
  text = "Calories vs Fat in Non-vegetarian Recipes ",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

d <- list(
  text = "Calories vs Fat in Vegetarian Recipes",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

p3 <- plot_ly(food.subset.nonveg.norm, x = ~log(calories), y = ~log(fat), type = 'scatter', mode = 'markers', 
              marker = list(size = ~log(calories + fat)+5, opacity = 0.5, color = 'rgb(255, 65, 54)')) %>%
  layout(annotations = c, yaxis = list(title = 'Calories'), xaxis = list(title = 'Fat'))

p4 <- plot_ly(food.subset.veg, x = ~log(calories), y = ~log(fat), type = 'scatter', mode = 'markers', 
              marker = list(size = ~log(calories + fat)+5, opacity = 0.5, color = 'rgb(255, 65, 54)')) %>%
  layout(annotations = d, yaxis = list(title = 'Calories'), xaxis = list(title = 'Fat'))
p <- subplot(p3, p4, titleX = TRUE, titleY = TRUE) %>%
  layout(showlegend = FALSE)
p
