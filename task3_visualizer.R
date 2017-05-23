
library(ggplot2)

ratings_data2 <- read.csv("C:/devstore/DM/project/ratings_edited_ingredients_data2.csv", sep = ",",
                         col.names = c("ingredients", "1 star", "2 stars", "3 stars", "4 stars", "5 stars"))

barplot(height = as.matrix(ratings_data[,-1]), main="Ingredients vs ratings",
        xlab="Ingredients", 
        legend = colnames(as.matrix(ratings_data[,-1])))

r_dat2 <- as.data.frame(ratings_data[,-1], col.names = colnames(ratings_data[,-1]))
rownames(r_dat2) <- ratings_data[,1]

mydf <- data.frame( X1 = c(2,4,1), X2 = c(3,2,NA), x3 = c(4,1,NA), row.names=c("A","B","C") )


barplot(height = as.matrix(r_dat2), main="Ingredients vs ratings",
        xlab="Ingredients", 
        legend = colnames(ratings_data[,-1]))



enrichment_df[ "day" ] <- rownames(enrichment_df)
df.molten2 <- melt( ratings_data2, id.vars="ingredients", value.name="Frequency", variable.name="Ratings" )

ggplot(df.molten2, aes( x = ingredients, y = Frequency, fill = Ratings ) ) + 
  geom_bar( position = "stack", stat = "identity" ) + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_fill_brewer(palette = "Set1") + 
  labs(x = "Ingredients", y = "Number of meals", title = "Relationship of Ingredients to Meal Ratings")


calories_data <- read.csv("C:/devstore/DM/project/calories_edited_ingredients_data.csv", sep = ",", header = TRUE)

cal.molten <- melt( calories_data, id.vars="ingredient", value.name="Frequency", variable.name="Calories" )

ggplot(cal.molten, aes( x = ingredient, y = Frequency, fill = Calories ) ) + 
  geom_bar( position = "stack", stat = "identity" ) + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_fill_brewer(palette = "Set1") + 
  labs(x = "Ingredient", y = "Number of meals", title = "Relationship of Ingredients to Calories in Meals")
