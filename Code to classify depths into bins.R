
# Sample data
df <- data.frame(depth = c(3, 8, 15, 25, 9, 18, 4))

# Create bins for depth
df$depth_category <- cut(df$depth, 
                         breaks = c(-Inf, 5, 10, 20, Inf), 
                         labels = c("less than 5 meters", 
                                    "between 5 and 10 meters", 
                                    "between 10 and 20 meters", 
                                    "more than 20 meters"), 
                         right = FALSE)

# View the updated data frame
print(df)

# Sample data
df <- data.frame(depth = c(3, 8, 15, 25, 9, 18, 4, 85, 95, 105))

# Create bins for depth with numerical values
df$depth_category_num <- as.numeric(cut(df$depth, 
                                        breaks = c(-Inf, 5, 10, 20, Inf), 
                                        labels = c(1, 2, 3, 4),
                                        right = FALSE))

# View the updated data frame
print(df)


