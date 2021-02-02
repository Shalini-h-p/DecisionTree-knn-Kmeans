# 3 kMeans clustering
library("foreign");

htru <- read.csv("data/htru.csv");
# factorizing the HTRU value from 0 and 1 to Negative and Positive
htru$HTRU <- factor(htru$HTRU, labels= c("Negative", "Positive"));

# setting clustering data
htruC <- htru;
htruC$HTRU <- NULL;
summary(htru);

# normalizing htruC values.
htruC <- scale(htruC);
summary(htruC)

# setting seed as 1  we get tot.withinss 92749.47
set.seed(1);
model <- kmeans(htruC, 2)
table(htru$HTRU, model$cluster);
model$tot.withinss

# plot for recongnizing the clusters centre visually
plot(htruC[,c("meanOfProfile", "sdOfProfile")], col = model$cluster)
# plot cluster centers
points(model$centers[,c("meanOfProfile", "sdOfProfile")], col = 8:9, pch = 11, cex=2)


# setting seed as 2 or more we get tot.withinss as 92209.22 and other lower values
set.seed(2);
model <- kmeans(htruC, 2)
table(htru$HTRU, model$cluster);
model$tot.withinss
