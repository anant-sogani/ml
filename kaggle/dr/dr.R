#!/usr/bin/env Rscript

library(ggplot2)

# Square Image Size.
S = 28

# Raw Data.
data = as.matrix(read.table("train.csv", header = TRUE, sep = ",",
                            nrows = 42001,
                            colClasses = sapply(1:(1 + S^2), class)))
# Feature Extraction.
features = function (v) {

    image = matrix(v, ncol = S, byrow = TRUE)

    # Feature 1 - Intensity.
    intensity = mean(image)

    # Feature 2 - Symmetry.

    # Horizontal Symmetry.
    hsym = mean(abs(image[(S/2 + 1):S, ] - image[(S/2):1, ]))

    # Vertical Symmetry.
    vsym = mean(abs(image[, (S/2 + 1):S] - image[, (S/2):1]))

    symmetry = mean(hsym, vsym)

    # Feature Vector.
    c(intensity, symmetry)
}

# Data Set.
X = t(apply(data[, -1], 1, features))
Y = data[, 1]

for (d in 0:9) {
    name = paste(d, ".png", sep = "")
    png(name)
    colors = c("brown", "darkgoldenrod", "blue4", "cornflowerblue",
               "aquamarine4", "chartreuse", "chartreuse3",
               "coral", "burlywood", "blueviolet")

    select = (Y %in% d)

    plot(X[select, 2], X[select, 1],
         xlab = "Symmetry", ylab = "Intensity",
         main = paste("Digit", d),
         col = c(colors[1 + Y[select]]), pch = 20)

    dev.off()
}
