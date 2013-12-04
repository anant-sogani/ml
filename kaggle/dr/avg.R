#!/usr/bin/env Rscript

library(png)

# Square Image Size.
S = 28

# Raw Data.
data = as.matrix(read.table("train.csv", header = TRUE, sep = ",",
                            colClasses = sapply(1:(1 + S^2), class)))

# Per Digit: Average Image Vector.
A = sapply(0:9, function (d) {colMeans(data[(data[, 1] == d), -1]) })

# Output Image Dimensions.
H = 2; W = 5

# Output Image Matrix.
I = matrix(nrow = S * H, ncol = S * W)

for (d in 0:9) {

    # Convert Average Image Vector to PNG-compatible Matrix.
    png = matrix(1 - (A[, 1 + d] / 255), ncol = S, byrow = TRUE)

    # Insert into Output Image at appropriate location.
    h = S * (d %/% W); w = S * (d %% W)
    I[h + 1:S, w + 1:S] = png
}

writePNG(I, target = "avg.png")
