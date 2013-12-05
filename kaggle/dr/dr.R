#!/usr/bin/env Rscript

library(e1071)

#################################################################
#           I N P U T    P R O C E S S I N G                    #
#################################################################

# Square Image Size.
S = 28

# Raw Data.
data = as.matrix(read.table("train.csv", header = TRUE, sep = ",",
                            nrows = 1001,
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


#################################################################
#           MODEL SELECTION USING CROSS-VALIDATION              #
#################################################################

#
# Learning Model:       Soft-Margin SVM
# Parameters to choose: (a) Kernel
#                       (b) Margin Violation Cost

# 
# Choice of Kernels.
#                                Gamma   Coef0  Degree
kernels = matrix(c('linear',        0,      0,     0, 
                   'polynomial',    1,      1,     2, 
                   'polynomial',    1,      1,     3, 
                   'polynomial',    1,      1,     4, 
                   'radial',        1,      0,     0, 
                   'radial',        2,      0,     0, 
                   'radial',        3,      0,     0, 
                   'radial',        10,     0,     0),
                 nrow = 4,
                 dimnames = list(c('K', 'gamma', 'coef0', 'Q')))

#
# Choice of Margin Violation Costs.
#
C = 10^c(-3:1)

# Cross-Validation Error Matrix.
Ecv = matrix(nrow = length(C), ncol = ncol(kernels),
             dimnames = list(factor(C)))

# Train all Models and obtain Ecv.
for (c in C)
for (i in 1:ncol(kernels)) {
    
    print(paste("Training Kernel", i, "Cost", c))

    model = svm(X, Y, type = "C-classification", cost = c,
                kernel  = kernels['K',     i],
                gamma   = kernels['gamma', i],
                coef0   = kernels['coef0', i],
                degree  = kernels['Q',     i],
                cross = 10)

    ecv = 1 - (model$tot.accuracy / 100)

    Ecv[as.character(c), i] = ecv
}

print(Ecv)
print(min(Ecv))
