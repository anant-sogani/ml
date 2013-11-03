#!/usr/bin/env Rscript

# Number of Data Sets.
K = 1000

# Target Function.
f = function (x) sin(pi * x)

# Generates one input point from [-1, 1].
random_x = function (void) runif(1, -1, 1)

# X Part of Training Set.
X = vector(length = 2)

# Y Part of Training Set.
Y = vector(length = 2)

#
# Per Data Set: w returned by Linear Regression.
#
wD = matrix(nrow = K, ncol = 2)

#
# Per Data Set: g obtained from Linear Regression.
#
gD = vector(mode = "list", length = K)

# Average Hypothesis and its weight vector.
gbar = NA; wbar = NA

# Hypotheses Sets to run the Experiment on.
Hs = c('a', 'ax', 'ax + b', 'ax2', 'ax2 + b')

# Creates a Hypothesis given Weight w and type.
create_g = function (w, type) {
    force(w)

    switch(type,
       '1' = function (x) w[1],
       '2' = function (x) w[1] * x,
       '3' = function (x) w[1] * x + w[2],
       '4' = function (x) w[1] * x * x,
       '5' = function (x) w[1] * x * x + w[2])
}
    
# Weight that minimizes Mean Squared Error.
calc_w = function (type) {
    switch(type,
       '1' = c(mean(Y), 0),
       '2' = c(sum(X * Y) / sum(X^2), 0),
       '3' = c(diff(Y) / diff(X), diff(X * rev(Y)) / diff(X)),
       '4' = c(sum((X^2) * Y) / sum(X^4), 0),
       '5' = c(diff(Y) / diff(X^2), diff(X^2 * rev(Y)) / diff(X^2)))
}

# Bias = ExpX<square(gbar(x) - f(x))>
bias = function () {
    T = 1000
    X_ = sapply(1:T, random_x)
    mean((sapply(X_, gbar) - sapply(X_, f))^2)
}

# Variance = ExpD<< ExpX<square(gD(x) - gbar(x))> >>
variance = function () {
    T = 1000
    X_ = sapply(1:T, random_x)

    expX = function (g)  {
        mean((sapply(X_, g) - sapply(X_, gbar))^2)
    }

    mean(sapply(gD, expX))
}

# Eout = ExpD<< ExpX<square(gD(x) - f(x))> >>
eout = function () {
    T = 1000
    X_ = sapply(1:T, random_x)

    expX = function (g)  {
        mean((sapply(X_, g) - sapply(X_, f))^2)
    }

    mean(sapply(gD, expX))
}

# Paint Routines.
paint_a = function () {
    for (i in 1:K) {
        w = wD[i, ]
        curve(w[1] * 1 + w[2] * x, lty=3, add=TRUE, from=-1, to=1)
    }
    curve(wbar[1] * 1 + wbar[2] * x, lty=1, col="yellow", add=TRUE)
}

paint_ax_b = function () {
    for (i in 1:K) {
        w = wD[i, ]
        curve(w[1] * x + w[2], lty=3, add=TRUE)
    }
    curve(wbar[1] * x + wbar[2], lty=1, col="yellow", add=TRUE)
}

paint_ax2_b = function () {
    for (i in 1:K) {
        w = wD[i, ]
        curve(w[1] * x * x + w[2], lty=3, add=TRUE)
    }
    curve(wbar[1] * x * x + wbar[2], lty=1, col="yellow", add=TRUE)
}

print("--------------------------------")
print(paste("Data Sets used K =", K))
print("--------------------------------")

# For all Hypotheses sets.
for (j in 1:5) {

    # Loop over K Data Sets.
    for (i in 1:K) {

        # Generate Training Set.
        X = sapply(1:2, random_x)
        Y = sapply(X, f)

        # Obtain w from Linear Regression.
        w = calc_w(as.character(j))

        # Final Hypothesis g.
        g = create_g(w, j)

        # Save.
        wD[i, ] = w
        gD[[i]] = g
    }

    # Calculate the Average Hypothesis.
    wbar = colMeans(wD)
    gbar = create_g(wbar, j)

    print(paste("H:'", Hs[j], "'  with gbar:"))
    print(wbar)

    # Calculate Bias.
    print(paste("Bias     =", bias()))

    # Calculate Variance.
    print(paste("Variance =", variance()))

    # Calculate Eout.
    print(paste("Eout     =", eout()))

    print("--------------------------------")

    # Paint curves.
    image = paste("bias_variance_", j, ".png", sep="")
    png(image)
    plot(0, 0, type="n", xlab="x", ylab="y")
    curve(sin(pi * x), -1, 1, col="blue")
    abline(0, 0)

    switch(as.character(j),
        '1' = paint_a(),
        '2' = paint_ax_b(),
        '3' = paint_ax_b(),
        '4' = paint_ax2_b(),
        '5' = paint_ax2_b())

    legend("bottomright", c("gD", "gbar", "Sin(pi * x)"),
           col=c("black", "yellow", "blue"),
           lty=c(3, 1, 1))

    dev.off()
    browseURL(image)
}
