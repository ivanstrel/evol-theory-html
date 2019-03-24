hw_isolateion <- function(n_gen = 150, isolate = FALSE, fp = 0.5){
    #Function for simulation of population differentiation
    #after isolateion
    #Initial population size =
    pop_size <- 1800
    #palette for plotting
    palette <- c("brown1", "olivedrab2", "lightblue")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   Main part
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isolate){
        fp1 <- fp
        fp2 <- fp
        pop_1 <- sample(c(1, 2, 3), 900,
                     prob = c(fp1 ** 2, 2 * fp1 * (1 - fp1), (1 - fp1) ** 2), rep = TRUE)
        pop_2 <- sample(c(1, 2, 3), 900,
                     prob = c(fp2 ** 2, 2 * fp2 * (1 - fp2), (1 - fp2) ** 2), rep = TRUE)
        restab_1 <- matrix(as.numeric(table(pop_1)), 1)
        restab_2 <- matrix(as.numeric(table(pop_2)), 1)
        for (i in c(1:(n_gen - 1))) {
            fp1 <- (restab_1[length(restab_1[, 1]), 1] * 2 + restab_1[length(restab_1[, 1]), 2]) / (pop_size)
            fp2 <- (restab_2[length(restab_2[, 1]), 1] * 2 + restab_2[length(restab_2[, 1]), 2]) / (pop_size)
            pop_1 <- sample(c(1, 2, 3), 900, prob = c(fp1**2, 2 * fp1 * (1 - fp1), (1 - fp1)**2), rep = TRUE)
            pop_2 <- sample(c(1, 2, 3), 900, prob = c(fp2**2, 2 * fp2 * (1 - fp2), (1 - fp2)**2), rep = TRUE)
            restab_1 <- rbind(restab_1, matrix(as.numeric(table(c(pop_1, c(1, 2, 3)))), 1) - c(1, 1, 1))
            restab_2 <- rbind(restab_2, matrix(as.numeric(table(c(pop_2, c(1, 2, 3)))), 1) - c(1, 1, 1))
        }
        pop <- c(pop_1, pop_2)
    }else{
        fp1 <- fp
        pop_1 <- sample(c(1, 2, 3), 1800, prob = c(fp1**2, 2 * fp1 * (1 - fp1), (1 - fp1)**2), rep = TRUE)
        restab_1 <- matrix(as.numeric(table(pop_1[1:900])), 1)
        restab_2 <- matrix(as.numeric(table(pop_1[901:1800])), 1)
        for (i in c(1:(n_gen - 1))) {
            fp1 <- (restab_1[length(restab_1[, 1]), 1] * 2 +
                    restab_1[length(restab_1[, 1]), 2]) / (pop_size * 2)
            fp1 <- fp1 + (restab_2[length(restab_2[, 1]), 1] * 2 +
                          restab_2[length(restab_2[, 1]), 2]) / (pop_size * 2)
            pop_1 <- sample(c(1, 2, 3), 1800,
                            prob = c(fp1**2, 2 * fp1 * (1 - fp1), (1 - fp1)**2), rep = TRUE)
            restab_1 <- rbind(restab_1,
                              matrix(as.numeric(table(c(pop_1[1:900], c(1, 2, 3)))), 1) - c(1, 1, 1))
            restab_2 <- rbind(restab_2,
                              matrix(as.numeric(table(c(pop_1[901:1800], c(1, 2, 3)))), 1) - c(1, 1, 1))
        }
        pop <- pop_1
    }
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   Plotting part
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Set layout
    layout(matrix(c(1, 2, 1, 3), 2, 2))

    #Grid preparation
    xy <- rbind(expand.grid(1:30, 1:30), expand.grid(31:60, 1:30))
    xy <- cbind(xy, pop)
    foo <- function(x, col){
        #Internal function to plot polygons in given locations
        A <- x[1] + c(-0.5, 0.5, 0.5, -0.5)
        B <- x[2] + c(0.5, 0.5, -0.5, -0.5)
        polygon(A, B, col = col[x[3]], border = "lightgray")
    }

    #Cell plotting
    par(mar = c(1, 0, 0, 0))
    x_lim <- c(0.5, 60.5)
    y_lim <- c(0.5, 30.5)
    plot(NA, xlim = x_lim, ylim = y_lim,
         bty = "n", xaxt = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "")
    A <- apply(xy, 1, FUN = foo, col = palette)
    #add isolateion line
    if (isolate){
        abline(v = 30.5, lwd = 4)
    }else{
        abline(v = 30.5, lwd = 4, col = "white")
    }
    par(mar = c(0, 0, 0, 0))
    legend("bottom", legend = c("AA","Aa","aa"),
           fill = rev(c("lightblue", "olivedrab2", "brown1")),
           bty = "n", horiz = TRUE, cex = 1.5, xpd = TRUE, ins = -0.08)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Add proportions scatter plot for pop_1
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Prepare initial proportions
    faa <- (1 - fp)**2 * (pop_size / 2)
    fAa <- (2 * fp * (1 - fp)) * (pop_size / 2)
    par(mar = c(4, 4, 0, 0), mgp = c(2, 1, 0))
    plot(NULL, ylim = c(1, pop_size / 2), xlim = c(1, n_gen),
         xlab = "Поколений", ylab = "Количество генотипов", font.lab = 2, font = 2, bty = "n")
    AA <- restab_1[, 1]
    Aa <- restab_1[, 2]
    aa <- restab_1[, 3]
        #Create polygons for each genotype (by count)
    polygon(c(1, 1:n_gen, n_gen), c(0, aa, 0), col = "lightblue")
    Aa <- Aa + aa
    polygon(c(1:n_gen, n_gen:1), c(Aa, rev(aa)), col = "olivedrab2")
    AA <- AA + Aa
    polygon(c(1:n_gen, n_gen:1), c(AA, rev(Aa)), col = "brown1")
        #Add lines of starting genotypes counts
    abline(h = faa, lty = 2, lwd = 2)
    abline(h = faa + fAa, lty = 2, lwd = 2)
    legend("top", legend = ("Субпопуляция 1"), ins = 0.05, bty = "n", cex = 1.3)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Add proportions scatter plot for pop_2
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    par(mar = c(4, 0, 0, 4), mgp = c(2, 1, 0))
    plot(NULL, ylim = c(1, pop_size / 2), xlim = c(1, n_gen),
         xlab = "Поколений", ylab = "", yaxt = "n", font.lab = 2, font = 2, bty = "n")
    AA <- restab_2[, 1]
    Aa <- restab_2[, 2]
    aa <- restab_2[, 3]
        #Create polygons for each genotype (by count)
    polygon(c(1, 1:n_gen, n_gen), c(0, aa, 0), col = "lightblue")
    Aa <- Aa + aa
    polygon(c(1:n_gen, n_gen:1), c(Aa, rev(aa)), col = "olivedrab2")
    AA <- AA + Aa
    polygon(c(1:n_gen, n_gen:1), c(AA, rev(Aa)), col = "brown1")
        #Add lines of starting genotypes counts
    abline(h = faa, lty = 2, lwd = 2)
    abline(h = faa + fAa, lty = 2, lwd = 2)
    legend("top", legend = ("Субпопуляция 2"), ins = 0.05, bty = "n", cex = 1.3)
    axis(side = 4, font = 2)
    mtext(side = 4, "Количество генотипов", font = 2, cex = 0.75, line = 2)

    #Return final proportions
    res <- rbind(restab_1[n_gen, ], restab_2[n_gen, ])
    colnames(res) <- c("AA", "Aa", "aa")
    return(res)
}