BotNeckSqr <- function(pop_size = 1600, bot_size = 30, p_a = 1, p_b = 1, p_c = 1, p_d = 1){
    #Function to simulate bottle neck effect in population
    #pop_size -- initial size of population
    #bot_size -- population size after bottle neck reduction
    #p_a, p_b, p_c, p_d -- initial probabilities of a,b,c and d genotipes

    #It looks best with: dev.new(height=8,width=14,unit="in")
    
    # Initiate output table
    out_df <- data.frame("Генотипы" = c("P(A)", "P(B)", "P(C)", "P(D)"),
                         "Исходная_популяция" = rep(NA, 4),
                         "Выборка_1" = rep(NA, 4),
                         "Выборка_2" = rep(NA, 4),
                         "Выборка_3" = rep(NA, 4),
                         "Выборка_4" = rep(NA, 4))

    # Calculate square properties
    side <- ceiling(sqrt(pop_size))#Calculate plotting square dimensions
    genotypes <- rep(NA, side**2)
    #Generate initial combination of genotipes
    genotypes[c(1:pop_size)] <- sample(c(2, 3, 4, 5), pop_size, replace = TRUE, prob = c(p_a, p_b, p_c, p_d))
    genotypes <- rev(genotypes)
    #Creating matrix with polygones centres location
    square_mat <- expand.grid(c(1:side), c(1:side))

    draw_square <- function(x){
        #Internal function to plot polygons in given locations
        x_coords <- x[1] + c(-0.5, 0.5, 0.5, -0.5)
        y_coords <- x[2] + c(0.5, 0.5, -0.5, -0.5)
        polygon(x_coords, y_coords, col = x[3], border = "lightgray")
    }

    #Create layout for future plotting
    #dev.new(height = 8, width = 14, unit = "in")
    layout(matrix(c(1, 1, 1, 1, 1, 1, 2, 3, 4, 5), 2, 5))
    par(mar = c(0, 0, 2, 0))

    #Calculating obtained probabilities of genotipes
    prob_a <- as.character(round(sum(genotypes == 2, na.rm = TRUE) / pop_size, 4))
    prob_b <- as.character(round(sum(genotypes == 3, na.rm = TRUE) / pop_size, 4))
    prob_c <- as.character(round(sum(genotypes == 4, na.rm = TRUE) / pop_size, 4))
    prob_d <- as.character(round(sum(genotypes == 5, na.rm = TRUE) / pop_size, 4))

    # Update out_df
    out_df[, 2] <- c(prob_a, prob_b, prob_c, prob_d)

    #Create string to be plotted as main text (it will contain probabilities)
    #prob_text <- paste("A=", prob_a, " ", "B=", prob_b, " ", "C=", prob_c, " ", "D=", prob_d, sep = "")

    plot(x = NULL, xlim = range(1:side) + c(-0.5, 0.5), ylim = range(1:side) + c(-0.5, 0.5),
    bty = "n", xaxt = "n", yaxt = "n", main = "Исходная популяция", cex.main = 1.5) # main = prob_text)
    #Plot polygones
    apply(cbind(square_mat, genotypes), 1, draw_square)

    #Add 4 new frames with bottle neck population size
    for (i in c(1:4)){
        #Recalculate size of bottle neck square
        side_small <- ceiling(sqrt(bot_size))
        genotypes_small <- rep(NA, side_small ** 2)

        #Generate bottle neck genotipes
        genotypes_small[c(1:bot_size)] <- sample(genotypes[c(1:pop_size)], bot_size, rep = TRUE)
        genotypes_small <- rev(genotypes_small)
        square_mat_b <- expand.grid(c(1:side_small), c(1:side_small))

        #Calculating obtained probabilities of genotipes after bottle neck
        prob_a <- as.character(round(sum(genotypes_small == 2, na.rm = TRUE) / bot_size, 4))
        prob_b <- as.character(round(sum(genotypes_small == 3, na.rm = TRUE) / bot_size, 4))
        prob_c <- as.character(round(sum(genotypes_small == 4, na.rm = TRUE) / bot_size, 4))
        prob_d <- as.character(round(sum(genotypes_small == 5, na.rm = TRUE) / bot_size, 4))
        par(mar = c(2, 0, 4, 0))

        #Create string to be plotted as main text
        #prob_text <- paste("A=", prob_a, " ", "B=", prob_b, " ", "C=", prob_c, " ", "D=", prob_d, sep = "")

        plot(x = NULL, xlim = range(1:side_small) + c(-0.5, 0.5), ylim = range(1:side_small) + c(-0.5, 0.5),
            bty = "n", xaxt = "n", yaxt = "n", 
            main = paste("Выборка", i), cex.main = 1.5) # main = prob_text)

        # Update out_df
        out_df[, i + 2] <- c(prob_a, prob_b, prob_c, prob_d)
        
        #Add polygones
        apply(cbind(square_mat_b, genotypes_small), 1, draw_square)
    }
    return(out_df)
}
