H0 <- rep('B', 5)
cat('H0: ',H0, '\n')
H1 <- rep(c('B','W'), c(4,1))
cat('H1: ',H1, '\n')
H2 <- rep(c('B','W'), c(3,2))
cat('H2: ',H2, '\n')
H3 <- rep(c('B','W'), c(2,3))
cat('H3: ',H3, '\n')
H4 <- rep(c('B','W'), c(1,4))
cat('H4: ',H4, '\n')
H5 <- rep('W', 5)
cat('H5: ',H5, '\n')

probB <- c(1, 4/5, 3/5, 2/5, 1/5, 0)
probW <- c(0, 1/5, 2/5, 3/5, 4/5, 1)

bayes <- function(prob, extr) {
    if (extr == 'B') {
        num <- probB * prob
        den <- sum(probB * prob)
        }
    else {
        num <- probW * prob
        den <- sum(probW * prob)
    }
    return(num/den)
}

f <- function() {
    
    par(mfrow=c(2,3))
    
    c <- readline(prompt="How many extraction? ")
    extraction <- (NULL)
    
    y0 <- (NULL)
    y1 <- (NULL)
    y2 <- (NULL)
    y3 <- (NULL)
    y4 <- (NULL)
    y5 <- (NULL)
    
    prob <- rep(1/6,6)
    for (i in 1:c) {
        ex <- readline(prompt="What color have you drawn? ")
        prob <- bayes(prob, ex)
        cat(prob, '\n')
        y0 <- c(y0,prob[1])
        plot(1:i, y0 ,main = "H0",col = "#7209B7",bg = "#7209B7",pch = 21, 
             xlab = "Number of extraction", ylab = "Probability",xaxt = "n", xlim=c(0,50), ylim=c(0,1))
        
        y1 <- c(y1,prob[2])
        plot(1:i, y1 ,main = "H1",col = "#7209B7",bg = "#7209B7",pch = 21, 
             xlab = "Number of extraction", ylab = "Probability",xlim=c(0,50), ylim=c(0,1))
        
        y2 <- c(y2,prob[3])
        plot(1:i, y2 ,main = "H2",col = "#7209B7",bg = "#7209B7",pch = 21,
             xlab = "Number of extraction",ylab = "Probability",xlim=c(0,50), ylim=c(0,1))
        
        y3 <- c(y3,prob[4])
        plot(1:i, y3 ,main = "H3",col = "#7209B7",bg = "#7209B7",pch = 21,
             xlab = "Number of extraction",ylab = "Probability",xlim=c(0,50), ylim=c(0,1))
        
        y4 <- c(y4,prob[5])
        plot(1:i, y4 ,main = "H4",col = "#7209B7",bg = "#7209B7",pch = 21,
             xlab = "Number of extraction",ylab = "Probability",xlim=c(0,50), ylim=c(0,1))
        
        y5 <- c(y5,prob[6])
        plot(1:i, y5 ,main = "H5",col = "#7209B7",bg = "#7209B7",pch = 21,
             xlab = "Number of extraction",ylab = "Probability",xlim=c(0,50), ylim=c(0,1))
        
        }

    return(prob)
}

f()