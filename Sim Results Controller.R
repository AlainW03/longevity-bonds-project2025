

simulations <- 10
Bond.Proportion <- 0
improvement.factor <- 0
coupon.rate <- 0.1425
inital.members <- 15
Original.Fund <- c()

system.time(source("Merged scripts with Controls.R"))

Original.Fund.Avg <- mean(Original.Fund)
result <- as.data.frame(rbind(simulations,
                              inital.members,
                              Prob.of.ruin,
                              VAR,
                              (100*VAR/Original.Fund.Avg),
                              Bond.Proportion,
                              improvement.factor,
                              coupon.rate))

colnames(result) <- "Results"
rownames(result) <- c("Simulations",
                      "Initial Nr of Members",
                      "Prob of Ruin",
                      "VAR @ 95%",
                      "VAR as % of Original Fund",
                      "Prop invested in Bond",
                      "Mort Improvement Factor",
                      "Coupon Rate")
View(result)
