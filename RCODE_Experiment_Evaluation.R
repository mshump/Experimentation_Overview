##XXXXXXXXXXXXXX##
## Chi-sqr Test ##
##XXXXXXXXXXXXXX##

arm_1 <- c(100,900)       # i.e. Intervention             
arm_2 <- c(10,990)        # i.e. Control              
          
exp_df <- data.frame(rbind(arm_1,arm_2))        #assimilate data into simple 2x2 table
names(exp_df) <- c("purchase", "no purchase")   # add columns names (optional)
exp_df                                          # verify data looks right

# Chi-sqr test function
chi_out <- chisq.test(exp_df)

str(chi_out)            # check out the parameters of the output
chi_out$p.value         # select one! p-value is the most important


# is this statistically signficant ??
pval_cut <- 0.05

isSignificant <- if(chi_out$p.value < pval_cut){"These are statistically different"
                        }else{"These are note statistially different"}

isSignificant


##XXXXXXXXXXXXXX##
##    t-test    ##
##XXXXXXXXXXXXXX##



arm_1 = c(18,21,22,19,20,22,23,24,22,21)  # Intervention
arm_2 = c(16,17,16,18,14,12,15,14,15,17)  # Control




# Check out what the distribution looks like
plot(density(arm_1))            # Looks about "normal"?
plot(density(arm_2))            # Looks about "normal"?

#Check out how averages and variation line up (does it look like there is overlap?)
boxplot(arm_1,arm_2,ylab="Avg Cart Size ($)",
                   names=c("arm_1","arm_2"),
                   main="Avg Cart Size ($) By Arm")


# Perform the t-test | lets assume the variations are about equal and they are approx normal
t_test_out <- t.test(arm_1,arm_2)


str(t_test_out)            # check out the parameters of the output
t_test_out$p.value         # select one! p-value is the most important


# is this statistically signficant ??
pval_cut <- 0.05

isSignificant <- if(t_test_out$p.value < pval_cut){"These are statistically different"
}else{"These are note statistially different"}

isSignificant


## Calculate Standard Deviation

std_1 <- sqrt(var(arm_1)/length(arm_1))
std_2 <- sqrt(var(arm_2)/length(arm_2))
