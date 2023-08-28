#
# Bivariate Correlation
#

r <- cor(X, Y) # calculate the correlation coefficient
r <- cor(X, Y, Z, Q) # calculate the correlation coefficient across more >2 variables?

r2 <-r^2
print(r, r2) # print r and r squared

# Once we know the size of r, we can determine if it's size is significantly different from 0
cor.test( x = parenthood$dan.sleep, y = parenthood$dan.grump )

# But you can also get p values too, along with alpha adjustments.
library(lsr)
correlate(parenthood, test=TRUE)

# Condfidence intervals
confint(object = regression.2, level = .99)