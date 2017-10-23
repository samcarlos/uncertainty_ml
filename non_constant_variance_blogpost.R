## code to see how bartmachine deals with different kind of posteriors

library(bartMachine)
library(ggplot2)

# create dataset
set.seed(1)
x = seq(-10,10,.001)
y = rnorm(length(x),0,sin(x)+2)

variance_dataset = data.frame(x, y)


#build model
bart_variance = bartMachine(data.frame(x = x), y)

#predict and calculate stdevs
new_x = seq(-15,15,.1)
bart_sample_pred = bart_machine_get_posterior(bart_variance, data.frame(x = new_x))
pred_stdev = apply(bart_sample_pred$y_hat_posterior_samples,1,sd)

#create stdevs data.frames to plot with
pred_stdev_data = data.frame(stdev = pred_stdev, x = new_x, type=rep('predicted',length(new_x)) )
true_stdev_data = data.frame(stdev =  sin(x)+2, x = x, type=rep('true',length(x)) )
true_pred_stdev_data = rbind(pred_stdev_data, true_stdev_data )

#plots

ggplot(variance_dataset, aes(x = x, y = y)) + geom_point() + theme_bw() + 
  ggtitle('Original Dataset - Non-Constant Variance')

ggplot(true_pred_stdev_data, aes(x = x, y = stdev, colour = type )) + geom_line() +geom_smooth() + theme_bw() + 
  ggtitle('Predicted and Actual stdev vs x')

ggplot(data.frame(true_stdev =sqrt( sin(new_x)+2), pred_stdev)[which(abs(new_x)<10),], aes(x = pred_stdev, y = true_stdev) ) + geom_point()+ theme_bw() + 
  ggtitle('Predicted vs Actual stdev')