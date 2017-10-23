# setup bimodal relationship

library(reshape)
library(bartMachine)
library(ggplot2)
library(ggridges)

#generate data
x1 = runif(1000,-1, 1)
x2 = runif(1000,-1, 1)
x3 = runif(1000,-1, 1)

y = .1*x1 + 5*x2 -10*x2*(x3>0) + rnorm(1000, 0, 1)

ggplot(data.frame(x2,y), aes(x = x2, y = y)) + geom_point()+ theme_bw() + 
  ggtitle('Generated Data Relationship')

#build model and predict
bart_bimodal = bartMachine(data.frame(x = x2), y) 
new_x = seq(-1.5, 1.5, .1)

bart_sample_pred = bart_machine_get_posterior(bart_bimodal, data.frame(x = new_x))
pred_stdev = apply(bart_sample_pred$y_hat_posterior_samples,1,sd)


#calculate stdevs and observe posterior densities
pred_dist_data = data.frame(x = new_x,  bart_sample_pred$y_hat_posterior_samples)
pred_dist_data_melt = melt(pred_dist_data, id.vars = 'x')
ggplot(pred_stdev_data_melt, aes( y = x,x = value, group = x)) +
  geom_density_ridges() + theme_bw() + ggtitle('Posterior Densities of Selected X Values')


pred_stdev_data = data.frame(new_x,pred_stdev)
ggplot(pred_stdev_data, aes(x = new_x, y = pred_stdev)) + geom_line() + theme_bw() + 
  ggtitle('Posterior stdev by x')

