

rownames(mydata) <- scenario.names #or parameter values? (1,2,3,4)

# plot CE plane
plot(x,y)

# select the dominant scenarios
scenarios <- identify(x, y, labels=row.names(mydata))