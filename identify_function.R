sens <- data.frame(x=unlist(performance(predictions, "sens")@x.values), 
                   y=unlist(performance(predictions, "sens")@y.values))
spec <- data.frame(x=unlist(performance(predictions, "spec")@x.values), 
                   y=unlist(performance(predictions, "spec")@y.values))

sensspec = data.frame(x=c(sens$x,spec$x), y = c(sens$y,spec$y))

plot(sensspec)
identify(sensspec, n = 1)