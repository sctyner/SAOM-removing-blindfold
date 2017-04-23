# florentine families recreate
library(ergm)
data(florentine)
flobusiness
flomarriage

plot(flobusiness)
plot(flomarriage)

list.vertex.attributes(flobusiness)
library(geomnet)
flobusi <- fortify(flobusiness)
flomar <- fortify(flomarriage)
flomar$from <- paste0("V", flomar$from)
flomar$to <- paste0("V", flomar$to)
ggplot(data = flomar) + 
  geom_net(aes(from_id = from, to_id = to, size = wealth))
