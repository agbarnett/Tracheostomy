# 1_event_diagram.R
# Feb 2019

names = c('Ventilation\nstarted','Tracheostomy','Outcome','Died in\nICU')
M <- matrix(nrow = 4, ncol = 4, byrow = TRUE, data = 0)
M[2,1] = "' '"
M[3,2] = "' '"
M[3,1] = "' '"
M[2,3] = "' '" # backwards
M[4,1] = "' '"
M[4,2] = "' '"
M[4,3] = "' '"
pos = matrix(ncol=2, byrow = TRUE, data = c(
  0.2,0.1,
  0.3,0.8,
  0.8,0.1,
  0.8,0.8
))
par(mai=c(0.1, 0.1, 0.1, 0.1))
plotmat(M, pos = pos, name = names, shadow.size = 0, curve = 0,
          lwd = 1, box.lwd = 2, cex.txt = 0.8, arr.pos = 0.4,
          box.size = 0.14, box.type = "square", box.prop = 0.5,
          main = NULL)
