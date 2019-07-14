
# MaxBet 
setwd("/Users/Gaston/Downloads/")

wines_data = read.csv("wines.csv", sep=";")
head(wines_data)

wines = t(wines_data[,3:10])
colnames(wines) = paste(wines_data$assessors, wines_data$variables, sep='.')
wines

Data = scale(wines, scale=FALSE)

blocks = list(1:4, 5:7, 8:11, 12:14)
K = length(blocks)

# Data in blocks
Blocks = vector("list", K)
for (k in 1:K) {
  Blocks[[k]] = Data[,blocks[[k]]]  
}
names(Blocks) = paste("block", 1:K, sep='')

# list to store scores
Scores = vector("list", K)

# list of vectors (loadings)
u = vector("list", K)
v = as.list(rep(0, K))
for (k in 1:K)
{
  u[[k]] = rep(1, length(blocks[[k]]))
}

# iterative procedure
iters = 1
repeat 
{
  # for each block k
  for (k in 1:K)
  {
    # select block k
    Xk = Blocks[[k]]
    # initialize vk
    vk = 0
    # for each block j
    for (j in 1:K) {
      Xj = Blocks[[j]]
      vk = vk + (t(Xk) %*% Xj) %*% u[[j]]
    }
    vk_norm = sqrt(sum(vk * vk))
    v[[k]] = as.numeric(vk / vk_norm)
  }
  # check convergence
  uv_diff = unlist(v) - unlist(u)
  if (sqrt(sum(uv_diff^2)) < 0.0001) break
  iters = iters + 1
  u = v
}

# deflation
for (k in 1:K)
{
  Xk = Blocks[[k]]
  # store scores
  Scores[[k]] = Xk %*% v[[k]]
  Blocks[[k]] = Xk - Xk %*% (v[[k]] %*% t(v[[k]]))
}

names(v) = paste("v", 1:K, sep='')

# v
# iters

results = list(
  scores = Scores,
  loadings = v,
  iterations = iters
  )





