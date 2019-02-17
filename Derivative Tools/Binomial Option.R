# function for Pricing options via Binomial Trees

# returns a data frame having the binomial tree mapped into it
getBinomTree <- function(S0, K, vol, dT, r, qdiv, N_steps, isPut=F, isAmerican=F, 
                         isAvgStrike=F, isKO=F, isChooser=F, H=NA, Kc=NA, Kp=NA, choose_t1=NA)
{
  # number of nodes in tree
  N_nodes <- (N_steps+1)*(1 + (N_steps+1))/2 # sum(1:(N_steps+1)) = n*(a_1 + a_n)/2 
  
  # time difference between steps
  dT_step <- dT/N_steps
  
  # discount factor for one step
  D_step <- exp(-r*dT_step) 
  
  # up-movement factor
  u <- exp(vol*dT_step^0.5)
  
  # down-movement factor
  d <- 1/u 
  
  # probability of up movement in a risk-neutral world
  p <- (exp((r - qdiv)*dT_step) - d)/(u - d)
  
  # create a data frame to keep nodes of tree with a mapping: dt(row_i) -> node_id like:
  # 0|1|2|...|i                 ... |n
  # ....................................................
  #                                 (n+1)*(1+n+1)/2 - n
  #                             ...
  #          (i+1)*(1+i+1)/2-i
  #     4 ...
  #   2     
  # 1   5 ...(i+1)*(1+i+1)/2-j
  #   3
  #     6 ...
  #          (i+1)*(1+i+1)/2    ...
  #                                 (n+1)*(1+n+1)/2
  
  df <- data.frame(i_node = 1:N_nodes, step=NA, N_u = NA, S = NA, P = NA)
  if (isAmerican) {
    df <- data.frame(i_node = 1:N_nodes, step=NA, N_u = NA, S = NA, P = NA, dP_Exerc = NA)
  }
  if (isChooser) {
    df <- data.frame(i_node = 1:N_nodes, step=NA, N_u = NA, S = NA, P_Call = NA, P_Put = NA, P = NA)
  }
  
  for (i in N_steps:0)
  {
    for (j in 0:i)
    {
      i_node <- (i+1)*(1+i+1)/2-j  # get id of node
      df$step[i_node] <- i  # time step
      df$N_u[i_node] <- j  # number of u multiplication
      df$S[i_node] <- S0 * d^(i-j) * u^j # underlying asset price
      
      # at terminal node - payoff at expir:
      if (i == N_steps) 
      {
        df$P[i_node] <- ifelse(isPut, max(K - df$S[i_node], 0), max(df$S[i_node] - K, 0))
        if (isAmerican)
        {
          df$dP_Exerc[i_node] <- 0
        }
        if (isAvgStrike)
        {
          Savg <- getAvgStrikes(S0, j, i-j, d, u) # avg values of underlying price along all possible paths
          if (isPut) {
            df$P[i_node] <- mean(pmax(Savg - df$S[i_node], 0))
          } else {
            df$P[i_node] <- mean(pmax(df$S[i_node] - Savg, 0))
          }
        }
        if (isChooser)
        {
          df$P_Call[i_node] <- max(df$S[i_node] - Kc, 0)
          df$P_Put[i_node] <- max(Kp - df$S[i_node], 0)
          df$P[i_node] <- NA
        }
      } 
      # at interim nodes - backward induction:
      if (i < N_steps) 
      { 
        i_node_u <- ((i+1)+1)*(1+(i+1)+1)/2-(j+1) # find id of up node
        i_node_d <- ((i+1)+1)*(1+(i+1)+1)/2-j     # find if of down node
        df$P[i_node] <- D_step * (p*df$P[i_node_u] + (1-p)*df$P[i_node_d])
        if (isAmerican)
        {
          P_Exerc <- ifelse(isPut, max(K - df$S[i_node], 0), max(df$S[i_node] - K, 0))
          df$dP_Exerc[i_node] <- P_Exerc - df$P[i_node]
          df$P[i_node] <- max(df$P[i_node], P_Exerc)
        }
        if (isChooser)
        {
          df$P_Call[i_node] <- D_step * (p*df$P_Call[i_node_u] + (1-p)*df$P_Call[i_node_d])
          df$P_Put[i_node] <- D_step * (p*df$P_Put[i_node_u] + (1-p)*df$P_Put[i_node_d])
          if (i == round(choose_t1/(dT/N_steps), 0)){
            df$P[i_node] <- max(df$P_Call[i_node], df$P_Put[i_node])
          }
        }
      }
      if (isKO)
      {
        if (isPut) {
          if (df$S[i_node] >= H) { df$P[i_node] <- 0 }
        } else {
          if (df$S[i_node] <= H) { df$P[i_node] <- 0 }
        }
      }
    }
  }
  return(df)
}

# example of inputs
getExampleInputs <- function()
{
  # standard inputs for European and American options
  S0 <<- 100         # asset price at t=0
  K <<- 105          # strike
  vol <<- 0.15       # volatility
  dT <<- 1           # time to maturity (years)
  r <<- 0.05         # risk-free rate
  qdiv <<- 0         # dividend rate
  N_steps <<- 50     # number of steps in tree
  isPut <<- F        # default: F (Call)
  isAmerican <<- F   # default: F (European)
  
  # additional inputs for exotic options
  isAvgStrike <<- F  # is average strike options
  isKO <<- F         # is knock-out option
  H <<- 110          # barrier strike for knock-out
  isChooser <<- F    # is chooser option
  Kc <<- 105         # call strike for chooser option
  Kp <<- 95          # put strike for chooser option
  choose_t1 <<- 0.5  # time to choose for chooser option
}

# "envelope" functions for different type of options
# chooser option
getBinomTree.chooser <- function(S0, K, vol, dT, r, qdiv, N_steps, Kc, Kp, choose_t1)
{
  df <- getBinomTree(S0, K, vol, dT, r, qdiv, N_steps, isPut=F, isAmerican=F, 
                     isAvgStrike=F, isKO=F, isChooser=T, H=NA, Kc, Kp, choose_t1)
  return(df)
}
# knock-out option
getBinomTree.ko <- function(S0, K, vol, dT, r, qdiv, N_steps, isPut, H)
{
  df <- getBinomTree(S0, K, vol, dT, r, qdiv, N_steps, isPut, isAmerican=F, 
                     isAvgStrike=F, isKO=T, isChooser=F, H, Kc=NA, Kp=NA, choose_t1=NA)
  return(df)
}
# average strike option
getBinomTree.avgK <- function(S0, vol, dT, r, qdiv, N_steps, isPut)
{
  if (N_steps > 10)
  {
    N_steps <- 10
    print("(!) N_steps set equal to 10. N_steps enters as factorial in number of possible paths!")
  }
  df <- getBinomTree(S0, K=NA, vol, dT, r, qdiv, N_steps, isPut, isAmerican=F, 
                     isAvgStrike=T, isKO=F, isChooser=F, H=NA, Kc=NA, Kp=NA, choose_t1=NA)
  return(df)
}

# get the average values of all possible paths of the underlying until the terminal node
# used for average strike options
getAvgStrikes <- function(S0, N_u, N_d, d, u)
{
  # (!) N of permutations of multisets = (N_u+N_d)!/(N_d!*N_u!)
  if ((N_u + N_d) > 10)
  {
    print("(!) Give a lower number of N steps for the tree. N enters as factorial!")
    return(NA)
  }
  mx_comb <- combn(1:(N_u + N_d), N_u, simplify = FALSE)
  mx_perm <- data.frame(matrix(unlist(mx_comb), nrow = length(mx_comb), byrow = T))
  mx_ud <- data.frame(matrix(d, nrow = nrow(mx_perm), ncol = (N_u + N_d)))
  mx_Savg <- rep(NA, nrow(mx_ud))
  for (i in 1:nrow(mx_ud))
  {
    mx_ud[i, unlist(mx_perm[i, ])] <- u
    St <- S0
    sumS <- 0
    for (j in 1:ncol(mx_ud))
    {
      St <- mx_ud[i, j] * St
      sumS <- sumS + St
    }
    mx_Savg[i] <- sumS/(ncol(mx_ud))
  }
  return(mx_Savg)
}