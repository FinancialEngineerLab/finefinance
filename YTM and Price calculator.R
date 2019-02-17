
### Ch2 #8 ###

bond_price <- function(C,T,c,y,m=2){
  x <- 1+y/m
  n <- T*m
  ans <-(C*c)/y*(1-1/x^n)+(C/ x^n)
}

#A
print(bond_price(1000,9, 0.08, 0.07, m=2))
#B
print(bond_price(1000,20, 0.09, 0.09, m=2))
#C
print(bond_price(1000, 15, 0.06, 0.1, m=2))
#D
print(bond_price(1000, 14, 0.0, 0.08, m=2))
      

### Ch3. #5 ###

# Create cash flow vector
# Create bond valuation function
bval <- function(i, cf,
                 t=seq(along = cf))
  sum(cf / (1 + i)^t)

# Create ytm() function using uniroot
ytm <- function(cf) {
  uniroot(bval, c(0, 1), cf = cf)$root
}

# Use ytm() function to find yield

ytm(cf)


### A ###
cf <- c(-884.2,35,35,35,35,1035)

#Create bond valuation function
bval <- function(i, cf,
                 t=seq(along = cf))
  sum(cf / (1 + i)^t)

# Create ytm() function using uniroot
ytm <- function(cf) {
  uniroot(bval, c(0, 1), cf = cf)$root
}

# Use ytm() function to find yield
ytm(cf)


### B ###
cf <- c(-948.90,40, 40,40,40,40,40,1040)

#Create bond valuation function
bval <- function(i, cf,
                 t=seq(along = cf))
  sum(cf / (1 + i)^t)

# Create ytm() function using uniroot
ytm <- function(cf) {
  uniroot(bval, c(0, 1), cf = cf)$root
}

# Use ytm() function to find yield
ytm(cf)

### C ###
cf <- c(-967.7,45, 45,45,1045)

#Create bond valuation function
bval <- function(i, cf,
                 t=seq(along = cf))
  sum(cf / (1 + i)^t)

# Create ytm() function using uniroot
ytm <- function(cf) {
  uniroot(bval, c(0, 1), cf = cf)$root
}

# Use ytm() function to find yield
ytm(cf)

### C ###
cf <- c(-456.39,0,0,0,0,0,0,0,0,0, 1000)

#Create bond valuation function
bval <- function(i, cf,
                 t=seq(along = cf))
  sum(cf / (1 + i)^t)

# Create ytm() function using uniroot
ytm <- function(cf) {
  uniroot(bval, c(0, 1), cf = cf)$root
}

# Use ytm() function to find yield
ytm(cf)

