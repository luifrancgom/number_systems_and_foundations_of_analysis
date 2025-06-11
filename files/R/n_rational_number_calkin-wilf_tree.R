# Find the binary representation ----
## Logic ----
binary_num <- integer(0)

while (decimal_num > 0) {
 remainder <- decimal_num %% 2
 binary_num <- c(remainder, binary_num) # Add at the beginning
 decimal_num <- decimal_num %/% 2
}

binary_num

## Function ----
decimal_to_binary <- function(decimal_num) {
 
 binary_num <- integer(0)
 
 while (decimal_num > 0) {
  remainder <- decimal_num %% 2
  binary_num <- c(remainder, binary_num) # Add at the beginning
  decimal_num <- decimal_num %/% 2
 }
 
 return(binary_num)
 
}

## Check ----
decimal_to_binary(decimal_num = 11)

# n rational number ----
## Logic ----
decimal_num <- decimal_to_binary(decimal_num = 11)
frac <- c(0,1)

for (i in decimal_num) {
  
 if (i == 0) {
  frac[2] <- frac[1] + frac[2] 
 } else {
  frac[1] <- frac[1] + frac[2] 
 }
 
}

frac

## Function ----
n_rational <- function(position) {
 
 # Function to find the binary representation
 decimal_to_binary <- function(decimal_num) {
  
  binary_num <- integer(0)
  
  while (decimal_num > 0) {
   remainder <- decimal_num %% 2
   binary_num <- c(remainder, binary_num) # Add at the beginning
   decimal_num <- decimal_num %/% 2
  }
  
  return(binary_num)
  
 }
 
 # Binary representation of the position
 position_to_binary <- decimal_to_binary(decimal_num = position)
 # Root
 frac <- c(0,1)
 
 for (i in position_to_binary) {
  
  if (i == 0) {
   frac[2] <- frac[1] + frac[2] 
  } else {
   frac[1] <- frac[1] + frac[2] 
  }
  
 }
 
 return(frac)
 
}

## Check ----
for (i in 1:15) {
  print(n_rational(position = i)) 
}


