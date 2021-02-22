library(extraDistr)

#Section 1


#q8

#a
#all permutations divided by overcounting for each team and then divided by arrangement of teams
res1 = factorial(12)/(2*factorial(5)*factorial(5)*factorial(2))
#choose first team of 5 then the other team of 5 and then divide by two since overcounted by 2 due to ordering
res2 = choose(12, 5) * choose(7, 5) / 2

#b
#split 12 people into 3 teams of equal size
#choose first team then second and divide by overcounting?
res1 = choose(12, 4) * choose(8, 4) / 6
res2 = factorial(12)/(factorial(4) * factorial(4) * factorial(4) * factorial(3))


#q9

#a
#Well, getting to (110, 111) will require 221 steps. Out of the 221 steps, We can choose which
#110 are steps to the right, and this determines the rest to be up steps.
a = choose(221, 110)
b = choose(221, 111)
#and then We still have to get from (110, 111) to (210, 211), so We need a delta of
#(100, 100). Encode the steps as a sequence, "up" or "right", and the sequence is 200 symbols long
#then We choose which symbols are "up". The order matters.
c = choose(200, 100)
res = a * c


#simulation of dice rolls

repeated_dice_value <- function(roll){
  count = rep(0, 6)
  for (i in roll){
    count[i] = count[i] + 1
  }
  for( i in (1:length(count))){
    if (count[i] > 1){
      return(TRUE)
    }
  }
  return(FALSE)
}

count = 0
n = 1000000
for (i in (1:n)){
  roll = sample((1:6), 6, replace = TRUE)
  contains_repeated = repeated_dice_value(roll = roll)
  if (contains_repeated){
    count = count + 1
  }
}
print(count/n)

#monty hall simulations
#door #1 is always "chosen" initially 

number_of_doors = 7
car = sample((1:number_of_doors), 1, replace = FALSE)
#countst 
no_switch_wins = 0
if (car == 1){
  no_switch_wins = no_switch_success + 1
} else {
  open_doors = sample((2:number_of_doors), 1, replace = FALSE)
  while(car %in% open_doors){
    open_doors = sample((2:number_of_doors), 1, replace = FALSE)
  }
  
}

#rando test
res = (13 * choose(4, 2) * 12 * choose(4, 2) * 44)/(2*choose(52, 5))
res1 = (choose(13, 2) * choose(4, 2)^2 * 44)/(choose(52, 5))




#Section 4

interval <- function(a, b){
  
}
NHGeom <- function(w, b, k){
  res = 1
  j = k-2
  for (i in 0:j){
    print(i)
    res = res * (b-i)/(w+b-i)
  }
  print(res)
  res = res * w/(w+b-k-1)
  return(res)
}
w = 10
b = 8
k = 3
res1 = NHGeom(w, b, k) 
res2 = dnhyper(x = k, n = b, m = w, r = 1)
res3 = (8/18) * (7/17) * (10/16)

k = 5
res1 = (0.05)^k
res2 = choose(50, k)*choose(950, 50-k)/choose(1000, 50)
res3 = choose(50, k)*choose(1000-k, 50-k)/choose(1000, 50)

res1 = 365*((1/365)^choose(50, 2))
res2 = 365*(1-(364/365)^50 - 50*(1/365)*(364/365)^49)

a = 1
for (i in 40:50){
  i = i*10
  print(i)
  a = a*i
}
b = 1
for (i in 490:500){
  b = b*i
}
res1 = a/b
res2 = 1 - res1


n = 100
a = sample(1:1000, n)
v = sum((a-mean(a))^2)/(n-1)
v1 = var(a)
c = 0
for (i in 1:n){
  for (j in 1:n)
    c = c + (a[i] - a[j])^2
}
c = c/(n*n)
c/v

n = 10
k = 5
a = choose(n, k)/(2^(choose(k, 2)))
b = choose(n, k)*(1/2)^(choose(k, 2))

n = 100
a = choose(n, 3)
b = 3*(n-1)
c = 3*choose(n, 2)

n=100
a = choose()

r=4
x=160
a = (factorial(x)/factorial(x-r))/(x^r)
