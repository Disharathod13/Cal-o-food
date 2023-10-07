1. What is the goal of the package?

Calofood's primary goal is to provide users with a comprehensive toolkit to access and assess nutritional data from popular fast-food establishments. This is accomplished through a list of functions.

2. What does your function(s) do?

- Interactive Shiny App: Calofood seamlessly integrates an interactive Shiny application `run_app()`, allowing users to conveniently explore and evaluate nutritional information, including calorie counts and specifics of a wide variety of fast food items from different restaurants.
- Plotting Capability: The package introduces `foodplot()`, a function that enables the creation of customized bar plots based on a vitamin rich diet. This assists users in identifying fast-food options that are rich in vitamins among the various choices available.
- Compound Interest Calculation: Calofood incorporates a numeric function, `comp_int()`, which enables users to perform compound interest (annualy) calculations. 

3. How do we use it?

library(Calofood) #loading the package
comp_int() #calling the numeric function
run_app() #launching the shiny app
foodplot() #plotting the recommendation for a vitamin rich diet

4. Why should we use it? (with examples)

```r
comp_int(725000, 3, 2)
```

```r
run_app()
```

```r
foodplot("Mcdonalds")
```

5. Where do we find and install it?

The user can install the package by refering to this link [Calofood](https://github.com/ETC5523-2023/rpkg-Disharathod13)

In R

```r
install.packages("Calofood")
```
