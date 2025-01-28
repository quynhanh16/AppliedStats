R Notebook
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*.

# One sample t test

``` r
temp=c(3.5, 4.8, 2.5, 6.3, 4.7, 2.7, 2.6, 5.5, 3.2,5.7)
t.test(temp, mu = 3, alternative = "greater")
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  temp
    ## t = 2.5626, df = 9, p-value = 0.01528
    ## alternative hypothesis: true mean is greater than 3
    ## 95 percent confidence interval:
    ##  3.327365      Inf
    ## sample estimates:
    ## mean of x 
    ##      4.15

- Step 1: $H_0: \mu=3$ vs $H_a: \mu > 3$

- Step 2: $t=2.5626$ and $DF=9$

- Step 3: p-value= 0.01528

- Step 4: $p=0.01528<\alpha=0.05$

**Conclusion:** We found significant statistical evidence at the level
of $\alpha= 0.05$ that the mean temperature increase exceeds 3 degree
Celsius.

# Match paired t test

``` r
New_Prog=c(21, 11, 16,5,20, 7, 16) 
Reg_Prog=c(15, 8, 19, 4, 16, 9, 11) 

t.test(New_Prog, Reg_Prog, paired=T,conf.level = 0.9, alternative ="greater")
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  New_Prog and Reg_Prog
    ## t = 1.5275, df = 6, p-value = 0.08874
    ## alternative hypothesis: true mean difference is greater than 0
    ## 90 percent confidence interval:
    ##  0.1149172       Inf
    ## sample estimates:
    ## mean difference 
    ##               2

- Step 1: $H_0: d=0$ vs $H_a: d > 0$

- Step 2: $t=1.5275$ and $DF=6$

- Step 3: p-value= 0.08875

- Step 4: $p=0.08875<\alpha=0.10$

**Conclusion:** We found significant statistical evidence at the level
of $\alpha =0.10$ that the mean score of the new program is higher than
that of the regular program.

# Two sample t test

``` r
LifeExpdata= read.csv("LifeExp.csv") 
t.test(LifeExpdata$Life.expectancy ~ LifeExpdata$Political.Color, var.equal=F, conf.level = 0.99)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  LifeExpdata$Life.expectancy by LifeExpdata$Political.Color
    ## t = 5.4352, df = 48.705, p-value = 1.739e-06
    ## alternative hypothesis: true difference in means between group Blue and group Red is not equal to 0
    ## 99 percent confidence interval:
    ##  1.065500 3.139262
    ## sample estimates:
    ## mean in group Blue  mean in group Red 
    ##           79.41905           77.31667

- Step 1: $H_0: \mu_L=\mu_R$ vs $H_a: \mu_L \neq \mu_R$

- Step 2: $t=5.4352$ and $DF=48.705$

- Step 3: p-value=1.739e-06

- Step 4: $p=1.739 \times 10^{-6}<\alpha=0.01$

**Conclusion:** We found significant statistical evidence at the level
of $\alpha =0.01$ that the life expectancy of Blue states and the red
states is different.

``` r
t.test(LifeExpdata$Life.expectancy ~ LifeExpdata$Political.Color, var.equal=T,conf.level = 0.99)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  LifeExpdata$Life.expectancy by LifeExpdata$Political.Color
    ## t = 5.1695, df = 49, p-value = 4.315e-06
    ## alternative hypothesis: true difference in means between group Blue and group Red is not equal to 0
    ## 99 percent confidence interval:
    ##  1.012469 3.192293
    ## sample estimates:
    ## mean in group Blue  mean in group Red 
    ##           79.41905           77.31667

- Step 1: $H_0: \mu_L=\mu_R$ vs $H_a: \mu_L \neq \mu_R$

- Step 2: $t=5.1695$ and $DF=49$

- Step 3: p-value=4.315e-06

- Step 4: $p=4.315 \times 10^{-6}<\alpha=0.01$

**Conclusion:** We found significant statistical evidence at the level
of $\alpha =0.01$ that the life expectancy of Blue states and the red
states is different.

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or
by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output
will be saved alongside it (click the *Preview* button or press
*Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.
