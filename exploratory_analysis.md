All code for this write-up can be found in model_comparisons.R

# How do Humans Structure Conditionally-Dependent Predictions?
Human-made machines tend to be carefully designed and callibrated for particular situations. But humans themselves are impressibly versatile. One component of this versatility is our ability to "predict" situations that may arise and prepare for them appropriately.

I write "predict" in scare quotes because prediction is a cognitive concept, and an ill-defined one at that. As we shall see, much of conditionally-dependent behavior can be explained by simple association. 

In our two pilot experiments, participants pressed one key if `X` appeared and another if `Y`. Two cues, `A` and `B`, had no direct relevance to the task, but could be used to predict whether `X` or `Y` was to follow.
Experiment 1.1 looks like this, where at each level (left to right) the colored veins represent conditional probabilities of the next stimulus occuring:
![Exp 1.1](figures/exp_1.1_vasculature.png)

Let's get an initial look at the data. I'm interested to see how consistent patterns are across participants.
![Exp 1.1](figures/Exp1.1.png)

Some initial subjective observations:
- There are clear individual differences in intercept (i.e. participants with fast RTs in one condition tend to have fast RTs across the board). This is not surprising.
- Almost all participants were faster for `A`→ `X` than for `A`→ `Y`. Most were also faster for `B`→ `Y` than for `B`→ `X`. This shows a clear effect of cue-conditional probability. Nevertheless, the fact that the `B` conditions showed less consistency in this pattern suggests that global probability of probes (or less practice with the `B` cue) may play a role. 

Experiment 1.2 looks like this:
![Exp 1.2](figures/exp_1.2_vasculature.png)

![Exp 1.2](figures/Exp1.2.png)

Some initial subjective observations:
- I see no clear pattern of differences between `A`→ `X` and `A`→ `Y` (even though the initial analysis did show that `A`→ `Y` was significantly longer). Some participants were faster on one, some on the other, and some barely different. I likewise see no clear pattern of differences between `B`→ `X` and`B`→ `Y`. 
- It does, however, look to me like participants who's times go up from `A`→ `X` to `A`→ `Y` also go up from `B`→ `X` to `B`→ `Y`, and vice versa. The magnitude of those differences also looks to vary by participant. Also, it looks a bit like participants who are faster on `Y`s relative to `X`s in the first four conditions also tend to be slower on `C` → `X`. In other words, it looks like there are individual differences in the extent to which people rely on cues or prompts for their predictions. Are my eyes decieving me? Let's look at some quick and dirty correlations.
```r
d2_agg %>%
  pivot_wider(id_cols = ID, names_from = condition, names_prefix = "RT_", values_from = RT) %>%
  mutate(AXtoAY_diff = RT_AX - RT_AY,
         BXtoBY_diff = RT_BX - RT_BY,
         ABXtoCX_diff = mean(c(RT_AX, RT_BX)) - RT_CX) %>%
  ungroup() %>%
  select(AXtoAY_diff:ABXtoCX_diff) %>%
  cor() %>%
  ggcorrplot(lab = T, type = "upper", title = "Experiment 1.2 Within-Participant Correlations")
```
![Exp 1.2 Correlation Matrix](figures/exp1.2corrplot.png)

Sure enough! This looks like initial evidence for individual differences in the extent to which participants are reliant on global (i.e. non-cue dependent) predictions. 

Is this pattern also in the experiment 1.1 results? Maybe. There, `AXtoAY_diff` and `BXtoBY_diff` are correlated at 0.205 - small, but still positive.

Before I continue, I'd like to take a closer look at the correlation between `AXtoAY_diff` and `BXtoBY_diff` in experiment 1.2. 

![Scatterplot of Differences Between X and Y RTs](figures/xydifference_scatterplot.png)

The "X is always quicker" group must be relying primarily on task-global probabilities of the prompts, which strongly favor X (X appears 80% of the time across conditions). The "X is quicker after A only" group is presumably more able to account for conditional probabilities, which favor X after A (67%) but not after B (33%). Unsurprisingly, this is the biggest group. 
The last two groups, for which Y is quicker after the A cue (or always), are the most strange to me. Maybe these are people with low sensitivity to any predictive cues who ended up there by random chance?
Here's the plot of Exp 1.2 results again, with those five participants highlighted.

![Exp 1.2](figures/Exp1.2_2.png)

The green one is what I expected here: slow all around, with little effect of condition at all. But the others are the fastest of the bunch! Not only that - they're especially fast on CX! This is very strange.

In the absence of a clear direction to go from here, I'll start trying some models.

### Model Comparisons
#### The Dataset
Each participant in the experiments underwent between three and four hundred trials. Since the models I'll be using here account for individual differences, I can train them with a split dataset: Trials are randomly distributed into one of two groups, and then averaged within participants/conditions. This effectively doubles the sample size.

Also, raw probabiilties of outcomes (e.g. conditional probability of `A`→ `X` in Exp. 1.1 = .8) have been transformed into log-odds. This means we can't use data from the `C`→ `X` condition (logit function goes to infinity as p approaches 1), but it should allow us to assume homoskedasticity.

```r
> d_agg2 %>% select(ID, experiment, condition, RT, odds_global, odds_conditional, odds_conjunction)
# A tibble: 272 × 7
       ID experiment   condition odds_global odds_conditional odds_conjunction    RT
    <int> <chr>        <chr>           <dbl>            <dbl>            <dbl> <dbl>
 1 166623 Experiment 1 AX              0.754             1.39            0.575  357.
 2 166623 Experiment 1 AY             -0.754            -1.39           -1.66   400.
 3 166623 Experiment 1 BX              0.754            -1.39           -3.18   393 
 4 166623 Experiment 1 BY             -0.754             1.39           -1.66   324 
 5 166623 Experiment 1 AX              0.754             1.39            0.575  358.
 6 166623 Experiment 1 AY             -0.754            -1.39           -1.66   402.
 7 166623 Experiment 1 BX              0.754            -1.39           -3.18   414.
 8 166623 Experiment 1 BY             -0.754             1.39           -1.66   344.
 9 166626 Experiment 1 AX              0.754             1.39            0.575  504.
10 166626 Experiment 1 AY             -0.754            -1.39           -1.66   538.
# … with 262 more rows
```

In this round, I'll be using Bayesian methodology since brms allows me more flexibility in defining systems of equations. This will be helpful for Model 6 and beyond.

#### Model 0: Conditional Probability Only
If participants fully understood the structure of the experiment, and behaved optimally, the resulting predictions would reflect cue-conditional probabilities. Thus if `A` appeared in experiment 1.1, the optimal prediction engine would evaluate the probability of `X` at .8 and of `Y` at .2. Likewise is `B` appeared, the engine would evaluate P(`X`|`B`) at .2 and P(`Y`|`B`) at .8. Thus reaction times would be equally short for the sequences `A`→ `X` and `B`→ `Y`, since P(`X`|`A`) = P(`Y`|`B`). Reaction times for `A`→ `Y` and `B`→ `X` would be longer, but likewise equal.

```r
# Using nonlinear syntax for consistency with later models.
aggmod_0_bayes <-
  brm(data = d_agg2, 
      family = gaussian,
      bf(RT ~ i + m,
         i ~ (1 | cor | ID),
         m ~ 0 + odds_conditional + (0 + odds_conditional | cor | ID),
         nl = TRUE),
      prior = c(prior(normal(450, 100), coef = "Intercept", nlpar = i),
                prior(normal(-12, 20), coef = "odds_conditional", nlpar = m)),
      iter = 5000, warmup = 2000, chains = 4, cores = 4)
```

##### Results
```r
> summary(aggmod_0_bayes)
 Family: gaussian 
  Links: mu = identity; sigma = identity 
Formula: RT ~ i + m 
         i ~ (1 | cor | ID)
         m ~ 0 + odds_conditional + (0 + odds_conditional | cor | ID)
   Data: d_agg2 (Number of observations: 272) 
  Draws: 4 chains, each with iter = 5000; warmup = 2000; thin = 1;
         total post-warmup draws = 12000

Group-Level Effects: 
~ID (Number of levels: 34) 
                                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sd(i_Intercept)                        59.13      7.68    45.94    75.81 1.00     1844     3555
sd(m_odds_conditional)                  6.34      2.98     0.69    12.35 1.00     2679     3074
cor(i_Intercept,m_odds_conditional)     0.34      0.33    -0.39     0.91 1.00     8108     5475

Population-Level Effects: 
                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
i_Intercept          453.90     10.48   432.32   474.11 1.01     1100     1977
m_odds_conditional   -11.74      2.23   -16.06    -7.32 1.00     6600     7573

Family Specific Parameters: 
      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma    32.62      1.58    29.66    35.89 1.00     6357     7607
```
Looking alright. Let's move on though. It was obvious from the first look at the data this is not the right model (e.g. `A`→ `X` and `B`→ `Y` have the same conditional probabilities but differ significantly in RT).

#### Model 1: Summed Parallel Predictions
Maybe conditional probability is not the whole story. Context-clues in the real world often come at many levels of temporal and conceptual abstraction. Perhaps participants are generating one assessment of outcome probability based on the task as a whole (`p_global`) and another on the cues alone (`p_conditional`).

```r
aggmod_1_bayes <-
  brm(data = d_agg2, 
      family = gaussian,
      bf(RT ~ i + m,
         i ~ (1 | cor | ID),
         m ~ 0 + odds_global + odds_conditional + (0 + odds_global + odds_conditional | cor | ID),
         nl = TRUE),
      prior = c(prior(normal(450, 100), coef = "Intercept", nlpar = i),
                prior(normal(-3, 20), coef = "odds_global", nlpar = m),
                prior(normal(-12, 20), coef = "odds_conditional", nlpar = m)),
      iter = 5000, warmup = 2000, chains = 4, cores = 4)
 ```

##### Results
```r
 Family: gaussian 
  Links: mu = identity; sigma = identity 
Formula: RT ~ i + m 
         i ~ (1 | ID)
         m ~ 0 + odds_global + odds_conditional + (0 + odds_global + odds_conditional | ID)
   Data: d_agg2 (Number of observations: 272) 
  Draws: 4 chains, each with iter = 5000; warmup = 2000; thin = 1;
         total post-warmup draws = 12000

Group-Level Effects: 
~ID (Number of levels: 34) 
                                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sd(i_Intercept)                          59.13      7.59    46.40    75.85 1.00     1884     3756
sd(m_odds_global)                        18.87      3.21    13.46    25.90 1.00     3545     6627
sd(m_odds_conditional)                    8.91      2.25     4.85    13.65 1.00     4096     6112
cor(m_odds_global,m_odds_conditional)    -0.52      0.24    -0.93     0.01 1.00     4222     4851

Population-Level Effects: 
                   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
i_Intercept          453.74     10.34   433.30   474.56 1.00     1153     2373
m_odds_global         -4.84      3.48   -11.84     1.90 1.00     3371     6344
m_odds_conditional   -12.32      2.14   -16.56    -8.00 1.00     7431     8408

Family Specific Parameters: 
      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma    25.34      1.41    22.76    28.25 1.00     6829     8664
```

![aggmod_1](figures/aggmod_1_bayes_fit.png)

This model seems to fit the data fairly well, actually. In fact, it even seems to have picked up on the strange individual differences I discussed above (found especially in Exp. 1.2), with zigzags going both ways. This may be connected to the fact the model is estimating a *positive* effect of global odds for lots of the participants. This is strange and seems probably wrong - higher global frequencies cause some people to take *longer*!? I'll come back to this.

Another interesting finding: the parameters for `odds_global` and `odds_conditional` are negatively correlated between participants. In other words, people who rely more on global odds show less sensitivity to conditional odds.

#### Model 2: Summed Proportional Stimulus-Response Associations
Perhaps prediction is not prediction at all but rather simply the sum of stimulus-response associations. This view produces a model similar to Model 1 but subtly different.
Both experiments involve two sets of stimuli: prompts (`X` and `Y`) and cues (`A` and `B`). These four stimuli are linked to two responses, corresponding to the two prompts. 
Associations between prompts and their corresponding responses are likely to be very strong, as participants were thoroughly drilled in the paradigm prior to the test phase. Nevertheless, these connections may be variably strenghtened in proportion to the number of times participants were exposed to each stimulus-response pair. This proportion of course is equal to the global probability of each prompt appearing within the paradigm, independent of cues (`p_global`). 
Associations between cues and responses would likewise vary by frequency of exposure. Since different cues appear with different frequencies, and since the frequencies of each response are conditional on the cue, connections between cues and responses should be proportional to the product of cue probability (`p_cue`) and conditional probability (`p_conditional`). I'll call this value `p_conjunction`.

```r
d_agg2 <- d_agg2 %>% mutate(p_conjunction = p_cue*p_conditional)
# odds_conjunction will then be log-odds of p_conjunction
```
Model 2 can therefore be formulated as follows:

```r
aggmod_2_bayes <-
  brm(data = d_agg2, 
      family = gaussian,
      bf(RT ~ i + mf,
         i ~ (1 | cor | ID),
         mf ~ 0 + odds_global + odds_conjunction + (0 + odds_global + odds_conjunction | cor | ID),
         nl = TRUE),
      prior = c(prior(normal(450, 100), coef = "Intercept", nlpar = i),
                prior(normal(-3, 20), coef = "odds_global", nlpar = mf),
                prior(normal(-12, 20), coef = "odds_conjunction", nlpar = mf)),
      iter = 5000, warmup = 2000, chains = 4, cores = 4)
```

##### Results
```r
 Family: gaussian 
  Links: mu = identity; sigma = identity 
Formula: RT ~ i + mf 
         i ~ (1 | ID)
         mf ~ 0 + odds_global + odds_conjunction + (0 + odds_global + odds_conjunction | ID)
   Data: d_agg2 (Number of observations: 272) 
  Draws: 4 chains, each with iter = 5000; warmup = 2000; thin = 1;
         total post-warmup draws = 12000

Group-Level Effects: 
~ID (Number of levels: 34) 
                                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sd(i_Intercept)                            58.26      7.81    44.99    75.82 1.00     2434     4772
sd(mf_odds_global)                         16.76      3.29    11.00    23.83 1.00     4340     7073
sd(mf_odds_conjunction)                     2.54      1.87     0.10     6.91 1.00     3014     4163
cor(mf_odds_global,mf_odds_conjunction)    -0.29      0.52    -0.97     0.85 1.00     8026     8284

Population-Level Effects: 
                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
i_Intercept           439.56     10.72   418.43   460.78 1.00     1636     3054
mf_odds_global         -4.09      3.48   -11.06     2.62 1.00     4966     7298
mf_odds_conjunction    -7.43      1.96   -11.38    -3.56 1.00    14127     8431

Family Specific Parameters: 
      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma    30.88      1.60    27.94    34.20 1.00    10029     8816
```

![aggmod_2](figures/aggmod_2_bayes_fit.png)

Hard to tell the difference from the visual alone. 

#### Model 6: LTM = Base Rate, WM = Conditional Best Guess
Maybe difference levels of conditionally-dependent prediction are driven by different memory systems? If conditional probability were handled by working memory, its representation might be binary: either the prediction is being held in WM or not. If this is the case, perhaps, we just pick the most likely outcome and hold that in mind at each step. 

If long term memory holds only global probabilities, the model would then look like this:

```r
aggmod_3 <- lmer(1 + p_global + best_guess + (1 + p_global + best_guess | ID), d_agg)
```

##### Results
```r
Formula: RT ~ 1 + p_global + best_guess + (1 + p_global + best_guess | ID)
   Data: d_agg

Fixed effects:
            Estimate Std. Error t value
(Intercept)  491.018     13.076  37.550
p_global     -50.450     14.659  -3.442
best_guess   -39.016      7.512  -5.194

Correlation of Fixed Effects:
           (Intr) p_glbl
p_global   -0.629       
best_guess -0.058 -0.127

Formula: RT ~ 1 + p_global + best_guess + (1 + p_global + best_guess | ID)
   Data: d1_agg

Fixed effects:
            Estimate Std. Error t value
(Intercept)  477.965     20.245  23.610
p_global     -46.545     23.741  -1.961
best_guess   -33.639      7.276  -4.623

Correlation of Fixed Effects:
           (Intr) p_glbl
p_global   -0.793       
best_guess  0.356 -0.596

Formula: RT ~ 1 + p_global + best_guess + (1 + p_global + best_guess | ID)
   Data: d2_agg

Fixed effects:
            Estimate Std. Error t value
(Intercept)   504.08      18.73  26.908
p_global      -51.61      20.82  -2.479
best_guess    -44.34      12.90  -3.437

Correlation of Fixed Effects:
           (Intr) p_glbl
p_global   -0.535       
best_guess -0.121 -0.153
```
Finally, the coefficients look similar in all three model fits! Even visually, I don't see any egregious deviations from the model fit. 

![aggmod_3](figures/aggmod_3.png)

To check whether `best_guess` is explaining any variance that `p_conditional` wasn't (`best_guess` is just a binned `p_conditional`), let's run that model again with both predictors.

```r
Formula: RT ~ 1 + p_global + p_conditional + best_guess + (1 + p_global +  
    p_conditional + best_guess | ID)
   Data: d_agg

Random effects:
 Groups   Name          Std.Dev. Corr             
 ID       (Intercept)    67.74                    
          p_global       69.30   -0.21            
          p_conditional 172.58   -0.22 -0.68      
          best_guess     56.83    0.08  0.73 -0.99
 Residual                22.64                    
Number of obs: 152, groups:  ID, 34

Fixed Effects:
  (Intercept)       p_global  p_conditional     best_guess  
       539.30         -26.81        -211.29          65.11  
```

Indeed, it looks like `best_guess` isn't doing anything except making the mixed-effects model confused about the independent effect of `p_conditional`.

#### Model 4: LTM = Summed Proportional Associations, WM = Conditional Load Threshold

So far we've seen that conditional probability is a really strong predictor of the Experiment 1 results, while Experiment 2 results are much more closely tied to `p_posterior` (i.e. cue probability times cue-conditional probability). What could account for this difference between the two experiments?

Ness and Meltzer-Asscher (2021) propose the following model for linguistic processing: "At every stage during sentence processing, multiple representations in long-term memory are pre-activated. Many different factors contribute to the activation level of a word: the context, lexical properties of the word (e.g., frequency), idiosyncratic influences, and random noise. Once the activation level of a certain word reaches a retrieval threshold, this word is regarded as retrieved, which initiates its integration into the sentence's representation being built in WM." 

In short, Ness and Meltzer-Asscher posit a LTM mechanism similar to the one formulated here in Model 2. In addition, though, they propose that when one prediction reaches a certain level of LTM activation (the threshold), it is updated into WM and recieves a boost. 

A slight modification of this model might account for our data nicely: Perhaps the threshold for WM pre-updating is based on a more top-down predictive process (one that closely approximates `p_conditional`), such that the model could be formulated as follows (with the thresholded boost modeled as a steep logistic function):

```r
aggmod_4 <- nlme::nlme(RT ~ b0 + b1*p_global + b2*p_posterior + b3*plogis(500*(p_conditional + brk)),
                    data = d_agg,
                    fixed = b0 + b1 + b2 + b3 + brk ~ 1,
                    random = b0 + brk ~ 1,
                    groups = ~ ID,
                    start = coef(naive_mod_4))
```

For the sake of simplicity, I'm starting by letting only the Intercept and the threshold vary by participant. Also, since the model is so complex, I'm modeling it only on the full dataset.

Placing the threshold in conditional probability is not as crazy as it may sound - late ERP positivity effects such as the P600 provide some evidence that the brain can incur additional neural consequences when it encounters words that violate highly constraining contexts, over and above those reflected by the N400 ([Kuperberg & Jaeger, 2016](https://www.tandfonline.com/doi/pdf/10.1080/23273798.2015.1102299?needAccess=true)).[Kuperberg (2007)](https://reader.elsevier.com/reader/sd/pii/S0006899306036821?token=DBAD6CA0B990F7C4A3F867A445220DF83A72944AE05923A161B970FEAF6E897559A6C8A2B35EF0427BE056A371F3850B&originRegion=eu-west-1&originCreation=20221123114908) goes so far as to propose two distinct streams in language processing: one purely lexical/associational and one that incorporates higher-level (e.g. syntactic) knowledge.

##### Results
```r
Nonlinear mixed-effects model fit by maximum likelihood
  Model: RT ~ b0 + b1 * p_global + b2 * p_posterior + b3 * plogis(500 * (p_conditional + brk)) 
  Data: d_agg 

Random effects:
 Formula: list(b0 ~ 1, brk ~ 1)
 Level: ID
 Structure: General positive-definite, Log-Cholesky parametrization
         StdDev       Corr  
b0       5.533979e+01 b0    
brk      5.250594e-04 -0.921
Residual 3.431272e+01       

Fixed effects:  b0 + b1 + b2 + b3 + brk ~ 1 
        Value Std.Error  DF    t-value p-value
b0   470.1223 12.028982 114    39.0825  0.0000
b1   -16.7075 12.743208 114    -1.3111  0.1925
b2    11.2485 23.927261 114     0.4701  0.6392
b3  -111.4537 15.049238 114    -7.4059  0.0000
brk   -0.8013  0.000768 114 -1043.4984  0.0000
 Correlation: 
    b0     b1     b2     b3    
b1  -0.475                     
b2  -0.056 -0.272              
b3   0.114 -0.038 -0.724       
brk  0.084 -0.222  0.342  0.010
```

The model doesn't seem to want the threshold to vary at all. Also, p_posterior has essentially no effect. Let's get a better sense of what this function looks like.

![aggmod_4](figures/aggmod_4_p_conditional.png)

Ahh. It wants to put the Experiment 1.1 `A`→ `X` and `B`→ `Y` conditions right in the middle of the sigmoid curve. That feels like cheating to me.
The above figure doesn't account for the real-life relationships between global, posterior, and conditional probabilities. Let's take a look at fitted values for the true data. 

![aggmod_4](figures/aggmod_4_fitted_p_conditional.png)
![aggmod_4](figures/aggmod_4_fitted_p_global.png)

This looks really good, actually.

Before formal model comparisons, I'm going to revise the model a bit. First, I'll take out `p_posterior`, which seems to be hurting. I'll also change the threshold to a fixed effect. With my extra parameters, I'll make random effects for `p_global` and the magnitude of the WM boost.

```r
aggmod_5 <- nlme::nlme(RT ~ b0 + b1*p_global + b2*plogis(100*(p_conditional + brk)),
                       data = d_agg,
                       fixed = b0 + b1 + b2 + brk ~ 1,
                       random = b0 + b1 + b2 ~ 1,
                       groups = ~ ID,
                       start = coef(naive_mod_5))

aggmod_5

#> Nonlinear mixed-effects model fit by maximum likelihood
#>   Model: RT ~ b0 + b1 * p_global + b2 * plogis(100 * (p_conditional + brk)) 
#>   Data: d_agg 
#>   Log-likelihood: -781.3611
#>   Fixed: b0 + b1 + b2 + brk ~ 1 
#>          b0          b1          b2         brk 
#> 474.9976198 -23.2791269 -95.7532512  -0.8022666 
#> 
#> Random effects:
#>  Formula: list(b0 ~ 1, b1 ~ 1, b2 ~ 1)
#>  Level: ID
#>  Structure: General positive-definite, Log-Cholesky parametrization
#>          StdDev   Corr         
#> b0       69.07262 b0     b1    
#> b1       67.61850 -0.697       
#> b2       56.79809  0.698 -0.693
#> Residual 23.71952              
#> 
#> Number of Observations: 152
#> Number of Groups: 34 
```

Sure enough, the model has the effect of `p_global` varying quite a lot between participants. This is important, because it explains the correlation I observed early in this analysis - that participants who's times go up more from `A`→`X` to `A`→`Y` also go up more from `B`→`X` to `B`→`Y`. I'm less sure what, if anything, to do with individual differences in the size of the WM boost.

#### Model 6: LTM = Summed Proportional Associations, WM = Resource-Optimal Model-Based Prediction






