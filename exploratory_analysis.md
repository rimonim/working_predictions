# How do Humans Structure Conditionally-Dependent Predictions?
Human-made machines tend to be carefully designed and callibrated for particular situations. But humans themselves are impressibly versatile. One component of this versatility is our ability to "predict" situations that may arise and prepare for them appropriately.

I write "predict" in scare quotes because prediction is a cognitive concept, and an ill-defined one at that. As we shall see, much of conditionally-dependent behavior can be explained by simple association. 

In our two pilot experiments, participants pressed one key if `X` appeared and another if `Y`. Two cues, `A` and `B`, had no direct relevance to the task, but could be used to predict whether `X` or `Y` was to follow.
Experiment 1.1 looks like this, where at each level (left to right) the colored veins represent conditional probabilities of the next stimulus occuring:
![Exp 1.1](figures/exp_1.1_vasculature.png)

Experiment 1.2 looks like this:
![Exp 1.2](figures/exp_1.2_vasculature.png)

### Competing Theories
#### The Dataset
The following models will be trained three times each: once on the data from experiment 1.1, once on the data from experiment 1.2, and once on a unified dataset. All datasets are aggregated by participant means within each condition.

```r
> d_agg
# A tibble: 152 × 10
# Groups:   ID, condition, cue, probe, p_cue, p_global, p_conditional [152]
       ID condition cue   probe p_cue p_global p_conditional best_guess    RT correct
    <int> <chr>     <chr> <chr> <dbl>    <dbl>         <dbl>      <dbl> <dbl>   <dbl>
 1 166623 AX        A     X       0.8     0.68           0.8          1  358.       1
 2 166623 AY        A     Y       0.8     0.32           0.2          0  401.       1
 3 166623 BX        B     X       0.2     0.68           0.2          0  409.       1
 4 166623 BY        B     Y       0.2     0.32           0.8          1  333.       1
 5 166626 AX        A     X       0.8     0.68           0.8          1  493.       1
 6 166626 AY        A     Y       0.8     0.32           0.2          0  536.       1
 7 166626 BX        B     X       0.2     0.68           0.2          0  529.       1
 8 166626 BY        B     Y       0.2     0.32           0.8          1  516.       1
 9 166640 AX        A     X       0.8     0.68           0.8          1  460.       1
10 166640 AY        A     Y       0.8     0.32           0.2          0  552        1
# … with 142 more rows
```

#### Model 0: Conditional Probability Only
If participants fully understood the structure of the experiment, and behaved optimally, the resulting predictions would reflect cue-conditional probabilities. Thus if `A` appeared in experiment 1.1, the optimal prediction engine would evaluate the probability of `X` at .8 and of `Y` at .2. Likewise is `B` appeared, the engine would evaluate P(`X`|`B`) at .2 and P(`Y`|`B`) at .8. Thus reaction times would be equally short for the sequences `A`→ `X` and `B`→ `Y`, since P(`X`|`A`) = P(`Y`|`B`). Reaction times for `A`→ `Y` and `B`→ `X` would be longer, but likewise equal.

```r
aggmod_0 <- lm(RT ~ p_conditional, d_agg)
```

#### Model 1: Summed Proportional Stimulus-Response Associations
Perhaps prediction is not prediction at all but rather simply the sum of stimulus-response associations. Both experiments involve two sets of stimuli: prompts (`X` and `Y`) and cues (`A` and `B`). These four stimuli are linked to two responses, corresponding to the two prompts. 
Associations between prompts and their corresponding responses are likely to be very strong, as participants were thoroughly drilled in the paradigm prior to the test phase. Nevertheless, these connections may be variably strenghtened in proportion to the number of times participants were exposed to each stimulus-response pair. This proportion of course is equal to the global probability of each prompt appearing within the paradigm, independent of cues (`p_global`). 
Associations between cues and responses would likewise vary by frequency of exposure. Since different cues appear with different frequencies, and since the frequencies of each response are conditional on the cue, connections between cues and responses should be proportional to the product of cue probability (`p_cue`) and conditional probability (`p_conditional`). I'll call this value `p_posterior`.

```r
d_agg <- d_agg %>% mutate(p_posterior = p_cue*p_conditional)
```
Model 1 can therefore be formulated as follows:

```r
aggmod_1 <- lm(RT ~ p_global + p_posterior, d_agg)
```

#### Model 2: LTM = Base Rate, WM = Conditional Best Guess

#### Model 3: LTM = Summed Proportional Associations, WM = Conditional Best Guess

#### Model 4: LTM = Summed Proportional Associations, WM = Conditional Load Threshold

Ness and Meltzer-Asscher (2021) 
"At every stage during sentence processing, multiple representations in long-term memory are pre-activated. Many different factors contribute to the activation level of a word: the context, lexical properties of the word (e.g., frequency), idiosyncratic influences, and random noise. Once the activation level of a certain word reaches a retrieval threshold, this word is regarded as retrieved, which initiates its integration into the sentence's representation being built in WM." 

### 
