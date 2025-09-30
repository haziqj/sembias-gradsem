# Bias-Reduced Estimation of Structural Equation Models

Finite-sample bias is a pervasive challenge in the estimation of structural equation models (SEMs), especially when sample sizes are small or measurement reliability is low. A range of methods have been proposed to improve finite-sample bias in the SEM literature, ranging from analytic bias corrections to resampling-based techniques, with each carrying trade-offs in scope, computational burden, and statistical performance. We apply the reduced-bias M-estimation framework (RBM, Kosmidis & Lunardon, 2024, J. R. Stat. Soc. Series B Stat. Methodol.) to SEMs. The RBM framework is attractive as it requires only first- and second-order derivatives of the log-likelihood, which renders it both straightforward to implement, and computationally more efficient compared to resampling-based alternatives such as bootstrap and jackknife. It is also robust to departures from modelling assumptions. Through extensive simulations studies under a range of experimental conditions, we illustrate that RBM estimators consistently reduce mean bias in the estimation of SEMs without inflating mean squared error. They also deliver improvements in both median bias and inference relative to maximum likelihood estimators, while maintaining robustness under non-normality. Our findings suggest that RBM offers a promising, practical, and broadly applicable tool for mitigating bias in the estimation of SEMs, particularly in small-sample research contexts.

Keywords: Structural equation models; growth curve models; small sample estimation; bias
reduction; penalized likelihood

## Citation

> Jamil, H., Rosseel, Y., Kemp, O., & Kosmidis, I. (2025). Bias-Reduced Estimation of Structural Equation Models. *Manuscript in Submission*. [`arXiv`]().

Please cite this work as:

``` latex
@online{jamil2025biasreduced,
  title = {Bias-{{Reduced Estimation}} of {{Structural Equation Models}}},
  author = {Jamil, Haziq and Rosseel, Yves and Kemp, Oliver and Kosmidis, Ioannis},
  date = {2025},
  abstract = {Finite-sample bias is a pervasive challenge in the estimation of structural equation models (SEMs), especially when sample sizes are small or measurement reliability is low. A range of methods have been proposed to improve finite-sample bias in the SEM literature, ranging from analytic bias corrections to resampling-based techniques, with each carrying trade-offs in scope, computational burden, and statistical performance. We apply the reduced-bias M-estimation framework (RBM, Kosmidis \& Lunardon, 2024, J. R. Stat. Soc. Series B Stat. Methodol.) to SEMs. The RBM framework is attractive as it requires only first- and second-order derivatives of the log-likelihood, which renders it both straightforward to implement, and computationally more efficient compared to resampling-based alternatives such as bootstrap and jackknife. It is also robust to departures from modelling assumptions. Through extensive simulations studies under a range of experimental conditions, we illustrate that RBM estimators consistently reduce mean bias in the estimation of SEMs without inflating mean squared error. They also deliver improvements in both median bias and inference relative to maximum likelihood estimators, while maintaining robustness under non-normality. Our findings suggest that RBM offers a promising, practical, and broadly applicable tool for mitigating bias in the estimation of SEMs, particularly in small-sample research contexts.},
  langid = {english},
  pubstate = {prepublished}
}

```
