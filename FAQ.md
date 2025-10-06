# FAQ and common objections

## 1) Bias is asymptotically negligible---it’s $O(1/n)$ while variance is $O(1/\sqrt{n})$

While this is true asymptotically, in practice it is a different story. In small or moderate samples, the $O(1/n)$ term can hugely distort confidence intervals, likelihood ratio tests, and convergence. Bias matters most in nonlinear, boundary-constrained, or model where asymptotic approximations are poor. Note that bias correction improves small-sample reliability and inferential calibration, not just point estimates.

## 2) Variance matters more; a little bias is fine.

It's well-known that bias and variance trade off, but not all bias is benign. Penalization *adds bias* to control variance, but bias correction removes structural bias due to likelihood curvature (at least ours does). The goal isn't zero bias---it's well-centred inference and correct coverage.

## 3) Bayesian methods already solve this

Bayesian methods (and penalization) solve different problems. Regularization and priors reshape the likelihood (adding curvature) to reduce variance. Bias correction recentres the likelihood (adjusting the score) to fix its symmetry. You can use both---but bias reduction is prior-free and computationally light, giving frequentists stability without prior assumptions.

## 4) Bias is just an expectation---it’s a useless theoretical property.

Yes, the expectation definition is abstract; it describes an ensemble we never observe. But the corrections derived from it have real finite-sample effects: they stabilize the likelihood, improve convergence, and enhance coverage. Bias correction ≠ “fixing the mean”; it’s rebalancing the likelihood geometry so that the mode and curvature behave more like the asymptotic ideal.

## 5) If the estimator’s distribution is skewed, the mean bias isn’t even meaningful.

Correct. When the sampling distribution is skewed, the mean can be unrepresentative. But that’s exactly why bias-reduction methods matter: they reduce skewness and make the mode, median, and mean align more closely. Bias reduction has the effect of regularizing the shape of the estimator’s distribution.

## 6) Bias correction is theoretical; we care about what happens in real data.

Bias correction is practical. It improves small-sample likelihood geometry and inference stability. Think of it as geometry repair, not abstract expectation manipulation. You can’t observe bias directly, but you can observe better-behaved estimates and more accurate confidence intervals.

## 7) Aren’t you over-obsessed with bias reduction?

The focus isn’t on bias for its own sake; it’s about diagnosing where classical estimators break down in finite samples. Bias correction gives a lens to understand curvature, boundary effects, and information imbalance---issues that also affect penalized and Bayesian estimators. In other words: bias correction is a gateway to understanding likelihood geometry, not an obsession with numerical bias.

## 8) Why not just reparameterize into a scale where the estimator is unbiased?

Unbiasedness is not invariant---and that’s the problem, not the solution. The fact that you can "remove" bias by changing the coordinate system means the concept itself is coordinate-dependent, and it has no intrinsic geometric meaning. A good inferential principle should not depend on an arbitrary choice of parameterization; scientific interpretation usually lives on one scale and not another. 
