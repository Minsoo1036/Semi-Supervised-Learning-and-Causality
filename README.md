# Semi-Supervised-Learning-and-Causality
===============================================

The performance of semi-supervised learning compared to just supervised learning can be predicted by causality.
For instance, when the outcome is the result of the predictors in the sense of causality, the information obtained by more data that have predictors without outcome doesn't help the supervised model's prediction [1].

Experiments are implemented on four different UCI datasets (mushroom data / iris data / splice data / balance scale data) and use SVM for supervised learning, TSVM for semi-supervised learning

[1] Jonas Peters, Dominik Janzing, and Bernhard Sholkopf. *Elements of causal inference: foundations and learning algorithms.* 2017.

==========================================================

**Experiment Results (Accuracy)**

|Dataset|SVM|TSVM|Causal/Anticausal|
|-----|----|----|------|
|mushroom|0.98414|0.9829|Anticausal|
|iris|0.7750|0.8625|Anticausal|
|splice|0.6733|0.6733|Causal|
|balance scale|0.8733|0.8733|Causal|



I think this is strange that the performances of SVM and TSVM are exactly same in the case of "Causal". I will investigate it later.
