
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quanteda.proximity

**Please follow the further development of this project here:
[tokenvars](https://github.com/gesistsa/tokenvars). Please don’t use
this software.**

The goal of quanteda.proximity is to add proximity vectors into the
`tokens` object of `quanteda`.

Proximity is measured by the number of tokens away from the keyword.
Given a tokenized sentence: \[“I”, “wash”, “this”, “apple”\] and suppose
“eat” is the keyword. The proximity vector is a vector with the same
length as the tokenized sentence and the values (using the default
settings) are \[2, 1, 2, 3\].
