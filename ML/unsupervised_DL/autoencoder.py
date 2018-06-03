#!/user/bin/env python
import numpy as np

def get_kaggle_mnist():
    """ Return MNIST dataset in Numpy Arrays.
    https://www.kaggle.com/c/digit-recognizer/data

    Column 0 are the labels.
    Column 1-785 is data. Each entry has a range of [0, 255].
    Size of csv file: 42000 samples, 1 label, 28 rows, 28 columns.

    Return
    ------
    Xtrain: array-like
    Ytrain: array-like
    Xtest:  array-like
    Ytest:  array-like
    ""
