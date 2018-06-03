#!/user/bin/env python
import numpy as np
import pandas as pd
from sklearn.utils import shuffle


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
    """
    train = pd.read_csv("train.csv").as_matrix().astype(np.float32)
    train = shuffle(train)

    Xtrain = train[:-1000, 1:] / 255
    Ytrain = train[:-1000, 0].astype(np.int32)

    Xtest = train[-1000:, 1:] / 255
    Ytest = train[-1000:, 0].astype(np.int32)

    return Xtrain, Ytrain, Xtest, Ytest

def xavier_initialization(shape):
    w = np.random.randn(*shape) / np.sqrt(sum(shape))
    return w.astype(np.float32)

def relu(x):
    return x * (x > 0)

def error_rate(p, t):
    return np.mean(p != t)
