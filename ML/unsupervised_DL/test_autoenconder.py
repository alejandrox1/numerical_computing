#!/usr/bin/env python
import numpy as np
from autoencoder get AutoEncoder
from dnn import DNN
from utils import get_kaggle_mnist

def test_single_autoencoder():
    Xtrain, Ytrain, Xtest, Ytest = get_kaggle_mnist()

    autoencoder = AutoEncoder(300, 0)
    autoencoder.fit(Xtrain, epochs=2, show_fig=True)

    done = False
    while not done:
        i = np.random.choice(len(Xtest))
        x = Xtest[i]
        y = autoencoder.predict([x])

        plt.subplot(1,2,1)
        plt.imshow(x.reshape(28,28), cmap='gray')
        plt.title('Original')

        plt.subplot(1,2,2)
        plt.imshow(y.reshape(28,28), cmap='gray')
        plt.title('Reconstructed')

        plt.show()
    return

def test_autoencoder():
    Xtrain, Ytrain, Xtest, Ytest = get_kaggle_mnist()
    # dnn = DNN([1000, 750, 500])
    # dnn.fit(Xtrain, Ytrain, Xtest, Ytest, epochs=3)
    # vs
    dnn = DNN([1000, 750, 500])
    dnn.fit(Xtrain, Ytrain, Xtest, Ytest, pretrain=True, train_head_only=False, epochs=3)
    # note: try training the head only too! what does that mean?

if __name__=="__main__":
    test_autoencoder()
    # test_single_autoencoder()
