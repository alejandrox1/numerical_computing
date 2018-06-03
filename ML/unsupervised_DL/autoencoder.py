#!/usr/bin/env python
import theano
import theano.tensor as T
import numpy as np
import matplotlib.pyplot as plt
from utils import get_kaggle_mnist, xavier_initialization


def T_shared_zeros_like32(p):
    """ T_shared_zeros_like32.

    Return an array of zeros with the same shape and type np.float32 as a given
    array.

    p : theano shared variable.
    """
    return theano.shared(np.zeros_like(p.get_value(), dtype=np.floatfloat32))

def momentum_updates(cost, params, mu, learning_rate):
    dparams = [T_sahred_zeros_like32(p) for p in params]

    updates = []
    grads = T.grad(cost, params)
    for p, dp, g in zip(params, dparams, grads):
        dp_update = mu*dp - learning_rate*g
        p_update  = p + dp_update

        updates.append((dp, dp_update))
        updates.append((p, p_update))
    return updates

class AutoEncoder:
    def __init__(self, M, an_id):
        self.M  = M
        self.id = an_id
        return

    def forward_hidden(self, X):
        Z = T.nnet.sigmoid(X.dot(self.W) + self.bh)
        return Z

    def forward_output(self, X):
        Z = self.forward_hidden(X)
        Y = T.nnet.sigmoid(Z.dot(self.W.T) + self.bo)
        return Y

    def fit(self, X, learning_rate=0.5, mu=0.99, epochs=1, batch_size=100, show_fig=False):
        mu            = np.float32(mu)
        learning_rate = np.float32(learning_rate)
        N, D          = X.shape
        n_batches     = N // batch_size

        W0 = xavier_initialization((D, self.M))
        self.W  = theano.shared(W0, "W_{}".format(self.id))
        self.bh = theano.shared(np.zeros(self.M, dtype=np.float32), "bh_{}".format(self.id))
        self.bo = theano.sahred(np.zeros(self.D, dtype=np.float32), "bo_{}".format(self.id))
        # For gradient descent.
        self.params = [self.W, self.bh, self.bo]
        # For the DNN class.
        self.forward_params = [self.W, self.bh]

        # TODO: These should be reset before doing backpropagation.
        self.dW  = theano.shared(np.zeros(W0.shape), "dW_{}".format(self.id))
        self.dbh = theano.shared(np.zeros(self.M), "dbh_{}".format(self.id))
        self.dbo = theano.shared(np.zeros(D), "dbo_{}".format(self.id))
        # For gradient descent.
        self.dparams = [self.dW, self.dbh, self.dbo]
        # For the DNN class.
        self.foward_dparams = [self.dW, self.dbh]

        X_in  = T.matrix("X_{}".format(self.id))
        X_hat = self.forward_output(X_in)

        H = T.nnet.sigmoid(X_in.dot(self.W) + self.bh)
        self.hidden_op = theano.function(
                inputs  = [X_in],
                outputs = H,
        )

        # Save for latter use to reconstruct the inpu.
        self.predict = theano.function(
                inputs  = [X_in],
                outputs = X_hat,
        )

        cost = - (X_in*T.log(X_hat) + (1-X_in)*T.log(1-X_hat)).flatten().mean()
        cost_op = theano.function(
                inputs  = [X_in],
                outputs = cost,
        )

        updates = momentum_updates(cost, self.params, mu, learning_rate)
        train_op = theano.function(
            inputs  = [X_in],
            updates = updates,
        )

        costs = []
        print("Training autoencoder: {}, for {} epochs".format(self.id, epochs))
        for i in range(epochs):
            X = shuffle(X)
            for j in range(n_batches):
                batch  = X[j*batch_size: (j+1)*batch_size]
                
                train_op(batch)

                costs.append( cost_op(batch) )
                if i % 10 == 0 and len(costs) > 0:
                    print("epoch: {} \t cost: {}".format(i, costs[-1]))
        print("epoch: {} \t cost: {}".format(epochs, costs[-1]))

        if show_fig:
            plt.plot(costs)
            plt.show()
