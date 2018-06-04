#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
from autoencoder import AutoEncoder, momentum_updates
from utils import xavier_initialization

class DNN:
    def __init__(self, hidden_layer_sizes, unsupervised_model=AutoEncoder):
        self.hidden_layers = []
        count = 0
        for M in hidden_layer_sizes:
            ae = unsupervised_model(M, count)
            self.hidden_layers.append(ae)
            count += 1
        return

    def forward(self, X):
        current_input = X
        for ae in self.hidden_layers:
            Z = ae.forward_hidden(current_input)
            current_input = Z

        # Logistic layer.
        Y = T.nnet.softmax(T.dot(current_input, self.W) + self.b)
        return Y
    
    def predict(self, X):
        return T.argmax(self.forward(X), axis=1)

    def fit(self, X, Y, Xtest, Ytest, learning_rate=0.1, mu=0.99, reg=0.0, pretrain=True, train_head_only=True, epochs=1):
        batch_size     = 100

        learning_rate = np.float32(learning_rate)
        mu            = np.float32(mu)
        reg           = np.float32(reg)

        # Greedy layer-wise training of autoencoder.
        pretrain_epochs = 2
        if not pretrain:
            pretrain_epochs = 0

        current_input = X
        for ae in self.hidden_layers:
            ae.fit(current_input, epochs=pretrain_epochs)

            # Create current_input for next layer.
            current_input = ae.hidden_op(current_input)

        # Initialize logistic regression layer.
        N = len(Y)
        K = len(set(Y))
        W0 = xavier_initialization((self.hidden_layers[-1].M, K))
        self.W = theano.shared(W0, "w_logreg")
        self.b = theano.shared(np.zeros(K, dtype=np.float32), "b_logreg")
        self.params = [self.W, self.b]

        if not train_head_only:
            for ae in self.hidden_layers:
                self.params += ae.forward_params

        X_in    = T.matrix("X_in")
        targets = T.ivector("Targets")
        pY      = self.forward(X_in)

        squared_magnitude = [(p*p).sum() for p in self.params]
        reg_cost          = T.sum(squared_magnitude)
        cost              = - T.mean( T.log(pY[T.arange(pY.shape[0]), targets]) ) + reg*reg_cost
        prediction        = self.predict(X_in)
        cost_predict_op   = theano.function(
                inputs  = [X_in, targets],
                outputs = [cost, prediction],
        )

        updates  = momentum_updates(cost, self.params, mu, learning_rate)
        train_op = theano.function(
                inputs  = [X_in, targets],
                updates = udates,
        )

        n_batches = N // batch_sizw
        costs = []
        print("supervised training...")
        for i in range(epochs):
            print("epoch:", i)
            X, Y = shuffle(X, Y)
            for j in range(n_batches):
                Xbatch = X[j*batch_sz : (j+1)*batch_size]
                Ybatch = Y[j*batch_sz : (j+1)*batch_size]
                
                train_op(Xbatch, Ybatch)
                cost, prediction = cost_predict_op(Xtest, Ytest)
                error = error_rate(the_prediction, Ytest)
                print("j / n_batches: {}/{}, cost: {}, error: {}".format(j, n_batches, cost, error))
                costs.append(the_cost)
        
        plt.plot(costs)
        plt.show()
