# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import numpy

from pomegranate import *

import numpy as np

#Distribution for node A node B and node C
A = DiscreteDistribution({'a0':0.1,'a1':0.9})
B = DiscreteDistribution({'b0':0.4,'b1':0.6})
C = DiscreteDistribution({'c0':0.3,'c1':0.7})

#node D is dependent on both node A and node B
#1st column is parent A,2nd is parent B, 3rd is child C, the last is the probability
D = ConditionalProbabilityTable(
    [['a0','b0','d0',0.9],
     ['a1','b1','d0',0.1],
     ['a0','b1','d0',0.7],
     ['a1','b0','d0',0.6],
     ['a0','b0','d1',0.1],
     ['a1','b1','d1',0.9],
     ['a0','b1','d1',0.3],
     ['a1','b0','d1',0.4]],[A,B])

#node e is dependent on both node C and node D
#1st column is parent C,2nd is parent D, 3rd is child E, the last is the probability
E = ConditionalProbabilityTable(
    [['c0','d0','e0',0.9],
     ['c1','d1','e0',0.1],
     ['c0','d1','e0',0.8],
     ['c1','d0','e0',0.7],
     ['c0','d0','e1',0.1],
     ['c1','d1','e1',0.9],
     ['c0','d1','e1',0.2],
     ['c1','d0','e1',0.3]],[C,D])

#node F is dependent on node D
F = ConditionalProbabilityTable(
    [['d0','f0',0.8],
     ['d1','f0',0.3],
     ['d0','f1',0.2],
     ['d1','f1',0.7]],[D])

#State p4objects hold for the distribution, and a high level name
s1 = State(A, name = "A")
s2 = State(B, name = "B")
s3 = State(C, name = "C")
s4 = State(D, name = "D")
s5 = State(E, name = "E")
s6 = State(F, name = "F")

#Create the Bayesian network
model = BayesianNetwork("Problem1.4")

#Add states
model.add_states(s1,s2,s3,s4,s5,s6)

#Add edges which represent conditional dependencies
model.add_edge(s1,s4)
model.add_edge(s2,s4)
model.add_edge(s3,s5)
model.add_edge(s4,s5)
model.add_edge(s4,s6)

#Finalize the topology of the model
model.bake()

#Print the structure
#print(model.structure)

#Marginal Distribution
#Q1.p(E=e0)
print(model.marginal())

#Probability of an Event
#Q2.p(A=a0,B=b1,C=c0,D=d1,E=e0,F=f1)
print(model.probability([['a0','b1','c0','d1','e0','f1']]))

#(Conditional)Probability of each variable

#Q3.p(A=a0|F=f0)
print(model.predict_proba([[None,None,None,None,None,'f0']]))

#Q4.p(E=e1|A=a0, B=b0)
print(model.predict_proba([['a0','b0',None,None,None,None]]))

#Q5.p(A=a0, B=b0|E=e1)
print(model.predict_proba([[None,None,None,None,'e1',None]]))

xx = np.loadtxt('data') 
model1 = BayesianNetwork.from_samples(xx, algorithm='exact-dp')
print(model1.structure) 

print(model1.predict([[0,1,0,1,1,1,0,1,0,0,1, None]]))
print(model1.predict([[0,0,0,1,1,0,0,1,0,1,1, None]]))
print(model1.predict([[0,1,1,0,0,0,0,1,0,1,1, None]]))
print(model1.predict([[1,0,1,1,0,1,0,1,1,0,0, None]]))
print(model1.predict([[1,1,1,1,0,1,1,0,1,0,1, None]]))
print(model1.predict([[0,1,0,0,0,1,0,0,1,0,1, None]]))
