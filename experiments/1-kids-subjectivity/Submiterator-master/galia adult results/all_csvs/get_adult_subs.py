#!/usr/bin/python
from __future__ import division

import re
import os
import itertools
from itertools import groupby
import sys
import csv
import numpy as np
import matplotlib.pyplot as plt
import scipy
from scipy import *
import operator
from scipy.special import comb #comb(N,k, exact=False)
from collections import OrderedDict


sub_dict = {}

infile = open('sub_judge_adult.txt', 'r')

readinfile = infile.readlines()

for x in readinfile:
    y  = x.split(',')
    if y[0] in sub_dict.keys():
        temp = sub_dict[y[0]]
        temp.append(float(y[1].strip()))
        sub_dict[y[0]] = temp
    else:
        sub_dict[y[0]] = [float(y[1].strip())]

#print sub_dict
#print len(sub_dict.keys())

av_sub_dict = {}

for word in sub_dict.keys():
    av_sub_dict[word] = sum(sub_dict[word])/len(sub_dict[word])

outfile = open('sub_judge_adult_av.txt','w')

for word in av_sub_dict.keys():
    outfile.write(word+'\t'+str(av_sub_dict[word])+'\n')

infile.close()
outfile.close()