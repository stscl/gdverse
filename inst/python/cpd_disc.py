# -*- coding: utf-8 -*-
# author: Wenbo Lv
# email: lyu.geosocial@gmail.com
# date: 2024-10-27
# references: 
# https://github.com/Zehua-Zhang-01/Robust_Geographical_Detector/blob/main/CPD_1.ipynb
# https://rstudio.github.io/reticulate/#type-conversions

import numpy as np
import pandas as pd
import ruptures as rpt
from joblib import Parallel, delayed

def cpd_disc(df,y,xvars,groups,minsizes,cores = 1):
    def cpd_disc1(dt,y,x,gs,min_size):
        df = dt.loc[:,[y,x]]
        IndusFile = df.values
        df_2 = df.copy()
        xvar = x
        y = IndusFile[:, 0].astype(float)
        x = IndusFile[:, 1].astype(float)
        b = np.array([y, x]).transpose()
        sorted_b = b[np.argsort(b[:, 1])]
        signals = np.array(sorted_b[:, 0])
        algo = rpt.Dynp(model = "l2", min_size = min_size, jump = 1).fit(signals)
        result = algo.predict(n_bkps = gs - 1)
        result.insert(0, 0)
        labels = ["empty"] * gs
        for i in range(0, gs):
            labels[i] = str("group" + str(i + 1))
        df_2['_category'] = pd.cut(df_2[xvar].rank(), bins=result, labels=labels)  
        return df_2.loc[:,'_category'].values
      
    def scalar4list(x):
        if isinstance(x, list):
            return(x)
        else:
            return([x])  
          
    xvars = scalar4list(xvars)
    groups = scalar4list(groups)
    minsizes = scalar4list(minsizes)
      
    if cores < 1:
        raise ValueError("Cores must be greater than or equal to 1")
    elif cores == 1:
        res = []
        for i in range(len(xvars)):
            result = cpd_disc1(df, y, xvars[i], groups[i], minsizes[i])
            res.append(result)
        res = pd.DataFrame(np.array(res).transpose(), columns=xvars)
    else:
      args_list = [(df,y,xvars[i],groups[i],minsizes[i]) for i in range(len(xvars))]
      res = Parallel(n_jobs=cores)(delayed(cpd_disc1)(d,y,x,g,m) for d,y,x,g,m in args_list)
      res = pd.DataFrame(np.array(res).transpose(), columns=xvars)
      
    return res
