# -*- coding: utf-8 -*-
# author: Wenbo Lyu
# email: lyu.geosocial@gmail.com
# date: 2025-10-10
# references: 
# https://github.com/Zehua-Zhang-01/Robust_Geographical_Detector/blob/main/CPD_1.ipynb
# https://rstudio.github.io/reticulate/#type-conversions

import numpy as np
import pandas as pd
import ruptures as rpt
from joblib import Parallel, delayed, parallel_backend

def cpd_disc(df, y, xvars, groups, minsizes, cores=1):
    def cpd_disc1(dt, y, x, gs, min_size):
        df_sub = dt.loc[:, [y, x]].copy()
        y_vals = df_sub[y].astype(float).values
        x_vals = df_sub[x].astype(float).values

        b = np.column_stack([y_vals, x_vals])
        sorted_b = b[np.argsort(b[:, 1])]
        signals = sorted_b[:, 0]

        algo = rpt.Dynp(model="l2", min_size=min_size, jump=1).fit(signals)
        result = list(algo.predict(n_bkps=gs - 1))
        result.insert(0, 0)

        labels = [f"group{i+1}" for i in range(gs)]
        df_sub["_category"] = pd.cut(df_sub[x].rank(), bins=result, labels=labels)
        return df_sub["_category"].values

    def ensure_list(x):
        return x if isinstance(x, list) else [x]

    xvars = ensure_list(xvars)
    groups = ensure_list(groups)
    minsizes = ensure_list(minsizes)

    if not (len(xvars) == len(groups) == len(minsizes)):
        raise ValueError("xvars, groups, and minsizes must have the same length")

    # Force thread-based backend
    if cores > 1:
        args_list = [(df, y, xvars[i], groups[i], minsizes[i]) for i in range(len(xvars))]
        with parallel_backend("threading"):
            res = Parallel(n_jobs=cores)(
                delayed(cpd_disc1)(d, y_, x, g, m) for (d, y_, x, g, m) in args_list
            )
    else:
        res = [cpd_disc1(df, y, xvars[i], groups[i], minsizes[i]) for i in range(len(xvars))]

    res = pd.DataFrame(np.array(res).T, columns=xvars)
    return res
