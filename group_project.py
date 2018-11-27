#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 26 12:48:40 2018

@author: jialiluan
"""

import statsmodels.api as sm
import statsmodels.formula.api as smf
import pandas as pd
import numpy as np
AQ = pd.read_csv('Beijing.csv')

dic = {1: "Winter",
       2: "Winter",
       3: "Spring",
       4: "Spring",
       5: "Spring",
       6: "Summer",
       7: "Summer",
       8: "Summer",
       9: "Fall",
       10: "Fall",
       11: "Fall",
       12: "Winter"}


AQ['season'] = AQ['month'].map(dic)
AQ = AQ.dropna()
AQ = AQ[AQ['pm2.5'] > 0]
AQ['pm25_log'] = np.log(AQ['pm2.5'])

AQ_cv = AQ[AQ['cbwd'] == 'cv']
AQ_cv = AQ_cv[(AQ_cv['pm25_log'] > 2.2) & (AQ_cv['pm25_log'] < 6.8)]

AQ_NE = AQ[AQ['cbwd'] == 'NE']
AQ_NE = AQ_NE[(AQ_NE['pm25_log'] > 0.5)]

AQ_NW = AQ[AQ['cbwd'] == 'NW']
AQ_NW = AQ_NW[(AQ_NW['pm25_log'] > 0.5)]

AQ_SE = AQ[AQ['cbwd'] == 'SE']
AQ_SE.sort_values(['pm25_log'], ascending=[False])
AQ_SE = AQ_SE[(AQ_SE['pm25_log'] > 0.5) & (AQ_SE['pm25_log'] < 6.291569)]

AQ_new = pd.concat([AQ_cv, AQ_NE, AQ_NW, AQ_SE])

mixed = smf.mixedlm("pm25_log ~ year+month+day+hour+DEWP+TEMP+PRES+Is+Ir", AQ_new, groups = AQ_new["cbwd"], re_formula="~hour+PRES")
mixed_fit = mixed.fit()
print(mixed_fit.summary())
