"""
Python script to handle data

Take as input the raw data of Shunyi station (resources/csv)
- Transform `month`, `day` to a float `month`
- Remove variable `wdir` (hard to deal with and strongly correlated with `month` and `wdsp`
- Remove variable `CO` (no WHO recommandation)
- Group all rows by (`year`, `month`, `day`), aggregating by mean of means.
Give as output the transform data (put in resources/csv)
"""
import pandas as pd
import numpy as np
import os as os


def month(year, month, day):
    days = np.array([31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])
    result = []
    for (y, m, d) in zip(year, month, day):
        if ((y % 4) == 0):
            days[1] = 29
        else:
            days[1] = 28
        past_days = 0.5 + (d - 1) + np.sum(days[:m-1])
        result.append(12 * past_days / (366 if (y % 4 == 0) else 365))
    return result

data = pd.read_csv("resources/csv/data.csv")

data['month'] = month(data['year'], data['month'], data['day'])
data.drop(['index', 'station', 'wdir', 'hour', 'day', 'CO'], axis=1, inplace=True)
data = data.groupby(['year', 'month']).mean()

directory = os.path.dirname('products/csv/')
if not os.path.exists(directory):
    os.makedirs(directory)

data.to_csv("products/csv/data.csv", index=True)
