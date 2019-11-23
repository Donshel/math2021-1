"""
Python script to handle data

Take as input the raw data of Shunyi station (resources/csv)
- Replace `month` by a continuous `month` based on `day` and `month`
- Remove variable `day` (contained in continuous `month`)
- Remove variable `CO` (no WHO recommandation)
- Remove variable `station` and `index` (useless)
- Group all rows by (`year`, `month`, `day`), aggregating by mean of means, except for the `wind`:
    - The more predominant `wdir` has been kept (taking into account the wind speed associated with each wind direction).
Give as output the transform data (put in resources/csv)
"""
import pandas as pd
import numpy as np
import os as os


"""
Computation of a continuous month variable from the full date, indicating the progress in year on a 
[0, 12) scale. 
"""
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

# Drop useless variables
data.drop(['index', 'station', 'hour', 'day', 'CO'], axis=1, inplace=True)

# Compute the predominant wind direction, taking into account the wind speed
weighted_wdir = data[['year', 'month', 'wdir', 'wspd']].groupby(['year', 'month', 'wdir']).sum()
id_most_common_wdir = weighted_wdir.groupby(['year', 'month']).idxmax()
wdir = [direction for (_, _, direction) in id_most_common_wdir['wspd']]

# Aggregate the data for each day (month has a unique value for each day of one year)
data = data.groupby(['year', 'month']).mean()

# Add the predominant wind direction
data['wdir'] = wdir

# Output the data
directory = os.path.dirname('products/csv/')
if not os.path.exists(directory):
    os.makedirs(directory)

data.to_csv("products/csv/data.csv", index=True)
