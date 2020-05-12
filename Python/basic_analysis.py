import pandas as pd
import numpy as np
import os

#import data, rename last column
opioids = (pd.read_csv("Opioid Pills Sold by Manufacturer.csv", dtype = str)
          .rename(columns = {"Combined_Labeler_Name":"manu"})
           .astype({"total_pills":"float64", "year":"int64"}))
opioids = (pd.read_csv("county_controls.csv", dtype = {"fips":str})
           .merge(opioids, how = "right", on = ["fips", "year"])
           )

#tasks i want to do
#plot states by total amount of opioid pills sold by year, highlighting
by_state = (opioids.groupby(["state","year"], as_index = False)["total_pills"]
            .sum()
            .pivot(index = "year", columns= "state", values = "total_pills"))
by_state.plot()
by_state.plot(x = "year", y = "total_pills", colormap = "")

#plot top ten manufacturers
#t-test on urban-rural counties
#scatter plot next to median household income


print(opioids.head(6))


print("enter")