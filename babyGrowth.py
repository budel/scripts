import os
import pandas as pd
import matplotlib as mpl
import numpy as np
import matplotlib.pyplot as plt
from scipy import ndimage
import datetime as dt
from datetime import date

from dateutil.relativedelta import relativedelta
import math 
from urllib.request import Request, urlopen

from labelLine import *

#Baby's Name:
babyName = "Baby"

#Baby's birthday:
babyBDay = pd.Timestamp('2020-07-17 03:00:00')

#Baby's gender (F/M)
babyGender = "M"

#Baby's dataFile
babyFile = "Baby.xlsx"

#Set working Directory
workDir = "./"
os.chdir(workDir)

#set directory for data
dirData = "/sdcard/Download/" #set to code root

excelBaby = pd.ExcelFile(dirData+babyFile, engine='openpyxl')

#Weight
dfW = excelBaby.parse('Weight')

def fetchWHOData(urlWHO, dirData, fileName):
    print("> Reading WHO data: ", fileName)
    try:
        print(">> Fetching from WHO site")
        req = Request(urlWHO+fileName)
        req.add_header('User-Agent', 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:77.0) Gecko/20100101 Firefox/77.0')
        content = urlopen(req)
        return pd.read_csv(content,sep="\t")
    except:
        print(">> URL failed. Reading data from file.")
        try:
            return pd.read_csv(dirData+'WHOData/'+fileName,sep="\t")
        except:
            raise Exception("Error: Missing WHO data:", fileName)

urlWHO = "https://www.who.int/childgrowth/standards/"

gender = "boys" if (babyGender == "M") else "girls"

#Weight
fileNameWHOWeight = "wfa_"+gender+"_p_exp.txt"
dfWFA = fetchWHOData(urlWHO, dirData, fileNameWHOWeight)

dfWFA.rename(columns={"Age": "days"}, inplace=True)
dfWFA["weeks"] = dfWFA['days']/7.
dfWFA["DT"] = dfWFA["days"].apply(lambda x: babyBDay + dt.timedelta(days=x))
dfWFA["Date"] = dfWFA["DT"].apply(lambda x: x.date())

dfW["DT"] = pd.to_datetime(dfW["DT"], format='%Y.%m.%d %H:%M')
dfW["Date"] = dfW["DT"].apply(lambda x: x.date())
#dfW["Date"] = pd.to_datetime(dfW["Date"], format='%Y.%m.%d')
dfW["W"] = dfW["W"] / 1000.
dfW["Err"] = dfW["Err"] / 1000.
#dfW["days"] = (dfW['Date'] - bDay).dt.days
dfW["days"] = (86400*((dfW['DT'] - babyBDay).dt.days)+((dfW['DT'] - babyBDay).dt.seconds))/86400
#dfW["days"] = ((dfW['DT'] - bDay).dt.days)
dfW["weeks"] = dfW['days']/7.
print(dfW,3)

xMin = 0
#yMin = 2.75
yMin = dfWFA["P1"].min()
xPLus = 5
yPlus = 0.1
xMax = dfW['days'].max() + xPLus
yMax = dfW['W'].max() + yPlus

#Percentile Column Llist
lColsP = [x for x in dfWFA.columns if 'P' in x]

fig, ax = plt.subplots(figsize=(20, 10))

tAxis = "days"
ax.set_xlim(xMin,xMax)
ax.set_ylim(yMin,yMax)

for i in lColsP:
    ax.plot(dfWFA[tAxis], dfWFA[i], label=str(i), lw=0.5)
labelLines(plt.gca().get_lines(),zorder=2.5)

ax.errorbar(dfW[tAxis], dfW["W"], yerr=dfW["Err"], c="b", label=babyName, lw=1, marker="")
ax.plot(dfW[dfW["officialFlag"]=='y'][tAxis], dfW[dfW["officialFlag"]=='y']["W"], c="r", lw=0, marker="o")

ax.set(xlabel='time (d)', ylabel='weight (kg)', title=babyName+'\'s weight')
ax.grid()

fig.savefig(dirData+babyName+"_W.png", bbox_inches = 'tight')
plt.show()

