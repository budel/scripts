import requests
import os, math
import datetime
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm

url = "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Impfquotenmonitoring.xlsx?__blob=publicationFile"
headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36'}
xlsxf = "impfungen.xlsx"
dirData = "/sdcard/Download/"

if False:
    if os.path.exists(dirData+xlsxf):
        os.remove(dirData+xlsxf)
    response = requests.get(url, headers=headers)
    with open(dirData+xlsxf, 'wb') as f:
        f.write(response.content)
    print(response)

df = pd.ExcelFile(dirData+xlsxf, engine='openpyxl') 
df = pd.read_excel(df, usecols=['Datum', 'Erstimpfung', 'Zweitimpfung', 'Gesamtzahl verabreichter Impfstoffdosen'], sheet_name='Impfungen_proTag')
df = df[:len(df)-4]
df = df.rename(columns={'Gesamtzahl verabreichter Impfstoffdosen':'Gesamt'})
df.insert(0, 'Days', range(1, len(df)+1))
print(df.tail())

X = sm.add_constant(df['Days'])
Y = df['Gesamt']
result = sm.OLS(Y, X).fit()
#print(result.summary())
a = result.params.Days
b = result.params.const

days = df['Days'] 
fittedy = [a*x+b for x in days]

fig = df.plot.scatter(x='Days', y='Gesamt').get_figure()
plt.plot(days, fittedy)
fig.savefig(dirData+"impfungen_pro_tag.png", bbox_inches = 'tight')

maxpop = int(2*832e5)
population = range(1,maxpop,100000) 
daypop = lambda p : (math.sqrt(2*a*p+b**2)-b) / a
dayof70p = daypop(0.7*maxpop)
dateof70p = df['Datum'][0] + datetime.timedelta(dayof70p)
print('noch', dayof70p-len(df), 'Tage bis 70% zweimal geimpft sind')
print('Also am', dateof70p)

maxdays = int(daypop(maxpop))+1
cumimpf = [0.5*a*x**2+b*x for x in range(maxdays)] 
dates = [df['Datum'][0] + datetime.timedelta(i) for i in range(maxdays)]
print(dayof70p)
fig2, ax = plt.subplots()
ax.plot(dates, cumimpf)
ax.grid(True)
ax.set_ylim(bottom=0)
ax.set_xlim(left=df['Datum'][0])
ax.set_ylabel('verabreichte Impfungen')
ax.set_xlabel('Tage')
ax.annotate('70%', xy=(dateof70p, 0.7*maxpop), xytext=(-15, 25), textcoords='offset points', arrowprops=dict(facecolor='black', shrink=0.05))
fig2.savefig(dirData+"tage_pro_impfung.png", bbox_inches = 'tight')

