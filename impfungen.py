import requests
import os, math
import datetime
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.units as munits
converter = mdates.ConciseDateConverter()
munits.registry[datetime.date] = converter
munits.registry[datetime.datetime] = converter

url = "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Impfquotenmonitoring.xlsx?__blob=publicationFile"
headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36'}
xlsxf = "impfungen.xlsx"
dirData = "/sdcard/Download/"
popGroups = [8600000, 13800000, 14900000]

if True:
    if os.path.exists(dirData+xlsxf):
        os.remove(dirData+xlsxf)
    response = requests.get(url, headers=headers)
    with open(dirData+xlsxf, 'wb') as f:
        f.write(response.content)
    print(response)

df = pd.ExcelFile(dirData+xlsxf, engine='openpyxl') 
df = pd.read_excel(df, usecols=['Datum', 'Einmal geimpft', 'Vollständig geimpft', 'Gesamtzahl verabreichter Impfstoffdosen'], sheet_name='Impfungen_proTag')
df = df[df['Datum'].first_valid_index():df['Datum'].isna().argmax()]
df = df.rename(columns={'Gesamtzahl verabreichter Impfstoffdosen':'Gesamt'})
df.insert(0, 'Days', range(1, len(df)+1))
df['cumsum'] = df['Gesamt'].cumsum()
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
ax.scatter(x=dates[:len(df)], y=df['cumsum'])
ax.grid(True)
ax.set_ylim(bottom=0, top=maxpop)
ax.set_xlim(left=dates[0], right=dates[-1])
ax.set_ylabel('verabreichte Impfungen')
ax.set_xlabel('Tage')
ax.annotate('70%', xy=(dateof70p, 0.7*maxpop), xytext=(-15, 25), textcoords='offset points', arrowprops=dict(facecolor='black'))

cumpop = 0
for i, pop in enumerate(popGroups):
    cumpop += pop
    for j, xytext in enumerate([(25,-25), (-25,25)]):
        txt = f'Gruppe ${i+1}' + ('$' if (j+1)==2 else '/2$')
        y = cumpop*(j+1)
        x = df['Datum'][0] + datetime.timedelta(daypop(y))
        print('am', x, 'sind', y, 'Menschen geimpft')
        ax.annotate(txt, xy=(x, y), xytext=xytext, textcoords='offset points', arrowprops=dict(facecolor='black'))

fig2.savefig(dirData+"tage_pro_impfung.png", bbox_inches = 'tight')

