import scipy
import pandas as pd
import matplotlib.pyplot as plt
from pandas.api.types import is_numeric_dtype
from statsmodels.stats.multicomp import pairwise_tukeyhsd
import scikit_posthocs as sp
import matplotlib.backends.backend_pdf

groups = ['CHOR1', 'CHOR2', 'KONTROLA']  #alternatywa - set(medical_data['grupa'])

#########################################################
'''                Preparing input data               '''
#########################################################

def prepare_data(fname):
    medical_data = pd.read_csv(f'./{fname}', sep=';', decimal=',')

    for name, col in medical_data.items():
        if is_numeric_dtype(col):
            if col.isna().any():
                print(f'For column {name} in indexes {medical_data[medical_data[name].isna()].index} NA-value were found')
                medical_data = medical_data.fillna({name: col.mean()})
                print(f'They were substituted with columns mean value - {col.mean()}')
        else:
            if col.isna().any():
                #medical_data = medical_data[medical_data[name].notna()]  #zostaw te wiersze które w danej kolumnie nie maja na
                medical_data = medical_data.dropna(subset=name)           #lub usun te ktore maja na w danej kolumnie
                print(f'Verse deleted {medical_data[medical_data[name].isna()].index} caued by NA-value in non-numerical column')

    return medical_data


def find_outliers(medical_data):
    medical_data.plot(kind="box", subplots=True, layout=(3, 3), figsize=(10, 10))
    plt.savefig('./figures/boxplots.pdf')
    q1 = medical_data.quantile(0.25)
    q3 = medical_data.quantile(0.75)
    iqr = q3 - q1
    IQR_outliers = medical_data[((medical_data < (q1 - 1.5 * iqr)) | (medical_data > (q3 + 1.5 * iqr)))]
    for name, col in IQR_outliers.items():
        if col.notnull().any():
            indices = IQR_outliers[IQR_outliers[name].notnull()].index
            print(f'For column {name} in indexes {indices} outliers were found\nOtlier values: ')
            for ind in indices:
                print(medical_data[name][ind])



#########################################################
'''                Group characeristics              '''
#########################################################
def characteraise_groups(medical_data):
    print(medical_data.groupby(['grupa']).describe())
    print(medical_data.iloc[:, 0:2].groupby(['grupa']).describe())


#########################################################
'''                Compareable analysis              '''
#########################################################
def shapiro_test(medical_data):
    cols = medical_data.iloc[:, 2:].columns
    normals = dict(zip(cols, [True]*len(cols)))
    normals_values = {k: [] for k in cols}
    for col in cols:
        for group in groups:
            x = scipy.stats.shapiro(medical_data[medical_data['grupa'] == group][col]).pvalue
            normals_values[col].append(x)
            if x < 0.05:
                normals[col] = False
    return normals, normals_values


def density_plotting(medical_data):
    #medical_data.LEU.plot.density(title='LEU')                     #pojedynczy wykres
    plt.figure(figsize=(12, 22))
    for x, col in enumerate(medical_data.iloc[:, 2:].columns):
        plt.subplot(5, 2, x+1)
        medical_data.groupby('grupa')[col].plot.density(legend=True, title=col)
    plt.savefig('./figures/densities.pdf')


def levene_tests(medical_data):
    cols = medical_data.iloc[:, 2:].columns
    homo_var = dict(zip(cols, [True] * len(cols)))
    for col in cols:
        p = scipy.stats.levene(medical_data[medical_data['grupa'] == groups[0]][col],
                           medical_data[medical_data['grupa'] == groups[1]][col],
                           medical_data[medical_data['grupa'] == groups[2]][col]).pvalue
        if p < 0.05:
            homo_var[col] = False
    return homo_var


def groups_analysis(medical_data, normals, homo_var):
    cols = medical_data.iloc[:, 2:].columns
    for i, col in enumerate(cols):
        if len(groups) > 2:
            g1 = medical_data[medical_data['grupa'] == 'CHOR1'][col]
            g2 = medical_data[medical_data['grupa'] == 'CHOR2'][col]
            g3 = medical_data[medical_data['grupa'] == 'KONTROLA'][col]
            if normals[col] == True and homo_var[col] == True:
                p = scipy.stats.f_oneway(g1, g2, g3).pvalue
                if p < 0.05:
                    print(f'Between groups in parameter {col} there are statistically significant differences - pvalue {p}')
                    tuckey = pairwise_tukeyhsd(endog=medical_data[col], groups=medical_data['grupa'], alpha=0.05)
                    print(tuckey)
                    plt.figure(figsize=(5, 6))
                    medical_data[['grupa'] + [col]].groupby('grupa').boxplot(subplots=False)
                    plt.savefig(f'./figures/diferences/dif{i}.pdf')
            else:
                p = scipy.stats.kruskal(g1, g2, g3).pvalue
                if p < 0.05:
                    print(f'Between groups in parameter {col} there are statistically significant differences - pvalue {p}')
                    dun = sp.posthoc_dunn(medical_data, val_col = col, group_col='grupa')
                    print(dun)
                    plt.figure(figsize=(5, 6))
                    medical_data[['grupa'] + [col]].groupby('grupa').boxplot(subplots=False)
                    plt.savefig(f'./figures/diferences/dif{i}.pdf')
        else:
            pass   #only when 2 groups - different tests



#########################################################
'''                Correlation analysis               '''
#########################################################
def correlation_analysis(medical_data, normals):
    pdf = matplotlib.backends.backend_pdf.PdfPages("./figures/cor_plots.pdf")
    for i, col in enumerate(medical_data.iloc[:, 2:].columns):
        for j, coll in enumerate(medical_data.iloc[:, 2:].columns):
            if j >= i:
                continue
            #if normals[col] == True and normals[coll] == True:
                #cor = medical_data.groupby('grupa')[[col, coll]].corr(method = 'pearson').unstack().iloc[:, 1]  #its perfect but doesnt return pvalue
            for group in groups:
                if normals[col] == True and normals[coll] == True:
                    cor = scipy.stats.pearsonr(medical_data[medical_data['grupa'] == group][col], medical_data[medical_data['grupa'] == group][coll])
                    if cor.pvalue < 0.05:
                        print(f'Between parameters {col} and {coll} in group {group} correlation was detected: {cor.statistic} ; pvalue {cor.pvalue}')
                        fig, axe = plt.subplots(figsize=(4.3, 4.5))
                        axe.scatter(medical_data[medical_data['grupa'] == group][col], medical_data[medical_data['grupa'] == group][coll])
                        axe.text(0.8, 0.5, f"correlation = {round(cor.statistic, 3)}", bbox=dict(facecolor='red', alpha=0.5), horizontalalignment = 'right', verticalalignment = 'top')
                        axe.set_title('Correlation scatterplot')
                        axe.set_xlabel(col)
                        axe.set_ylabel(coll)
                        #m, b = np.polyfit(medical_data[medical_data['grupa'] == group][col], medical_data[medical_data['grupa'] == group][coll], 1)
                        #axe.plot(medical_data[medical_data['grupa'] == group][col], m*medical_data[medical_data['grupa'] == group][col]+b, color = 'red')
                        plt.close(fig)
                        pdf.savefig(fig)
                else:
                    cor = scipy.stats.spearmanr(medical_data[medical_data['grupa'] == group][col], medical_data[medical_data['grupa'] == group][coll])
                    if cor.pvalue < 0.05:
                        print(f'Between parameters {col} and {coll} in group {group} correlation was detected: {cor.correlation} ; pvalue {cor.pvalue}')
                        fig, axe = plt.subplots(figsize=(4.3, 4.5))
                        axe.scatter(medical_data[medical_data['grupa'] == group][col], medical_data[medical_data['grupa'] == group][coll])
                        axe.text(0.8, 0.5, f"correlation = {round(cor.correlation, 3)}", bbox=dict(facecolor='red', alpha=0.5), horizontalalignment = 'right', verticalalignment = 'top')
                        axe.set_title('Correlation scatterplot')
                        axe.set_xlabel(col)
                        axe.set_ylabel(coll)
                        #m, b = np.polyfit(medical_data[medical_data['grupa'] == group][col], medical_data[medical_data['grupa'] == group][coll], 1)
                        #axe.plot(medical_data[medical_data['grupa'] == group][col], m * medical_data[medical_data['grupa'] == group][col] + b, color='red')
                        plt.close(fig)
                        pdf.savefig(fig)
    pdf.close()
    return


#########################################################
'''                Main               '''
#########################################################

def main():
    df = prepare_data("przykladoweDane-Projekt.csv")
    print(df)
    find_outliers(df)
    characteraise_groups(df)
    normals, normals_values = shapiro_test(df)
    density_plotting(df)
    homo_var = levene_tests(df)
    groups_analysis(df, normals, homo_var)
    correlation_analysis(df, normals)


if __name__ == '__main__':
    main()
