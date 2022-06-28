# -*- coding: utf-8 -*-
"""

@author: Chase_Yi_One

"""

#%%

import os
os.chdir("C:/Users/c4780/Desktop/desktop/Quant/Action_chinoise_A")
os.getcwd()

import pandas as pd

from warnings import simplefilter
simplefilter(action="ignore", category=pd.errors.PerformanceWarning)

stockA = pd.read_csv("2007-2203日后复权.csv",sep=',',encoding='GB18030',header=None)
stockA.info()

# 代码 名称 日期 开/高/低/收/量/额
# (后复权)

# 修改列名 : 
stockA.rename(columns={0:'code', 1:'name', 2:'date', 3:'open', 4:'high', 5:'low', 6:'close'}, inplace = True)
stockA = stockA.loc[:,['code','date','close']]

code_list = stockA.loc[:,['code']]
code_list.drop_duplicates(keep='first', inplace=True)
code_list = code_list.reset_index(drop=True)

datetime = stockA.loc[:,['date']]
datetime.drop_duplicates(keep='last', inplace=True)
datetime = datetime.reset_index(drop=True)

df = pd.DataFrame([])

# 构建新的DataFrame
for i in range(0,len(code_list)) :
    
    print(i)
    
    # 将列表/数组转换为pandas系列,然后在执行赋值时,系列中缺少的索引将填充NaN
    df.loc[:, code_list.iat[i,0]] = pd.Series(stockA['close'].loc[stockA['code']==code_list['code'].loc[i]].values)

# 重新设置索引
df.loc[:,'date'] = datetime['date']
df = df.set_index('date')

# 对列数据进行过滤 : 
def drop_col(df, cutoff=0.99): # 如果这一列中有1%以上的缺失值，那么就从df中删除这一列
    n = len(df)
    cnt = df.count() # 对列进行非nan值计数
    cnt = cnt / n # 求出非nan值的百分比
    return df.loc[:, cnt[cnt >= cutoff].index] # 根据cnt记录的百分比，过滤出cnt百分百大于等于0.99的（也就是去掉nan值大于0.01的索引），然后对df进行选择，行所有，列为满足要求的cnt的索引。

df = drop_col(df)
print(df)

# 删除表中含有任何NaN的行
df = df.dropna(axis=0,how='any') #drop all rows that have any NaN values
print(df)

# 计算每一列里的空值比例 
df.isnull().sum()/len(df)

# 3670,209 [行数,列数] => df {209只股票}

# 数据存储
# from pathlib import Path  
# filepath = Path('C:/Users/c4780/Desktop/desktop/Quant/Action_chinoise_A/stockA.csv')  
# filepath.parent.mkdir(parents=True, exist_ok=True) 

# 数据读取
stockA_df = pd.read_csv('stockA.csv') 
    
#%%
import akshare as ak
import pandas as pd 

# 对列数据进行过滤 : 
def drop_col(df, cutoff=0.99): # 如果这一列中有1%以上的缺失值，那么就从df中删除这一列
    n = len(df)
    cnt = df.count() # 对列进行非nan值计数
    cnt = cnt / n # 求出非nan值的百分比
    return df.loc[:, cnt[cnt >= cutoff].index] # 根据cnt记录的百分比，过滤出cnt百分百大于等于0.99的（也就是去掉nan值大于0.01的索引），然后对df进行选择，行所有，列为满足要求的cnt的索引。

# pandas DataFrame打印输出列名对齐 (中文列名) :
pd.set_option('display.unicode.ambiguous_as_wide', True)
pd.set_option('display.unicode.east_asian_width', True)

import os
os.chdir("C:/Users/c4780/Desktop/desktop/Quant/Action_chinoise_A")
os.getcwd()

# 数据读取
stock_pick = pd.read_csv('stock_pick.csv')

stock_pick = stock_pick.loc[:,'Actions choisies']

stock_pick =  pd.DataFrame(stock_pick)

stock_pick.rename(columns={'Actions choisies':'code'}, inplace = True)

# 股票指数
# 实时行情数据
# 接口: stock_zh_index_spot
# 目标地址: http://vip.stock.finance.sina.com.cn/mkt/#hs_s
# 描述: 中国股票指数数据, 注意该股票指数指新浪提供的国内股票指数
# 限量: 单次返回所有指数的实时行情数据
stock_zh_index_spot_df = ak.stock_zh_index_spot()

stock_zh_index_spot_df.rename(columns={'代码':'code','名称':'name'},inplace=True)

stock_zh_index_spot_df = stock_zh_index_spot_df.loc[:,['code','name']]

stock_zh_index_spot_df['code'] = stock_zh_index_spot_df['code'].str.upper()

indice = pd.merge(stock_pick,stock_zh_index_spot_df,
                     how='inner',
                     left_on=['code'],
                     right_on=['code'])

stock_pick = stock_pick.drop(labels = stock_pick.index[stock_pick['code'].isin(indice['code'])], axis=0) #删除股票指数的对应代码

stock_pick = stock_pick.reset_index(drop=True)

import tushare as ts
# 设置token
ts.set_token('49cbb8fa012ee0c2295a1b9da1b6c3a9bab7c45941579767d6daaf40')
# 初始化pro接口
pro = ts.pro_api()
#查询当前所有正常上市交易的股票列表
code_list = pro.stock_basic(exchange='', list_status='L', fields='ts_code')
code_list.rename(columns={'ts_code':'code'}, inplace = True)
str_ = code_list['code'].str.split('.')

for s in range(0,len(code_list)) :
    p2 = str_[s][0:1]
    p1 = str_[s][1:2]
    p3 = p1+p2
    string = ''
    p3 = string.join(p3)
    code_list.iloc[s,0] = p3

# 合并 股票代码 : stock_pick + code_list {交集}
action_list = pd.merge(stock_pick,code_list,
  how='inner',
  left_on=['code'],
  right_on=['code'])

# 第0行第0列的数据
# action_list.iloc[0,0]

# 股票代码去除地区标识 : SH,SZ
action_list = pd.DataFrame(action_list['code'].str[2:])

###############################################################################
# 数据读取
stockA_df = pd.read_csv('stockA.csv') 

for j in range(0,len(stock_pick)) :
    
    print(j+1)
    
    locals()['new_close_'+str(j+1)] = pd.DataFrame(stockA_df, columns=['Date',stock_pick.iloc[j,0]])

dfs = [ eval('new_close_'+str(1)),eval('new_close_'+str(2)),eval('new_close_'+str(3)),#1
        eval('new_close_'+str(4)),eval('new_close_'+str(5)),eval('new_close_'+str(6)),#2
        eval('new_close_'+str(7)),eval('new_close_'+str(8)),eval('new_close_'+str(9)),#3
        eval('new_close_'+str(10)),eval('new_close_'+str(11)),eval('new_close_'+str(12)),#4  
        eval('new_close_'+str(13)),eval('new_close_'+str(14)),eval('new_close_'+str(15)),#5
        eval('new_close_'+str(16)),eval('new_close_'+str(17)),eval('new_close_'+str(18)),#6
        eval('new_close_'+str(19)),eval('new_close_'+str(20)),eval('new_close_'+str(21)),#7
        eval('new_close_'+str(22)),eval('new_close_'+str(23)),eval('new_close_'+str(24)),#8
        eval('new_close_'+str(25)),eval('new_close_'+str(26)),eval('new_close_'+str(27)),#9
        eval('new_close_'+str(28)),eval('new_close_'+str(29)),eval('new_close_'+str(30)),#10
        eval('new_close_'+str(31)),eval('new_close_'+str(32)),eval('new_close_'+str(33)),#11
        eval('new_close_'+str(34)),eval('new_close_'+str(35)),eval('new_close_'+str(36)),#12
        eval('new_close_'+str(37)),eval('new_close_'+str(38)),eval('new_close_'+str(39)),#13
        eval('new_close_'+str(40)),eval('new_close_'+str(41)),eval('new_close_'+str(42)),#14
        eval('new_close_'+str(43)),eval('new_close_'+str(44)),eval('new_close_'+str(45)),#15
        eval('new_close_'+str(46)),eval('new_close_'+str(47)),eval('new_close_'+str(48)),#16
        eval('new_close_'+str(49)),eval('new_close_'+str(50)),eval('new_close_'+str(51)),#17
        eval('new_close_'+str(52)),eval('new_close_'+str(53)),eval('new_close_'+str(54)),#18
        eval('new_close_'+str(55)),eval('new_close_'+str(56)),eval('new_close_'+str(57)),#19
        eval('new_close_'+str(58)),eval('new_close_'+str(59)),eval('new_close_'+str(60)),#20
        eval('new_close_'+str(61)),eval('new_close_'+str(62)),eval('new_close_'+str(63)),#21
        eval('new_close_'+str(64)),eval('new_close_'+str(65)),eval('new_close_'+str(66)),#22
        eval('new_close_'+str(67)),eval('new_close_'+str(68)),eval('new_close_'+str(69)),#23
        eval('new_close_'+str(70)),eval('new_close_'+str(71)),eval('new_close_'+str(72)),#24
        eval('new_close_'+str(73)),eval('new_close_'+str(74)),eval('new_close_'+str(75)),#25
        eval('new_close_'+str(76)),eval('new_close_'+str(77)),eval('new_close_'+str(78)),#26
        eval('new_close_'+str(79)),eval('new_close_'+str(80)),eval('new_close_'+str(81)),#27
        eval('new_close_'+str(82)),eval('new_close_'+str(83)),eval('new_close_'+str(84)),#28
        eval('new_close_'+str(85)),eval('new_close_'+str(86)),eval('new_close_'+str(87))]#29

# 通过设置axis=1, 可以横向合并
df_final = pd.concat(dfs, axis=1)
new_close = df_final.T.drop_duplicates(keep='first').T 

#将指定数据转为相应类型的datetime64型数据
new_close['Date'] = pd.to_datetime(new_close['Date'])
new_close.dtypes

# 每个日期对应的月份单独作为一列
new_close['Mois'] = new_close['Date'].dt.month 

# 按季度筛选 [3,6,9,12]
new_close = new_close.loc[new_close['Mois'].isin([3,6,9,12])]

# 日期只显示年份和月份
new_close['Date'] = new_close['Date'].apply(lambda x: x.strftime('%Y-%m')).values

# 每个季度最后一天的数据 (3,6,9,12)
new_close = new_close.drop_duplicates(subset='Date',keep='last')

###############################################################################

for i in range(0,int(len(action_list)/2)) :
    
    # print (list(range(0,int(len(action_list)/2))))
    print(i+1)

    # 在量化投资研究中普遍采用后复权数据
    """
    后复权：保证历史价格不变，在每次股票权益事件发生后，调整当前的股票价格。 
    后复权价格和真实股票价格可能差别较大，不适合用来看盘。 
    其优点在于，可以被看作投资者的长期财富增长曲线，反映投资者的真实收益率情况。
    """
    pick_close = new_close.loc[:,['Date','Mois',stock_pick.iloc[i,0]]]
    
    # 财务报表-新浪
    # 接口: stock_financial_report_sina
    # 目标地址: https://vip.stock.finance.sina.com.cn/corp/go.php/vFD_BalanceSheet/stockid/600004/ctrl/part/displaytype/4.phtml
    # 描述: 新浪财经-财务报表-三大报表
    # 限量: 单次获取指定报表的所有年份数据
    stock_financial_report_sina_df = ak.stock_financial_report_sina(stock= action_list.iloc[i,0], symbol="现金流量表")
    
    if '单位' in stock_financial_report_sina_df.columns:
        stock_financial_report_sina_df = stock_financial_report_sina_df.drop(labels=['单位'],axis=1)  # axis=1 表示按列删除
    
    stock_financial_report_sina_df = drop_col(stock_financial_report_sina_df)
    
    # 计算每一列 数据为0 出现的频率
    zero_nombre = pd.DataFrame((stock_financial_report_sina_df == '0').astype(int).sum(axis=0)/(len(stock_financial_report_sina_df)))
    
    zero_nombre =  zero_nombre.T
    
    # 筛选出 0值出现频率小于 80% 的列
    z_cond = zero_nombre < 0.8
    
    z_List = []
    
    for a in range(0,zero_nombre.shape[1]) :
        if (z_cond.iloc[0,a] == True) :
           z_List.append(z_cond.columns[a])
    
    colonne =','.join(z_List)
    colonne = colonne.split(',')
    
    stock_financial_report_sina_df = stock_financial_report_sina_df.loc[:,colonne]
    
    # 修改列名
    stock_financial_report_sina_df.rename(columns={'报表日期':'Date'}, inplace = True)
    
    #将指定数据转为相应类型的datetime64型数据
    stock_financial_report_sina_df['Date'] = pd.to_datetime(stock_financial_report_sina_df ['Date'])
    stock_financial_report_sina_df.dtypes
    
    # 每个日期对应的月份单独作为一列
    stock_financial_report_sina_df['Mois'] = stock_financial_report_sina_df['Date'].dt.month 
    
    # 按季度筛选 [3,6,9,12]
    stock_financial_report_sina_df  = stock_financial_report_sina_df.loc[stock_financial_report_sina_df['Mois'].isin([3,6,9,12])]
    
    # 日期只显示年份和月份
    stock_financial_report_sina_df['Date'] = stock_financial_report_sina_df['Date'].apply(lambda x: x.strftime('%Y-%m')).values
    
    # 合并列表
    locals()['Action_'+str(i+1)] = pd.merge(pick_close,stock_financial_report_sina_df,
                    how='inner',
                    left_on=['Date','Mois'],
                    right_on=['Date','Mois']) 

#%%
for y in range(int(len(action_list)/2),len(action_list)) :
    
    # print (list(range(int(len(action_list)/2),len(action_list))))
    print(y+1)

    # 在量化投资研究中普遍采用后复权数据
    """
    后复权：保证历史价格不变，在每次股票权益事件发生后，调整当前的股票价格。 
    后复权价格和真实股票价格可能差别较大，不适合用来看盘。 
    其优点在于，可以被看作投资者的长期财富增长曲线，反映投资者的真实收益率情况。
    """
    pick_close = new_close.loc[:,['Date','Mois',stock_pick.iloc[y,0]]]
    
    # 财务报表-新浪
    # 接口: stock_financial_report_sina
    # 目标地址: https://vip.stock.finance.sina.com.cn/corp/go.php/vFD_BalanceSheet/stockid/600004/ctrl/part/displaytype/4.phtml
    # 描述: 新浪财经-财务报表-三大报表
    # 限量: 单次获取指定报表的所有年份数据
    stock_financial_report_sina_df = ak.stock_financial_report_sina(stock= action_list.iloc[y,0], symbol="现金流量表")
    
    if '单位' in stock_financial_report_sina_df.columns:
        stock_financial_report_sina_df = stock_financial_report_sina_df.drop(labels=['单位'],axis=1)  # axis=1 表示按列删除
    
    stock_financial_report_sina_df = drop_col(stock_financial_report_sina_df)
    
    # 计算每一列 数据为0 出现的频率
    zero_nombre = pd.DataFrame((stock_financial_report_sina_df == '0').astype(int).sum(axis=0)/(len(stock_financial_report_sina_df)))
    
    zero_nombre =  zero_nombre.T
    
    # 筛选出 0值出现频率小于 80% 的列
    z_cond = zero_nombre < 0.8
    
    z_List = []
    
    for b in range(0,zero_nombre.shape[1]) :
        if (z_cond.iloc[0,b] == True) :
           z_List.append(z_cond.columns[b])
    
    colonne =','.join(z_List)
    colonne = colonne.split(',')
    
    stock_financial_report_sina_df = stock_financial_report_sina_df.loc[:,colonne]
    
    # 修改列名
    stock_financial_report_sina_df.rename(columns={'报表日期':'Date'}, inplace = True)
    
    #将指定数据转为相应类型的datetime64型数据
    stock_financial_report_sina_df['Date'] = pd.to_datetime(stock_financial_report_sina_df ['Date'])
    stock_financial_report_sina_df.dtypes
    
    # 每个日期对应的月份单独作为一列
    stock_financial_report_sina_df['Mois'] = stock_financial_report_sina_df['Date'].dt.month 
    
    # 按季度筛选 [3,6,9,12]
    stock_financial_report_sina_df  = stock_financial_report_sina_df.loc[stock_financial_report_sina_df['Mois'].isin([3,6,9,12])]
    
    # 日期只显示年份和月份
    stock_financial_report_sina_df['Date'] = stock_financial_report_sina_df['Date'].apply(lambda x: x.strftime('%Y-%m')).values
    
    # 合并列表
    locals()['Action_'+str(y+1)] = pd.merge(pick_close,stock_financial_report_sina_df,
                    how='inner',
                    left_on=['Date','Mois'],
                    right_on=['Date','Mois'])

#%%
import os
os.chdir("C:/Users/c4780/Desktop/desktop/Quant/Action_chinoise_A")
os.getcwd()

for o in range(0,len(action_list)) :
    
    print(o+1)
    
    name = 'Action_'+ str(o+1) + '.csv'
    
    eval('Action_'+str(o+1)).to_csv(name,index=0)

#%%
import pandas as pd
import numpy as np

import os
os.chdir("C:/Users/c4780/Desktop/desktop/Quant/Action_chinoise_A")
os.getcwd()

# 读取数据
for u in range(0,87) :
    
    print(u+1)
    
    name = 'Action_'+ str(u+1) + '.csv'
    
    locals()['Action_'+str(u+1)] = pd.read_csv(name)

# 将所有数据框放到一个list当中 : 
Actions = [ eval('Action_'+str(1)),eval('Action_'+str(2)),eval('Action_'+str(3)),#1
        eval('Action_'+str(4)),eval('Action_'+str(5)),eval('Action_'+str(6)),#2
        eval('Action_'+str(7)),eval('Action_'+str(8)),eval('Action_'+str(9)),#3
        eval('Action_'+str(10)),eval('Action_'+str(11)),eval('Action_'+str(12)),#4  
        eval('Action_'+str(13)),eval('Action_'+str(14)),eval('Action_'+str(15)),#5
        eval('Action_'+str(16)),eval('Action_'+str(17)),eval('Action_'+str(18)),#6
        eval('Action_'+str(19)),eval('Action_'+str(20)),eval('Action_'+str(21)),#7
        eval('Action_'+str(22)),eval('Action_'+str(23)),eval('Action_'+str(24)),#8
        eval('Action_'+str(25)),eval('Action_'+str(26)),eval('Action_'+str(27)),#9
        eval('Action_'+str(28)),eval('Action_'+str(29)),eval('Action_'+str(30)),#10
        eval('Action_'+str(31)),eval('Action_'+str(32)),eval('Action_'+str(33)),#11
        eval('Action_'+str(34)),eval('Action_'+str(35)),eval('Action_'+str(36)),#12
        eval('Action_'+str(37)),eval('Action_'+str(38)),eval('Action_'+str(39)),#13
        eval('Action_'+str(40)),eval('Action_'+str(41)),eval('Action_'+str(42)),#14
        eval('Action_'+str(43)),eval('Action_'+str(44)),eval('Action_'+str(45)),#15
        eval('Action_'+str(46)),eval('Action_'+str(47)),eval('Action_'+str(48)),#16
        eval('Action_'+str(49)),eval('Action_'+str(50)),eval('Action_'+str(51)),#17
        eval('Action_'+str(52)),eval('Action_'+str(53)),eval('Action_'+str(54)),#18
        eval('Action_'+str(55)),eval('Action_'+str(56)),eval('Action_'+str(57)),#19
        eval('Action_'+str(58)),eval('Action_'+str(59)),eval('Action_'+str(60)),#20
        eval('Action_'+str(61)),eval('Action_'+str(62)),eval('Action_'+str(63)),#21
        eval('Action_'+str(64)),eval('Action_'+str(65)),eval('Action_'+str(66)),#22
        eval('Action_'+str(67)),eval('Action_'+str(68)),eval('Action_'+str(69)),#23
        eval('Action_'+str(70)),eval('Action_'+str(71)),eval('Action_'+str(72)),#24
        eval('Action_'+str(73)),eval('Action_'+str(74)),eval('Action_'+str(75)),#25
        eval('Action_'+str(76)),eval('Action_'+str(77)),eval('Action_'+str(78)),#26
        eval('Action_'+str(79)),eval('Action_'+str(80)),eval('Action_'+str(81)),#27
        eval('Action_'+str(82)),eval('Action_'+str(83)),eval('Action_'+str(84)),#28
        eval('Action_'+str(85)),eval('Action_'+str(86)),eval('Action_'+str(87))]#29

#  Actions[0]

# 分类器~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

from sklearn.model_selection import train_test_split
from sklearn import metrics

Best_stock_list = []

detail = []

for d in range(0,len(Actions)) :
    
    df = Actions[d].copy()
    
    # We have to make sure our calculations don’t take in data that technically had not occurred yet
    # To do this, we will lag the data
    lag_df = df.iloc[:,3:].shift(1)
    
    # Log Return
    lag_df['Returns'] = np.log(df.iloc[:,2]/df.iloc[:,2].shift(1))
    
    Label_List = []
    
    lag_df.dropna(inplace=True)
    
    # If returns are positive, it will be labelled 1, otherwise it will be labelled 0
    for e in lag_df['Returns']:
            
            if (e>=0):
                Label_List.append(1) #收益率为正得1分
                
            else :
                Label_List.append(0) #收益率为负不得分
                
    lag_df['Label'] = Label_List
    
    Model_Dict = {}
    
    X = np.array(lag_df.drop(columns=['Returns','Label']))

    y = np.array(lag_df['Label'])
    
    # 特征二值化
    # 根据阈值将数据二值化（将特征值设置为0或1）大于阈值的值映射到1，而小于或等于阈值的值映射到0
    # 默认阈值为0，只有正值映射到1
    from sklearn import preprocessing
    binarizer = preprocessing.Binarizer()
    X = binarizer.transform(X)
    
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 17)
    
    Model_Dict = {}
    Model_Dict['X Train'] = X_train
    Model_Dict['X Test'] = X_test
    Model_Dict['y Train'] = y_train
    Model_Dict['y Test'] = y_test

    from sklearn.naive_bayes import BernoulliNB
    
    model = BernoulliNB()
    
    model.fit(Model_Dict['X Train'], Model_Dict['y Train'])
    
    y_pred = model.predict(Model_Dict['X Test'])
    
    Model_Dict['y Prediction'] = y_pred
    
    prediction_length = len(Model_Dict['y Prediction'])
    
    pred_date = df['Date'][-prediction_length:]
    
    pred_date = pred_date.reset_index(drop=True)
 
    Model_Dict['Accuracy'] = metrics.accuracy_score(Model_Dict['y Test'], Model_Dict['y Prediction'])
    Model_Dict['Accuracy'] = round(Model_Dict['Accuracy'], 2)
     
    Model_Dict['Precision'] = metrics.precision_score(Model_Dict['y Test'], Model_Dict['y Prediction'],pos_label=1)
    Model_Dict['Precision']  = round(Model_Dict['Precision'] , 2)
    
    
    if Model_Dict['Accuracy'] >= 0.5 and Model_Dict['Precision'] >= 0.5 :
        
        pred = pd.DataFrame(y_pred)
        pred.rename(columns={0:'Score_prévision'},inplace=True)
        
        pred_date_code = pd.DataFrame(pred_date)
        
        pred_date_code['Code'] = df.columns[2]
       
        action_eval = pd.merge(pred_date_code,pred,
                             how='left',
                             left_index=True,
                             right_index=True)
        
        detail = detail + [action_eval]
        
        Score =  y_pred.sum()  # 预测季度收益率类型所得分数求和 
        
        info = df.columns[2] + " , Accuracy:" + str(Model_Dict['Accuracy']) + " , Precision:" + str(Model_Dict['Precision']) + ", Score:" + str(Score)
        
        Best_stock_list.append(info)

for g in range(0,len(detail)):
   locals()['Evaluation_'+str(g+1)] = detail[g]

Best_stock_df = pd.DataFrame(Best_stock_list)

Best_stock_df  = Best_stock_df [0].str.split(',',expand=True) 

# 修改列名 : 
Best_stock_df.rename(columns={0:'code', 1:'Accuracy', 2:'Precision', 3:'Score'}, inplace = True)

# 去掉代码后面的空格
Best_stock_df['code'] = Best_stock_df['code'].str.replace(" " , "")
# Best_stock_df.iloc[0,0]

Best_stock_df['Accuracy'] = Best_stock_df['Accuracy'].str[10:]
Best_stock_df['Precision'] = Best_stock_df['Precision'].str[11:]
Best_stock_df['Score'] = Best_stock_df['Score'].str[7:]

# Score 从字符串变为数字 :
Best_stock_df['Score'] = pd.to_numeric(Best_stock_df['Score'])

import tushare as ts
# 设置token
ts.set_token('49cbb8fa012ee0c2295a1b9da1b6c3a9bab7c45941579767d6daaf40')
# 初始化pro接口
pro = ts.pro_api()
#查询当前所有正常上市交易的股票列表
data = pro.stock_basic(exchange='', list_status='L', fields='ts_code,name,industry')
data.rename(columns={'ts_code':'code'}, inplace = True)
str_ = data['code'].str.split('.')

for s in range(0,len(data)) :
    p2 = str_[s][0:1]
    p1 = str_[s][1:2]
    p3 = p1+p2
    string = ''
    p3 = string.join(p3)
    data.iloc[s,0] = p3

# 合并 股票代码 : {交集}
StockA_42 = pd.merge(data,Best_stock_df,
                     how='inner',
                     left_on=['code'],
                     right_on=['code'])

# 按照得分 从大到小依次排序
StockA_42 = StockA_42.sort_values(by=['Score','Precision','Accuracy'],ascending=False)
StockA_42 = StockA_42.reset_index(drop=True)
 
# 解决pandas展示数据输出时列名不能对齐的问题
pd.set_option('display.unicode.ambiguous_as_wide', True)
pd.set_option('display.unicode.east_asian_width', True)
pd.set_option('display.width', 60)

print('\n')
print(Best_stock_df)
print('\n')
print(StockA_42)

#%%

# 满分12分 (12个季度, 2019-2021)

print(eval('Evaluation_' + str(22)))

print('\n', StockA_42.iloc[0,:])  


#%%

# 现代投资组合理论认为不同风险资产进行组合后,在保证投资收益的基础上可以有效地降低组合的风险。
# 以沪市上市公司为例,根据上市公司2001-2005年近五年来的市场表现,分析投资组合规模、风险和收益的关系。
# 通过研究发现:投资组合存在适度组合规模,组合规模过大会出现过度组合的问题;
# 组合规模的增加能够有效地降低非系统性风险,但在提高组合收益上效果并不明显。
# 证券组合的资产数量并不是越多越好，而是要恰到好处, 一般在15种到25种之间，可以达到证券组合的效能最大化
# https://global.cnki.net/kcms/detail/detail.aspx?dbcode=cjfd&dbname=cjfd2006&filename=SYJJ200627038

# Reasonable levels of diversification require as little as 25 securities

# 本项目的投资组合 : 25只股票

StockA_25= StockA_42.iloc[0:25,:]

print(StockA_25)

import os
os.chdir("C:/Users/c4780/Desktop/desktop/Quant/Action_chinoise_A")
os.getcwd()

StockA_25.to_csv('portefeuille.csv',index=0)

#%%
import pandas as pd 
import os
os.chdir("C:/Users/c4780/Desktop/desktop/Quant/Action_chinoise_A")
os.getcwd()

portefeuille_df = pd.read_csv('portefeuille.csv') 