# -*- coding: utf-8 -*-
"""
Created on Thu Apr 30 20:56:10 2020

@author: MSRBCEL
"""

from mypython import get_data_set,write_out_dataset
import datetime

#This function assumes that the records are sorted by ID.
def process_chess_duplicates(inputfile):
    currentid=0
    duprecs=[]
    dupids=[]
    finalfile=[]
    dataset=get_data_set(inputfile+'.csv',True)
    chess=add_extra_variables(dataset[1])
    h=dataset[0]
    header=[h[0],"obsid","serialid","clerical"]+h[1:len(h)]
    print(header)
    global dah, dai, sbd, edo, dli, tnm, caseid
    dah=header.index('hospitaladmissiondate')
    dai=header.index('dateadmittedicu')
    sbd=header.index('sbdate')
    edo=header.index('estimateddateonset')
    dli=header.index('dateleavingicu')
    tnm=header.index('trustname')
    caseid=header.index('caseid')
    #hoai=header.index
    #print (header)
    #print(chess[58])
    #print(len(chess))
    for record in chess:    
        record[tnm]=record[tnm].replace(","," ")
    
    for record in chess: 
        if record[caseid]==currentid:
            if currentid not in dupids:
                dupids=dupids+[currentid]
        else:
            currentid=record[caseid] 

    for record in chess:
        if record[caseid] in dupids:
            duprecs=duprecs+[record]
        else:
            finalfile=finalfile+[record]
                
    for cid in dupids:
        dupset=[]
        for record in duprecs:
            if record[caseid]==cid:
                dupset=dupset+[record]
        out=process_dupset(dupset)
        finalfile=finalfile+out
 
    finalfile=[header]+addobsid(finalfile)
    write_out_dataset(inputfile+' deduplicated.csv',finalfile)       
                
def addobsid(indataset):
    currentcaseid=0
    currentobsid=1
    outdataset=[]
    for record in indataset:
        if record[caseid]==currentcaseid:
            currentobsid=currentobsid+1
            record[1]=currentobsid
        else:
            currentcaseid=record[caseid]
            currentobsid=1
        outdataset.append(record)  
    return(outdataset)
            
    
def process_dupset(dupset):
    if len(dupset)==2:
        return(process_duplicate_pair(dupset))
    elif len(dupset)==4 and is_crossover_pattern(dupset):
        return(process_duplicate_pair([dupset[0],dupset[2]]))
    else:
        return(add_clerical_indicator(dupset))
    
def is_crossover_pattern(dupset):
    return(dupset[0][dah]==dupset[2][dah] and dupset[1][dah]==dupset[3][dah] and chess_earlier(dupset[0][dah],dupset[1][dah]))

def add_clerical_indicator(inset):
    outset=[]        
    for record in inset:
        outset.append(record[0:3]+["X"]+record[4:len(record)])
    return(outset)

def process_duplicate_pair(dupset):
    r1,r2=dupset[0],dupset[1]
        
    if r1[dai]==r2[dai]:
        if r1[dah]==r2[dah]:
            if chess_earlier(r1[sbd],r2[sbd]):
                return ([r1])
            else:
                return([r2])
        else:    
            if chess_earlier(r1[dah],r2[dah]):
                return ([r1])
            else:
                return([r2])
    elif r1[dai]=='':
        return([r2])
    elif r2[dai]=='':
        return([r1])
    elif contiguous_icu_spells(r1,r2):
        return([merge_contiguous_records(r1,r2)])
    else: 
        return(dupset) # this might cause issues if we want to use this recrsively on larger dupsets.
    
def chess_earlier(d1,d2):
    if d1=='':
        return(False)
    elif d2=='':
        return(True)
    else:
        return(datetime.datetime.strptime(d1, '%d/%m/%Y') < datetime.datetime.strptime(d2, '%d/%m/%Y'))

def chess_earliest(d1,d2):

    if d1=='':
        return(d2)
    elif d2=='':
        return(d1)
    elif datetime.datetime.strptime(d1, '%d/%m/%Y') < datetime.datetime.strptime(d2, '%d/%m/%Y'):
        return(d1)
    else:
        return(d2)

def chess_later(d1,d2):
    if d1=='':
        return(False)
    elif d2=='':
        return(True)
    else:
        return(datetime.datetime.strptime(d1, '%d/%m/%Y') > datetime.datetime.strptime(d2, '%d/%m/%Y'))

def chess_latest(d1,d2):
    if d1=='':
        return(d2)
    elif d2=='':
        return(d1)
    elif datetime.datetime.strptime(d1, '%d/%m/%Y') > datetime.datetime.strptime(d2, '%d/%m/%Y'):
        return(d1)
    else:
        return(d2)

def contiguous_icu_spells(r1,r2):
    return(r1[dai]==r2[dli] or r1[dli]==r2[dai])

def merge_contiguous_records(r1,r2):
    m=r1
    #could add some other dates in if helpful
    m[dah]=chess_earliest(r1[dah],r2[dah])
    if r1[dli]=='' or r2[dli]=='':
        m[dli]=''
    else:
        m[dli]=chess_latest(r1[dli],r2[dli])
    m[dai]=chess_earliest(r1[dai],r2[dai])       
    return(m)

def add_extra_variables(indataset):
    outdataset=[]
    count=1
    for record in indataset:
        outdataset.append([record[0],1,count,""]+record[1:len(record)])
        count+=1
    return(outdataset) 

process_chess_duplicates('CHESS COVID19 CaseReport 20200512')       
 

    