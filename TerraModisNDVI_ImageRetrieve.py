#!/usr/bin/env python

#imports
import os
import sys
from glob import glob

import matplotlib.pyplot as plt
import numpy as np

from modisWSDL import *

#function to retrieve MODIS NDVI image for given area and date
def getImage( shortName, lat, lon, startDate, endDate, imSize ):

  if os.path.isfile(shortName+'.pkl')==False:
    
    client=setClient( )
  
    #get modis data:
    m=modisClient( client,\
      product=modisDict[shortName]['product'],\
      band=modisDict[shortName]['band'],\
      lat=lat,\
      lon=lon,\
      startDate=int(startDate),\
      endDate=int(endDate), \
      kmAboveBelow=imSize, \
      kmLeftRight=imSize )
   
    #get QA data:
    if modisDict[shortName]['QA']!=None:
      modisGetQA(m, modisDict[shortName]['QA'])
    
      #print m.kmLeftRight, m.kmAboveBelow
      #print np.shape(m.data)
      #print np.shape(m.QA)

      #apply QA
      m.filterQA(np.arange(0,2**16,2), fill=-9999)  


    m.applyScale()
    
    return m


if __name__=="__main__":

  import matplotlib.pyplot as plt
  from datetime import datetime, timedelta

  def dateIsOK(year,day,dataName):
    if dataName == "ndviT":
      if year==2000 and day<55:
        return False
    if dataName == "ndviA":
      if year==2000 or year==2001 or year==2002 and day<185:
        return False
    return True

  modisDict={}
  modisDict['ndviT'] = {'product':'MOD13Q1','band':'250m_16_days_EVI','QA':'250m_16_days_VI_Quality'}

  imSize=100
  pixSize=250
  stepSize=16
  
  realPixSize=926.62543305/4.
  
  lat=-2.651337
  lon=37.358667

  nX=imSize*2*1000./pixSize+1
  
  #download the modis data
  for dataName in modisDict:
    for year in xrange(200,2017): 
      for day in xrange(1,365,stepSize):
   
        if dateIsOK(year,day,dataName)==False:
          continue
       
        startDate="%d%03d"%(year, day)
        endDate=str(int(startDate)+stepSize)  

	print startDate, dataName
        
        fName='m'+startDate+dataName+'.asc'
        f=open(fName,'w')

        modis=getImage(dataName, lat, lon, startDate, endDate, imSize=imSize)  
        #print np.shape(modis.data)
        modis.data=np.reshape(modis.data[0], [nX,nX])      
        print >> f,'NCOLS',nX
        print >> f,'NROWS',nX
        print >> f,'XLLCORNER', modis.xllcorner
        print >> f,'YLLCORNER', modis.yllcorner
        print >> f,'CELLSIZE',realPixSize
        print >> f,'NODATA_VALUE',-0.9999
        #np.savetxt('m'+startDate+dataName+'.csv', modis.data,fmt="%0.4f", delimiter=',')
        for i in xrange(int(nX)):
          for j in xrange(int(nX)):
            print >> f, modis.data[i,j],
          print >> f, ""

        f.close()
        

