library(RgoogleMaps)
library(RCurl)
library(XML)
Start = Sys.time()

cat("Obtaining data ...\n")
website = paste("http://datacenter.mep.gov.cn/report/air_daily/air_dairy.jsp?city=&startdate=2015-02-02&enddate=2015-02-02&page=",1,sep = "")
pmweb = readLines(website,encoding="UTF-8")
pmweb = paste(pmweb,collapse = "")
pmhtml = htmlParse(pmweb,encoding="UTF-8")
totalPages = as.numeric(xmlValue(getNodeSet(pmhtml,"//font[@color='#004e98']")[[2]]))
cat("\tThere are",totalPages-1,"pages need to be downloaded ...\n")
pmtotal = getNodeSet(pmhtml,"//table/tr/td")
pmvalue = sapply(X=pmtotal,FUN=xmlValue)
pmvalue = pmvalue[(match(x = "首要污染物",table = pmvalue)+1):((1:length(pmvalue))[grepl(pattern = "记录总数",x = pmvalue)]-1)]
pm = matrix(pmvalue,byrow = T,ncol = 6)
pm = data.frame(city=pm[,2],date=pm[,3],pm=as.numeric(pm[,4]),type=pm[,6],degree=pm[,5])
pb = txtProgressBar(min = 1,max = totalPages,style = 3)
if(totalPages>=2)
{
  for(page in 2:totalPages )
  {
    cat("Obtaining data for page",page,"...\n")
    website = paste("http://datacenter.mep.gov.cn/report/air_daily/air_dairy.jsp?city=&startdate=2015-02-02&enddate=2015-02-02&page=",page,sep = "")
    pmweb = readLines(website,encoding="UTF-8")
    pmweb = paste(pmweb,collapse = "")
    pmhtml = htmlParse(pmweb,encoding="UTF-8")
    pmtotal = getNodeSet(pmhtml,"//table/tr/td")
    pmvalue = sapply(X=pmtotal,FUN=xmlValue)
    pmvalue = pmvalue[(match(x = "首要污染物",table = pmvalue)+1):((1:length(pmvalue))[grepl(pattern = "记录总数",x = pmvalue)]-1)]
    pm.new = matrix(pmvalue,byrow = T,ncol = 6)
    pm.new = data.frame(city=pm.new[,2],date=pm.new[,3],pm=as.numeric(pm.new[,4]),type=pm.new[,6],degree=pm.new[,5])
    pm = rbind(pm,pm.new)
  }
}
ChinaLocation = read.csv(file = "China.Cities.Location.csv")

pmCityLocation = ChinaLocation[match(x = pm[,1],table = ChinaLocation[,1]),]
pmCity = cbind(pm,pmCityLocation)
pmCity = pmCity[complete.cases(pmCity),]
lon = pmCity$GPSLontitude
lat = pmCity$GPSLatitude
pm = pmCity$pm

tp.pm = GetMap.bbox(lonR=lon,latR=lat)
pm.max = max(pm)
red = seq(from=0,to=1,length.out=pm.max)
green = seq(from=1,to=0,length.out=pm.max)
blue = seq(from=0,to=0,length.out=pm.max)
col = rgb(red=red,green=green,blue=blue,alpha=0.5)
col = col[pm]
cex = pm/100 + 5
suppressWarnings(expr=PlotOnStaticMap(MyMap=tp.pm,lat=lat,lon=lon,pch=20,col=col,cex=cex,mar=c(0,0,2,0)))
title(main=paste(date,"全国",length(pm),"个主要城市PM2.5分布情况",sep=""))
text.col = rgb(red=1,green=1,blue=1,alpha=0.9)
text(x=210,y=-250,labels=paste("数据来源：","中华人民共和国环境保护部数据中心",sep=""),cex=0.8,col=text.col)
text(x=210,y=-230,labels="R语言  RgoogleMaps  @Conda",cex=0.8,col=text.col)
Stop = Sys.time()
Cost = Stop - Start
cat(paste("\n\n\nIt cost",Cost,"minutes.\n"))
