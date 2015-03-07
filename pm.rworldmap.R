library(rworldmap)
library(XML)

###Set date like yyyy-mm-dd
StartDate = format(Sys.time() - 86400 ,"%Y-%m-%d")
EndDate = format(Sys.time() - 86400 ,"%Y-%m-%d")

Start = Sys.time()
cat("Obtaining data ...\n")
website = paste("http://datacenter.mep.gov.cn/report/air_daily/air_dairy.jsp?city=&startdate=",StartDate,"&enddate=",EndDate,"&page=",1,sep = "")
pmweb = readLines(website,encoding="UTF-8")
pmweb = paste(pmweb,collapse = "")
pmhtml = htmlParse(pmweb,encoding="UTF-8")
totalPages = as.numeric(xmlValue(getNodeSet(pmhtml,"//font[@color='#004e98']")[[2]]))
cat("\tThere are",totalPages-1,"pages need to be downloaded ...\n")
pmtotal = getNodeSet(pmhtml,"//table/tr/td")
pmvalue = sapply(X=pmtotal,FUN=xmlValue)
pmvalue = pmvalue[-1:(-match(x = "首要污染物",table = pmvalue))]
pmvalue = pmvalue[1:((1:length(pmvalue))[grepl(pattern = "记录总数",x = pmvalue)]-1)]
pm = matrix(pmvalue,byrow = T,ncol = 6)
pm = data.frame(city=pm[,2],date=pm[,3],pm=as.numeric(pm[,4]),type=pm[,6],degree=pm[,5])
if(totalPages>=2)
{
  for(page in 2:totalPages )
  {
    cat("Obtaining data for page",page,"...\n")
    website = paste("http://datacenter.mep.gov.cn/report/air_daily/air_dairy.jsp?city=&startdate=",StartDate,"&enddate=",EndDate,"&page=",page,sep = "")
    pmweb = readLines(website,encoding="UTF-8")
    pmweb = paste(pmweb,collapse = "")
    pmhtml = htmlParse(pmweb,encoding="UTF-8")
    pmtotal = getNodeSet(pmhtml,"//table/tr/td")
    pmvalue = sapply(X=pmtotal,FUN=xmlValue)
    pmvalue = pmvalue[-1:(-match(x = "首要污染物",table = pmvalue))]
    pmvalue = pmvalue[1:((1:length(pmvalue))[grepl(pattern = "记录总数",x = pmvalue)]-1)]
    pm.new = matrix(pmvalue,byrow = T,ncol = 6)
    pm.new = data.frame(city=pm.new[,2],date=pm.new[,3],pm=as.numeric(pm.new[,4]),type=pm.new[,6],degree=pm.new[,5])
    pm = rbind(pm,pm.new)
  }
}

if(version$os=="mingw32")
{
  ChinaLocation = read.csv(file = "China.Cities.Location.Win.csv")
}else
  ChinaLocation = read.csv(file = "China.Cities.Location.Linux.csv")

pmCityLocation = ChinaLocation[match(x = pm[,1],table = ChinaLocation[,1]),]
pmCity = cbind(pm,pmCityLocation)
pmCity = pmCity[complete.cases(pmCity),]

pm.max = max(pmCity$pm)
red = seq(from=0,to=1,length.out=pm.max)
green = seq(from=1,to=0,length.out=pm.max)
blue = seq(from=0,to=0,length.out=pm.max)
col = rgb(red=red,green=green,blue=blue,alpha=0.5)
cex = pmCity$pm/100 + 5

pmCity = data.frame(pmCity,color=col[pmCity$pm])
mapBubbles(dF = pmCity,
           oceanCol = "skyblue",
           borderCol = "grey",
           landCol = "lightgreen",
           nameX = "GPSLontitude",
           nameY = "GPSLatitude",
           nameZSize = "pm",
           mapRegion = "China",
           nameZColour = "color",
           addColourLegend = F)

title(main=paste(StartDate,"-",EndDate,"全国",nrow(pmCity),"个主要城市PM2.5分布情况",sep=""))
title(sub=paste("数据来源：","中华人民共和国环境保护部数据中心 drawed by R author:Conda",sep=""),col.sub="grey")
Stop = Sys.time()
Cost = Stop - Start
cat(paste("\n\n\nIt cost",Cost,"minutes.\n"))


