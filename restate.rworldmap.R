library(rworldmap)
library(XML)

website = "http://www.creprice.cn/rank/cityforsale.html"
web = readLines(website,encoding="UTF-8")
web = paste(web,collapse = "")
html = htmlParse(web,encoding="UTF-8")
nodes = getNodeSet(html,"//tbody[@id='order_f']/tr/td")
values = sapply(nodes,FUN = xmlValue)
values = gsub(pattern = "\\s+|,",replacement = "",x = values,perl = T)

realEstatePrice = matrix(values,ncol = 5,byrow = T)
realEstatePrice = realEstatePrice[,c(-1,-4)]
realEstatePrice = data.frame(City=realEstatePrice[,1],
                             Price=as.numeric(realEstatePrice[,2]))

if(version$os=="mingw32")
{
  ChinaLocation = read.csv(file = "China.Cities.Location.Win.csv")
}else
  ChinaLocation = read.csv(file = "China.Cities.Location.Linux.csv")

ChinaLocationRE = unlist(lapply(ChinaLocation[,1],FUN = function(x)gsub(pattern = "市|县",replacement = "",perl = T,x=x)))
CityLocation = ChinaLocation[match(x = realEstatePrice[,1],table = ChinaLocationRE),]

REPrice = cbind(CityLocation,realEstatePrice)

priceMax = max(REPrice$Price)
col = heat.colors(priceMax)
REPrice = data.frame(REPrice,color=col[REPrice$Price],size=100)
mapBubbles(dF = REPrice,
           symbolSize = .3,
           oceanCol = "skyblue",
           borderCol = "grey",
           landCol = "lightgreen",
           nameX = "GPSLontitude",
           nameY = "GPSLatitude",
           mapRegion = "China",
           nameZSize = "size",
           nameZColour = "color",
           addLegend = F,
           addColourLegend = F)
addMapLegend(cutVector = seq(from = min(REPrice$Price),to = max(REPrice$Price),length.out = 11),
             colourVector = col[seq(from = max(REPrice$Price),to = min(REPrice$Price),length.out = 10)],
             horizontal = F,labelFontSize=0.8,legendWidth=0.5)
title(main=paste("全国",nrow(REPrice),"个主要城市平均房价分布情况",sep=""))
title(sub=paste("数据来源：","http://www.creprice.cn/ drawed by R author:Conda",sep=""),col.sub="grey")
