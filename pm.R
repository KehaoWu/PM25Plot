library(rworldmap)
library(XML)
StartDate = "2015-01-01"
EndDate = as.character(format(Sys.time(),"%Y-%m-%d"))

if(!file.exists("pm.RData"))
{
  ###Set date like yyyy-mm-dd

  
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
  save(list=c("pm"),file = "pm.RData")
  
} else {
  cat("Loading pm.RData ...\n  ")
  load("pm.RData")
}


if(!file.exists("pm.CityCorrelate.RData"))
{
  cities = unique(pm$city)
  dates = unique(pm$date)
  data = matrix(NA,ncol = length(cities),nrow=length(dates))
  
  for(i in 1:nrow(pm))
  {
    ColIndex = match(as.character(pm$city[i]),cities)
    RowIndex = match(as.character(pm$date[i]),dates)
    data[RowIndex,ColIndex] = pm$pm[i]
  }
  
  city1 = NULL
  city2 = NULL
  R2 = NULL
  for(i in 1:(length(cities)-1))
  {
    for(j in (i+1):length(cities))
    {
      city1 = c(city1,i)
      city2 = c(city2,j)
      trait = data.frame(data[,i],data[,j])
      trait = trait[complete.cases(trait),]
      cat(i,j,"\n")
      if(nrow(trait)>=30)
        R2 = c(R2,cor(trait[,1],trait[,2])) else
          R2 = c(R2,0)
    }
  }
  save(list=c("cities","city1","city2","R2"),file = "pm.CityCorrelate.RData")
} else {
  cat("Loading pm.CityCorrelate.RData ...\n  ")
  load("pm.CityCorrelate.RData")
}

pmNetwork = function(cityInclude,cities,city1,city2,R2,cutoff=0.5,StartDate,EndDate)
{
  cityInculde = read.table("Province.txt",header = T)[,3]
  cityInculde = gsub(pattern = "\\s",replacement = "",cityInculde)
  cityIncludeIndex = match(cityInculde,cities)
  cat("Delete cities",cityInculde[is.na(cityIncludeIndex)],"because of lack of data ...\n")
  cityInclude = cityInculde[!is.na(cityIncludeIndex)]
  cityIncludeIndex = cityIncludeIndex[!is.na(cityIncludeIndex)]
  
  city1Child = city1[city1 %in% cityIncludeIndex]
  city2Child = city2[city1 %in% cityIncludeIndex]
  R2Child = R2[city1 %in% cityIncludeIndex]
  city1Child = city1Child[city2Child %in% cityIncludeIndex]
  R2Child = R2Child[city2Child %in% cityIncludeIndex]
  city2Child = city2Child[city2Child %in% cityIncludeIndex]
  
  
  
  
  data = data.frame(city1=city1Child,city2=city2Child,R2=R2Child)
  data = data[complete.cases(data),]
  data = data[abs(data$R2)>=cutoff,]
  
  CitySet = unique(c(data[,1],data[,2]))
  CityLabelSet = as.character(cities)[CitySet]
  data$city1 = match(data$city1,CitySet)
  data$city2 = match(data$city2,CitySet)
  g = graph(as.vector(rbind(data$city1,data$city2)),directed = T)
  E(g)$arrow.mode = 0
  direction = ifelse(data$R2>=0,1,2)
  data$R2 = abs(data$R2)
  lwd = (data$R2 - min(data$R2))/(max(data$R2)-min(data$R2)) * 4 + 4
  E(g)$width = lwd
  E(g)$color = heat.colors(10)[10-floor(data$R2*10)]
  E(g)$label = round(data$R2,digits = 2)
  E(g)$label.color = "slategrey"
  E(g)$label.size = 0.8
  
  V(g)$color = "white"
  V(g)$frame.color = "white"
  V(g)$label = CityLabelSet
  V(g)$label.color = "black"
  plot(g,layout = layout.fruchterman.reingold)

  title(main = paste("全国省会城市之间pm2.5相关系数网络关系图 R2>=",cutoff,sep=""),
        sub=paste("收据收集时间段",StartDate,"至",EndDate," @conda",sep=""))
  g
}
jpeg("pm2.5Network.2015.%d.jpeg",width = 1000,height = 1000,quality = 100)
g = pmNetwork(cityInclude,cities,city1,city2,R2,cutoff=0,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,cutoff=0.1,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,cutoff=0.2,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,cutoff=0.3,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,cutoff=0.4,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,cutoff=0.5,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,cutoff=0.6,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,cutoff=0.7,StartDate,EndDate)
g = pmNetwork(cityInclude,cities,city1,city2,R2,cutoff=0.8,StartDate,EndDate)
dev.off()