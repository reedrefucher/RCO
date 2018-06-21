install.packages("plyr")

# Generating table with order lines #

orderLines <- as.data.frame(matrix(nrow = 66, ncol = 3))   
colnames(orderLines) <- c("orderNumber", "productNumber", "units")
orderLines$orderNumber <-as.integer(c(21, 9,8, 14, 17, 15, 7, 16, 6, 19, 18, 5, 4, 3, 2, 1, 20, 13, 13, 13, 13, 13, 13, 13,
                                      12, 11, 10, 27, 11, 10, 27, 11, 10, 27, 11, 10, 27, 11, 10, 27, 11, 10, 27, 11, 10,
                                      27, 26, 25, 24, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 22, 23, 23, 23, 23))
orderLines$productNumber <-as.integer(c(5, 4, 3, 2, 2, 2, 2, 2, 1, 1, 1, 7, 7, 7, 7, 7, 8, 9, 6, 10,
                                        12, 11, 14, 13, 16, 15, 15, 15, 17, 17, 17, 21, 21, 21, 20, 20, 20, 19, 19, 19, 18, 18, 18, 
                                        23, 23, 23, 22, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 28, 27, 26, 25, 24))
orderLines$units <- as.integer(c(1, 10, 10 , 6, 8, 8, 10, 10, 10, 10, 10, 3, 3, 3, 3, 3, 8, 22, 22, 22, 22, 22, 44, 22, 2, 1,
                                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 2, 4, 1, 8, 2, 1, 2, 2, 2,
                                 2, 1, 1, 1, 1, 2, 1, 1, 1))

# Generating table with revenues generated from sale of one unit of each product #

productRevenues <- as.data.frame(matrix(nrow = 41, ncol = 2)) 
colnames(productRevenues) <- c("productNumber", "revenuePerUnit")
productRevenues$productNumber <- as.integer((1:41))
productRevenues$revenuePerUnit <- as.integer(c(4851, 2106, 6504, 530, 6500, 683, 3168, 3419, 1, 5839, 1329, 177, 1093, 843,
                                               0, 1202, 0, 16663, 11977, 0, 6309, 11, 0, 0, 76658, 208, 44, 5796, 339, 7096, 9997, 
                                               570, 531, 15999, 118, 232, 3438, 345, 720, 533, 1975))

# Putting them in one table and calculating revenue per order #

allData <- plyr::join(orderLines, productRevenues)
allData$revenue <- allData$units*allData$revenuePerUnit


######## Optimization aproach #########


# Data preparation #

RPO <- aggregate(revenue~orderNumber, allData, sum) # Calculating revenue per order 
n <- length(unique(allData$productNumber))
NO <- length(unique(allData$orderNumber))

OrdersHelper1 <- replicate(NO, list())
for (i in 1:NO) {
  OrdersHelper1[[i]] <- paste(subset(allData, orderNumber == RPO$orderNumber[i])$productNumber, collapse = "; ")
}
names(OrdersHelper1) <- RPO$orderNumber
OrdersHelper1 <- plyr:: ldply(OrdersHelper1, rbind)
OrderGroups <- levels(OrdersHelper1$`1`)
print(length(OrderGroups)) # 27 orders could be grouped into 15 unique combination of products. 
n <- length(OrderGroups)
colnames(OrdersHelper1) <- c("orderNumber", "simGroup")
OrdersHelper1$SimId <- as.numeric(OrdersHelper1$simGroup)
allData <- plyr:: join(allData, OrdersHelper1)
RPOGroup <-  aggregate(revenue~SimId, allData, sum) # Calculating revenue per products groups


OrdersHelper2 <- replicate(NO, list())
for (i in 1:NO) {
  OrdersHelper2[[i]] <- subset(allData, orderNumber == RPO$orderNumber[i])$productNumber
}


n <- length(unique(allData$productNumber))

ProductsOrders <- replicate(n, list())
for (i in 1:n) {
  ProductsOrders[[i]] <- subset(allData, productNumber ==i)$orderNumber # List of oders that require specific products 
}

ProductsMatrix <- matrix(ncol=n, nrow = n)
colnames(ProductsMatrix) <- 1:n
rownames(ProductsMatrix) <- 1:n
RevenueMatrix <- ProductsMatrix

for (i in 1:n) {
  for (j in 1:n){
    ProductsMatrix[i,j] <- min(ProductsOrders[[i]] %in% ProductsOrders[[j]])
    ProductsinOrders <- unique(subset(allData, orderNumber %in% c(ProductsOrders[[i]][ProductsOrders[[i]] %in% ProductsOrders[[j]]]))$productNumber)
    RevFromOrders <- RPO[ProductsOrders[[i]][ProductsOrders[[i]] %in% ProductsOrders[[j]]],2]
    RevenueMatrix[i,j] <- if(length(unique(subset(allData, productNumber==i)$SimId))==1)  
      {sum(RevFromOrders)} else { min (RevFromOrders)}
  }}

RevenueMatrix[RevenueMatrix==0] <- Inf
ProductsMatrix <- as.data.frame(ProductsMatrix)
ProductsMatrix$power <- rowSums(ProductsMatrix)
RevenuesProduct <- apply(RevenueMatrix,1, FUN = min) # The revenue that addition of minimum size product group that contain this product could bring
ProductsPowers <- as.data.frame(cbind(rownames(ProductsMatrix), ProductsMatrix$power, RevenuesProduct))
colnames(ProductsPowers) <- c("productNumber", "power", "revenuesProduct")
ProductsPowers$productNumber <- as.character(ProductsPowers$productNumber)
ProductsPowers$power <- as.numeric(as.character(ProductsPowers$power))
ProductsPowers$revenuesProduct <- as.numeric(as.character(ProductsPowers$revenuesProduct))
ProductsPowers$revenuesProduct[is.na(ProductsPowers$revenuesProduct)] <-0 
ProductsOrdersTable <- plyr:: ldply(ProductsOrders, rbind)
ProductsOrdersTable$code <- apply(ProductsOrdersTable, 1, function(x) paste(na.omit(x[1:ncol(ProductsOrdersTable)]), collapse = " "))  
ProductsOrdersTable$code <-as.numeric(as.factor(ProductsOrdersTable$code))
ProductsOrdersTable <- as.data.frame(cbind(1:nrow(ProductsOrdersTable), ProductsOrdersTable$code))
colnames(ProductsOrdersTable) <- c("productNumber", "productOrderGroup")
ProductsPowers <-plyr::join(ProductsPowers, ProductsOrdersTable)
ProductsPowers$productOrderGroup <- paste(ProductsPowers$revenuesProduct, ProductsPowers$productOrderGroup, sep="_")

## Calculation ## 

# First Iteration #

start <- min(ProductsPowers$power) # The minimum number of products required to fullfil at least one order
n <- length(unique(allData$productNumber))+start-1
Decisions <- as.data.frame(matrix(ncol=3, nrow = n))
colnames(Decisions) <- c("product", "revenues", "totalPower")
i= start
s <- subset(ProductsPowers, power <=start)
SelectedGroup <- unique(s[which(s$revenuesProduct == max(s$revenuesProduct)),"productOrderGroup"])[1]
Decisions$product[i] <- unique(paste(subset(s,productOrderGroup==SelectedGroup)$productNumber, 
                                     sep = " ", collapse = " "))
Decisions$revenues[i] <- max(s$revenuesProduct)
Decisions$totalPower[i] <- subset(s,productOrderGroup==SelectedGroup)$power[1]

# Main Cycle #


for (i in ((start+1):n)) {
  
  
  s <- subset(ProductsPowers, power <=max(i, start))
  prevprod <- unique(c(na.omit(unlist(strsplit(Decisions$product[i-1], split=" ")))))
  s <- s[-which(s$productNumber %in% prevprod),]
  SelectedGroup <- unique(s[which(s$revenuesProduct == max(s$revenuesProduct)),"productOrderGroup"])[1]
  decprod <- unique(paste(Decisions$product[i-1],subset(s,productOrderGroup==SelectedGroup)$productNumber, 
                          sep = " ", collapse = " "))
  decprod <- paste(unique(c(na.omit(unlist(strsplit(decprod, split=" "))))), sep =" ", collapse = " ")
  thisroundprod <- subset(s,productOrderGroup==SelectedGroup)$productNumber
  SimIDS1 <- unique(allData$SimId[which(allData$productNumber %in%  as.numeric(unlist(strsplit(decprod, split=" "))))])
  Decisions$product[i] <- decprod
  curprod <- unique(c(na.omit(unlist(strsplit(Decisions$product[i], split=" ")))))
  NewPower <- length(subset(ProductsPowers, productNumber %in% as.character(thisroundprod))$power)     
  Decisions$totalPower[i] <- length(subset(ProductsPowers, productNumber %in% prevprod)$power)+NewPower                          
  Decisions$revenues[i] <- sum(subset(RPOGroup, SimId %in% SimIDS1))

  
  if (Decisions$totalPower[i] > i) {
    repeat {
      Decisions$totalPower[i] <- length(subset(ProductsPowers, productNumber %in% prevprod)$power)+
        length(subset(ProductsPowers, productNumber %in% as.character(thisroundprod))$power)
      PowerDif <- Decisions$totalPower[i]-Decisions$totalPower[i-1]
      if ((i-PowerDif) <=0) {thisroundprod2 <- thisroundprod} else {
        thisroundprod2 <-  paste(Decisions$product[max(which(Decisions$totalPower<=(i-PowerDif)))],
                                 paste(unique(c(na.omit(unlist(strsplit(thisroundprod, split=" "))))),
                                       sep =" ", collapse = " "), sep = " ", collapse = " ")}
      NewRev<-as.numeric(sum(plyr::ldply(OrdersHelper2,function(x) floor(mean(x %in% as.numeric(unlist(strsplit(
        thisroundprod2, split=" "))) )))*RPO$revenue))
    PowerDif <- Decisions$totalPower[i]-Decisions$totalPower[i-1]
    margin <- (if(mean(s$power==1)>0 & PowerDif>1) {max(subset(s, power==1)$revenuesProduct)}) 
    margin <-ifelse(is.null(margin),0,margin)
    OldRev <-  Decisions$revenues[i-1]+ margin
    if(NewRev>=OldRev) {
      Decisions$product[i] <- paste(unique(c(na.omit(unlist(strsplit(thisroundprod2, split=" "))))), sep =" ", collapse = " ")
      Decisions$totalPower[i] <- length(unlist(strsplit( Decisions$product[i],split=" ")))
      Decisions$revenues[i] <-NewRev
    } else { 
     s <- s[-which(s$productNumber %in% thisroundprod),]
     if(dim(s)[1]==0) {
       Decisions$product[i] <- Decisions$product[i-1]
       Decisions$totalPower[i] <- length(unlist(strsplit( Decisions$product[i],split=" ")))
       Decisions$revenues[i] <- Decisions$revenues[i-1]
     } else {
     SelectedGroup <- unique(s[which(s$revenuesProduct == max(s$revenuesProduct)),"productOrderGroup"])[1]
     thisroundprod <- subset(s,productOrderGroup==SelectedGroup)$productNumber
     }  
     }
      if (Decisions$totalPower[i] <= i){
        break
      }
  }
  }
}

Decisions <- na.omit(Decisions)
Decisions$iteration <- 1:n 
write.csv2(Decisions, "OPT.csv", row.names = F)

## Visualization ##

install.packages("plotly")
library(plotly)
library(plyr)
plot_ly(data = Decisions, x =~ totalPower, y=~ round(revenues/max(revenues)*100,0), type = 'scatter', mode = 'lines') %>%
  layout(yaxis = list(title = 'Revenue Share', titlefont=list(
    family='Arial, sans-serif',
    size=24), tickfont=list(size=24)
    )) %>%
  layout(xaxis = list(title = 'Number of Products', titlefont=list(
    family='Arial, sans-serif',
    size=24), tickfont=list(size=24)
  )) %>%
  layout(yaxis = list(ticksuffix = "%")) %>%
  layout(margin = list(l = 100, r = 50, b = 60, t = 0, pad = 4))
  

SplitList <- strsplit(Decisions$product, split = " ")
k <- lengths(SplitList)    ## expansion size
FlatColumn <- unlist(SplitList, use.names = FALSE)
Decisions2 <- data.frame(iteration = rep.int(Decisions$iteration, k),
                      revenues = rep.int(Decisions$revenues, k),
                      product = FlatColumn)
Decisions2$iteration <- as.factor(Decisions2$iteration)

order1 <- as.data.frame(levels(Decisions2$product))
colnames(order1) <- "level"
order1$id <- as.integer(as.character(order1$level))
order1$rowname <- as.numeric(rownames(order1))
order1 <- order1[with(order1, order(id,decreasing = F)), ]
Decisions2$product <- factor(Decisions2$product,levels(Decisions2$product)[c(order1$rowname)])

plot_ly(Decisions2, x=~iteration, y=~product, z=~revenues, showscale=FALSE) %>% 
  add_histogram2d() %>% 
  layout(xaxis = list(title="Iteration", titlefont=list(
    family='Arial, sans-serif',
    size=24), tickfont=list(size=18)
  ), yaxis = list(title ="Product", titlefont=list(
    family='Arial, sans-serif',
    size=24), tickfont=list(size=18))) %>%
  layout(margin = list(l = 80, r = 50, b = 80, t = 0, pad = 4)) 
  

