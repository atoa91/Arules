# install.packages("visNetwork")
if(! "arules" %in% installed.packages()) install.packages("arules")
if(! "arulesViz" %in% installed.packages()) install.packages("arulesViz")

library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)

data<-read.csv("female_like.csv",stringsAsFactors =F,sep="\t",header=T)

names(data)
names(data)<-c("id","name")

str(data$name)
#make the data to the list for transfer to transaction with 'split'
lst <- split(data$name,data$id)
head(lst,1)


# 환경변수를 없애서 패키지 충돌로 인한 오류 방지용 함수. 
# 혹시 문제가 있을 시 이 함수를 적용 후, 다시 실행 권장. 
dev.off()

aggrData <- lst
listData <- list()

#중복 제거. 
for (i in 1:length(aggrData)) {
  listData[[i]] <- as.character(aggrData[[i]][!duplicated(aggrData[[i]])])
}

# user Id 별로 잘 들어 갔는지 확인해 봅시다. 
#2번째 사용자의 좋아하는 페이지 
listData[[2]]

# make transactions합니다. 
trans <- as(listData,"transactions")

#head로 데이터를 앞에 6개만 확인하려 하면, sparse format의 transactions이다라고만 나옵니다. 
head(trans)
#앞에서 의도한 대로 head를 쓰고 싶다면, trans 데이터는 inspect()함수로 확인해야 합니다.  
inspect(head(trans,2))

#dim()함수는 dimension의 줄임말로, 해당 객체의 차원 정보, 즉, 몇개의 columns와, rows로 이루어져 있는지 말해주는 함수입니다. 
#총 2262명의 사용자들의 좋아하는 page들은 75587개다라는 정보를 알 수 있습니다. 
dim(trans)

# 이게 무슨 말일까요? 앞에서 말씀드렸다시피, 객체의 차원이 그렇다면 75587 x 2262라는 건데요.
#transaction data의 형태는, (슬라이드) 이와 같이 말 그대로, sparse의 형태이기 때문에, '75587개의 columns와 2262개의 rows로 이루어진 sparse Matrix다.'라는 말입니다. 
#그렇기 때문에 head()함수로 transactions을 표현하기는 힘들기 때문에, inpect를 사용합니다. 그리고, 전체적인 객체의 형태를 알고 싶다면, summary()함수를 사용해서 데이터를 확인합니다. 
summary(trans) 

#----------------------------------------------------------------------------------


# 자, 이번엔 시각화를 통해서 데이터의 분포를 간단히 살펴 봅니다.
#check the most frequency of the items
#상위 30개 가장 많이 좋아요를 받은 페이지들을 살펴 볼까요?
# 페이지 이름이 너무 서로 겹치지 않도록 70%정도의 크기로 x축에 나오게 설정합니다.
itemFrequencyPlot(trans, topN=30, cex.names=.7)


# Mine Association Rules
# 자 그럼, 이제 본격적으로 apriori()함수를 사용해서, rule들을 추출해 봅니다.
# 아래의 설정은, trans 데이터를 가지고, parameter로 조건을 설정하는데, support를 5%이상의 출현, 그리고 rule의 크기는 'lhs, rhs를 합쳐서 2의 크기 (ex A -> B) 이상의 길이를 가진 규칙이면 다 뽑아라.'고 설정한 것입니다. 
r <- apriori(trans, parameter = list(supp=0.05,minlen=2))

#이렇게 생성한 규칙에서 support가 가장 높은 순서대로 top 15을 알아보겠습니다. 
inspect(head(sort(r,by="support",decreasing=T),n=15))

# 이 자료의 경우, 3%이상의 다 컨피던스가 높아요. 0.8. 즉, 80%이상이죠 왠만하면. 그래서 딱히 confidence에서 변별력을 찾을 수 없기때문에, 서포트에 중점을 두고 우리는 볼 필요가 있겠습니다. 
plot(r)


sub<-head(sort(r,by="lift"),n=10)
inspect(head(sort(r,by="lift"),n=10))
#plot(head(sort(r,by="lift"),n=10),method="paracoord",control=list(type="items"))

dd<-plot(head(sort(r,by="lift"),n=10),method="graph",control=list(type="items"))

#----------------------------------움직이는 시각화 -----------------------------------#
ig_df <- get.data.frame(dd, what="both")

inspect(head(sort(r,by="lift"),n=10))
#data preprocessing
ifelse(is.na(ig_df$vertices$support),0.00001,ig_df$vertices$support)
ig_df$vertices$support<-ifelse(is.na(ig_df$vertices$support),0.0001,ig_df$vertices$support)


visNetwork(
  nodes=data.frame(
    id=ig_df$vertices$name
    ,value=ig_df$vertices$support
    ,title=ifelse(ig_df$vertices$label=="",ig_df$vertices$name,ig_df$vertices$label)
    ,ig_df$vertices
  )
  ,edges = ig_df$edges
)%>%
  visEdges(arrows ="to")%>%
  visOptions(highlightNearest=T)




#plot(head(sort(r,by="support"),n=50)) 5%로는 몇명일까. 
y <- nrow(trans)*0.05
y