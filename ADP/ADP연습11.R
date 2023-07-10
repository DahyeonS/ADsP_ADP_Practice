# 텍스트마이닝
library(tm)
data(crude, package = "tm")
m = TermDocumentMatrix(crude, control = list(removePunctuation = T, stopwords = T))
inspect(m) # 단어별 문서에서 나오는 횟수
# <<TermDocumentMatrix (terms: 1000, documents: 20)>>
# Non-/sparse entries: 1738/18262
# Sparsity           : 91%
# Maximal term length: 16
# Weighting          : term frequency (tf)
# Sample             :
#         Docs
# Terms    144 236 237 242 246 248 273 489 502 704
# bpd      4   7   0   0   0   2   8   0   0   0
# crude    0   2   0   0   0   0   5   0   0   0
# dlrs     0   2   1   0   0   4   2   1   1   0
# last     1   4   3   0   2   1   7   0   0   0
# market   3   0   0   2   0   8   1   0   0   2
# mln      4   4   1   0   0   3   9   3   3   0
# oil     12   7   3   3   5   9   5   4   5   3
# opec    13   6   1   2   1   6   5   0   0   0
# prices   5   5   1   2   1   9   5   2   2   3
# said    11  10   1   3   5   7   8   2   2   4
findFreqTerms(m, 10) # 10개 이상 사용된 단어를 표시
# [1] "barrel"     "barrels"    "bpd"        "crude"     
# [5] "dlrs"       "government" "industry"   "kuwait"    
# [9] "last"       "market"     "meeting"    "minister"  
# [13] "mln"        "new"        "official"   "oil"       
# [17] "one"        "opec"       "pct"        "price"     
# [21] "prices"     "production" "reuter"     "said"      
# [25] "saudi"      "sheikh"     "will"       "world"
findAssocs(m, "oil", 0.7) # "oil" 단어와 70% 이상 연관성이 있는 단어를 표시
# $oil
#     158      opec     named   clearly      late    prices    trying    winter 
#    0.87      0.87      0.81      0.79      0.79      0.79      0.79      0.79 
# markets      said  analysts agreement emergency    buyers     fixed 
#    0.78      0.78      0.77      0.76      0.74      0.71      0.71

# 소셜 네트워크 분석
library(igraph)
data(studentnets.M182, package = "NetData")
m182_full_nonzero_edges <- subset(m182_full_data_frame, (friend_tie > 0 | social_tie > 0 | task_tie > 0))
m182_full <- graph.data.frame(m182_full_nonzero_edges) 
m182_friend <- delete.edges(m182_full, E(m182_full)[get.edge.attribute(m182_full,name = "friend_tie")==0])
m182_friend_und <- as.undirected(m182_friend, mode='collapse')
m182_friend_no_iso <- delete.vertices(m182_friend_und, V(m182_friend_und)[degree(m182_friend_und)==0])

friend_comm_wt = walktrap.community(m182_friend_no_iso, step = 200, modularity = T)
friend_comm_wt # 커뮤니티 수를 측정
# IGRAPH clustering walktrap, groups: 4, mod: 0.19
# + groups:
#   $`1`
#   [1] "1"  "3"  "6"  "9"  "10" "11" "15"
# 
#   $`2`
#   [1] "2"  "7"  "8"  "13" "14"
# 
#   $`3`
#   [1] "5"
# 
#   $`4`
#   + ... omitted several groups/vertices
friend_comm_dend = as.dendrogram(friend_comm_wt, use.modularity = T)
plot(friend_comm_dend)

friend_comm_eb = edge.betweenness.community(m182_friend_no_iso)
friend_comm_eb # 최단거리 중 연결 갯수를 따라 측정
# IGRAPH clustering edge betweenness, groups: 3, mod: 0.28
# + groups:
#   $`1`
#   [1] "1"  "9"  "10" "12" "15"
# 
#   $`2`
#   [1] "2"  "7"  "8"  "13" "14"
# 
#   $`3`
#   [1] "3"  "5"  "6"  "11"
plot(as.dendrogram(friend_comm_eb))
