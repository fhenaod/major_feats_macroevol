library(apTreeshape)

d<-dir("Phylos/")
t<-grep("tree_",d)
tt<-d[t]
e.trees<-list()

#read trees
for(i in 1:length(tt)){
  typ<- strsplit(tt[[i]], split="_", fixed=TRUE)[[1]][3]
  prefix<-paste(strsplit(tt[[i]], split="_", fixed=TRUE)[[1]][2])
  if (typ == ".nex"){
    tree <- read.nexus(paste0("Phylos/",tt[[i]]))
    e.trees[[i]]<-tree
  } else {
    tree <- read.tree(paste0("Phylos/",tt[[i]]))
    e.trees[[i]]<-tree
  }
}  

imbalance_metrics=function(trees,n.mc){
  tree.ts<-c()
  
  shape.yule<-c()
  colles.yule<-c()
  p.coless.t.y.less<-c()
  p.coless.t.y.great<-c()
  sackin.yule<-c()
  p.lt.yule<-c()
  
  shape.pda<-c()
  colles.pda<-c()
  p.coless.t.pda.less<-c()
  p.coless.t.pda.great<-c()
  sackin.pda<-c()
  p.lt.pda<-c()
  
  imbalance_metrics<-c()
  for (i in 1:length(trees)){
    tree<-e.trees[[i]]
    tree.ts<-as.treeshape(tree)
    
    shape.yule[i]<-shape.statistic(tree.ts, norm = "yule")
    colles.yule[i]<-colless(tree.ts,norm = "yule")
    
    df<-data.frame(colless.test(tree.ts, model = "yule", alternative = "less", n.mc = n.mc))
    p.coless.t.y.less[i]<-df$p.value
    df<-data.frame(colless.test(tree.ts, model = "yule", alternative = "greater", n.mc = n.mc))
    p.coless.t.y.great[i]<-df$p.value
    df<-data.frame(likelihood.test(tree.ts, model = "yule", alternative = "two.sided"))
    p.lt.yule[i]<-df$p.value
    
    sackin.yule[i]<-sackin(tree.ts, norm = "yule")
    
    shape.pda[i]<-shape.statistic(tree.ts, norm = "pda")
    colles.pda[i]<-colless(tree.ts,norm = "pda")
    
    df<-data.frame(colless.test(tree.ts, model = "pda", alternative = "less", n.mc = n.mc))
    p.coless.t.pda.less[i]<-df$p.value
    df<-data.frame(colless.test(tree.ts, model = "pda", alternative = "greater", n.mc = n.mc))
    p.coless.t.pda.great[i]<-df$p.value
    df<-data.frame(likelihood.test(tree.ts, model = "pda", alternative = "two.sided"))
    p.lt.pda[i]<-df$p.value
    sackin.pda[i]<-sackin(tree.ts, norm = "pda")
    
imbalance.metrics<-data.frame(
  shape.yule,colles.yule,p.coless.t.y.less,p.coless.t.y.great,sackin.yule,p.lt.yule,
  shape.pda,colles.pda,p.coless.t.pda.less,p.coless.t.pda.great,sackin.pda,p.lt.pda)
  }
  return(imbalance.metrics)
}
imbalance.metrics<-imbalance_metrics(e.trees,1000)