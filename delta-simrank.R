#graph <- read.csv("Relationships1.txt", sep="\t",header=FALSE)
#graph <- read.csv("Relationships2.txt", sep="\t",header=FALSE)
graph <- read.csv("Relationships3.txt", sep="\t",header=FALSE)
#graph <- read.csv("Relationships-Companies-FinancialOrg.txt", sep=" ",header=FALSE)
#graph <- read.csv("Relationships-Persons-Companies.txt", sep=" ",header=FALSE)
#graph <- read.csv("Tests2.txt", sep="\t",header=FALSE)

cat(paste("Started: ",format(Sys.time(),"%H:%M:%S %Y-%m-%d")))


simrank <- function(graph){
list_relations <- neighbours(graph)
df_simrank <- similarity(relations=list_relations$relations, sim_df = list_relations$sim_df, sim_df_old = list_relations$sim_df_old, delta_sim_rank = list_relations$delta_sim_rank)
}
	
neighbours <- function(graph){

k <- 1

relations <- list()
nos <- unique(c(graph[,1],graph[,2]))
## inicial simrank df  with diagonal 1
sim_df <- data.frame()
sim_df <- rep(0,length(nos)^2)
dim(sim_df) <- c(length(nos),length(nos))
rownames(sim_df) <- c(nos)
colnames(sim_df) <- c(nos)
sim_df_old <- sim_df
delta_sim_rank <- sim_df
diag(sim_df) <- 1
diag(delta_sim_rank) <- 1

for (k in 1:length(nos)){
relations <- c(relations,list(unique(c(nos[k],graph[graph[,1]==nos[k],2],graph[graph[,2]==nos[k],1]))))
}
lapply(relations, write, "adjency_list.txt", append=TRUE, ncolumns=10000)
return (list(relations=relations,sim_df=sim_df, sim_df_old=sim_df_old,delta_sim_rank=delta_sim_rank))
}

niter <- 100
similarity <- function(relations, sim_df, sim_df_old,delta_sim_rank){

n <- length(relations)
r <- 0.9
s_uv <- 0
eps <- 10^-4
nodes <- c()

for (node in 1:n){
nodes <- c(nodes,relations[[node]][1])
}
#browser()
for(iter in 1:niter){
if(converged(sim_df,sim_df_old,eps)){break}
sim_df_old <- sim_df
for (u in 1:n){
for (v in 1:n){
node_relations_u <- relations[[u]]
node_relations_v <- relations[[v]]
node_u <- relations[[u]][1]
node_v <- relations[[v]][1]
length_relations_u <- length(relations[[u]])-1
length_relations_v <- length(relations[[v]])-1
node_related_u <- relations[[u]][-1]
node_related_v <- relations[[v]][-1]

#if (node_u == node_v || ((sim_df[u,v]-sim_df_old[u,v]<eps) & (delta_sim_rank[u,v] == 0))) {next} else {s_uv=0.0}

if (u == v) {next} else {s_uv=0.0}
            
for (n_u in node_related_u){

	
	for (n_v in node_related_v){
	s_uv = s_uv + sim_df_old[as.character(n_u),as.character(n_v)]
	#print(paste(u,"-",v,"-",n_u,"-",n_v,"-",s_uv))
	}
}

    sim_df[u,v] = r * s_uv / (length(node_related_u)*length(node_related_v))
	sim_df[v,u] = sim_df[u,v]
	delta_sim_rank[u,v]= sim_df[u,v]-sim_df_old[u,v]
	delta_sim_rank[v,u]=delta_sim_rank[u,v]
	if (((delta_sim_rank[u,v]<eps) & (delta_sim_rank[u,v] == 0))||((delta_sim_rank[v,u]<eps) & (delta_sim_rank[v,u] == 0))){
		break #???
	}


}
}
iter <- iter + 1
}
#browser()
return (sim_df)
}


converged <- function(sim, sim_old, eps = 10^-4){
		if (any(abs(sim - sim_old) >= eps)) {return (FALSE)}
		else {return (TRUE)}
}

index <- function(relations, node)
{
i <- 1
for(i in 1:length(relations)){
if(relations[[i]][1]==node){
return (i)
}
}

}

recommender <- function(sim_matrix, threshold, relations){

var_a <- c()
var_b <- c()
report <- list()
sim_matrix_zeros <- sim_matrix
for (n in 1:length(relations$relations)){report[[n]] <- relations$relations[[n]][1]}
#browser()
for (i in 1:nrow(sim_matrix_zeros)){
for (j in 1:ncol(sim_matrix_zeros)){
if(sim_matrix[i,j] < 1 & j < i){sim_matrix_zeros[i,j] = 0}
}
}
index <- which(sim_matrix_zeros > threshold & sim_matrix_zeros !=1 , arr.ind=TRUE)


for (k in 1:nrow(index)){

for (l in 1:length(relations$relations)){
if(relations$relations[[l]][1]==index[k,1]){
neighbours_a <- relations$relations[[l]][-1]}
if(relations$relations[[l]][1]==index[k,2]){
neighbours_b <- relations$relations[[l]][-1]}
}

add_neighbours_a <- neighbours_b[which((neighbours_b %in% neighbours_a)==FALSE)]
add_neighbours_b <- neighbours_a[which((neighbours_a %in% neighbours_b)==FALSE)]

for (m in 1:length(report)){
if(report[[m]][1]==index[k,1]){
report[[m]] <- unique(c(report[[m]],add_neighbours_a))}
if(report[[m]][1]==index[k,2]){
report[[m]] <- unique(c(report[[m]],add_neighbours_b))}
}
}
return(report)
}



relations <- neighbours(graph)
sim_matrix <- simrank(graph)
#report <- recommender(sim_matrix, threshold=0.5, relations)
cat(paste("\nEnded ",format(Sys.time(),"%H:%M:%S %Y-%m-%d"),"\n"))