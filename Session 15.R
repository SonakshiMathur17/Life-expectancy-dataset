

placement=read.csv("C:/Users/Admin/Downloads/College_placement.csv)


n =nrow(placement)

set.seed(123)

train_index = sample(l:n, size=0.7'n)

train_data = placement[train_index,]
test_data = placement[-train_index,]

colnames(train_data)

