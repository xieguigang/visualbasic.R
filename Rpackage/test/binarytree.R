#Region "Microsoft.ROpen::93c72523316f56eb4fdc5ea9f9be2c03, test\binarytree.R"

    # Summaries:


#End Region

require(VisualBasic.R)

src = c(1,1,1,2,3,400,5,6,5,6,7,88,99,0);
tree <- binaryTree(src, function(x) x, function(x,y) x-y);

node.is_leaf(tree[["14"]]);
node.left(tree, tree[["6"]])
node.find(tree, 400, function(x, y) x - y);
node.find(tree, 0, function(x, y) x - y);




src = runif(10000, 0.0, 10.0)

tree <- binaryTree(src, function(x) x, function(x,y) {
	if (abs(x - y) <= 0.5) {
		0;
	} else {
		x - y;
	}
});
