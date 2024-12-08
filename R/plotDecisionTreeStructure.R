#' @title 绘制决策树结构图
#' @description 使用 DiagrammeR 包绘制更美观的决策树结构
#' @param tree 决策树模型
#' @return 无返回值，直接绘制决策树结构图
#' @examples
#' \dontrun{
#' plotDecisionTreeStructure(tree)
#' }
#' @export
plotDecisionTreeStructure <- function(tree) {
  convert_to_data_tree <- function(tree_node) {
    if (tree_node$type == "leaf") {
      node <- Node$new(paste0("Class: ", tree_node$class))
    } else {
      node <- Node$new(paste0("Feature: ", tree_node$feature))
      for (name in names(tree_node$branches)) {
        child <- convert_to_data_tree(tree_node$branches[[name]])
        node$AddChildNode(child)
        childname <- paste0(tree_node$feature, " = ", name)
      }
    }
    return(node)
  }
  
  data_tree <- convert_to_data_tree(tree)
  
  # 用于将 data.tree 转换为 DOT 格式的函数
  data_tree_to_dot <- function(node) {
    dot <- ""
    
    if (length(node$children) == 0) {
      dot <- paste0('"', node$name, '" [label="', node$name, '" shape=ellipse style=filled fillcolor=lightgreen fontname="Helvetica-Bold" fontsize=12];\n')
    } else {
      dot <- paste0('"', node$name, '" [label="', node$name, '" shape=box style=filled fillcolor=lightyellow fontname="Helvetica-Bold" fontsize=12];\n')
      for (child in node$children) {
        dot <- paste0(dot, '"', node$name, '" -> "', child$name, '" [label="', child$name, '" fontsize=10];\n')
        dot <- paste0(dot, data_tree_to_dot(child))  # 递归调用
      }
    }
    
    return(dot)
  }
  
  dot_graph <- paste0("digraph tree {\n",
                      "node [fontname = 'Helvetica', fontsize = 12];\n",  # 设置字体和大小
                      "edge [fontsize = 10];\n",  # 设置边的字体大小
                      data_tree_to_dot(data_tree), 
                      "}")
  
  # 使用 DiagrammeR 绘制树形结构图
  DiagrammeR::grViz(dot_graph)
}
