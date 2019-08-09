plot_projmat <- function(p_mat, stages = NULL) {
  ### input: a projection matrix (as matrix), and optional a vector of stagenames
  ###   (if stages is NULL, use existing column names from the matrix)
  ### output: a life cycle diagram
  
  if(!is.null(stages)) {
    ### if non-null value of stages, assign that to colnames
    if(length(stages) != dim(p_mat)[2]) {
      stop('Dimensions of stagename vector and proj matrix columns must match')
    }
    colnames(p_mat) <- stages
  }
  
  ### convert proj matrix to dataframes for use in DiagrammeR.
  p_df <- p_mat %>%
    as.data.frame() %>%
    ### set a column of stage to (i.e. the row numbers):
    mutate(to = 1:n()) %>%
    ### gather other columns (i.e. the columns):
    gather(from_txt, coef, -to) %>%
    ### convert colnames to a factor, then use the index of these factors
    ### to infer the column number (stage_from)
    mutate(from_txt = fct_inorder(from_txt),
           from = as.numeric(from_txt))
  
  ### Notes dataframe is stages:
  p_nodes_df <- p_df %>%
    select(id = from, label = from_txt) %>%
    distinct() %>%
    mutate(color = 'yellow', shape = 'ellipse', style = 'filled', )
  p_edges_df <- p_df %>%
    filter(coef != 0) %>% 
    mutate(rel = ifelse(to == 1, paste0('F_', to, from, ' = ', coef),
                        paste0('p_', to, from, ' = ', coef)),
           id = 1:n()) %>%
    select(id, to, from, rel) %>%
    mutate(label = rel,
            fontsize  = 6,
            fontcolor = 'grey20',
            fontname  = 'Helvetica',
            penwidth  = 1,
            arrowsize = .5)

  pmat_gr <- DiagrammeR::create_graph(nodes_df = p_nodes_df,
                                      edges_df = p_edges_df) %>%
    DiagrammeR::add_global_graph_attrs(
      attr      = c("layout", "rankdir"),
      value     = c("dot",    "LR"),
      attr_type = c("graph",  "graph"))
  
  DiagrammeR::render_graph(pmat_gr)
}