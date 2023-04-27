## -------->>  [[file:../nstandr.src.org::*visualize][visualize:1]]
##' Checks if string has something to print
##' @param x string to test
##' @return boolean
is_empty <- function(x) {
    if(is.null(x) || is.na(x) || isTRUE(is.character(x) && x == ""))
        TRUE
    else
        FALSE
}


##' Makes <TR><TD> record for dot node <TABLE>
##' @param str content
##' @param bg_color row's background color
##' @param font row's font family
##' @param b make it bold
##' @param i make it intallic
##' @return string
paste_dot_node_tr_td <- function(str, bg_color = NULL, font = NULL, b = FALSE, i = FALSE) {
    if(length(str) != 0 && !is.na(str) && str != "") {
        c('<TR><TD', if(!is.null(bg_color)) paste0(' BGCOLOR="', bg_color, '"'), '>'
        , if(i) '<I>'
        , if(b) '<B>'
        , if(!is.null(font)) paste0('<FONT FACE="', font, '">')
        , str
        , if(!is.null(font)) '</FONT>'
        , if(b) '</B>'
        , if(i) '</I>'
        , '</TD></TR>') |> paste(collapse = "")
    }
}


##' Makes a dot node (as html table) from procedure's attributes.
##' 
##' @param procedure Procedure's function either as charater string or as call (with optional arguments)
##' @param procedure_title This will overwrites the node's title. By default the title is either from procedure's attribute 'title' (prefixed by `procedure_attr_prefix`) or just a name of procedure if there is no such attibute.
##' @param id Node ID that is used for refering in edges (arrows) definition
##' @param node_bg_color node's background color
##' @param node_header_color node's header background color
##' @param node_font main font family
##' @param node_fix_width_font fixed with font family
##' @param add_example wheather to add example from procedure's attribure 'example' (prefixed by `procedure_attr_prefix`)
##' @param add_procedure_name wheather to add procedure's name
##' @param add_procedure_args wheather to add optional procedure's arguments
##' @param procedure_attr_prefix prefix used to specify procedures' attibutes
##' @return dot node as string
paste_dot_node <- function(procedure, id
                         , procedure_title = NULL
                        , node_bg_color = "white"
                        , node_header_color = "#bdc3c7"
                        , node_font = "Helvetica"
                        , node_fix_width_font = "Courier"
                        , add_example = TRUE
                        , add_procedure_name = FALSE
                        , add_procedure_args = FALSE
                        , procedure_attr_prefix = "nstandr_procedure_") {
    procedure_name <- as.character(procedure[[1]])
    obj <- get0(procedure_name, ifnotfound = NULL)
    att <- list()
    att_to_get <- c("title"
                  , "example"
                  , "ref"
                  , "pp")
    if(!is.null(obj)) {
        ## bind procedure description attribures
        within(att
             , for(tag in att_to_get) {
                   if(requireNamespace("htmltools", quietly=TRUE)) {
                       assign(tag, attr(obj, paste0(procedure_attr_prefix, tag)) |> htmltools::htmlEscape())
                   } else {
                       assign(tag, attr(obj, paste0(procedure_attr_prefix, tag)))
                   }
               })
    }
    paste0("node_", id, " [label = " , 
           c('<<TABLE BGCOLOR="', node_bg_color, '" BORDER="0" CELLBORDER="1" CELLSPACING="0">'
           , if(!is_empty(procedure_title)) {
                 paste_dot_node_tr_td(procedure_title, bg_color = node_header_color)
             } else {
                 paste_dot_node_tr_td(att$title, bg_color = node_header_color)
             }
           , ## add procedure_name (as title if there is no title yet)
             if(is_empty(att$title) && is_empty(procedure_title)) {
                 paste_dot_node_tr_td(procedure_name, bg_color = node_header_color, font = node_fix_width_font)
             } else if(add_procedure_name) {
                 paste_dot_node_tr_td(procedure_name, font = node_fix_width_font)
             }
           , if(add_procedure_args && is.call(procedure) && length(as.list(procedure)[-1]) != 0) {
                 sub(procedure_name, "", deparse(procedure), fixed = TRUE) |>
                     paste_dot_node_tr_td(font = node_fix_width_font)
             }
           , if(add_example) {
                 paste_dot_node_tr_td(att$example, i = TRUE)
             }
           , "</TABLE>>") |> paste(collapse = "")
         , "];")
}


##' Generates description of dot graph nodes.
##' 
##' @param procedures_list List of procedures. See `sandardize` for how to specify of procedures list.
##' @param node_prefix pefix for node IDs
##' @param procedures_group_bg_color Background color for visuallizing groupped procedures (nested list of procedures)
##' @param procedures_group_title_font Font family for group of procedures title
##' @inheritDotParams paste_dot_node
##' @return dot nodes as string
make_dot_nodes <- function(procedures_list, node_prefix = NULL, ...
                          , procedures_group_bg_color = "#ecf0f1"
                          , procedures_group_title_font = "Helvetica") {
    procedures_names <- names(procedures_list)
    if(is.null(procedures_names)) {
        procedures_names <- rep(NA, length(procedures_list))
    }
    mapply(\(p, name, n) {
        if(is.list(p)) {
            c(paste0("subgraph cluster_", n," {")
            , if(!is.na(name)) paste0("    label=\"", name, "\";")
            , paste0('    bgcolor="', procedures_group_bg_color, '";')
            , paste0('    fontname="', procedures_group_title_font, '"')
            , paste0("    ", make_dot_nodes(p, node_prefix = paste0(n, "_"), ...))
            , "}")
        } else {
            paste_dot_node(p, paste0(node_prefix, n), procedure_title = name, ...)
        }
    }
  , p = procedures_list
  , name = procedures_names
  , n = seq_along(procedures_list)) |>
      unlist()
}


##' Makes dot graph edges for visualizing arrows between sequence of procedures. 
##' 
##' @param procedures_list List of procedures. See `sandardize` for how to specify of procedures list.
##' @param prefix Prefix for edge IDs
##' @return 
make_dot_edges <- function(procedures_list, prefix = NULL) {
    mapply(\(p, p_next, n, n_next) {
        ## from
        edge_from <- NULL
        edge_from_pre <- NULL
        if(is.list(p)) {
            edge_from_pre <- c(paste0("// cluster_", n)
                             , paste0("  ", make_dot_edges(p, prefix = paste0(n, "_"))))
            edge_from <- paste0("node_", n, "_", length(p))
        } else {
            edge_from <- paste0("node_", n)
        }
        ## to
        lhead <- NULL
        edge_to <- NULL
        edge_to_post <- NULL
        if(is.list(p_next)) {
            lhead <- paste0('lhead = "cluster_', n_next, '"')
            edge_to <- paste0("node_", n_next, "_1")
            ## add nested list here in case of last element
            edge_to_post <- 
                if(n_next == length(procedures_list)) {
                    c(paste0("// cluster_", n_next)
                    , paste0("  ", make_dot_edges(p_next, prefix = paste0(n_next, "_"))))
                }
        } else {
            edge_to <- paste0("node_", n_next)
        }
        ## edge
        c(edge_from_pre
        , paste0(edge_from, " -> ", edge_to, " [", lhead, "];")
        , edge_to_post)
    }
  , p = procedures_list[-length(procedures_list)]
  , p_next = procedures_list[-1]
  , n = paste0(prefix, seq(length(procedures_list) - 1))
  , n_next = paste0(prefix, seq(2, length(procedures_list)))) |>
      unlist()
}


##' Generates graph description for visualizing list of procedures in dot format.
##'
##' The visualization itself requires some dependencies to be installed (e.g., `DiagrammeR` package). If you do not have those you can `cat()` the returned string to the R console and copy it to some web tool for dot visualization (e.g.http://magjac.com/graphviz-visual-editor)
##' @param procedures_list List of procedures. See `sandardize` for how to specify of procedures list.
##' @inheritDotParams make_dot_nodes
##' @return dot graph as string
##' @export 
make_dot_graph <- function(procedures_list, ...) {
    paste("digraph nstantr_procedures {"
        , "  compound=true;"
        , "  node [shape = plain, fontname = Helvetica]"
        , paste0("  ", make_dot_nodes(procedures_list, ...)) |> paste(collapse = "\n")
        , paste0("  ", make_dot_edges(procedures_list)) |> paste(collapse = "\n")
        , "}"
        , collapse = "\n", sep = "\n")
}




##' Saves dot graph as file using system command 'dot' from GraphViz (https://graphviz.org/) if installed.
##' 
##' @param dot_graph Dot graph as a character string
##' @param save_to_file Name of file to save to. Without path or relative path saves to the R's current working directory. If NULL returns file content as string.
##' @param save_as File format. Default is "png"
##' @param width width of the procedures graph in pixels
##' @param height height of the procedures graph in pixels
##' @return Ethier raw file as character string if `save_to_file` is NULL or a file name, i.e. the value of `save_to_file` is it is not NULL
save_dot_graph_as <- function(dot_graph
                            , save_to_file = NULL
                            , save_as = c("png", "svg", "pdf")
                            , width = NULL
                            , height = NULL) {
    save_as <- match.arg(save_as)
    if(system("which dot", intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE) |>
       suppressWarnings() |>
       attr("status", exact = TRUE) |>
       is.null()) {
        cmd <- paste0('dot -T', save_as, ':cairo')
        if(!is.null(width) || !is.null(height)) {
            if(is.null(width)) width <- height
            if(is.null(height)) height <- width
            width <- width / 100
            height <- height / 100
            cmd <- paste0(cmd, ' -Gsize=', width, ',', height, '\\! -Gdpi=100')
        }
        if(!is.null(save_to_file)) {
            paste0(cmd, ' -o "', save_to_file, '"') |>
                system(input = dot_graph, intern = TRUE)
            return(save_to_file)
        } else {
            return(system(cmd, input = dot_graph, intern = TRUE))
        }
    } else {
        warning("Saving dot graph as pdf requires 'dot' command to be installed on your system. See https://graphviz.org/download/ for installation. You might try to open procedures visualization in a browser by using `visualize` command setting its 'save_to_file' argument to NULL (default).")
    }
}



##' Generates a temporary html file with visualization of given nstandr procedures and opens it in a browser (specified in `options('browser')`). This requires `DiagrammeR` package to be installed. If you do not have those you can `cat()` the returned string from `nstandr:::make_dot_graph()' to the R console and copy it to some web tool for dot visualization (e.g.http://magjac.com/graphviz-visual-editor)
##'
##' @inheritParams make_dot_graph
##' @param procedures_list List of procedures. See `sandardize` for how to specify of procedures list.
##' @param width width of the procedures graph in pixels
##' @param height height of the procedures graph in pixels
##' @inheritDotParams make_dot_graph
##' @return An object of class ‘htmlwidget’ that can be used in Rmarkdown
##' @export 
browse_dot_graph <- function(procedures_list
                           , width = NULL
                           , height = NULL
                           , ...) {
    if(requireNamespace("DiagrammeR", quietly=TRUE)) {
        procedures_list |>
            make_dot_graph(...) |>
            DiagrammeR::grViz()
    } else {
        message("To browse procedures visualization `DiagrammeR` package should be installed (you can install it with `install.packages('DiagrammeR')`). If you do not have it you can just `cat()` the returned string from `nstandr:::make_dot_graph()' to the R console and copy it to some web tool for dot visualization (e.g.http://magjac.com/graphviz-visual-editor). Otherwise you might try to save visualization to file. For that GraphViz should be installed on your system.")
    }
}




##' Visualizes list of procedures.
##' 
##' @param procedures_list List of procedures. See `sandardize` for how to specify of procedures list.
##' @param save_to_file If set tries to save visualization to file in the format `save_as` with `dot` system command (should be installed on your system). If it is NULL tries to open the visualization in browser (this requires DiagrammeR R package to be installed.)
##' @param save_as Type of file to save if `save_to_file` is not NULL. If not specified (i.e. NULL) then it tries to guess the type of file from `save_to_file` extention. Currently "png", "svg" and "pdf" are supported.
##' @param width width of the procedures graph in pixels
##' @param height height of the procedures graph in pixels
##' @inheritDotParams make_dot_graph
##' @return An object of class ‘htmlwidget’ that can be used in Rmarkdown or a file name, i.e. the value of `save_to_file` is it is not NULL.
##' @export 
visualize <- function(procedures_list
                    , save_to_file = NULL
                    , save_as = NULL
                    , width = NULL
                    , height = NULL
                    , ...) {
    if(is.null(save_to_file)) {
        browse_dot_graph(procedures_list
                       , width
                       , height
                       , ...)
    } else {
        if(is.null(save_as)) {
            save_as <- tools::file_ext(save_to_file)
        }
        save_as <- match.arg(save_as, c("png", "svg", "pdf"))
        procedures_list |>
            make_dot_graph(...) |>
            save_dot_graph_as(save_to_file
                            , save_as
                            , width
                            , height)
    }
 }
## --------<<  visualize:1 ends here


