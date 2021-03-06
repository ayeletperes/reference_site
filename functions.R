## functions


allele_appearance <- function(data_, g_group){
  n_alleles <- length(unique(data_[grepl(g_group,v_gene),v_allele]))
  data_ <- data_[grepl(g_group,v_gene)]
  data_[,v_alleles2:=paste0("*",v_allele)]
  ggplot(data_, aes(v_alleles2, fill = v_alleles2)) + 
    geom_bar() + facet_grid(.~project) + 
    labs(x = "allele", y = "# Individuals", fill = "") + 
    bbplot::bbc_style()  + theme(legend.position = "right", 
                                 axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1)) + 
    scale_fill_manual(values = pal %>% usecol(n = n_alleles))
  
}

sequence_depth <- function(data_, g_group){
  
  n_alleles <- length(unique(data_[grepl(g_group,v_gene),v_allele]))
  data_ <- data_[grepl(g_group,v_gene)]
  data_[,v_alleles2:=paste0("*",v_allele)]
  data_[,text:=paste(
    '</br>Project: ',
    project,
    '</br>Subject: ',
    subject,
    '</br>Alleles: ',
    v_allele,
    '</br># assignments: ',
    count,
    '</br>Relative freq.: ',
    round(freq, 4),
    '</br>Relative Rep. freq.: ',
    round(freq2, 4)
  )]
  
  colors <- pal %>% usecol(n = n_alleles)
  colors <- setNames(colors[1:length(unique(data_$project))],unique(data_$project))
  p_list <- lapply(unique(data_$project),function(p){
    g1 <- ggplot(data_[project==p], aes(v_alleles2, count, text = text)) + 
      geom_boxplot(outlier.shape=NA, color = colors[p]) +
      geom_point(position=position_jitter(width = 0.1), color = colors[p]) + 
      labs(x = "allele", y = "# Sequences", color = "") + 
      bbplot::bbc_style() + scale_color_manual(values = pal %>% usecol(n = n_alleles)) +
      theme(axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1))
    
    g1 <- ggplotly(g1, tooltip = "text") %>%
      add_annotations(
        text = p,
        x = 0.5,
        y = 1.1,
        yref = "paper",
        xref = "paper",
        xanchor = "middle",
        yanchor = "top",
        showarrow = FALSE,
        font = list(size = 15)
      )
    
    g1$x$data <- lapply(g1$x$data, FUN = function(x){
      if(x$marker$line$color=="rgba(0,0,0,1)") x$marker = list(opacity = 0)
      return(x)
    })
    
    return(g1)
  })
  
  subplot(p_list, nrows = length(colors), 
          shareY = F, titleX = T, 
          titleY = T, shareX = F, margin = 0.07)
}


hline <- function(y = 0, color = "red", x0 = 0, x1 = 1) {
  list(
    type = "line",
    x0 = x0,
    x1 = x1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, dash = "dot")
  )
}

plot_zygousity <- function(tmp, state, allele_thresh, g){
  
  tmp_plot <- dplyr::filter(tmp, zygousity_state == state) %>% dplyr::rowwise() %>% dplyr::mutate(
    v_alleles_p = v_alleles_abc,
    v_alleles_p = gsub(";", "\n", v_alleles_p),
    text = paste(
      '</br>Project: ',
      project,
      '</br>Subject: ',
      subject,
      '</br>Alleles: ',
      v_alleles,
      '</br># assignments: ',
      count,
      '</br>Relative freq.: ',
      round(freq, 4),
      '</br>Relative Rep. freq.: ',
      round(freq2, 4)
    )
  ) %>% ungroup()
  
  
  loc2 <-
    setNames(1:length(unique(tmp_plot$v_allele_axis)), sort(unique(tmp_plot$v_allele_axis)))
  
  tmp_plot$loc2 <- loc2[tmp_plot$v_allele_axis]
  
  if(state!=1 & length(unique(tmp_plot$v_alleles_p))!=1){
    loc_jitter <- list()
    for(ii in 1:length(unique(tmp_plot$loc2))){
      loc_c <- as.character(unique(tmp_plot$loc2)[ii])
      loc_jitter[[loc_c]] <-
        seq(0, 0.5, length.out = length(unique(tmp_plot$v_alleles_p[tmp_plot$loc2==unique(tmp_plot$loc2)[ii]])))
      
      loc_jitter[[loc_c]]  <-
        setNames(loc_jitter[[loc_c]] , sort(unique(tmp_plot$v_alleles_p[tmp_plot$loc2==unique(tmp_plot$loc2)[ii]])))
    }
    
    
    tmp_plot <-
      tmp_plot %>% dplyr::arrange(loc2, v_alleles_p) %>% dplyr::group_by(loc2) %>% 
      dplyr:: mutate(loc_plot = loc2 + loc_jitter[[as.character(unique(loc2))]][v_alleles_p],) %>% ungroup()
    
      if(length(tmp_plot$loc_plot)) tmp_plot <- tmp_plot %>% dplyr::mutate(jitter_offset = jitter(loc_plot))
  }else{
    tmp_plot <-
      tmp_plot %>% dplyr::arrange(loc2, v_alleles_p) %>% dplyr::group_by(loc2) %>% 
      dplyr:: mutate(loc_plot = loc2,) %>% ungroup()
    if(length(tmp_plot$loc_plot)) tmp_plot <- tmp_plot %>% dplyr::mutate(jitter_offset = jitter(loc_plot))
  }
  
  tickvals_tmp <-
    tmp_plot %>% dplyr::pull(loc_plot) %>% unique() %>% sort()
  
  tickvals <- c()
  
  for (i in 1:length(loc2)) {
    tickvals <- c(tickvals, mean(tickvals_tmp[floor(tickvals_tmp) == i]))
  }
  
  
  ticktext <-
    tmp_plot %>% dplyr::pull(v_allele_axis) %>% unique() %>% sort()
  
  plotly1 <-
    tmp_plot %>% rowwise() %>% dplyr::mutate(group = paste0(project, "-", v_alleles_p)) %>%
    highlight_key(., ~ subject) %>%
    plot_ly() %>%
    add_trace(
      type = "scatter",
      x = ~ (jitter_offset),
      y = ~ freq,
      text = ~ text,
      symbol = ~ project,
      mode = 'markers',
      marker = list(color = "grey", size = 12),
      showlegend = TRUE,
      opacity = 0.9,
      hoverinfo = 'none',
      legendgroup = ~ project
    ) %>%
    add_trace(
      type = "scatter",
      x = ~ (jitter_offset),
      y = ~ freq,
      text = ~ text,
      color = ~ v_alleles_p,
      mode = 'markers',
      showlegend = FALSE,
      opacity = 0.8,
      hoverinfo = 'text',
      legendgroup = ~ v_alleles_p
    ) %>%
    add_trace(
      x = ~ as.numeric(loc_plot),
      y = ~ freq,
      color = ~ v_alleles_p,
      type = "box",
      hoverinfo = "none",
      fillcolor = "transparent",
      name = ~ v_alleles_p,
      legendgroup = ~ v_alleles_p
    ) %>%
    layout(
      hovermode = 'closest',
      shapes = list(hline(allele_thresh/100)),
      legend = list(
        tracegroupgap = 20,
        title = list(text =
                       '<b>  </b>'),
        orientation = "V"
      ),
      xaxis = list(
        title = paste0(g, " Alleles"),
        autotick = F,
        tickmode = "array",
        tickvals = tickvals,
        ticktext = ticktext
      ),
      yaxis = list(title = "Relative allele frequency",
                   range = c(0,1.05))
    )  %>% plotly::highlight(
      on = "plotly_click",
      selected = attrs_selected(showlegend = FALSE),
      opacityDim = 0.3,
      persistent = TRUE
    ) %>% plotly_build()
  
  return(plotly1)
}

data_cutoff <- function(tmp, func_groups, g_group, allele_thresh = 0.5, or_allele){
  v_gene_cut <- ifelse(grepl("G",g_group), g_group, func_groups[as.character(g_group)])
  tmp <- tmp %>%
    dplyr:: filter(v_gene == v_gene_cut, !is.na(v_allele)) %>% 
    ungroup()
  
  tmp <- tmp %>% dplyr::group_by(subject)
  
  tmp <- tmp %>% dplyr::arrange(desc(freq)) %>%
    dplyr::group_by(subject, v_gene) %>% dplyr::mutate(
      zygousity_state = as.numeric(sum(freq > allele_thresh/100, na.rm = T)),
      v_alleles = paste0(1:unique(zygousity_state), " - ", or_allele[v_allele[1:unique(zygousity_state)]], collapse = ";"),
      v_alleles_abc = paste0(sort(or_allele[v_allele[1:unique(zygousity_state)]]), collapse = ";"),
      v_allele_axis = or_allele[v_allele]
    ) %>% arrange(subject)
  tmp <- tmp %>% dplyr::group_by(subject, zygousity_state) %>% dplyr::mutate(loc_state = loc <= zygousity_state) %>% filter(loc_state) %>% ungroup()
  
  return(tmp)
}


data_cutoff_allele_thresh <- function(tmp, func_groups, g_group, allele_thresh = c("01" = 0.5), or_allele){
  v_gene_cut <- ifelse(grepl("G",g_group), g_group, func_groups[as.character(g_group)])
  tmp <- tmp %>%
    dplyr:: filter(v_gene == v_gene_cut, !is.na(v_allele)) %>% 
    ungroup()
  
  tmp <- tmp %>% dplyr::group_by(subject)
  
  tmp <- tmp %>% dplyr::group_by(v_allele) %>% dplyr::filter(freq >= allele_thresh[v_allele]/100)
  
  
  tmp <- tmp %>% dplyr::arrange(desc(freq)) %>%
    dplyr::group_by(subject, v_gene) %>% dplyr::mutate(
      zygousity_state = as.numeric(length(unique(v_allele))),
      v_alleles = paste0(1:unique(zygousity_state), " - ", or_allele[v_allele[1:unique(zygousity_state)]], collapse = ";"),
      v_alleles_abc = paste0(sort(or_allele[v_allele[1:unique(zygousity_state)]]), collapse = ";"),
      v_allele_axis = or_allele[v_allele]
    ) %>% arrange(subject)
  tmp <- tmp %>% dplyr::group_by(subject, zygousity_state) %>% 
    dplyr::mutate(loc_state = loc <= zygousity_state) %>% filter(loc_state) %>% ungroup()
  
  return(tmp)
}


plot_zygousity_allele_thresh <- function(tmp, state, allele_thresh, g){
  
  tmp_plot <- dplyr::filter(tmp, zygousity_state == state) %>% dplyr::rowwise() %>% dplyr::mutate(
    v_alleles_p = v_alleles_abc,
    v_alleles_p = gsub(";", "\n", v_alleles_p),
    text = paste(
      '</br>Project: ',
      project,
      '</br>Subject: ',
      subject,
      '</br>Alleles: ',
      v_alleles,
      '</br># assignments: ',
      count,
      '</br>Relative freq.: ',
      round(freq, 4),
      '</br>Relative Rep. freq.: ',
      round(freq2, 4)
    )
  ) %>% ungroup()
  
  
  
  loc2 <-
    setNames(1:length(unique(tmp_plot$v_allele_axis)), sort(unique(tmp_plot$v_allele_axis)))
  
  allele_thresh <- allele_thresh[names(allele_thresh) %in% names(loc2)]
  
  tmp_plot$loc2 <- loc2[tmp_plot$v_allele_axis]
  
  if(state!=1 & length(unique(tmp_plot$v_alleles_p))!=1){
    loc_jitter <- list()
    for(ii in 1:length(unique(tmp_plot$loc2))){
      loc_c <- as.character(unique(tmp_plot$loc2)[ii])
      loc_jitter[[loc_c]] <-
        seq(0, 0.5, length.out = length(unique(tmp_plot$v_alleles_p[tmp_plot$loc2==unique(tmp_plot$loc2)[ii]])))
      
      loc_jitter[[loc_c]]  <-
        setNames(loc_jitter[[loc_c]] , sort(unique(tmp_plot$v_alleles_p[tmp_plot$loc2==unique(tmp_plot$loc2)[ii]])))
    }
    
    
    tmp_plot <-
      tmp_plot %>% dplyr::arrange(loc2, v_alleles_p) %>% dplyr::group_by(loc2) %>% 
      dplyr:: mutate(loc_plot = loc2 + loc_jitter[[as.character(unique(loc2))]][v_alleles_p],) %>% ungroup()
      if(length(tmp_plot$loc_plot)) tmp_plot <- tmp_plot %>% dplyr::mutate(jitter_offset = jitter(loc_plot))
  }else{
    tmp_plot <-
      tmp_plot %>% dplyr::arrange(loc2, v_alleles_p) %>% dplyr::group_by(loc2) %>% 
      dplyr:: mutate(loc_plot = loc2,) %>% ungroup()
      if(length(tmp_plot$loc_plot)) tmp_plot <- tmp_plot %>% dplyr::mutate(jitter_offset = jitter(loc_plot))
  }
  
  tickvals_tmp <-
    tmp_plot %>% dplyr::pull(loc_plot) %>% unique() %>% sort()
  
  tickvals <- c()
  
  for (i in 1:length(loc2)) {
    tickvals <- c(tickvals, mean(tickvals_tmp[floor(tickvals_tmp) == i]))
  }
  
  
  ticktext <-
    tmp_plot %>% dplyr::pull(v_allele_axis) %>% unique() %>% sort()
  
  cols <- setNames(rev(c("#FAAB18", "#1380A1","#990000", "#588300")),as.character(unique(allele_thresh)))
  
  plotly1 <-
    tmp_plot %>% rowwise() %>% dplyr::mutate(group = paste0(project, "-", v_alleles_p)) %>%
    highlight_key(., ~ subject) %>%
    plot_ly() %>%
    add_trace(
      type = "scatter",
      x = ~ (jitter_offset),
      y = ~ freq,
      text = ~ text,
      symbol = ~ project,
      mode = 'markers',
      marker = list(color = "grey", size = 12),
      showlegend = TRUE,
      opacity = 0.9,
      hoverinfo = 'none',
      legendgroup = ~ project
    ) %>%
    add_trace(
      type = "scatter",
      x = ~ (jitter_offset),
      y = ~ freq,
      text = ~ text,
      color = ~ v_alleles_p,
      mode = 'markers',
      showlegend = FALSE,
      opacity = 0.8,
      hoverinfo = 'text',
      legendgroup = ~ v_alleles_p
    ) %>%
    add_trace(
      x = ~ as.numeric(loc_plot),
      y = ~ freq,
      color = ~ v_alleles_p,
      type = "box",
      hoverinfo = "none",
      fillcolor = "transparent",
      name = ~ v_alleles_p,
      legendgroup = ~ v_alleles_p
    ) %>%
    layout(
      hovermode = 'closest',
      shapes = lapply(1:length(names(allele_thresh)), function(ia){a = names(allele_thresh)[ia]; xx = seq(0,1,length.out = length(allele_thresh)+1); hline(unname(allele_thresh[a])/100, x0 = xx[ia], x1 = ia*(1/length(allele_thresh)), color = cols[as.character(allele_thresh[a])])}),
      legend = list(
        tracegroupgap = 20,
        title = list(text =
                       '<b>  </b>'),
        orientation = "V"
      ),
      xaxis = list(
        title = paste0(g, " Alleles"),
        autotick = F,
        tickmode = "array",
        tickvals = tickvals,
        ticktext = ticktext
      ),
      yaxis = list(title = "Relative allele frequency",
                   range = c(0,1.05))
    )  %>% plotly::highlight(
      on = "plotly_click",
      selected = attrs_selected(showlegend = FALSE),
      opacityDim = 0.3,
      persistent = TRUE
    ) %>% plotly_build()
  
  return(plotly1)
}


seq_align <- function(v_calls, allele_db, vgerms, chain, mat, g_group){
  alleles <- allele_db %>% dplyr::filter(new_allele %in% v_calls) %>% dplyr::pull(or_allele)
  new_alleles <- setNames(allele_db %>% filter(new_allele %in% v_calls) %>% dplyr::pull(new_allele), alleles)
  sequences <- substr(vgerms[[chain]][alleles],1,318)
  names(sequences) <- new_alleles[names(sequences)]
  
  sequences <- sapply(sequences, function(seq) ifelse(nchar(seq)<318, paste0(seq,paste0(rep(".",318-nchar(seq)), collapse = ""), collapse = ""), seq) )
  
  mat_sub <- mat[alleles,alleles]
  
  colnames(mat_sub) <-  gsub(paste0(g_group,"[*]"),"",new_alleles[colnames(mat_sub)])
  rownames(mat_sub) <-  gsub(paste0(g_group,"[*]"),"",new_alleles[rownames(mat_sub)])
  
  matrix_sequences <- as.data.frame(sapply(sequences,seqinr::s2c), stringsAsFactors = F)
  
  nucs <- nrow(matrix_sequences)-sum(apply(matrix_sequences, 1, function(x) all(x==".")))
  
  hc <- hclust(as.dist(mat_sub), method = "complete")
  dend <- as.dendrogram(hc)
  dend <- dendextend::set(dend, "labels_cex", 2)
  ggd1 <- as.ggdend(dend)
  p_dend <- ggplot(ggd1, theme = bbplot::bbc_style())  + theme(
    axis.line = element_blank(), axis.title.x = element_blank(),
    axis.ticks.x = element_blank(), axis.text.x = element_blank(),
    axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.border = element_blank(), panel.background = element_blank(), 
    legend.position = "none" ) + scale_y_continuous(sec.axis = sec_axis(~.*nucs, name = "Mutations")) +
    ylab("Ratio")
  
  
  
  matrix_sequences$annot <- apply(matrix_sequences, 1, function(x) length(unique(x)) != 1) 
  matrix_sequences$pos <- 1:318
  matrix_sequences_plot <- reshape2::melt(matrix_sequences, id.vars = c("pos","annot"))
  matrix_sequences_plot$id <- matrix_sequences_plot$pos
  matrix_sequences_plot$allele <- gsub(paste0(g_group,"[*]"),"",matrix_sequences_plot$variable)
  matrix_sequences_plot$allele <- factor(matrix_sequences_plot$allele, levels = unique(matrix_sequences_plot$allele))
  matrix_sequences_plot$value[matrix_sequences_plot$value=="."] <- NA
  matrix_sequences_plot$annot_text <- sapply(1:nrow(matrix_sequences_plot), function(i) ifelse(matrix_sequences_plot$annot[i],matrix_sequences_plot$value[i],""))
  
  plot_align <- function(data, low_bound = 1, upper_boud = 80){
    
    
    ggplot(data[data$id>=low_bound & data$id<upper_boud, ], aes(x=(pos), y=(allele))) + 
      geom_tile(aes(fill=value),colour="white") + 
      geom_text(aes(label = annot_text), color = "black") +
      #coord_equal(expand = F, xlim = c(low_bound, upper_boud), ratio = 9/5, clip = "off") + 
      bbplot::bbc_style() + 
      scale_fill_manual(values = c("#1380A1", "#FAAB18", "#990000", "#588300", "gray50")) + theme_align
    
  }
  
  theme_align <- theme(
    axis.line = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank(),
    axis.ticks.y = element_blank(), axis.ticks.x = element_line(), 
    axis.text.y = element_text(size = 24), 
    axis.text.x = element_text(size = 24, angle = 45, hjust = 0.9, vjust = 1),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    panel.border = element_blank(), panel.background = element_blank(), 
    legend.position = "none" )
  
  
  
  p_list <- apply(data.frame(low_bound = seq(1, 318, by = 80),
                             upper_bound = c(seq(81, 318, by = 80),319)),
                  1, function(x){
                    plot_align(matrix_sequences_plot, x[1], x[2])
                  }
                  )
  
  
  
  
  p1 <- cowplot::plot_grid(plotlist = p_list, nrow=4, align = "v")
  
  return(cowplot::plot_grid(plotlist = list(p_dend, p1), nrow = 2, rel_heights = c(0.4,0.6), align = "hv"))
}

rect.dendrogram2 <- function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2, 
                              cluster = NULL, horiz = FALSE, density = NULL, angle = 45, 
                              text = NULL, text_cex = 1, text_col = 1, xpd = TRUE, lower_rect, 
                              upper_rect = 0, prop_k_height = 0.5, stop_if_out = FALSE, 
                              ...) 
{
  if (!is.dendrogram(tree)) 
    stop("x is not a dendrogram object.")
  if (length(h) > 1L | length(k) > 1L) {
    stop("'k' and 'h' must be a scalar(i.e.: of length 1)")
  }
  tree_heights <- heights_per_k.dendrogram(tree)[-1]
  tree_order <- order.dendrogram(tree)
  if (!is.null(h)) {
    if (!is.null(k)) {
      stop("specify exactly one of 'k' and 'h'")
    }
    ss_ks <- tree_heights < h
    k <- min(as.numeric(names(ss_ks))[ss_ks])
    k <- max(k, 2)
  }
  else if (is.null(k)) {
    stop("specify exactly one of 'k' and 'h'")
  }
  if (k < 2 | k > length(tree_heights)) {
    if (stop_if_out) {
      stop(gettextf("k must be between 2 and %d", length(tree_heights)), 
           domain = NA)
    }
    else {
      warning(gettextf("k must be between 2 and %d", length(tree_heights)), 
              domain = NA)
    }
  }
  if (is.null(cluster)) {
    cluster <- cutree(tree, k = k)
  }
  clustab <- table(cluster)[unique(cluster[tree_order])]
  m <- c(0, cumsum(clustab))
  if (!is.null(x)) {
    if (!is.null(which)) {
      stop("specify exactly one of 'which' and 'x'")
    }
    which <- x
    for (n in seq_along(x)) which[n] <- max(which(m < x[n]))
  }
  else if (is.null(which)) {
    which <- 1L:k
  }
  if (any(which > k)) {
    stop(gettextf("all elements of 'which' must be between 1 and %d", 
                  k), domain = NA)
  }
  border <- rep_len(border, length(which))
  retval <- list()
  old_xpd <- par()["xpd"]
  par(xpd = xpd)
  for (n in seq_along(which)) {
    next_k_height <- tree_heights[names(tree_heights) == 
                                    k + 1]
    if (length(next_k_height) == 0) {
      next_k_height <- 0
      prop_k_height <- 1
    }
    if (!horiz) {
      xleft <- m[which[n]] + 0.66
      if (missing(lower_rect)) {
        lower_rect <- -max(strheight2(labels(tree)))
        dLeaf <- -0.75 * strheight("x")
        extra_space <- -strheight2("_")
        lower_rect <- lower_rect + dLeaf + extra_space
      }
      ybottom <- lower_rect
      xright <- m[which[n] + 1] + 0.33
      ytop <- tree_heights[names(tree_heights) == k] * 
        prop_k_height + next_k_height * (1 - prop_k_height) + 
        upper_rect
    }
    else {
      ybottom <- m[which[n]] + 0.66
      if (missing(lower_rect)) {
        lower_rect <- min(strwidth(labels(tree)))
        dLeaf <- 0.75 * strwidth("w")
        extra_space <- strwidth("_")
        lower_rect <- lower_rect + dLeaf + extra_space
      }
      xright <- lower_rect
      ytop <- m[which[n] + 1] + 0.33
      xleft <- tree_heights[names(tree_heights) == k] * 
        prop_k_height + next_k_height * (1 - prop_k_height) + 
        upper_rect
    }
    rect(xleft, ybottom, xright, ytop, border = border[n], 
         density = density, angle = angle, ...)
    if (!is.null(text)) {
      text((m[which[n]] + m[which[n] + 1] + 1)/2, ytop+0.01, text[n], 
           cex = text_cex, col = text_col)
    }
    retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
  }
  par(xpd = old_xpd)
  invisible(retval)
}
