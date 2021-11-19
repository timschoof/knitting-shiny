
# determine stitch location of bobbles
set_bobble_locations <- function(first_bobble, nstitches, tot_bobble_rows, bobble_freq, nknit_start, jitter, jitter_amount, method){
  for (i in 1:tot_bobble_rows) {
    if (i == 1) {
      x_add = seq(from = first_bobble, to = nstitches, by = bobble_freq)
      x = x_add
      bobbles_row = length(x)
      y = rep(i+nknit_start,bobbles_row)
    }
    else {
      if (method == "circular") {
        prev_bobble = x_add[length(x_add)]
        next_bobble = bobble_freq - (nstitches - prev_bobble)
        x_add = seq(from = next_bobble, to = nstitches, by = bobble_freq)
      }
      else {
        if ((i %% 2) == 0) {
          # even rows
          prev_bobble = x_add[length(x_add)]
          next_bobble = nstitches - prev_bobble
          
          odd_dir_bobble = nstitches - (bobble_freq - next_bobble) + 1
          
          x_add = seq(from = odd_dir_bobble, to = 1, by = -1*bobble_freq)
        }
        else {
          # odd rows
          prev_bobble = x_add[length(x_add)]
          next_bobble = (bobble_freq - prev_bobble) + 1
          
          x_add = seq(from = next_bobble, to = nstitches, by = bobble_freq)
        }
      }
      bobbles_row = length(x_add)
      x = append(x, x_add)
      y_add = rep(i+nknit_start,bobbles_row)
      y = append(y,y_add)
    }
  }
  # create data frame
  # optional: add jitter along x-axis (i.e. stitches)
  if (jitter == "no") {
    df = tibble(x = x,
                y = y)
  } else {
    df = tibble(x = round(jitter(x, amount = jitter_amount)),
                y = y)
    # prevent values from going negative
    df = df %>%
      mutate(x = if_else(x < 1, 1, x))
  }
  return(df)
}

# compute inter-bobble timings
inter_bobble_timing <- function(df) {
  dft <- df %>%
    arrange(y,x) %>%
    group_by(y) %>%
    group_modify(~ add_row(.x, x = nstitches + 1)) %>%
    mutate(knit = x -1,
           diff = lag(x),
           k = if_else(is.na(diff), knit, knit-diff),
           index = paste("ind_", row_number(), sep ="")) %>%
    select(-knit, -diff) %>%
    ungroup()
  
  return(dft)
}

# write out pattern
write_pattern <- function(dft, nrows, nstitches, nknit_start, nknit_end) {
  pat <- dft %>%
    select(-x) %>%
    pivot_wider(id_cols = y, names_from = index, values_from = k) %>%
    rowwise() %>%
    # write out pattern
    unite_("pattern", colnames(.)[-1]) %>%
    mutate(pattern = str_replace(pattern, regex("^0_"), "mb, k"),
           pattern = str_remove(pattern, "_0"),
           pattern = str_replace_all(pattern, "_", ", mb, k"),
           pattern = str_remove(pattern, ", mb, kNA"),
           pattern = paste("k", pattern, sep = ""),
           pattern = str_replace(pattern, "kmb", "mb"),
           row = paste("Row ", y, ":", sep = "")) %>% 
    # add first and last rounds of knitting
    add_row(y = 1, row = paste("Row 1-", nknit_start, ":", sep = ""), pattern = paste("k", nstitches, sep = "")) %>%
    add_row(y = nrows-(nknit_end-1), row = paste("Row ",nrows-(nknit_end-1), "-", nrows, ":", sep = ""), pattern = paste("k", nstitches, sep = "")) %>%
    arrange(y) %>%
    # reorder columns
    select(-y) %>%
    relocate(row)
    
    return(pat)
}

# plot knitting pattern
plot_pattern <- function(df, nrows, nstitches, method) {
  # specify y-axis labels
  y_left = rep("",nrows)
  y_right = rep("",nrows)
  y_left[seq(from = 2, to = nrows, by = 2)] <- seq(from = 2, to = nrows, by = 2)
  y_right[seq(from = 1, to = nrows, by = 2)] <- seq(from = 1, to = nrows, by = 2)
  
  # plot
  p = df %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(size = 3, stroke = 1, colour = "#9e9e00") #pch = 15, 
  if (method == "circular") {
    p = p + scale_y_continuous(breaks = seq(from = 1, to = nrows, by = 1), position = "right")
  } else {
    p = p + scale_y_continuous(breaks = seq(from = 1, to = nrows, by = 1),
                               labels = y_left,
                               sec.axis = sec_axis(~ . *1,
                                                   breaks = seq(from = 1, to = nrows, by = 1),
                                                   labels = y_right))
  }
  p = p + scale_x_reverse(breaks = seq(from = nstitches, to = 1, by = -1)) +
    coord_fixed(xlim =c(nstitches+0.5, 0.5), ylim = c(0.5,nrows+0.5), clip = "on", expand = FALSE) +
    labs(title = "",
         subtitle = ,
         y = "",
         x = "") + 
    theme_bw() +
    theme(panel.border = element_blank(), #panel.grid.major = element_blank(),
          panel.grid.major = element_line(size = 0.25, colour= "#b8b800"),
          panel.grid.minor = element_line(size = 0.25, colour= "#b8b800"),
          #axis.title = element_blank(),
          axis.ticks = element_blank(),
          #axis.text.y = element_text(size = 10, face = "bold", colour = "black"),
          #axis.text.x = element_blank(),
          axis.text = element_blank())
  
  return(p)
}
