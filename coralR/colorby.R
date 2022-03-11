
# Define a function to create a vector of colors based on selected kinases
color.by.selected <- function(df,sel,bg.col,sel.col)
{

 # set background color
  color.vector = rep(bg.col,nrow(df))
  #print("color.vector")
  #print(color.vector)
  # recolor selected kinases
  if (length(sel) > 0)
  {
    color.vector[which(df$id.coral %in% sel)] = sel.col
  }
  #print("recolored color.vector")
  #print(color.vector)
  # return color vector
  return (color.vector)
}

# Define a function creates color vector from group
color.by.group <- function(df,recolordf,colors,bg.col="#D3D3D3",categories=NULL)
{
  # set background color
  color.vector = rep(bg.col,nrow(df))
  
  # keep track of group labels
  group.vector = rep("none",nrow(df))
  
  # get group names
  group.names = names(table(recolordf[,2]))

  # used user supplied groups
  if (is.null(categories) == FALSE)
  {
   group.names = categories
  }
  
  # Determine the number of groups
  numgroups = length(group.names)
  
  # set palette
  if (numgroups > length(colors))
  {
    colors = colorRampPalette(colors)(numgroups)
  }
  pal = colors[1:numgroups]
  
  groupcolormapping = c()
  for (i in 1:numgroups)
  {
    # get group and color
    group.name  = group.names[i]
    group.color = pal[i]
    
    # record the group/color mapping
    groupcolormapping = c(groupcolormapping,group.color)
    names(groupcolormapping)[length(groupcolormapping)] = group.name
    
    # get kinases from this group
    kinsase.from.this.group = recolordf[which(recolordf[,2]==group.name),1]
    
    # update vector of colors
    color.vector[which(df$id.coral %in% kinsase.from.this.group)] = group.color
    group.vector[which(df$id.coral %in% kinsase.from.this.group)] = group.name
  }

  return( list(color.vector,group.vector,groupcolormapping))
}


# Define a function creates color vector from values
color.by.value <- function(df ,recolordf ,colors  ,heatrange , bg.col="#D3D3D3")
{
  # set background color
  color.vector = rep(bg.col,nrow(df))
  
  # kep track of group labels
  value.vector = rep(NA,nrow(df))
  
  # convert to numeric
  recolordf[,2] = as.numeric(recolordf[,2])
  
  # convert to numeric
  recolordf$color = map2color(recolordf[,2],pal = colors, limits = heatrange)
  
  # find indices to recolor
  dflookup = match(recolordf[,1],df[,1])
  
  # update colors and values
  color.vector[dflookup] = recolordf$color
  value.vector[dflookup] = recolordf[,2]

  return (list(color.vector, value.vector))  
}

resizes.by.value <- function(df, 
														 resizedf, 
														 sizerange, # size range = c(1,60) == (min, max)
														 controlledrange = FALSE, 
														 minvalue = 5, 
														 maxvalue = 60,
														 showall ="show", 
														 sel_colm
												    )
 {
  # set values for non supplied kinases
  radius.vector = rep(0,nrow(df))
  if (showall == "show") {
				radius.vector = rep(sizerange[1],nrow(df)) # all min values repeated
  } 
  
  # keep track of group labels
  value.vector = rep(NA,nrow(df))

  # print(head(resizedf))

  # convert to numeric
  tmp <- lapply(resizedf, typeof)

  if (tmp[[sel_colm]] != "double") {
      resizedf[[sel_colm]] = as.numeric(resizedf[[sel_colm]])
      }
  
  if (controlledrange == FALSE)
  {
    # (1) get range
    rangesize = sizerange[2] - sizerange[1]
    
    minvalue = 0
    maxvalue = 100
    if (max(resizedf[[sel_colm]]) > 100) {
				remainder <- max(resizedf[[sel_colm]]) - 100
				resizedf <- resizedf %>%
								mutate(!!sel_colm := !!as.name(sel_colm) - remainder)
				}
  }
  
  if (controlledrange == TRUE)
  {
    # truncate values beyond the range
    resizedf[[sel_colm]][which(resizedf[[sel_colm]] < minvalue)] = minvalue
    resizedf[[sel_colm]][which(resizedf[[sel_colm]] > maxvalue)] = maxvalue
    
    # (1) get range
    rangesize = sizerange[2] - sizerange[1]
  }

  # (2) shift values such that they start at zero
  # no need since always at zero
  radii = resizedf[[sel_colm]]
  
  # (3) scale so max = 1
  radii = radii / maxvalue
  
  # (3) multiply to fit range
  radii = radii * rangesize
  
  # (4) increase all values to be within range
  # radii = radii + sizerange[1]
  
  resizedf$radii = radii
  
  # find indices to resize
  dflookup = match(resizedf$KINASE,df$id.coral)
  
  # update colors and values

  # print(radii)
  # print(paste0(rep("-", 20), collapse = ""))
  # print(dflookup)

  radius.vector[dflookup] = resizedf$radii
  value.vector[dflookup] = resizedf[[sel_colm]]
  resizedf$dflookup = dflookup
  
  return (list(radius.vector, value.vector, resizedf))  
}
