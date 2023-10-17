
##################
# function plots
##################
#' Title
#'
#' @param t
#' @param dbh_max
#' @param k
#' @param c
#'
#' @return
#' @export
#'
#' @examples
eq_dbh <- function(t, dbh_max, k, c) {
  dbh_max * (1 - exp(-k * t)) ^ c
}

eq_dbh_inc <- function(dbh, dbh_max, k, c) {
  dbh * c * k * (((dbh/dbh_max) ^ (-1/c)) - 1)
}

eq_dbh_inc_max <- function(dbh_max, k, c) {
  dbh_max * k * ((1 - (1/c)) ^ (c-1))
}

eq_dbh_k <- function(dbh_max, dbh_inc_max, c) {
  dbh_inc_max/(dbh_max * ((1 - (1/c)) ^ (c-1)))
}



#a=30	b=0.8
#a <- 30*(0.01^0.8) #a_new = a*(0.01^b)
#next a dan b dibalik

#' The allometric equation of DBH (D) and Height (H) of tree in the isolated area.
#'
#' The equation is H = alpha*D^beta
#' The default parameters (alpha=0.8, beta=0.75) is for Hevea brasiliensis species
#'
#' @param dbh The diameter at breast height (DBH) in cm
#' @param alpha The scaling exponent
#' @param beta The proportionality coefficient
#'
#' @return Height of tree in meter (m)
eq_height <- function(dbh, alpha = 0.8, beta = 0.75) {
  beta * (dbh ^ alpha) #reversed a/b parameter from the previous SExI-FS model
}


#' The equation to get the crown width of tree
#'
#' @param dbh
#' @param a
#' @param b
#'
eq_crownwidth <- function(dbh, a = 2, b = 0.24) {
  a + dbh * b
}



#' generate_triangle_idx
#'
#' @param lateral_n
#' @param vertical_n
#'
generate_triangle_idx <- function(lateral_n = LATERAL_DIMENSION,
                                  vertical_n = VERTICAL_DIMENSION) {
  a <- c(1:lateral_n)
  b <- c(a[-1], 1)
  c <- a + lateral_n

  a2 <- c(a, c)
  b2 <- c(b, b)
  c2 <- c(c, c[-1], lateral_n + 1)

  tr_df <- data.frame(a2,b2,c2)
  colnames(tr_df) <- c("a", "b", "c")

  rows= c(1:nrow(tr_df))
  times = vertical_n
  tr_df <- tr_df[rep(rows, times),]

  tr_f <- unlist(lapply(c(0:(vertical_n-1)), function(x){
    rep(x,lateral_n*2)
  }))
  tr_c <- c("a", "b", "c")
  tr_df[tr_c] <- tr_df[tr_c] + tr_f * lateral_n
  rn <- nrow(tr_df)
  tr_df <- tr_df[-c((rn-lateral_n+1):rn),]
  rn <- nrow(tr_df)
  tr_df[c((rn-lateral_n+1):rn), "c"] <- lateral_n * vertical_n + 1
  v <- as.vector(t(as.matrix(tr_df)))
  return(v)
}


LATERAL_DIMENSION <- 15
VERTICAL_DIMENSION <- 15
lateralAngleDiv = pi * 2/LATERAL_DIMENSION
verticalAngleDiv = pi/(VERTICAL_DIMENSION * 2)

triangle_index <- generate_triangle_idx()


#' Generate branches
#'
#' @param crown_depth
#' @param crown_curve_depth
#' @param crown_radius
#' @param crown_radius_init_angle
#'
generate_vbranches <- function(crown_depth = 3,
                               crown_curve_depth = 2,
                               crown_radius = 1,
                               crown_radius_init_angle = 0) {
  crown_curve_depth <- min(max(crown_curve_depth, crown_depth/2), crown_depth)
  radius_count <- length(crown_radius)

  ver_extension <- crown_depth - crown_curve_depth
  extAngle <- ifelse(ver_extension  > 0, asin(ver_extension/crown_curve_depth), 0)
  ver_angle_div <- (extAngle + pi/2)/VERTICAL_DIMENSION
  rangeLatDim <- LATERAL_DIMENSION/radius_count
  rangeFactor <- c(1:radius_count-1) * rangeLatDim

  #generate vertical and horizontal index and fill into dataframe
  ver_id <- unlist(lapply(c(1:VERTICAL_DIMENSION), function(x){
                    rep(x,LATERAL_DIMENSION)
                  }))
  hor_id <- rep(c(1:LATERAL_DIMENSION),VERTICAL_DIMENSION)
  br_df <- data.frame(hor_id, ver_id)
  br_df$br_id <- c(1:nrow(br_df))

  br_df$ver_angle <- (br_df$ver_id - 1) * ver_angle_div - extAngle
  br_df$ver_height <- ver_extension + crown_curve_depth * sin(br_df$ver_angle)

  br_df$hor_angle <- br_df$hor_id * lateralAngleDiv + crown_radius_init_angle
  idx1 <- ceiling(br_df$hor_id/rangeLatDim)
  idx2 <- (idx1) %% radius_count + 1
  hor_radius <- crown_radius[idx1] + (crown_radius[idx2]-crown_radius[idx1])*(br_df$hor_id-rangeFactor[idx1]-1)/rangeLatDim
  br_df$hor_length <- hor_radius * cos(br_df$ver_angle);

  br_df$x <- br_df$hor_length * cos(br_df$hor_angle)
  br_df$y <- br_df$hor_length * sin(br_df$hor_angle)
  br_df$z <- br_df$ver_height
  #add one apex branches
  br_df <- rbind(br_df, c(0, 0, LATERAL_DIMENSION * VERTICAL_DIMENSION + 1,
                 pi, crown_depth, 0, 0, 0, 0, crown_depth))
  return(br_df)
}

get_dtaper <- function(dbh, height, total_height, taper = 1.6, bh = 1.3) {
  #https://en.wikipedia.org/wiki/Tree_taper
  dsqr <- dbh*dbh*((total_height - height)/(total_height - bh))
  dsqr <- pmax(1, dsqr)
  return(sqrt(dsqr))
}

generate_realistic_branches <- function(tree) {

  #main stem
  cbh <- tree$crown_base_height
  cr <- tree$crown_radius_avg/6

  l <- c(cbh, rep((tree$height-cbh)/5, 5))
  l_cum <- head(c(0, cumsum(l)), -1)
  d1 <- get_dtaper(tree$dbh, l_cum, tree$height)
  d2 <- c(d1[-1], 1)
  id <- c(1:6)
  br_df <- data.frame(
    "LinkNumber" = id,
    "Length" = l,
    "Diam1" = d1/100,
    "Diam2" = d2/100,
    "ConnectedTo" = id-1,
    "VerAng" = 0,
    "RotAng" = 0,
    "TopX" = 0,
    "TopY" = cumsum(l),
    "TopZ" = 0
  )
  n <- nrow(br_df)
  br_df[c(2:n), "VerAng"] <- runif(n-1)*5*pi/180
  br_df[c(2:n), "RotAng"] <- runif(n-1)*2*pi

  #plagiothropic
  vb_df <- tree$vbranches #vbranches use field coords vert = z

  pl_df <- data.frame(
    idp = c(7:21),
    con = c(rep(1,3), rep(2,3), rep(3,3), rep(4,3), rep(5,3)),
    ver_id = c(rep(2,3), rep(6,3), rep(9,3), rep(12,3), rep(14,3)),
    hor_id = c(5, 10, 15, 3, 8, 13, 1, 6, 11, 4, 9, 14, 2, 7, 12)
  )

  pl_df <- merge(pl_df, vb_df, by = c("ver_id", "hor_id"), all.x = T, sort = F)
  pl_df <- merge(pl_df, br_df[c("Diam2", "TopX", "TopY", "TopZ", "LinkNumber")],
                 by.x = "con", by.y = "LinkNumber", all.x = T, sort = F)
  pl_df$z <- pl_df$z + cbh

  dist <- function(ax, ay, az, bx, by, bz){
    sqrt((bx-ax)^2 + (by-ay)^2 + (bz-az)^2)
  }

  pl_df$length <- dist(pl_df$TopX, pl_df$TopY, pl_df$TopZ,
                       pl_df$x, pl_df$z, pl_df$y)
  pl_df$verAng <- acos((pl_df$z - pl_df$TopY)/pl_df$length)
  pl_df$rotAng <- atan(pl_df$y/pl_df$x) + ifelse(pl_df$x < 0, pi, 0) + pi
  pl_df$x <-  -sin(pl_df$verAng) * pl_df$length * cos(pl_df$rotAng)
  pl_df$y <-  sin(pl_df$verAng) * pl_df$length * sin(pl_df$rotAng)

  pl_df$hx <- pl_df$x/2
  pl_df$hz <- pl_df$y/2
  pl_df$hy <- pl_df$TopY + (pl_df$z - pl_df$TopY)/2

  leaves_df <- data.frame(
    "x" = pl_df$x * 0.8,
    "y" = pl_df$TopY + (pl_df$z - pl_df$TopY)  * 0.8,
    "z" = pl_df$y * 0.8
  )

  leaves_df2 <- data.frame(
    "x" = pl_df$hx,
    "y" = pl_df$hy,
    "z" = pl_df$hz
  )

  # leaves_df2 <- data.frame(
  #   "x" = c(pl_df$hx, rep(0,3)),
  #   "y" = c(pl_df$hy, br_df[c(1:3), "TopY"]),
  #   "z" = c(pl_df$hz, rep(0,3))
  # )

  leaves_df <- rbind(leaves_df, leaves_df2)


  brp_df <- data.frame(
    "LinkNumber" = pl_df$idp,
    "Length" = pl_df$length,
    "Diam1" = pl_df$Diam2 * 0.7,
    "Diam2" = 1/100,
    "ConnectedTo" = pl_df$con,
    "VerAng" = pl_df$verAng,
    "RotAng" = pl_df$rotAng,
    "TopX" = pl_df$hx,
    "TopY" = pl_df$hy,
    "TopZ" = pl_df$hz
  )

  brp_df2 <- brp_df
  brp_df2$ConnectedTo <- brp_df2$LinkNumber
  brp_df2$LinkNumber <- brp_df2$LinkNumber + nrow(brp_df)
  brp_df2$Length <- brp_df2$Length/2
  brp_df2$Diam1 <- brp_df2$Diam1 * 0.4

  brp_df3 <- brp_df2
  brp_df3$LinkNumber <- brp_df3$LinkNumber + nrow(brp_df2)

  twig_df <- rbind(brp_df2, brp_df3)
  n <- nrow(twig_df)
  twig_df$VerAng <- rnorm(n, twig_df$VerAng, pi/8)
  twig_df$RotAng <- rnorm(n, twig_df$RotAng, pi/3)
  va <- twig_df[twig_df$ConnectedTo %in% c(6,7,8), "VerAng"]
  twig_df[twig_df$ConnectedTo %in% c(6,7,8), "VerAng"] <- va %% pi/2

  # twig_df$TopX <-  twig_df$TopX - sin(twig_df$VerAng) * (twig_df$Length*0.8) * cos(twig_df$RotAng)
  # twig_df$TopZ <-  twig_df$TopZ + sin(twig_df$VerAng) * (twig_df$Length*0.8) * sin(twig_df$RotAng)
  # twig_df$TopY <-  twig_df$TopY + cos(twig_df$VerAng) * (twig_df$Length*0.8)
  twleaves_df <- data.frame(
    "x" = twig_df$TopX - sin(twig_df$VerAng) * (twig_df$Length-cr) * cos(twig_df$RotAng),
    "y" = twig_df$TopY + cos(twig_df$VerAng) * (twig_df$Length-cr),
    "z" = twig_df$TopZ + sin(twig_df$VerAng) * (twig_df$Length-cr) * sin(twig_df$RotAng)
  )
  leaves_df <- rbind(leaves_df, twleaves_df)

  br_df <- rbind(br_df, brp_df)
  br_df <- rbind(br_df, twig_df)

  #return(list("branches" = list(br_df), "leaves" = list(leaves_df)))
  return(list("branches" = br_df, "leaves" = leaves_df))
}

is.blank <- function(x, false.triggers=FALSE){
  if(is.function(x)) return(FALSE) # Some of the tests below trigger
  # warnings when used on functions
  return(
    is.null(x) ||                # Actually this line is unnecessary since
      length(x) == 0 ||            # length(NULL) = 0, but I like to be clear
      all(is.na(x)) ||
      all(x=="") ||
      (false.triggers && all(!x))
  )
}

#' Generate Tree object
#'
#' The function for generating Tree object.
#' The object can be viewed using threeforest().
#'
#' @param dbh Tree size
#' @param height
#' @param crown_base_height
#' @param crown_width_max_height
#' @param crown_radius
#' @param crown_radius_init_angle
#'
#' @return Tree object
#' @export
#'
generate_tree <- function(dbh = 15,
                          height = NULL,
                          crown_base_height = NULL,
                          crown_width_max_height = NULL,
                          crown_radius = NULL,
                          crown_radius_init_angle = 0) {

  if(is.blank(height)) height <- eq_height(dbh)
  if(is.blank(crown_base_height)) crown_base_height <- height/2
  crown_depth <- height - crown_base_height
  if(is.blank(crown_radius)) crown_radius <- eq_crownwidth(dbh)/2
  if(is.blank(crown_width_max_height))
    crown_width_max_height <- crown_base_height + crown_depth/3
  crown_curve_depth <- height - crown_width_max_height


  vbranches <- generate_vbranches(crown_radius = crown_radius,
                                  crown_depth = crown_depth,
                                  crown_curve_depth = crown_curve_depth,
                                  crown_radius_init_angle = crown_radius_init_angle)
  br_tips <- vbranches[c("x", "y", "z")]
  br_tips$z <- br_tips$z + crown_base_height

  v <- as.vector(t(as.matrix(br_tips)))



  t <- list(dbh = dbh,
            height = height,
            crown_base_height = crown_base_height,
            crown_radius = crown_radius,
            crown_radius_avg = mean(crown_radius),
            crown_radius_max = max(crown_radius),
            vbranches = vbranches,
            vertices = v,
            indices = triangle_index - 1,
            crown_color = "#A52A2A",
            is_branch_capped = F
            )
  class(t) <- "tree"

  br <- generate_realistic_branches(t)
  t$branches <- br$branches
  t$leaves <- br$leaves

  return(t)
}




#' Plant trees in a plot of area
#'
#' @param tree_df
#' @param area
#'
#' @return
#' @export
#'
#' @examples
plant <- function(tree_df, area = c(100, 100)) {
  f <- list()
  no_dbh <- "Argument 'tree_df' has no information on 'dbh'"

  if (is.data.frame(tree_df)) {
    if(!("dbh" %in% colnames(tree_df))) stop(no_dbh)
    for(i in 1:nrow(tree_df)) {
      t <- generate_tree(dbh = tree_df[i, "dbh"],
                         height = tree_df[i, "height"],
                         crown_base_height = tree_df[i, "crown_base_height"],
                         crown_width_max_height = tree_df[i, "crown_width_max_height"],
                         crown_radius = tree_df[i, "crown_radius"])
        t$x <- ifelse(is.blank(tree_df[i, "x"]), area[1] * runif(1), tree_df[i, "x"])
        t$y <- ifelse(is.blank(tree_df[i, "y"]), area[2] * runif(1), tree_df[i, "y"])
      id <- ifelse(is.blank(tree_df[i, "id"]), i, tree_df[i, "id"])
      t$id <- id
      f <- append(f, list(id = t))
    }
  } else if(is(tree_df,"numeric")) {
    for(i in 1:length(tree_df)) {
      t <- generate_tree(tree_df[i])
      t$x <- area[1] * runif(1)
      t$y <- area[2] * runif(1)
      t$id <- i
      f <- append(f, list(i = t))
    }
  } else if(is(tree_df,"list")) {
    if(!("dbh" %in% names(tree_df))) stop(no_dbh)
    for(i in 1:length(tree_df[["dbh"]])) {
      t <- generate_tree(dbh = tree_df[["dbh"]][i],
                         height = tree_df[["height"]][i],
                         crown_base_height = tree_df[["crown_base_height"]][i],
                         crown_width_max_height = tree_df[["crown_width_max_height"]][i],
                         crown_radius = tree_df[["crown_radius"]][i])
      id <- ifelse(is.blank(tree_df[["id"]][i]), i, tree_df[["id"]][i])
      t$id <- id
      f <- append(f, list(id = t))
    }

  } else {
    stop("Argument 'tree_df' has unknown format")
  }
  f$area <- area
  class(f) <- "forest"
  return(f)
}


#' Shows the forest
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
plot.forest <- function(x, ...) {
  threeforest(x, ...)
}

# d <- c(10, 20, 30)
# f <- plant(d)
#
# df <- data.frame(dbh = d, height = c(10, 30, 50))
# f2 <- plant(df)
#
# df2 <- list(dbh = d, height = c(10, 30))
# f2 <- plant(df2)
