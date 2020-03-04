power_xy <- function(x, y=NULL, xlab=NULL, ylab=NULL,  
                     linkingGroup, 
                     linkingKey, from = -5, to = 5, 
                     ...) {
    if (is.null(xlab)) {xlab <- "x"}
    if (is.null(ylab)) {ylab <- "y"} 
    if (missing(linkingKey)) {
        linkingKey <- paste0(seq(0, length(x) -1))
    }
    if (missing(linkingGroup)) {
        linkingGroup <- deparse(substitute(x))
    }
    
    powerfun <- function(x, alpha) {
        if (alpha == 0) log(x) else (x^alpha-1)/alpha}
    
    scale01 <- function(x) {
        minx <- min(x, na.rm = TRUE)
        maxx <- max(x, na.rm = TRUE)
        (x-minx)/(maxx - minx) + 1
    }
    
    tt <- tktoplevel()
    xs <- scale01(x)
    ys <- scale01(y)
    
    # scatterplot
    p <- l_plot(x=xs, y=ys, xlabel=xlab, ylabel=ylab, 
                linkingGroup = linkingGroup, 
                linkingKey = linkingKey,
                parent=tt, ...)
    # Save the x and y values from p
    xy_p <- data.frame(x = p["x"], y = p["y"])
    
    fit <- lm(y ~ x, data = xy_p)
    # layer fit
    xrng <- range(xy_p$x)
    yhat <- predict(fit, data.frame(x = xrng))
    line <- l_layer_line(p, x = xrng, y = yhat, 
                         linewidth = 3, index = "end")
    
    # histogram on each
    h_x <- l_hist(xs, xlabel=xlab, yshows="density", 
                  linkingGroup = linkingGroup, linkingKey = linkingKey)
    h_y <- l_hist(ys, xlabel=ylab, yshows="density",  
                  linkingGroup = linkingGroup, linkingKey = linkingKey,
                  swapAxes = TRUE)
    # save the original histogram values
    h_x_vals <- h_x["x"]
    h_y_vals <- h_y["x"]
    
    # Set up power transformations
    alpha_x <- tclVar('1')
    alpha_y <- tclVar('1')
    
    # Create two slider scales for the two powers
    # Reverse to and from because scales are vertical
    sx <- tkscale(tt, orient='vertical', label='power x', variable=alpha_x,
                  from=to, to=from, resolution=0.1)
    sy <- tkscale(tt, orient='vertical', label='power y', variable=alpha_y,
                  from=to, to=from, resolution=0.1)
    
    # pack the sliders into the same window as the scatterplot
    tkpack(sy, sx, fill='y', side='right')
    tkpack(p, fill='both', expand=TRUE)
    
    
    # the update function, called when sliders move
    update <- function(...) {
        ## powers
        alphax <- as.numeric(tclvalue(alpha_x))
        alphay <- as.numeric(tclvalue(alpha_y))
        
        ## labels
        xlabel <- if (alphax==0) {
            paste0("log(",xlab,")")
        } else {
            if (alphax==1) {
                xlab
            } else {
                paste0(xlab,"^",alphax)
            }
        }
        ylabel <- if (alphay==0) {
            paste0("log(",ylab,")")
        } else {
            if (alphay==1) {
                ylab
            } else {
                paste0(ylab,"^",alphay)
            }
        }
        # new plot x and y
        p_xnew <- scale01(powerfun(xy_p$x, alphax))
        p_xnew_range <- range(p_xnew)
        p_ynew <- scale01(powerfun(xy_p$y, alphay))
        
        # update the fitted line values
        fit.temp <- lm(p_ynew ~ p_xnew)
        yhat <- predict(fit.temp, data.frame(p_xnew = p_xnew_range))
        l_configure(line, y = yhat, x = p_xnew_range)
        
        # update plot
        l_configure(p,
                    x = p_xnew,
                    y = p_ynew,
                    xlabel = xlabel,
                    ylabel = ylabel
        )
        l_scaleto_world(p)
        
        # update the histograms
        binwidthx <- h_x['binwidth']
        binwidthy <- h_y['binwidth']
        h_x_vals_new <- scale01(powerfun(h_x_vals, alphax))
        h_y_vals_new <- scale01(powerfun(h_y_vals, alphay))
        l_configure(h_x, x = h_x_vals_new,
                    binwidth = binwidthx,
                    #origin = originx,
                    yshows="density",
                    xlabel= xlabel)
        l_configure(h_y, x = h_y_vals_new,
                    binwidth = binwidthy,
                    #origin = originy,
                    yshows="density",
                    xlabel= ylabel)
        l_scaleto_plot(h_x)
        l_scaleto_plot(h_y)
    }
    # end of update function
    # 
    # attach the update to the sliders
    tkconfigure(sx, command=update)
    tkconfigure(sy, command=update)
    invisible(p)
}