library(shiny)

shinyServer( function (input, output, session) {

### load the data set
    inputData <- shiny::reactive({
        message("inputData ", input$dataSet)
        loadDataSet(input$dataSet)
    })

### extract colnames from data set and put into the choose
### variables widget
    shiny::observe({
        indata <- inputData()
        varnames <- colnames(indata@data)
        metanames <- colnames(as(indata, 'data.frame'))
        
        message("update var lists ", varnames)
        shiny::updateSelectInput(
            session, 'varNames',
            choices = varnames,
            selected = varnames[1:3]
        )
        message("update meta var lists", metanames)
        shiny::updateSelectInput(
            session, 'colorNames',
            choices = metanames,
            selected = metanames[1:3]
        )
    })

### draw onto the output canvas
    output$outPlot <- shiny::renderPlot({
        message("rendering Plots")
        orgDataOut <- input$orgDataOutputType
        varn <- input$varNames
        metan <- input$colorNames
        tmpInput <- as(inputData(), 'data.frame')
        tmpData <- tmpInput[,varn]
        tmpColData <- tmpInput[,metan, drop = FALSE]
        tmpRGB <- colorize(tmpColData)
        switch(
            orgDataOut,
            'pairpl' = pairs(tmpData, gap = 0, col = tmpRGB),
            'parpl'  = MASS::parcoord(tmpData, col = tmpRGB),
            '2vars'  = plot(tmpData[,1:2], col = tmpRGB),
            plot.new()
        )
    })

    output$outPlot3d <- shinyRGL::renderWebGL({
        message("rendering Plots")
        orgDataOut <- input$orgDataOutputType
        varn <- input$varNames
        metan <- input$colorNames
        tmpInput <- as(inputData(), 'data.frame')
        tmpData <- tmpInput[,varn]
        tmpColData <- tmpInput[,metan, drop = FALSE]
        tmpRGB <- colorize(tmpColData)
        switch(
            orgDataOut,
            '3vars'  = rgl::points3d(tmpData[,1:3], col = tmpRGB)
        )
    })

### embed data
    embData <- shiny::reactive({
        message("calculating embedding")
        data <- inputData()
        method <- input$dimRedMethod
        emb <- switch(
            method,
            'PCA'                     = function (x) pca@fun(x),
            'kPCA'                    = function (x) kpca@fun(x),
            'DRR'                     = function (x) drr@fun(x),
            'Isomap'                  = function (x) isomap@fun(x),
            't-SNE'                   = function (x) tsne@fun(x),
            'NMDS'                    = function (x) nmds@fun(x),
            'MDS'                     = function (x) mds@fun(x),
            'fastICA'                 = function (x) fastica@fun(x),
            'Diffusion Map'           = function (x) diffmap@fun(x),
            'Laplacian Eigenmaps'     = function (x) leim@fun(x),
            'Local Linear Embedding'  = function (x) lle@fun(x),
            'Local Ordinal Embedding' = function (x) loe@fun(x),
            'Soft Ordinal Embedding'  = function (x) soe@fun(x),
            'Fruchterman Reingold'    = function (x) fruchterman_reingold@fun(x),
            'DrL'                     = function (x) drl@fun(x),
            'Kamada Kawai'            = function (x) kamada_kawai@fun(x),
            function (x) new('dimRedResult', data = data, org.data = data@data,
                             apply = I, inverse = I, has.org.data = TRUE,
                             has.apply = TRUE, has.inverse = TRUE)
        )
        emb(data)
    })

### extract colnames from embedded data set and put into the choose
### variables widget
    shiny::observe({
        indata <- embData()
        varnames <- colnames(indata@data@data)
        metanames <- colnames(as(indata, 'data.frame'))
        
        message("update emb var lists ", varnames)
        shiny::updateSelectInput(
            session, 'embAxes',
            choices = varnames,
            selected = varnames[1:3]
        )
        message("update emb meta var lists", metanames)
        shiny::updateSelectInput(
            session, 'embColors',
            choices = metanames,
            selected = metanames[1:3]
        )
    })
    
### draw onto the embedded output canvas
    output$outEmbed <- shiny::renderPlot({
        message("rendering embedded Plots")
        orgDataOut <- input$embDataOutputType
        varn <- input$embAxes
        metan <- input$embColors
        tmpInput <- as(embData(), 'data.frame')
        tmpData <- tmpInput[,varn]
        tmpColData <- tmpInput[,metan, drop = FALSE]
        tmpRGB <- colorize(tmpColData)
        switch(
            orgDataOut,
            'pairpl' = pairs(tmpData, gap = 0, col = tmpRGB),
            'parpl'  = MASS::parcoord(tmpData, col = tmpRGB),
            '2vars'  = plot(tmpData[,1:2], col = tmpRGB),
            plot.new()
        )
    })

    output$outEmbed3d <- shinyRGL::renderWebGL({
        message("rendering Plots")
        orgDataOut <- input$embDataOutputType
        varn <- input$embAxes
        metan <- input$embColors
        tmpInput <- as(embData(), 'data.frame')
        tmpData <- tmpInput[,varn]
        tmpColData <- tmpInput[,metan, drop = FALSE]
        tmpRGB <- colorize(tmpColData)
        switch(
            orgDataOut,
            '3vars'  = rgl::points3d(tmpData[,1:3], col = tmpRGB)
        )
    })

    
}) # end shinyserver
