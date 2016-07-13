library(shiny)

shinyUI( shiny::fluidPage(
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::titlePanel('Parameters'),
### choose data set
            shiny::selectInput(
                'dataSet',
                'Choose a dataset',
                choices = c(
                    'Swiss Roll',       
                    'broken Swiss Roll',
                    'Helix',
                    'Twin Peaks',
                    'Sphere',              
                    'Ball',                
                    '3D S Curve',
                    'variable Noise Helix',
                    'Iris'
                )
            ),

### choose original data plot type
            shiny::selectInput(
                'orgDataOutputType',
                'Choose a method for visualization of the original data',
                choices = c(
                    '2 Variables'   = '2vars',
                    '3 Variables'   = '3vars',
                    'Parallel Plot' = 'parpl',
                    'Pairs Plot'    = 'pairpl'
                )
            ),
### choose variable of original data for plotting axes
            shiny::selectInput(
                'varNames',
                'choose variables to show',
                multiple = TRUE,
                choices = c()
            ),
### choose meta values of original data for coloring
            shiny::selectInput(
                'colorNames',
                'choose variables to color the plot',
                multiple = TRUE,
                choices = c()
            ),

### choose method for dimensionality reduction
            shiny::selectInput(
                'dimRedMethod',
                'Choose a method for dimensionality reduction',
                choices = c(
                    'Original Data',
                    'PCA',
                    'kPCA',
                    'DRR',
                    'Isomap',
                    't-SNE',
                    'NMDS',
                    'MDS',
                    'fastICA',
                    'Diffusion Map',
                    'Laplacian Eigenmaps',
                    'Local Linear Embedding',
                    'Local Ordinal Embedding',
                    'Soft Ordinal Embedding',
                    'Fruchterman Reingold', 
                    'DrL',                  
                    'Kamada Kawai'
                )
                ),
            
### choose visualization of dimensionality reduction
            shiny::selectInput(
                'embDataOutputType',
                'Choose a method for visualization of the embedded data',
                choices = c(
                    '2 Variables'   = '2vars',
                    '3 Variables'   = '3vars',
                    'Parallel Plot' = 'parpl',
                    'Pairs Plot'    = 'pairpl'
                )
            ),
### choose axes of dimensionality reduction for axes of plotting
            shiny::selectInput(
                'embAxes',
                'choose embedding axes as axes for plotting',
                multiple = TRUE,
                choices = c()
            ),
### choose variables for coloring
            shiny::selectInput(
                'embColors',
                'choose variables for coloring',
                multiple = TRUE,
                choices = c(0)
            )
            
        ),

        shiny::mainPanel(
### the plots or the original data are shown here
            shiny::titlePanel('Original Data'),
            shiny::conditionalPanel(
                "input.orgDataOutputType != '3vars'",            
                shiny::plotOutput('outPlot')
            ),
            shiny::conditionalPanel(
                "input.orgDataOutputType == '3vars'",
                shinyRGL::webGLOutput('outPlot3d')
            ),
### the plots or the embedded data are shown here
            shiny::titlePanel('Embedded Data'),
            shiny::conditionalPanel(
                "input.embDataOutputType != '3vars'",
                shiny::plotOutput('outEmbed')
            ),
            shiny::conditionalPanel(
                "input.embDataOutputType == '3vars'",
                shinyRGL::webGLOutput('outEmbed3d')
            )
        )
        
    )
))
