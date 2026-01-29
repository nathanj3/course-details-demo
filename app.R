library(shiny)
library(bslib)
library(ggplot2)
library(ggiraph)
library(dplyr)

# Sample data
courses <- tibble(
  year = rep(2021:2024, each = 4),
  season = rep(c("Spr", "Sum", "Fall", "Win"), 4),
  course = c(
    "Crocheting", "Gardening", "Painting", "Cooking",
    "Photography", "Woodworking", "Pottery", "Baking",
    "Knitting", "Hiking", "Drawing", "Wine Tasting",
    "Yoga", "Kayaking", "Sculpture", "Cheese Making"
  ),
  enrollment = sample(10:50, 16),
  location = c(
    "Room 101", "Garden", "Studio A", "Kitchen 1",
    "Studio B", "Workshop", "Studio C", "Kitchen 2",
    "Room 102", "Trails", "Studio A", "Tasting Room",
    "Gym", "Lake", "Workshop", "Kitchen 3"
  ),
  topics_covered = c(
    "basic stitches, patterns, yarn types",
    "composting, seasonal planting, pest management, outdoors activities",
    "acrylics, watercolors, color theory, composition",
    "knife skills, meal planning, international cuisine",
    "lighting, composition, editing, portrait techniques",
    "joinery, finishing, tool safety, furniture design",
    "wheel throwing, glazing, firing techniques, hand building",
    "bread making, pastries, cake decorating, gluten-free options",
    "basic patterns, advanced stitches, garment construction",
    "trail navigation, wildlife identification, outdoors safety, camping",
    "charcoal, pencil, perspective, figure drawing",
    "tasting notes, regions, pairing, fermentation",
    "breathing, flexibility, meditation, stress relief",
    "paddling techniques, safety, river navigation, outdoors adventures",
    "clay modeling, casting, finishing, installation",
    "cultures, aging, pairing, fermentation processes"
  ),
  details = c(
    "This comprehensive crocheting course covers everything from basic chain stitches to complex patterns. Students will learn to read patterns, choose appropriate yarn weights, and create their first project—a cozy scarf. Perfect for beginners with no prior experience.",
    "Dive into sustainable gardening practices with hands-on learning in our community garden. Learn about soil health, companion planting, organic pest control, and how to grow vegetables year-round. Includes take-home seedlings.",
    "Explore various painting mediums in this foundational art class. We'll cover color mixing, brush techniques, and composition principles. Each session includes guided practice and critique. All materials provided, and you'll complete 3-4 finished pieces.",
    "Master fundamental cooking techniques in this hands-on culinary course. From knife skills to sauce making, you'll build confidence in the kitchen. Each class includes recipe packets and a shared meal of what we cook together.",
    "Learn the art of digital photography from exposure basics to advanced editing. Covers DSLR and smartphone photography, composition rules, lighting setups, and post-processing in Lightroom. Bring your own camera.",
    "Introduction to woodworking with focus on hand tools and joinery. Build practical skills while creating a cutting board and small shelf. Safety is emphasized throughout. No experience necessary, but participants must be 16+.",
    "Get your hands dirty with clay in this pottery fundamentals course. Learn wheel throwing, hand-building techniques, glazing, and firing processes. Studio access included for practice between classes. Clay and firing fees included.",
    "Professional baker teaches home baking techniques from bread to pastries. Learn about yeast, gluten development, lamination, and decoration. Each participant takes home fresh baked goods weekly. Recipes provided.",
    "Advanced knitting course for those comfortable with basic stitches. Learn to read complex patterns, knit in the round, and construct a sweater. Emphasis on fit, finishing techniques, and troubleshooting common mistakes.",
    "Guided nature hikes focusing on local ecology and trail safety. Learn plant and wildlife identification, Leave No Trace principles, and navigation skills. Different trail each week. Moderate fitness level required.",
    "Develop drawing skills through observation and practice. Covers perspective, shading, proportion, and various drawing media. Weekly assignments encourage regular practice. Suitable for all levels, portfolio development supported.",
    "Journey through wine regions and varietals with expert sommelier. Learn tasting techniques, food pairing principles, and wine storage. Each session features 6-8 wines. Must be 21+. Light appetizers provided.",
    "Gentle yoga class focusing on flexibility, strength, and mindfulness. Appropriate for beginners and those returning to practice. Learn breathing techniques, basic poses, and meditation. Bring your own mat or borrow one.",
    "Learn kayaking basics including paddling strokes, water safety, and river navigation. Equipment provided. Progression from pool practice to calm lake to moving water. Strong swimming ability required. Wetsuit provided.",
    "Three-dimensional art course exploring sculpture techniques and materials. Work with clay, plaster, and found objects. Study contemporary sculptors and develop your artistic voice. Access to studio facilities included.",
    "Artisan cheese making from starter cultures to aging techniques. Make mozzarella, cheddar, and soft cheeses. Learn about milk chemistry, cultures, and proper aging conditions. Take home cheese from each class and recipes."
  )
) %>%
  mutate(course_id = row_number())

ui <- page_sidebar(
  title = "Course Catalog",
  sidebar = sidebar(
    textInput("search", "Search topics:", placeholder = "e.g., 'outdoors'"),
    hr(),
    p("Click any course rectangle to see full details."),
    p("Enter a search term to highlight matching courses."),
    br(),
    hr(),
    p(
      style = "color: #2f4f4f;",
      "Made by", 
      a(href="https://nathan-jeffery.netlify.app/", target="_blank", "Nathan Jeffery"), 
      "in his work at",
      a(href="https://citl.illinois.edu/data-analytics-0", target = "_blank", 
        "CITL Data Analytics"),
      "at the",
      a(href="https://illinois.edu", target="_blank", "University of Illinois"),
      "using",
      a(href="https://gallery.shinyapps.io/assistant/#", target="_blank", "Shiny Assistant", .noWS = "after"),
      ". The code is available on",
      a(href="https://github.com/nathanj3/course-details-demo", target="_blank", "GitHub", .noWS = "after"),
      "."
    )
  ),
  card(
    card_header("Course Schedule (2021-2024)"),
    girafeOutput("course_plot", height = "600px")
  )
)

server <- function(input, output, session) {
  
  # Reactive data with search highlighting
  course_data <- reactive({
    data <- courses
    
    # Add highlight flag based on search
    if (!is.null(input$search) && input$search != "") {
      search_term <- tolower(input$search)
      data <- data %>%
        mutate(
          highlight = grepl(search_term, tolower(topics_covered)),
          opacity = ifelse(highlight, 1, 0.3)
        )
    } else {
      data <- data %>%
        mutate(highlight = FALSE, opacity = 1)
    }
    
    data
  })
  
  output$course_plot <- renderGirafe({
    data <- course_data()
    
    # Create season positions
    season_levels <- c("Spr", "Sum", "Fall", "Win")
    data <- data %>%
      mutate(
        season_num = match(season, season_levels),
        xmin = season_num - 0.4,
        xmax = season_num + 0.4,
        ymin = year - 0.4,
        ymax = year + 0.4
      )
    
    # Create plot
    p <- ggplot(data, aes(xmin = xmin, xmax = xmax, 
                          ymin = ymin, ymax = ymax)) +
      geom_rect_interactive(
        aes(
          tooltip = paste0("<b>", course, "</b><br>",
                          "Location: ", location, "<br>",
                          "Enrollment: ", enrollment),
          data_id = course_id,
          onclick = sprintf("Shiny.setInputValue(\"course_clicked\", %d, {priority: \"event\"})", 
                           course_id),
          fill = ifelse(highlight, "#4A90E2", "#95a5a6"),
          alpha = opacity
        ),
        color = "white",
        linewidth = 2
      ) +
      geom_text(aes(x = (xmin + xmax) / 2, 
                   y = (ymin + ymax) / 2, 
                   label = course),
               size = 12, size.unit = 'pt', color = "black", fontface = "bold") +
      scale_x_continuous(breaks = 1:4, labels = season_levels) +
      scale_y_continuous(breaks = 2021:2024) +
      labs(x = "Season", y = "Year") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = 'none'
      ) +
      coord_fixed(ratio = 1)
    
    girafe(
      ggobj = p,
      width_svg = 10,
      height_svg = 8,
      options = list(
        opts_selection(type = 'single'),
        opts_toolbar(hidden = c('selection', 'zoom', 'misc'))
      )
    )
  })
  
  # Handle course clicks and show modal
  observeEvent(input$course_clicked, {
    course_info <- courses %>%
      filter(course_id == input$course_clicked)
    
    if (nrow(course_info) > 0) {
      showModal(modalDialog(
        title = paste(course_info$course, "-", course_info$season, course_info$year),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        
        tags$div(
          tags$h4("Course Information"),
          tags$hr(),
          tags$p(tags$strong("Location: "), course_info$location),
          tags$p(tags$strong("Enrollment: "), course_info$enrollment, " students"),
          tags$p(tags$strong("Topics Covered: "), course_info$topics_covered),
          tags$hr(),
          tags$h5("Details"),
          tags$p(course_info$details)
        )
      ))
    }
  })
}

shinyApp(ui, server)
