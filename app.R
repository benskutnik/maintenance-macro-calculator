# Maintenance Calorie and Macronutrient Calculator
# Single-file Shiny app using maintenance-only nutrition-dashboard logic.

library(shiny)
library(bslib)
library(tibble)
library(glue)
library(scales)
library(purrr)

# Footer link.
site_url <- "https://benskutnik.com/?ref=macro-calculator"

# Whole numbers with commas, such as 2,950.
format_number <- label_comma(accuracy = 1)

# One color per macronutrient, used in the targets table and the bar.
macro_colors <- c(Protein = "#111111", Fat = "#8A8A8A", Carbohydrates = "#D9D9D9")

# ---------- Calculation helpers ----------

convert_height_to_cm <- function(unit_system, height_ft = NA, height_in_extra = NA, height_cm = NA) {
  if (unit_system == "Imperial") {
    total_inches <- (height_ft * 12) + height_in_extra
    total_inches * 2.54
  } else {
    height_cm
  }
}

convert_weight_to_kg <- function(unit_system, weight_lb = NA, weight_kg = NA) {
  if (unit_system == "Imperial") {
    weight_lb * 0.45359237
  } else {
    weight_kg
  }
}

calculate_rmr <- function(sex, age, height_cm, weight_kg, body_fat_percent = NA) {
  has_body_fat <- !is.na(body_fat_percent) && body_fat_percent > 0

  if (has_body_fat) {
    fat_free_mass_kg <- weight_kg * (1 - body_fat_percent / 100)
    rmr <- 500 + (22 * fat_free_mass_kg)
    method <- "Cunningham equation, using fat-free mass"
  } else if (sex == "Male") {
    fat_free_mass_kg <- NA_real_
    rmr <- 88.362 + (13.397 * weight_kg) + (4.799 * height_cm) - (5.677 * age)
    method <- "Revised Harris-Benedict equation"
  } else {
    fat_free_mass_kg <- NA_real_
    rmr <- 447.593 + (9.247 * weight_kg) + (3.098 * height_cm) - (4.33 * age)
    method <- "Revised Harris-Benedict equation"
  }

  list(
    rmr = rmr,
    method = method,
    fat_free_mass_kg = fat_free_mass_kg,
    has_body_fat = has_body_fat
  )
}

activity_multiplier_base <- function(activity_level) {
  switch(
    activity_level,
    "Low" = 1.2,
    "Moderate" = 1.4,
    "High" = 1.6
  )
}

training_adjustment <- function(training_sessions) {
  switch(
    training_sessions,
    "0" = 0,
    "1 to 3" = 0.1,
    "4 to 6" = 0.2,
    "7+" = 0.3
  )
}

protein_multiplier <- function(has_body_fat, protein_preference) {
  if (has_body_fat) {
    switch(
      protein_preference,
      "Low" = 2.0,
      "Medium" = 2.4,
      "High" = 2.75
    )
  } else {
    switch(
      protein_preference,
      "Low" = 1.6,
      "Medium" = 1.9,
      "High" = 2.2
    )
  }
}

calculate_protein <- function(weight_kg, fat_free_mass_kg, has_body_fat, protein_preference) {
  multiplier <- protein_multiplier(has_body_fat, protein_preference)

  if (has_body_fat) {
    fat_free_mass_kg * multiplier
  } else {
    weight_kg * multiplier
  }
}

calculate_maintenance_targets <- function(
  sex,
  age,
  unit_system,
  height_ft = NA,
  height_in_extra = NA,
  height_cm = NA,
  weight_lb = NA,
  weight_kg = NA,
  body_fat_percent = NA,
  activity_level,
  training_sessions,
  protein_preference
) {
  height_cm_final <- convert_height_to_cm(unit_system, height_ft, height_in_extra, height_cm)
  weight_kg_final <- convert_weight_to_kg(unit_system, weight_lb, weight_kg)

  rmr_result <- calculate_rmr(
    sex = sex,
    age = age,
    height_cm = height_cm_final,
    weight_kg = weight_kg_final,
    body_fat_percent = body_fat_percent
  )

  base_multiplier <- activity_multiplier_base(activity_level)
  training_bump <- training_adjustment(training_sessions)
  adjusted_multiplier <- base_multiplier + training_bump
  maintenance_calories <- rmr_result$rmr * adjusted_multiplier

  protein_g <- calculate_protein(
    weight_kg = weight_kg_final,
    fat_free_mass_kg = rmr_result$fat_free_mass_kg,
    has_body_fat = rmr_result$has_body_fat,
    protein_preference = protein_preference
  )

  fat_floor_g <- ((height_cm_final - 150) / 2) + 30
  fat_percent_g <- maintenance_calories * 0.275 / 9
  fat_g <- max(fat_floor_g, fat_percent_g)
  carb_g <- max(0, (maintenance_calories - (protein_g * 4) - (fat_g * 9)) / 4)

  protein_kcal <- protein_g * 4
  fat_kcal <- fat_g * 9
  carb_kcal <- carb_g * 4

  fat_free_mass_text <- if (is.na(rmr_result$fat_free_mass_kg)) {
    "Not used"
  } else {
    glue("{round(rmr_result$fat_free_mass_kg, 1)} kg")
  }

  detail_table <- tibble(
    metric = c(
      "Height",
      "Weight",
      "Fat-free mass",
      glue("Resting metabolic rate ({rmr_result$method})"),
      "Activity multiplier"
    ),
    value = c(
      glue("{round(height_cm_final, 1)} cm"),
      glue("{round(weight_kg_final, 1)} kg"),
      fat_free_mass_text,
      glue("{format_number(rmr_result$rmr)} kcal/day"),
      glue("{adjusted_multiplier} ({base_multiplier} daily activity + {training_bump} training)")
    )
  )

  list(
    height_cm = height_cm_final,
    weight_kg = weight_kg_final,
    fat_free_mass_kg = rmr_result$fat_free_mass_kg,
    rmr = rmr_result$rmr,
    maintenance_calories = maintenance_calories,
    protein_g = protein_g,
    fat_g = fat_g,
    carb_g = carb_g,
    protein_kcal = protein_kcal,
    fat_kcal = fat_kcal,
    carb_kcal = carb_kcal,
    protein_pct = protein_kcal / maintenance_calories * 100,
    fat_pct = fat_kcal / maintenance_calories * 100,
    carb_pct = carb_kcal / maintenance_calories * 100,
    detail_table = detail_table
  )
}

# ---------- UI helpers ----------

info_panel <- function(title, ...) {
  wellPanel(
    h4(title),
    ...
  )
}

# ---------- UI ----------

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#111111",
    primary = "#6D2838",
    # Franklin Gothic URW from the benskutnik.com Adobe Fonts web project,
    # loaded in tags$head() below. The system fonts are the fallback.
    base_font = font_collection(
      "franklin-gothic-urw", "system-ui", "-apple-system", "Segoe UI",
      "Helvetica Neue", "Arial", "sans-serif"
    ),
    "headings-font-weight" = 700,
    "border-radius" = "0",
    "border-radius-sm" = "0",
    "border-radius-lg" = "0"
  ),

  tags$head(
    tags$link(rel = "icon", href = "favicon-48.png"),
    tags$link(rel = "apple-touch-icon", href = "apple-touch-icon.png"),
    tags$link(rel = "stylesheet", href = "https://use.typekit.net/lvf3qiv.css"),
    tags$link(rel = "stylesheet", href = "styles.css")
  ),

  titlePanel("Maintenance calorie and macronutrient calculator"),

  tabsetPanel(
    tabPanel(
      "Start here",
      br(),
      fluidRow(
        column(
          width = 6,
          info_panel(
            "What this calculator provides",
            tags$p("This calculator estimates maintenance calories and gives a starting macronutrient breakdown. It does not create a fat-loss deficit or muscle-gain surplus."),
            tags$p("Use the results as a starting point, then compare them with body weight trends, training performance, hunger, and recovery over time.")
          ),
          info_panel(
            "RMR method",
            tags$p("If body fat percentage is entered, the calculator uses the Cunningham equation because it can estimate resting needs from fat-free mass."),
            tags$p("If body fat percentage is left blank, it uses the revised Harris-Benedict equation."),
            tags$p("If you do not have a recent and accurate measure of body fat, it is better to leave this blank than to guess.")
          ),
          info_panel(
            "Protein preference",
            tags$p("If body fat percentage is left blank, protein is based on total body weight:"),
            tags$ul(
              tags$li("Low: 1.6 g/kg body weight, or 0.73 g/lb"),
              tags$li("Medium: 1.9 g/kg body weight, or 0.86 g/lb"),
              tags$li("High: 2.2 g/kg body weight, or 1.00 g/lb")
            ),
            tags$p("If body fat percentage is entered, protein is based on fat-free mass:"),
            tags$ul(
              tags$li("Low: 2.0 g/kg fat-free mass, or 0.91 g/lb"),
              tags$li("Medium: 2.4 g/kg fat-free mass, or 1.09 g/lb"),
              tags$li("High: 2.75 g/kg fat-free mass, or 1.25 g/lb")
            )
          )
        ),
        column(
          width = 6,
          info_panel(
            "Activity multiplier",
            tags$p("Choose the option that best describes your normal daily movement outside of structured resistance training."),
            tags$ul(
              tags$li(strong("Low activity: "), "You work a desk job, drive most places, average under 5,000 steps per day, walk occasionally but do not exercise, or are in a rehab, injury, or deliberate rest phase."),
              tags$li(strong("Moderate activity: "), "You average 6,000 to 8,000 steps per day, play recreational sport about once per week, or stand and walk part of the day without labor-intensive work."),
              tags$li(strong("High activity: "), "You average 8,000 to 12,000 steps per day, or your work keeps you on your feet most of the day, such as teaching, nursing, retail, or coaching.")
            ),
            tags$p("Resistance training frequency is counted separately so structured training is not mixed together with daily movement.")
          ),
          info_panel(
            "Fat and carbohydrate targets",
            tags$p("Fat is set at the higher of the height-based floor or 27.5% of maintenance calories."),
            tags$p("Carbohydrates fill the remaining calories after protein and fat are set."),
            tags$p("Carbs and fats can be adjusted offline based on preference or training demands, as long as total calories stay unchanged.")
          )
        )
      )
    ),

    tabPanel(
      "Calculator",
      br(),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "unit_system",
            label = strong("Unit system"),
            choices = c("Imperial", "Metric"),
            selected = "Imperial",
            inline = TRUE
          ),

          conditionalPanel(
            condition = "input.unit_system == 'Imperial'",
            numericInput("height_ft", strong("Height, feet"), value = 5, min = 3, max = 8, step = 1),
            numericInput("height_in_extra", strong("Additional inches"), value = 10, min = 0, max = 11, step = 1),
            numericInput("weight_lb", strong("Weight, pounds"), value = 180, min = 50, max = 700, step = 1)
          ),

          conditionalPanel(
            condition = "input.unit_system == 'Metric'",
            numericInput("height_cm", strong("Height, centimeters"), value = 178, min = 90, max = 250, step = 1),
            numericInput("weight_kg", strong("Weight, kilograms"), value = 82, min = 25, max = 320, step = 0.5)
          ),

          numericInput("age", strong("Age"), value = 35, min = 10, max = 100, step = 1),

          radioButtons(
            inputId = "sex",
            label = strong("Sex"),
            choices = c("Female", "Male"),
            selected = "Male",
            inline = TRUE
          ),

          numericInput(
            inputId = "body_fat_percent",
            label = strong("Body fat percentage, optional"),
            value = NA,
            min = 3,
            max = 65,
            step = 0.1
          ),

          selectInput(
            inputId = "activity_level",
            label = strong("General activity level"),
            choices = c("Low", "Moderate", "High"),
            selected = "Moderate"
          ),

          selectInput(
            inputId = "training_sessions",
            label = strong("Resistance training sessions per week"),
            choices = c("0", "1 to 3", "4 to 6", "7+"),
            selected = "4 to 6"
          ),

          selectInput(
            inputId = "protein_preference",
            label = strong("Protein target"),
            choices = c("Low", "Medium", "High"),
            selected = "Medium"
          ),

          helpText("See the Start here tab for definitions and calculation notes."),
          helpText("If a value looks wrong after switching units, check height and weight before using the result.")
        ),

        mainPanel(
          uiOutput("validation_notes"),
          p(
            class = "result-sentence",
            "Your maintenance is about ",
            strong(textOutput("maintenance_calories", inline = TRUE)),
            " a day."
          ),
          p(
            class = "result-note",
            "That's a starting point. Adjust it based on body weight trends, training performance, hunger, and recovery."
          ),
          h4("Daily targets"),
          tableOutput("macro_table"),
          uiOutput("macro_bar"),
          h4("Calculation details"),
          tableOutput("results_table")
        )
      )
    )
  ),

  tags$footer(
    class = "site-footer",
    tags$a(
      href = site_url,
      target = "_blank",
      "Made by",
      tags$img(src = "ben-skutnik-wordmark-oxblood.svg", alt = "Ben Skutnik", height = 14)
    )
  )
)

# ---------- Server ----------

server <- function(input, output, session) {

  converting_units <- reactiveVal(FALSE)

  observeEvent(input$unit_system, {
    if (converting_units()) {
      return()
    }

    converting_units(TRUE)
    on.exit(converting_units(FALSE), add = TRUE)

    if (input$unit_system == "Metric") {
      current_height_cm <- convert_height_to_cm(
        "Imperial",
        height_ft = input$height_ft,
        height_in_extra = input$height_in_extra
      )
      current_weight_kg <- convert_weight_to_kg(
        "Imperial",
        weight_lb = input$weight_lb
      )

      if (!is.na(current_height_cm)) {
        updateNumericInput(session, "height_cm", value = round(current_height_cm, 1))
      }
      if (!is.na(current_weight_kg)) {
        updateNumericInput(session, "weight_kg", value = round(current_weight_kg, 1))
      }
    } else {
      current_total_inches <- input$height_cm / 2.54
      current_feet <- floor(current_total_inches / 12)
      current_inches <- round(current_total_inches - (current_feet * 12))

      if (!is.na(current_inches) && current_inches == 12) {
        current_feet <- current_feet + 1
        current_inches <- 0
      }

      current_weight_lb <- input$weight_kg / 0.45359237

      if (!is.na(current_feet) && !is.na(current_inches)) {
        updateNumericInput(session, "height_ft", value = current_feet)
        updateNumericInput(session, "height_in_extra", value = current_inches)
      }
      if (!is.na(current_weight_lb)) {
        updateNumericInput(session, "weight_lb", value = round(current_weight_lb, 1))
      }
    }
  }, ignoreInit = TRUE)

  clean_inputs <- reactive({
    validate(
      need(!is.na(input$age) && input$age >= 10 && input$age <= 100, "Enter an age between 10 and 100."),
      need(input$sex %in% c("Female", "Male"), "Select sex."),
      need(input$activity_level %in% c("Low", "Moderate", "High"), "Select a general activity level."),
      need(input$training_sessions %in% c("0", "1 to 3", "4 to 6", "7+"), "Select resistance training sessions per week."),
      need(input$protein_preference %in% c("Low", "Medium", "High"), "Select a protein target.")
    )

    if (input$unit_system == "Imperial") {
      validate(
        need(!is.na(input$height_ft) && input$height_ft >= 3 && input$height_ft <= 8, "Enter height in feet."),
        need(!is.na(input$height_in_extra) && input$height_in_extra >= 0 && input$height_in_extra <= 11, "Enter additional inches from 0 to 11."),
        need(!is.na(input$weight_lb) && input$weight_lb >= 50 && input$weight_lb <= 700, "Enter weight in pounds.")
      )
    } else {
      validate(
        need(!is.na(input$height_cm) && input$height_cm >= 90 && input$height_cm <= 250, "Enter height in centimeters."),
        need(!is.na(input$weight_kg) && input$weight_kg >= 25 && input$weight_kg <= 320, "Enter weight in kilograms.")
      )
    }

    height_cm_final <- convert_height_to_cm(
      unit_system = input$unit_system,
      height_ft = input$height_ft,
      height_in_extra = input$height_in_extra,
      height_cm = input$height_cm
    )
    weight_kg_final <- convert_weight_to_kg(
      unit_system = input$unit_system,
      weight_lb = input$weight_lb,
      weight_kg = input$weight_kg
    )
    bmi <- weight_kg_final / ((height_cm_final / 100) ^ 2)

    validate(
      need(!is.na(height_cm_final) && height_cm_final >= 90 && height_cm_final <= 250, "Check height. The calculator accepts 90 to 250 cm."),
      need(!is.na(weight_kg_final) && weight_kg_final >= 25 && weight_kg_final <= 320, "Check weight. The calculator accepts 25 to 320 kg."),
      need(!is.na(bmi) && bmi >= 12 && bmi <= 80, "The height and weight combination looks unrealistic. Check that the correct unit system is selected.")
    )

    if (!is.na(input$body_fat_percent)) {
      validate(
        need(input$body_fat_percent >= 3 && input$body_fat_percent <= 65, "Enter body fat percentage from 3 to 65, or leave it blank.")
      )
    }

    list(
      height_cm_final = height_cm_final,
      weight_kg_final = weight_kg_final,
      bmi = bmi
    )
  })

  validation_messages <- reactive({
    inputs <- clean_inputs()
    messages <- character(0)

    if (inputs$bmi < 16 || inputs$bmi > 45) {
      messages <- c(messages, "The height and weight combination is unusual. Check that the units and entries are correct before using the result.")
    }

    if (!is.na(input$age) && (input$age < 18 || input$age > 80)) {
      messages <- c(messages, "This calculator is intended primarily for adults. Results may be less reliable for teens or older adults.")
    }

    if (!is.na(input$body_fat_percent)) {
      if ((input$sex == "Male" && input$body_fat_percent < 5) ||
          (input$sex == "Female" && input$body_fat_percent < 10) ||
          input$body_fat_percent > 45) {
        messages <- c(messages, "The body fat percentage entered is uncommon. Use only a recent and accurate measurement, or leave the field blank.")
      }
    }

    messages
  })

  results <- reactive({
    clean_inputs()

    calculate_maintenance_targets(
      sex = input$sex,
      age = input$age,
      unit_system = input$unit_system,
      height_ft = input$height_ft,
      height_in_extra = input$height_in_extra,
      height_cm = input$height_cm,
      weight_lb = input$weight_lb,
      weight_kg = input$weight_kg,
      body_fat_percent = input$body_fat_percent,
      activity_level = input$activity_level,
      training_sessions = input$training_sessions,
      protein_preference = input$protein_preference
    )
  })

  output$validation_notes <- renderUI({
    messages <- validation_messages()

    if (length(messages) == 0) {
      return(NULL)
    }

    div(
      class = "alert alert-warning",
      tags$strong("Check your inputs"),
      tags$ul(lapply(messages, tags$li))
    )
  })

  # Rounded to the nearest 50, since the result is an estimate.
  output$maintenance_calories <- renderText({
    calories <- round(results()$maintenance_calories / 50) * 50
    glue("{format_number(calories)} calories")
  })

  # One row per macronutrient. The small square matches its color in the bar.
  output$macro_table <- renderTable({
    r <- results()
    tibble(
      Macronutrient = glue("<span class='swatch' style='background-color:{macro_colors}'></span>{names(macro_colors)}"),
      Grams = glue("{round(c(r$protein_g, r$fat_g, r$carb_g))} g"),
      Calories = format_number(c(r$protein_kcal, r$fat_kcal, r$carb_kcal)),
      Share = glue("{round(c(r$protein_pct, r$fat_pct, r$carb_pct))}%")
    )
  }, sanitize.text.function = identity, align = "lrrr", width = "100%")

  # A single stacked bar: each macronutrient's share of calories.
  output$macro_bar <- renderUI({
    r <- results()
    shares <- c(r$protein_pct, r$fat_pct, r$carb_pct)
    div(
      class = "macro-bar",
      role = "img",
      `aria-label` = glue(
        "Protein {round(shares[1])}%, fat {round(shares[2])}%, ",
        "carbohydrates {round(shares[3])}% of calories"
      ),
      map2(shares, macro_colors, \(share, color) {
        span(style = glue("width: {share}%; background-color: {color};"))
      })
    )
  })

  output$results_table <- renderTable({
    results()$detail_table
  }, colnames = FALSE, align = "lr", width = "100%")
}

shinyApp(ui = ui, server = server)
