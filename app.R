library(shiny)
library(readxl)
library(ggplot2)
library(lmtest)
library(car)
library(DT)
library(dplyr)

ui <- fluidPage(
  titlePanel("Регрессионный анализ (МНК + диагностика)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Загрузка файла",
                accept = c(".csv", ".txt", ".dat", ".xlsx")),
      
      uiOutput("var_select"),
      
      #checkboxInput("multiple", "Множественная регрессия", FALSE),
      
      checkboxGroupInput("diagnostics",
                         "Диагностика",
                         choices = c("Остатки" = "residuals",           # ключ = значение
                                     "Нормальность" = "normality",
                                     "Гомоскедастичность" = "homoscedasticity",
                                     "Автокорреляция" = "autocorrelation")),
      
      sliderInput("alpha",
                  "Уровень значимости (α)",
                  min = 0.001,
                  max = 1,
                  value = 0.05,
                  step = 0.01),
      
      actionButton("run", "Рассчитать")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Данные", 
                 uiOutput("data_info"), # Строка со статистикой
                 DTOutput("data_table")),
        tabPanel("Модель",
                 verbatimTextOutput("model_summary")),
        tabPanel("График",
                 plotOutput("plot")),
        tabPanel("Остатки",
                 plotOutput("residual_plot")),
        tabPanel("Диагностика",
                 verbatimTextOutput("tests")),
        tabPanel("ANOVA", 
                 DTOutput("anova"),
                 br(),
                 verbatimTextOutput("r2_text")),
        tabPanel("Доверительные интервалы",
                 DTOutput("ci"),
                 uiOutput("hypotheses_check")  
        ) 
      )
    )
  )
)

server <- function(input, output, session) {
  
  # --- 1. Загрузка данных ---
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    
    df <- tryCatch({
      switch(ext,
             # fill = TRUE позволяет читать строки разной длины (ошибка "line 9 doesn't contain 2 elements" исчезнет)
             "csv"  = read.csv(input$file$datapath, fill = TRUE, stringsAsFactors = FALSE),
             "txt"  = read.table(input$file$datapath,fill = TRUE, stringsAsFactors = FALSE),
             "dat"  = read.table(input$file$datapath, fill = TRUE, stringsAsFactors = FALSE),
             "xlsx" = readxl::read_excel(input$file$datapath)
      )
    }, error = function(e) {
      # Если файл невозможно прочитать (например, неверный формат внутри)
      return(NULL)
    })
    
    validate(need(df, "Не удалось прочитать файл. Проверьте структуру данных."))
    return(df)
  })
  
  # --- Отображение данных во вкладке ---
  output$data_table <- renderDT({
    # Если переменные X и Y еще не выбраны, показываем сырые данные
    if (is.null(input$x) || is.null(input$y)) {
      return(datatable(data(), options = list(pageLength = 10)))
    }
    
    # Если переменные выбраны, берем очищенные данные из нашего реактивного блока
    df <- cleaned_data()
    
    datatable(df, 
              caption = 'Данные после обработки (удалены пустые и нечисловые строки)',
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle(
        columns = colnames(df),
        backgroundColor = '#f9f9f9' # Легкая подсветка, чтобы отличить "чистые" данные
      )
  })
  
  # --- Информационный счётчик ---
  output$data_info <- renderUI({
    req(data())
    raw_n <- nrow(data())
    
    # Если расчет еще не запущен, просто пишем общее кол-во
    if (is.null(input$x) || is.null(input$y)) {
      return(p(paste("Загружено строк всего:", raw_n)))
    }
    
    clean_n <- nrow(cleaned_data())
    removed_n <- raw_n - clean_n
    
    if (removed_n > 0) {
      HTML(paste0(
        "<div style='color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
        "<b>Внимание:</b> Из анализа исключено <b>", removed_n, "</b> строк с пропусками или некорректными данными. ",
        "Используется для модели: <b>", clean_n, "</b> из ", raw_n, ".",
        "</div>"
      ))
    } else {
      p(paste("Все данные корректны. Использовано строк:", clean_n), style = "color: green;")
    }
  })
  
  # --- 2. Выбор переменных ---
  output$var_select <- renderUI({
    df <- data()
    cols <- names(df)
    
    tagList(
      selectInput("y", "Отклик (Y)", choices = cols),
      selectInput("x", "Фактор (X)",
                  choices = cols, multiple = TRUE)
    )
  })
  
  # --- 2.5. Предобработка и валидация данных ---
  cleaned_data <- reactive({
    req(input$x, input$y)
    
    # Берем сырые данные
    df_raw <- data()
    req(df_raw)
    
    # Оставляем только нужные колонки (Отклик и Факторы)
    cols_to_keep <- c(input$y, input$x)
    
    # Проверка: существуют ли выбранные колонки в данных (на случай смены файла)
    validate(
      need(all(cols_to_keep %in% colnames(df_raw)), "Выбранные переменные отсутствуют в новом файле.")
    )
    
    df_sub <- df_raw[, cols_to_keep, drop = FALSE]
    
    # Принудительно конвертируем всё в числовой формат.
    df_sub[] <- suppressWarnings(lapply(df_sub, function(col) as.numeric(as.character(col))))
    
    # Удаление пропусков (NA)
    df_clean <- na.omit(df_sub)
    
    # Финальная валидация
    # Проверяем, остались ли данные после чистки. 
    min_rows <- length(input$x) + 2
    validate(
      need(nrow(df_clean) >= min_rows, 
           "Ошибка: После удаления пропусков и нечисловых значений осталось слишком мало данных для расчета.")
    )
    
    return(df_clean)
  })
  
  # --- 3. Построение модели ---
  model <- eventReactive(input$run, {
    req(input$x, input$y)
    
    df <- cleaned_data()
    
    formula <- as.formula(
      paste(input$y, "~", paste(input$x, collapse = "+"))
    )
    
    lm(formula, data = df)
  })
  
  # --- 4. Оценки параметров ---
  output$model_summary <- renderPrint({
    summary(model())
  })
  
  # --- 5. Визуализация ---
  output$plot <- renderPlot({
    df <- cleaned_data()
    
    if (length(input$x) == 1) {
      ggplot(df, aes_string(x = input$x, y = input$y)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE, level = 1 - input$alpha) +
        labs(
          x = paste("Фактор:", input$x),
          y = paste("Отклик:", input$y)
        )
    }
  })
  
  # --- 6. Остатки ---
  # График 1: предсказанные значения и остатки
  output$residual_plot <- renderPlot({
    req(model(), input$x)
    m <- model()
    df <- cleaned_data()
    
    first_x <- input$x[1]
    
    par(mfrow = c(1, 2))  # Два графика рядом
    
    # График 1: Фактор X vs Остатки
    plot(df[[first_x]], residuals(m),
         xlab = paste("Фактор: ", first_x),
         ylab = "Остатки",
         main = "Центрированный относительно предсказанного график остатков")
    abline(h = 0, col = "red", lwd = 2)
    
    # График 2: Q-Q plot для проверки нормальности
    qqnorm(residuals(m),
           main = "Q-Q график остатков",
           xlab = "Теоретические квантили",
           ylab = "Выборочные квантили")
    
    qqline(residuals(m), col = "red", lwd = 2)
  })
  
  # --- 7. Диагностика ---
  output$tests <- renderPrint({
    m <- model()
    alpha <- input$alpha
    choices <- input$diagnostics 
    
    cat("Уровень значимости α =", alpha, "\n\n")
    
    # Проверка, выбрал ли пользователь какие-либо тесты
    if (is.null(choices)) {
      cat("Не выбрано ни одного диагностического теста.\n")
      cat("   Отметьте нужные тесты в панели слева.\n")
      return()
    }
    
    # --- 7.1. Остатки (визуальный анализ) ---
    if ("residuals" %in% choices) {
      cat("-", rep("-", 40), "\n", sep = "")
      cat("===АНАЛИЗ ОСТАТКОВ===\n")
      
      # Основные статистики остатков
      resid_values <- residuals(m)
      cat("Среднее остатков:", mean(resid_values), "\n")
      cat("Медиана остатков:", median(resid_values), "\n")
      cat("Дисперсия остатков:", var(resid_values), "\n")
      cat("Сумма квадратов остатков:", sum(resid_values^2), "\n")
      
      # Проверка: близко ли среднее к нулю
      if (abs(mean(resid_values)) < 1e-6) {
        cat("Среднее остатков близко к 0\n")
      } else {
        cat("Среднее остатков не равно 0 (проблема!)\n")
      }
      cat("\n")
    }
    
    # --- 7.2. Нормальность (Shapiro-Wilk) ---
    if ("normality" %in% choices) {
      cat("-", rep("-", 40), "\n", sep = "")
      cat("===ТЕСТ НА НОРМАЛЬНОСТЬ (Shapiro-Wilk)===\n")
      
      sh <- shapiro.test(resid(m))
      cat("H₀: распределение нормальное\n")
      cat("Статистика W =", round(sh$statistic, 4), "\n")
      cat("p-значение =", format(sh$p.value, scientific = FALSE), "\n")
      
      if (sh$p.value < alpha) {
        cat("РЕЗУЛЬТАТ: Остатки НЕ НОРМАЛЬНЫ (p < α)\n")
        #cat("   → Используйте робастные методы или преобразование данных\n")
      } else {
        cat("РЕЗУЛЬТАТ: Остатки НОРМАЛЬНЫ (p ≥ α)\n")
      }
      cat("\n")
    }
    
    # --- 7.3. Гомоскедастичность (Breusch-Pagan) ---
    if ("homoscedasticity" %in% choices) {
      cat("-", rep("-", 40), "\n", sep = "")
      cat("===ТЕСТ НА ГОМОСКЕДАСТИЧНОСТЬ (Breusch-Pagan)===\n")
      
      bp <- bptest(m)
      cat("H₀: дисперсия постоянна (гомоскедастичность)\n")
      cat("Статистика BP =", round(bp$statistic, 4), "\n")
      cat("p-значение =", format(bp$p.value, scientific = FALSE), "\n")
      
      if (bp$p.value < alpha) {
        cat("РЕЗУЛЬТАТ: Есть ГЕТЕРОСКЕДАСТИЧНОСТЬ (p < α)\n")
        #cat("   → Используйте взвешенный МНК или робастные стандартные ошибки\n")
      } else {
        cat("РЕЗУЛЬТАТ: Гомоскедастичность выполняется (p ≥ α)\n")
      }
      cat("\n")
    }
    
    # --- 7.4. Автокорреляция (Durbin-Watson) ---
    if ("autocorrelation" %in% choices) {
      cat("-", rep("-", 40), "\n", sep = "")
      cat("===ТЕСТ НА АВТОКОРРЕЛЯЦИЮ (Durbin-Watson)===\n")
      
      dw <- dwtest(m)
      cat("H₀: нет автокорреляции\n")
      cat("Статистика DW =", round(dw$statistic, 4), "\n")
      cat("p-значение =", format(dw$p.value, scientific = FALSE), "\n")
      
      # Интерпретация DW статистики
      if (dw$statistic < 1.5) {
        cat("Статистика DW < 1.5 → возможна ПОЛОЖИТЕЛЬНАЯ автокорреляция\n")
      } else if (dw$statistic > 2.5) {
        cat("Статистика DW > 2.5 → возможна ОТРИЦАТЕЛЬНАЯ автокорреляция\n")
      } else {
        cat("Статистика DW близка к 2 → автокорреляция маловероятна\n")
      }
      
      if (dw$p.value < alpha) {
        cat("РЕЗУЛЬТАТ: Есть АВТОКОРРЕЛЯЦИЯ (p < α)\n")
        #cat("   → Используйте модели с лагами или ARIMA\n")
      } else {
        cat("РЕЗУЛЬТАТ: Нет оснований отвергнуть нулевую гипотезу на уровне значимости α \n")
      }
      cat("\n")
    }
    })
  
  # --- 8. ANOVA ---
  output$anova <- renderDT({
    req(model())
    
    # Получаем результат и делаем из него датафрейм
    anova_res <- anova(model())
    anova_df <- as.data.frame(anova_res)
    
    # Добавляем названия строк в отдельную колонку
    anova_df <- cbind(Фактор = rownames(anova_df), anova_df)
    rownames(anova_df) <- NULL
    
    # Переводим названия колонок на русский
    colnames(anova_df) <- c("Источник", "Степени свободы (Df)", 
                            "Сумма квадратов (Sum Sq)", "Средний квадрат (Mean Sq)", 
                            "F-статистика", "p-value")
    
    
    # Рендерим таблицу
    datatable(anova_df, 
              options = list(dom = 't', # Убираем поиск и пагинацию (оставляем только таблицу)
                             bSort = FALSE), # Отключаем сортировку, чтобы не ломать логику ANOVA
              rownames = FALSE,
              class = 'cell-border stripe') %>% 
      formatRound(columns = c("Сумма квадратов (Sum Sq)", "Средний квадрат (Mean Sq)", "F-статистика"), digits = 3) %>%
      formatSignif(columns = "p-value", digits = 3) %>%
      formatStyle(
        'p-value',
        target = 'row'
      )
  })
  
  output$r2_text <- renderPrint({
    req(model())
    
    s <- summary(model())
    
    cat("Коэффициент детерминации (R²):",
        round(s$r.squared, 4), "\n")
    
    cat("Скорректированный R²:",
        round(s$adj.r.squared, 4), "\n")
  })
  
  # --- 9. Доверительные интервалы ---
  output$ci <- renderDT({

    alpha <- input$alpha
    req(model(), input$alpha)
    # Получаем интервалы
    ci <- confint(model(), level = 1 - alpha)
    ci_df <- as.data.frame(ci)
    
    # Добавляем названия коэффициентов и текущие значения (estimates)
    estimates <- coef(model())
    res_df <- data.frame(
      Параметр = rownames(ci_df),
      Оценка = round(estimates, 3),
      `Нижняя граница` = round(ci_df[,1], 3),
      `Верхняя граница` = round(ci_df[,2], 3),
      check.names = FALSE
    )
    
    # Логика проверки на 0
    res_df$Статус = ifelse(
      res_df$`Нижняя граница` > 0 | res_df$`Верхняя граница` < 0,
      "Значим (0 вне интервала)",
      "Не значим (0 внутри)"
    )
    
    datatable(res_df, options = list(dom = 't'), rownames = FALSE) %>%
      formatStyle(
        'Статус',
        color = styleEqual(
          c("Значим (0 вне интервала)", "Не значим (0 внутри)"),
          c('green', 'red')
        ),
        fontWeight = 'bold'
      )
  })
  
  output$hypotheses_check <- renderUI({

    alpha <- input$alpha
    req(model(), input$alpha)
    ci <- confint(model(), level = 1 - alpha)
    
    # Проверяем основной коэффициент (обычно это вторая строка после Intercept)
    beta1_low <- ci[2, 1]
    beta1_high <- ci[2, 2]
    
    is_significant <- beta1_low > 0 | beta1_high < 0
    
    if(is_significant) {
      HTML(paste0(
        "<div style='color: green; font-weight: bold;'>",
        "Результат: Гипотеза H0: β1 = 0 ОТВЕРГАЕТСЯ на уровне значимости ", alpha, ".<br>",
        "Вывод: Фактор оказывает статистически значимое влияние на результат.",
        "</div>"
      ))
    } else {
      HTML(paste0(
        "<div style='color: red; font-weight: bold;'>",
        "Результат: Гипотеза H0: β1 = 0 НЕ отвергается на уровне значимости ", alpha, ".<br>",
        "Вывод: Влияние фактора не доказано (интервал содержит 0).",
        "</div>"
      ))
    }
  })
}

shinyApp(ui, server) 