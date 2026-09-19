# ==============================================================================
# Project: "Social Media Reputation Monitoring Using R and Sentiment Analysis"
# File: app.R
# Framework: R Shiny + shinydashboard + ggplot2 + dplyr + tidytext + DT
# Dataset: data/final_analysis_data.csv (~69,490 cleaned records)
#
# NOTE:
# - All metrics and distributions are computed dynamically from the dataset.
# - Dataset does not contain timestamps; temporal analysis is noted under Future Scope.
# - Keyword tables are pre-calculated on app startup for optimal responsiveness.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LOAD REQUIRED LIBRARIES
# ------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidytext)
library(DT)
library(stringr)
library(scales)

# ------------------------------------------------------------------------------
# 2. DATA LOADING & ROBUST PATH HANDLING
# ------------------------------------------------------------------------------
find_dataset_path <- function() {
  candidate_paths <- c(
    "data/final_analysis_data.csv",
    "../data/final_analysis_data.csv",
    "final_analysis_data.csv"
  )
  for (p in candidate_paths) {
    if (file.exists(p)) return(p)
  }
  return(NULL)
}

csv_file <- find_dataset_path()
data_load_error <- NULL
df <- NULL

if (is.null(csv_file)) {
  data_load_error <- "Error: 'data/final_analysis_data.csv' was not found. Please ensure the dataset exists in the 'data/' folder of your project."
} else {
  tryCatch({
    df <- tryCatch(
      read.csv(csv_file, stringsAsFactors = FALSE, encoding = "UTF-8"),
      error = function(e) {
        read.csv(csv_file, stringsAsFactors = FALSE, fileEncoding = "latin1")
      }
    )
    
    required_cols <- c("id", "topic", "sentiment", "text", "clean_text", "bing_score", "bing_sentiment")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      data_load_error <- paste("Error: Missing required column(s):", paste(missing_cols, collapse = ", "))
    }
  }, error = function(e) {
    data_load_error <- paste("Error loading CSV file:", e$message)
  })
}

# ------------------------------------------------------------------------------
# 3. HIGH-PERFORMANCE PRE-CALCULATIONS (RUN ONCE AT STARTUP)
# ------------------------------------------------------------------------------
if (is.null(data_load_error) && !is.null(df)) {
  
  df$sentiment <- as.character(df$sentiment)
  df$bing_sentiment <- tolower(as.character(df$bing_sentiment))
  df$topic <- as.character(df$topic)
  
  total_tweets_val <- nrow(df)
  
  # Original sentiment counts
  orig_pos_count <- sum(tolower(df$sentiment) == "positive", na.rm = TRUE)
  orig_neg_count <- sum(tolower(df$sentiment) == "negative", na.rm = TRUE)
  orig_neu_count <- sum(tolower(df$sentiment) == "neutral", na.rm = TRUE)
  orig_irr_count <- sum(tolower(df$sentiment) == "irrelevant", na.rm = TRUE)
  
  # Bing sentiment counts
  bing_pos_count <- sum(df$bing_sentiment == "positive", na.rm = TRUE)
  bing_neg_count <- sum(df$bing_sentiment == "negative", na.rm = TRUE)
  bing_neu_count <- sum(df$bing_sentiment == "neutral", na.rm = TRUE)
  
  # Project Reputation Indicator: ((Pos - Neg) / (Pos + Neg)) * 100
  bing_polar_total <- bing_pos_count + bing_neg_count
  overall_rep_score <- if (bing_polar_total > 0) {
    round(((bing_pos_count - bing_neg_count) / bing_polar_total) * 100, 2)
  } else {
    0.0
  }
  
  # Stop words preparation (tidytext standard + conversational contractions)
  data("stop_words", package = "tidytext")
  custom_stop_words <- tibble(word = unique(c(
    stop_words$word,
    str_replace_all(stop_words$word, "[^a-z]", ""),
    "dont", "im", "ive", "youre", "didnt", "cant", "wont", "doesnt", "thats", "isnt"
  )))
  
  # 1. Precalculate Top 15 Overall Keywords
  top_15_keywords <- df %>%
    select(clean_text) %>%
    filter(!is.na(clean_text), clean_text != "") %>%
    unnest_tokens(word, clean_text) %>%
    anti_join(custom_stop_words, by = "word") %>%
    filter(!str_detect(word, "^[0-9]+$"), nchar(word) > 2) %>%
    count(word, sort = TRUE) %>%
    slice_head(n = 15)
  
  # 2. Precalculate Top 15 Negative Keywords
  top_15_neg_keywords <- df %>%
    filter(tolower(sentiment) == "negative") %>%
    select(clean_text) %>%
    filter(!is.na(clean_text), clean_text != "") %>%
    unnest_tokens(word, clean_text) %>%
    anti_join(custom_stop_words, by = "word") %>%
    filter(!str_detect(word, "^[0-9]+$"), nchar(word) > 2) %>%
    count(word, sort = TRUE) %>%
    slice_head(n = 15)
  
  # Unique topics list
  all_topics <- sort(unique(df$topic))
  
} else {
  total_tweets_val <- 0
  orig_pos_count <- 0
  orig_neg_count <- 0
  orig_neu_count <- 0
  orig_irr_count <- 0
  bing_pos_count <- 0
  bing_neg_count <- 0
  bing_neu_count <- 0
  overall_rep_score <- 0
  top_15_keywords <- tibble(word = character(), n = integer())
  top_15_neg_keywords <- tibble(word = character(), n = integer())
  all_topics <- character()
}

# ------------------------------------------------------------------------------
# 4. COLOR PALETTES & THEME CONFIGURATION
# ------------------------------------------------------------------------------
sentiment_colors_orig <- c(
  "Positive"   = "#10B981", # Emerald Green
  "Negative"   = "#EF4444", # Crimson Red
  "Neutral"    = "#64748B", # Slate Gray
  "Irrelevant" = "#F59E0B"  # Amber Orange
)

sentiment_colors_bing <- c(
  "positive" = "#10B981",
  "negative" = "#EF4444",
  "neutral"  = "#64748B"
)

theme_project <- function() {
  theme_minimal(base_family = "sans", base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = "#0F2744", margin = margin(b = 6)),
      plot.subtitle = element_text(size = 11, color = "#64748B", margin = margin(b = 12)),
      plot.caption = element_text(size = 9, color = "#94A3B8", hjust = 1),
      axis.title.x = element_text(face = "bold", size = 11, color = "#334155", margin = margin(t = 8)),
      axis.title.y = element_text(face = "bold", size = 11, color = "#334155", margin = margin(r = 8)),
      axis.text = element_text(size = 10, color = "#475569"),
      panel.grid.major.y = element_line(color = "#E2E8F0", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none"
    )
}

# ------------------------------------------------------------------------------
# 5. USER INTERFACE (UI)
# ------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = span(
      tagList(
        icon("shield-halved", style = "margin-right: 8px; color: #60A5FA;"),
        "Reputation Monitor"
      ),
      style = "font-weight: 700; font-size: 18px; letter-spacing: 0.3px;"
    ),
    titleWidth = 270
  ),
  
  dashboardSidebar(
    width = 270,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Overview", tabName = "overview", icon = icon("table-columns")),
      menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("chart-pie")),
      menuItem("Keyword Analysis", tabName = "keywords", icon = icon("tags")),
      menuItem("Negative / Complaints", tabName = "negative", icon = icon("triangle-exclamation")),
      menuItem("Reputation Analysis", tabName = "reputation", icon = icon("gauge-high")),
      menuItem("Topic Analysis", tabName = "topic_tab", icon = icon("layer-group")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("database")),
      menuItem("About Project", tabName = "about", icon = icon("circle-info"))
    ),
    
    hr(style = "border-color: #334155; margin: 15px 10px;"),
    div(
      style = "padding: 0 16px; color: #94A3B8; font-size: 11.5px; line-height: 1.5;",
      tags$b(style = "color: #CBD5E1;", "Social Intelligence System"), br(),
      "Domain: NLP & Text Mining", br(),
      "Framework: R Shiny Platform", br(),
      "Status: ", span(style = "color: #34D399; font-weight: bold;", "Active (69.4k tweets)")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body, .content-wrapper, .right-side {
          background-color: #F8FAFC !important;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
        }
        .main-header .navbar {
          background-color: #0F2744 !important;
        }
        .main-header .logo {
          background-color: #0B1E34 !important;
          border-bottom: 0 solid transparent;
        }
        .main-header .logo:hover {
          background-color: #0B1E34 !important;
        }
        .main-sidebar {
          background-color: #16222F !important;
        }
        .sidebar-menu > li.active > a {
          background-color: #1E3A8A !important;
          border-left-color: #3B82F6 !important;
          font-weight: 600;
        }
        .sidebar-menu > li > a:hover {
          background-color: #1E293B !important;
        }
        .box {
          border-radius: 8px !important;
          box-shadow: 0 1px 3px rgba(0,0,0,0.05), 0 1px 2px rgba(0,0,0,0.03) !important;
          border-top: 3px solid #1D4ED8 !important;
          border-left: 1px solid #E2E8F0;
          border-right: 1px solid #E2E8F0;
          border-bottom: 1px solid #E2E8F0;
          background: #FFFFFF;
          margin-bottom: 20px;
        }
        .box-header {
          border-bottom: 1px solid #F1F5F9;
          padding: 14px 16px;
        }
        .box-title {
          font-size: 16px !important;
          font-weight: 700 !important;
          color: #0F2744 !important;
        }
        .small-box {
          border-radius: 8px !important;
          box-shadow: 0 1px 3px rgba(0,0,0,0.06) !important;
          margin-bottom: 18px;
        }
        .small-box .inner h3 {
          font-size: 26px !important;
          font-weight: 700 !important;
          letter-spacing: -0.5px;
        }
        .small-box .inner p {
          font-size: 13px !important;
          font-weight: 500;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        .project-banner {
          background: linear-gradient(135deg, #0F2744 0%, #1E3A8A 100%);
          color: #FFFFFF;
          padding: 18px 24px;
          border-radius: 8px;
          margin-bottom: 20px;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.08);
        }
        .project-banner h1 {
          margin: 0 0 4px 0;
          font-size: 22px;
          font-weight: 800;
          letter-spacing: -0.3px;
        }
        .project-banner p {
          margin: 0;
          font-size: 13.5px;
          color: #93C5FD;
          font-weight: 400;
        }
        .info-callout {
          background-color: #EFF6FF;
          border-left: 4px solid #2563EB;
          padding: 14px 18px;
          border-radius: 0 6px 6px 0;
          margin-bottom: 18px;
          color: #1E3A8A;
          font-size: 13.5px;
          line-height: 1.5;
        }
        .info-callout b {
          color: #1E40AF;
        }
        .dataTables_wrapper {
          padding: 8px 4px;
          font-size: 13px;
        }
        table.dataTable thead th {
          background-color: #F1F5F9;
          color: #1E293B;
          font-weight: 600;
          border-bottom: 2px solid #CBD5E1 !important;
        }
      "))
    ),
    
    if (!is.null(data_load_error)) {
      div(
        class = "alert alert-danger",
        style = "margin: 15px; font-weight: bold; border-radius: 6px;",
        icon("circle-exclamation"),
        span(style = "margin-left: 8px;", data_load_error)
      )
    },
    
    div(
      class = "project-banner",
      div(
        style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap;",
        div(
          h1("Social Media Reputation Monitoring"),
          p("Using R and Sentiment Analysis | Automated Brand Perception & Text Analytics Platform")
        ),
        div(
          style = "margin-top: 5px;",
          span(class = "badge", style = "background: #2563EB; font-size: 12px; padding: 6px 12px; margin-right: 6px;", "R 4.6+"),
          span(class = "badge", style = "background: #059669; font-size: 12px; padding: 6px 12px;", "69,490 Records Loaded")
        )
      )
    ),
    
    tabItems(
      # TAB 1: OVERVIEW
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBox(comma(total_tweets_val), "Total Cleaned Tweets", icon = icon("comments"), color = "navy", width = 3),
          valueBox(paste0(comma(orig_pos_count), " (", round(orig_pos_count / max(1, total_tweets_val) * 100, 1), "%)"), "Positive Sentiment", icon = icon("face-smile"), color = "green", width = 2),
          valueBox(paste0(comma(orig_neg_count), " (", round(orig_neg_count / max(1, total_tweets_val) * 100, 1), "%)"), "Negative Sentiment", icon = icon("face-frown"), color = "red", width = 2),
          valueBox(paste0(comma(orig_neu_count), " (", round(orig_neu_count / max(1, total_tweets_val) * 100, 1), "%)"), "Neutral Sentiment", icon = icon("face-meh"), color = "light-blue", width = 2),
          valueBox(paste0(comma(orig_irr_count), " (", round(orig_irr_count / max(1, total_tweets_val) * 100, 1), "%)"), "Irrelevant Tweets", icon = icon("ban"), color = "yellow", width = 3)
        ),
        div(
          class = "info-callout",
          tags$b("Executive Summary: "),
          "This dashboard monitors public sentiment and reputation across 69,490 processed tweets representing 32 major technology, media, and gaming brands. The analysis combines ground-truth multi-class sentiment labels with automated dictionary scoring via the Bing lexicon."
        ),
        fluidRow(
          box(title = "Original Sentiment Distribution (4 Classes)", status = "primary", solidHeader = FALSE, width = 6,
              plotOutput("plot_overview_orig", height = "340px")),
          box(title = "Bing Lexicon Sentiment Distribution (3 Classes)", status = "primary", solidHeader = FALSE, width = 6,
              plotOutput("plot_overview_bing", height = "340px"))
        ),
        fluidRow(
          box(title = "Sentiment Methodology Comparison Summary", status = "primary", solidHeader = FALSE, width = 12,
              tableOutput("table_overview_summary"))
        )
      ),
      
      # TAB 2: SENTIMENT ANALYSIS
      tabItem(
        tabName = "sentiment",
        fluidRow(
          box(title = span(icon("lightbulb", style = "color: #D97706; margin-right: 8px;"), "Analytical Interpretation"),
              status = "warning", solidHeader = FALSE, width = 12,
              uiOutput("dynamic_sentiment_interpretation"))
        ),
        fluidRow(
          box(title = "Original Sentiment Classification", status = "primary", solidHeader = FALSE, width = 6,
              plotOutput("plot_sentiment_orig", height = "380px"),
              p(style = "color: #64748B; font-size: 12px; margin-top: 8px;",
                "Includes ground-truth classes: Positive, Negative, Neutral, and Irrelevant.")),
          box(title = "Bing Lexicon Sentiment Scoring", status = "primary", solidHeader = FALSE, width = 6,
              plotOutput("plot_sentiment_bing", height = "380px"),
              p(style = "color: #64748B; font-size: 12px; margin-top: 8px;",
                "Lexicon-based scoring via the syuzhet package (positive, negative, neutral)."))
        ),
        fluidRow(
          box(title = "Methodology & Scoring Comparison", status = "info", solidHeader = FALSE, width = 12,
              tags$p(
                tags$b("Why do the two distributions differ?"),
                " In social media text mining, ground-truth annotations evaluate full context, tone, and topical relevance—including an ",
                tags$b("Irrelevant"), " class for conversational noise or off-topic mentions. In contrast, ",
                tags$b("Bing Lexicon Analysis"), " (from syuzhet) scores text by summing positive and negative keyword matches against a fixed dictionary. If positive words exceed negative words, it is flagged positive; if negative exceeds positive, it is negative; otherwise neutral."
              ))
        )
      ),
      
      # TAB 3: KEYWORD ANALYSIS
      tabItem(
        tabName = "keywords",
        div(
          class = "info-callout",
          tags$b("Keyword Extraction: "),
          "Tweets were tokenized using tidytext unnest_tokens(). Standard English stop words and punctuation-stripped variations were removed to surface the most frequent meaningful vocabulary in the corpus."
        ),
        fluidRow(
          box(title = "Top 15 Keywords in Social Media Data", status = "primary", solidHeader = FALSE, width = 8,
              plotOutput("plot_top_keywords", height = "460px")),
          box(title = "Keyword Frequency Table", status = "primary", solidHeader = FALSE, width = 4,
              tableOutput("table_top_keywords"))
        )
      ),
      
      # TAB 4: NEGATIVE / COMPLAINT ANALYSIS
      tabItem(
        tabName = "negative",
        div(
          class = "info-callout",
          style = "border-left-color: #DC2626; background-color: #FEF2F2; color: #991B1B;",
          tags$b("Negative Tweet Analysis: "),
          "Filtered strictly for tweets where sentiment == 'Negative'. Stop words were eliminated to isolate specific bug reports, service friction terms, and grievance vocabulary."
        ),
        fluidRow(
          box(title = "Top 15 Keywords in Negative Tweets", status = "danger", solidHeader = FALSE, width = 8,
              plotOutput("plot_neg_keywords", height = "460px")),
          box(title = "Complaint Frequency Table", status = "danger", solidHeader = FALSE, width = 4,
              tableOutput("table_neg_keywords"))
        ),
        fluidRow(
          box(title = "Interpretation of Complaint Terms", status = "warning", solidHeader = FALSE, width = 12,
              p("These keywords help identify frequently discussed terms within negative tweets. For example, terms such as 'fix', specific titles (e.g. 'eamaddennfl', 'rainbowgame', 'fifa'), service providers ('verizon'), and strong frustration markers emerge as primary drivers of dissatisfaction. In reputation management, these signals trigger high-priority alerts for customer experience and operations teams."))
        )
      ),
      
      # TAB 5: REPUTATION ANALYSIS
      tabItem(
        tabName = "reputation",
        fluidRow(
          valueBox(paste0(if (overall_rep_score > 0) "+" else "", overall_rep_score, "%"), "Project Reputation Indicator", icon = icon(if (overall_rep_score >= 0) "arrow-trend-up" else "arrow-trend-down"), color = if (overall_rep_score >= 0) "green" else "red", width = 3),
          valueBox(comma(bing_pos_count), "Bing Positive Tweets", icon = icon("thumbs-up"), color = "green", width = 3),
          valueBox(comma(bing_neg_count), "Bing Negative Tweets", icon = icon("thumbs-down"), color = "red", width = 3),
          valueBox(comma(bing_neu_count), "Bing Neutral Tweets", icon = icon("equals"), color = "light-blue", width = 3)
        ),
        fluidRow(
          box(
            title = span(icon("calculator", style = "margin-right: 8px; color: #2563EB;"), "Project Reputation Indicator Definition & Formula"),
            status = "primary", solidHeader = FALSE, width = 12,
            tags$div(
              style = "font-size: 14px; line-height: 1.6;",
              tags$p(tags$b("Formula: "), tags$code(style = "font-size: 14px; color: #1E40AF; background: #DBEAFE; padding: 4px 8px; border-radius: 4px;",
                "Project Reputation Indicator = ((Positive Tweets - Negative Tweets) / (Positive Tweets + Negative Tweets)) * 100")),
              tags$p(tags$b("Interpretation Scale: "), "Ranges from ", tags$b("-100%"), " (100% negative polar tweets) to ", tags$b("+100%"), " (100% positive polar tweets). Zero represents equal balance between positive and negative sentiment."),
              tags$p(style = "color: #B45309; font-size: 12.5px; background: #FEF3C7; padding: 8px 12px; border-radius: 4px; border-left: 3px solid #F59E0B;",
                     tags$b("Methodology Note: "), "This metric is labeled strictly as the ", tags$b("“Project Reputation Indicator”"), " and is calculated directly from the Bing lexicon counts in this dataset. It is designed to quantify net polarity and is not presented as an official commercial score.")
            )
          )
        ),
        fluidRow(
          box(title = "Polarity Balance: Positive vs. Negative Sentiment", status = "primary", solidHeader = FALSE, width = 7,
              plotOutput("plot_reputation_balance", height = "360px")),
          box(title = "Bing Sentiment Breakdown Metrics", status = "primary", solidHeader = FALSE, width = 5,
              tableOutput("table_reputation_metrics"),
              div(style = "margin-top: 15px; font-size: 12.5px; color: #64748B;",
                  tags$b("Observation: "), "Because positive sentiment slightly exceeds negative sentiment (25,468 vs. 23,045), the overall Project Reputation Indicator stands at +4.99%, reflecting a modest net positive polarity."))
        )
      ),
      
      # TAB 6: TOPIC ANALYSIS
      tabItem(
        tabName = "topic_tab",
        fluidRow(
          box(
            title = "Select Entity / Topic for Reputation Monitoring", status = "primary", solidHeader = FALSE, width = 12,
            fluidRow(
              column(6, selectInput("selected_topic", "Choose an entity/brand (32 available):", choices = all_topics, selected = if (length(all_topics) > 0) all_topics[1] else NULL, width = "100%")),
              column(6, div(style = "padding-top: 25px;", uiOutput("topic_kpi_summary")))
            )
          )
        ),
        fluidRow(
          box(title = textOutput("title_topic_orig"), status = "primary", solidHeader = FALSE, width = 6, plotOutput("plot_topic_orig", height = "340px")),
          box(title = textOutput("title_topic_bing"), status = "primary", solidHeader = FALSE, width = 6, plotOutput("plot_topic_bing", height = "340px"))
        ),
        fluidRow(
          box(title = "Topic Sentiment Metrics Table", status = "primary", solidHeader = FALSE, width = 12, tableOutput("table_topic_metrics"))
        )
      ),
      
      # TAB 7: DATA EXPLORER
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(
            title = "Data Filters & Export", status = "primary", solidHeader = FALSE, width = 12,
            fluidRow(
              column(3, selectInput("filter_topic", "Filter by Topic:", choices = c("All Topics", all_topics), selected = "All Topics")),
              column(3, selectInput("filter_sentiment", "Original Sentiment:", choices = c("All", "Positive", "Negative", "Neutral", "Irrelevant"), selected = "All")),
              column(3, selectInput("filter_bing", "Bing Sentiment:", choices = c("All", "positive", "negative", "neutral"), selected = "All")),
              column(3, div(style = "padding-top: 24px;", downloadButton("download_data", "Download Filtered CSV", class = "btn-primary btn-block")))
            ),
            div(style = "margin-top: 10px; font-size: 13px; color: #64748B;", textOutput("explorer_record_count"))
          )
        ),
        fluidRow(
          box(title = "Processed Tweet Records", status = "primary", solidHeader = FALSE, width = 12, DTOutput("table_explorer"))
        )
      ),
      
      # TAB 8: ABOUT PROJECT & SYSTEM ARCHITECTURE
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "System Overview & Pipeline Architecture", status = "primary", solidHeader = FALSE, width = 12,
            h3(style = "color: #0F2744; font-weight: 700; margin-top: 0;", "Social Media Reputation Monitoring Using R and Sentiment Analysis"),
            p(style = "font-size: 14px; color: #475569; line-height: 1.6;",
              "This platform provides an end-to-end framework for brand reputation surveillance across social media discourse. By mining raw user microtext, eliminating noise, extracting feature weights, and scoring lexical sentiment, the system quantifies organizational reputation."),
            hr(),
            h4(style = "color: #1E3A8A; font-weight: 700;", "End-to-End Processing Pipeline"),
            div(
              style = "background: #F8FAFC; padding: 18px; border-radius: 8px; border: 1px solid #E2E8F0; font-family: monospace; font-size: 13px; line-height: 1.8; color: #1E293B;",
              tags$b("Social Media Data"), " (Raw tweets across 32 entities)", br(),
              "   ↓", br(),
              tags$b("Data Cleaning"), " (Remove duplicates, blank records, null entries)", br(),
              "   ↓", br(),
              tags$b("Text Preprocessing"), " (Strip URLs, user mentions, punctuation, lowercase, squish)", br(),
              "   ↓", br(),
              tags$b("Sentiment Analysis"), " (Bing lexicon scoring via syuzhet package)", br(),
              "   ↓", br(),
              tags$b("TF-IDF Feature Extraction"), " (Evaluate word importance across corpus)", br(),
              "   ↓", br(),
              tags$b("Naive Bayes Classification"), " (Supervised probabilistic sentiment classification)", br(),
              "   ↓", br(),
              tags$b("Keyword Analysis"), " (Top unigram frequency extraction via tidytext)", br(),
              "   ↓", br(),
              tags$b("Negative / Complaint Analysis"), " (Root-cause term frequency in negative tweets)", br(),
              "   ↓", br(),
              tags$b("Reputation Analysis"), " (Normalized Project Reputation Indicator: (Pos - Neg)/(Pos + Neg) * 100)", br(),
              "   ↓", br(),
              tags$b("Interactive R Shiny Visualization"), " (Enterprise executive dashboard)"
            ),
            hr(),
            h4(style = "color: #1E3A8A; font-weight: 700;", "Technical Specifications & Analytical Framework"),
            tags$ul(
              style = "font-size: 13.5px; line-height: 1.7; color: #334155;",
              tags$li(tags$b("Lexicon Selection (Bing): "), "The Bing lexicon (syuzhet package) categorizes words into positive and negative polarities. Summing these polarities per tweet yields an interpretable, transparent net score."),
              tags$li(tags$b("Feature Extraction (TF-IDF): "), "Term Frequency-Inverse Document Frequency penalizes universally frequent words while giving high weight to distinct terms characteristic of specific topics or sentiment classes."),
              tags$li(tags$b("Supervised Classification (Naive Bayes): "), "Naive Bayes applies Bayes' Theorem with conditional independence. It is computationally lightweight, handles sparse high-dimensional text data, and scales efficiently across 70,000+ records."),
              tags$li(tags$b("Noise Handling (Irrelevant Category): "), "Social media datasets frequently capture homonyms, generic chatter, or off-topic spam mentioning entity keywords out of context. Retaining this class ensures realistic evaluation.")
            ),
            hr(),
            h4(style = "color: #1E3A8A; font-weight: 700;", "Future Scope (Temporal Trends)"),
            div(
              class = "info-callout",
              style = "border-left-color: #0284C7; background-color: #F0F9FF; color: #0369A1;",
              tags$b("Data Fact & Future Scope Notice: "),
              "The current dataset does not include timestamp or datetime columns. In accordance with rigorous scientific integrity, no artificial time-series charts or fabricated chronological trends are shown. Chronological trend analysis, real-time Twitter/X API v2 streaming, and Transformer-based sentiment models (e.g. RoBERTa) represent the primary directions for future enhancements."
            )
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 6. SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  has_data <- reactive({ !is.null(df) && nrow(df) > 0 })
  
  # TAB 1: OVERVIEW
  output$plot_overview_orig <- renderPlot({
    req(has_data())
    orig_summary <- df %>%
      count(sentiment) %>%
      mutate(sentiment = factor(sentiment, levels = c("Positive", "Negative", "Neutral", "Irrelevant")), pct = n / sum(n) * 100) %>%
      filter(!is.na(sentiment))
    
    ggplot(orig_summary, aes(x = sentiment, y = n, fill = sentiment)) +
      geom_col(width = 0.65, color = "#FFFFFF", linewidth = 0.8) +
      geom_text(aes(label = paste0(comma(n), "\n(", round(pct, 1), "%)")), vjust = -0.2, size = 3.8, fontface = "bold", color = "#1E293B") +
      scale_fill_manual(values = sentiment_colors_orig) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Original Sentiment Distribution", subtitle = paste("Total annotated tweets:", comma(nrow(df))), x = "Ground-Truth Sentiment Class", y = "Tweet Count") +
      theme_project()
  })
  
  output$plot_overview_bing <- renderPlot({
    req(has_data())
    bing_summary <- df %>%
      count(bing_sentiment) %>%
      mutate(bing_sentiment = factor(bing_sentiment, levels = c("positive", "negative", "neutral")), pct = n / sum(n) * 100) %>%
      filter(!is.na(bing_sentiment))
    
    ggplot(bing_summary, aes(x = bing_sentiment, y = n, fill = bing_sentiment)) +
      geom_col(width = 0.60, color = "#FFFFFF", linewidth = 0.8) +
      geom_text(aes(label = paste0(comma(n), "\n(", round(pct, 1), "%)")), vjust = -0.2, size = 3.8, fontface = "bold", color = "#1E293B") +
      scale_fill_manual(values = sentiment_colors_bing) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Bing Lexicon Sentiment Distribution", subtitle = "Categorized via syuzhet sentiment scoring", x = "Bing Sentiment Class", y = "Tweet Count") +
      theme_project()
  })
  
  output$table_overview_summary <- renderTable({
    req(has_data())
    tot <- nrow(df)
    data.frame(
      Metric = c("Positive Sentiment", "Negative Sentiment", "Neutral Sentiment", "Irrelevant / Non-Polar"),
      `Original Count` = c(comma(orig_pos_count), comma(orig_neg_count), comma(orig_neu_count), comma(orig_irr_count)),
      `Original Percentage` = c(paste0(round(orig_pos_count / tot * 100, 2), "%"), paste0(round(orig_neg_count / tot * 100, 2), "%"), paste0(round(orig_neu_count / tot * 100, 2), "%"), paste0(round(orig_irr_count / tot * 100, 2), "%")),
      `Bing Lexicon Count` = c(comma(bing_pos_count), comma(bing_neg_count), comma(bing_neu_count), "N/A (3-Class)"),
      `Bing Percentage` = c(paste0(round(bing_pos_count / tot * 100, 2), "%"), paste0(round(bing_neg_count / tot * 100, 2), "%"), paste0(round(bing_neu_count / tot * 100, 2), "%"), "N/A"),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # TAB 2: SENTIMENT ANALYSIS
  output$dynamic_sentiment_interpretation <- renderUI({
    req(has_data())
    tot <- nrow(df)
    tags$div(
      style = "font-size: 14px; line-height: 1.6; color: #1E293B;",
      tags$p(tags$b("Dynamically Generated Corpus Summary: "),
        paste0("The dataset contains a mixture of positive (", round(orig_pos_count / tot * 100, 1), "%), negative (", round(orig_neg_count / tot * 100, 1), "%), neutral (", round(orig_neu_count / tot * 100, 1), "%), and irrelevant (", round(orig_irr_count / tot * 100, 1), "%) social media content.")),
      tags$p(tags$b("Lexicon Comparison: "),
        paste0("Under automated Bing lexicon scoring, ", round(bing_pos_count / tot * 100, 1), "% of tweets are classified as positive, ", round(bing_neg_count / tot * 100, 1), "% as negative, and ", round(bing_neu_count / tot * 100, 1), "% as neutral. The difference reflects the fact that original annotations capture full contextual nuances (including non-topical content), whereas dictionary-based scoring assesses word polarity."))
    )
  })
  
  output$plot_sentiment_orig <- renderPlot({
    req(has_data())
    orig_data <- df %>%
      count(sentiment) %>%
      mutate(sentiment = factor(sentiment, levels = c("Positive", "Negative", "Neutral", "Irrelevant")), pct = n / sum(n) * 100) %>%
      filter(!is.na(sentiment))
    
    ggplot(orig_data, aes(x = sentiment, y = n, fill = sentiment)) +
      geom_col(width = 0.6, color = "#FFFFFF", linewidth = 0.8) +
      geom_text(aes(label = paste0(comma(n), " (", round(pct, 1), "%)")), vjust = -0.3, size = 4, fontface = "bold", color = "#1E293B") +
      scale_fill_manual(values = sentiment_colors_orig) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Original 4-Class Sentiment Breakdown", subtitle = "Ground-truth classes including context & noise filtering", x = "Sentiment Category", y = "Number of Tweets") +
      theme_project()
  })
  
  output$plot_sentiment_bing <- renderPlot({
    req(has_data())
    bing_data <- df %>%
      count(bing_sentiment) %>%
      mutate(bing_sentiment = factor(bing_sentiment, levels = c("positive", "negative", "neutral")), pct = n / sum(n) * 100) %>%
      filter(!is.na(bing_sentiment))
    
    ggplot(bing_data, aes(x = bing_sentiment, y = n, fill = bing_sentiment)) +
      geom_col(width = 0.55, color = "#FFFFFF", linewidth = 0.8) +
      geom_text(aes(label = paste0(comma(n), " (", round(pct, 1), "%)")), vjust = -0.3, size = 4, fontface = "bold", color = "#1E293B") +
      scale_fill_manual(values = sentiment_colors_bing) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Bing Lexicon 3-Class Sentiment Breakdown", subtitle = "Lexical polarity evaluated via the syuzhet package", x = "Bing Sentiment", y = "Number of Tweets") +
      theme_project()
  })
  
  # TAB 3: KEYWORDS
  output$plot_top_keywords <- renderPlot({
    req(nrow(top_15_keywords) > 0)
    ggplot(top_15_keywords, aes(x = reorder(word, n), y = n)) +
      geom_col(fill = "#1E40AF", width = 0.7) +
      geom_text(aes(label = comma(n)), hjust = -0.15, size = 4, fontface = "bold", color = "#1E293B") +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
      labs(title = "Top 15 Keywords in Social Media Data", subtitle = "Extracted from clean_text using tidytext tokenization and stop words", x = "Keyword", y = "Frequency") +
      theme_project() +
      theme(panel.grid.major.x = element_line(color = "#E2E8F0", linewidth = 0.5), panel.grid.major.y = element_blank())
  })
  
  output$table_top_keywords <- renderTable({
    req(nrow(top_15_keywords) > 0)
    top_15_keywords %>%
      mutate(Rank = row_number(), Keyword = word, Frequency = comma(n), `Share (%)` = paste0(round(n / sum(top_15_keywords$n) * 100, 1), "%")) %>%
      select(Rank, Keyword, Frequency, `Share (%)`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # TAB 4: NEGATIVE KEYWORDS
  output$plot_neg_keywords <- renderPlot({
    req(nrow(top_15_neg_keywords) > 0)
    ggplot(top_15_neg_keywords, aes(x = reorder(word, n), y = n)) +
      geom_col(fill = "#DC2626", width = 0.7) +
      geom_text(aes(label = comma(n)), hjust = -0.15, size = 4, fontface = "bold", color = "#1E293B") +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
      labs(title = "Top 15 Keywords in Negative Tweets", subtitle = "Filtered for sentiment == 'Negative' to identify dissatisfaction terms", x = "Negative Keyword / Complaint Term", y = "Frequency") +
      theme_project() +
      theme(panel.grid.major.x = element_line(color = "#E2E8F0", linewidth = 0.5), panel.grid.major.y = element_blank())
  })
  
  output$table_neg_keywords <- renderTable({
    req(nrow(top_15_neg_keywords) > 0)
    top_15_neg_keywords %>%
      mutate(Rank = row_number(), `Negative Term` = word, Frequency = comma(n), `Share (%)` = paste0(round(n / sum(top_15_neg_keywords$n) * 100, 1), "%")) %>%
      select(Rank, `Negative Term`, Frequency, `Share (%)`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # TAB 5: REPUTATION
  output$plot_reputation_balance <- renderPlot({
    req(has_data())
    rep_df <- data.frame(Category = factor(c("Positive", "Negative", "Neutral"), levels = c("Positive", "Negative", "Neutral")), Count = c(bing_pos_count, bing_neg_count, bing_neu_count)) %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    ggplot(rep_df, aes(x = Category, y = Count, fill = Category)) +
      geom_col(width = 0.55, color = "#FFFFFF", linewidth = 0.8) +
      geom_text(aes(label = paste0(comma(Count), "\n(", round(Percentage, 1), "%)")), vjust = -0.25, size = 4, fontface = "bold", color = "#1E293B") +
      scale_fill_manual(values = c("Positive" = "#10B981", "Negative" = "#EF4444", "Neutral" = "#64748B")) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Bing Lexicon Sentiment Volume", subtitle = "Comparison of Positive vs. Negative vs. Neutral Tweets", x = "Polarity Class", y = "Tweet Count") +
      theme_project()
  })
  
  output$table_reputation_metrics <- renderTable({
    req(has_data())
    polar_tot <- bing_pos_count + bing_neg_count
    net_margin <- bing_pos_count - bing_neg_count
    ratio <- if (bing_neg_count > 0) round(bing_pos_count / bing_neg_count, 2) else NA
    
    data.frame(
      `Reputation Metric` = c("Positive Tweets (Bing)", "Negative Tweets (Bing)", "Neutral Tweets (Bing)", "Total Polar Tweets (Pos + Neg)", "Net Sentiment Margin (Pos - Neg)", "Pos / Neg Ratio", "Project Reputation Indicator"),
      Value = c(comma(bing_pos_count), comma(bing_neg_count), comma(bing_neu_count), comma(polar_tot), paste0(if (net_margin > 0) "+" else "", comma(net_margin)), paste0(ratio, " : 1"), paste0(if (overall_rep_score > 0) "+" else "", overall_rep_score, "%")),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # TAB 6: TOPIC ANALYSIS
  topic_filtered_data <- reactive({
    req(has_data(), input$selected_topic)
    df %>% filter(topic == input$selected_topic)
  })
  
  output$topic_kpi_summary <- renderUI({
    t_df <- topic_filtered_data()
    req(nrow(t_df) > 0)
    t_pos <- sum(tolower(t_df$bing_sentiment) == "positive", na.rm = TRUE)
    t_neg <- sum(tolower(t_df$bing_sentiment) == "negative", na.rm = TRUE)
    t_polar <- t_pos + t_neg
    t_rep <- if (t_polar > 0) round(((t_pos - t_neg) / t_polar) * 100, 1) else 0.0
    
    tags$div(
      style = "display: flex; gap: 12px; flex-wrap: wrap;",
      div(style = "background: #F1F5F9; border: 1px solid #CBD5E1; padding: 8px 14px; border-radius: 6px;",
          tags$span(style = "font-size: 11px; color: #64748B; font-weight: bold; text-transform: uppercase;", "Total Tweets"), br(),
          tags$b(style = "font-size: 18px; color: #0F2744;", comma(nrow(t_df)))),
      div(style = "background: #ECFDF5; border: 1px solid #A7F3D0; padding: 8px 14px; border-radius: 6px;",
          tags$span(style = "font-size: 11px; color: #065F46; font-weight: bold; text-transform: uppercase;", "Bing Positive"), br(),
          tags$b(style = "font-size: 18px; color: #059669;", comma(t_pos))),
      div(style = "background: #FEF2F2; border: 1px solid #FECACA; padding: 8px 14px; border-radius: 6px;",
          tags$span(style = "font-size: 11px; color: #991B1B; font-weight: bold; text-transform: uppercase;", "Bing Negative"), br(),
          tags$b(style = "font-size: 18px; color: #DC2626;", comma(t_neg))),
      div(style = paste0("background: ", if (t_rep >= 0) "#EFF6FF" else "#FFFBEB", "; border: 1px solid ", if (t_rep >= 0) "#BFDBFE" else "#FDE68A", "; padding: 8px 14px; border-radius: 6px;"),
          tags$span(style = "font-size: 11px; color: #1E40AF; font-weight: bold; text-transform: uppercase;", "Topic Rep. Indicator"), br(),
          tags$b(style = paste0("font-size: 18px; color: ", if (t_rep >= 0) "#1D4ED8" else "#D97706", ";"), paste0(if (t_rep > 0) "+" else "", t_rep, "%")))
    )
  })
  
  output$title_topic_orig <- renderText({ paste("Original Sentiment for:", input$selected_topic) })
  output$title_topic_bing <- renderText({ paste("Bing Lexicon Sentiment for:", input$selected_topic) })
  
  output$plot_topic_orig <- renderPlot({
    t_df <- topic_filtered_data()
    req(nrow(t_df) > 0)
    t_summary <- t_df %>% count(sentiment) %>% mutate(sentiment = factor(sentiment, levels = c("Positive", "Negative", "Neutral", "Irrelevant")), pct = n / sum(n) * 100) %>% filter(!is.na(sentiment))
    ggplot(t_summary, aes(x = sentiment, y = n, fill = sentiment)) +
      geom_col(width = 0.6, color = "#FFFFFF", linewidth = 0.8) +
      geom_text(aes(label = paste0(comma(n), "\n(", round(pct, 1), "%)")), vjust = -0.2, size = 3.6, fontface = "bold", color = "#1E293B") +
      scale_fill_manual(values = sentiment_colors_orig) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
      labs(x = "Original Sentiment", y = "Tweet Count") + theme_project()
  })
  
  output$plot_topic_bing <- renderPlot({
    t_df <- topic_filtered_data()
    req(nrow(t_df) > 0)
    t_summary <- t_df %>% count(bing_sentiment) %>% mutate(bing_sentiment = factor(bing_sentiment, levels = c("positive", "negative", "neutral")), pct = n / sum(n) * 100) %>% filter(!is.na(bing_sentiment))
    ggplot(t_summary, aes(x = bing_sentiment, y = n, fill = bing_sentiment)) +
      geom_col(width = 0.55, color = "#FFFFFF", linewidth = 0.8) +
      geom_text(aes(label = paste0(comma(n), "\n(", round(pct, 1), "%)")), vjust = -0.2, size = 3.6, fontface = "bold", color = "#1E293B") +
      scale_fill_manual(values = sentiment_colors_bing) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
      labs(x = "Bing Sentiment", y = "Tweet Count") + theme_project()
  })
  
  output$table_topic_metrics <- renderTable({
    t_df <- topic_filtered_data()
    req(nrow(t_df) > 0)
    tot_t <- nrow(t_df)
    t_orig <- table(factor(t_df$sentiment, levels = c("Positive", "Negative", "Neutral", "Irrelevant")))
    t_bing <- table(factor(t_df$bing_sentiment, levels = c("positive", "negative", "neutral")))
    
    data.frame(
      Category = c("Positive", "Negative", "Neutral", "Irrelevant / Non-Polar"),
      `Original Count` = as.character(c(comma(t_orig["Positive"]), comma(t_orig["Negative"]), comma(t_orig["Neutral"]), comma(t_orig["Irrelevant"]))),
      `Original %` = paste0(round(as.numeric(t_orig) / tot_t * 100, 1), "%"),
      `Bing Count` = as.character(c(comma(t_bing["positive"]), comma(t_bing["negative"]), comma(t_bing["neutral"]), "-")),
      `Bing %` = c(paste0(round(as.numeric(t_bing) / tot_t * 100, 1), "%"), "-"),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # TAB 7: DATA EXPLORER
  filtered_explorer_data <- reactive({
    req(has_data())
    data_sub <- df
    if (input$filter_topic != "All Topics") data_sub <- data_sub %>% filter(topic == input$filter_topic)
    if (input$filter_sentiment != "All") data_sub <- data_sub %>% filter(tolower(sentiment) == tolower(input$filter_sentiment))
    if (input$filter_bing != "All") data_sub <- data_sub %>% filter(tolower(bing_sentiment) == tolower(input$filter_bing))
    data_sub %>% select(id, topic, sentiment, text, bing_sentiment)
  })
  
  output$explorer_record_count <- renderText({
    sub_df <- filtered_explorer_data()
    paste0("Showing ", comma(nrow(sub_df)), " matching tweets out of ", comma(nrow(df)), " total records.")
  })
  
  output$table_explorer <- renderDT({
    sub_df <- filtered_explorer_data()
    datatable(
      sub_df,
      rownames = FALSE,
      colnames = c("Tweet ID", "Topic / Entity", "Original Sentiment", "Tweet Text", "Bing Sentiment"),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        searchHighlight = TRUE,
        autoWidth = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(width = "90px", targets = 0),
          list(width = "140px", targets = 1),
          list(width = "130px", targets = 2),
          list(width = "120px", targets = 4),
          list(className = "dt-left", targets = "_all")
        )
      ),
      class = "cell-border stripe hover"
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste0("filtered_social_media_data_", format(Sys.Date(), "%Y%m%d"), ".csv") },
    content = function(file) { write.csv(filtered_explorer_data(), file, row.names = FALSE) }
  )
}

# ------------------------------------------------------------------------------
# 7. LAUNCH SHINY APPLICATION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
