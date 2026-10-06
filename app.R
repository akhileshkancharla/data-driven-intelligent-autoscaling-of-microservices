## Intelligent Autoscaling of Microservices — real trace, simulated policies
## Run: Rscript app.R --prepare   or   Rscript -e "shiny::runApp('app.R')"

root <- if (file.exists("dataset-replica-mcr-mt.csv")) "." else "outputs"
raw_path <- file.path(root, "dataset-replica-mcr-mt.csv")
series_path <- file.path(root, "selected_service.csv")
model_path <- file.path(root, "forecast_model.rds")
metrics_path <- file.path(root, "test_metrics.csv")

mae <- function(a, b) mean(abs(a - b), na.rm = TRUE)
rmse <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))

prepare_data <- function() {
  if (!file.exists(raw_path)) stop("Source CSV missing: ", raw_path)
  raw <- read.csv(raw_path, header = FALSE, colClasses = c("numeric", "numeric", "character", "character", "numeric", "numeric", "numeric"))
  names(raw) <- c("timestamp_1", "timestamp_2", "service_id", "container_id", "observed_replicas", "service_time", "call_rate")
  raw <- raw[is.finite(raw$timestamp_2) & is.finite(raw$call_rate) & raw$call_rate >= 0, ]
  if (!nrow(raw)) stop("No valid rows in source CSV")
  ## Collapse repeated container/time rows first. Call rate is identical for
  ## these repeats in this file; median also handles varying replica reports.
  key <- paste(raw$service_id, raw$timestamp_2, raw$container_id, sep = "|")
  container_group <- split(seq_len(nrow(raw)), key)
  raw <- do.call(rbind, lapply(container_group, function(ii) {
    d <- raw[ii[1], ]
    d$call_rate <- median(raw$call_rate[ii])
    d$observed_replicas <- median(raw$observed_replicas[ii], na.rm = TRUE)
    d
  }))
  group <- split(seq_len(nrow(raw)), paste(raw$service_id, raw$timestamp_2, sep = "|"))
  buckets <- do.call(rbind, lapply(group, function(ii) {
    d <- raw[ii, ]
    data.frame(service_id = d$service_id[1], timestamp_ms = d$timestamp_2[1],
               call_rate = median(d$call_rate),
               observed_replicas = median(d$observed_replicas, na.rm = TRUE),
               containers = nrow(d))
  }))
  buckets <- buckets[order(buckets$service_id, buckets$timestamp_ms), ]
  ## Select the longest contiguous, nonconstant 60,000-unit run. Tie-break by ID.
  by_service <- split(buckets, buckets$service_id)
  candidates <- lapply(by_service, function(d) {
    d <- d[order(d$timestamp_ms), ]
    run_id <- cumsum(c(TRUE, diff(d$timestamp_ms) != 60000))
    runs <- split(d, run_id)
    runs <- Filter(function(x) nrow(x) >= 20 && sd(x$call_rate) > 0, runs)
    if (!length(runs)) return(NULL)
    runs[[which.max(vapply(runs, nrow, integer(1)))]]
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) stop("No service with a suitable contiguous nonconstant run")
  lengths <- vapply(candidates, nrow, integer(1))
  winner <- names(candidates)[which.max(lengths)]
  d <- candidates[[winner]]
  d$minute <- seq_len(nrow(d)) - 1L
  d <- d[, c("minute", "timestamp_ms", "service_id", "call_rate", "observed_replicas", "containers")]
  rownames(d) <- NULL
  write.csv(d, series_path, row.names = FALSE)
  d
}

make_features <- function(d) {
  n <- nrow(d)
  if (n < 30) stop("Selected series is too short")
  i <- 4:(n - 1)
  data.frame(origin = i, target_minute = d$minute[i + 1],
             current = d$call_rate[i], lag1 = d$call_rate[i - 1],
             lag2 = d$call_rate[i - 2], lag3 = d$call_rate[i - 3],
             mean3 = (d$call_rate[i] + d$call_rate[i - 1] + d$call_rate[i - 2]) / 3,
             growth = d$call_rate[i] - d$call_rate[i - 2],
             actual = d$call_rate[i + 1])
}

train_model <- function(d) {
  if (!requireNamespace("rpart", quietly = TRUE)) stop("Install rpart first")
  f <- make_features(d)
  n <- nrow(f)
  train_end <- floor(n * .60)
  valid_end <- floor(n * .80)
  tr <- seq_len(train_end)
  va <- (train_end + 1):valid_end
  te <- (valid_end + 1):n
  formula <- actual ~ current + lag1 + lag2 + lag3 + mean3 + growth
  cps <- c(.0001, .001, .005, .01, .02, .05)
  errors <- vapply(cps, function(cp) {
    m <- rpart::rpart(formula, f[tr, ], method = "anova",
                      control = rpart::rpart.control(cp = cp, minsplit = 8, minbucket = 3, maxdepth = 5, xval = 0))
    mae(f$actual[va], pmax(0, predict(m, f[va, ])))
  }, numeric(1))
  chosen_cp <- cps[which.min(errors)]
  model <- rpart::rpart(formula, f[seq_len(valid_end), ], method = "anova",
                         control = rpart::rpart.control(cp = chosen_cp, minsplit = 8,
                                                        minbucket = 3, maxdepth = 5, xval = 0))
  f$forecast <- pmax(0, as.numeric(predict(model, f)))
  f$baseline <- f$current
  f$split <- ifelse(seq_len(n) <= train_end, "Train", ifelse(seq_len(n) <= valid_end, "Validation", "Test"))
  metrics <- data.frame(method = c("rpart", "Persistence"),
                        MAE = c(mae(f$actual[te], f$forecast[te]), mae(f$actual[te], f$baseline[te])),
                        RMSE = c(rmse(f$actual[te], f$forecast[te]), rmse(f$actual[te], f$baseline[te])),
                        test_origins = length(te))
  bundle <- list(model = model, service_id = d$service_id[1], n_buckets = nrow(d),
                 train_origins = length(tr), validation_origins = length(va), test_origins = length(te),
                 cp = chosen_cp, features = f, series = d, metrics = metrics,
                 source = "https://zenodo.org/records/14245634")
  saveRDS(bundle, model_path)
  write.csv(metrics, metrics_path, row.names = FALSE)
  bundle
}

## At minute t, reactive sees current demand. Predictive sees only the
## forecast made at t-1 for t. A recommendation takes startup_delay steps.
simulate_policy <- function(d, f, policy, capacity, safety, min_rep, max_rep,
                            startup_delay, fixed_rep, begin, end) {
  ix <- which(d$minute >= begin & d$minute <= end)
  if (!length(ix)) return(data.frame())
  ## All policies start from the same chosen replica count for a fair window.
  active <- fixed_rep
  pending_time <- integer(0); pending_rep <- integer(0)
  out <- vector("list", length(ix))
  forecast_by_minute <- setNames(f$forecast, f$target_minute)
  for (j in seq_along(ix)) {
    k <- ix[j]; minute <- d$minute[k]
    due <- which(pending_time <= minute)
    if (length(due)) {
      active <- tail(pending_rep[due], 1)
      pending_time <- pending_time[-due]; pending_rep <- pending_rep[-due]
    }
    demand <- d$call_rate[k]
    action <- 0L
    if (policy != "Fixed") {
      signal <- if (policy == "Reactive") demand else forecast_by_minute[as.character(minute + 1)]
      if (length(signal) && is.finite(signal)) {
        target <- min(max_rep, max(min_rep, ceiling(signal * safety / capacity)))
        queued <- if (length(pending_rep)) tail(pending_rep, 1) else active
        if (target != queued) {
          action <- 1L
          if (startup_delay == 0L) active <- target
          else { pending_time <- c(pending_time, minute + startup_delay)
                 pending_rep <- c(pending_rep, target) }
        }
      }
    }
    out[[j]] <- data.frame(minute = minute, policy = policy, actual = demand,
                            replicas = active, overloaded = as.integer(demand > active * capacity),
                            actions = action)
  }
  do.call(rbind, out)
}

make_replay <- function(bundle, capacity, safety, min_rep, max_rep, delay, fixed_rep, begin, end) {
  do.call(rbind, lapply(c("Fixed", "Reactive", "Predictive"), function(p)
    simulate_policy(bundle$series, bundle$features, p, capacity, safety,
                    min_rep, max_rep, as.integer(delay), fixed_rep, begin, end)))
}

if ("--prepare" %in% commandArgs(trailingOnly = TRUE)) {
  d <- prepare_data(); b <- train_model(d)
  cat("Selected service:", b$service_id, "\nSelected observed buckets:", b$n_buckets,
      "\nTrain/validation/test origins:", b$train_origins, b$validation_origins, b$test_origins,
      "\nObserved cadence: 60,000 timestamp units\n")
  print(b$metrics, row.names = FALSE)
} else {
  if (!requireNamespace("shiny", quietly = TRUE)) stop("Install shiny first")
  if (!file.exists(model_path)) { d <- prepare_data(); train_model(d) }
  bundle <- readRDS(model_path)
  d <- bundle$series; f <- bundle$features
  test <- f[f$split == "Test", ]
  eligible_rises <- which(d$minute[-1] %in% test$target_minute)
  rise <- eligible_rises[which.max(diff(d$call_rate)[eligible_rises])] + 1L
  demo_begin <- max(min(d$minute), d$minute[rise] - 8L)
  demo_end <- min(max(d$minute), d$minute[rise] + 12L)

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML("body{background:#f4f7fb;color:#162337;font-family:Segoe UI,Arial}.container-fluid{max-width:1380px;margin:auto}.hero{background:linear-gradient(115deg,#142e52,#136d8f);color:white;padding:24px 30px;border-radius:16px;margin:18px 0}.hero h1{margin:0 0 8px;font-weight:700}.hero p{margin:0;color:#d8eff8}.well,.tab-content{background:white;border:1px solid #dce6ee;border-radius:14px;box-shadow:0 4px 18px #1634550d}.tab-content{padding:22px;margin-top:12px}.metric{display:inline-block;background:#eaf5fa;border-radius:10px;padding:12px 18px;margin:7px 8px 7px 0;font-weight:600}.note{color:#526377;font-size:13px}.nav-tabs>li>a{font-weight:600}"))),
    shiny::div(class="hero", shiny::h1("Intelligent Autoscaling of Microservices"),
               shiny::p("Real Alibaba-derived trace · 60-second-ahead rpart forecast · simulated scaling replay")),
    shiny::fluidRow(
      shiny::column(3, shiny::wellPanel(
        shiny::h4("Replay controls"),
        shiny::numericInput("capacity", "Heuristic capacity / replica (call-rate units)", 50, min=1, step=5),
        shiny::sliderInput("safety", "Safety factor", min=1, max=2, value=1.2, step=.05),
        shiny::numericInput("minrep", "Minimum replicas", 1, min=1),
        shiny::numericInput("maxrep", "Maximum replicas", 10, min=1),
        shiny::numericInput("fixed", "Fixed-policy replicas", 2, min=1),
        shiny::sliderInput("delay", "Replica startup delay (minutes)", min=0, max=5, value=1, step=1),
        shiny::actionButton("demo", "Load observed rise", class="btn-primary"),
        shiny::br(), shiny::br(),
        shiny::sliderInput("window", "Replay window (elapsed minutes)",
                           min=min(d$minute), max=max(d$minute), value=c(demo_begin,demo_end), step=1),
        shiny::div(class="note", "Capacity is a user-chosen heuristic. Overload means call rate exceeds simulated replica capacity; service-time changes are not inferred."))),
      shiny::column(9, shiny::tabsetPanel(
        shiny::tabPanel("Real Trace", shiny::h3("One selected service, observed traffic"),
          shiny::div(class="metric", paste(nrow(d), "minute buckets")),
          shiny::div(class="metric", paste("Service", substr(bundle$service_id,1,14), "…")),
          shiny::plotOutput("trace_plot", height="360px"),
          shiny::p(class="note", "Blue: median container-row call rate per timestamp. Gray: median reported replica count, on its own axis. The prepared window includes a rise in the held-out test segment. The source calls these 30-second intervals, but this file's second timestamp advances by 60,000 units; no intermediate observations were invented.")),
        shiny::tabPanel("Forecast", shiny::h3("Forecast one observed step / 60 seconds ahead"),
          shiny::plotOutput("forecast_plot", height="360px"),
          shiny::h4("Held-out test results"), shiny::tableOutput("metrics"),
          shiny::p(class="note", "Chronological 60% train / 20% validation / 20% test origins. Hyperparameter chosen on validation; final model trained on train + validation; test kept untouched. Persistence predicts the current call rate.")),
        shiny::tabPanel("Scaling Replay", shiny::h3("Same observed demand, three simulated policies"),
          shiny::plotOutput("replay_plot", height="460px"),
          shiny::p(class="note", "Fixed holds the chosen count. Reactive targets current traffic. Predictive targets the one-minute-ahead forecast. Actions take the configured startup delay. Results are simulations, not Kubernetes operations.")),
        shiny::tabPanel("Comparison", shiny::h3("Simulated outcomes over selected window"),
          shiny::tableOutput("comparison"),
          shiny::plotOutput("comparison_plot", height="330px"),
          shiny::p(class="note", "Overload intervals count observed minutes above the simulated capacity. Replica-minutes measure allocated simulated replicas. No measured latency or service-time improvement is claimed."))
      ))),
    shiny::hr(), shiny::p(class="note", "Data: Mehran et al., A trace of microservice time, call rate, and number of replicas, Zenodo (2024), DOI: 10.5281/zenodo.14245634. Derived from Alibaba 2021 microservice traces. RL/PPO and live Kubernetes control are future extensions.")
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$demo, {
      shiny::updateSliderInput(session, "window", value=c(demo_begin,demo_end))
      shiny::updateNumericInput(session, "capacity", value=50)
      shiny::updateSliderInput(session, "safety", value=1.2)
      shiny::updateSliderInput(session, "delay", value=1)
      shiny::updateNumericInput(session, "fixed", value=2)
    })
    replay <- shiny::reactive({
      shiny::req(input$minrep <= input$maxrep)
      make_replay(bundle, input$capacity, input$safety, input$minrep,
                  input$maxrep, input$delay, input$fixed, input$window[1], input$window[2])
    })
    output$trace_plot <- shiny::renderPlot({
      op <- par(mar=c(4,4,2,4)); on.exit(par(op))
      plot(d$minute,d$call_rate,type="l",lwd=2.5,col="#137da8",xlab="Elapsed minutes in selected run",ylab="Observed call rate (source units)")
      abline(v=input$window,col="#eda442",lty=2)
      par(new=TRUE); plot(d$minute,d$observed_replicas,type="l",col="#8595a8",axes=FALSE,xlab="",ylab="")
      axis(4,col.axis="#65778a"); mtext("Reported replicas (context)",4,line=2.5,col="#65778a")
      legend("topleft",c("Call rate","Reported replicas","Replay window"),col=c("#137da8","#8595a8","#eda442"),lty=c(1,1,2),bty="n")
    })
    output$forecast_plot <- shiny::renderPlot({
      plot(test$target_minute,test$actual,type="l",lwd=3,col="#172f54",xlab="Elapsed minute",ylab="Call rate (source units)",ylim=range(c(test$actual,test$forecast,test$baseline)))
      lines(test$target_minute,test$forecast,col="#07a6a6",lwd=2)
      lines(test$target_minute,test$baseline,col="#e59d3e",lwd=2,lty=2)
      legend("topleft",c("Actual","rpart forecast","Persistence"),col=c("#172f54","#07a6a6","#e59d3e"),lty=c(1,1,2),lwd=2,bty="n")
    })
    output$metrics <- shiny::renderTable({ transform(bundle$metrics, MAE=round(MAE,2), RMSE=round(RMSE,2)) }, striped=TRUE)
    output$replay_plot <- shiny::renderPlot({
      x <- replay(); cols <- c(Fixed="#7b91aa",Reactive="#e39839",Predictive="#008f9c")
      op <- par(mfrow=c(2,1),mar=c(3,4,2,1),oma=c(1,0,0,0)); on.exit(par(op))
      a <- x[x$policy=="Fixed",]
      plot(a$minute,a$actual,type="l",lwd=2,col="#172f54",xlab="",ylab="Observed call rate")
      abline(h=input$fixed*input$capacity,lty=3,col="#7b91aa")
      legend("topleft",c("Observed demand","Fixed capacity"),col=c("#172f54","#7b91aa"),lty=c(1,3),bty="n")
      plot(range(x$minute),range(x$replicas),type="n",xlab="Elapsed minute",ylab="Simulated replicas")
      for(p in names(cols)) {z <- x[x$policy==p,]; lines(z$minute,z$replicas,type="s",col=cols[p],lwd=2)}
      legend("topleft",names(cols),col=cols,lty=1,lwd=2,bty="n",horiz=TRUE)
    })
    output$comparison <- shiny::renderTable({
      x <- replay(); z <- split(x,x$policy)
      data.frame(Policy=names(z),`Overload intervals`=vapply(z,function(y) sum(y$overloaded),numeric(1)),
                 `Replica-minutes`=vapply(z,function(y) sum(y$replicas),numeric(1)),
                 `Scaling actions`=vapply(z,function(y) sum(y$actions),numeric(1)),check.names=FALSE)
    }, striped=TRUE)
    output$comparison_plot <- shiny::renderPlot({
      x <- replay(); z <- split(x,x$policy)
      overload <- vapply(z,function(y) sum(y$overloaded),numeric(1))
      barplot(overload,col=c(Fixed="#7b91aa",Reactive="#e39839",Predictive="#008f9c")[names(overload)],
              ylab="Simulated overload intervals",main="Selected observed window")
    })
  }
  shiny::shinyApp(ui, server)
}
