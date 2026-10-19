# Check the employment data against official national CAGED series
# published by IpeaData (requires internet access), and against the
# state counts built from raw PDET microdata (Dados/emprego_uf.csv, made
# by R/pdet_extract.R).

library(testthat)

ipea_series <- function(code) {
  url <- sprintf(
    "http://www.ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='%s')",
    code
  )
  x <- jsonlite::fromJSON(url)$value
  data.frame(month = substr(x$VALDATA, 1, 7), value = x$VALVALOR)
}

emprego <- read_caged_uf(here::here("Dados/caged_uf_2004_2019.csv"))
national <- aggregate(
  cbind(admissions, dismissals) ~ month,
  data = emprego,
  FUN = sum
)
microdata <- read.csv(here::here("Dados/emprego_uf.csv"))

compare_national <- function(adm_code, dis_code) {
  x <- national
  adm <- ipea_series(adm_code)
  dis <- ipea_series(dis_code)
  x$ipea_adm <- adm$value[match(x$month, adm$month)]
  x$ipea_dis <- dis$value[match(x$month, dis$month)]
  x
}

test_that("every month has all 27 states", {
  expect_true(all(table(emprego$month) == 27))
  months <- sort(unique(emprego$month))
  expected <- format(
    seq(as.Date(paste0(months[1], "-01")), by = "month", length.out = length(months)),
    "%Y-%m"
  )
  expect_equal(months, expected)
})

test_that("national totals match the official series", {
  x <- compare_national("CAGED12_ADMIS", "CAGED12_DESLIG")
  expect_false(anyNA(x$ipea_adm))
  # IpeaData gives 1,248,002 dismissals for March 2018, a digit
  # transposition of 1,284,002: its own net balance series (56,151)
  # agrees with the microdata, not with its dismissals figure.
  x <- x[x$month != "2018-03", ]
  expect_lt(max(abs(x$admissions / x$ipea_adm - 1)), 0.001)
  expect_lt(max(abs(x$dismissals / x$ipea_dis - 1)), 0.001)
})

test_that("state counts match the raw microdata exactly", {
  raw <- microdata[microdata$source == "CAGED", ]
  x <- merge(raw, emprego, by = c("month", "UF"))
  expect_equal(nrow(x), nrow(raw))
  expect_equal(x$admissions.x, x$admissions.y)
  expect_equal(x$dismissals.x, x$dismissals.y)
})

test_that("remapped legacy file agrees with raw extraction for intact months", {
  raw <- microdata[microdata$source == "CAGED", ]
  x <- merge(raw, read_legacy(), by = c("month", "UF"))
  expect_equal(nrow(x), nrow(raw))
  agree <- x$admissions.x == x$admissions.y & x$dismissals.x == x$dismissals.y
  # One state-month (PR, February 2010) differs by a single admission
  expect_gte(mean(agree), 0.999)
  expect_lte(
    max(abs(x$admissions.x - x$admissions.y), abs(x$dismissals.x - x$dismissals.y)),
    1
  )
})
