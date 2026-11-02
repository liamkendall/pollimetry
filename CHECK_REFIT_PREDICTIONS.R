# Run from the pollimetry project root.
# Tests local prediction functions against the installed pollimetrydata package.
# Does not read saved refit files or modify model objects.
local({
  e <- new.env(parent = globalenv())
  for (nm in c('equation_validation', 'prediction_helpers', 'bodysize', 'foraging.range'))
    sys.source(file.path('R', paste0(nm, '.R')), e)
  stopifnot(requireNamespace("pollimetrydata", quietly = TRUE),
            utils::packageVersion("pollimetrydata") >= "1.0.1")
  cat("Testing installed pollimetrydata", as.character(utils::packageVersion("pollimetrydata")),
      "from", find.package("pollimetrydata"), "\n")
  results <- list()
  check <- function(label, code) {
    result <- tryCatch({force(code); 'PASS'}, error = function(err) paste('FAIL:', conditionMessage(err)))
    results[[length(results) + 1L]] <<- data.frame(check = label, result = result)
  }
  valid <- function(y, n, columns) {
    stopifnot(nrow(y) == n, all(is.finite(as.matrix(y[, columns]))),
              all(as.matrix(y[, columns]) > 0), all(y[[columns[2]]] <= y[[columns[3]]]))
  }
  mapping <- c(bee_IT='ITD', bee_sex='sex', bee_tax_mod='taxo', bee_phy_mod='phylo',
               hov_IT='ITD', hov_sex='sex', hov_tax_mod='taxo')
  for (nm in names(mapping)) {
    check(nm, {
      m <- e$.model(nm)
      x <- m$data[1:2, intersect(c('IT','Sex','Family','Subfamily','Region','Species'),names(m$data)),drop=FALSE]
      x$ITD <- x$IT
      set.seed(42)
      y <- e$bodysize(x, if (grepl('^bee',nm)) 'bee' else 'hov', mapping[[nm]])
      valid(y,2,c('Est.Weight','CI_Lower','CI_Upper'))
    })
  }
  check('Pipizinae', {
    m <- e$.model('hov_tax_mod')
    i <- which(as.character(m$data$Subfamily) == 'Pipizinae')[1]
    stopifnot(!is.na(i))
    x <- m$data[i, c('IT','Sex','Subfamily','Region','Species'),drop=FALSE]; x$ITD <- x$IT
    valid(e$bodysize(x,'hov','taxo'),1,c('Est.Weight','CI_Lower','CI_Upper'))
  })
  for (mt in c('social','ITD')) for (re in c('reduced','phylo','full')) for (me in c('realized','potential')) {
    check(paste('foraging',mt,re,me,sep='/'), {
      m <- e$.model(if(mt=='social') 'all_social_bm_mod' else 'all_IT_bm_mod')
      x <- data.frame(ITD=c(2,3),species=as.character(m$data$species[1:2]), sociality=c('Solitary','Highly Eusocial'))
      set.seed(42)
      y <- e$foraging.range(x,mt,re,me)
      valid(y,4,c('range.estimate','range.lower','range.upper'))
      stopifnot(identical(y$ITD,rep(x$ITD,each=2)),y$wgt[1]==y$wgt[2],y$wgt[3]==y$wgt[4])
    })
  }
  report <- do.call(rbind, results)
  print(report, row.names = FALSE)
  if (any(report$result != "PASS")) stop("Some prediction checks failed; see the table above.", call. = FALSE)
})
