

packageStartupMessage('Compiling models (this will take a minute...)')

dest <- file.path(R_PACKAGE_DIR, paste0('libs', R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

packageStartupMessage(paste('Writing models to:', dest))
packageStartupMessage(paste('Compiling using binary:', R.home('bin')))


linear.growth.src <- file.path(R_PACKAGE_SOURCE, 'inst', 'stan', 'prophet_linear_growth.stan')
linear.growth.binary <- file.path(dest, 'prophet_linear_growth.RData')
linear.growth.stanc <- rstan::stanc(linear.growth.src)
linear.growth.stanm <- rstan::stan_model(stanc_ret = linear.growth.stanc,
                                         model_name = 'linear_growth')
save('linear.growth.stanm', file = linear.growth.binary)

packageStartupMessage('------ Models successfully compiled!')
packageStartupMessage('You can ignore any compiler warnings above.')
