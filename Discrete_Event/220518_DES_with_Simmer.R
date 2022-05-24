# https://cran.r-project.org/web/packages/simmer/vignettes/simmer-01-introduction.html
install.packages("simmer")
install.packages("simmer.plot")
install.packages("parallel")

library(simmer)

set.seed(42)

env <- simmer("SuperDuperSim")
env

patient <- trajectory("patients' path") %>%
  ## add an intake activity 
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 5)) %>%
  release("administration", 1)

env %>%
  add_resource("nurse", 1) %>%
  add_resource("doctor", 2) %>%
  add_resource("administration", 1) %>%
  add_generator("patient", patient, function() rnorm(1, 10, 2))

env %>% 
  run(80) %>% 
  now()

env %>% peek(3)

#####################################################
library(simmer)

t0 <- trajectory() %>%
  seize("res0", 1) %>%
  branch(function() 1, c(TRUE, FALSE),
         trajectory() %>%
           clone(2,
                 trajectory() %>%
                   seize("res1", 1) %>%
                   timeout(1) %>%
                   release("res1", 1),
                 trajectory() %>%
                   trap("signal",
                        handler=trajectory() %>%
                          timeout(1)) %>%
                   timeout(1)),
         trajectory() %>%
           set_attribute("dummy", 1) %>%
           seize("res2", function() 1) %>%
           timeout(function() rnorm(1, 20)) %>%
           release("res2", function() 1) %>%
           release("res0", 1) %>%
           rollback(11)) %>%
  synchronize() %>%
  rollback(2) %>%
  release("res0", 1)

library(simmer.plot)

get_palette <- scales::brewer_pal(type = "qual", palette = 1)
plot(t0, fill = get_palette)

#######################################################
library(simmer)
library(parallel)

patient <- trajectory("patients' path") %>%
  ## add an intake activity 
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 5)) %>%
  release("administration", 1)

envs <- mclapply(1:100, function(i) {
  simmer("SuperDuperSim") %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", patient, function() rnorm(1, 10, 2)) %>%
    run(80) %>%
    wrap()
})

library(simmer.plot)

resources <- get_mon_resources(envs)
plot(resources, metric = "utilization")

plot(resources, metric = "usage", c("nurse", "doctor"), items = "server")

plot(get_mon_resources(envs[[6]]), metric = "usage", "doctor", items = "server", steps = TRUE)
