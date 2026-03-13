library(pwr)
library(simr)
library(sjPlot)

# sjPlot::tab_model(context_model)
context_pwr <- powerSim(context_model,
                        nsim = 100,
                        test = fcompare(response_num ~ verb * discourse_type))

context_pwr

fixed_context_pwr <- powerSim(fixed_context_model,
                        nsim = 200,
                        test = fcompare(response_num ~ verb * discourse_type))
fixed_context_pwr
