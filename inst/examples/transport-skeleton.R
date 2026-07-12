library(GAMSr)

m <- gams_model("transport")
i <- gams_set(m, "i", records = c("seattle", "san-diego"))
j <- gams_set(m, "j", records = c("new-york", "chicago", "topeka"))

a <- gams_parameter(m, "a", domain = i, records = c(seattle = 350, `san-diego` = 600))
b <- gams_parameter(m, "b", domain = j, records = c(`new-york` = 325, chicago = 300, topeka = 275))
x <- gams_variable(m, "x", domain = c(i, j), type = "positive")
supply <- gams_equation(m, "supply", domain = i)
demand <- gams_equation(m, "demand", domain = j)

list(model = m, sets = list(i = i, j = j), parameters = list(a = a, b = b), variable = x,
     equations = list(supply = supply, demand = demand))
