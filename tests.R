

###########################################

my_cwsi <-CWSI$new()

my_cwsi$load_canopyTemp(array(c(30, 30, 03, 30, 30, 30), dim = c(6)), time_series = TRUE)

my_cwsi$load_micrometeo(Rs = c(200, 120, 130, 124, 150, 180),
                        Ta = c(20, 21, 19, 22, 20, 21),
                        u = c(1, 2, 1.5, 1.8, 2.5, 1.2),
                        rH = c(50, 60, 55, 65, 70, 75),
                        h = c(0.5),
                        z = c(10))

my_cwsi$calc_CWSI(method = "theoretical")






