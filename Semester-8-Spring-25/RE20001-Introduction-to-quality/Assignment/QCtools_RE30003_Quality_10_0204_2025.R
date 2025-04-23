## ------------------------------------------------------------------------
cManpower <- c("Receptionist", "Record. Operator", 
               "Storage operators")
cMaterials <- c("Supplier", "Transport agency", 
                "Packing")
cMachines <- c("Compressor type", 
               "Operation conditions",
               "Machine adjustment")
cMethods <- c("Reception", "Transport method")
cMeasurements <- c("Recording method", 
                   "Measurement appraisal")
cGroups <- c("Manpower", "Materials", "Machines", 
             "Methods", "Measurements")
cEffect <- "Too high density"

## ----ceqcc, results='hide', fig.height=4, fig.show='hide'----------------
library(qcc)
cause.and.effect(
  cause = list(Manpower = cManpower,
               Materials = cMaterials,
               Machines = cMachines,
               Methods = cMethods,
               Measurements = cMeasurements),
  effect = cEffect)

## ----cess, results='hide', fig.show='hide', fig.width=5.5, fig.height=4----
library(SixSigma)
ss.ceDiag(
  effect = cEffect,
  causes.gr <- cGroups,
  causes = list(cManpower, cMaterials, cMachines, 
                cMethods, cMeasurements),
  main = "Cause-and-effect diagram",
  sub = "Pellets Density")

## ------------------------------------------------------------------------
data_checkSheet <- rbind(
  data.frame(Group = "Manpower", 
             Cause = cManpower),
  data.frame(Group = "Machines", 
             Cause = cMachines),
  data.frame(Group = "Materials", 
             Cause = cMaterials),
  data.frame(Group = "Methods", 
             Cause = cMethods),
  data.frame(Group = "Measurements", 
             Cause = cMeasurements)
)

## ------------------------------------------------------------------------
data_checkSheet$A_supplier <- NA
data_checkSheet$B_supplier <- NA
data_checkSheet$C_supplier <- NA

## ------------------------------------------------------------------------
pdensity <- c(10.6817, 10.6040, 10.5709, 10.7858, 
              10.7668, 10.8101, 10.6905, 10.6079, 
              10.5724, 10.7736, 11.0921, 11.1023, 
              11.0934, 10.8530, 10.6774, 10.6712, 
              10.6935, 10.5669, 10.8002, 10.7607, 
              10.5470, 10.5555, 10.5705, 10.7723)

## ----firstccbis, fig.show='hide', eval=TRUE------------------------------
myControlChart <- qcc(data = pdensity, 
                      type = "xbar.one")
summary(myControlChart)

## ------------------------------------------------------------------------
myControlChart$violations

## ----hist1, fig.show='hide'----------------------------------------------
hist(pdensity)

## ----hist2, fig.show='hide'----------------------------------------------
par(bg = "gray95")
hist(pdensity, 
     main = "Histogram of pellets density",
     sub = "Data from ceramic process",
     xlab = expression("Density (g"/"cm"^3*")"),
     col = "steelblue",
     border = "white",
     lwd = 2,
     las = 1,
     bg = "gray")

## ----hist3, fig.show='hide'----------------------------------------------
library(lattice)
histogram(pdensity,
    xlab = expression("Pellets density (g"/"cm"^3*")"),
    ylab = "Probability density",
    type = "density",
    panel = function(x, ...) {
        panel.histogram(x, ...)
        panel.mathdensity(dmath = dnorm, 
                          col = "black", 
                          lwd = 3,
                          args = list(mean = mean(x), 
                                      sd = sd(x)))
    } )

## ----hist4, fig.show='hide'----------------------------------------------
library(ggplot2)
ggplot(data = data.frame(pdensity), 
       aes(x = pdensity)) + 
  geom_histogram(fill = "seagreen", 
                 colour = "lightgoldenrodyellow", 
                 binwidth = 0.1) + 
  labs(title = "Histogram", 
       x = expression("Density ("*g/cm^3*")"), 
       y = "Frequency")

## ------------------------------------------------------------------------
data_checkSheet$A_supplier <- c(2, 0, 0, 2, 1, 7, 1, 
                                3, 6, 0, 1, 2, 0)
data_checkSheet$B_supplier <- c(0, 0, 1, 1, 2, 1, 12,
                                1, 2, 1, 0, 0, 1)
data_checkSheet$C_supplier <- c(0, 1, 0, 6, 0, 2, 2, 
                                4, 3, 0, 1, 0, 2)
data_checkSheet$Total <- data_checkSheet$A_supplier + 
  data_checkSheet$B_supplier + 
  data_checkSheet$C_supplier

## ----R.options=list(width=55)--------------------------------------------
data_checkSheet

## ----barplot1, fig.show='hide'-------------------------------------------
barplot(height = data_checkSheet$Total, 
        names.arg = data_checkSheet$Cause)

## ----pareto1, fig.width=6, fig.show='hide'-------------------------------
data_pareto <- data_checkSheet[order(
  data_checkSheet$Total, 
  decreasing = TRUE), ]
par(mar = c(8, 4, 4, 2) + 0.1)
barplot(height = data_pareto$Total, 
        names.arg = data_pareto$Cause, 
        las = 2,
        main = "Pareto chart for total causes")

## ----pareto2, fig.show='hide', R.options=list(width=54)------------------
library(qcc)
data_pareto2 <- data_pareto$Total
names(data_pareto2) <- data_pareto$Cause
#pareto.chart(x = data_pareto2, main = "Out-of-control causes")
pareto.chart(data_pareto2)
## ----pareto3, fig.show='hide', R.options=list(width=52)------------------
#library(qualityTools)
#paretoChart(x = data_pareto2, 
#            main = "Out-of-control causes")

## ----pareto4, fig.show='hide', R.options=list(width=50)------------------
library(qicharts)
spreadvector <- rep(names(data_pareto2), 
    times = data_pareto2)
paretochart(spreadvector)

## ------------------------------------------------------------------------
set.seed(1234)
ptemp <- - 140 + 15*pdensity + rnorm(24)

## ----scatter1, fig.show='hide'-------------------------------------------
plot(pdensity ~ ptemp, 
     col = "gray40", 
     pch = 20,
     main = "Pellets density vs. temperature",
     xlab = "Temperature (Celsius)",
     ylab = expression("Density ("*g/cm^3*")"))

## ------------------------------------------------------------------------
psupplier <- rep(c("A", "B", "C"), each = 8)

## ----bpstrat, fig.show='hide'--------------------------------------------
boxplot(pdensity ~ psupplier,
        col = "gray70",
        xlab = "Supplier",
        ylab = expression("Density ("*g/cm^3*")"),
        main = "Box plots by supplier")

