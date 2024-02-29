#####
# Coffe Shop Project
#####
library(readxl)
data <- "C:/Users/Faus_/OneDrive/Documentos/KU-Ingolstadt/3rd_Semester/Software_Tools/Simulation/Project/Transport/Output.xlsx"
sheet_names <- excel_sheets(data)

# Sheets
sheets <- c("Original_Simulation_2", "Original_Simulation_1", "Modified_Simulation_2", "Modified_Simulation_1")

# Read each sheet into separate variables with the sheets names
for (i in seq_along(sheet_names)) {
        assign(sheets[i], read_excel(data, sheet = sheet_names[i]))
}

####################################################################################
#Parameter1: Vector with observations
#Parameter2: Confidence level alpha

confidence_interval<-function(sample, alpha){
        m<-mean(sample)
        v<-var(sample)
        t<-qt(p=1-(alpha/2),df=length(sample)-1)
        hir<-t*sqrt(v/length(sample))
        lowerb<-m-hir
        upperb<-m+hir
        
        print(paste0("Confidence interval with alpha=", alpha, ": [", lowerb, ", ", upperb,"]"))
        print(paste0("Average Total Revenue: ", m))
}


# Average Total Avenue of the Original Simulation with 2 Employees in Service
######
confidence_interval(Original_Simulation_2$`Total revenue`,0.05)
######
# Average Total Avenue of the Modified Simulation with 2 Employees in Service
######
confidence_interval(Modified_Simulation_2$`Total revenue`,0.05)

# Average Very Satisfied of the Original Simulation with 2 Employees in Service
######
confidence_interval(Original_Simulation_2$`Very satisfied` ,0.05)
######
# Average Very Satisfied of the Modified Simulation with 2 Employees in Service
######
confidence_interval(Modified_Simulation_2$`Very satisfied`,0.05)



### 
# Testing the difference
###

t.test(Modified_Simulation_2$`Total revenue`,Original_Simulation_2$`Total revenue` ,conf.level = 0.95)
t.test(Modified_Simulation_2$`Very satisfied`,Original_Simulation_2$`Very satisfied` ,conf.level = 0.95)
