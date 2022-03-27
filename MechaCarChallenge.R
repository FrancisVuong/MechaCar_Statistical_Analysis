```{r}
library(tidyverse) 
```

```{r}
mechaCarData <- read_csv("Resources/MechaCar_mpg.csv") 
head(mechaCarData) 
```

```{r}
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + AWD + ground_clearance,data=mechaCarData) 
```

```{r}
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + AWD + ground_clearance,data=mechaCarData)) 
```

```{r}
suspension_coil_data <- read_csv("Resources/Suspension_Coil.csv") 
head(suspension_coil_data)
```

```{r}
total_summary <- suspension_coil_data %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') 
total_summary
```

```{r}
lot_summary <- suspension_coil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') 
lot_summary
```

```{r}
global_sample_table <- suspension_coil_data %>% sample_n(50) 
```

```{r}
plt <- ggplot(suspension_coil_data,aes(x=PSI)) 
plt + geom_density() 
```

```{r}
plt <- ggplot(global_sample_table,aes(x=PSI)) 
plt + geom_density() 
```

```{r}
plt <- ggplot(global_sample_table,aes(x=log10(PSI))) 
plt + geom_density() 
```

```{r}
t.test(global_sample_table$PSI,mu=mean(suspension_coil_data$PSI)) 
```

```{r}
psi_lot1_sample <- suspension_coil_data %>% subset(Manufacturing_Lot=='Lot1') %>% sample_n(25) 
t.test(psi_lot1_sample$PSI,mu=mean(suspension_coil_data$PSI)) 
```

```{r}
psi_lot2_sample <- suspension_coil_data %>% subset(Manufacturing_Lot=='Lot2') %>% sample_n(25) 
t.test(psi_lot2_sample$PSI,mu=mean(suspension_coil_data$PSI))
```

```{r}
psi_lot3_sample <- suspension_coil_data %>% subset(Manufacturing_Lot=='Lot3') %>% sample_n(25) 
t.test(psi_lot3_sample$PSI,mu=mean(suspension_coil_data$PSI)) 
