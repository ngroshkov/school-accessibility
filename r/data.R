data_not_accessible_students_by_radius_percent_eqn = function(df){
  m = lm(df[[2]] ~ df[[3]], df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

data_not_accessible_students_by_google_percent_eqn = function(df){
  m = lm(df[[2]] ~ df[[4]], df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,
                   list(a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


data_colnames = c("Округ",
                  "Среднее число обучающихся на одну школу",
                  "Процент школьников, проживающих на недоступной территории по радиусу 500 от школ, от всех школьников жилой зоны",
                  "Процент школьников, проживающих на недоступной от школ территории по данным Google, от всех школьников жилой зоны",
                  "Число школьников, проживающих дальше 500 м от ближайшей школы",
                  "Число школьников, проживающих дальше 500 м от ближайшей школы по пешеходным маршрутам");

colors_students_per_school_mean = c("#730000", "#A80000", "#e60000", "#ff7f7f", "#a3ff73", "#55ff00", "#4ce600", "#38a800", "#267300", "#264500");

freqhist_students_per_school_mean_hist <- ggplot(data, aes(x=data$students_per_school_mean)) +
  geom_histogram(aes(fill=cut(data$students_per_school_mean, breaks=seq(99, 999, by = 100))), binwidth = 25) +
  #geom_density(alpha=.2) +
  scale_x_continuous(name="Среднее число обучающихся на одну школу", breaks=seq(0, 1000, by = 100), limits=c(0, 1000)) +
  scale_y_continuous(name="Частота распределения", breaks=0:20) +
  scale_fill_manual("", breaks=levels(cut(data$students_per_school_mean, breaks=seq(99, 999, by = 100), right = FALSE)), values=colors_students_per_school_mean) +
  theme(legend.position="none")



freqhist_not_accessible_students_by_radius <- ggplot(data, aes(x=data$not_accessible_area_by_radius)) +
  geom_histogram(aes(y=..count..), colour="black", fill="white", binwidth = 50) +
  #geom_density(alpha=.2) +
  scale_x_continuous(name="Число школьников, проживающих дальше 500 м \n от ближайшей школы", breaks=seq(0, 3400, by = 200)) +
  scale_y_continuous(name="Частота распределения", breaks=0:15)

freqhist_not_accessible_students_by_google <- ggplot(data, aes(x=data$not_accessible_area_by_google)) +
  geom_histogram(aes(y=..count..), colour="black", fill="white", binwidth = 250) +
  #geom_density(alpha=.2) +
  scale_x_continuous(name="Число школьников, проживающих дальше 500 м \n от ближайшей школы по пешеходным маршрутам",  breaks=seq(0, 10000, by = 500)) +
  scale_y_continuous(name="Частота распределения", breaks=0:15)



lm_eqn = function(df, x, y){
  m = lm(y ~ x, df);
  eq <- substitute(italic(Ŷ) == a + b %.% italic(X)*","~~italic(R)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

scat_not_accessible_students_by_radius_ON_students_per_school_mean <- ggplot(data, aes(x=data$not_accessible_area_by_radius, y=data$students_per_school_mean)) +
  geom_point(aes(color=cut(data$students_per_school_mean, breaks=seq(99, 999, by = 100)), size=4)) +
  #geom_text(aes(label=rownames(data)), colour="black", size=3, hjust=-0.1, vjust=0) +
  scale_colour_manual("", values=colors_students_per_school_mean) +
  #scale_colour_manual(values = c("red","green")) +
  scale_x_continuous(limits = c(min(data$not_accessible_area_by_radius), max(data$not_accessible_area_by_radius) + 0.0001)) +
  scale_y_continuous(limits = c(0, max(data$students_per_school_mean))) +
  theme(legend.position="none") +
  geom_smooth(method=lm) +
  geom_text(aes(x = 2000, y = 1050, label = lm_eqn(data, data$not_accessible_area_by_radius, data$students_per_school_mean)), parse = TRUE, family="serif", fontface="italic", size=16) +
  #geom_text(aes(x = data$not_accessible_area_by_radius, y = data$students_per_school_mean, label = not_accessible_students_by_radius_percent_eqn(data)), parse = TRUE) +
  xlab("Число школьников, проживающих дальше 500 м \n от ближайшей школы") +
  ylab("Среднее число обучающихся на одну школу");

scat_not_accessible_students_by_google_ON_students_per_school_mean <- ggplot(data, aes(x=data$not_accessible_area_by_google, y=data$students_per_school_mean)) +
  geom_point(aes(color=cut(data$students_per_school_mean, breaks=seq(99, 999, by = 100)), size=4)) +
  #geom_text(aes(label=rownames(data)), colour="black", size=3, hjust=-0.1, vjust=0) +
  scale_colour_manual("", values=colors_students_per_school_mean) +
  scale_x_continuous(limits = c(min(data$not_accessible_area_by_google), max(data$not_accessible_area_by_google) + 0.0001)) +
  scale_y_continuous(limits = c(0, max(data$students_per_school_mean))) +
  theme(legend.position="none") +
  geom_smooth(method=lm) +
  geom_text(aes(x = 6750, y = 1050, label = lm_eqn(data, data$not_accessible_area_by_google, data$students_per_school_mean)), parse = TRUE, family="serif", fontface="italic", size=16) +
  #geom_text(aes(x = data[[3]], y = data[[2]], label = not_accessible_students_by_google_percent_eqn(data)), parse = TRUE) +
  xlab("Число школьников, проживающих дальше 500 м \n от ближайшей школы по пешеходным маршрутам") +
  ylab("Среднее число обучающихся на одну школу");







