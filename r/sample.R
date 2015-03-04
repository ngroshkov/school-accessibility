sample_not_accessible_students_by_radius_percent_eqn = function(df){
  m = lm(df[[6]] ~ df[[10]], df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
    list(a = format(coef(m)[1], digits = 2),
         b = format(coef(m)[2], digits = 2),
         r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


sample_not_accessible_students_by_google_percent_eqn = function(df){
  m = lm(df[[6]] ~ df[[14]], df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
    list(a = format(coef(m)[1], digits = 2),
         b = format(coef(m)[2], digits = 2),
         r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

sample_schools_buildings_distance_eqn = function(df){
  m = lm(df[[6]] ~ df[[5]], df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
    list(a = format(coef(m)[1], digits = 2),
         b = format(coef(m)[2], digits = 2),
         r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

sample_not_accessible_students_by_radius_percent <- ggplot(sample, aes(x=sample[[10]], y=sample[[6]])) +
  geom_point(aes(color=factor(sign(sample[[6]]-500)), size=4)) +
  geom_text(aes(label=rownames(sample)), colour="black", size=3, hjust=-0.1, vjust=0) +
  scale_colour_manual(values = c("red","green")) +
  scale_x_continuous(labels = percent, limits = c(min(sample[[10]]), max(sample[[10]]) + 0.15)) +
  theme(legend.position="none") +
  geom_smooth(method=lm, se=FALSE) +
  #geom_text(aes(x = sample[[10]], y = sample[[6]], label = sample_not_accessible_students_by_radius_percent_eqn(sample)), parse = TRUE) +
  xlab(gsub('(.{1,46})(\\s|$)', '\\1\n', sample_colnames[[10]])) +
  ylab(gsub('(.{1,46})(\\s|$)', '\\1\n', sample_colnames[[6]])) ;

sample_not_accessible_students_by_google_percent <- ggplot(sample, aes(x=sample[[14]], y=sample[[6]])) +
  geom_point(aes(color=factor(sign(sample[[6]]-500)), size=4)) +
  geom_text(aes(label=rownames(sample)), colour="black", size=3, hjust=-0.1, vjust=0) +
  scale_colour_manual(values = c("red","green")) +
  scale_x_continuous(labels = percent, limits = c(min(sample[[14]]), max(sample[[14]]) + 0.15)) +
  theme(legend.position="none") +
  geom_smooth(method=lm, se=FALSE) +
  #geom_text(aes(x = sample[[14]], y = sample[[6]], label = sample_not_accessible_students_by_google_percent_eqn(sample)), parse = TRUE) +
  xlab(gsub('(.{1,46})(\\s|$)', '\\1\n', sample_colnames[[14]])) +
  ylab(gsub('(.{1,46})(\\s|$)', '\\1\n', sample_colnames[[6]]));

sample_schools_buildings_distance <- ggplot(sample, aes(x=sample[[5]], y=sample[[6]])) +
  geom_point(aes(color=factor(sign(sample[[6]]-500)), size=4)) +
  geom_text(aes(label=rownames(sample)), colour="black", size=3, hjust=-0.1, vjust=0) +
  scale_colour_manual(values = c("red","green")) +
  scale_x_continuous(limits = c(min(sample[[5]]), max(sample[[5]]) + 150)) +
  theme(legend.position="none") +
  geom_smooth(method=lm, se=FALSE) +
  #geom_text(aes(x = sample[[5]], y = sample[[6]], label = sample_schools_buildings_distance_eqn(sample)), parse = TRUE) +
  xlab(gsub('(.{1,46})(\\s|$)', '\\1\n', sample_colnames[[5]])) +
  ylab(gsub('(.{1,46})(\\s|$)', '\\1\n', sample_colnames[[6]]));

