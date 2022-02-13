
library(data.table)
library(usmap)
library(ggplot2)
library(gridExtra)


d <- readRDS("data/US_processed/US_single_state_fitted_values.rds")

# Column descriptions for data/US_single_state_fitted_values.rds :

#	state = two-letter state abbreviations
#	pop_size = state population size
#	date = date
#	c_cases = confirmed infections
#	y_cases = predicted total infections (MERMAID)
#	Lambda = predicted infection potential (MERMAID)
#   R_t = estimated effective reproductive number R_t (MERMAID)
#   pi_t = estimated ascertainment probability (MERMAID)


# ------------------------------------------------------------------------------------------

# Prepare data for plotting

tmp <- merge(
	d[,list(
		"Ascertainment"=100*sum(c_cases)/sum(y_cases), 
		"Mean_Ascertainment" = 100*mean(pi_t),
		"Prevalence"=100*sum(y_cases)/pop_size[1],
		"Confirmed"=100*sum(c_cases)/pop_size[1],
		"Peak_date" = date[which.max(y_cases)],
		"Median_date" = median( data.table(date,y=round(y_cases))[,list(rep(date,y[1])),by=date]$V1 ),
		"Mean_date" = weighted.mean(x = date, w = y_cases, na.rm = TRUE)
	),by=list(abbr=state)],
	usmap::statepop, by = "abbr"
)

# ------------------------------------------------------------------------------------------


q1 <- ggplot(tmp[order(-Prevalence)], aes(y = Prevalence, x = Mean_date)) + stat_smooth(method = 'lm', colour = 'black', size = 0.25, fill = 'gray', alpha = 0.25) + geom_point(aes(size=pop_2015),shape=21,colour='gray',fill='lightblue')  + theme_bw() + theme(legend.position = 'top') + guides(size=FALSE) + xlab("Mean infection date") + ylab("Cumulative prevalence (%)") + geom_text(aes(label=abbr), size = 2)  #+ guides(size=guide_legend(title= "Population size")) 

q2 <- ggplot(tmp[order(-Prevalence)], aes(y = Ascertainment, x = Mean_date))+ stat_smooth(method = 'lm', colour = 'black', size = 0.25, fill = 'gray', alpha = 0.25) + geom_point(aes(size=pop_2015),shape=21,colour='gray',fill='lightblue') + theme_bw()  + theme(legend.position = 'top')  + guides(size=FALSE)+ xlab("Mean infection date") + ylab("Total % ascertained") + geom_text(aes(label=abbr), size = 2) #+ guides(size=guide_legend(title= "Population size")) 

q3 <- ggplot(tmp[order(-Ascertainment)], aes(x = Ascertainment, y= Prevalence))+ stat_smooth(method = 'lm', colour = 'black', size = 0.25, fill = 'gray', alpha = 0.25) + geom_point(aes(size=pop_2015),shape=21,colour='gray',fill='lightblue') + theme_bw() + theme(legend.position = 'top') + xlab("Total % ascertained") + ylab("Cumulative prevalence (%)") + guides(size=FALSE) + geom_text(aes(label=abbr), size = 2) #+ guides(size=guide_legend(title= "Population size")) 

q4 <- ggplot(tmp[order(-Mean_Ascertainment)], aes(y = Mean_Ascertainment, x = Prevalence))+ stat_smooth(method = 'lm', colour = 'black', size = 0.25, fill = 'gray', alpha = 0.25) + geom_point(aes(size=pop_2015),shape=21,colour='gray',fill='lightblue') + theme_bw() + theme(legend.position = 'top') + ylab("Mean daily % ascertained") + xlab("Cumulative prevalence (%)") + guides(size=FALSE) + geom_text(aes(label=abbr), size = 2) #+ guides(size=guide_legend(title= "Population size")) 

q5 <-  ggplot(tmp[order(-Mean_Ascertainment)], aes(y = Prevalence, x = Confirmed))+ stat_smooth(method = 'lm', colour = 'black', size = 0.25, fill = 'gray', alpha = 0.25) + geom_point(aes(size=pop_2015),shape=21,colour='gray',fill='lightblue') + theme_bw() + theme(legend.position = 'top') + xlab("Cumulative confirmed (%)") + ylab("Cumulative prevalence (%)") + guides(size=FALSE) + geom_text(aes(label=abbr), size = 2) #+ guides(size=guide_legend(title= "Population size")) 

p1 <- plot_usmap( labels = TRUE, label_color = "white", data = tmp, values = "Ascertainment", color = "white") + 
  scale_fill_continuous(name = "Total %\nAscertained\n", label = scales::comma, trans = 'reverse') + 
  theme(legend.position = "top", legend.key.height = unit(0.25, "cm"), legend.key.width = unit(1.5, 'cm'))
  
p2 <- plot_usmap( labels = TRUE, label_color = "white",data = tmp, values = "Prevalence", color = "white") + 
  scale_fill_continuous(name = "Cumulative\nPrevalence (%)\n", label = scales::comma, trans = 'reverse') + 
  theme(legend.position = "top", legend.key.height = unit(0.25, "cm"), legend.key.width = unit(1.5, 'cm'))

p1$layers[[2]]$aes_params$size <- 2.5
p2$layers[[2]]$aes_params$size <- 2.5

ggp_1 <- grid.arrange(p1 + ggtitle("A"), p2 + ggtitle("B"), nrow = 1)
ggp_2 <- grid.arrange(q3 + ggtitle("C"),  q1+ ggtitle("D"),  q2+ ggtitle("E"), q5 + ggtitle("F"),  nrow=1)

ggp_f <- grid.arrange(ggp_1, ggp_2, nrow = 2, heights = c(1.5,1))

ggsave("figure_6/figure_6.png", height = 7, width = 10, ggp_f)
ggsave("figure_6/figure_6.pdf", height = 7, width = 10, ggp_f)

