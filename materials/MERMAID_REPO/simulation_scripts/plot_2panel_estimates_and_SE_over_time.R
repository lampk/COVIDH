
# Plot mean estimates+bias (panel 1) and SE calibration (panel 2) over time across simulation replicates
make_2panel_plot <- function(data, param_list,  xlab_list, ylab_list, scales = "fixed", ylim = as.numeric(c(NA, NA)), MERMAID_only = FALSE ){

	require(data.table)
	require(ggplot2)
	require(gridExtra)
	require(cowplot)

	CONST <- 10
	COLFUN <- function(x) NA

	brk <- as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01","2020-06-01", "2020-07-01", "2020-08-01","2020-09-01", "2020-10-01","2020-11-01", "2020-12-01", "2021-01-01"))
	lab <- c("Jan", "", "Mar", "",  "May", "",  "Jul",  "", "Sep",  "", "Nov", "", "")
	
	tmp <- subset(data, param == param_list[1])[,list(
	date = rep(as.Date(gsub("^.*:", "", variable)), 2),
	"Truth*" = Truth, 
	"Truth" = Truth, 
	"MERMAID" = Mean, 
	"Reported counts" = subset(data, param == param_list[2])$Mean, 
	"Reconvolution" = subset(data, param == param_list[3])$Mean, 
	SE, StdDev, sim_name
	),]
	
	tmp <- melt(tmp, 
	measure.vars = c("Truth*", "MERMAID", "Reported counts", "Reconvolution"),
	id.vars = c("date", "sim_name", "SE", "StdDev", "Truth")
	)

	if( MERMAID_only ){
		tmp$value[!(tmp$variable %in% c("Truth*", "MERMAID") )] <- NA
		tmp$SE[!(tmp$variable %in% c("Truth*", "MERMAID") )] <- NA
		tmp$StdDev[!(tmp$variable %in% c("Truth*", "MERMAID") )] <- NA
	}

	tmp$SE[tmp$variable != "MERMAID"] <- NA
	tmp$StdDev[tmp$variable != "MERMAID"] <- NA

	require(ggplot2)
	p1 <- ggplot(
	tmp, 
	aes(x = as.Date(date), y = value, ymin = value - CONST*SE, ymax = value + CONST*SE, colour = variable, linetype = variable)
	) + guides( colour = guide_legend(title = NULL), linetype = guide_legend(title = NULL)) + 
	  scale_colour_manual(values = c("green", "black", "magenta", "blue")) + 
	  geom_line(size = 0.35) + 
	  theme_bw() +
	  theme(axis.line = element_line(colour = "black"),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.border = element_blank(),
		panel.background = element_blank()
		) + 
	  ylab(ylab_list[[1]]) + xlab(xlab_list[[1]]) + facet_wrap(~sim_name,scales=scales) + scale_x_date(labels = lab, breaks = brk) + ylim(ylim[1], ylim[2]) + theme(strip.background = element_rect(colour = NA), strip.text.x = element_text(margin = margin(0,0,0,0, "lines")))

	tmp_SE <- rbind(
		subset(tmp, variable == "MERMAID")[,list(sim_name,date, "value" = StdDev, "variable" = "\nTruth / \nEmpirical SE"),],
		subset(tmp, variable == "MERMAID")[,list(sim_name,date, "value" = SE, "variable" = "\nMERMAID\nMean est. SE" ),]
	)
	tmp_SE$variable <- factor(tmp_SE$variable, levels = c("\nTruth / \nEmpirical SE", "\nMERMAID\nMean est. SE"), order = TRUE)

	p2_SE <-ggplot(
		tmp_SE, 
		aes(x = date, y = value, colour = variable, linetype = variable)
	) + guides( colour = guide_legend(title = NULL), linetype = guide_legend(title = NULL)) + 
	  scale_colour_manual(values = c("green", "black")) + 
	  geom_line(size = 0.35) + 
	  theme_bw() +
	  theme(axis.line = element_line(colour = "black"),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.border = element_blank(),
		panel.background = element_blank()
		) + 
	  ylab("Standard error") + xlab(NULL) + facet_wrap(~sim_name,scales=scales) + scale_x_date(labels = lab, breaks = brk) + ylim(0, NA) + 
	  theme(strip.background = element_rect(colour = NA), strip.text.x = element_text(margin = margin(0,0,0,0, "lines")))

	cbn <- grid.arrange(p1 + ggtitle("A"), p2_SE + ggtitle("B"), nrow = 2, heights = c(1,1))

	return(cbn)
}

