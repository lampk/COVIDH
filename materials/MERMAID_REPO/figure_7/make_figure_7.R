
require(ggplot2)
require(data.table)
require(gridExtra)
require(Matrix)

#-----------------
# Load data 
#-----------------

## Boostrapped estimates from Rt model with no time trend (policy effects & state-specific intercepts only)
bs_coef_0 <- readRDS("data/US_processed/US_bootstrap_model_0.rds")
## Boostrapped estimates from Rt model with US-wide shared 9-knot B-spline time trend (+ policy effects + state-specific intercepts)
bs_coef_2 <- readRDS("data/US_processed/US_bootstrap_model_2.rds")

## Load US policy data from OxCGRT
C_data <- readRDS("data/US_processed/US_policy_time_series_data.rds")


#-----------------
# Process data 
#-----------------

# Function to make table of coefficients from matrix of bootstrap estimates
get_coef_table <- function(coef_data, alpha_level = 0.05 ){

	nm <- colnames(coef_data)

	var_nm <- gsub("\\).*", "", gsub(".*\\(", "", nm))
	lev_nm <- gsub(".*\\)", "", nm)

	pl_dat <- data.table(
	var = gsub("_", " ", var_nm), lev = lev_nm,
	y = colMeans(coef_data, na.rm=TRUE), 
	y_L = apply(coef_data, 2, quantile, probs = alpha_level/2, na.rm=TRUE),
	y_U = apply(coef_data, 2, quantile, probs = 1 - alpha_level/2, na.rm=TRUE)
	)

	pl_dat$lev <- factor(pl_dat$lev, levels = 4:1, order = TRUE)

	pl_dat

}

# Process BS estimates for policy effect plot (panel B)

pl_data_0 <- get_coef_table(bs_coef_0)
pl_data_0$type <- "No time trend"
pl_data_2 <- get_coef_table(bs_coef_2)
pl_data_2$type <- "9-knot B-spline"

pl_data_c <- rbind(pl_data_0, pl_data_2) 
pl_data_c$Fit <- "Joint"

# Process data for policy summary plot (panel A)

dt_policy <- C_data[,list(
NPI = c("Facial_coverings","Stay_home","Close_public_transport","Gathering_restrictions","Workplace_closing"),
value = c(Facial_coverings,Stay_home,Close_public_transport,Gathering_restrictions,Workplace_closing)),by=list(state,date)]

dt_policy <- dt_policy[,list(n=.N),by=list(NPI,level = factor(pmax(0,value,na.rm=TRUE)),date)]
dt_policy[,freq := n/sum(n),by=list(date,NPI)]


#-----------------
# Make plots
#-----------------

# Make panel B

panel_B <- ggplot(subset(pl_data_c, !is.na(lev)), aes(y = y, ymin = y_L, ymax = y_U, x = lev, colour = factor(type))) + facet_wrap(var ~., scales = 'free',ncol=1) + ylim(-0.65,0.25) + theme_minimal() + geom_hline(yintercept = 0, linetype = 'solid', size = 0.3, colour = 'black') + coord_flip() + geom_point(position=position_dodge(width=0.5),size=0.65) + geom_errorbar(width = 0,size=0.35, position=position_dodge(width=0.5)) + xlab(NULL) + ylab(NULL) + guides(colour=guide_legend(title=NULL)) + scale_colour_brewer(palette = 'Dark2' ) +  theme(
	# panel.grid = element_blank(),
	legend.position = "right",
	#panel.margin=unit(.05, "lines"),
	#panel.border = element_rect(color = "gray", fill = NA, size = 0.1), 
	strip.background = element_blank()
) + scale_x_discrete(breaks = NULL)


# Make panel A

panel_A <- ggplot(dt_policy, aes(y = n, fill = level, x = date)) + facet_wrap(~gsub("_", " ", NPI),ncol = 1,scales='free_x') + geom_col(width=1,colour=NA,size=0.05) + theme_minimal() + coord_cartesian(expand = FALSE) + ylab("No. states") + xlab(NULL) + guides(fill = guide_legend(title = "Policy\nlevel")) + xlab(NULL) + scale_fill_brewer(palette = "Spectral")  + scale_x_date( breaks = as.Date(c(paste0("2020-0",4:9,"-01"),paste0("2020-",10:12,"-01")) ), labels = c("Apr", "", "Jun", "", "Aug", "", "Oct", "", "Dec")) + theme(axis.ticks = element_line()) # + geom_vline(xintercept = NPI_REG_START_DATE, colour = "black", linetype = "dotted")

require(gridExtra)
figure_7 <- grid.arrange(panel_A + ggtitle("A"), panel_B + ggtitle("B"), nrow = 1, widths = c(2,1.75))

ggsave("figure_7/figure_7.png", height = 6, width = 9, figure_7)
ggsave("figure_7/figure_7.pdf", height = 6, width = 9, figure_7)


