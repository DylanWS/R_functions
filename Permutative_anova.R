# Created March 2016 by Schneider.DW@Gmail.com
# Currently a very rough version.


# Test for significant difference in means and variance.
signif <- array(as.numeric(NA),dim=c(dim(independents1)[2],3),dimnames=list(names(independents1),c("Interaction","Method","Independent")))

invisible(sapply(1:dim(independents1)[2], function(i)
{
	if(length(unique(independents1[,i]))>1)
	{
		# Create stacked data or long form
		{
			independent= c(independents1[,i],independents1[,i])
			method=	c(rep(names(dependents1)[1],length(dependents1[,1])),rep(names(dependents1)[2],length(dependents1[,2])))
			dependents = c(dependents1[,1],dependents1[,2])
		}

		# Calculate interaction p_value for actual data.
		p_stat_interaction <- summary(aov(dependents~method*independent))[[1]][3,"Pr(>F)"]
	
		dp_stat_interaction <- numeric()
		for(j in 1:1999)
		{
			dependents_scrambled <- sample(t(dependents1))
			dp_stat_interaction[j] <- summary(aov(dependents_scrambled~method*independent))[[1]][3,"Pr(>F)"]
		}
	

		signif[i,1] <<- mean(dp_stat_interaction <= p_stat_interaction)
	
		# If not significant then run additive model to check for main effects
		if(signif[i,1]>0.05)
		{
			p_stat_main_effects <- summary(aov(dependents~method+independent))[[1]][1:2,"Pr(>F)"]
		
		
			dp_stat_main_effects <- NULL
			for(j in 1:1999)
			{
				dependents_scrambled <- sample(t(dependents1))
				dp_stat_main_effects <- rbind(dp_stat_main_effects, summary(aov(dependents_scrambled~method+independent))[[1]][1:2,"Pr(>F)"])
			}
	
			signif[i,2:3] <<- c(mean(dp_stat_main_effects[,1] <= p_stat_main_effects[1]),mean(dp_stat_main_effects[,2] <= p_stat_main_effects[2]))
		
		}
	}
	
	cat("\r\r Significance determined for", i, "of", dim(independents1)[2], "variables. \r") # ;   flush.console()

}))
