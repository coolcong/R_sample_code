path <- 'report/'
#day <- format(Sys.time(),"%m%d-%H%M%S")
day <- format(Sys.time(),"%m%d")
## reporting stuff

require(ggplot2) || install.packages("ggplot2",dependencies=T); require(ggplot2)

# g <- ggplot(dall, aes(x=testcontrol,y=ctr) )
# g + geom_point() + geom_line( ) + facet_grid(facets=concept~size)
# 
# g + geom_boxplot() + facet_grid(facets=concept~size)            
# 
# g + geom_boxplot() + facet_grid(facets=concept~retargeting)            
# 
# gd <- ggplot(dall, aes(x=ctr,..density..) )
# gd + geom_density(aes(colour=concept)) + facet_grid(facets=testcontrol~size)

require(sqldf)
require(plyr)
summary_placementbyconceptbytestcontrol <- 
  sqldf('select placeid, sizeandconcept, concept, size, testcontrol,
        sum(impressions) as impressions, sum(clicks) as clicks, sum(conversions) as conversions 
        from dall group by placeid, concept, size, testcontrol')
summary_placementbyconceptbytestcontrol$ctr <- summary_placementbyconceptbytestcontrol$clicks/summary_placementbyconceptbytestcontrol$impressions
summary_placementbyconceptbytestcontrol$cvr <- summary_placementbyconceptbytestcontrol$conversions/summary_placementbyconceptbytestcontrol$impressions


summary_conceptbysizebytestcontrol <- 
  sqldf('select sizeandconcept, concept, size, testcontrol,
        sum(impressions) as impressions, 
        sum(clicks) as clicks,
        sum(conversions) as conversions
        from dall group by concept, size, testcontrol')
summary_conceptbysizebytestcontrol$ctr <- summary_conceptbysizebytestcontrol$clicks/summary_conceptbysizebytestcontrol$impressions
summary_conceptbysizebytestcontrol$cvr <- summary_conceptbysizebytestcontrol$conversions/summary_conceptbysizebytestcontrol$impressions
summary_conceptbysizebytestcontrol$ad.size <- summary_conceptbysizebytestcontrol$size
summary_conceptbysizebytestcontrol

# agg <- ggplot(summary_conceptbysizebytestcontrol, aes(x=testcontrol,y=cvr)  )
# agg + geom_point() + facet_grid(facets=concept~ad.size)
# 
# ggplot(summary_conceptbysizebytestcontrol, aes(x=ctr,y=cvr,colour=concept)) +
#   facet_grid(facets=testcontrol~ad.size) + 
#   geom_text(aes(label = concept, size=.5))


summary_conceptbytestcontrol <- 
  sqldf('select concept, testcontrol,
        sum(impressions) as impressions, 
        sum(clicks) as clicks,
        sum(conversions) as conversions
        from dall group by concept, testcontrol')
summary_conceptbytestcontrol$ctr <- summary_conceptbytestcontrol$clicks/summary_conceptbytestcontrol$impressions
summary_conceptbytestcontrol$cvr <- summary_conceptbytestcontrol$conversions/summary_conceptbytestcontrol$impressions
summary_conceptbytestcontrol

summary_conceptpooled <- 
  sqldf('select concept,
        sum(impressions) as impressions, 
        sum(clicks) as clicks,
        sum(conversions) as conversions
        from d group by concept')
summary_conceptpooled$ctr <- summary_conceptpooled$clicks/summary_conceptpooled$impressions
summary_conceptpooled$cvr <- summary_conceptpooled$conversions/summary_conceptpooled$impressions
summary_conceptpooled

summary_TestVsControl <- 
  sqldf('select testcontrol,
        sum(impressions) as impressions, 
        sum(clicks) as clicks,
        sum(conversions) as conversions
        from dall group by testcontrol')
summary_TestVsControl$ctr <- summary_TestVsControl$clicks/summary_TestVsControl$impressions
summary_TestVsControl$cvr <- summary_TestVsControl$conversions/summary_TestVsControl$impressions
summary_TestVsControl 

# ggplot( d.long, aes(x=placeid_concept, y=ranef.coef.samp) ) +
#   geom_boxplot(aes(colour=concept))
d.samp <- d.long[sample(1:nrow(d.long), 30000,replace=FALSE),] 

pdf(file=paste(path,"placement_concept_ranef_boxplot_",day,".pdf",sep=""),h=22,w=17,)
d.samp$placeid_concept_rev <- factor(d.samp$placeid_concept, levels = rev(levels(d.samp$placeid_concept)) ) 
p <- ggplot( d.samp, aes(x=placeid_concept_rev, y=ranef.coef.samp) ) +
  geom_boxplot(aes(fill=concept,colour=concept)) + 
  coord_flip() +
  theme(axis.text.x=element_text(size=.5)) +
  theme(axis.text.y=element_text(size=.5))    
print(p)
dev.off()
rm(p)
gc()
gc()

pdf(file=paste(path,"publisher_ranef_boxplot_",day,".pdf",sep=""),h=22,w=17,)
d.samp$placeid_concept_rev <- factor(d.samp$placeid_concept, levels = rev(levels(d.samp$placeid_concept)) ) 
p <- ggplot( d.samp, aes(x=site, y=ranef.coef.samp) ) +
  geom_boxplot(aes(fill=concept,colour=concept)) + 
  coord_flip()   
print(p)
dev.off()
rm(p)
gc()
gc()

pdf(file=paste(path,"placement_concept_boxplot_",day,".pdf",sep=""),h=22,w=17)
p <- ggplot( d.samp, aes(x=placeid_concept_rev, y=linpred.samp) ) +
  geom_boxplot(aes(fill=concept,colour=concept)) + 
  coord_flip()
#  opts(axis.text.x=theme_text(size=.5)) +
#  opts(axis.text.y=theme_text(size=.5))    
print(p)
dev.off()

rm(p)
gc()
gc()

pdf(file=paste(path,"publisher_concept_boxplot_",day,".pdf",sep=""),h=22,w=17)
p <- ggplot( d.samp, aes(x=site, y=linpred.samp) ) +
  geom_boxplot(aes(fill=concept,colour=concept)) + 
  coord_flip()
#  opts(axis.text.x=theme_text(size=.5)) +
#  opts(axis.text.y=theme_text(size=.5))    
print(p)
dev.off()

rm(p)
gc()
gc()
pdf(file=paste(path,"size_concept_densities_",day,".pdf",sep=""),h=11,w=8.5, onefile=T)

p <- ggplot( d.samp, aes(x=linpred.samp, ..density..) ) +
  geom_density(aes(colour=concept)) +
  facet_grid(facets=ad_size~.) 
print(p)

dev.off()
rm(p)
gc();gc()


summary_placementbyconceptbytestcontrol$allocimpvol[seq(2,nrow(d)*2,by=2)] <- d$allocimpvol
summary_placementbyconceptbytestcontrol$allocimpvol[seq(1,nrow(d)*2,by=2)] <- 0

summary_placementbyconceptbytestcontrol$outallocprob[seq(2,nrow(d)*2,by=2)] <- d$outallocprob 
summary_placementbyconceptbytestcontrol$outallocprob[seq(1,nrow(d)*2,by=2)] <- 0

size <- rep(NA, n.obs)
size[  grep( "160x600", placement ) ] <- "160x600"
size[  grep( "300x250", placement ) ] <- "300x250"
size[  grep( "728x90", placement ) ] <- "728x90"
size <- factor(size)

outallocprobConceptbySize.12 <- 
  sqldf('select sizeandconcept,size,concept,
        sum(impressions) as impressions, 
        sum(clicks) as clicks,
        sum(conversions) as conversions,
        sum(allocimpvol) as allocimpvol,
        avg(outallocprob) as outallocprob_avg from d group by sizeandconcept')
outallocprobConceptbySize.12$ctr <- outallocprobConceptbySize.12$clicks/outallocprobConceptbySize.12$impressions
outallocprobConceptbySize.12$cvr <- outallocprobConceptbySize.12$conversions/outallocprobConceptbySize.12$impressions
outallocprobConceptbySize.12$allocprob <- rep(0,9)
for (si in levels(size)){
  curobs <- which( outallocprobConceptbySize.12$size == si )
  outallocprobConceptbySize.12$allocprob[curobs] <- outallocprobConceptbySize.12$allocimpvol[curobs]/sum(outallocprobConceptbySize.12$allocimpvol[curobs])
}
outallocprobConceptbySize.12

outallocprobbyConcept <- 
  sqldf('select concept,
        sum(impressions) as impressions, 
        sum(clicks) as clicks,
        sum(conversions) as conversions,
        sum(allocimpvol) as allocimpvol,
        avg(outallocprob) as outallocprob_avg from d group by concept')
outallocprobbyConcept$ctr <- outallocprobbyConcept$clicks/outallocprobbyConcept$impressions
outallocprobbyConcept$cvr <- outallocprobbyConcept$conversions/outallocprobbyConcept$impressions
outallocprobbyConcept$allocprob <- outallocprobbyConcept$allocimpvol/sum(outallocprobbyConcept$allocimpvol)
outallocprobbyConcept

outallocprobConceptbySize.wcontrol <- round(outallocprobConceptbySize.12$allocprob*82,0)
outallocprobConceptbySize.wcontrol[3] <- 82-sum(outallocprobConceptbySize.wcontrol[c(1,2)])
outallocprobConceptbySize.wcontrol[6] <- 82-sum(outallocprobConceptbySize.wcontrol[c(4,5)])
outallocprobConceptbySize.wcontrol[9] <- 82-sum(outallocprobConceptbySize.wcontrol[c(7,8)])
outallocprobConceptbySize.wcontrol

rotationweights.PlacementByConcept <- data.frame(matrix(outallocprob,ncol=3, byrow=T))
names(rotationweights.PlacementByConcept) <- levels(d$concept)
rotationweights.PlacementByConcept$Placement <- levels(d$placement)
allocimpvol <- data.frame(matrix(d$allocimpvol,ncol=3,byrow=T))                                                
rotationweights.PlacementByConcept$impvol_CM <- allocimpvol[,1]
rotationweights.PlacementByConcept$impvol_IV <- allocimpvol[,2]
rotationweights.PlacementByConcept$impvol_OS <- allocimpvol[,3]


rotationweights.PlacementByConcept

outallocprobbyConceptPublisher <- 
  sqldf('select concept,site,
        sum(impressions) as impressions, 
        sum(clicks) as clicks,
        sum(conversions) as conversions,
        sum(allocimpvol) as allocimpvol,
        avg(outallocprob) as outallocprob_avg from d group by site,concept order by site,concept')
outallocprobbyConceptPublisher <- outallocprobbyConceptPublisher[order(outallocprobbyConceptPublisher$site),]
outallocprobbyConceptPublisher$ctr <- outallocprobbyConceptPublisher$clicks/outallocprobbyConceptPublisher$impressions
outallocprobbyConceptPublisher$cvr <- outallocprobbyConceptPublisher$conversions/outallocprobbyConceptPublisher$impressions

temp <- ddply(outallocprobbyConceptPublisher,.(site),summarise, totalSite=sum(allocimpvol))
temp <- temp[order(temp$site),]
outallocprobbyConceptPublisher$totalSite <- temp[rep(1:NROW(temp),each=3),'totalSite']
outallocprobbyConceptPublisher$allocprob <- as.integer(82*outallocprobbyConceptPublisher$allocimpvol/outallocprobbyConceptPublisher$totalSite + 0.5 + 6)
outallocprobbyConceptPublisher


options(width = 160)

write.csv(rotationweights.PlacementByConcept, file=paste(path,"rotationweights.PlacementByConcept_",day,".csv",sep="") )


sink(file=paste(path,"CHK-summaries-NonRetargeting-",day,".txt",sep="") )
cat("------------ Summaries (NonRetargeting) -----------\n")

cat("------------ Overall Test vs. Control -----------\n")
print(summary_TestVsControl)
cat("------------ Concept Overall Pooled -----------\n")
print(summary_conceptpooled)
cat("------------ Concept by Test vs. Control -----------\n")
print(summary_conceptbytestcontrol)
cat("------------ Size-and-Concept by Test vs. Control -----------\n")
print(summary_conceptbysizebytestcontrol)

cat("------------ Size-and-Concept Performance (Test Group) and Rotation Weights -----------\n")
print(outallocprobConceptbySize.12)
cat("------------ Concept Performance (Test Group) and Rotation Weights -----------\n")
print(outallocprobbyConcept)
cat("------------ Rotation Weights by Publisher and Size -----------\n")
print(outallocprobbyConceptPublisher)
cat("------------ Model Output: heterogeneity -----------\n")
print(summary(me))
cat("------------ Model Output: homogeneity -----------\n")

sink()
