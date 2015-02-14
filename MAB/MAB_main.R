require(sqldf) || install.packages("sqldf",dependencies=T); require(sqldf)
require(lme4) || install.packages("lme4",dependencies=T); require(lme4)
require(arm) || install.packages("arm",dependencies=T); require(arm)

#skip the data pre-processing #


#glme1 <- lmer(cbind(conversions,impressions-conversions) ~ (1|unitid) + concept + size, family=binomial(link="logit"), data=d)
#glme2 <- lmer(cbind(conversions,impressions-conversions) ~ (1|unitid) + concept*size, family=binomial(link="logit"), data=d) 
#glme3 <- lmer(cbind(conversions,impressions-conversions) ~ (1+concept|unitid) + size , family=binomial(link="logit"), data=d)
#glme4 <- lmer(cbind(conversions,impressions-conversions) ~ (1+concept+size|unitid) , family=binomial(link="logit"), data=d)
#glme5 <- lmer(cbind(conversions,impressions-conversions) ~ concept+size + (0+concept+size|unitid) , family=binomial(link="logit"), data=d)
#glme6 <- lmer(cbind(conversions,impressions-conversions) ~ concept*size + (0+concept|placeid) , family=binomial(link="logit"), data=d)
#glme7 <- lmer(cbind(conversions,impressions-conversions) ~ concept + (0+concept|placeid), family=binomial(link="logit"), data=d)
# glme8 <- glmer(cbind(conversions,impressions-conversions) ~ size + concept + size:concept + (0+concept|placeid)
#                , family=binomial(link="logit")
#                , nAGQ=1, data=d)

#glme9 <- glmer(cbind(conversions,impressions-conversions) ~ size + concept + size:concept + (1+concept|placeid), family=binomial(link="logit"), data=d)
#glme10 <- lmer(cbind(conversions,impressions-conversions) ~ size*concept + (1|placeid) + (0+concept|placeid), family=binomial(link="logit"), data=d)

#use placeid instead of placement
# optimize for clicks rather than conversions
#glme8 <- lmer(cbind(clicks,impressions-clicks) ~ size + concept + size:concept + (0+concept|placeid), family=binomial(link="logit"), data=d)

# glmer11 <- glmer(cbind(conversions,impressions-conversions) ~ 
#                    size + concept + size:concept + (concept|placeid), family=binomial(link="logit"), nAGQ=0, data=d)
# glmer12 <- glmer(cbind(conversions,impressions-conversions) ~ 
#                    concept + (concept|placeid), family=binomial(link="logit"), nAGQ=0, data=d)
# glmer13 <- glmer(cbind(conversions,impressions-conversions) ~ 
#                    size + concept + (concept|placeid), family=binomial(link="logit"), nAGQ=0, data=d)
# glmer14 <- glmer(cbind(conversions,impressions-conversions) ~ 
#                    size + concept + (1|placeid), family=binomial(link="logit"), nAGQ=0, data=d)
# 
# 
# glmer21 <- glmer(cbind(conversions,impressions-conversions) ~ 
#                    size + concept + size:concept + (concept|site), family=binomial(link="logit"), nAGQ=0, data=d)
# glmer22 <- glmer(cbind(conversions,impressions-conversions) ~ 
#                    concept + (concept|site), family=binomial(link="logit"), nAGQ=0, data=d)
# glmer23 <- glmer(cbind(conversions,impressions-conversions) ~ 
#                    size + concept + (concept|site), family=binomial(link="logit"), nAGQ=0, data=d)
# glmer24 <- glmer(cbind(conversions,impressions-conversions) ~ 
#                    size + concept + (1|site), family=binomial(link="logit"), nAGQ=0, data=d)

glmer20 <- glmer(cbind(conversions,impressions-conversions) ~ 
                   size + concept + (0+concept|site), family=binomial(link="logit"), nAGQ=0, data=d)

# glmer25 <- glmer(cbind(conversions,impressions-conversions) ~ 
#                    size + concept + (0+concept|site) + (1|site), family=binomial(link="logit"), nAGQ=0, data=d)

# pick model to use
me <- glmer20

summary(me)

fixef(me)

vc <- VarCorr(me)
sqrt(diag(vc$site))

#placement-specific point estimates of mean and sd for each concept
ranef(me)$site[c(1,2),]
se.ranef(me)$site[c(1,2),]



ranefmat <- ranef(me)$site  

# create matrix of estimates with nrows = nrows(d)  = 99*4
medf.ranef <- data.frame(placeid = d$placeid,site=d$site)

concept.names <- levels(d$concept)
medf.ranef$concept <- d$concept

medf.ranef$placeid_concept <- paste(medf.ranef$placeid, medf.ranef$concept, sep="_")

medf.ranef <- with(medf.ranef,medf.ranef[order(placeid,concept),])

medf.ranef$mean.ranef <- 0
medf.ranef$se.ranef <- 0

for(s in unique(site)){
  medf.ranef$mean.ranef[medf.ranef$concept==concept.names[1] & medf.ranef$site == s] <- ranef(me)$site[s,1]
  medf.ranef$mean.ranef[medf.ranef$concept==concept.names[2] & medf.ranef$site == s] <- ranef(me)$site[s,2]
  medf.ranef$mean.ranef[medf.ranef$concept==concept.names[3] & medf.ranef$site == s] <- ranef(me)$site[s,3]
  
  medf.ranef$se.ranef[medf.ranef$concept==concept.names[1] & medf.ranef$site == s] <- se.ranef(me)$site[s,1]
  medf.ranef$se.ranef[medf.ranef$concept==concept.names[2] & medf.ranef$site == s] <- se.ranef(me)$site[s,2]
  medf.ranef$se.ranef[medf.ranef$concept==concept.names[3] & medf.ranef$site == s] <- se.ranef(me)$site[s,3]
  
}


medf.ranef$CIlo <- medf.ranef$mean.ranef - 2*medf.ranef$se.ranef
medf.ranef$CIhi <- medf.ranef$mean.ranef + 2*medf.ranef$se.ranef

#simulate ranef 
set.seed(100)
n.iters # predefined
dim(d.long) # predefined

# make df with nrow(d)*n.iters to form ranef.coef.samp
# make df with nrow(d)*n.iters to form ranef.linpred.samp   (with is the total random effects)
# make df with nrow(d)*n.iters with placementid, ranef.linpred.samp

ranef.est.coef <- medf.ranef$mean.ranef
ranef.est.varcov <- diag(medf.ranef$se.ranef^2)
ranef.coef.samp.wide <- mvrnorm(n.iters, ranef.est.coef, ranef.est.varcov)

d.long$ranef.coef.samp <- c(ranef.coef.samp.wide) # append column over column


### fixed effects from model into d --> d.long

fixef(me)
names(fixef(me))

fixef.est.coef <- fixef(me)
fixef.est.varcov <- vcov(me)
medf.fixef <- data.frame( fixef.est.coef )
names(medf.fixef) <- "est.mean"
medf.fixef$est.se <- sqrt(diag( fixef.est.varcov ))
medf.fixef

n.iters # predefined
dim(d.long) # predefined

# reorder design matrix acccording to our simulated coefficients
designmatrix.nointeractions <- model.matrix( ~ size_each+concept_each)
creative.names <- paste(size_each, concept_each, sep="_")
rownames(designmatrix.nointeractions) <- creative.names
desmat <- designmatrix.nointeractions[,c(1:5)] 

fixef.coef.samp <- mvrnorm(n.iters, fixef.est.coef, fixef.est.varcov)

fixef.linpred.samp.wide <- fixef.coef.samp  %*% t(desmat)
colnames(fixef.linpred.samp.wide)

# get dataframe with nrow(d)*n.iters: placeid, size , concept, fixef.linpred.samp 

# merge with dataframe with nrow(d)*n.iters: placeid, 


fixef.linpred.samp <- rep(0, nrow(d)*n.iters)
for (place in levels(d$placeid) ){
  for (conc in levels(d$concept) ){
    curobslong <- which(d.long$placeid==place & d.long$concept==conc)
    sandc <- unique(d.long$sizeandconcept[curobslong])
    fixef.linpred.samp[ curobslong ] <- fixef.linpred.samp.wide[,sandc]
     print(paste(place,conc,min(curobslong),max(curobslong)))
  }
  
}

### no change beyond this

d.long$fixef.linpred.samp <- fixef.linpred.samp

d.long$linpred.samp <- d.long$fixef.linpred.samp + d.long$ranef.coef.samp



d.long$iter <- paste("iter", rep(n.iters*10+ 1:n.iters , nrow(d) ) ,sep="")
d.long$placeid_iter <- paste(d.long$placeid, d.long$iter, sep="_")

d.long[d.long$placeid_iter=="p101_iter100001",c("placeid","concept","iter","placeid_iter", "linpred.samp")] 

zstar.samp <- d.long$linpred.samp
zstar.which.max.perplaceid <- tapply(d.long$linpred.samp, INDEX=d.long$placeid_iter, FUN=which.max)
zstar.which.max.perplaceid.table <- table( zstar.which.max.perplaceid, rep(levels(d$placeid),each=n.iters) )

alloc.probs.table <- zstar.which.max.perplaceid.table/n.iters

outallocprob <- as.numeric( zstar.which.max.perplaceid.table /n.iters)


d <- with(d,d[order(placeid,concept),])


d$outallocprob <- outallocprob 


placeimp <- rep(0,nrow(d))
for (j in 1:nrow(d) ){
  cp <- d$placementsite[j]
  si <- d$size[j]                                                                                     
  curobs <- which( d$placementsite == cp & d$size == si )
  tempimp <- d$impressions[curobs]
  placeimp[curobs] <- sum( tempimp,na.rm=T )
}

d$placeimp <- placeimp
d$allocimpvol <- d$outallocprob * 3e7*placeimp/sum(placeimp) # how many of 10,000,000 tomorrow?

## scale weights to available test % (ie 82% of impressions are test)

outrotationweights <- round( d$outallocprob*82 )
## fix any < 82 issues from rounding off 
for (j in seq(3,nrow(d),by=3) ){
  outrotationweights[j] <- 82 - sum( outrotationweights[c(j-2,j-1)] )
}

# create dataframe of weights - with 18/3=6 for control of each combination
outrotationweights.wcontrol <- rep(0,nrow(d)*2)
outrotationweights.wcontrol[seq(1,nrow(d)*2,by=2)] <- 6
outrotationweights.wcontrol[seq(2,nrow(d)*2,by=2)] <- outrotationweights

d2 <- sqldf('select site, placement, placeid, placementsite, unitid, size, concept, sizeandconcept, placeid_concept,
            retargeting, sum(impressions) as impressions, sum(clicks) as clicks, sum(conversions) as conversions,
            X, adsize, testcontrol from dall group by 1,2,3,4,5,6,7,8,9,10,14,15,16 order by placeid,concept')

outrotationweights.df <- data.frame(Placement = d$placement[rep(1:nrow(d),each=2)], 
                                    Concept = d$concept[rep(1:nrow(d),each=2)], 
                                    RotationWeight = outrotationweights.wcontrol
                                    ,CVR = d2$conversions/d2$impressions
                                    ,Impressions = d2$impressions
                                    ,Conversions = d2$conversions
                                    ,Size = d2$adsize
                                    ,TestControl = rep(c('control','test'),times=nrow(d))
)


source('src/report.r')
source('src/IMF.r')
