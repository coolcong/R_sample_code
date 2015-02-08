set.seed(100)

setwd("Y:/MAB/Phase2Test/Checking/NonReTargeting")
rawd_all_unpooled <- read.csv( "Y:/MAB/Phase2Test/Checking/0RawData/Generated/Chk_Combine_Upto_20141109.csv",header=T, stringsAsFactors=FALSE)


# remove . from column names
cnames <- colnames(rawd_all_unpooled)
colnames(rawd_all_unpooled) <- gsub('[.]','',cnames)
# group rows

require(sqldf) || install.packages("sqldf",dependencies=T); require(sqldf)

rawd.all <- sqldf('select
                  Campaign
                  ,SiteDFA as Site
                  ,Placement
                  ,Ad
                  ,Creative
                  ,sum(Impressions) as Impressions
                  ,sum(Clicks) as Clicks
                  ,Clicks/Impressions as ClickRate
                  ,sum(EOAccountsFloodlightNewCustomerEOAccountsOpenedClickthroughConversions) as EOClickConversion
                  ,sum(EOAccountsFloodlightNewCustomerEOAccountsOpenedViewthroughConversions)  as EOViewConversion
                  ,sum(MobileFloodlightMobileEOAccountOpenedClickthroughConversions) as MobileClickConversion        
                  ,sum(MobileFloodlightMobileEOAccountOpenedViewthroughConversions)  as MobileViewConversion
                  from rawd_all_unpooled group by 1,2,3,4,5 ')


# subset on creatives that have to be optimised
CT.which <- grep("CT2:",rawd.all$Ad)

# remove placements that are not test and ones with less than 6 combinations
# 3 creatives X2 for test/cntrl
include.which <- (1:nrow(rawd.all) %in% CT.which) & 
  !(rawd.all$Placement %in% names( which(table(factor(rawd.all$Placement)) < 6 ) ) )

rawd <- rawd.all[include.which, ]
rawd <- droplevels(rawd)

#########special handle ###########

## Take out the placements on Turn.com which is not in our media plan

rm.ind1 =  grep( "Turn", rawd$Site)
rawd = rawd[-rm.ind1,]


 change.ind1 = grep("GDN_KCT - AutoPcmt: \"work from home / telecommute\" terms_xx_300x250_xx_dCPM_Test April 2014_xx_noDFAcost_xx" , rawd$Placement)
 rawd[change.ind1,]$Placement = "GDN_KCT - AutoPcmt: \\work from home / telecommute\\ terms_xx_300x250_xx_dCPM_Test April 2014_xx_noDFAcost_xx"
 change.ind2 = grep("GDN_KCT - AutoPcmt: \"work from home / telecommute\" terms_xx_728x90_xx_dCPM_Test April 2014_xx_noDFAcost_xx" , rawd$Placement)
 rawd[change.ind2,]$Placement = "GDN_KCT - AutoPcmt: \\work from home / telecommute\\ terms_xx_728x90_xx_dCPM_Test April 2014_xx_noDFAcost_xx"
 
 
 change.ind3 = grep("LinkedIn_ROS - \"Individual Contributors\"_xx_160x600_2per24_CPM_Test July 2014_xx_xx_xx" , rawd$Placement)
 rawd[change.ind3,]$Placement =  "LinkedIn_ROS - \\Individual Contributors\\_xx_160x600_2per24_CPM_Test July 2014_xx_xx_xx";
 change.ind4 = grep("LinkedIn_ROS - \"Individual Contributors\"_xx_300x250_2per24_CPM_Test July 2014_xx_xx_xx" , rawd$Placement)
 rawd[change.ind4,]$Placement = "LinkedIn_ROS - \\Individual Contributors\\_xx_300x250_2per24_CPM_Test July 2014_xx_xx_xx"

 rm.ind2 = grep("Comcast_Run of Comcast Xfinity - ATF_xx_160x600_3per24_CPM_xx_Guaranteed_xx_xx",rawd$Placement)
 rawd =rawd[-rm.ind2,]
###################################

#############################
### need to remove retargetings (this will be fast way to modify codes)

n.obs1 = dim(rawd)[1]
retargeting <- rep("No",n.obs1)
retargeting[  grep( "etarget", rawd$Placement ) ] <- "Yes"  #retargeting, Retargeting
retargeting <- factor(retargeting)
retarget.ind = which(retargeting== "Yes")

rawd = rawd[-retarget.ind,]
retargeting= retargeting[-retarget.ind]

#############################

n.obs <- dim(rawd)[1]

placement <- factor(rawd$Placement)
placement.names <- levels(placement)
n.placements <- length(placement.names)

# create unique placement ID's
placeid <- placement 
levels(placeid) <- paste("p",100 + 1:n.placements, sep="")
n.placeid <- length(levels(placeid))

# extract placement site irrespective of size - ie drop the size part in placement
placementsite <- factor( sub(pattern="728x90|160x600|300x250", replacement="ALLSIZES", x=placement) )
placementsite <- droplevels(placementsite)
placementsite.names <- levels(placementsite)
n.placementsites <- length(placementsite.names)

# create unique unit id's from palcement site
unitid <- placementsite
levels(unitid) <- paste("j",100 + 1:n.placementsites, sep="")
n.unitid <- length(levels(unitid))

# extract size data from placement
size <- rep(NA, n.obs)
size[  grep( "160x600", placement ) ] <- "160x600"
size[  grep( "300x250", placement ) ] <- "300x250"
size[  grep( "728x90", placement ) ] <- "728x90"
size <- factor(size)

# extract test/control data from Creative
testcontrol <- rep(NA,n.obs)
testcontrol[  grep( "Test", rawd$Creative ) ] <- "Test"
testcontrol[  grep( "Cntrl", rawd$Creative ) ] <- "Control"
testcontrol <- factor(testcontrol)

# extract concept data from Creative
concept <- rep(NA,n.obs)
concept[  grep( "Lookatyougo", rawd$Creative ) ] <- "Lookatyougo"
concept[  grep( "GetAhead", rawd$Creative ) ] <- "GetAhead"
concept[  grep( "invite", rawd$Creative ) ] <- "Invite"
concept <- factor(concept)

# create interaction of size and concept and placeid and concept
sizeandconcept <- factor( paste( size, concept, sep="_") )
placeid_concept <- factor( paste( placeid, concept, sep="_") )




conversions <- rawd[,"EOClickConversion"] +
  rawd[,"EOViewConversion"] +
  rawd[,"MobileClickConversion"] +
  rawd[,"MobileViewConversion"]


clicks <- rawd$Clicks
impressions <- rawd$Impressions
site <- rawd$Site
dall <- data.frame(site, placement, placeid, placementsite, unitid, size, concept,
                   sizeandconcept, placeid_concept, retargeting, testcontrol,
                   impressions, clicks, conversions)
dall$X <-  cbind( 1,
                  model.matrix( ~ size)[,-1],
                  model.matrix( ~ concept)[,-1] 
)       
#dall <- with(dall,dall[order(placeid,concept),])

dall$cvr <- dall$conversions/dall$impressions
dall$ctr <- dall$clicks/dall$impressions 
dall$adsize <- dall$size

dataout <- data.frame(impressions, clicks, conversions,unitid,size,concept,sizeandconcept,retargeting,testcontrol,site,dall$X)
rownames(dataout) <- NULL
names(dataout) <- c("impressions", "clicks", "conversions", 
                    "unitid","size","concept","sizeandconcept","retargeting","testcontrol","site",               
                    "intercept", "size300x250", "size728x90", 
                    "conceptInvite", "conceptLookatyougo")

## reporting stuff end



#d <- dall[ dall$testcontrol=="Test", ]
## consider all the data; since test vs control doesnt mean different treatments
d <- sqldf('select site, placement, placeid, placementsite, unitid, size, concept, sizeandconcept, placeid_concept,
           retargeting, sum(impressions) as impressions, sum(clicks) as clicks, sum(conversions) as conversions,
           X, size as ad_size from dall group by 1,2,3,4,5,6,7,8,9,10,14,15')

d <- with(d,d[order(placeid,concept),])

n.iters <- 10000
d.long <- d[ rep(1:nrow(d), each=n.iters), ]


# count repeated observations in d per unitid
d$unitid.obs <- rep(1,nrow(d))
for (i in 2:nrow(d)){
  if (d$unitid[i]==d$unitid[i-1]){
    d$unitid.obs[i] <- d$unitid.obs[i-1] + 1
  } 
}
d$oneperunit <- 1*(d$unitid.obs==1)

perunitid.obs <- rep(1,n.unitid) 
for (j in 1:n.unitid){
  curidx <- d$unitid == levels(d$unitid)[j]  
  perunitid.obs[j] <- max(d$unitid.obs[curidx])
}
perunitid.obs
table(perunitid.obs)

concept_each <- factor(rep(levels(concept),3))
size_each <- factor(rep(levels(size),each=3))
creative.names <- paste(size_each, concept_each, sep="_")
designmatrix <- cbind(model.matrix( ~ size_each )[,-1], 
                      model.matrix( ~ concept_each)[,-1]
) 
rownames(designmatrix) <- creative.names
designmatrix.interactions <- model.matrix( ~ size_each*concept_each)[,-1] 
rownames(designmatrix.interactions) <- creative.names
creative.names

write.csv(d,file='data/d.csv',row.names=F)
save(d,file='data/d.rdata')

require(lme4) || install.packages("lme4",dependencies=T); require(lme4)
require(arm) || install.packages("arm",dependencies=T); require(arm)


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


# sample linear predictors for whole dataset from whole model
# test for fixed and random effects separately
#d.long$linpred.samp <- d.long$ranef.coef.samp
#d.long$linpred.samp <- d.long$fixef.linpred.samp

d.long$linpred.samp <- d.long$fixef.linpred.samp + d.long$ranef.coef.samp

#p102 <- d.long[d.long$placeid=="p102",c("linpred.samp","concept")]
#densityplot(~linpred.samp,groups=concept,data=p102,auto.key=T,plot.points=F)

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
