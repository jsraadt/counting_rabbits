#Let's get started with some R Basics
#R is an Object-Oriented Programming Language for Statistical Computing

#Create some objects

#a character object
x='Hello, World'
x
class(x)

#a numeric object
x=2
x
class(x)

#a character vector
x=c('Hello',',','World')
class(x)

#a numeric vector
x=c(1,2,3)

#a function object
x=function(y){return(y*2+1)}
class(x)
x(2)
x(c(1,2,3))

#a matrix
x=matrix(c(1,2,3,4),nrow=2,ncol=2)
x
class(x)

#another version, this one is by row and has dimnames, notice the function can cross lines
x=matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=T,
         dimnames=list(c('row 1','row 2'),c('col 1','col 2')))
x

#another matrix...
x=matrix(c(1,2,3,'Hello',',','World'),ncol=2,
         dimnames=list(c(),c('col 1','col 2')))
x

#problem: the numeric objects in the matrix inherited the character class
#solution: use a data frame

x=data.frame(col1=c(1,2,3),
             col2=c('Hello',',','World'))
x

#Let's work with some real data!

#See the .pdf for more information about the study

#create a data frame
Rabbits_data=data.frame(Transect=c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8),
                        Habitat=c(rep('Native Perennial',4),
                                  rep('Sandsage Prairies',2),
                                  rep('Yucca Grassland',2),
                                  rep('Western Wheatgrass',2),
                                  rep('Crested Wheatgrass',2),
                                  rep('Mowed Grass',2),
                                  rep('Weedy Forbs',2)),
                        Species=c(rep(c('Jackrabbit','Cottontail'),8)),
                        Count=c(3,4,22,3,1,0,9,5,3,5,0,1,1,4,3,3))

#or, import from excel
install.packages('readxl')
require(readxl)
Rabbits_data=read_excel(file.choose())
View(Rabbits_data)

#let's start the analysis

View(Rabbits_data)

#we can do a statistical test of group differences
#however, the DV is a count variable, so a t-test won't work

#count variables follow a Poisson process
#originated by Simeon Poisson (1781-1840)
#studying deaths by accidental Horse kick in Prussian Military
#any count variable is probably best modelled with a Poisson process
#*radioactive decay,   
#*photons hitting a detector,
#*phone calls in a phone center,
#*defective coke bottles,
#*rabbits spotted,...
#*<what is a count variable in your field of study?>

#use a Poisson test, where test-statistic is ratio of group counts
#rather than a t-test, where the test statistic is difference in group means

#so hypothesis is
#Null: ratio = 1.0
#Alternative: ratio != 1.0

poisson.test(c(sum(Rabbits_data$Count[Rabbits_data$Species=='Jackrabbit']),
               sum(Rabbits_data$Count[Rabbits_data$Species=='Cottontail'])))

#the two groups are stat. sig. diff. at alpha=.05

#however, there seems to be an outlier in one transect
boxplot(Count~Species,Rabbits_data)

#moreover, there is variance in the transects
boxplot(Count~Transect,Rabbits_data,xaxt='n')

#thus, the count variable has a nested structure

#you need a mixed effects poisson regression

#get the lme4 package
install.packages('lme4')
require(lme4)

#a great mixed effects regression analysis compares several alterantive models

#random intercept, random slope
my_glme1=glmer(Count~Species+(Species|Transect),data=Rabbits_data,family='poisson')

#fixed slope, random intercept
my_glme2=glmer(Count~Species+(1|Transect),data=Rabbits_data,family='poisson')

#fixed intercept, random slope
my_glme3=glmer(Count~Species+(0+Species|Transect),data=Rabbits_data,family='poisson')

#notice no. 1 and no. 3 had a singular fit, but no.2 did not
#singular fit means the model is too complex
#by process of elimination, it's the random slope that makes the model too complex
#so, the random intercept model is best

#get 95% confidence intervals for the random itnercept model
confint(my_glme2)

#the random intercept is stat. sig.
#the fixed intercept is not stat. sig.
#the fixed slope is stat. sig.

#there is no difference in species after accounting for transect
#there is a variance by transect

#see IDRE for more inforamtion on interpreting a poisson generalized mixed effects regression
browseURL('https://stats.idre.ucla.edu/r/faq/random-coefficient-poisson-models/')
