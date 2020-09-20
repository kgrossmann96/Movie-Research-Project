movies=read.csv(choose.files(),header = T)
#file name is topmovies

movie_edit=data.frame(movies)

for(i in 1:400){
  if(movie_edit$Gross[i]==0 ||is.na(movie_edit$Gross[i])){
    movie_edit$Gross[i]=0.000001
  }
  movie_edit$ROI[i]=movie_edit$Gross[i]/movie_edit$Budget[i]
}

attach(movie_edit)

summary(cbind(Runtime,Budget,Gross,ROI,Stars,Awards,Num_Rating))
hist(Budget)
hist(Gross)
hist(ROI)

lnBudget=log(Budget)
lnGross=log(Gross)
lnROI=log(ROI)

scatter=data.frame(Runtime,Budget,Gross,ROI,Stars,Awards,Num_Rating,Rating)
pairs(scatter, upper.panel = NULL)

top_hundred=subset(movie_edit,id<101)
bottom_hundred=subset(movie_edit, id>100 & id <201)
noise=subset(movie_edit,id>200)

m0=lm(Rating~Runtime+Budget+Gross+ROI+Stars+Awards+Num_Rating+factor(Sequel)+factor(Book))
summary(m0)
# moderate R^2 values, low s, technically significant as a whole; base model

m1a=lm(Rating~Runtime+lnBudget+Gross+ROI+Stars+Awards+Num_Rating+factor(Sequel)+factor(Book))
summary(m1a)
#higher R^2 values, lower s; add one ln term-budget
m1b=lm(Rating~Runtime+Budget+lnGross+ROI+Stars+Awards+Num_Rating+factor(Sequel)+factor(Book))
summary(m1b)
#higher s than m3, lower R^2s; one ln term- gross

m1c=lm(Rating~Runtime+Budget+Gross+lnROI+Stars+Awards+Num_Rating+factor(Sequel)+factor(Book))
summary(m1c)
#higher s than m3, lower R^2s; one ln term- ROI

m2a=lm(Rating~Runtime+lnBudget+lnGross+ROI+Stars+Awards+Num_Rating+factor(Sequel)+factor(Book))
summary(m2a)
#lower s, better R^2 values;add two ln terms-budget and gross; of first 4, the best model

m2b=lm(Rating~Runtime+Budget+lnGross+lnROI+Stars+Awards+Num_Rating+factor(Sequel)+factor(Book))
summary(m2b)
# compared to m3 slightly higher s, near identical but smaller R^2s;add two ln terms-ROI and gross

m2c=lm(Rating~Runtime+lnBudget+Gross+lnROI+Stars+Awards+Num_Rating+factor(Sequel)+factor(Book))
summary(m2c)
#higher s than m5, lower R^2s than m5; add two ln terms-ROI and budget
m3=lm(Rating~Runtime+lnBudget+lnGross+lnROI+Stars+Awards+Num_Rating+factor(Sequel)+factor(Book))
summary(m3)
#slightly higher s, slightly lower R^2;all three ln terms; overall, worse than previous model

best_model=m2a
summary(best_model)
#remove awards

best_model1=lm(Rating~Runtime+lnBudget+lnGross+ROI+Stars+Num_Rating+factor(Sequel)+factor(Book))
summary(best_model1)
#remove stars

best_model2=lm(Rating~Runtime+lnBudget+lnGross+ROI+Num_Rating+factor(Sequel)+factor(Book))
summary(best_model2)
#all variables significant at alpha=0.05 level

check=lm(Rating~Genre+Runtime+lnBudget+lnGross+ROI+Num_Rating+factor(Sequel)+factor(Book))

anova(best_model2,check)
#p-value>0.05, genre is not a significant predictor
plot(residuals(best_model2) ~ fitted.values(best_model2), main="Residuals vs. Fitted Values")