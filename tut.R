mydata <-read.csv('tutdata.csv')
mydata

sample_n(data,3)


sample_frac(mydata,0.1)

x1 = distinct(mydata)

x2 = distinct(mydata, Index, .keep_all= TRUE)

x2 = distinct(mydata, Index, Y2010, .keep_all= TRUE)

mydata2 = select(mydata, Index, State:Y2008)

mydata = select(mydata, c(-Index, -State))
mydata3 = select(mydata, starts_with("Y"))

mydata3 = select(mydata, starts_with("Y"))


mydata33 = select(mydata, -starts_with("Y"))





mydata4 = select(mydata, contains("I"))
# mydata4


mydata5 = select(mydata, State, everything())
mydata5


mydata6 = rename(mydata, Index=Index)


mydata7 = filter(mydata, Index == "A")

mydata7 = filter(mydata6, Index %in% c("A", "C"))

mydata8 = filter(mydata6, Index %in% c("A", "C") & Y2002 >= 1300000 )


mydata10 = filter(mydata6, !Index %in% c("A", "C"))


mydata10 = filter(mydata6, grepl("Ar", State))
mydata10

summarise(mydata, Y2015_mean = mean(Y2015), Y2015_med=median(Y2015))

summarise_at(mydata, vars(Y2005, Y2006), c(mean=mean,median=median))
summarise_at(mydata, vars(Y2005, Y2006), list(n=~n(), mean=mean, median=median))

summarise_at(mydata, vars(Y2005, Y2006), ~length(unique(.)))
summarise_at(mydata, vars(Y2005, Y2006), list(~n(), ~mean(.), ~median(.)))


summarise_at(mydata, vars(Y2011, Y2012),list(mean, median), na.rm = TRUE)


summarise_at(mydata, vars(Y2011, Y2012),
             list(~n(), missing = ~sum(is.na(.)), ~mean(., na.rm = TRUE), ~median(.,na.rm = TRUE)))


set.seed(222)
mydata <- data.frame(X1=sample(1:100,100), X2=runif(100))
summarise_at(mydata,vars(X1,X2), function(x) var(x - mean(x)))

summarise_at(mydata,vars(X1,X2), ~ var(. - mean(.)))

summarise_if(mydata, is.numeric, ~ var(. - mean(.)))


summarise_all(mydata["Index"], list(~nlevels(.), nmiss=~sum(is.na(.))))


arrange(mydata, Index, Y2011)

arrange(mydata, desc(Index), Y2011)


dt = mydata %>% select(Index, State) %>% sample_n(10)
dt

# t = mydata %>% group_by(Index) %>%
#   summarise_at(vars(Y2011:Y2015), funs(n(), mean(., na.rm = TRUE)))


t = mydata %>% group_by(Index) %>%
  summarise_at(vars(Y2011:Y2015), list(~n(), ~mean(., na.rm = TRUE)))
t

t = mydata %>% filter(Index %in% c("A", "C","I")) %>% group_by(Index) %>%
  do(head( . , 2))
t


t = mydata %>% select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  
  filter(min_rank(desc(Y2015)) == 3)

t


t = mydata %>%
  group_by(Index)%>%
  summarise(Mean_2014 = mean(Y2014, na.rm=TRUE),
            Mean_2015 = mean(Y2015, na.rm=TRUE)) %>%
  arrange(desc(Mean_2015))

t
mydata1 = mutate(mydata, change=Y2015/Y2014)
mydata1






# mydata11 = mutate_all(mydata, list(new = ~. * 1000))





mydata12 = mutate_at(mydata, vars(Y2008:Y2010), list(Rank=~min_rank(.)))
mydata12
