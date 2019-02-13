airq = airquality
ind = sample(nrow(airq), 10)
airq$Wind[ind] = NA
airq$Wind = cut(airq$Wind, c(0,8,16,24))
summary(airq)

impute(airq, classes = list(integer = imputeMean(), factor = imputeMode()))

airq2 = mutate_if(airq, is.factor, as.character)
impute(airq2, class = list(integer = imputeLearner('regr.rpart'), character = imputeLearner('classif.rpart')))

imp = impute(airq2, 
             class = list(integer = imputeMean(), 
                          character = imputeLearner('classif.gbm')))
d = data %>% mutate_if(is.character, as.factor) %>% as.data.frame
glimpse(d)

cols = list(SaleType = imputeLearner('classif.rpart'),
            Utilities = imputeLearner('classif.rpart'),
            Functional = imputeLearner('classif.rpart'),
            MSZoning = imputeLearner('classif.rpart'),
            MasVnrType = imputeLearner('classif.rpart'),
            GarageYrBlt = imputeLearner('regr.gbm'),
            LotFrontage = imputeLearner('regr.gbm')) 


imp = impute(d %>% select(-SalePrice), cols = cols)
imp$data %>% is.na() %>% colSums() %>% sort()

cols_regr = list(GarageYrBlt = imputeLearner('regr.gbm'),
            LotFrontage = imputeLearner('regr.gbm'))
impute(imp$data, cols = cols_regr)
