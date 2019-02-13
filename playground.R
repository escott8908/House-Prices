set.seed(555666)

ps = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
  makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot")),
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x,
                   requires = quote(kernel == "rbfdot")),
  makeIntegerParam("degree", lower = 2L, upper = 5L,
                   requires = quote(kernel == "polydot"))
)
ctrl = makeTuneControlIrace(maxExperiments = 200L)
rdesc = makeResampleDesc("Holdout")
res1 = tuneParams("classif.ksvm", iris.task, rdesc, par.set = ps, control = ctrl,
                 show.info = FALSE)
print(head(as.data.frame(res$opt.path)))


# part II -----------------------------------------------------------------


base.learners = list(
  makeLearner("classif.ksvm"),
  makeLearner("classif.randomForest")
)
lrn = makeModelMultiplexer(base.learners)

set.seed(555666)

ps = makeModelMultiplexerParamSet(lrn,
                                  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x),
                                  makeIntegerParam("ntree", lower = 1L, upper = 500L)
)
print(ps)

rdesc = makeResampleDesc("CV", iters = 2L)
ctrl = makeTuneControlIrace(maxExperiments = 200L)
res2 = tuneParams(lrn, iris.task, rdesc, par.set = ps, control = ctrl,
                 show.info = FALSE)
print(head(as.data.frame(res$opt.path)))


# my part -----------------------------------------------------------------

minid = data2 %>% select(-SalePrice, -Origin) %>% filter(!is.na(LotFrontage))
rtask = makeRegrTask(data = minid, target = "LotFrontage")

base.learners = list(
  makeLearner("regr.rpart"),
  makeLearner("regr.randomForestSRC", ntree = 100)
)

lrn = makeModelMultiplexer(base.learners)

paramset = makeModelMultiplexerParamSet(lrn,
                                        makeNumericParam("cp", lower = 0, upper = 1),
                                        makeIntegerParam("mtry", lower = 2, upper = 16)
)

rdesc = makeResampleDesc("CV", predict = 'both', iters = 3)
ctrl = makeTuneControlIrace(maxExperiments = 100)

measures = list(
  rmse.test.mean = setAggregation(rmse, test.mean),
  rmse.test.sd = setAggregation(rmse, test.sd),
  rmse.train.mean = setAggregation(rmse, train.mean),
  rmse.train.sd = setAggregation(rmse, train.sd)
)

parallelStartMulticore(6)
res3 = tuneParams(lrn, rtask, rdesc, measures = measures, par.set = paramset, control = ctrl)
parallelStop()


as.data.frame(res$opt.path)
