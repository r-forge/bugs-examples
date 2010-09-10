mkModel <- function(name, parameters, nIter=20000, nBurnin=5000, nThin=1, nChain=1)
{
    return(list("name"=name,
                "parameters"=parameters,
                "nIter"=nIter,
                "nBurnin"=nBurnin,
                "nThin"=nThin,
                "nChain"=nChain))
}
                
Air <- mkModel("Air", c("X","theta" ), nIter=50000, nBurnin=10000)
Alligators <- mkModel("Alligators", c("b", "beta", "g", "gamma"), nIter=50000, nBurnin=10000)
Asia <- mkModel("Asia", c("bronchitis","either","lung.cancer","smoking", "tuberculosis",  "xray"),
                 nIter=110000, nBurnin=10000, nThin=10)
Beetles <- mkModel("Beetles", c("alpha", "beta", "rhat"))
BiRats <- mkModel("BiRats", c("mu.beta", "sigma"))
Biopsies <- mkModel("Biopsies", c("error[2,]", "error[3,]", "error[4,]","p"))
Blockers <- mkModel("Blockers", c("d", "delta.new", "sigma"))
Bones <- mkModel("Bones", c("theta"))
Camel <- mkModel("Camel", c("Sigma2", "rho", "tau"), nIter=101000, nThin=10)
Cervix <- mkModel("Cervix", c("beta", "beta0C", "gamma1", "gamma2", "phi","q"))
Dogs <- mkModel("Dogs", c("A", "B", "alpha", "beta"))
Dugongs <- mkModel("Dugongs", c("U3","alpha", "beta", "gamma","sigma"))
Dyes <- mkModel("Dyes", c("sigma2.btw","sigma2.with", "theta"), nIter=125000, nThin=10, nBurnin=25000)
Endo <- mkModel("Endo", "beta", nIter=15000, nBurnin=5000)
Epil <- mkModel("Epil", c("alpha0", "alpha.Age", "alpha.Base", "alpha.BT", "alpha.Trt", "alpha.V4","sigma.b","sigma.b1"),
                 nIter=15000, nBurnin=5000)
Equiv <- mkModel("Equiv", c("equiv", "mu", "phi", "pi", "sigma1", "sigma2", "theta"))
Eyes <- mkModel("Eyes", c("P","lambda", "sigma"))
Eyetracking <- mkModel("Eyetracking", c("K", "deviance", "mu[92]"))
Hearts <- mkModel("Hearts", c("alpha", "beta", "delta", "theta"))
HepatitisME <- mkModel("HepatitisME", c("alpha0", "beta0", "gamma", "sigma"), nIter=50000, nBurnin=10000)
Hepatitis <- mkModel("Hepatitis", c("alpha0", "beta0", "gamma", "sigma"))
Hips2 <- mkModel("Hips2", c("BL", "BQ", "C", "mean.BL", "mean.BQ","mean.C"),
                  nIter=50000, nBurnin=10000, nThin=2)
Hips3 <- mkModel("Hips3", c("BL", "BQ", "C", "mean.BL", "mean.BQ", "mean.C", "var.BL", "var.BQ","var.C"),
                  nIter=50000, nThin=2, nBurnin=10000)
Ice <- mkModel("Ice",c("logRR", "sigma"), nIter=101000, nThin=10)
Jama <- mkModel("Jama", c("alpha.desc", "beta.desc"), nIter=15000, nBurnin=5000)
Jaws <- mkModel("Jaws", c("beta0", "beta1", "mu", "Sigma"))
Kidney <- mkModel("Kidney", c("alpha", "beta.sex", "beta.dis", "r", "sigma"), nIter=50000, nBurnin=10000)
Leukfr <- mkModel("Leukfr", c("beta", "sigma"))
Leuk <- mkModel("Leuk", c("beta", "S.placebo", "S.treat"))
Line <- mkModel("Line", c("alpha", "beta", "sigma"))
Lsat <- mkModel("Lsat", c("a", "beta"))
Magnesium <- mkModel("Magnesium", c("OR", "tau"), nIter=5000)
Mice <- mkModel("Mice", c("median", "pos.control", "r", "test.sub", "veh.control"))
OtreesMVN <- mkModel("OtreesMVN", c("mu", "sigma", "sigmaC"), nIter=50000, nBurnin=5000)
Otrees <- mkModel("Otrees", c("mu", "sigma", "sigmaC"), nIter=30000, nBurnin=10000)
Oxford <- mkModel("Oxford", c("alpha", "beta1", "beta2", "sigma"))
Pigs <- mkModel("Pigs", c("i", "p"), nIter=110000, nThin=10, nBurnin=10000)
Pigweights <- mkModel("Pigweights", "Sm")
Pumps <- mkModel("Pumps", c("alpha", "beta", "theta"))
Rats <- mkModel("Rats", c("alpha0", "beta.c", "sigma"))
Salm <- mkModel("Salm", c("alpha", "beta", "gamma", "sigma"))
Schools <- mkModel("Schools", c("beta", "gamma", "phi", "theta"))
Seeds <- mkModel("Seeds", c("alpha0", "alpha1", "alpha12", "alpha2", "sigma"))
StVeit <- mkModel("StVeit", "within", nIter=50000, nBurnin=10000)
Stacks <- mkModel("Stacks", c("b", "b0", "outlier[21]", "outlier[3]", "outlier[4]", "sigma"))
Stagnantc <- mkModel("Stagnantc", c("alpha", "beta", "sigma", "x.change"))
Surgical <- mkModel("Surgical", "p")
Surgicalrand <- mkModel("Surgicalrand", c("p", "pop.mean", "sigma"))
tdf <- mkModel("t-df", "d")
Scotland <- mkModel("Scotland", c("RR","alpha0","alpha1","sigma"), nIter=50000, nBurnin=10000)
LHA <- mkModel("LHA", c("RR","alpha","beta","sigma.b","sigma.h"), nIter=100000, nThin=4, nBurnin=20000)
Scotland1 <- mkModel("Scotland1", c("RR","alpha","sigma"))
Elevation <- mkModel("Elevation", c("height.pred","height.pred.multi"))
Forest <- mkModel("Forest", c("theta0","theta1","theta2","spatial.effect"), nIter=20000, nBurnin=10000)
Huddersfield <- mkModel("Huddersfield", c("PC.base","PC.latent","PC.no2","rate.base","rate.latent","rate.no2"),
                         nIter=60000, nBurnin=30000)
###MVCAR <- mkModel("MVCAR", c("alpha","corr","sigma"), nIter=51000, nBurnin=1000)
Shared <- mkModel("Shared", c("RR.ratio","delta","frac.shared","var.shared","var.specific"),
                   nIter=100000, nBurnin=30000)
Pollution <- mkModel("Pollution", c("beta","mu","sigma","sigma.err"))

###PROBLEMS with source:
###
###Alligators misspelled "Aligators"
###nBurnin[modelNames=="Otrees"] = 10000 #Repeated
###nIter[modelNames=="Otrees"] = 50000 #Two values given









