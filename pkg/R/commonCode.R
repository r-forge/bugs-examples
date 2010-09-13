makeExample <- function(name, parameters, nSample=20000, nBurnin=5000,
                        nThin=1, nChain=1)
{
    return(list("name"=name,
                "parameters"=parameters,
                "nSample"=nSample,
                "nBurnin"=nBurnin,
                "nThin"=nThin,
                "nChain"=nChain))
}

Air <- makeExample("Air", c("X","theta" ), nSample=50000, nBurnin=10000)
Alligators <- makeExample("Alligators", c("b", "beta", "g", "gamma"), nSample=50000, nBurnin=10000)
Asia <- makeExample("Asia", c("bronchitis","either","lung.cancer","smoking", "tuberculosis",  "xray"),
                 nSample=110000, nBurnin=10000, nThin=10)
Beetles <- makeExample("Beetles", c("alpha", "beta", "rhat"))
BiRats <- makeExample("BiRats", c("mu.beta", "sigma"))
Biopsies <- makeExample("Biopsies", c("error[2,]", "error[3,]", "error[4,]","p"))
Blockers <- makeExample("Blockers", c("d", "delta.new", "sigma"))
Bones <- makeExample("Bones", c("theta"))
Camel <- makeExample("Camel", c("Sigma2", "rho", "tau"), nSample=100000, nThin=10)
Cervix <- makeExample("Cervix", c("beta", "beta0C", "gamma1", "gamma2", "phi","q"))
Dogs <- makeExample("Dogs", c("A", "B", "alpha", "beta"))
Dugongs <- makeExample("Dugongs", c("U3","alpha", "beta", "gamma","sigma"))
Dyes <- makeExample("Dyes", c("sigma2.btw","sigma2.with", "theta"), nSample=125000, nThin=10, nBurnin=25000)
Endo <- makeExample("Endo", "beta", nSample=15000, nBurnin=5000)
Epil <- makeExample("Epil", c("alpha0", "alpha.Age", "alpha.Base", "alpha.BT", "alpha.Trt", "alpha.V4","sigma.b","sigma.b1"),
                 nSample=15000, nBurnin=5000)
Equiv <- makeExample("Equiv", c("equiv", "mu", "phi", "pi", "sigma1", "sigma2", "theta"))
Eyes <- makeExample("Eyes", c("P","lambda", "sigma"))
Eyetracking <- makeExample("Eyetracking", c("K", "deviance", "mu[92]"))
Hearts <- makeExample("Hearts", c("alpha", "beta", "delta", "theta"))
HepatitisME <- makeExample("HepatitisME", c("alpha0", "beta0", "gamma", "sigma"), nSample=50000, nBurnin=10000)
Hepatitis <- makeExample("Hepatitis", c("alpha0", "beta0", "gamma", "sigma"))
Hips2 <- makeExample("Hips2", c("BL", "BQ", "C", "mean.BL", "mean.BQ","mean.C"),
                  nSample=50000, nBurnin=10000, nThin=2)
Hips3 <- makeExample("Hips3", c("BL", "BQ", "C", "mean.BL", "mean.BQ", "mean.C", "var.BL", "var.BQ","var.C"),
                  nSample=50000, nThin=2, nBurnin=10000)
Ice <- makeExample("Ice",c("logRR", "sigma"), nSample=100000, nThin=10)
Jama <- makeExample("Jama", c("alpha.desc", "beta.desc"), nSample=15000, nBurnin=5000)
Jaws <- makeExample("Jaws", c("beta0", "beta1", "mu", "Sigma"))
Kidney <- makeExample("Kidney", c("alpha", "beta.sex", "beta.dis", "r", "sigma"), nSample=50000, nBurnin=10000)
Leukfr <- makeExample("Leukfr", c("beta", "sigma"))
Leuk <- makeExample("Leuk", c("beta", "S.placebo", "S.treat"))
Line <- makeExample("Line", c("alpha", "beta", "sigma"))
Lsat <- makeExample("Lsat", c("a", "beta"))
Magnesium <- makeExample("Magnesium", c("OR", "tau"), nSample=5000)
Mice <- makeExample("Mice", c("median", "pos.control", "r", "test.sub", "veh.control"))
OtreesMVN <- makeExample("OtreesMVN", c("mu", "sigma", "sigmaC"), nSample=50000, nBurnin=5000)
Otrees <- makeExample("Otrees", c("mu", "sigma", "sigmaC"), nSample=30000, nBurnin=10000)
Oxford <- makeExample("Oxford", c("alpha", "beta1", "beta2", "sigma"))
Pigs <- makeExample("Pigs", c("i", "p"), nSample=110000, nThin=10, nBurnin=10000)
Pigweights <- makeExample("Pigweights", "Sm")
Pumps <- makeExample("Pumps", c("alpha", "beta", "theta"))
Rats <- makeExample("Rats", c("alpha0", "beta.c", "sigma"))
Salm <- makeExample("Salm", c("alpha", "beta", "gamma", "sigma"))
Schools <- makeExample("Schools", c("beta", "gamma", "phi", "theta"))
Seeds <- makeExample("Seeds", c("alpha0", "alpha1", "alpha12", "alpha2", "sigma"))
StVeit <- makeExample("StVeit", "within", nSample=50000, nBurnin=10000)
Stacks <- makeExample("Stacks", c("b", "b0", "outlier[21]", "outlier[3]", "outlier[4]", "sigma"))
Stagnantc <- makeExample("Stagnantc", c("alpha", "beta", "sigma", "x.change"))
Surgical <- makeExample("Surgical", "p")
Surgicalrand <- makeExample("Surgicalrand", c("p", "pop.mean", "sigma"))
tdf <- makeExample("t-df", "d")
Scotland <- makeExample("Scotland", c("RR","alpha0","alpha1","sigma"), nSample=50000, nBurnin=10000)
LHA <- makeExample("LHA", c("RR","alpha","beta","sigma.b","sigma.h"), nSample=100000, nThin=4, nBurnin=20000)
Scotland1 <- makeExample("Scotland1", c("RR","alpha","sigma"))
Elevation <- makeExample("Elevation", c("height.pred","height.pred.multi"))
Forest <- makeExample("Forest", c("theta0","theta1","theta2","spatial.effect"), nSample=20000, nBurnin=10000)
Huddersfield <- makeExample("Huddersfield", c("PC.base","PC.latent","PC.no2","rate.base","rate.latent","rate.no2"),
                         nSample=60000, nBurnin=30000)
MVCAR <- makeExample("MVCAR", c("alpha","corr","sigma"), nSample=51000, nBurnin=1000)
Shared <- makeExample("Shared", c("RR.ratio","delta","frac.shared","var.shared","var.specific"),
                   nSample=100000, nBurnin=30000)
Pollution <- makeExample("Pollution", c("beta","mu","sigma","sigma.err"))










