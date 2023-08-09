
install.packages("gllvm")
library(gllvm)


# before starting run these codes to help you understand the structure of each data: 

library(mvabund)
data(antTraits)
y <- as.matrix(antTraits$abund)
X <- as.matrix(antTraits$env)
TR <- antTraits$traits



# now create data.rel (relative abundance of species), data.env (Environemtal variable), and data.traits (species traits)

# data.env or x needs: environmental variables as head of each column with their numeric value (row are species but we have to remove
# this species column later - the code is provided)

# data.rel or y needs: species as head names of each column with their relative abundance as values 

# data.traits or TR needs: Species names as rows and and their traits as columns 

# Important: data.env and data.rel needs to have the exact same number of rows
# Important 2: data.rel number of column has to be exactly as data.traits rows



# Convert  your species file to a data frame.

df_for_gllvm <- as.data.frame(XXX)

# Convert data to relative abundances (if needed)

data.rel <- df_for_gllvm

data.rel <- t(apply(data.rel, 1, function(x) x/sum(x)))


# Add the environmental data to the data frame (dpends on what env var we want to include)

library (readr)
data.env <- read_tsv("./XXX.tsv", 
                     col_types = cols_only(Samples = col_character(), pH = col_double(),
                                           Conductivity = col_double(), Temp = col_double()))


# Use data.rel to match rowname with  data.env 

data.env <- data.env[match(rownames(data.rel), data.env$Samples), ]


# Read in the trait file
data.trait <- read.table("XXX.tsv", header = TRUE, row.names = 1, sep = "\t")

# Transpose the pathways file
data.trait <- t(data.trait)

# delete the first row if any
data.trait <- as.data.frame(data.trait[-1,])


# Remove the ".asv" suffix from the row names to match the sample names in your data
pathways <- rownames_to_column(pathways, var = "pathway") %>% 
  mutate(pathway = sub("\\.asv", "", pathway)) %>% 
  column_to_rownames(var = "pathway")

# Subset the trait data frame to include only the traits that have at least one non-zero abundance in any sample
data.trait <- data.trait[, apply(data.trait, 2, function(x) any(x != 0))]

# Convert the tibble to a data frame
data.env <- as.data.frame(data.env)

# Set the row names
rownames(data.env) <- data.env$row_id

# Remove the row_id column
data.env$row_id <- NULL




# Match row names of data.rel and traits
rownames(data.rel) <- rownames(data.trait)
rownames(data.env) <- rownames(data.rel)




# Convert data.rel and data.traits to matrices
data.rel <- as.data.frame(data.rel)
data.trait <- as.data.frame(data.trait)
data.env <- as.data.frame(data.env)


# Check the data types of columns in data.env
str(data.env)

# Convert non-numeric columns to numeric if necessary
data.env$pH <- as.numeric(data.env$pH)
data.env$Conductivity <- as.numeric(data.env$Conductivity)
data.env$Temp <- as.numeric(data.env$Temp)


data.env <- data.env[, -1]







# Check for missing values in data.env
any(is.na(data.env))

# Check for missing values in data.rel
any(is.na(data.rel))

# Check if all row names in data.rel are present in traits
all(match(rownames(data.rel), rownames(data.trait)) == 1)

# Check if all row names in traits are present in data.rel
all(match(rownames(data.trait), rownames(data.rel)) == 1)


# Find the row names that are present in both data.rel and pathways
common_rows <- intersect(rownames(data.rel), rownames(data.trait))



# Create a list containing your data frames
my_data <- list(rel = data.rel, env = data.env, traits = data.trait)


# Fit the gllvm model using my_data
fit_gllvm <- gllvm(y = my_data$rel, X = my_data$env, TR = my_data$traits, 
             family = "negative.binomial", multithread = TRUE, cores = 7)





# Using the residual diagnostic plot we can check
# that the chosen NB distribution is suitable for 
# the data at hand. The Dunn-Smyth residuals given
# by the NB model are normally distributed around zero,
# thus supporting the choice. Using argument n.plot we 
# can choose a number of randomly selected species to be
# plotted in order to make any patterns in the residual 
# plots more apparent. This can be useful argument when
# data is high-dimensional.

# Take a look at the data 

meanY <- apply(my_data$rel,2, mean)
varY <- apply(my_data$rel,2, var)
plot(log(meanY),varY, log = "y", main = "Species mean-variance relationship")
# As species variances increases while mean increases, it is a clear indication of the overdispersion in the data.


# In order to study if the effect of environmental variables
# is seen in an unconstrained ordination plot,
# we first consider a GLLVM with two latent variables and no predictors,
# and constructed an ordination plot based on the predicted latent variables.
# For count data with overdispersion we consider here the negative binomial (NB)
# distribution. We also include random effects for sites in order to account
# for the differences in site totals. This can be done by defining the structure
# for random row effects in an argument row.eff and including a data frame 
# with ‘Site’ variable as factor to an argument ‘studyDesign’. 
# Note that in the examples we consider below we don’t necessarily
# need standard errors for the parameters for this specific model 
# so we define sd.errors = FALSE in the function call. When needed,
# standard errors can also be calculated afterwards using function se.


# for me the sites were Ponds, for ZOOP we have to decide

sDesign<-data.frame(Site=data.env$Pond)

# Fit the gllvm model using my_data
fit_gllvm <- gllvm(y = my_data$rel, studyDesign = sDesign,
                   family = "negative.binomial", num.lv = 2, sd.errors = FALSE, row.eff = ~(1|Site))


print(fit_gllvm)



# the rest of the code is from data. once we have the above files we can proceed... 

par(mfrow = c(1, 2))
plot(fit_gllvm, which = 1:2, var.colors = 1, n.plot = 100)

# Define colors according to the values of pH, MeHg and Conductivity
library(grDevices)
ph <- data.env$pH
rbPal <- colorRampPalette(c('mediumspringgreen', 'blue'))
Colorsph <- rbPal(20)[as.numeric(cut(ph, breaks = 20))]
breaks <- seq(min(ph), max(ph), length.out = 30)
MeHg <- data.env$MeHg
ColorsMeHg <- rbPal(20)[as.numeric(cut(MeHg, breaks = 20))]
breaks <- seq(min(MeHg), max(MeHg), length.out = 30)
Cond <- data.env$Conductivity
ColorsCond <- rbPal(20)[as.numeric(cut(Cond, breaks = 20))]
breaks <- seq(min(Cond), max(Cond), length.out = 30)
# Define symbols for different sampling locations:
pchr = NULL
pchr[data.env$Pond == "P125"] = 1
pchr[data.env$Pond == "P15"] = 2
pchr[data.env$Pond == "P118"] = 3
pchr[data.env$Pond == "P02"] = 4
pchr[data.env$Pond == "P35"] = 5
pchr[data.env$Pond == "P124"] = 6
pchr[data.env$Pond == "P109"] = 7
# Ordination plots. Dark color indicates high environmental covariate value.
ordiplot(fit_gllvm, main = "Ordination of sites, color: pH",
         symbols = TRUE, pch = pchr, s.colors = Colorsph)
legend("topleft", legend = c("P125", "P15", "P118", "P02", "P35", "P124", "P109"),
       pch = c(1, 2, 3, 4, 5, 6, 7), bty = "n")


ordiplot(fit_gllvm, main = "Ordination of sites, color: MeHg", 
         symbols = TRUE, pch = pchr, s.colors = ColorsMeHg)
legend("topleft", legend = c("P125", "P15", "P118", "P02", "P35", "P124", "P109"), pch = c(1, 2, 3, 4, 5, 6, 7), bty = "n")



ordiplot(fit_gllvm, main = "Ordination of sites, color: Conductivity",
         symbols = TRUE, pch = pchr, s.colors = ColorsCond)
legend("topleft", legend = c("P125", "P15", "P118", "P02", "P35", "P124", "P109"), pch = c(1, 2, 3, 4, 5, 6, 7), bty = "n")


# A clear gradient in the pH values of sites is observed,
# whereas there is less evidence of such pattern with the
# two other soil variables. It is also clear that the three
# sampling locations differ in terms of species composition.
# Standard deviation for the random site effects can be extracted
# by ftNULL$params$sigma. By plotting the predicted random site effects,
# we can possibly see differences in sampling intensity of the eight sites.


# Sampling locations of the eight sampling sites:
locaSites<-c(1, 2, 3, 4, 5, 6, 7)
plot(fit_gllvm$params$row.params, xlab = "Pond", col = locaSites, pch = locaSites, 
     main = "Site effects", ylab = "Site effect", xaxt = 'n', ylim = c(-1,1.5))
axis(1, at=1:8, labels=levels(sDesign$Site))
legend("topright", legend = c("P125", "P15", "P118", "P02", "P35", "P124", "P109"), pch = c(1, 2, 3, 4, 5, 6, 7), 
       col = c(1, 2, 3, 4, 5, 6, 7), bty = "n")


# Next we produce a biplot based on GLLVM.
# Below, column indices of the 15 species with largest
# factor loadings are added in the (rotated) ordination plot.
# The biplot suggests a small set of indicator species which
# prefer sites with low pH values and a larger set of indicator species for high pH sites.


# Plot the species using column indices of the species:

rownames(fit_gllvm$params$theta) <- 1:ncol(my_data$rel)
ordiplot(fit_gllvm, main = "Ordination of sites and species", xlim = c(-6, 5), 
         ylim = c(-4, 4), symbols = TRUE, pch = pchr, s.colors = ColorsMeHg, 
         biplot = TRUE, ind.spp = 15, cex.spp = 0.9)
legend("topleft", legend = c("P125", "P15", "P118", "P02", "P35", "P124", "P109"), pch=c(1, 2, 3, 4, 5, 6, 7), bty = "n")


# In order to study if pH value alone is capable
# of explaining the variation in species composition
# across sites, we included it as explanatory variable
# in the GLLVM. When the Poisson distribution performed
# so poorly on the null model, we consider only NB GLLVMs
# in the following examples.


ftXph <- gllvm(y = my_data$rel, X = my_data$env, studyDesign = sDesign, formula = ~pH, family = "negative.binomial", 
               row.eff = ~(1|Site), num.lv = 2)
print (ftXph)


# Ranked point estimates with 95% confidence intervals are
# plotted below using function coefplot and indicate that 
# pH value strongly affects to the species composition as
# so many of the confidence intervals do not contain zero
# value (black). The species names are not informative in
# the coefficient plot when the number of species is so 
# large and can be removed using an argument y.label.




coefplot(ftXph, cex.ylab = 0.5, y.label = FALSE)

# The corresponding ordination plot given below indicates 
# that the gradient in the pH values of the sites vanishes,
# but the ordination still exhibits a sampling location effect.
# In particular, several Kilpisjarvi sites seem to differ what
# comes to the bacterial species composition and all Mayrhofen 
# sites are located at the top of the ordination.

ordiplot(ftXph, main = "Ordination of sites", 
         symbols = TRUE, pch = pchr, s.colors = Colorsph)
legend("topleft", legend = c("P125", "P15", "P118", "P02", "P35", "P124", "P109"), 
       pch = c(1, 2, 3, 4, 5, 6, 7), bty = "n")


# Next we include all environmental variables as explanatory variables in the GLLVM.

ftX <- gllvm(y = my_data$rel, X = my_data$env, studyDesign = sDesign, formula = ~pH + MeHg + Conductivity + Temp,
             family = "negative.binomial", row.eff = ~(1|Site), num.lv = 2)

# The information criteria for the model with all covariates were worse
# than for the model with only pH as a covariate. Below, point estimates
# with 95% confidence intervals below indicate that pH value is the main
# covariate affecting the species composition.

coefplot(ftX, cex.ylab = 0.5, y.label = FALSE, mar = c(4, 2, 2, 1))


# STUDYING CO-OCCURRENCE PATTERNS
# Latent variables induce correlation across response variables,
# and so provide a means of estimating correlation patterns across species,
# and the extent to which they can be explained by environmental variables.
# As explained previously, information on correlation is stored in the factor loadings,
# and the getResidualCor() function can be used to estimate the correlation matrix
# of the linear predictor across species. This can be visualized using the corrplot package:


cr <- getResidualCor(ftXph)
library(corrplot)
library(gclus)
corrplot(cr, diag = FALSE, 
         type = "lower", method = "number", order = "FPC", cl.cex = 0.8 ,tl.srt = 45, tl.col = "red")

cr_subset <- cr[1:50, 1:50]

corrplot(cr_subset, diag = FALSE, 
         type = "lower", method = "square", order = "FPC", cl.cex = 0.8 ,tl.srt = 45, tl.col = "red")

formula_4th = ~ PWY-5304 + PWY-5345 + PWY-6830 + METH-ACETATE-PWY + PWY-181 

New_path <- t(pathways)
New_path <- as.data.frame(New_path)
New_path$`1CMET2-PWY`

# INCORPORATING FUNCTIONAL TRAITS INTO ‘FOURTH CORNER’ MODELS

# Fit the gllvm model using my_data
fit_4th <- gllvm(y = my_data$rel, X = my_data$env, TR = my_data$pathways, 
                 formula = y ~ (pH + MeHg + Temp + Conductivity),
                   family = "negative.binomial", num.lv = 4, seed = 123, row.eff = "random", randomX = ~ pH + MeHg + Temp + Conductivity, 
                 control.start =list(n.init = 3, jitter.var = 0.01))

coefplot(fit_4th, cex.ylab = 0.5, y.label = TRUE, mar = c(4, 2, 2, 1))

coefplot(fit_4th)
# Fourth corner can be plotted also with next lines
fourth = fit_4th
library(lattice)
a = max( abs(fourth) )
colort = colorRampPalette(c("blue","white","red"))
plot.4th = levelplot(t(as.matrix(fourth)), xlab = "Environmental Variables",
             ylab = "Species traits", col.regions = colort(100),
             at = seq( -a, a, length = 100), scales = list( x = list(rot = 45)))
print(plot.4th)

library(ade4)
fourthcorner(fit_4th)



