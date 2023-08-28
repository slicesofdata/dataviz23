/* open data
new file.
DEFINE !path()"X:\Courses\CMCStats\2013\PsychStats_Student_Supplements_Sp14\02 Readings\Topic 09 - Prediction - Bivariate Linear Regression\Tempo and Alcohol\"!ENDDEFINE.
GET FILE=!path+"tempo and alcohol - Bach and James, 1979.sav".

/*GET FILE='X:\Courses\CMCStats\2010\0.lect\Topic 13 - Regression and the Prediction Model\tempo and alcohol - Bach and James, 1979.sav'.

/* Get means.
means tempo sips 
/cells mean stdev variance count.


/* run Pearson correlation
CORRELATIONS
  /VARIABLES=Tempo Sips
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE .

temp.
select if (not(any(patron, 3))).
CORRELATIONS
  /VARIABLES=Tempo Sips
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE .


/*to add regression line, open plot and under ELEMENTS, click add line at total.
GRAPH
  /SCATTERPLOT(BIVAR)=Tempo WITH Sips
  /MISSING=LISTWISE .

temp.
select if (not(any(patron, 3))).
GRAPH
  /SCATTERPLOT(BIVAR)=Tempo WITH Sips
  /MISSING=LISTWISE .


/* run bivariate linear regression
temp.
select if (not(any(patron, 3))).

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI BCOV R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT Sips
  /METHOD=ENTER Tempo
  /RESIDUALS DURBIN
  /CASEWISE PLOT(ZRESID*Zpred) OUTLIERS(3) .
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Sips
  /METHOD=ENTER Tempo
  /PARTIALPLOT ALL
  /SCATTERPLOT=(*SRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID) NORMPROB(ZRESID).

compute ssreg = .195.
compute sstot = .682.
compute rsqr = .195/.682.
FORMATS rsqr (F8.3).

exe.


/* graph to predict sips from tempo.
IGRAPH /VIEWNAME='Scatterplot' /X1 = VAR(Tempo) TYPE = SCALE /Y = VAR(Sips) TYPE = SCALE /COORDINATE = VERTICAL  /FITLINE
  METHOD = REGRESSION LINEAR LINE = TOTAL SPIKE=OFF /X1LENGTH=3.0 /YLENGTH=3.0 /X2LENGTH=3.0 /CHARTLOOK='NONE' /SCATTER
  COINCIDENT = NONE.
EXE.

/* graph to predict tempo from sips.
IGRAPH /VIEWNAME='Scatterplot' /X1 = VAR(sips) TYPE = SCALE /Y = VAR(tempo) TYPE = SCALE /COORDINATE = VERTICAL  /FITLINE
  METHOD = REGRESSION LINEAR LINE = TOTAL SPIKE=OFF /X1LENGTH=3.0 /YLENGTH=3.0 /X2LENGTH=3.0 /CHARTLOOK='NONE' /SCATTER
  COINCIDENT = NONE.
EXE.

