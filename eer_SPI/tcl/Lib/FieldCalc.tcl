#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : FieldCalc.tk
# Creation : Juillet 2001 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Creation d'une calculatrice appliquee aux champs.
#
# Fonctions:
#
#   FieldCalc::Close          { }
#   FieldCalc::ConvertFactor  { }
#   FieldCalc::ConvertList    { Frame }
#   FieldCalc::FuncList       { Frame }
#   FieldCalc::Window         { Parent }
#   FieldCalc::FormulaLoad    { }
#   FieldCalc::FormulaSave    { Name }
#   FieldCalc::FormulaSet     { { Formula VP }
#   FieldCalc::InsertDigit    { Digit }
#   FieldCalc::InsertFunc     { Func }
#   FieldCalc::InsertOperator { Op }
#   FieldCalc::Operand        { VP Id Fields }
#   FieldCalc::Paste          { Text }
#   FieldCalc::Save           { File }
#   FieldCalc::WidgetConst    { Frame }
#   FieldCalc::WidgetConvert  { Frame }
#   FieldCalc::WidgetOps      { Frame }
#   FieldCalc::WidgetFunc     { Frame }
#
# Remarques :
#
#===============================================================================

package provide FieldCalc 2.2

catch { SPI::Splash "Loading Widget Package FieldCalc 2.2" }

namespace eval FieldCalc {
   variable Lbl
   variable Bubble
   variable Msg
   variable Data
   variable Param

   set Param(Geom)     { 620x190+[winfo rootx $Parent]+[winfo rooty $Parent] }
   set Param(Title)    { "Calculatrice" "Calculator" }
   set Param(Version)  2.2
   set Param(ShowAll)  False

   set Data(FieldNo)    0
   set Data(Formulas)   {}
   set Data(FuncType)   ""     ;#----- Current function type
   set Data(ConvType)   ""     ;#----- Current conversion type
   set Data(ConvFrom)   ""     ;#----- Current conversion from
   set Data(ConvTo)     ""     ;#----- Current conversion to
   set Data(ConvFactor) ""     ;#----- Current conversion factor

   set Param(All) { }

   set Param(Logical) {
      { ifelse(A,B,C) "Operateur conditionnel (Si A alors B sinon C)" "Conditional operator (If A then B else C)" }
      { within(A,B,C) "Si la valeur de A est entre B et C"            "Checks if the Value of A is between B and C" }
      { in(A,B)       "Si la valeur de A existe dans B"               "Checks if the value of A exists in B" }
   }

   set Param(Derivative) {
      { darea(A)      "Aire des cellules"                                         "Area of grid cells" }
      { dlat(A)       "Latitudes de chaque points de grilles"                     "Latitudes of grid cells" }
      { dlon(A)       "Longitudes de chaque points de grilles"                    "Longitudes of grid cells" }
      { ddx(A)        "Longueur des cellules selon X en mètres"                   "Length of grid cells in meters along X axis" }
      { ddy(A)        "Longueur des cellules selon Y en mètres"                   "Length of grid cells in meters along Y axis" }
      { dangle(A)     "Orientation des cellules par rapport au nord géographique" "Grid cell orientation in relation to geographical north" }
      { dslopedeg(A)  "Pente en dégrée"                                           "Slope in degrees" }
      { dslope100(A)  "Pente en pourcentage (90º = 100%)"                         "Slope in percentage (90º = 100%)" }
      { daspect(A)    "Angle azimutale de la pente"                               "Azimuthal angel of the slope" }
      { ddxfirst(A)   "Dérivée en x du premier degré"                             "First degree derivate along x" }
      { ddyfirst(A)   "Dérivée en y du premier degré"                             "First degree derivate along y" }
      { ddxsecond(A)  "Dérivée en x du second degré"                              "Second degree derivate along x" }
      { ddysecond(A)  "Dérivée en y du second degré"                              "Second degree derivate along y" }
      { ddxysecond(A) "Dérivée en xy du second degré"                             "Second degree derivate along xy"  }
      { dprofcurve(A) "Calcul de la courbature du profil"                         "Profile curvature" }
      { dtangcurve(A) "Calcul de la courbature tangentielle "                     "Tangential profile curvature" }
   }

   set Param(Table) {
      { win(A,B,C)      "Nombre d'occurence de valeur de C entre A et B"                                                                    "Number of occurence of C value between A and B" }
      { tcount(A,B)     "Compte le nombre d'indices de A apparaissant dans B"                                                               "Number of occurence of A in B" }
      { lut(A,B,C)      "Assigne a A la valeur correspondante de B dans C (lookup table)"                                                   "Assigns to A the corresponding value of B in C (lookup table)" }
      { slut(A,B,C)     "Assigne a A la valeur correspondante de B dans C, pour les tables de correspondances triées (sorted lookup table)" "Assigns to A the corresponding value of B in C for sorted tables (sorted lookup table)" }
      { fkernel(A,B)    "filtre (convolution) la matrice A en utilisant le kernel B"                                                        "Filter (convolution) matrix A using kernel B" }
      { fcentile(A,B,C) "filtre centile de la matrice A en utilisant le kernel de dimension B en sélectionnant le centile C"                "Centile filter matrix A using kernel of dimension B while selecting centile C" }
   }

   set Param(Mathematical) {
      { log(A)       "Logarithme base 10"                             "Logarithm" }
      { logn(A)      "Logarithme naturel"                             "Natural logarithm" }
      { exp(A)       "Exponentielle"                                  "Exponential" }
      { sqrt(A)      "Racine carré"                                   "Square" }
      { cbrt(A)      "Racine cubique"                                 "Cubic" }
      { abs(A)       "Valeur absolue"                                 "Aboslute value" }
      { ceil(A)      "Valeur entiere plafond"                         "Higher integer value" }
      { floor(A)     "Valeur entiere plancher"                        "Lower integer value" }
      { round(A)     "Valeur entiere arondie"                         "Round to nearest integer" }
      { rem(A,n)     "Reste point flottant"                           "Remainder value" }
      { mod(A,n)     "Module point flottant"                          "Modulo value" }
      { min(A,n)     "Définir le minium"                              "Set minimum to n" }
      { max(A,n)     "Définir le maximum"                             "set maximum to n" }
      { clamp(A,m,n) "Définir le minimum et le maximum"               "Set minimum to m and maximum value to n" }
      { frand(A,m,n) "Nombre aléatoire entre m et n"                  "Random number between m and n" }
      { sin(A)       "Sinus"                                          "Sinus" }
      { cos(A)       "Cosinus"                                        "Cosinus" }
      { tan(A)       "Tangente"                                       "Tangent" }
      { asin(A)      "Arc sinus"                                      "Arc sinus" }
      { acos(A)      "Arc cosinus"                                    "Arc cosinus" }
      { atan(A)      "Arc tangente"                                   "Arc tangent" }
      { atan2(A,B)   "Arc tangente de X sur Y (résultat de -PI a PI)" "Arc tangent of X over Y (results fromt -PI to PI)" }
      { sinh(A)      "Sinus hyperbolique"                             "Sinus hyperbolic" }
      { cosh(A)      "Cosinus hyperbolique"                           "Cosinus hyperbolic" }
      { tanh(A)      "Tangente hyperbolique"                          "Tangent hyperbolic" }
      { asinh(A)     "Arc sinus hyperbolique"                         "Arc sinus hyperbolic" }
      { acosh(A)     "Arc cosinus hyperbolique"                       "Arc cosinus hyperbolic" }
      { atanh(A)     "Arc tangente hyperbolique"                      "Arc tangent hyperbolic" }
   }

   set Param(Statistical) {
      { smb(A,B)    "Biais moyen" "Mean bias" }
      { sme(A,B)    "Erreur moyenne" "Mean error" }
      { snmb(A,B)   "Biais moyen normalisé" "Normalized mean bias" }
      { snme(A,B)   "Erreur moyenne normalisée" "Normalized mean error" }
      { smnb(A,B)   "Moyenne du biais normalisé" "Mean normalized bias" }
      { smne(A,B)   "Moyenne de l'erreur normalisée" "Mean normalized error" }
      { slmnb(A,B)  "Biais moyen logarithmique normalisé" "Logarithmic mean normalized bias" }
      { slmne(A,B)  "Erreur moyenne logarithmique normalisée" "Logarithmic mean normalized error" }
      { sfb(A,B)    "Biais fractionnel" "Fractionnal bias" }
      { smfb(A,B)   "Biais fractionel moyen" "Mean fractional bias" }
      { smfe(A,B)   "Erreur fractionelle moyenne" "Mean fractional error" }
      { smre(A,B)   "Erreur relative moyenne" "Mean relative error" }
      { smaxb(A,B)  "Biais maximum" "Maximum bias" }
      { smaxe(A,B)  "Erreur maximale" "Maximum error" }
      { smaxre(A,B) "Erreur maximale relative" "Maximum relative error" }
      { srmse(A,B)  "Erreur moyenne quadratique" "Root mean square error" }
      { snrmse(A,B) "Erreur moyenne quadratique normalisée" "Normalized Root mean square error" }
      { snmse(A,B)  "Erreur moyenne au carré normalisée" "Normalized mean square error" }
      { sgmb(A,B)   "Biais géométrique moyen" "Geometric mean bias" }
      { sgmv(A,B)   "Variance géométrique moyenne" "Geometric mean variance" }
      { scor(A,B)   "Coefficient de corrélation" "Correlation coeficient" }
      { scov(A,B)   "Covariance" "Covariance" }
      { svar(A)     "Variance" "Variance" }
      { sregb(A,B)  "Coefficient de régression (Pente de la courbe)" "Regression coefficient (Slope of curve fitting)" }
      { srega(A,B)  "Coefficient de régression (Interscetion de la courbe)" "Regression coefficient (Intercept of curve fitting)" }
      { serra(A,B)  "Erreur standard pour a" "Standard Error for a" }
      { serrb(A,B)  "Erreut standard pour b" "Standard Error for b" }
      { sssx(A,B)   "Somme des carrés en X" "Sum of squared values X" }
      { sssy(A,B)   "Somme des carrés en Y" "Sum of squared values Y" }
      { sssxy(A,B)  "Somme des carrés en XY" "Sum of squared values XY" }
      { snb(A)      "Nombre d'échantillon" "Number of sample" }
      { ssdev(A)    "Déviation standard" "Standard deviation" }
      { ssdevx(A,B) "Déviation standard en X" "Standard deviation X" }
      { ssdevy(A,B) "Déviation standard en Y" "Standard deviation Y" }
      { smed(A)     "Médianne" "Median" }
      { smedx(A,B)  "Médianne en X" "Median X" }
      { smedy(A,B)  "Médianne en Y" "Median Y" }
      { suniq(A)    "Nombre de valeur unique" "Number of unique values" }
      { ssum(A)     "Somme" "Sum" }
      { smin(A)     "Minimum" "Minimum" }
      { smax(A)     "Maximum" "Maximum" }
      { savg(A)     "Moyenne" "Average" }
      { na(A,B)     "Nombre de valeurs rejetées (NoData)" "Number of rejected values (NoData)" }
      { srna(A,B)   "Ratio de valeurs rejetées (NoData)" "Ratio of rejected values (NoData)" }
      { sfoex(A,B)  "Facteur d'excédance" "Factor of excedance" }
      { sfa2(A,B)   "Facteur 2" "Factor of 2" }
      { sfa5(A,B)   "Facteur 5" "Factor of 5" }
      { sfa10(A,B)  "Facteur 10" "Factor of 10" }
      { snad(A,B)   "Différence absolue normalisée" "Normalized absolute difference" }
      { sfms(A,B)   "Figure de mérite dans l'espace" "Figure of merit in space" }
      { fmsb(A,B)   "Figure de mérite dans l'espace (binaire)" "Figure of merit in space (binary)" }
      { sfmsi(A,B)  "Figure de mérite dans l'espace (intégré)" "Figure of merit in space (integrated)" }
      { osf(A,B)    "Fonction de score objectif" "Objective scoring function" }
      { sosfb(A,B)  "Fonction de score objectif (binaire)" "Objective scoring function (binary)" }
      { sosfi(A,B)  "Fonction de score objectif (intégré)" "Objective scoring function (integrated)" }
      { spcc(A,B)   "Coefficient de corrélationde Pearson" "Pearson's correlation coefficient" }
      { sksp(A,B)   "Paramètre de Kolmogorov-Smirnov" "Kolmogorov-Smirnov parameter" }
      { srank(A,B)  "Rang calculaté avec une combinaison de fb, fms, fa2, ksp et nad" "Rank calculated with a combination of fb, fms, fa2, ksp and nad" }
      { snbeq(A,B)  "Nombre de valeur égale" "Number of equal values" }
      { snbgt(A,B)  "Nombre de valeur de B plus grande que A"  "Number of values of B greater than A" }
      { snblt(A,B)  "Nombre de valeur de B plus petite que A" "Number of values of B smaller than A" }
      { snbfa(A,B)  "Nombre de fausse alarmes (A=0, B!=0)" "Number of false alarms (A=0, B!=0)" }
      { snbmi(A,B)  "Nombre de d'échec (A!=0, B=0)" "Number of misses (A!=0, B=0)" }
      { snbnp(A,B)  "Nombre de paire nulles (A=0, B=0)" "Number of null pairs (A=0, B=0)" }
      { saov(A,B)   "Région de superposition (A!=0 B!=0)" "Overlap area (A!=0, B!=0)" }
      { safn(A,B)   "Région de faux négatifs (A!=0, B=0)" "False negative area (A!=0, B=0)" }
      { safp(A,B)   "Région de faux positifs (A=0, B!=0)" "False positive area (A=0, B!=0)" }
      { sax(A,B)    "Région de valeurs no-bulle pour A" "Area covered by non-null values in field A" }
      { say(A,B)    "Région de valeurs no-bulle pour B" "Area covered by non-null values in field B" }
   }

   set Param(Distance) {
      { m    1.0           "mètre"              "meter" }
      { in   0.0254        "pouce"              "inch" }
      { A    1e-10         "angstrom"           "angstrom" }
      { km   1000.0        "kilomètre"          "kilometer" }
      { ft   0.3048        "pied"               "foot" }
      { yd   0.9144        "verge"              "yard" }
      { nm   1852          "mile nautique"      "nautical mile" }
      { au   1.49597870e11 "unité astronomique" "astronomical unit" }
      { pc   3.0857e16     "parsec"             "parsec" }
      { ly   9.4607e15     "anneé lumière"      "lightyear" }
      { fg   201.168       "furlong"            "furlong" }
      { fa   1.8288        "brasse"             "fathom" }
      { ch   20.1168       "chaine"             "chain" }
      { mile 1609.344      "mile"               "mile" }
      { cb   182.88        "câble"              "cable" }
   }

   set Param(Area) {
      { m²    1.0            "mètre carré" "square meter" }
      { hc    10000.0        "hectare"     "hectare" }
      { hi    485000.0       "hide"        "hide" }
      { in²   6.4516e-4      "pouce carré" "square inch" }
      { ft²   9.290304e-2    "pied carré"  "square foot" }
      { yd²   8.3612736e-1   "verge carré" "square yard" }
      { rood  1.01171e3      "rood"        "rood" }
      { acre  4.0468564224e3 "acre"        "acre" }
      { mile² 2.58998811e6   "mile carré"  "square mile" }
   }

   set Param(Volume) {
      { m³     1.0            "mètre cube"    "cubic meter" }
      { in³    1.6387064e-5   "pouce cube"    "cubic inch" }
      { ft³    2.8316846e-2   "pied cube"     "cubic foot" }
      { yd³    7.64554858e-1  "verge cube"    "cubic yard" }
      { mile³  4.168181825e9  "mile cube"     "cubic mile" }
      { min    6.16115e-8     "minim"         "minim" }
      { dram   3.69669e-6     "dram liquide"  "fluid dram" }
      { liqoz  2.95735e-5     "once liquide"  "liquid ounce" }
      { gi     1.18294e-4     "gill"          "gill" }
      { liqpt  4.73176e-4     "pinte liquide" "liquid pint" }
      { liqqt  9.46353e-4     "quart liquide" "liquid quart" }
      { gal    3.785411784e-3 "gallon"        "gallon" }
      { drypt  5.50610e-4     "pinte sèche"   "dry pint" }
      { dryqt  1.10122e-3     "quart sec"     "dry quart" }
      { pk     8.80977e-3     "peck"          "peck" }
      { bu     3.52391e-2     "bushel"        "bushel" }
      { bbl    1.58987e-1     "baril"         "barrel" }
      { drybbl 1.15627e-1     "baril sec"     "dry barrel" }
      { L      1.0e-3         "litre"         "litre" }
   }

   set Param(Mass) {
      { kg    1.0        "kilogramme" "kilogram" }
      { dr    1.77185e-3 "dram"       "dram" }
      { oz    2.83495e-2 "once"       "ounce" }
      { lb    0.45359237 "livre"      "pound" }
      { shcwt 4.53592e1  "centième"   "short hundredweight" }
      { shtn  9.07185e2  "tonne"      "short ton" }
      { ca    0.0002     "carat"      "carat" }
      { gr    0.000065   "grain"      "grain" }
   }

   set Param(Speed) {
      { ms    1.0        "mètre par seconde"     "meter second" }
      { kmh   2.77778e-1 "kilomètre heure"       "kilometer per hour" }
      { mph   4.4704e-1  "mile a l'heure"        "mile per hour" }
      { knot  5.14444e-1 "noeud"                 "knot" }
      { Mach  340.2933   "Vitesse du son"        "Sound speed" }
      { Light 99790000   "Vitesse de la lumiere" "Light speed" }
   }

  set Param(Pressure) {
      { Pa      1.0       "pascal"                      "pascal" }
      { lbf/ft² 4.78803e1 "livre-force par pied carré"  "pound-force per square foot" }
      { lbf/in² 6.89476e3 "livre-force par pouce carré" "pound-force per square inch" }
      { mmHg    1.33322e2 "millimètre de mercure"       "millimeter of mercury" }
      { inH2O   2.49089e2 "pouce d'eau"                 "inch of water" }
      { inHg    3.38639e3 "pouce de mercure"            "inch of mercury" }
      { atm     1.01325e5 "atmosphère standard"         "standard atmosphere" }
      { torr    1.33322e2 "torr"                        "torr" }
      { bar     1e5       "bar"                         "bar" }
   }

  set Param(Energy) {
      { J     1.0           "joule"                "joule" }
      { eV    1.6021892e-19 "electronvolt"         "electronvolt" }
      { ftlbf 1.35582       "foot pound force"     "foot pound force" }
      { cal   4.1868        "calorie"              "calorie" }
      { kgfm  9.80665       "kilogram force meter" "kilogram force meter" }
      { Btu   1.05506e3     "British Thermal Unit" "British Thermal Unit" }
      { Wh    3.6e3         "watt heure"           "watt hour" }
      { kWh   3.6e6         "kilowatt heure"       "kilowatt hour" }
      { therm 1.05506e8     "therm"                "therm" }
      { erg   1e-7          "erg"                  "erg" }
   }

  set Param(Power) {
      { W   1.0       "watt"                "watt" }
      { hp  7.45700e2 "horsepower"          "horsepower" }
      { hpm 7.35499e2 "horsepower métrique" "horsepower metric" }
   }

  set Param(Force) {
      { N   1.0        "newton"           "newton" }
      { dyn 1e-5       "dyne"             "dyne" }
      { lbf 4.44822    "livre force"      "pound force" }
      { kgf 9.80665    "kilogramme force" "kilogram force" }
      { sn  1e3        "sthene"           "sthene" }
      { pdl 1.38255e-1 "poundal"          "poundal" }
   }

   set Param(Radiation) {
      { Bq      1.0     "becquerel"              "becquerel" }
      { Ci      3.7e10  "curie"                  "curie" }
      { Sv      1e-2    "sievert"                "sievert" }
      { rem     1.0     "Rontgen Equivalent Man" "Rontgen Equivalent Man" }
      { C/kg    2.58e-4 "coulomb par kilogramme" "coulomb per kilogram" }
      { rontgen 1.0     "rontgen"                "rontgen" }
   }

   set Param(Const) {
      { Pi  3.14159265358979323846264338327 "Pi"                                                                      "Pi" }
      { Re  6.37e6                          "Rayon moyen de la Terre (m)"                                             "Average radius of Earth (m)" }
      { g   9.81                            "Gravite moyenne de la Terre (m/s²)"                                      "Mean gravity of Earth (m/s²)" }
      { W   7.292e-5                        "Velocitee angulaire de la Terre (rad/s)"                                 "Angular velocity of Earth (rad/s)" }
      { c   2.998e8                         "Vitesse de la lumiere (m/s)"                                             "Velocity of light (m/s)" }
      { Rx  8.3143e3                        "Constante des gaz universel (J/K/mol)"                                   "Universal gas constant (J/K/mol)" }
      { k   1.381e-23                       "Constante de Boltzman (J/K/mol)"                                         "Boltzmann`s constant (J/K/mol/)" }
      { Na  6.022e23                        "Nombre d'Avogadro (mol-1)"                                               "Avogadro number (mol-1)" }
      { S   5.6696e-8                       "Constante de Stefan-Boltzman (W m-2 K-4)"                                "Stefan-Boltzman constant (W m-2 K-4)" }
      { h   6.6262e-34                      "Constante de Planck (J s)"                                               "Planck constant (J s)" }
      { e0  8.85e-12                        "Permissivitee du vacuum (C2 N-1 m2)"                                     "Permittivity of vacuum (C2 N-1 m2)" }
      { Md  28.97                           "Poid moyen de l'air sec"                                                 "Average molecule weight of dry air" }
      { Rd  287                             "Constante des gas de l'air sec (J K-1 kg-1)"                             "Gas constant of dry air (J K-1 kg-1)" }
      { Rv  461                             "Constante des gaz de la vapeur d'eau (J K-1 kg-1)"                       "Gas constant of water vapor (J K-1 kg-1)" }
      { rd  1.275                           "Densite de l'air sec a 0ºC et 1000mb (kg/m³)"                            "Density of dry air at 0ºC and 1000mb (kg/m³)" }
      { rw  1.0e3                           "Densite de l'eau liquide a 0ºC (kgm³)"                                   "Density of liquid water at 0ºC (kg/m³)" }
      { ri  0.917e3                         "Densite de la glace a 0ºC (kg/m³)"                                       "Density of ice at 0ºC (kg/m³)" }
      { cpd 1004                            "Chaleur specifique de l'air sec a pression constante (J K-1 kg-1)"       "Specific dry air heat at constant pressure (J K-1 kg-1)" }
      { cvd 717                             "Chaleur specifique de l'air a volume constant volume (J K-1 kg-1)"       "Specific dry air heat at constant volume (J K-1 kg-1)" }
      { cpv 1952                            "Chaleur specifique de la vapeur d'eau a pression constante (J K-1 kg-1)" "Specific water vapor heat at constant pressure (J K-1 kg-1)" }
      { cvv 1463                            "Chaleur specifique de la vapeur d'eau a volume constant(J K-1 kg-1)"     "Specific water vapor heat at constant volume (J K-1 kg-1)" }
      { cw  4218                            "Chaleur specifique de la vapeur d'eau a 0ºC (J K-1 kg-1)"                "Specific water vapor heat at 0ºC (J K-1 kg-1)" }
      { ci  2106                            "Chaleur specifique de la glace a 0ºC (J K-1 kg-1)"                       "Specific ice heat at 0ºC (J K-1 kg-1)" }
   }

   #----- Textes et labels
   set Lbl(To)       { " en " " to " }
   set Lbl(Yes)      { "Oui" "Yes" }
   set Lbl(No)       { "Non" "No" }
   set Lbl(None)     { "Aucune" "None" }
   set Lbl(Funcs)    { "Fonctions" "Functions" }
   set Lbl(Conv)     { "Conversion" "Conversion" }
   set Lbl(Const)    { "Constantes" "Constant" }

   set Lbl(Operators) {
      { "Tous"          "All" }
      { "Mathématiques" "Mathematical" }
      { "Logiques"      "Logical" }
      { "Dérivatifs"    "Derivative" }
      { "Satistiques"   "Statistical" }
      { "Tables"        "Table" } }

   set Lbl(Converts) {
      { "Distance"  "Distance" }
      { "Région"    "Area" }
      { "Volume"    "Volume" }
      { "Masse"     "Mass" }
      { "Vitesse"   "Speed" }
      { "Pression"  "Pressure" }
      { "Energie"   "Energy" }
      { "Puissance" "Power" }
      { "Force"     "Force" }
      { "Radiation" "Radiation" } }

   #----- Messagess
   set Msg(Del)     { "Voulez-vous vraiment supprimer cette fonction ?" "Do you really want to suppress this function ?" }
   set Msg(Exist)   { "Cette fonction existe deja, voulez-vous la remplacer ?" "This function is already defined, do you want to replace it ?" }
   set Msg(Name)    { "Veuillez spéfifier le nom de la formule." "Please enter the formula name." }
   set Msg(Saved)   { "Formule sauvegardee." "Formula saved." }
   set Msg(Operand)  { "L'opérande n'est pas un champs.\n\n\tRésultat: " "Operand is not a field.\n\n\tResult: " }

   #----- Bulles d'aides
   set Bubble(Param)      { "Définir/Sauvegader les paramêtres du champs résultant" "Define/Save the result field parameters" }
   set Bubble(Formula)    { "Nom de la formule courante" "Current formula name" }
   set Bubble(Save)       { "Sauvegarde de la formule courante" "Save the current formula" }
   set Bubble(Del)        { "Suppression de la formule courante de la liste des formules sauvegardees" "Suppress the current formula from the saved list" }
   set Bubble(Info)       { "Mantisse\nTapper la formule ou utiliser les boutons de la calculatrice.\nPour inserer un champs,\
                             selectionne le dans une boite de champs avec le bouton central de la souris."
                            "Mantisse\nType in the formula or use the calculator buttons\nTo insert a field,\
                             click on it with the middle mouse button in a fieldbox." }
   set Bubble(List)       { "Liste des operations effectuees" "Past operations made" }
   set Bubble(ConvType)   { "Selection du type de conversion" "Select the conversion type" }
   set Bubble(ConvFrom)   { "Unite du format a convertir" "Unit to convert from" }
   set Bubble(ConvTo)     { "Unite de conversion" "Unit to convert to" }
   set Bubble(ConvFact)   { "Facteur de conversion\nAppuyer pour appliquer a la mantisse"
                           "Conversion factor\nPress to apply to mantissa" }
   set Bubble(FuncType)   { "Selection du type de fonctions" "Select the function type" }
   set Bubble(FuncSearch) { "Recherche une fonction" "Search functions" }
   set Bubble(FuncList)   { "Liste des fonctions, cliquez pour insérer" "List of functions, click one to insert" }
   set Bubble(NOT)        { "Negation logique" "Logical negation" }
   set Bubble(AND)        { "ET logique" "Logical AND" }
   set Bubble(OR)         { "OU logique" "Logical OR" }
   set Bubble(Int2)       { "Interpolation lineaire" "Linear interpolation" }
   set Bubble(Int3)       { "Interpolation cubique"  "cubic interpolation" }
   set Bubble(Idx)        { "Indexation vectorielle ou norme du vecteur"  "Vectorial indexation or vector length" }
   set Bubble(Prime)      { "Afficher / ne pas afficher les champs participant au calcul"  "Display / do not display fields used in the calculus" }
   set Bubble(Equal)      { "Appliquer la formule à la vue active"  "Apply formula to the active viewport" }
   set Bubble(Clear)      { "Effacer la formule de la vue active"  "Clear formula from active viewport" }
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::Apply>
# Creation : Mai 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Appliquer le changement à l'opérande.
#
# Parametres  :
#   <Clear<   : Clear operand
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::Apply { { Clear True } } {
   variable Data

   if { $Clear } {
      catch {
         foreach field $Data(Fields$Page::Data(VP)) {
            fstdfield configure $field -active True
         }
      }
      set FieldCalc::Data(Operand) ""
   }

   FieldCalc::FormulaSet ""
   Viewport::UpdateData $Page::Data(Frame)
   Page::UpdateCommand $Page::Data(Frame)
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::Close>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermer la calculatrice et liberer les donnes temporaires et
#            l'affichage.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::Close { } {
   variable Data

   destroy .fieldcalc
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::ConvertFactor>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Determiner le facteur de conversion.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::ConvertFactor { } {
   variable Param
   variable Data

   if { $Data(ConvFrom)!= "" && $Data(ConvTo)!= "" } {
      set from [lindex [lsearch -index 0 -inline $Param($Data(ConvArray)) $Data(ConvFrom)] 1]
      set to   [lindex [lsearch -index 0 -inline $Param($Data(ConvArray)) $Data(ConvTo)] 1]

      set Data(ConvFactor) [expr $from * (1.0/$to)]
   } else {
      set Data(ConvFactor) ""
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::QuickConvert>
# Creation : Avril 2016 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Convertir rapidement une donnée
#
# Parametres   :
#  <Type>   : Type d'unité à convertir (Distance, Volume, etc.)
#  <From>   : Unité de la valeur à convertir (m, L, etc.)
#  <To>     : Unité de la valeur finale (m, L, etc.)
#  <Val>    : La valeur à convertir
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::QuickConvert { Type From To Val } {
   variable Param

   set from [lindex [lsearch -index 0 -inline $Param($Type) $From] 1]
   set to   [lindex [lsearch -index 0 -inline $Param($Type) $To] 1]

   return [expr {$Val*$from/$to}]
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::Window>
# Creation : Juin 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche l'interface de la calculatrice.
#
# Parametres   :
#   <Parent>   : Nom du widget
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::Window { { Parent .} } {
   global GDefs
   variable Bubble
   variable Lbl
   variable Data
   variable Param

   if { [winfo exists .fieldcalc] } {
      return
   }

   toplevel     .fieldcalc
   wm title     .fieldcalc "[lindex $SPI::Title(SPI) $GDefs(Lang)] $GDefs(Version) ([lindex $Param(Title) $GDefs(Lang)] $Param(Version))"
   wm transient .fieldcalc $Parent
   eval wm geom .fieldcalc $Param(Geom)
   wm protocol  .fieldcalc WM_DELETE_WINDOW "FieldCalc::Close"

   FieldCalc::FormulaLoad

   frame .fieldcalc.expr -relief raised -bd 1
      entry .fieldcalc.expr.op -textvariable FieldCalc::Data(Operand) -bd 1 -bg $GDefs(ColorLight)
      button .fieldcalc.expr.param -image INFOLOG -relief flat -state disabled -overrelief raised -bd 1 \
         -command { if { [fstdfield is CALC$Page::Data(VP)] } { FieldParams::Window CALC$Page::Data(VP) } }
      ComboBox::Create .fieldcalc.expr.sel FieldCalc::Data(Formula) edit unsorted nodouble -1 $FieldCalc::Data(Formulas) 15 3 { FieldCalc::FormulaSet $FieldCalc::Data(Formula) }
      button .fieldcalc.expr.fsave -image CALCSAVE -relief flat -overrelief raised -bd 1 -command { FieldCalc::FormulaSave [Dialog::Get . $FieldCalc::Bubble(Save) $FieldCalc::Msg(Name)]}
      button .fieldcalc.expr.fdel  -image CALCDEL -relief flat -overrelief raised -bd 1 -command FieldCalc::FormulaDel
      pack .fieldcalc.expr.op -side left -fill both -expand true
      pack .fieldcalc.expr.param -side left
      pack .fieldcalc.expr.sel -side left -fill both
      pack .fieldcalc.expr.fsave .fieldcalc.expr.fdel -side left -fill both
   pack .fieldcalc.expr -side top -anchor e -padx 2 -pady 2 -fill x

#   tkdnd::drop_target register .fieldcalc.expr.op DND_Text
#   bind .fieldcalc.expr.op <<Drop>> { return [FieldCalc::Paste %D] }

   Bubble::Create .fieldcalc.expr.sel   $Bubble(Formula)
   Bubble::Create .fieldcalc.expr.param $Bubble(Param)
   Bubble::Create .fieldcalc.expr.fsave $Bubble(Save)
   Bubble::Create .fieldcalc.expr.fdel  $Bubble(Del)

   #----- Creation des fonctions
   TabFrame::Create .fieldcalc.func 2 ""
   pack .fieldcalc.func -side right -fill both -expand true -padx 2 -pady 2

   FieldCalc::WidgetFunc    [TabFrame::Add .fieldcalc.func 1 [lindex $Lbl(Funcs) $GDefs(Lang)] False ""]
   FieldCalc::WidgetConvert [TabFrame::Add .fieldcalc.func 1 [lindex $Lbl(Conv)  $GDefs(Lang)] False ""]
   FieldCalc::WidgetConst   [TabFrame::Add .fieldcalc.func 1 [lindex $Lbl(Const) $GDefs(Lang)] False ""]
   FieldCalc::WidgetOps     .fieldcalc

   TabFrame::Select .fieldcalc.func 0

   bind .fieldcalc.expr.op <KeyRelease> { FieldCalc::FormulaSet "" }
   bind .fieldcalc.expr.op <Return>     { Viewport::UpdateData $Page::Data(Frame); Page::UpdateCommand $Page::Data(Frame) }

   focus .fieldcalc.expr.op
   .fieldcalc.expr.op icursor 0

   #----- Reset internal variable
   set Data(Operand) ""
   set Data(FieldNo) 0
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::FormulaDel>
# Creation : Janvier 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprime une formule de la liste
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::FormulaDel { } {
   global GDefs env
   variable Data
   variable Lbl
   variable Msg

   if { $Data(Formula)!="" && $Data(Formula)!=[lindex $Lbl(None) $GDefs(Lang)] && [set idx [lsearch -exact $Data(Formulas) $Data(Formula)]]!=-1  } {

      set del [Dialog::Default . 200 WARNING $Msg(Del) "" 0 $Lbl(No) $Lbl(Yes)]

      if { $del } {

         ComboBox::Del .fieldcalc.expr.sel $Data(Formula)
         set Data(Formulas) [lreplace $Data(Formulas) $idx $idx]
         set Data(Formula) ""

         file rename -force $env(HOME)/.spi/FieldCalc $env(HOME)/.spi/FieldCalc.old

         set f [open $env(HOME)/.spi/FieldCalc w]

         foreach name $Data(Formulas) {
            puts $f "\{$name\} \{$Data(Formula$name)\}"
         }

         close $f
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::FormulaLoad>
# Creation : Aout 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire la liste des formules.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::FormulaLoad { } {
   global GDefs env
   variable Data
   variable Lbl

   set Data(Formulas) [lindex $Lbl(None) $GDefs(Lang)]
   set Data(Formula[lindex $Lbl(None) $GDefs(Lang)]) ""

   set paths $env(HOME)/.spi
   if { [info exists env(SPI_TOOL)] } {
      set paths [concat [split $env(SPI_TOOL) :] $paths]
   }

   foreach path $paths {
      if { [file exist $path/FieldCalc] } {

         set f [open $path/FieldCalc r]
         while { ![eof $f] } {
            gets $f line

            if { $line!="" } {
               set name [lindex $line 0]
               set oper [lindex $line 1]
               set Data(Formula$name) $oper
               lappend Data(Formulas) $name
            }
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::FormulaSave>
# Creation : Aout 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder la liste des formules.
#
# Parametres :
#   <Name>   : Layout name
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::FormulaSave { Name } {
   global env
   variable Lbl
   variable Msg
   variable Data

   #----- If this is a valid name
   if { [set Name [string trim $Name]]=="" } {
     return
   }

   set ok  1
   set add 1
   if { [lsearch -exact $Data(Formulas) $Name]!=-1 } {
      set ok [Dialog::Default .fieldcalc 200 WARNING $Msg(Exist) "" 0 $Lbl(No) $Lbl(Yes)]
      if { $ok } {
         set add 0
      }
   }

   if { !$ok } {
      return
   }

   if { $add } {
      set Data(Formula) $Name
      lappend Data(Formulas) $Data(Formula)
      ComboBox::Add .fieldcalc.expr.sel $Data(Formula)
   }
   set Data(Formula$Data(Formula)) $Data(Operand)

   if { [file exist $env(HOME)/.spi/FieldCalc] } {
      file rename -force $env(HOME)/.spi/FieldCalc $env(HOME)/.spi/FieldCalc.old
   }

   set f [open $env(HOME)/.spi/FieldCalc w]

   foreach name $Data(Formulas) {
      if { $Data(Formula$name)!= "" } {
         puts $f "\{$name\} \{$Data(Formula$name)\}"
      }
   }

   close $f
   Dialog::Info . $Msg(Saved)
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::FormulaSet>
# Creation : Aout 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Instaurer la formule macro.
#
# Parametres :
#   <Formula>: Formule
#   <VP>     : Vue a assigner
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::FormulaSet { Formula { VP "" } } {
   variable Data

   set Animator::Play(Stop) 1
   
   catch {
      set Data(Formula) $Formula
      if { $Formula!="" } {
         set Data(Operand) $Data(Formula$Formula)
      }

      if { $VP=="" } {
         set VP $Page::Data(VP)
      }
      set Viewport::Data(Operand$VP) $Data(Operand)
      set Viewport::Data(Formula$VP) $Data(Formula)
   }
}
#----------------------------------------------------------------------------
# Nom      : <FieldCalc::InsertDigit>
# Creation : Juin 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer un chiffre dans l'expression.
#
# Parametres :
#   <Digit>  : Chiffre a inserer
#
# Retour:
#
# Remarques :
#   -Si une selection existe, elle est remplacer par le chiffre sinon
#    on insere le chiffre a la position du curseur.
#
#----------------------------------------------------------------------------

proc FieldCalc::InsertDigit { Digit } {
   variable Data

   if { [.fieldcalc.expr.op selection present] } {
      .fieldcalc.expr.op delete sel.first sel.last
   }
   .fieldcalc.expr.op insert insert $Digit
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::InsertFunc>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer une fonction dans l'expression.
#
# Parametres :
#   <Func>   : Fonction a inserer
#   <Argc>   : Nombre d'argument de la fonction
#
# Retour:
#
# Remarques :
#   -Si une selection existe, on applique la fonction a la selection sinon
#    on insere la fonctiona la position du curseur.
#
#----------------------------------------------------------------------------

proc FieldCalc::InsertFunc { Func } {
   variable Data

   set func [lindex $Func 0]

   #----- Si il y a selection, s'en servir pour le premier argument de la fonction
   if { [.fieldcalc.expr.op selection present] } {
      set idx [string first A $func]
      .fieldcalc.expr.op insert sel.first [string range $func 0 [expr $idx-1]]
      .fieldcalc.expr.op insert sel.last [string range $func [expr $idx+1] end]
      .fieldcalc.expr.op icursor [.fieldcalc.expr.op index sel.last]

  } else {
     .fieldcalc.expr.op insert insert $func
  }

   .fieldcalc.expr.op selection clear
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::InsertOperator>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer un operateur dans l'expression.
#
# Parametres :
#   <Op>     : Operateur a inserer
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::InsertOperator { Op } {
   variable Data

   .fieldcalc.expr.op insert insert $Op
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::Operand>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Appliquer le calcul MACRO a la liste de champs disponibles.
#
# Parametres :
#   <VP>     : Identificateur du viewport
#   <Fields> : Liste des champs disponibles
#   <Result> : Nom du champ resultant
#
# Retour:
#   <List>   : Liste des donnees
#
# Remarques :
#   -La fonction remplace les identificateur ASCII par les champs dans l'ordre
#   -Si il n'y a pas assez de champs, il y arura une erreur du Lexer.
#   -Si il y a trop de champs, ils seront pas inclus dans la macro mais retourne
#    dans la liste resultante
#
#----------------------------------------------------------------------------

proc FieldCalc::IsOperand { VP } {

   if { [info exists ::Viewport::Data(Operand$VP)] && [string trim $Viewport::Data(Operand$VP)]!="" } {
      return True
   }
   return False
}

proc FieldCalc::Operand { VP Fields { Result "" }} {
   global   GDefs
   variable Param
   variable Data
   variable Msg

   if { $Result=="" } {
      set Result CALC$VP
   }

   #----- Cleanup existing operand result
   if { [fstdfield is $Result] } {
      fstdfield free $Result
      FSTD::UnRegister $Result False
   }

   catch { .fieldcalc.expr.param configure -state disabled }
   
   #----- Re-enable all fields
   catch {
      foreach fld $Data(Fields$VP) {
         fstdfield configure $fld -active True        
      }
   }
   
   #----- If no operand available, do nothing
   if { ![FieldCalc::IsOperand $VP] } {
      return
   }

   set Data(Fields$VP) {}
   set fields {}
   set nout   0
   set vert   False
   set ids    { A B C D E F G H I J K L M N O P Q R S T U V W X Y Z }
   set nids   0

   #----- Separate the fields from the rest
   foreach fld $Fields {
      if { [fstdfield is $fld True] && [fstdfield define $fld -GRTYP]!="V" } {
         if { [fstdfield define $fld -NK]>1 } {
            set vert True
         }
         lappend fields $fld
      }
   }

   #---- No fields, just do calculus
   if { ![set lfield [llength $fields]] } {
      catch { set Data(Operand) [vexpr CALC$Data(FieldNo) $Viewport::Data(Operand$VP)] }
      return
   }

   #----- Replace the expression tokens with the fields
   set expr ""
   for { set i 0 } { $i < [string length $Viewport::Data(Operand$VP)] } { incr i } {
      set c [string index $Viewport::Data(Operand$VP) $i]
      set idx [lsearch -exact $ids $c]
      if { $idx>=0 } {
         incr nids
         if { $idx<$lfield } {
            set fld [lindex $fields $idx]
            if { $vert } {
               fstdfield readcube $fld
            }

            fstdfield configure $fld -active True
            lappend Data(Fields$VP) $fld
            append expr $fld
         } else {
            incr nout
         }
      } else {
         append expr $c
      }
   }

   if { !$nids } {
      catch { set Data(Operand) [vexpr CALC$Data(FieldNo) $Viewport::Data(Operand$VP)] }
      set Viewport::Data(Operand$VP) ""
      return
   }

   #----- Check that we have enough fields
   if { !$nout && $expr!="" } {
      set res [vexpr $Result $expr]
      if { ![fstdfield is $res True] } {
         Dialog::Info . $Msg(Operand) $res
      } else {
         FSTD::Register $res
         fstdfield stats $res -tag [fstdfield stats [lindex $fields 0] -tag]
         catch { .fieldcalc.expr.param configure -state normal }
         
         foreach fld $Data(Fields$VP) {
            fstdfield configure $fld -active $Param(ShowAll)         
         }
      }
   } else {
      return
   }

   return $Result
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::Paste>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer un champs dans l'expression.
#
# Parametres :
#   <File>   : Numero d'unite du fichier
#   <Field>  : Index du champs dans le fichier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::Paste { Text } {
   variable Data

   if { ! [winfo exists .fieldcalc] } {
      return
   }

   if { [string range $Text 0 4]=="field" } {
     .fieldcalc.expr.op insert insert "$Text"
     return copy
   } else {
     return refuse_drop
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::WidgetFunc>
# Creation : Octobre 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame des fonctions.
#
# Parametres :
#   <Frame>  : Frame parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::WidgetFunc { Frame } {
   global GDefs
   variable Lbl
   variable Data
   variable Bubble

   set Data(FuncType) [lindex [lindex $Lbl(Operators) 0] $GDefs(Lang)]

   frame  $Frame.type
      ComboBox::Create $Frame.type.sel FieldCalc::Data(FuncType) noedit unsorted nodouble -1 [lmap lbl $Lbl(Operators) {lindex $lbl $GDefs(Lang)}] 9 6 "FieldCalc::FuncList $Frame"
      entry $Frame.type.search -bg $GDefs(ColorLight) -textvariable FieldCalc::Param(Search) -width 10 -relief sunken -bd 1
      pack  $Frame.type.sel -side left -fill x -expand True
      pack  $Frame.type.search -side left
      pack  $Frame.type -side top -fill x -padx 5 -pady 2

   frame $Frame.funcs -padx 5 -pady 2
      listbox $Frame.funcs.list -relief sunken -bd 1 -exportselection false  -highlightthickness 0 \
         -yscrollcommand "$Frame.funcs.scroll set" -height 1 -width 1 -background $GDefs(ColorLight)
      scrollbar $Frame.funcs.scroll -command "$Frame.funcs.list yview" -bd 1 -width 10  -highlightthickness 0
      pack $Frame.funcs.list -side left -expand true -fill both
      pack $Frame.funcs.scroll -side left -fill y
   pack $Frame.funcs -side bottom -anchor w -expand true -fill both

   Bubble::Create $Frame.type.sel     $Bubble(FuncType)
   Bubble::Create $Frame.type.search  $Bubble(FuncSearch)
   Bubble::Create $Frame.funcs        $Bubble(FuncList)

   bind  $Frame.type.search <Any-KeyRelease>   "FieldCalc::FuncList $Frame"
   bind  $Frame.funcs.list  <B1-ButtonRelease> { FieldCalc::InsertFunc [%W get [%W nearest %y]]; FieldCalc::FormulaSet "" }

   FieldCalc::FuncList $Frame
}

proc FieldCalc::FuncList { Frame } {
   global GDefs
   variable Lbl
   variable Param
   variable Data

   $Frame.funcs.list delete 0 end

   set idx    [lsearch -index $GDefs(Lang) $Lbl(Operators) $Data(FuncType)]
   set arrays [lindex [lindex $Lbl(Operators) $idx] 1]

   if { $arrays=="All" } {
      set arrays [lmap lbl $Lbl(Operators) {lindex $lbl 1}]
   }
   foreach array $arrays {
      if { $Param(Search)!="" } {
         foreach func [lsort -unique [concat [lsearch -all -inline -index 0 -glob $Param($array) *$Param(Search)*] [lsearch -all -inline -index [expr $GDefs(Lang)+1] -glob $Param($array) *$Param(Search)*]]] {
            $Frame.funcs.list insert end [format "%-15s %s" [lindex $func 0] [lindex $func [expr $GDefs(Lang)+1]]]
         }
      } else {
         foreach func $Param($array) {
            $Frame.funcs.list insert end [format "%-15s %s" [lindex $func 0] [lindex $func [expr $GDefs(Lang)+1]]]
         }
      }
   }
   return True
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::WidgetConvert>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame des facteur de conversions.
#
# Parametres :
#   <Frame>  : Frame parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::WidgetConvert { Frame } {
   global GDefs
   variable Lbl
   variable Bubble

   ComboBox::Create $Frame.type FieldCalc::Data(ConvType) noedit unsorted nodouble -1 [lmap lbl $Lbl(Converts) {lindex $lbl $GDefs(Lang)}] 9 6 \
      "FieldCalc::ConvertList $Frame"
   pack  $Frame.type -side top -fill x -padx 5 -pady 2

   frame $Frame.conv
      ComboBox::Create $Frame.conv.from FieldCalc::Data(ConvFrom) noedit unsorted nodouble -1 "" 9 6 \
         "FieldCalc::ConvertFactor"
      label $Frame.conv.ind -text [lindex $Lbl(To) $GDefs(Lang)]
      ComboBox::Create $Frame.conv.to FieldCalc::Data(ConvTo) noedit unsorted nodouble -1 "" 8 6 \
         "FieldCalc::ConvertFactor"
      pack  $Frame.conv.from $Frame.conv.to -side left -fill x -expand true
      pack  $Frame.conv.ind -before $Frame.conv.to -side left
   button $Frame.factor -textvariable FieldCalc::Data(ConvFactor) -relief groove -bd 2 \
      -command "if { \$FieldCalc::Data(ConvFactor)!=\"\" } { FieldCalc::InsertOperator *\$FieldCalc::Data(ConvFactor) }"

   pack $Frame.conv -side top -fill x -padx 5 -anchor w
   pack $Frame.factor -side top -padx 5 -fill x -expand true

   Bubble::Create $Frame.type      $Bubble(ConvType)
   Bubble::Create $Frame.conv.from $Bubble(ConvFrom)
   Bubble::Create $Frame.conv.to   $Bubble(ConvTo)
   Bubble::Create $Frame.factor    $Bubble(ConvFact)
}

proc FieldCalc::ConvertList { Frame } {
   global GDefs
   variable Lbl
   variable Data
   variable Param

   set idx             [lsearch -index $GDefs(Lang) $Lbl(Converts) $Data(ConvType)]
   set Data(ConvArray) [lindex [lindex $Lbl(Converts) $idx] 1]
   set units           [lmap conv $Param($Data(ConvArray)) {lindex $conv 0}]

   ComboBox::DelAll $Frame.conv.from
   ComboBox::DelAll $Frame.conv.to

   ComboBox::AddList $Frame.conv.from $units
   ComboBox::AddList $Frame.conv.to   $units

   set Data(ConvFactor) ""
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::WidgetConst>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame des constantes.
#
# Parametres :
#   <Frame>  : Frame parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::WidgetConst { Frame } {
   variable Param

   frame $Frame.sunk -relief sunken -bd 1
      set n 0
      set c 0
      foreach const $Param(Const) {
         if { !$n } {
            incr c
            frame $Frame.sunk.l$c
            pack  $Frame.sunk.l$c -side top -fill y -anchor nw
        }
         set name [lindex $const 0]
         set val  [lindex $const 1]
         button $Frame.sunk.l$c.c$name -text [format "%-3s" $name] -bd 1 -command "FieldCalc::InsertDigit $val"
         Bubble::Create  $Frame.sunk.l$c.c$name [lrange $const 2 3]
         pack $Frame.sunk.l$c.c$name -side left -anchor nw

         if { [incr n]==6 } {
            set n 0
         }
      }
      pack  $Frame.sunk.l1 $Frame.sunk.l2 $Frame.sunk.l3 -side top -fill y
   pack $Frame.sunk -side top -pady 5 -padx 5 -anchor nw
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::WidgetOps>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame des operateurs.
#
# Parametres :
#   <Frame>  : Frame parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::WidgetOps { Frame } {
   variable Bubble

   frame $Frame.ops

   frame $Frame.ops.num
      button $Frame.ops.num.zero   -text "0" -command "FieldCalc::InsertDigit 0" -bd 1
      button $Frame.ops.num.one    -text "1" -command "FieldCalc::InsertDigit 1" -bd 1
      button $Frame.ops.num.two    -text "2" -command "FieldCalc::InsertDigit 2" -bd 1
      button $Frame.ops.num.three  -text "3" -command "FieldCalc::InsertDigit 3" -bd 1
      button $Frame.ops.num.four   -text "4" -command "FieldCalc::InsertDigit 4" -bd 1
      button $Frame.ops.num.five   -text "5" -command "FieldCalc::InsertDigit 5" -bd 1
      button $Frame.ops.num.six    -text "6" -command "FieldCalc::InsertDigit 6" -bd 1
      button $Frame.ops.num.seven  -text "7" -command "FieldCalc::InsertDigit 7" -bd 1
      button $Frame.ops.num.eight  -text "8" -command "FieldCalc::InsertDigit 8" -bd 1
      button $Frame.ops.num.nine   -text "9" -command "FieldCalc::InsertDigit 9" -bd 1
      button $Frame.ops.num.dot    -text "." -command "FieldCalc::InsertDigit ." -bd 1
      grid $Frame.ops.num.seven  -column 0 -row 0
      grid $Frame.ops.num.eight  -column 1 -row 0
      grid $Frame.ops.num.nine   -column 2 -row 0
      grid $Frame.ops.num.four   -column 0 -row 1
      grid $Frame.ops.num.five   -column 1 -row 1
      grid $Frame.ops.num.six    -column 2 -row 1
      grid $Frame.ops.num.one    -column 0 -row 2
      grid $Frame.ops.num.two    -column 1 -row 2
      grid $Frame.ops.num.three  -column 2 -row 2
      grid $Frame.ops.num.zero   -column 0 -row 3
      grid $Frame.ops.num.dot    -column 1 -row 3
   pack $Frame.ops.num -side left -padx 2

   frame $Frame.ops.op
      frame $Frame.ops.op.l
         button $Frame.ops.op.l.p -text "(" -command "FieldCalc::InsertOperator (" -bd 1
         button $Frame.ops.op.l.plus     -text "+" -command "FieldCalc::InsertOperator +" -bd 1
         button $Frame.ops.op.l.minus    -text "-" -command "FieldCalc::InsertOperator -" -bd 1
         pack $Frame.ops.op.l.p $Frame.ops.op.l.plus $Frame.ops.op.l.minus -side top
      frame $Frame.ops.op.r
         button $Frame.ops.op.r.p -text ")" -command "FieldCalc::InsertOperator )" -bd 1
         button $Frame.ops.op.r.mul -text "*" -command "FieldCalc::InsertOperator *" -bd 1
         button $Frame.ops.op.r.div   -text "/" -command "FieldCalc::InsertOperator /" -bd 1
         button $Frame.ops.op.r.pow   -text "^" -command "FieldCalc::InsertOperator ^" -bd 1
         pack $Frame.ops.op.r.p $Frame.ops.op.r.mul $Frame.ops.op.r.div $Frame.ops.op.r.pow -side top
      pack $Frame.ops.op.l $Frame.ops.op.r -side left -anchor n
   pack $Frame.ops.op -side left -anchor n -padx 2

   frame $Frame.ops.log
      frame $Frame.ops.log.a
         button $Frame.ops.log.a.lt -text "<" -command "FieldCalc::InsertOperator <" -bd 1
         button $Frame.ops.log.a.gt -text ">" -command "FieldCalc::InsertOperator >" -bd 1
         pack $Frame.ops.log.a.lt $Frame.ops.log.a.gt -side top
      frame $Frame.ops.log.b
         button $Frame.ops.log.b.le -text "<=" -command "FieldCalc::InsertOperator <=" -bd 1
         button $Frame.ops.log.b.ge -text ">=" -command "FieldCalc::InsertOperator >=" -bd 1
         button $Frame.ops.log.b.eq -text "==" -command "FieldCalc::InsertOperator ==" -bd 1
         button $Frame.ops.log.b.ne -text "!=" -command "FieldCalc::InsertOperator !=" -bd 1
         pack $Frame.ops.log.b.le $Frame.ops.log.b.ge $Frame.ops.log.b.eq $Frame.ops.log.b.ne -side top
      pack $Frame.ops.log.a $Frame.ops.log.b -side left -anchor n
   pack $Frame.ops.log -side left -anchor n -padx 2

   frame $Frame.ops.if
      button $Frame.ops.if.not -text "! " -command "FieldCalc::InsertOperator !" -bd 1
      button $Frame.ops.if.and -text "&&" -command "FieldCalc::InsertOperator &&" -bd 1
      button $Frame.ops.if.or  -text "||" -command "FieldCalc::InsertOperator ||" -bd 1
      pack $Frame.ops.if.not $Frame.ops.if.and $Frame.ops.if.or -side top
   pack $Frame.ops.if -side left -anchor n -padx 2

   pack $Frame.ops -side top -pady 2

   frame $Frame.cmd
      checkbutton $Frame.cmd.prime -text " # "  -bd 1 -variable FieldCalc::Param(ShowAll) -onvalue True -offvalue False -indicatoron False -relief raised -command { Viewport::UpdateData $Page::Data(Frame); Page::UpdateCommand $Page::Data(Frame) }
      button $Frame.cmd.equal -text "   =  "  -bd 1 -command { FieldCalc::Apply False }
      button $Frame.cmd.clear -text "C"  -bd 1 -bg red -command { FieldCalc::Apply True }
      button $Frame.cmd.int2  -text "<<" -command "FieldCalc::InsertOperator <<" -bd 1
      button $Frame.cmd.int3  -text "<<<" -command "FieldCalc::InsertOperator <<<" -bd 1
      button $Frame.cmd.idx   -text "\[\]" -command "FieldCalc::InsertOperator \\\[\\\]" -bd 1
      pack $Frame.cmd.clear $Frame.cmd.equal $Frame.cmd.prime -side left -fill y
      pack $Frame.cmd.idx $Frame.cmd.int3 $Frame.cmd.int2 -side right
   pack $Frame.cmd -side bottom -fill both  -padx 2 -pady 2

   Bubble::Create $Frame.ops.if.not $Bubble(NOT)
   Bubble::Create $Frame.ops.if.and $Bubble(AND)
   Bubble::Create $Frame.ops.if.or  $Bubble(OR)
   Bubble::Create $Frame.cmd.int2   $Bubble(Int2)
   Bubble::Create $Frame.cmd.int3   $Bubble(Int3)
   Bubble::Create $Frame.cmd.idx    $Bubble(Idx)
   Bubble::Create $Frame.cmd.prime  $Bubble(Prime)
   Bubble::Create $Frame.cmd.equal  $Bubble(Equal)
   Bubble::Create $Frame.cmd.clear  $Bubble(Clear)
}
