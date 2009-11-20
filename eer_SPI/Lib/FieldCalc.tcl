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
#   FieldCalc::Window         { Parent }
#   FieldCalc::FormulaLoad    { }
#   FieldCalc::FormulaSave    { Name }
#   FieldCalc::FormulaSet     { { Formula False } }
#   FieldCalc::InsertDigit    { Digit }
#   FieldCalc::InsertFunc     { Func Argc Trigo }
#   FieldCalc::InsertOperator { Op }
#   FieldCalc::Operand        { VP Id Fields }
#   FieldCalc::Paste          { File Field }
#   FieldCalc::Save           { File }
#   FieldCalc::WidgetCond     { Frame }
#   FieldCalc::WidgetConst    { Frame }
#   FieldCalc::WidgetConv     { Frame }
#   FieldCalc::WidgetMath     { Frame }
#   FieldCalc::WidgetMem      { Frame }
#   FieldCalc::WidgetOps      { Frame }
#   FieldCalc::WidgetStat     { Frame }
#   FieldCalc::WidgetTrigo    { Frame }
#
# Remarques :
#
#===============================================================================

package provide FieldCalc 2.1

catch { SPI::Splash "Loading Widget Package FieldCalc 2.1" }

namespace eval FieldCalc {
   variable Lbl
   variable Bubble
   variable Error
   variable Msg
   variable Data
   variable Const
   variable Conv
   variable Unit
   variable Desc
   variable Param

   set Param(Geom)     { 620x190+[winfo rootx $Parent]+[winfo rooty $Parent] }
   set Param(Title)    { "Calculatrice" "Calculator" }
   set Param(Version)  2.1

   set Data(FieldNo)  0
   set Data(Formulas) {}
   set Data(Mem)      {}

   #----- Textes et labels

   set Lbl(To)       { " en " " to " }
   set Lbl(Yes)      { "Oui" "Yes" }
   set Lbl(No)       { "Non" "No" }
   set Lbl(None)     { "Aucune" "None" }
   set Lbl(Mem)      { "Memoire" "Memory" }
   set Lbl(Conv)     { "Conversion" "Conversion" }
   set Lbl(Const)    { "Constantes" "Constant" }
   set Lbl(Trigo)    { "Trigonometrie" "Trigonometry" }
   set Lbl(Math)     { "Math" "Math" }
   set Lbl(Cond)     { "Condition" "Condition" }
   set Lbl(Stat)     { "Stat" "Stat" }

   #----- Erreurs

   set Error(Operand)  { "FieldCalc Operand : Champs resultants invalide" "FieldCalc Operand : Invalid result field" }

   #----- Messagess

   set Msg(Del)     { "Voulez-vous vraiment supprimer cette fonction ?" "Do you really want to suppress this function ?" }
   set Msg(Exist)   { "Cette fonction existe deja, voulez-vous la remplacer ?" "This function is already defined, do you want to replace it ?" }
   set Msg(Name)    { "Veuillez spéfifier le nom de la formule." "Please enter the formula name." }
   set Msg(Saved)   { "Formule sauvegardee." "Formula saved." }

   #----- Bulles d'aides

   set Bubble(Param)   { "Définir les paramêtres dur champs résultant" "Define the result field parameters" }
   set Bubble(Formula) { "Nom de la formule courante" "Current formula name" }
   set Bubble(Save)    { "Sauvegarde de la formule courante" "Save the current formula" }
   set Bubble(Del)     { "Suppression de la formule courante de la liste des formules sauvegardees" "Suppress the current formula from the saved list" }
   set Bubble(Info)    { "Mantisse\nTapper la formule ou utiliser les boutons de la calculatrice.\nPour inserer un champs,\
                          selectionne le dans une boite de champs avec le bouton central de la souris."
                         "Mantisse\nType in the formula or use the calculator buttons\nTo insert a field,\
                          click on it with the middle mouse button in a fieldbox." }
   set Bubble(List)  { "Liste des operations effectuees" "Past operations made" }
   set Bubble(Field) { "Sauvegarder le champs resultant" "Save the result field" }

   set Bubble(STO)   { "Memoriser le resultat" "Memorize the result" }
   set Bubble(RCL)   { "Rappeler le resultat memorise" "Recall memorized result" }
   set Bubble(DEL)   { "Supprimer le resultat memorise" "Delete memorized result" }
   set Bubble(LST)   { "Liste des resultats memorise" "Memorized result list" }

   set Bubble(ConvType)   { "Selection du type de conversion" "Select the conversion type" }
   set Bubble(ConvFrom)   { "Unite du format a convertir" "Unit to convert from" }
   set Bubble(ConvTo)     { "Unite de conversion" "Unit to convert to" }
   set Bubble(ConvFact)   { "Facteur de conversion\nAppuyer pour appliquer a la mantisse"
                           "Conversion factor\nPress to apply to mantissa" }

   set Bubble(CondNOT)    { "Negation logique" "Logical negation" }
   set Bubble(CondAND)    { "ET logique" "Logical AND" }
   set Bubble(CondOR)     { "OU logique" "Logical OR" }
   set Bubble(CondMIN)    { "min(A,B)\nMinimum entre A a B" "min(A,B)\nMinimum between A an B" }
   set Bubble(CondMAX)    { "min(A,B)\nMaximum entre A a B" "min(A,B)\nMaximum between A an B" }
   set Bubble(CondCLAMP)  { "clamp(A,B,C)\nFixe le minimum de A a B et le maximum a C" \
                            "clamp(A,B,C)\nSet the minimum of A to B and the maximum to C" }
   set Bubble(CondIFELSE) { "ifelse(A,B,C)\nSi A alors A=B sinon A=C" "ifelse(A,B,C)\nIf A then A=B else A=C" }

   set Bubble(MathLN)     { "ln(A)\nLogarithme naturel de A" "ln(A)\nNatural logarithm of A" }
   set Bubble(MathEXP)    { "exp(A)\nExponentielle de A" "exp(A)\nExponential of A" }
   set Bubble(MathLOG)    { "log(A)\nLogarithme en base 10 de A" "log(A)\nBase 10 logarithme of A" }
   set Bubble(MathSQRT)   { "sqrt(A)\nRacine carre de A" "sqrt(A)\nSquare root of A" }
   set Bubble(MathCBRT)   { "cbrt(A)\nRacine cubique de A" "cbrt(A)\nCubic root of A" }
   set Bubble(MathFMOD)   { "fmod(A,B)\nReste de la division de A par B" "fmod(A,B)\nRemainder of A divided by B" }
   set Bubble(MathCEIL)   { "ceil(A)\nArrondi vers le haut au plus proche entier" "ceil(A)\nRound upward to the nearest integer" }
   set Bubble(MathFLOOR)  { "floor(A)\nArrondi vers le bas au plus proche entier" "floor(A)\Round downward to the nearest integer" }
   set Bubble(MathROUND)  { "round(A)\nArrondi au plus proche entier" "round(A)Round to the nearest integer\n" }
   set Bubble(MathABS)    { "abs(A)\nValeur absolue de A" "abs(A)\nAbsolute value of A" }
   set Bubble(MathDIF)    { "dif(A,b)\nPourcentage de difference entre A et B" "dif(A,b)\nPercent difference between A and B" }

   set Bubble(StatSSUM)   { "ssum(A)\nSomme de la matrice A"   "ssum(A)\nSum the matrix A" }
   set Bubble(StatSMIN)   { "smin(A)\nMinimum de la matrice A" "smin(A)\nMinimum of the matrix A" }
   set Bubble(StatSMAX)   { "smax(A)\nMaximum de la matrice A" "smax(A)\nMaximum of the matrix A" }
   set Bubble(StatSAVG)   { "savg(A)\nMoyenne de la matrice A" "savg(A)\nAverage of the matrix A" }
   set Bubble(StatSNB)    { "snbg(A)\nNombre d'item de la matrice A" "snbg(A)\nNumber of item of the matrix A" }
   set Bubble(StatSVAR)   { "svar(A)\nVariance de la matrice A" "svar(A)\nVariance of the matrix A" }
   set Bubble(StatSCOR)   { "scor(A,B)\nCorrelation entre la matrice A et B" "scor(A,B)\nCorrelation between matrix A and B" }
   set Bubble(StatSCOV)   { "scov(A,B)\nCovariance entre la matrice A et B" "scov(A,B)\nCovariance between matrix A and B" }
   set Bubble(StatSRMSE)  { "srms(A,B)\nRMS entre la matrice A et B" "srms(A,B)\nRoot Mean Square between matrix A and B" }
   set Bubble(StatSREGA)  { "srega(A,B)\nCoefficient de regression a entre la matrice A et B" "srega(A,B)\nRegression coefficient a between matrix A and B" }
   set Bubble(StatSREGB)  { "sregb(A,B)\nCoefficient de regression b entre la matrice A et B" "sregb(A,B)\nRegression coefficient b between matrix A and B" }
   set Bubble(StatSERRA)  { "serra(A,B)\nErreur standard a entre la matrice A et B" "serra(A,B)\nStandard error a between matrix A and B" }
   set Bubble(StatSERRB)  { "serrb(A,B)\nErreur standard b entre la matrice A et B" "serrb(A,B)\nStandard error b between matrix A and B" }
   set Bubble(StatSMB)    { "smb(A,B)\nBiais moyen entre la matrice A et B" "smb(A,B)\nMean bias between matrix A and B" }
   set Bubble(StatSNMB)   { "snmb(A,B)\nBiais moyen normalise entre la matrice A et B" "smb(A,B)\nNormalized mean bias between matrix A and B" }
   set Bubble(StatSNME)   { "snme(A,B)\nErreur moyenne normalisee entre la matrice A et B" "smb(A,B)\nNormalized mean error between matrix A and B" }
   set Bubble(StatSME)    { "sme(A,B)\n Erreur moyenne entre la matrice A et B" "sme(A,B)\nMean error between matrix A and B" }
   set Bubble(StatSNME)   { "snme(A,B)\nErreur moyenne normalise entre la matrice A et B" "snme(A,B)\nNormalized mean error  between matrix A and B" }
   set Bubble(StatSMNB)   { "smnb(A,B)\nBiais moyen normalise entre la matrice A et B " "smnb(A,B)\nMean normalized bias  between matrix A and B" }
   set Bubble(StatSMNE)   { "smne(A,B)\nErreur moyenne normalise entre la matrice A et B" "smne(A,B)\nMean normalized error between matrix A and B" }
   set Bubble(StatSLMNE)  { "slmne(A,B)\nErreur moyenne normalise logarithmique entre la matrice A et B" "slmne(A,B)\nLogarithmic mean normalized error between matrix A and B" }
   set Bubble(StatSLMNB)  { "slmnb(A,B)\nBiais moyen normalise logarithmique entre la matrice A et B" "slmnb(A,B)\nLogarithmic mean normalized bias between matrix A and B" }
   set Bubble(StatSMFB)   { "smfb(A,B)\nBiais moyen fractionnaire entre la matrice A et B" "smfb(A,B)\nMean fractional bias between matrix A and B" }
   set Bubble(StatSMFE)   { "smfe(A,B)\nErreur moyenne fractionnaire entre la matrice A et B" "smfe(A,B)\nMean fractional error  between matrix A and B" }
   set Bubble(StatSNRMSE) { "snrmse(A,B)\nRMS normalise entre la matrice A et B" "snrmse(A,B)\nNormalized Root mean square error between matrix A and B" }

   set Bubble(TrigoARC)   { "Applique la fonction inverse" "Appy inverse function" }
   set Bubble(TrigoHYP)   { "Applique la fonction hyperbolique" "Apply hyperbolic function" }
   set Bubble(TrigoSIN)   { "sin(A)\nSinus the A en radian" "sin(A)\nSinus of A in radian" }
   set Bubble(TrigoCOS)   { "cos(A)\nCosinus the A en radian" "cos(A)\nCosinus of A in radian" }
   set Bubble(TrigoTAN)   { "tan(A)\nTangente the A en radian" "tan(A)\nTangent of A in radian" }

   set Bubble(Int2)       { "Interpolation lineaire" "Linear interpolation" }
   set Bubble(Int3)       { "Interpolation cubique"  "cubic interpolation" }
   set Bubble(Idx)        { "Indexation vectorielle ou norme du vecteur"  "Vectorial indexation or vector length" }

   #----- Definitions de constantes

   set Const(Pi)  3.14159265358979323846264338327
   set Const(Re)  6.37e6
   set Const(g)   9.81
   set Const(W)   7.292e-5
   set Const(c)   2.998e8
   set Const(Rx)  8.3143e3
   set Const(k)   1.381e-23
   set Const(Na)  6.022e23
   set Const(S)   5.6696e-8
   set Const(h)   6.6262e-34
   set Const(e0)  8.85e-12
   set Const(Md)  28.97
   set Const(Rd)  287
   set Const(Rv)  461
   set Const(rd)  1.275
   set Const(rw)  1.0e3
   set Const(ri)  0.917e3
   set Const(cpd) 1004
   set Const(cvd) 717
   set Const(cpv) 1952
   set Const(cvv) 1463
   set Const(cw)  4218
   set Const(ci)  2106

   set Desc(Pi)  { "Pi" "Pi" }
   set Desc(Re)  { "Rayon moyen de la Terre (m)" "Average radius of Earth (m)" }
   set Desc(g)   { "Gravite moyenne de la Terre (m s-2)" "Mean gravity of Earth (m s-2)" }
   set Desc(W)   { "Velocitee angulaire de la Terre (rad s-1)" "Angular velocity of Earth (rad s-1)" }
   set Desc(c)   { "Vitesse de la lumiere (m s-1)" "Velocity of light (m s-1)" }
   set Desc(Rx)  { "Constante des gaz universel (J K-1 mol-1)" "Universal gas constant (J K-1 mol-1)" }
   set Desc(k)   { "Constante de Boltzman (J K-1 mol-1)" "Boltzmann`s constant (J K-1 mol-1)" }
   set Desc(Na)  { "Nombre d'Avogadro (mol-1)" "Avogadro number (mol-1)" }
   set Desc(S)   { "Constante de Stefan-Boltzman (W m-2 K-4)" "Stefan-Boltzman constant (W m-2 K-4)" }
   set Desc(h)   { "Constante de Planck (J s)" "Planck constant (J s)" }
   set Desc(e0)  { "Permissivitee du vacuum (C2 N-1 m2)" "Permittivity of vacuum (C2 N-1 m2)" }
   set Desc(Md)  { "Poid moyen de l'air sec" "Average molecule weight of dry air" }
   set Desc(Rd)  { "Constante des gas de l'air sec (J K-1 kg-1)" "Gas constant of dry air (J K-1 kg-1)" }
   set Desc(Rv)  { "Constante des gaz de la vapeur d'eau (J K-1 kg-1)" "Gas constant of water vapor (J K-1 kg-1)" }
   set Desc(rd)  { "Densite de l'air sec a 0C et 1000mb (kg m3)" "Density of dry air at 0C and 1000mb (kg m3)" }
   set Desc(rw)  { "Densite de l'eau liquide a 0C (kg m3)" "Density of liquid water at 0C (kg m3)" }
   set Desc(ri)  { "Densite de la glace a 0C (kg m3)" "Density of ice at 0C (kg m3)" }
   set Desc(cpd) { "Chaleur specifique de l'air sec a pression constante (J K-1 kg-1)" "Specific dry air heat at constant pressure (J K-1 kg-1)" }
   set Desc(cvd) { "Chaleur specifique de l'air a volume constant volume (J K-1 kg-1)" "Specific dry air heat at constant volume (J K-1 kg-1)" }
   set Desc(cpv) { "Chaleur specifique de la vapeur d'eau a pression constante (J K-1 kg-1)" "Specific water vapor heat at constant pressure (J K-1 kg-1)" }
   set Desc(cvv) { "Chaleur specifique de la vapeur d'eau a volume constant(J K-1 kg-1)" "Specific water vapor heat at constant volume (J K-1 kg-1)" }
   set Desc(cw)  { "Chaleur specifique de la vapeur d'eau a 0c (J K-1 kg-1)" "Specific water vapor heat at 0c (J K-1 kg-1)" }
   set Desc(ci)  { "Chaleur specifique de la glace a 0c (J K-1 kg-1)" "Specific ice heat at 0c (J K-1 kg-1)" }

   #----- Definitions de facteur de conversion

   set Unit(From)  ""
   set Unit(To)    ""

   set Conv(Type)  ""
   set Conv(Types) { "Distance Region Volume Masse Vitesse Pression Energie Puissance Force Radiation" \
                     "Distance Area Volume Mass Speed Pressure Energy Power Force Radiation" }

   set Conv(0) { in m km ft yd nm au pc ly fg fa ch cb mile A }
   set Conv(1) { in2 m2 ft2 yd2 rood acre mile2 hc hi }
   set Conv(2) { m3 in3 ft3 yd3 mile3 min dram liqoz gi liqpt liqqt gal drypt dryqt pk bu bbl drybbl }
   set Conv(3) { dr oz lb shcwt shtn kg ca gr }
   set Conv(4) { kmh mph knot ms Mach Light }
   set Conv(5) { Pa lbf/ft2 lbf/in2 mmHg inH2O inHg atm torr bar }
   set Conv(6) { J eV ftlbf cal kgfm Btu Wh kWh therm erg }
   set Conv(7) { W hp hpm }
   set Conv(8) { dyn N lbf kgf sn pdl }
   set Conv(9) { Bq Ci }

   #--- Distance

   set Unit(in)    { "pouce" "inch" }
   set Unit(m)     { "metre" "meter" }
   set Unit(A)     { "angstrom" "angstrom" }
   set Unit(km)    { "kilometre" "kilometer" }
   set Unit(ft)    { "pied" "foot" }
   set Unit(yd)    { "verge" "yard" }
   set Unit(nm)    { "mile nautique" "nautical mile" }
   set Unit(au)    { "unite astronomique" "astronomical unit" }
   set Unit(pc)    { "parsec" "parsec" }
   set Unit(ly)    { "annee lumiere" "lightyear" }
   set Unit(fg)    { "furlong" "furlong" }
   set Unit(fa)    { "brasse" "fathom" }
   set Unit(ch)    { "chaine" "chain" }
   set Unit(mile)  { "mile" "mile" }
   set Unit(cb)    { "cable" "cable" }

   set Conv(m)    1.0
   set Conv(A)    1E-10
   set Conv(km)   1000.0
   set Conv(in)   0.0254
   set Conv(ft)   0.3048
   set Conv(yd)   0.9144
   set Conv(fa)   1.8288
   set Conv(ch)   20.1168
   set Conv(fg)   201.168
   set Conv(mile) 1609.344
   set Conv(nm)   1852
   set Conv(au)   1.49597870E11
   set Conv(ly)   9.4607E15
   set Conv(pc)   3.0857E16
   set Conv(cb)   182.88

   #----- Area

   set Unit(m2)    { "metre carre" "square meter" }
   set Unit(hc)    { "hectare" "hectare" }
   set Unit(hi)    { "hide" "hide" }
   set Unit(in2)   { "pouce carre" "square inch" }
   set Unit(ft2)   { "pied carre" "square foot" }
   set Unit(yd2)   { "verge carre" "square yard" }
   set Unit(rood)  { "rood" "rood" }
   set Unit(acre)  { "acre" "acre" }
   set Unit(mile2) { "mile carre" "square mile" }

   set Conv(m2)    1.0
   set Conv(hc)    10000.0
   set Conv(hi)    485000.0
   set Conv(in2)   6.4516E-4
   set Conv(ft2)   9.290304E-2
   set Conv(yd2)   8.3612736E-1
   set Conv(rood)  1.01171E3
   set Conv(acre)  4.0468564224E3
   set Conv(mile2) 2.58998811E6

   #----- Volume

   set Unit(m3)     { "metre cube" "cubic meter" }
   set Unit(in3)    { "pouce cube" "cubic inch" }
   set Unit(ft3)    { "pied cube" "cubic foot" }
   set Unit(yd3)    { "verge cube" "cubic yard" }
   set Unit(mile3)  { "mile cube" "cubic mile" }
   set Unit(min)    { "minim" "minim" }
   set Unit(dram)   { "dram liquide" "fluid dram" }
   set Unit(liqoz)  { "once liquide" "liquid ounce" }
   set Unit(gi)     { "gill " "gill" }
   set Unit(liqpt)  { "pinte liquide" "liquid pint" }
   set Unit(liqqt)  { "quart liquide" "liquid quart" }
   set Unit(gal)    { "gallon" "gallon" }
   set Unit(drypt)  { "pinte seche" "dry pint" }
   set Unit(dryqt)  { "quart sec" "dray quart" }
   set Unit(pk)     { "peck" "peck" }
   set Unit(bu)     { "bushel" "bushel" }
   set Unit(bbl)    { "baril" "barrel" }
   set Unit(drybbl) { "baril sec" "dry barrel" }

   set Conv(m3)     1.0
   set Conv(in3)    1.6387064E-5
   set Conv(ft3)    2.8316846E-2
   set Conv(yd3)    7.64554858E-1
   set Conv(mile3)  4.168181825E9
   set Conv(min)    6.16115E-8
   set Conv(dram)   3.69669E-6
   set Conv(oz)     2.95735E-5
   set Conv(gi)     1.18294E-4
   set Conv(liqpt)  4.73176E-4
   set Conv(liqqt)  9.46353E-4
   set Conv(gal)    3.785411784E-3
   set Conv(bbl)    1.58987E-1
   set Conv(drypt)  5.50610E-4
   set Conv(dryqt)  1.10122E-3
   set Conv(drybbl) 1.15627E-1
   set Conv(pk)     8.80977E-3
   set Conv(bu)     3.52391E-2

   #----- Mass

   set Unit(dr)    { "dram" "dram" }
   set Unit(oz)    { "once" "ounce" }
   set Unit(lb)    { "livre" "pound" }
   set Unit(shcwt) { "centieme" "short hundredweight" }
   set Unit(shtn)  { "tonne" "short ton" }
   set Unit(kg)    { "kilogramme" "kilogram" }
   set Unit(ca)    { "carat" "carat" }
   set Unit(gr)    { "grain" "grain" }

   set Conv(kg)    1.0
   set Conv(ca)    0.0002
   set Conv(gr)    0.000065
   set Conv(dr)    1.77185E-3
   set Conv(oz)    2.83495E-2
   set Conv(lb)    0.45359237
   set Conv(shcwt) 4.53592E1
   set Conv(shtn)  9.07185E2

   #----- Speed

   set Unit(kmh)   { "kilometre heure" "kilometer per hour" }
   set Unit(mph)   { "mile a l'heure" "mile per hour" }
   set Unit(knot)  { "noeud" "knot" }
   set Unit(ms)    { "metre par seconde" "meter second" }
   set Unit(Mach)  { "Vitesse du son" "Sound speed" }
   set Unit(Light) { "Vitesse de la lumiere" "Light speed" }

   set Conv(ms)    1.0
   set Conv(kmh)   2.77778E-1
   set Conv(mph)   4.4704E-1
   set Conv(knot)  5.14444E-1
   set Conv(Mach)  340.2933
   set Conv(Light) 299790000

   #----- Pressure

   set Unit(Pa)      { "pascal" "pascal" }
   set Unit(lbf/ft2) { "livre-force par pied carre" "pound-force per square foot" }
   set Unit(lbf/in2) { "livre-force par pouce carre" "pound-force per square inch" }
   set Unit(mmHg)    { "millimetre de mercure" "millimeter of mercury" }
   set Unit(inH2O)   { "pouce d'eau" "inch of water" }
   set Unit(inHg)    { "pouce de mercure" "inch of mercury" }
   set Unit(atm)     { "atmosphere standard" "standard atmosphere" }
   set Unit(torr)    { "torr" "torr" }
   set Unit(bar)     { "bar" "bar" }

   set Conv(Pa)      1.0
   set Conv(lbf/ft2) 4.78803E1
   set Conv(lbf/in2) 6.89476E3
   set Conv(mmHg)    1.33322E2
   set Conv(torr)    1.33322E2
   set Conv(inH2O)   2.49089E2
   set Conv(inHg)    3.38639E3
   set Conv(bar)     1E5
   set Conv(atm)     1.01325E5

   #----- Energy

   set Unit(J)     { "joule" "joule" }
   set Unit(eV)    { "electronvolt" "electronvolt" }
   set Unit(ftlbf) { "foot pound force" "foot pound force" }
   set Unit(cal)   { "calorie" "calorie" }
   set Unit(kgfm)  { "kilogram force meter" "kilogram force meter" }
   set Unit(Btu)   { "British Thermal Unit" "British Thermal Unit" }
   set Unit(Wh)    { "watt heure" "watt hour" }
   set Unit(kWh)   { "kilowatt heure" "kilowatt hour" }
   set Unit(therm) { "therm" "therm" }
   set Unit(erg)   { "erg" "erg" }

   set Conv(J)     1.0
   set Conv(eV)    1.6021892E-19
   set Conv(erg)   1E-7
   set Conv(ftlbf) 1.35582
   set Conv(cal)   4.1868
   set Conv(kgfm)  9.80665
   set Conv(Btu)   1.05506E3
   set Conv(Wh)    3.6E3
   set Conv(kWh)   3.6E6
   set Conv(therm) 1.05506E8

   #----- Power

   set Unit(W)   { "watt" "watt" }
   set Unit(hp)  { "horsepower" "horsepower" }
   set Unit(hpm) { "horsepower metrique" "horsepower metric" }

   set Conv(W)   1.0
   set Conv(hpm) 7.35499E2
   set Conv(hp)  7.45700E2

   #----- Force

   set Unit(dyn) { "dyne" "dyne" }
   set Unit(N)   { "newton" "newton" }
   set Unit(lbf) { "livre force" "pound force" }
   set Unit(kgf) { "kilogramme force" "kilogram force" }
   set Unit(sn)  { "sthene" "sthene" }
   set Unit(pdl) { "poundal" "poundal" }

   set Conv(N)   1.0
   set Conv(dyn) 1E-5
   set Conv(pdl) 1.38255E-1
   set Conv(lbf) 4.44822
   set Conv(kgf) 9.80665
   set Conv(sn)  1E3

   #----- Radiation

   set Unit(Bq)      { "becquerel" "becquerel" }
   set Unit(Ci)      { "curie" "curie" }
   set Unit(Sv)      { "sievert" "sievert" }
   set Unit(rem)     { "Rontgen Equivalent Man" "Rontgen Equivalent Man" }
   set Unit(C/kg)    { "coulomb par kilogramme" "coulomb per kilogram" }
   set Unit(rontgen) { "rontgen" "rontgen" }

   set Conv(Bq)      1.0
   set Conv(Ci)      3.7E10

   set Conv(rem)     1.0
   set Conv(Sv)      1E-2

   set Conv(rontgen) 1.0
   set Conv(C/kg)    2.58E-4
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
   variable Conv

   if { $Conv(From)!= "" && $Conv(To)!= "" } {
      set Conv(Factor) [expr $Conv($Conv(From)) * (1.0/$Conv($Conv(To)))]
   } else {
      set Conv(Factor) ""
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::ConvertList>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer les convertisseur dans les liste.
#
# Parametres :
#  <Frame>   : Frame contenant les listes de convertisseur
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::ConvertList { Frame } {
   global GDefs
   variable Conv

   set list [lindex $Conv(Types) $GDefs(Lang)]
   set idx  [lsearch -exact $list $Conv(Type)]

   ComboBox::DelAll $Frame.conv.from
   ComboBox::DelAll $Frame.conv.to

   ComboBox::AddList $Frame.conv.from $Conv($idx)
   ComboBox::AddList $Frame.conv.to   $Conv($idx)

   set Conv(Factor) ""
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
      button .fieldcalc.expr.save -image FOLDIN -relief flat -state disabled -overrelief raised -bd 1 \
         -command { if { [fstdfield is CALC$Viewport::Data(VP)] } { FieldCalc::Save [FileBox::Create .fieldcalc "" Save [list $FileBox::Type(FSTD)]] } }
      button .fieldcalc.expr.param -image INFOLOG -relief flat -state disabled -overrelief raised -bd 1 \
         -command { if { [fstdfield is CALC$Viewport::Data(VP)] } { FieldParams::Window CALC$Viewport::Data(VP) } }
      ComboBox::Create .fieldcalc.expr.sel FieldCalc::Data(Formula) edit unsorted nodouble -1 $FieldCalc::Data(Formulas) 15 3 { FieldCalc::FormulaSet True }
      button .fieldcalc.expr.fsave -image CALCSAVE -relief flat -overrelief raised -bd 1 -command { FieldCalc::FormulaSave [Dialog::Get . $FieldCalc::Bubble(Save) $FieldCalc::Msg(Name)]}
      button .fieldcalc.expr.fdel  -image CALCDEL -relief flat -overrelief raised -bd 1 -command FieldCalc::FormulaDel
      pack .fieldcalc.expr.op -side left -fill both -expand true
      pack .fieldcalc.expr.param .fieldcalc.expr.save -side left
      pack .fieldcalc.expr.sel -side left -fill both
      pack .fieldcalc.expr.fsave .fieldcalc.expr.fdel -side left -fill both
   pack .fieldcalc.expr -side top -anchor e -padx 2 -pady 2 -fill x

   Bubble::Create .fieldcalc.expr.sel   $Bubble(Formula)
   Bubble::Create .fieldcalc.expr.save  $Bubble(Field)
   Bubble::Create .fieldcalc.expr.param $Bubble(Param)
   Bubble::Create .fieldcalc.expr.fsave $Bubble(Save)
   Bubble::Create .fieldcalc.expr.fdel  $Bubble(Del)

   #----- Creation des fonctions

   TabFrame::Create .fieldcalc.func 2 ""
   pack .fieldcalc.func -side right -fill both -expand true -padx 2 -pady 2

   FieldCalc::WidgetMem    [TabFrame::Add .fieldcalc.func 1 [lindex $Lbl(Mem)   $GDefs(Lang)] False ""]
   FieldCalc::WidgetConv   [TabFrame::Add .fieldcalc.func 1 [lindex $Lbl(Conv)  $GDefs(Lang)] False ""]
   FieldCalc::WidgetConst  [TabFrame::Add .fieldcalc.func 1 [lindex $Lbl(Const) $GDefs(Lang)] False ""]
   FieldCalc::WidgetTrigo  [TabFrame::Add .fieldcalc.func 2 [lindex $Lbl(Trigo) $GDefs(Lang)] False ""]
   FieldCalc::WidgetMath   [TabFrame::Add .fieldcalc.func 2 [lindex $Lbl(Math)  $GDefs(Lang)] False ""]
   FieldCalc::WidgetStat   [TabFrame::Add .fieldcalc.func 2 [lindex $Lbl(Stat)  $GDefs(Lang)] False ""]
   FieldCalc::WidgetCond   [TabFrame::Add .fieldcalc.func 2 [lindex $Lbl(Cond)  $GDefs(Lang)] False ""]
   FieldCalc::WidgetOps    .fieldcalc

   bind .fieldcalc.expr.op <KeyRelease> { set FieldCalc::Data(Formula) ""; FieldCalc::FormulaSet }
   bind .fieldcalc.expr.op <Return>     { Viewport::UpdateData $Page::Data(Frame); Page::UpdateCommand $Page::Data(Frame) }

   wm resizable .fieldcalc True False

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
   global GDefs
   variable Data
   variable Lbl
   variable Msg

   if { $Data(Formula)!="" && $Data(Formula)!=[lindex $Lbl(None) $GDefs(Lang)] && [set idx [lsearch -exact $Data(Formulas) $Data(Formula)]]!=-1  } {

      set del [Dialog::Default . 200 WARNING $Msg(Del) "" 0 $Lbl(No) $Lbl(Yes)]

      if { $del } {

         ComboBox::Del .fieldcalc.expr.sel $Data(Formula)
         set Data(Formulas) [lreplace $Data(Formulas) $idx $idx]
         set Data(Formula) ""

         file rename -force $GDefs(DirEER)/FieldCalc $GDefs(DirEER)/FieldCalc.old

         set f [open $GDefs(DirEER)/FieldCalc w]

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
   global GDefs
   variable Data
   variable Lbl

   set Data(Formulas) [lindex $Lbl(None) $GDefs(Lang)]
   set Data(Formula[lindex $Lbl(None) $GDefs(Lang)]) ""

   if { ![file exist $GDefs(DirEER)/FieldCalc] } {
      return
   }

   set f [open $GDefs(DirEER)/FieldCalc r]

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
   global GDefs
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

   if { [file exist $GDefs(DirEER)/FieldCalc] } {
      file rename -force $GDefs(DirEER)/FieldCalc $GDefs(DirEER)/FieldCalc.old
   }

   set f [open $GDefs(DirEER)/FieldCalc w]

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
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::FormulaSet { { Formula False } } {
   variable Data

   catch {
      if { $Formula } {
         set Data(Operand) $Data(Formula$Data(Formula))
      }

      set Viewport::Data(Operand$Viewport::Data(VP)) $Data(Operand)
      set Viewport::Data(Formula$Viewport::Data(VP)) $Data(Formula)
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
#   <Trigo>  : La fonction est-elle trigonometrique ?
#
# Retour:
#
# Remarques :
#   -Si une selection existe, on applique la fonction a la selection sinon
#    on insere la fonctiona la position du curseur.
#
#----------------------------------------------------------------------------

proc FieldCalc::InsertFunc { Func Argc Trigo } {
   variable Data

   #----- Dans le cas trigo, on verifie les inverses et hyperboliques

   if { $Trigo } {
      if { $Data(Arc) } {
         set Func "a$Func"
      }
      if { $Data(Hyp) } {
         set Func "${Func}h"
      }
   }

   #----- Si il y a selection, s'en servir pour le premier argument de la fonction

   if { [.fieldcalc.expr.op selection present] } {
      .fieldcalc.expr.op insert sel.first "${Func}("

      set virg ""

      while { [incr Argc -1] } {
         append virg ","
      }
      .fieldcalc.expr.op insert sel.last "${virg})"
      .fieldcalc.expr.op icursor [expr [.fieldcalc.expr.op index sel.last] +1]

  } else {

      set virg "${Func}("
      set idx $Argc

      while { [incr Argc -1] } {
         append virg ","
      }

      .fieldcalc.expr.op insert inser "${virg})"
      .fieldcalc.expr.op icursor [expr [.fieldcalc.expr.op index insert] -$idx]
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

proc FieldCalc::Operand { VP Fields } {
   global   GDefs
   variable Error
   variable Data

   #----- Cleanup existing operand result
   if { [fstdfield is CALC$VP] } {
      fstdfield free CALC$VP
      FSTD::UnRegister CALC$VP False
   }

   catch { .fieldcalc.expr.save configure -state disabled; .fieldcalc.expr.param configure -state disabled }

   #----- If no operand vailable, do nothing
   if { [string trim $Viewport::Data(Operand$VP)]=="" } {
      return $Fields
   }

   set data   ""
   set others {}
   set fields {}
   set nfield 0
   set nout   0
   set vert   False
   set ids    { A B C D E F G H I J K L M N O P Q R S T U V W X Y Z }
   set nids   0

   #----- Separate the fields from the rest
   foreach fld $Fields {
      if { [fstdfield is $fld] && [fstdfield define $fld -GRTYP]!="V" } {
         if { [fstdfield define $fld -NK]>1 } {
            set vert True
         }
         lappend fields $fld
      } else {
         lappend others $fld
      }
   }
   if { ![set lfield [llength $fields]] } {
      catch { set Data(Operand) [vexpr CALC$Data(FieldNo) $Viewport::Data(Operand$VP)] }
      return $Fields
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
            append expr $fld
            incr nfield
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
      return $Fields
   }

   #----- Check that we have enough fields

   if { !$nout && $expr!="" } {
      set res [vexpr CALC$VP $expr]
      if { ![fstdfield is $res] } {
         Dialog::Error . $Error(Operand) "\n\n$res" $GDefs(Lang)
         set data $Fields
      } else {
         FSTD::Register $res
         lappend data $res
         fstdfield stats $res -tag [fstdfield stats [lindex $fields 0] -tag]
         catch { .fieldcalc.expr.save configure -state normal; .fieldcalc.expr.param configure -state normal }
      }
   } else {
       return $Fields
   }
   if { $nfield<[llength $fields] } {
      set data [concat $data [lrange $fields $nfield end]]
   }

   return [concat $data $others]
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

proc FieldCalc::Paste { File Field } {
   variable Data

   if { ! [winfo exists .fieldcalc] } {
      return
   }

   if { !$File } {
     .fieldcalc.expr.op insert insert "$Field"
   } else {
     .fieldcalc.expr.op insert insert "field($File,$Field)"
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::Save>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder le champs resultat.
#
# Parametres :
#   <File>   : Fichier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::Save { File } {
   variable Data

   if { $File=="" } {
      return
   }

   FieldParams::Window CALC$Viewport::Data(VP)

   fstdfile open CALCFILE write $File
   fstdfield write CALC$Viewport::Data(VP) CALCFILE 0 True
   fstdfile close CALCFILE
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::WidgetCond>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame des fonction conditionnelle.
#
# Parametres :
#   <Frame>  : Frame parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::WidgetCond { Frame } {
   variable Bubble

   frame $Frame.sunk -relief sunken -bd 1
      frame $Frame.sunk.l1
         button $Frame.sunk.l1.min   -text " min " -command "FieldCalc::InsertFunc min  2 0" -bd 1
         button $Frame.sunk.l1.max   -text " max " -command "FieldCalc::InsertFunc max  2 0" -bd 1
         button $Frame.sunk.l1.clamp -text "clamp" -command "FieldCalc::InsertFunc clamp  3 0" -bd 1
         pack $Frame.sunk.l1.min $Frame.sunk.l1.max $Frame.sunk.l1.clamp -side top
      frame $Frame.sunk.l2
         button $Frame.sunk.l2.if -text "ifelse" -command "FieldCalc::InsertFunc ifelse  3 0" -bd 1
         pack $Frame.sunk.l2.if -side top -anchor n
      pack  $Frame.sunk.l1 $Frame.sunk.l2  -side left -fill y

   frame $Frame.ops -relief sunken -bd 1
      frame $Frame.ops.if
         button $Frame.ops.if.not -text "! " -command "FieldCalc::InsertOperator !" -bd 1
         button $Frame.ops.if.and -text "&&" -command "FieldCalc::InsertOperator &&" -bd 1
         button $Frame.ops.if.or  -text "||" -command "FieldCalc::InsertOperator !!" -bd 1
         pack $Frame.ops.if.not $Frame.ops.if.and $Frame.ops.if.or -side top
      pack $Frame.ops.if -side left -fill y

   pack $Frame.ops $Frame.sunk -side left -pady 10 -padx 5 -anchor nw

   Bubble::Create $Frame.sunk.l1.min   $Bubble(CondMIN)
   Bubble::Create $Frame.sunk.l1.max   $Bubble(CondMAX)
   Bubble::Create $Frame.sunk.l1.clamp $Bubble(CondCLAMP)
   Bubble::Create $Frame.sunk.l2.if    $Bubble(CondIFELSE)
   Bubble::Create $Frame.ops.if.not    $Bubble(CondNOT)
   Bubble::Create $Frame.ops.if.and    $Bubble(CondAND)
   Bubble::Create $Frame.ops.if.or     $Bubble(CondOR)
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
   variable Const
   variable Desc

   frame $Frame.sunk -relief sunken -bd 1
      frame $Frame.sunk.l1
         button $Frame.sunk.l1.pi  -text "pi" -bd 1 -command "FieldCalc::InsertDigit $Const(Pi)"
         button $Frame.sunk.l1.re  -text "Re" -bd 1 -command "FieldCalc::InsertDigit $Const(Re)"
         button $Frame.sunk.l1.g   -text "g " -bd 1 -command "FieldCalc::InsertDigit $Const(g)"
         button $Frame.sunk.l1.w   -text "W " -bd 1 -command "FieldCalc::InsertDigit $Const(W)"
         button $Frame.sunk.l1.c   -text "c " -bd 1 -command "FieldCalc::InsertDigit $Const(c)"
         button $Frame.sunk.l1.md   -text "Md " -bd 1 -command "FieldCalc::InsertDigit $Const(Md)"
         Bubble::Create $Frame.sunk.l1.pi $Desc(Pi)
         Bubble::Create $Frame.sunk.l1.re $Desc(Re)
         Bubble::Create $Frame.sunk.l1.g  $Desc(g)
         Bubble::Create $Frame.sunk.l1.w  $Desc(W)
         Bubble::Create $Frame.sunk.l1.c  $Desc(c)
         Bubble::Create $Frame.sunk.l1.md $Desc(Md)
      pack $Frame.sunk.l1.pi $Frame.sunk.l1.re $Frame.sunk.l1.g $Frame.sunk.l1.w $Frame.sunk.l1.c $Frame.sunk.l1.md -side left -anchor n
      frame $Frame.sunk.l2
         button $Frame.sunk.l2.rx  -text "Rx" -bd 1 -command "FieldCalc::InsertDigit $Const(Rx)"
         button $Frame.sunk.l2.na  -text "Na" -bd 1 -command "FieldCalc::InsertDigit $Const(Na)"
         button $Frame.sunk.l2.k   -text "k " -bd 1 -command "FieldCalc::InsertDigit $Const(k)"
         button $Frame.sunk.l2.s   -text "S " -bd 1 -command "FieldCalc::InsertDigit $Const(S)"
         button $Frame.sunk.l2.h   -text "h " -bd 1 -command "FieldCalc::InsertDigit $Const(h)"
         button $Frame.sunk.l2.cpd -text "cpd" -bd 1 -command "FieldCalc::InsertDigit $Const(cpd)"
         Bubble::Create $Frame.sunk.l2.rx  $Desc(Rx)
         Bubble::Create $Frame.sunk.l2.na  $Desc(Na)
         Bubble::Create $Frame.sunk.l2.k   $Desc(k)
         Bubble::Create $Frame.sunk.l2.s   $Desc(S)
         Bubble::Create $Frame.sunk.l2.h   $Desc(h)
         Bubble::Create $Frame.sunk.l2.cpd $Desc(cpd)
         pack $Frame.sunk.l2.rx $Frame.sunk.l2.na $Frame.sunk.l2.k $Frame.sunk.l2.s $Frame.sunk.l2.h $Frame.sunk.l2.cpd -side left -anchor n
      frame $Frame.sunk.l3
         button $Frame.sunk.l3.rd  -text "Rd" -bd 1 -command "FieldCalc::InsertDigit $Const(Rd)"
         button $Frame.sunk.l3.rv  -text "Rv" -bd 1 -command "FieldCalc::InsertDigit $Const(Rv)"
         button $Frame.sunk.l3.rw  -text "rw" -bd 1 -command "FieldCalc::InsertDigit $Const(rw)"
         button $Frame.sunk.l3.ri  -text "ri" -bd 1 -command "FieldCalc::InsertDigit $Const(ri)"
         button $Frame.sunk.l3.rrd -text "rd" -bd 1 -command "FieldCalc::InsertDigit $Const(rd)"
         button $Frame.sunk.l3.cvd -text "cvd" -bd 1 -command "FieldCalc::InsertDigit $Const(cvd)"
         Bubble::Create $Frame.sunk.l3.rd  $Desc(Rd)
         Bubble::Create $Frame.sunk.l3.rv  $Desc(Rv)
         Bubble::Create $Frame.sunk.l3.rw  $Desc(rw)
         Bubble::Create $Frame.sunk.l3.ri  $Desc(ri)
         Bubble::Create $Frame.sunk.l3.rrd $Desc(rd)
         Bubble::Create $Frame.sunk.l3.cvd $Desc(cvd)
         pack $Frame.sunk.l3.rd $Frame.sunk.l3.rv $Frame.sunk.l3.rw $Frame.sunk.l3.ri $Frame.sunk.l3.rrd $Frame.sunk.l3.cvd -side left -anchor n
      pack  $Frame.sunk.l1 $Frame.sunk.l2 $Frame.sunk.l3 -side top -fill y
   pack $Frame.sunk -side top -pady 10 -padx 5 -anchor nw
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::WidgetConv>
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

proc FieldCalc::WidgetConv { Frame } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Conv

   ComboBox::Create $Frame.type FieldCalc::Conv(Type) noedit unsorted nodouble -1 [lindex $Conv(Types) $GDefs(Lang)] 9 6 \
      "FieldCalc::ConvertList $Frame"
   frame $Frame.conv
      ComboBox::Create $Frame.conv.from FieldCalc::Conv(From) noedit unsorted nodouble -1 "" 9 6 \
         "FieldCalc::ConvertFactor"
      label $Frame.conv.ind -text [lindex $Lbl(To) $GDefs(Lang)]
      ComboBox::Create $Frame.conv.to FieldCalc::Conv(To) noedit unsorted nodouble -1 "" 8 6 \
         "FieldCalc::ConvertFactor"
      pack  $Frame.conv.from $Frame.conv.ind $Frame.conv.to -side left
   button $Frame.factor -textvariable FieldCalc::Conv(Factor) -relief groove -bd 2 \
      -command "if { \$FieldCalc::Conv(Factor)!=\"\" } { FieldCalc::InsertOperator *\$FieldCalc::Conv(Factor) }"

   pack $Frame.type -side top -pady 10 -padx 5 -anchor w
   pack $Frame.conv -side top -padx 5 -anchor w
   pack $Frame.factor -side top -padx 5 -fill x -expand true

   Bubble::Create $Frame.type      $Bubble(ConvType)
   Bubble::Create $Frame.conv.from $Bubble(ConvFrom)
   Bubble::Create $Frame.conv.to   $Bubble(ConvTo)
   Bubble::Create $Frame.factor    $Bubble(ConvFact)
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::WidgetMath>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame des fonctions mathematiques.
#
# Parametres :
#   <Frame>  : Frame parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::WidgetMath { Frame } {
   variable Bubble

   frame $Frame.sunk -relief sunken -bd 1
      frame $Frame.sunk.l1
         button $Frame.sunk.l1.ln   -text "ln " -command "FieldCalc::InsertFunc ln   1 0" -bd 1
         button $Frame.sunk.l1.exp  -text "exp" -command "FieldCalc::InsertFunc exp  1 0" -bd 1
         button $Frame.sunk.l1.log  -text "log" -command "FieldCalc::InsertFunc log  1 0" -bd 1
         pack $Frame.sunk.l1.ln $Frame.sunk.l1.exp $Frame.sunk.l1.log -side top
      frame $Frame.sunk.l2
         button $Frame.sunk.l2.sqrt -text "sqrt" -command "FieldCalc::InsertFunc sqrt 1 0" -bd 1
         button $Frame.sunk.l2.cbrt -text "cbrt" -command "FieldCalc::InsertFunc cbrt 1 0" -bd 1
         button $Frame.sunk.l2.mod  -text "fmod" -command "FieldCalc::InsertFunc fmod 2 0" -bd 1
         pack $Frame.sunk.l2.sqrt $Frame.sunk.l2.cbrt $Frame.sunk.l2.mod -side top
      pack  $Frame.sunk.l1 $Frame.sunk.l2 -side top
      frame $Frame.sunk.l3
         button $Frame.sunk.l3.ceil  -text "ceil " -command "FieldCalc::InsertFunc ceil 1 0" -bd 1
         button $Frame.sunk.l3.floor -text "floor" -command "FieldCalc::InsertFunc floor 1 0" -bd 1
         button $Frame.sunk.l3.round -text "round" -command "FieldCalc::InsertFunc round 1 0" -bd 1
         pack $Frame.sunk.l3.ceil $Frame.sunk.l3.floor $Frame.sunk.l3.round -side top
      pack  $Frame.sunk.l1 $Frame.sunk.l2 $Frame.sunk.l3 -side left
      frame $Frame.sunk.l4
         button $Frame.sunk.l4.abs   -text "abs" -command "FieldCalc::InsertFunc abs 1 0" -bd 1
         button $Frame.sunk.l4.dif   -text "dif" -command "FieldCalc::InsertFunc dif 2 0" -bd 1
         pack $Frame.sunk.l4.abs $Frame.sunk.l4.dif -side top -anchor n
      pack  $Frame.sunk.l1 $Frame.sunk.l2 $Frame.sunk.l3 $Frame.sunk.l4 -side left  -fill y
   pack $Frame.sunk -side top -pady 10 -padx 5 -anchor nw

   Bubble::Create $Frame.sunk.l1.ln    $Bubble(MathLN)
   Bubble::Create $Frame.sunk.l1.exp   $Bubble(MathEXP)
   Bubble::Create $Frame.sunk.l1.log   $Bubble(MathLOG)
   Bubble::Create $Frame.sunk.l2.sqrt  $Bubble(MathSQRT)
   Bubble::Create $Frame.sunk.l2.cbrt  $Bubble(MathCBRT)
   Bubble::Create $Frame.sunk.l2.mod   $Bubble(MathFMOD)
   Bubble::Create $Frame.sunk.l3.ceil  $Bubble(MathCEIL)
   Bubble::Create $Frame.sunk.l3.floor $Bubble(MathFLOOR)
   Bubble::Create $Frame.sunk.l3.round $Bubble(MathROUND)
   Bubble::Create $Frame.sunk.l4.abs   $Bubble(MathABS)
   Bubble::Create $Frame.sunk.l4.dif   $Bubble(MathDIF)
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::WidgetMem>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame des fonctions memoires.
#
# Parametres :
#   <Frame>  : Frame parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::WidgetMem { Frame } {
   global GDefs
   variable Bubble

   frame $Frame.fun -relief sunken -bd 1
      button $Frame.fun.sto -text "STO" -bd 1 \
         -command "if { \[fstdfield is \$FieldCalc::Data(Operand)\] } { incr FieldCalc::Data(FieldNo) }; $Frame.mem.list insert end \$FieldCalc::Data(Operand)"
      button $Frame.fun.rcl -text "RCL" -bd 1 \
         -command "catch { FieldCalc::InsertDigit \[$Frame.mem.list get \[$Frame.mem.list curselection]\] }"
      button $Frame.fun.del -text "DEL" -bd 1 \
         -command "catch { $Frame.mem.list delete \[set fld \[$Frame.mem.list curselection\]\]; if { \[fstdfield is \$fld\] } { fstdfield free \$fld } }"
      pack  $Frame.fun.sto $Frame.fun.rcl $Frame.fun.del -side top
   pack $Frame.fun -side left -pady 10 -padx 5

   frame $Frame.mem
      listbox $Frame.mem.list -bg $GDefs(ColorLight) -width 17 -bd 1 -yscrollcommand "$Frame.mem.scroll set" -exportselection false -listvar FieldCalc::Data(Mem)
      scrollbar $Frame.mem.scroll -orient vertical -bd 1 -width 10 -command "$Frame.mem.list yview"
      pack $Frame.mem.list -side left -fill both -expand true
      pack $Frame.mem.scroll -side left -fill y
   pack $Frame.mem -side left -fill both -pady 10 -padx 5 -expand true

   Bubble::Create $Frame.fun.sto  $Bubble(STO)
   Bubble::Create $Frame.fun.rcl  $Bubble(RCL)
   Bubble::Create $Frame.fun.del  $Bubble(DEL)
   Bubble::Create $Frame.mem.list $Bubble(LST)
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

   pack $Frame.ops -side top -pady 2

   frame $Frame.cmd
      button $Frame.cmd.equal -text "   =  "  -bd 1 -command { Viewport::UpdateData $Page::Data(Frame); Page::UpdateCommand $Page::Data(Frame) }
      button $Frame.cmd.clear -text "C"  -bd 1 -bg red -command { set FieldCalc::Data(Operand) ""; Viewport::UpdateData $Page::Data(Frame); Page::UpdateCommand $Page::Data(Frame)}
      button $Frame.cmd.int2  -text "<<" -command "FieldCalc::InsertOperator <<" -bd 1
      button $Frame.cmd.int3  -text "<<<" -command "FieldCalc::InsertOperator <<<" -bd 1
      button $Frame.cmd.idx   -text "\[\]" -command "FieldCalc::InsertOperator \\\[\\\]" -bd 1
      pack $Frame.cmd.clear -side left
      pack $Frame.cmd.equal -side left
      pack $Frame.cmd.idx $Frame.cmd.int3 $Frame.cmd.int2 -side right
   pack $Frame.cmd -side bottom -fill both  -padx 2 -pady 2

   Bubble::Create $Frame.cmd.int2 $Bubble(Int2)
   Bubble::Create $Frame.cmd.int3 $Bubble(Int3)
   Bubble::Create $Frame.cmd.idx  $Bubble(Idx)
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::WidgetStat>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame des fonctions de reduction de matrices.
#
# Parametres :
#   <Frame>  : Frame parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::WidgetStat { Frame } {
   variable Bubble

   frame $Frame.sunk -relief sunken -bd 1
      frame $Frame.sunk.l1
         button $Frame.sunk.l1.smin -text "smin" -command "FieldCalc::InsertFunc smin 1 0" -bd 1
         button $Frame.sunk.l1.smax -text "smax" -command "FieldCalc::InsertFunc smax 1 0" -bd 1
         button $Frame.sunk.l1.savg -text "savg" -command "FieldCalc::InsertFunc savg 1 0" -bd 1
         pack $Frame.sunk.l1.smin $Frame.sunk.l1.smax $Frame.sunk.l1.savg -side top
      frame $Frame.sunk.l2
         button $Frame.sunk.l2.snb  -text "snb " -command "FieldCalc::InsertFunc snb  1 0" -bd 1
         button $Frame.sunk.l2.ssum -text "ssum" -command "FieldCalc::InsertFunc ssum 1 0" -bd 1
         button $Frame.sunk.l2.svar -text "svar" -command "FieldCalc::InsertFunc svar 1 0" -bd 1
         pack $Frame.sunk.l2.snb $Frame.sunk.l2.ssum $Frame.sunk.l2.svar -side top
      frame $Frame.sunk.l3
         button $Frame.sunk.l3.scor  -text "scor " -command "FieldCalc::InsertFunc scor  2 0" -bd 1
         button $Frame.sunk.l3.scov  -text "scov " -command "FieldCalc::InsertFunc scov  2 0" -bd 1
         button $Frame.sunk.l3.srms  -text "srmse" -command "FieldCalc::InsertFunc srmse 2 0" -bd 1
         pack $Frame.sunk.l3.scor $Frame.sunk.l3.scov $Frame.sunk.l3.srms -side top
      frame $Frame.sunk.l4
         button $Frame.sunk.l4.smb  -text "smb "  -command "FieldCalc::InsertFunc smb 2 0" -bd 1
         button $Frame.sunk.l4.snmb -text "snmb"  -command "FieldCalc::InsertFunc snmb 2 0" -bd 1
         button $Frame.sunk.l4.smnb -text "smnb"  -command "FieldCalc::InsertFunc smnb 2 0" -bd 1
         pack $Frame.sunk.l4.smb $Frame.sunk.l4.snmb $Frame.sunk.l4.smnb -side top
      frame $Frame.sunk.l5
         button $Frame.sunk.l5.sme  -text "sme "  -command "FieldCalc::InsertFunc sme 2 0" -bd 1
         button $Frame.sunk.l5.snme -text "snme"  -command "FieldCalc::InsertFunc snme 2 0" -bd 1
         button $Frame.sunk.l5.smne -text "smne"  -command "FieldCalc::InsertFunc smne 2 0" -bd 1
         pack $Frame.sunk.l5.sme $Frame.sunk.l5.snme $Frame.sunk.l5.smne -side top
      frame $Frame.sunk.l6
         button $Frame.sunk.l6.srega -text "srega"  -command "FieldCalc::InsertFunc srega 2 0" -bd 1
         button $Frame.sunk.l6.sregb -text "sregb"  -command "FieldCalc::InsertFunc sregb 2 0" -bd 1
         pack $Frame.sunk.l6.srega $Frame.sunk.l6.sregb -side top
      pack  $Frame.sunk.l1 $Frame.sunk.l2 $Frame.sunk.l3 $Frame.sunk.l4 $Frame.sunk.l5 $Frame.sunk.l6 -side left  -fill y
   pack $Frame.sunk -side top -pady 10 -padx 5 -anchor nw

   Bubble::Create $Frame.sunk.l1.smin  $Bubble(StatSMIN)
   Bubble::Create $Frame.sunk.l1.smax  $Bubble(StatSMAX)
   Bubble::Create $Frame.sunk.l1.savg  $Bubble(StatSAVG)
   Bubble::Create $Frame.sunk.l2.ssum  $Bubble(StatSSUM)
   Bubble::Create $Frame.sunk.l2.snb   $Bubble(StatSNB)
   Bubble::Create $Frame.sunk.l2.svar  $Bubble(StatSVAR)
   Bubble::Create $Frame.sunk.l3.scor  $Bubble(StatSCOR)
   Bubble::Create $Frame.sunk.l3.scov  $Bubble(StatSCOV)
   Bubble::Create $Frame.sunk.l3.srms  $Bubble(StatSRMSE)
   Bubble::Create $Frame.sunk.l4.smb   $Bubble(StatSMB)
   Bubble::Create $Frame.sunk.l4.snmb  $Bubble(StatSNMB)
   Bubble::Create $Frame.sunk.l4.smnb  $Bubble(StatSMNB)
   Bubble::Create $Frame.sunk.l5.sme   $Bubble(StatSME)
   Bubble::Create $Frame.sunk.l5.snme  $Bubble(StatSNME)
   Bubble::Create $Frame.sunk.l5.smne  $Bubble(StatSMNE)
   Bubble::Create $Frame.sunk.l6.srega $Bubble(StatSREGA)
   Bubble::Create $Frame.sunk.l6.sregb $Bubble(StatSREGB)
}

#----------------------------------------------------------------------------
# Nom      : <FieldCalc::WidgetTrigo>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer le frame des fonctions trigonometriques.
#
# Parametres :
#   <Frame>  : Frame parent
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldCalc::WidgetTrigo { Frame } {
   variable Bubble

   frame $Frame.typ -relief sunken -bd 1
      checkbutton $Frame.typ.arc -text "arc" -bd 1 -command "" -variable FieldCalc::Data(Arc) -indicatoron false
      checkbutton $Frame.typ.hyp -text "hyp" -bd 1 -command "" -variable FieldCalc::Data(Hyp) -indicatoron false
      pack  $Frame.typ.arc $Frame.typ.hyp -side left -fill y -ipadx 5 -ipady 3
   pack $Frame.typ -side left -pady 10 -padx 5 -anchor nw

   frame $Frame.fun -relief sunken -bd 1
      button $Frame.fun.sin -text "sin" -bd 1 -command "FieldCalc::InsertFunc sin 1 1"
      button $Frame.fun.cos -text "cos" -bd 1 -command "FieldCalc::InsertFunc cos 1 1"
      button $Frame.fun.tan -text "tan" -bd 1 -command "FieldCalc::InsertFunc tan 1 1"
       pack $Frame.fun.sin $Frame.fun.cos $Frame.fun.tan -side left -fill x -expand true
   pack $Frame.fun -side left -pady 10 -padx 5 -anchor nw

   Bubble::Create $Frame.typ.arc $Bubble(TrigoARC)
   Bubble::Create $Frame.typ.hyp $Bubble(TrigoHYP)
   Bubble::Create $Frame.fun.sin $Bubble(TrigoSIN)
   Bubble::Create $Frame.fun.cos $Bubble(TrigoCOS)
   Bubble::Create $Frame.fun.tan $Bubble(TrigoTAN)
}
