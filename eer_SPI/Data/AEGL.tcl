#AEGL - Acute Exposure Guideline Level
#
#Updated 30 sep 2009
#
#Number of final AEGLs: 50
#(this list contains final AEGLs only)
#
#Final AEGLs are published by the National Research Council, National Academy of Sciences (NRC/NAS) following NRC/NAS peer review.
#The NAS publications may differ slightly from the final AEGL technical support documents due to editorial changes.
#
#
#Definitions:
#
#AEGLs represent threshold exposure limits for the general public and are applicable to emergency exposure periods ranging
#from 10 min to 8 h. AEGL-2 and AEGL-3, and AEGL-1 values as appropriate, will be developed for each of five exposure periods
#(10 and 30 min, 1 h, 4 h, and 8 h) and will be distinguished by varying degrees of severity of toxic effects. It is believed
#that the recommended exposure levels are applicable to the general population including infants and children, and other
#individuals who may be susceptible. The three AEGLs have been defined as follows:
#
#AEGL-1 is the airborne concentration (expressed as parts per million or milligrams per cubic meter (ppm or mg/m3)) of a
#substance above which it is predicted that the general population, including susceptible individuals, could experience
#notable discomfort, irritation, or certain asymptomatic nonsensory effects. However, the effects are not disabling and are
#transient and reversible upon cessation of exposure.

#AEGL-2 is the airborne concentration (expressed as ppm or mg/m3) of a substance above which it is predicted that the general
#population, including susceptible individuals, could experience irreversible or other serious, long-lasting adverse health
#effects or an impaired ability to escape.

#AEGL-3 is the airborne concentration (expressed as ppm or mg/m3) of a substance above which it is predicted that the general
#population, including susceptible individuals, could experience life-threatening health effects or death.
#
#Airborne concentrations below the AEGL-1 represent exposure levels that can produce mild and progressively increasing but
#transient and nondisabling odor, taste, and sensory irritation or certain asymptomatic, nonsensory effects. With increasing
#airborne concentrations above each AEGL, there is a progressive increase in the likelihood of occurrence and the severity of
#effects described for each corresponding AEGL. Although the AEGL values represent threshold levels for the general public,
#including susceptible subpopulations, such as infants, children, the elderly, persons with asthma, and those with other
#illnesses, it is recognized that individuals, subject to unique or idiosyncratic responses, could experience the effects
#described at concentrations below the corresponding AEGL.
#
#
#References:
#http://www.epa.gov/oppt/aegl/pubs/compiled_aegls_050409.pdf
#
#
#Format:
#1st column: CAS Number
#2nd column: Chemical Compound
#3rd column: Unit [ppm, mg/m3]
#4th column: AEGL-1 (10min 30min 60min 4hr 8hr)
#5th column: AEGL-2 (10min 30min 60min 4hr 8hr)
#6th column: AEGL-3 (10min 30min 60min 4hr 8hr)
#
#NR = not recommended due to insufficient data

array set AEGL {
"Acetone cyanohydrin (75-86-5)" { ppm { { 2.5 17 27 } { 2.5 10 21 } { 2 7.1 15 } { 1.3 3.5 8.6 } { 1.0 2.5 6.6 } } }
"Agent GA (Tabun) (77-81-6)" { ppm { { 0.0010 0.013 0.11 } { 0.00060 0.0075 0.057 } { 0.00042 0.0053 0.039 } { 0.00021 0.0026 0.021 } { 0.00015 0.0020 0.015 } } }
"Agent GB (Sarin) (107-44-8)" { ppm { { 0.0012 0.015 0.064} { 0.00068 0.0085 0.032 } { 0.00048 0.0060 0.022 } { 0.00024 0.0029 0.012 } { 0.00017 0.0022 0.0087 } } }
"Agent GD (Soman) (96-64-0)" { ppm { { 0.00046 0.0057 0.049 } { 0.00026 0.0033 0.025 } { 0.00018 0.00220.017 } { 0.000091 0.0012 0.0091 } { 0.000065 0.00085 0.0066 } } }
"Agent GF (329-99-7)" { ppm { { 0.00049 0.0062 0.053 } { 0.00028 0.0035 0.027 } { 0.00020 0.0024 0.018 } { 0.00010 0.0013 0.0098 } { 0.000070 0.00091 0.0071 } } }
"Agent VX (50782-69-9)" { ppm { { 0.000052 0.00065 0.0027 } { 0.000030 0.00038 0.0014 } { 0.000016 0.00027  0.00091} { 0.0000091 0.00014 0.00048 } { 0.0000065 0.000095 0.00035 } } }
"Allyl Amine (107-11-9)" { ppm { { 0.42 3.3 150 } { 0.42 3.3 40 } { 0.42 3.3 18 } { 0.42 1.8 3.5 } { 0.42 1.2 2.3 } } }
"Aluminium phosphide ()" { ppm { {  4.0 7.2 } {  4.0 7.2 } {  2.0 3.6 } {  0.5 0.9 } {  0.25 0.45 } } }
"Ammonia (7664-41-7)" { ppm { { 30 220 2700 } { 30 220 1600 } { 30 160 1100 } { 30 110 550 } { 30 110 390 } } }
"Aniline (62-53-3)" { ppm { { 48 72 120 } { 16 24 40 } { 8 12 20 } { 2 3 5 } { 1 1.5 2.5 } } }
"Arsine (7784-42-1)" { ppm { { 0.300.91 } { 0.21 0.63 } { 0.17 0.50 } { 0.040 0.13 } { 0.020 0.060 } } }
"Calcium phosphide (1305-99-3)" { ppm { {  2.0 3.6 } {  2.0 3.6 } {  1.0 1.8 } {  0.25 0.45 } {  0.13 0.23 } } }
"Carbon disulfide (75-15-0)" { ppm { { 17 200 600 } { 17 200 600 } { 13 160 480 } { 8.4 100 300 } { 6.7 50 150 } } }
"Chlorine (7782-50-5)" { ppm { { 0.50 2.8 50 } { 0.50 2.8 28 } { 0.50 2.0 20 } { 0.50 1.0 10 } { 0.50 0.70 7.1 } } }
"Chlorine dioxide (10049-04-4)" { ppm { { 0.15 1.4 3.0 } { 0.15 1.4 3.0 } { 0.15 1.1 2.4 } { 0.15 0.69 1.5 } { 0.15 0.45 0.98 } } }
"Chlorine trifluoride (7790-91-2)" { ppm { { 0.12 8.1 84 } { 0.12 3.5 36 } { 0.12 2.0 21 } { 0.12 0.7 7.3 } { 0.12 0.41 7.3 } } }
"cis-Crotonaldehyde (4170-30-3)" { ppm { { 0.19 27 44 } { 0.19 8.9 27 } { 0.19 4.4 14 } { 0.19 1.1 2.6 } { 0.19 0.56 1.5 } } }
"trans-Crotonaldehyde (123-73-9)" { ppm { { 0.19 27 44 } { 0.19 8.9 27 } { 0.19 4.4 14 } { 0.19 1.1 2.6 } { 0.19 0.56 1.5 } } }
"Cyclohexylamine (108-91-8)" { ppm { { 1.8 11 38 } { 1.8 11 38 } { 1.8 8.6 30 } { 1.8 5.4 19 } { 1.8 2.7 9.5 } } }
"Diborane (19287-45-7)" { ppm { {  2.0 7.3 } {  2.0  7.3 } {  1.0 3.7 } {  0.25 0.92 } {  0.13  0.46 } } }
"1,1-Dimethyl hydrazine (57-14-7)" { ppm  { {  18 65 } {  6 22 } {  3 11 } {  0.75 2.7 } {  0.38 1.4 } } }
"1,2-Dimethyl hydrazine (540-73-8)" { ppm { {  18 65 } {  6.0 22 } {  3.0 11 } {  0.75 2.7 } {  0.38 1.4 } } }
"Ethylene diamine (107-15-3)" { ppm {  { 12 25 } { 12 25 } { 9.7 20 } { 6.1 13 } { 4.8 10 } } }
"HCFC 141b (1717-00-6)" { ppm { { 1000 1700 3000 } { 1000 1700 3000 } { 1000 1700 3000 } { 1000 1700 3000 } { 1000 1700 3000 } } }
"HFC 134A (811-97-2)" { ppm { { 8000 13000 27000 } { 8000 13000 27000 } { 8000 13000 27000 } { 8000 13000 27000 } { 8000 13000 27000 } } }
"HFE-7100 (mixture : 40% Methyl nonafluorobutyl ether (163702-07-6) and 60% Methyl nonafluoroisobutyl ether (163702-08-7)) " { ppm { { 2500 8200 15000 } { 2500 8200 15000 } { 2500 8200 15000 } { 2500 8200 15000 } { 2500 8200 15000 } } }
"Hydrogen chloride (7647-01-0)" { ppm { { 1.8 100 620 } { 1.8 43 210 } { 1.8 22 100 } { 1.8 11 26 } { 1.8 11 26 } } }
"Hydrogen cyanide (74-90-8)" {  ppm { { 2.5 17 27 } { 2.5 10 21 } { 2.0 7.1 15 } { 1.3 3.5 8.6 } { 1.0 2.5 6.6 } } }
"Hydrogen fluoride (7664-39-3)" { ppm { { 1.0 95 170 } { 1.0 34 62 } { 1.0 24 44 } { 1.0 12 22 } { 1.0 12 22 } } }
"Iron pentacarbonyl (13463-40-6)" { ppm { {  0.077 0.23 } {  0.077 0.23 } {  0.060 0.18 } {  0.037 0.11 } {  0.025 0.075 } } }
"Magnesium aluminum phosphide (-----)" { ppm { {  1.3 2.4 } {  1.3 2.4 } {  0.67 1.2 } {  0.17 0.30 } {  0.080 0.15 } } }
"Magnesium phosphide (12057-74-8)" { ppm { {  2.0 3.6 } {  2.0 3.6 } {  1.0 1.8 } {  0.25 0.45 } {  0.13 0.23 } } }
"Methyl hydrazine (60-34-4)" { ppm  { { 5.3 16 } { 1.8 5.5 } { 0.9 2.7 } { 0.23 0.68 } { 0.11 0.34 } } }
"Methyl isocyanate (624-83-9)" { ppm { {  0.40 1.2 } {  0.13 0.40 } {  0.067 0.20 } {  0.017 0.050 } {  0.0080 0.025 } } }
"Monochloroacetic acid (79-11-8)" { ppm { {  12  } {  8.3  } {  6.6  } {  1.7  } {  0.83  } } }
"Nickel carbonyl (13463-39-3)" { ppm { {  0.10 0.46 } {  0.072 0.32 } {  0.036 0.16 } {  0.0090 0.040 } {  0.0045 0.020 } } }
"Otto Fuel (mainly Propylene Glycol Dinitrate 6423-43-4) (106602-80-6)" { ppm { { 0.33 2.0 16 } { 0.33 2.0 16 } { 0.17 1.0 13 } { 0.050 0.25 8.0 } { 0.030 0.13 5.3 } } }
"Phenol (108-95-2)" { ppm { { 19 29  } { 19 29  } { 15 23  } { 9.5 15  } { 6.3 12  } } }
"Phosgene (75-44-5)" { ppm  { { 0.60 3.6 } { 0.60 1.5 } { 0.30 0.75 } { 0.080 0.20 } { 0.040 0.090 } } }
"Phosphine (7803-51-2)" { ppm { { 4.0 7.2 } { 4.0 7.2 } { 2.0 3.6 } { 0.50 0.90 } { 0.25 0.45 } } }
"Potassium phosphide (20770-41-6)" { ppm { {  4.0 7.2 } {  4.0 7.2 } {  2.0 3.6 } {  0.5 0.9 } {  0.25 0.45 } } }
"Propylene Glycol Dinitrate (main component of Otto Fuel 106602-80-6) (6423-43-4)" { ppm { { 0.33 2.0 16 } { 0.33 2.0 16 } { 0.17 1.0 13 } { 0.050 0.25 8.0 } { 0.030 0.13 5.3 } } }
"Sodium phosphide (12058-85-4)" { ppm { {  4.0 7.2 } {  4.0 7.2 } {  2.0 3.6 } {  0.5 0.9 } {  0.25 0.45 } } }
"Strontium phosphide (12504-13-1)" { ppm { {  2.0 3.6 } { 2.0 3.6 } {  1.0 1.8} {  0.25 0.45 } {  0.13 0.23 } } }
"Sulfur Mustard (505-60-2)" { ppm { { 0.060 0.090 0.59 } { 0.020 0.030 0.41 } { 0.010 0.020  0.32 } { 0.0030 0.0040 0.080 } { 0.0010 0.0020 0.040 } } }
"Tetranitromethane (509-14-8)" { ppm { {  0.66 2.2 } {  0.66 2.2 } {  0.52 1.7 } {  0.33 1.1 } {  0.17 0.55 } } }
"2,4-Toluene Diisocyanate (584-84-9)" { ppm { { 0.020 0.24 0.65 } { 0.020 0.17 0.65 } { 0.020 0.083 0.51 } { 0.010 0.021 0.32} { 0.010 0.021 0.16 } } }
"2,6-Toluenediisocyanate (91-08-7)" { ppm { { 0.020 0.24  0.65 } { 0.020 0.17 0.65 } { 0.020 0.083 0.51 } { 0.010  0.021  0.32 } { 0.010 0.021 0.16 } } }
"Uranium hexafluoride (7783-81-5)" { mg/m3 { { 3.6 28 216 } { 3.6 19 72 } { 3.6 9.6 36 } {  2.4 9.0 } {  1.2 4.5 } } }
"Zinc phosphide (1314-84-7)" { ppm { {  2.0 3.6 } {  2.0 3.6 } {  1.0 1.8 } {  0.25 0.45 } {  0.13 0.23 } } }
}