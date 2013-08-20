#ERPG - Emergency Response Planning Guideline
#
#Updated 30 sep 2009
#
#Number of ERPGs: 140 (as mentioned in original 2009 .pdf document)
#
######
#NOTE: This information is correct as of January 1, 2009. It is possible
#that an ERPG under review by the Committee will be balloted and
#approved in 2009, making it eligible for inclusion in the 2009 ERPG
#Document Set.
#
#These chemicals are under consideration or review by the AIHA ERP Committee:
#Benzene
#Boron trifluoride
#1,3-Butadiene
#n-Butyl acetate
#Carbon disulfide
#Carbon monoxide
#1-Chloro-1,1- (HCFC-142b)
#Chloroacetophenone
#Chloroacetic acid
#Chloroacetyl chloride
#Chloroform
#Chloromethyl methyl ether
#Chloropicrin
#Chlorosulfonic acid
#Cyanogen chloride
#Diborane
#1,2-Dichloroethane
#Diesel/Fuel Oil/Kerosene
#Dimethyl disulfide
#Dimethyl sulfate
#Ethyl acrylate
#Ethanol
#Fluorine
#Formaldehyde
#Formic acid
#Furfural
#Gasoline
#Hydrogen chloride
#Hydrogen peroxide
#Hydroquinone
#Iodine
#Isocyanuric acid
#Methyl bromide
#Methyl mercaptan
#Methylene diphenyl diisocyanate (MDI)
#Naphthalene
#1-Octanol
#Phosphine
#Phosphoric acid
#Polyethylene glycol
#Polypropylene
#Propylene glycol momomethylether acetate
#Propylene oxide
#Quinoline
#Sodium chloroacetate
#Sulfur dioxide
#Sulfuryl chloride
#Sulfuryl fluoride
#Tetrachlorosilane
#Tetraethoxysilane
#Tetramethoxysilane
#Thionyl chloride
#Toluene
#Toluene 2,4-(2,6-) Diisocyanate (TDI)
#1,1,1-Trichloroethane
#Trichlorosilane
#Trimethyl Chlorosilane 
#######
#
#Definitions:
#
#ERPG-1: The maximum airborne concentration below which it is believed nearly all individuals could be
#exposed for up to 1 hour without experiencing more than mild, transient adverse health effects or without
#perceiving a clearly defined objectionable odor.
#
#ERPG-2: The maximum airborne concentration below which it is believed nearly all individuals could be
#exposed for up to 1 hour without experiencing or developing irreversible or other serious health effects
#or symptoms that could impair an individual's ability to take protective action.
#
#ERPG-3: The maximum airborne concentration below which it is believed nearly all individuals could be
#exposed for up to 1 hour without experiencing or developing life-threatening health effects.
#
#
#Reference:
#The AIHA 2007 Emergency Response Planning Guidelines & Workplace Environmental Exposure Levels Handbook, American Industrial Hygiene Association, Fairfax, Virginia
#ISBN 978-1-931504-74-4
#AIHA Press
#American Industrial Hygiene Association
#2700 Prosperity Ave., Suite 250
#Fairfax, VA 22031
#Tel.: (703) 849-8888
#Fax: (703) 207-3561
#http://www.aiha.org
#e-mail: infonet@aiha.org
#Stock No. AEAH07-559
#
#
#Format:
#1st column: Chemical Compound
#2nd column: CAS Number
#3rd column: Unit [ppm, mg/m3, ug/m3]
#4th column: ERPG-1
#5th column: ERPG-2
#6th column: ERPG-3
#
#NA = not appropriate
#ID = insufficient data

array set ERPG {
"Acetaldehyde (75-07-0)" { ppm { { 10 200 1000 } } }
"Acetic Acid (64-19-7)" { ppm { { 5 35 250 } } }
"Acetic Anhydride (108-24-7)" { ppm { { 0.5 15 100 } } }
"Acrolein (107-02-8)" { ppm { { 0.05 0.15 1.5 } } }
"Acrylic Acid (79-10-7)" { ppm  { { 2 50 750 } } }
"Acrylonitrile (107-13-1)" { ppm { { 10 35 75 } } }
"Allyl Chloride (107-05-1)" { ppm { { 3 40 300 } } }
"Ammonia (7664-41-7)" { ppm { { 25 150 750 } } }
"Arsine (7784-42-1)" { ppm { {  0.5 1.5 } } }
"Benzene (71-43-2)" { ppm { { 50 150 1000 } } }
"Benzoyl Chloride (98-88-4)" { ppm  { { 0.3 5 20 } } }
"Benzyl Chloride (100-44-7)"  { ppm { { 1 10 50 } } }
"Beryllium (7440-41-)" { ug/m3 { {  25 100 } } }
"Bis (Chloromethyl) Ether (542-88-1)" { ppm { {  0.1 0.5 } } }
"Boron Trifluoride (7637-07-2)" { mg/m3 { { 2 30 100 } } }
"Bromine (7726-95-6)" { ppm { { 0.1 0.5 5 } } }
"1,3-Butadiene (106-99-0)" { ppm { { 10 200 5000 } } }
"n-Butyl Acetate (123-86-4)" { ppm { { 5 200 3000 } } }
"n-Butyl Acrylate (141-32-2)" { ppm { { 0.05 25 250 } } }
"n-Butyl Isocyanate (111-36-4)" { ppm { { 0.01 0.05 1 } } }
"Carbon Disulfide (75-15-0)" { ppm { { 1 50 500 } } }
"Carbon Monoxide (630-08-0)" { ppm { { 200 350 500 } } }
"Carbon Tetrachloride (56-23-5)" { ppm { { 20 100 750 } } }
"Chlorine (7782-50-5)" { ppm { { 1 3 20 } } }
"Chlorine Dioxide (10049-04-4)" { ppm { {  0.5 3 } } }
"Chlorine Trifluoride (7790-91-2)" { ppm { { 0.1 1 10 } } }
"1-Chloro-1,1-Difluoroethane (HCFC-142b) (75-68-3)" { ppm { { 10000 15000 25000 } } }
"2-Chloro-1,1,1,2-Tetrafluoroethane (2837-89-0)" { ppm { { 1,000 5,000 10,000 } } }
"Chloroacetyl Chloride (79-04-9)" { ppm { { 0.05 0.5 10 } } }
"o-Chlorobenzylidene Malononitrile (2698-41-1)" { mg/m3 { { 0.005 0.1 25 } } }
"Chloroform (67-66-3)" { ppm { {  50 5000 } } }
"Chloromethyl Methyl Ether (107-30-2)" { ppm { {  1.0 10 } } }
"Chloropicrin (76-06-2)" { ppm { { 0.1 0.3 1.5 } } }
"Chlorosulfonic Acid (7790-94-5)" { mg/m3 { { 2 10 30 } } }
"Chlorotrifluoroethylene (79-38-9)" { ppm { { 20 100 300 } } }
"Cobalt Hydrocarbonyl (16842-03-8)" { ppm { {  0.13 0.42 } } }
"Crotonaldehyde (4170-30-3)" { ppm { { 0.2 5 15 } } }
"Cyanogen Chloride (506-77-4)" { ppm { {  0.4 4 } } }
"Diborane (19287-45-7)" { ppm { {  1 3 } } }
"1,2-Dichloroethane (107-06-2)" { ppm { { 50 200 300 } } }
"2,4-Dichlorophenol (120-83-2)" { ppm { { 0.2 2 20 } } }
"Dicyclopentadiene (77-73-6)" { ppm { { 0.01 5 75 } } }
"1,1-Difluoroethane (HFC 152a) (75-37-6)" { ppm { { 10000 15000 25000 } } }
"Diketene (674-82-8)" { ppm { { 1 5 20 } } }
"Dimethylamine (124-40-3)" { ppm { { 0.6 100 350 } } }
"Dimethyldichlorosilane (75-78-5)" { ppm { { 2 10 75 } } }
"Dimethyl Disulfide (624-92-0)" { ppm { { 0.01 50 250 } } }
"Dimethylformamide (68-12-2)" { ppm { { 2 100 200 } } }
"Dimethyl Sulfide (75-18-3)" { ppm { { 0.5 1000 5000 } } }
"Dowtherm J (25340-17-4)" { ppm { { 10 100 500 } } }
"Dowtherm Q (68987-42-8)" { mg/m3 { {  150  } } }
"Epichlorohydrin (106-89-8)" { ppm { { 5 20 100 } } }
"Ethyl Acrylate (140-88-5)" { ppm { { 0.01 30 300 } } }
"Ethyl Chloroformate (541-41-3)" { ppm { {  5 10 } } }
"2-Ethyl hexanol (104-76-7)" { ppm { { 0.1 100 200 } } }
"Ethylene Oxide (75-21-8)" { ppm { {  50 500 } } }
"Ethylidene Norbornene (16219-75-3)" { ppm { { 0.2 100 500 } } }
"Fluorine (7782-41-4)" { ppm { { 0.5 5 20 } } }
"Fluorosulfonic acid (7789-21-1)" { mg/m3 { { 2 10 30 } } }
"Formaldehyde (50-00-0)" { ppm { { 1 10 25 } } }
"Furfural (98-01-1)" { ppm { { 2 10 100 } } }
"Gluteraldehyde (111-30-8)" { ppm  { { 0.2 1 5 } } }
"HCFC-123 (306-83-2)" { ppm { {  1000 10000 } } }
"Hexachlorobutadiene (87-68-3)" { ppm { { 1 3 10 } } }
"Hexafluoroacetone (684-16-2)" { ppm { {  1 50 } } }
"Hexafluoropropylene (116-15-4)" { ppm { { 10 50 500 } } }
"1-Hexene (592-41-6)" { ppm { {  500 5,000 } } }
"Hydrazine (302-01-2)" { ppm { { 0.5 5 30 } } }
"Hydrogen Chloride (7647-01-0)" { ppm { { 3 20 150 } } }
"Hydrogen Cyanide (74-90-8)" { ppm { {  10 25 } } }
"Hydrogen Fluoride (7664-39-3)" { ppm { { 2 20 50 } } }
"Hydrogen Peroxide (7722-84-1)" { ppm { { 10 50 100 } } }
"Hydrogen Selenide (7783-07-5)" { ppm { {  0.2 2 } } }
"Hydrogen Sulfide (7783-06-4)" { ppm { { 0.1 30 100 } } }
"Iodine (7553-56-2)" { ppm { { 0.1 0.5 5 } } }
"Isobutyronitrile (78-82-0)" { ppm { { 10 50 200 } } }
"2-Isocyanatoethyl Methacrylate (30674-80-7)" { ppm { {  0.1 1 } } }
"Isoprene (78-79-5)" { ppm { { 5 1000 4000 } } }
"Isopropyl Chloroformate (108-23-6)" { ppm { {  5 20 } } }
"Lithium Hydride (7580-67-8)" { ug/m3 { { 25 100 500 } } }
"Maleic Anhydride (108-31-6)" { ppm { { 0.2 2 20 } } }
"Mercury Vapor (7439-97-6)" { ppm { {  0.25 0.5 } } }
"Methanol (67-56-1)" { ppm { { 200 1000 5000 } } }
"Methyl Bromide (74-83-9)" { ppm { {  50 200 } } }
"Methyl Chloride (74-87-3)" { ppm { {  400 1000 } } }
"Methyl Chloroformate (79-22-1)" { ppm { {  2 5 } } }
"Methyl Iodide (74-88-4)" { ppm { { 25 50 125 } } }
"Methyl Isocyanate (624-83-9)" { ppm { { 0.025 0.25 1.5 } } }
"Methyl Mercaptan (74-93-1)" { ppm { { 0.005 25 100 } } }
"Methyl tert-Butyl Ether (1634-0404)" { ppm { { 5 1000 5000 } } }
"Methylene Chloride (75-09-02)" { ppm { { 300 750 4000 } } }
"Methylene Diphenyl Diisocyanate (MDI) (101-68-8)" { mg/m3 { { 0.2 2 25 } } }
"Methyltrichlorosilane (75-79-6)" { ppm { { 0.5 3 15 } } }
"Monomethylamine (74-89-5)" { ppm { { 10 100 500 } } }
"Nitric Acid WFNA (7697-37-2)" { ppm { { 1 6 78 } } }
"Nitrogen Dioxide (10102-44-0)" { ppm { { 1 15 30 } } }
"Nitrogen Triflouride (7783-54-2)" { ppm { {  400 800 } } }
"1-Octene (111-66-0)" { ppm { { 40 800 2000 } } }
"Perchloroethylene (127-18-4)" { ppm { { 100 200 1000 } } }
"Perfluoroisobutylene (382-21-8)" { ppm { {  0.1 0.3 } } }
"Phenol (108-95-2)" { ppm { { 10 50 200 } } }
"Phosgene (75-44-5)" { ppm { {  0.5 1.5 } } }
"Phosphine (7803-51-2)" { ppm { {  0.5 5 } } }
"Phosphorus Pentoxide (1314-56-3)" { mg/m3 { { 1 10 50 } } }
"Phosphorus Trichloride (7719-12-2)" { ppm { { 0.5 3 15 } } }
"Propylene Glycol Methyl Ether Acetate (108-65-6 ?-isomer) (70657-70-4 ?-isomer)" { ppm { { 50 1000 5000 } } }
"Propylene Oxide (75-56-9)" { ppm { { 50 250 750 } } }
"Sodium Hydroxide (1310-73-2)" { mg/m3 { { 0.5 5 50 } } }
"Stibine (7803-52-3)" { ppm { {  0.5 1.5 } } }
"Styrene (100-42-5)" { ppm { { 50 250 1000 } } }
"Sulfur Dioxide (7446-09-5)" { ppm { { 0.3 3 15 } } }
"Sulfur Trioxide (7446-11-9)" { mg/m3 { { 2 10 30 } } }
"Sulfuric Acid Oleum (8014-95-7)" { mg/m3 { { 2 10 30 } } }
"Sulfuric Acid (7664-93-9)" { mg/m3 { { 2 10 30 } } }
"Sulfuryl Chloride (7791-25-5)" { ppm { { 0.3 3 15 } } }
"Tetrachlorosilane (10026-04-7)" { ppm { { 0.75 5 37 } } }
"Tetraethoxysilane (78-10-4)" { ppm { { 25 100 300 } } }
"Tetrafluoroethylene (116-14-3)" { ppm  { { 200 1000 10000 } } }
"Tetrahydrofuran (109-99-9)" { ppm { { 100 500 5000 } } }
"Tetramethoxysilane (681-84-5)" { ppm { {  10 20 } } }
"Thionyl Chloride (7719-09-7)" { ppm { { 0.2 2 10 } } }
"Titanium Tetrachloride (7550-45-0)" { mg/m3 { { 5 20 100 } } }
"Toluene (108-88-3)" { ppm { { 50 300 1000 } } }
"Toluene 2,4- (2,6-) Diisocyanate (TDI) (584-84-9)" { ppm { { 0.01 0.15 0.6 } } }
"1,1,1-Trichloroethane (71-55-6)" { ppm { { 350 700 3500 } } }
"Trichloroethylene (79-01-6)" { ppm { { 100 500 5000 } } }
"Trichlorosilane (10025-78-2)" { ppm { { 1 3 25 } } }
"Triethoxysilane (998-30-1)" { ppm { { 0.5 4 10 } } }
"Trimethoxysilane (2487-90-3)" { ppm { { 0.5 2 5 } } }
"Trimethylamine (75-50-3)" { ppm { { 0.1 100 500 } } }
"Trimethylchlorosilane (75-77-4)" { ppm { { 3 20 150 } } }
"Triuranium Octaoxide (1344-59-8)" { mg/m3 { {  10 50 } } }
"Uranium Dioxide (1344-57-6)" { mg/m3 { {  10 30 } } }
"Uranium Hexafluoride (7783-81-5)" { mg/m3 { { 5 15 30 } } }
"Uranium Trioxide (1344-58-7)" { mg/m3 { {  0.5 3 } } }
"Vinyl Acetate (108-05-4)" { ppm { { 5 75 500 } } }
"Vinyl Chloride (75-01-4)" { ppm { { 500 5000 20000 } } }
"Vinyl Trichlorosilane (75-94-5)" { ppm { { 0.5 5 50 } } }
"Vinylidene Chloride (75-35-4)" { ppm { {  500 1000 } } }
}