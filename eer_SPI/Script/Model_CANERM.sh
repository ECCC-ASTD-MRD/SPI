#===============================================================================
# Environnement Canada - Service meteorologique du Canada
# Centre meteorologique canadien
# 2121 Route Trans-canadienne
# Dorval, Quebec
# H9P 1J3
#
# Projet     : CANERM model processing interface.
# Nom        : <Model_CANERM.sh>
# Creation   : 24 October 2007 - A. Malo - CMC/CMOE
#
# Description: Support file for CANERM models, it defines the following functions:
#                - {Model}_Pre     : Do the pre-processing stuff
#                - {Model}_Run     : Run the model
#                - {Model}_Post    : Do the post-processing stuff
# Variables:
#    CANERM_FREQIN           Frequency on input meteo fields
#    CANERM_FREQOUT          Frequency of output files
#    CANERM_PREVDIR          Directory of previous simulation (For continuation)
#    CANERM_PREVRESTART      Location of previous restart (For continuation)
#    CANERM_RESTART          Name of restart file
#    CANERM_NOPREV           Numbre of previous simulation (For continuation)
#    CANERM_ISOLST           List of istopes
#    CANERM_ACCDATE          Accident date
#    CANERM_SIMDATE          Simulation date
#
# Remarques  :
#   Aucune.
#===============================================================================

function CANERM_Pre {

   taskstatus=0

   #----- Check if this is a new simulation or not
   if [[ ${CANERM_NOPREV} -ge 0 ]] ; then
      Log_Print DEBUG "This is not a new experiment"

      #----- Try to find previous restart locally
      restart=${CANERM_PREVDIR}/results/`echo ${CANERM_PREVRESTART} | sed 's/^.*\///g' | head -1`
      if [[ -r ${restart} ]] ; then
         ln -s ${restart} ${MODEL_TMPDIR}/oldrst
      else
         Log_Print INFO "Can't find restart on ${MODEL_RUNTYPE} host (${MODEL_RUNHOST}). Trying to get it from ${MODEL_LOCALHOST}."
         scp -p ${MODEL_USER}@${MODEL_LOCALHOST}:${CANERM_PREVRESTART} ./oldrst
         if [ $? != 0 ] ; then
            Log_Print ERROR "Unable to get restart from ${MODEL_LOCALHOST}."
            return 1
         else
            Log_Print INFO "Got the restart from ${MODEL_LOCALHOST}."
         fi
      fi

      #----- Check if grid has changed form previous simulation.
      if [[ ${CANERM_GRIDCHANGED} -eq 1 ]] ; then
         Log_Print INFO "Grid has changed, interpolating restart."
         CANERM_InterpolateRestart
      fi
   else
      Log_Print DEBUG "This is a new experiment"
   fi

   #----- Check for pre-calculated meteo.
   if [[ ${#CANERM_METEO} -gt 10 ]] ; then
      Log_Print INFO "Metfields are pre-calculated."
      if [[ ${MODEL_NEEDCOPY} -eq 1 ]] ; then
         scp -p ${MODEL_USER}@${MODEL_LOCALHOST}:${CANERM_METEO}/*  ${MODEL_RUNDIR}/meteo
         taskstatus=$?
         MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))
         if [[ ! ${taskstatus} -eq 0 ]] ; then
            Log_Print ERROR "Could not copy pre-calculated metfields."
         fi
      else
         for file in `ls -1 ${CANERM_METEO}/*`
         do
            ln -s $file ../meteo/`echo ${file} | sed 's/^.*\///g' | head -1`
         done
      fi
   else
      #----- Create the meteo
      ${EER_DIRSCRIPT}/Model_Meteo${MODEL_NAME}.sh ${MODEL_TMPDIR} ${CANERM_METEO} ${CANERM_FREQOUT} ${MODEL_PRE} \
         >${MODEL_TMPDIR}/Model_Meteo${MODEL_NAME}.out 2>${MODEL_TMPDIR}/Model_Meteo${MODEL_NAME}.err
      taskstatus=$?
      MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

      if [[ ${taskstatus} -eq 0 ]] ; then
         Log_MailIf "Meteorological preprocessing done (NORMAL)" ${MODEL_TMPDIR}/Model_Meteo${MODEL_NAME}.out
         Log_Print INFO "Meteorological preprocessing has runned successfully."
      else
         Log_Print ERROR "Problems in metfields calculations."
         Log_Mail "Meteorological preprocessing done (ERROR)" ${MODEL_TMPDIR}/Model_Meteo${MODEL_NAME}.err
      fi
   fi

   #----- Build links to meteo since CANERM needs it in the same edirectory it runs
   for file in `ls -1 ../meteo/*`
   do
      ln -s $file `echo ${file} | sed 's/^.*\///g' | head -1`
   done
   return ${taskstatus}
}

function CANERM_Post {

   #----- Get the results list from previous runs, needed for dosage calulations
   if [[ -r previous.in ]]; then
      for file in `cat previous.in`
      do
         name=`echo ${file} | sed 's/^.*\///g' | head -1`
         path=`echo ${file} | tr "/" "\\012" | tail -6 |  tr "\\012" " " | cut -d" " -f1,4 | tr " " "_"`

         if [[ -r ${file} ]]; then
            ln -s ${file} ${name}
         elif [[ -r ../../${path}/results/${name} ]]; then
            ln -s ../../${path}/results/${name} ${name}
         else
            Log_Print INFO "Can't find previous run file ${name} on ${MODEL_RUNTYPE} host (${MODEL_RUNHOST}). Trying to get it from ${MODEL_LOCALHOST}."
            scp ${MODEL_LOCALHOST}:${file} ${name}
            taskstatus=$?
            MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+${taskstatus}))
            if [[ ${taskstatus} != 0 ]]; then
               Log_Print ERROR "Unable to get  previous run file ${name} from ${MODEL_LOCALHOST}., skipping post-processing"
               return 1
            fi
         fi
      done

      #----- List result files from previous simulations
      no=1
      files=""

      for file in `ls *_*c` ; do
         nb=`printf %02i ${no}`
         files="${files} -file${nb} ${file}"
         no=$((no+1))
      done
   fi

   #----- List result files from current simulation
   for file in `ls ../results/*_*c` ; do
      nb=`printf %02i ${no}`
      files="${files} -file${nb} ${file}"
      no=$((no+1))
   done

   #----- Run dosage calculations
   Log_Print INFO "Running dosage calculations on following files:\n   ${files}"
   for iso in ${CANERM_ISOLST} ; do
      if [ ${iso} != TRACER1 -a ${iso} != TRACER2 -a ${iso} != TRACER3 -a ${iso} != VOLCAN ] ; then
         select="-dose 1 -conc 1 -depo 1"
      else
         select="-dose 0 -conc 1 -depo 1"
      fi

      ${EER_DIRBIN}/calc_dose_it_mf \
         -fconst ${EER_DIRDATA}/Specie.src \
         -iso ${iso} \
         -etiketi ${iso} \
         -datesim ${CANERM_SIMDATE} \
         -dateacc ${CANERM_ACCDATE} \
         ${select} \
         ${files} \
         -l ../tmp/listing_${iso}.out \
         -debug \
         > calc_dose_it_mf_${iso}.out 2>calc_dose_it_mf_${iso}.err
      taskstatus=$?
   done

   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+${taskstatus}))
   if [[ ${taskstatus} -eq 0 ]] ; then
      mv *_???p ../results/
      Log_Print INFO "Dosage calculations ended successfully."
   else
      Log_Print ERROR "An error occured during dosage calculations. (Exit code: ${taskstatus})"
   fi

   #----- Encode the pool info on all of the result files
   Model_PoolEncode

   return ${taskstatus}
}

function CANERM_Run {

   #----- Remove previous results, in case of relaunch.
   rm -f *_???[c,r]

   ${EER_DIRBIN}/${MODEL_NAME} \
      -initial oldrst \
      -climfld ../meteo/tape20 \
      -restart ${CANERM_RESTART} \
      -i ersinp.in \
      -in_freq ${CANERM_FREQIN} \
      -out_freq ${CANERM_FREQOUT} \
      -date NOW \
      -l canerm.out \
      > ${MODEL_NAME}.out 2>${MODEL_NAME}.err

   taskstatus=$?
   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

   #----- Verify if model has terminated successfully.
   if [[ ${taskstatus} -eq 0 ]] ; then
      mv canerm.out ${MODEL_NAME}.out
      mv *_???[c,r] ../results
      Log_MailIf "Atmospheric dispersion model done (NORMAL)" ${MODEL_TMPDIR}/${MODEL_NAME}.out
   else
      Log_Print ERROR "${MODEL_NAME} has encountered an error."
      Log_Mail "Atmospheric dispersion model done (ERROR)" ${MODEL_TMPDIR}/${MODEL_NAME}.err
   fi

   return ${taskstatus}
}

function CANERM_InterpolateRestart {

   #----- Lit les dimensions de la nouvelle grille
   read < griddef.in grid

   #----- Directives d'interpolation.
   cat << EOF_PGSM_RESTART > pgsm_restart.dir
 SORTIE(STD,500)
 GRILLE(PS,${grid})
 EXTRAP(0.0)
 HEURE(-1)
 COMPAC=-32
 CONV(XX,0.0,1.0,[0.0,1.0E37])
 CONV(DI,0.0,1.0,[0.0,1.0E37])
 CONV(WI,0.0,1.0,[0.0,1.0E37])
 CHAMP(XX,TOUT)
 CHAMP(DI,TOUT)
 CHAMP(WI,TOUT)
EOF_PGSM_RESTART

   mv oldrst oldrst.ori
   pgsm -iment oldrst.ori \
      -ozsrt oldrst \
      -i pgsm_restart.dir \
      >pgsm_restart.out 2>pgsm_restart.err

   taskstatus=$?
   if [[ ${taskstatus} -eq 0 ]] ; then
      Log_Print INFO "Restart has been interpolated successfully."
   else
      Log_Print ERROR "An error occured while interpolating the restart. (Exit code: ${taskstatus})"
   fi

   MODEL_EXITSTATUS=$((MODEL_EXITSTATUS+$taskstatus))

   return ${taskstatus}
}

