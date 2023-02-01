 ;Copyright notice:
;This is the RLOS (Relativistic Line-Of-Sight) code, version 2.10. This program is released under the LGPL-3.0-or-later licence, Copyright 2015-2019 Theodoros Smponias. 
;Please see the COPYING files in this distribution for copies of the licences.
;This program may call IDL routines provided by Andrea Mignone, with the PLUTO code, located in the same folder of this distribution.



pro just_for_copy_paste_first_time

;****************************************************************************
;110819 THE FIRST TIME WE OPEN THE PROGRAM, THE FOLLOWING PORTION, BETWEEN THE DOUBLE LINES, MUST BE RUN AS IS! ONLY EDIT PATH, HERE AND IN THE DATA FILE!

;140819 we might have to cancel first run if it gets stuck at some assignement (ctrl shift f6 keep pressed till end, then reset and rerun!)
;************************************************************************************************************************************************************************************************
;************************************************************************************************************************************************************************************************
maxdummy=15.0


DATAPATH='E:\scrange\neutrino_scale\torblob18dummies\'
cd, datapath
GDL_DIR='C:\Program Files (x86)\gnudatalanguage\gdlde'
PATH=!PATH+'C:\Program Files (x86)\gnudatalanguage\gdlde\'
!PATH=!PATH+datapath
file_early =DATAPATH+'rlos_params_v254.txt'
CLOSE, 18
 OPENR, 18, file_early
 ;110819 this rewinds file to its beginning! also put it to  main reading part, later on in this code!
 POINT_LUN, 18, 0 
 
; Read one line at a time, saving the result into array
text_array_early = ''
line_early = ''
WHILE NOT EOF(18) DO BEGIN & $
  READF, 18, line_early & $
  text_array_early = [text_array_early, line_early] & $
  ;array_double = [array_double, line] & $
ENDWHILE
; Close the file and free the file unit
CLOSE, 18
print,text_array_early
sfactor_external=text_array_early(8)/1.0D
pload_float_factor=text_array_early(66)/1.0D

print,'pload_float_factor,sfactor', pload_float_factor,sfactor_external
if (pload_float_factor eq 1.0) then print,'tsi' else print,'tsa'
;110819 for copy-paste initially, begin endif construct wont work line by line! single line if then else thing seems to pull the trick nicely for initial manual copy-paste!
if (pload_float_factor eq 1.0) then pload, 10,shrink =sfactor_external, dir=datapath,/float else pload, 1,shrink =sfactor_external, dir=datapath,/double
;110819 till here copy paste at starters, in order to call pload for the first time, while automatically having read the value of sfactor and the float/double form of binary data, from the param file! 
;110819 no need to manually alter sfactor and (float or double) up here each time we change those!
;************************************************************************************************************************************************************************************************
;************************************************************************************************************************************************************************************************
;090519 for flt reading need keyword, or messes with dbl.out etc!7
;MUST ALSO ALTER FLT KWORD LATER ON SOS SOSARA! FIND A BETTER WAY TO DOTHIS!!!
return
end






;***********************************************************************************
;***********************************************************************************
;131119 RLOS 2.10 code description.

;Make this also into a new section in the rlos paper.

;This is the latest version of rlos. Version 2 is a major upgrae of the original rlos code. This time the program is broken up into procedures and functions, with a modular structure.  

;The program allows the user to select which case to simulate, through an external parameter file. There is a unified approach, where the same modules operate on different geometries, through parameterization.  
;The user may select the values of the parameters of rlos version 1, and fully ermploy them . There is no moar a sifferent version of rlos1 for XZ and YZ plane image formation. Now, there is one version of the code for both cases. Furthermore, for each of those cases the user may select either radiograph or camera obscura imaging tachnique. 

;Radiograph has parallel LOS's, just like rlos 1. This means the film (fiducial imaging screen) is the size of the scene (grid), like an X-ray. The image shows clearely the various details of the system.   
;
;Camera obscura, or focused beam, on the other hand, has a focal point, where the eye of the observer is located. 
;The imaging screen, in cmera obscura, also of varied size: It may be equal, or smaller to the grid slice, at a given point along either x or y axis, depending on YZ or XZ imaging plane case.
;At the moment, screen must be parallel to the side of the grid where the proverbial imaging plane of the case is located, i.e. either XZ or YZ. 
;Screen location on-axis may vary within the grid. The smaller the screen, the smaller the image.
; The focal point may reside either on the side of the grid, or outside the grid, BUT within the limits of the projection of the proverbial XZ or YZ plane. i.e. It may have negative or zero axis position, but its two planar coords must be smaller than the grid size.

;LOS is drawn with a different set of azimuth (phi1) and elevation (phi2) angles for each LOS. Angles are calced using the focal point and the imaging screen point, which is the target point for the LOS. 
;The LOS then begins from the focal point, if it resides on the grid side, or from the LOS entry point, calced suitably (here recent relevant calc, upgrade of bversion 2.10). 
;From then on, it advances using aiming algos, trying to pass through the targeted screen point. It normally gets the target, or misses it closely!  In general, the higher the resolution, the better the accuracy in this respect.

;SOS for GR pseudo-Newtonian sim, logical next step is to introduce D(phi1), D(phi2), i.e. alter angles aALONG a LOS, from cell to cell, according to the effect of the potential. 
;Then, jet production may be imaged, if the hydrocode can employ external forces from a Kerr BHole.

;SOS ADD A FIGURE HERE in the rlos paper version of this text. 

;In this version, calculations may be done either ahead in time, or backwards in time, from a selected time instant (tpicked) backwards. For camera obscura, back in time is generally the correct way to proceed. 
;For radiograph, ahead in time also works fine, assuming a suitable fiducial setup of the jet system vs the observer. 
;tpicked is only employed when back in time switch is activated in the external param file. tpicked must be generally towards the end of the pre-selected range of dump files, or timeshots, to be loaded to RAM. Suffcient 
;backwards time range must be provided, for the LOS to travel back through time without reaching the beginning of the gris. Else, code cannot finish integration along LOS. 
;When testing, the facility of altering light speed, from rlos1, may be used to play and study this effect. 

;Pathfinding algos have been upgraded for this version. For each combination of XZ or YZ and radiograph or camera obscura, a certain set of such pathfinders are employed. 
;
;in order to find the beginning of main code, please search for the following two lines:
;
;210819 START OF MAIN PROGRAM! 
;
;290619 YO HO HO HERE WE GO!
;

;
;
;
;
;SUPER SOS corrected routin for bwd in time for both shotmin moar than 1 and shotmax less than shotnumber(tmax)
;
;;aaaa_back_in_time_tpicked=where((   (curtime_fun)  -   (t_fun))<0,count_tpicked_fun)
;so we amend it. 040123
;aaaa_back_in_time_tpicked=where((   (curtime_fun)  -   (t_fun))<0,count_tpicked_fun)
;;040123 count_tpicked_fun extracted above, is number of timeshots from tmax backwards to current timeshot
;
;
;
;;040123 SOS the following only works when we begin at shotmin=1
;shotnumber_fun= shotmax_fun-count_tpicked_fun-shotmin_fun

;040123 lets make it work with moar shotmin choices!
;040123 count_tpicked_fun is number of timeshots from tmax backwards to current timeshot
;shotnumber_fun= (size(t_fun, /n_elements)-count_tpicked_fun) 
;shotnumber_fun= shotmax_fun-count_tpicked_fun-shotmin_fun
;Absolute number of timeshot, in overalll time array!(size(t_fun, /n_elements)-count_tpicked_fun)
;shotnumber, though is measured from shot(tmin) to shot(tmax), in timeshot steps
;seems to work for both tshotmax<tmax and tshotmin > tmin=0
;shotnumber_fun=(size(t_fun, /n_elements)-count_tpicked_fun)-shotmin_fun
;
;
;020123  SOS corrected module than goes back in time, now seems to work fine! Pics now are NOT instantaneous any moar!
;
;311222 SUPERSOS change sign in coslosu, also in B, LOS! SOS LOS is coming towards us, whereas in the code it was considered to move away from us! 
;SOS fix it in both losu and losb. SOS!!! change sign!!!
;
;
;201222 SOS seems to work now, back in time! SOS verify it though!
;
;191222 SOS VERIFY back in time calc, especially latest effort to do this right thingy! debug mode employ! It seems to work, but verify it, then use it for SL, using two different tshot max.
;
;
; 
;290722 SOS we now did it, added multiplication factor the fast proton profile fn for the synchrotron case. Now verify it,
;and then consider adding it as an option to other emiss scenarios as well. Also, consider adding it to absorption as well!SOS
;
;;280722 SUPERSOS we add kappa value power law to emiss_4d, cause N0 density in pachz is FAST proton density,
;i.e. slow proton density times kappa_value_power_law. Latter is portion of fast protons out of slow proton total
;Change should olny affect scale of the result, as kabs remains a percentage-style thingy, whereas emiss result just jets multiplied now by a 10^-6 -type facxtor.
;SOS 280722 we now remain to add fast proton profile factor function in front of both here and in kabs.
;if(synchrotron_emissivity_switch eq 1.0) then(  emiss_4d=(kappa_value_power_law*c5_pachz(gammac))*rho_4d*( ( bfield_4d*sinlosb_4d )^( (gammac+1.)/2.) )*((nobs/2.)/c1_pachz)^((1.-gammac)/2.)*(dopplerfactor_4d*dopplerfactor_4d)   )
;170722 testing proved dfactor squared has many nans, so drop it for now then fix it
;180722 sos no problem, we just had the speed multiplier too high, 2.88. set it back to unity, and guess what, it now works like a charm!
;if(synchrotron_emissivity_switch eq 1.0) then(  emiss_4d=(kappa_value_power_law*c5_pachz(gammac))*rho_4d*( ( bfield_4d*sinlosb_4d )^( (gammac+1.)/2.) )*((nobs/2.)/c1_pachz)^((1.-gammac)/2.)*(dopplerfactor_4d*dopplerfactor_4d))

;
;
;
;190722 SOS we multiply din and so in by the length of the cell in cgs, in order to calibrate the total(eikona,/nan) result. Also, we
;calibrate the result by the distance to the system and the beam of the array/detector here on earth. Those are calibration multipliers
;and params. 
;but result looks ok, it drops.
;
;
;180722 was ok we just set speed factor adjustment to unity, and it works. But, the nan line appearts in the image
;;180722 so in=in/loslength(second_coord,nz10) this, loslength is zero sometimes and it causes some trouble.
;180722 SOS the nan line is perhaps related to the margins of the image plotted
;
;170722
;170722 dfactor squared did the trouble in early temp mapping
;print, min(coslosu_4d/ dummy1_4d,/NaN)
;print, max(coslosu_4d/ dummy1_4d,/NaN)
;stop
;surface, eikona, ax=75,/zlog, xcharsize=3, ycharsize=3, zcharsize=3
;;170722 find nans in kabs_4d:none for synchrotron pachz
;
;PRINT, WHERE(FINITE(kabs_4d, /nan))
;stop
;;170722 find nans in emiss_4d: plenty for emiss_4d in synchr pachz
;PRINT, WHERE(FINITE(emiss_4d, /nan))
;stop
;;first part of emiss_4d looks ok 170722
;  test_emiss_4d_part1=(c5_pachz(gammac))*rho_4d*( ( bfield_4d*sinlosb_4d )^( (gammac+1.)/2.) )
;  PRINT, WHERE(FINITE(test_emiss_4d_part1, /nan))
;stop
;;second part of emiss_4d has problem 170722
;
;  test_emiss_4d_part2=((nobs/2.)/c1_pachz)^((1.-gammac)/2.)*(dopplerfactor_4d*dopplerfactor_4d)
;  PRINT, WHERE(FINITE(test_emiss_4d_part2, /nan))
;  stop
;  ;170722 third part is ok
;test_emiss_4d_part3=((nobs/2.)/c1_pachz)^((1.-gammac)/2.) 
;PRINT, WHERE(FINITE(test_emiss_4d_part3, /nan)) 
;stop
;;part 4 problem dfactor
;test_emiss_4d_part4=(dopplerfactor_4d*dopplerfactor_4d)
;PRINT, WHERE(FINITE(test_emiss_4d_part4, /nan))
;stop

;150722 SOS pachz N0 is FAST proton base density, not slow proton base density at a distro (for us, a distro per cell).  
;150722 So, we multiply lowercase k by uppercase K: N=KN0 E^-gamma, whereas pachz has N0 to include K. So we use K. K is muchless than unity.
;150722 K is perhaps 10^-6, is the percentage (electron count portion, not energetic) of fast protons to slow protons. 
;150722 Top k was up to 1000? So if kappa is up to few hundrends, we are good. MULTIPLY k by K
;
;150722 SOS tested c5_pachz, c6_pachz, for values of spectral index argument from 0.5 to 6.0, and compared results to pachz appendix tables. 
;150722 agreement for c5 to 0.1%, for c6 to 1% tops. Seem legit then.
;
;140722 did bot emiss and abs coeffs, but now kabs seems to be more than unity, not allowed! 
;140722 see below for history of testing kabs from pachz. CGS units 
;nobs/c1_pachz is well bwlow unity, 10&-13 or something. but it is raised to a power of MINUS three, so it works out a large number, that outweighs 
;the first factors of the formula, which turn out to be small, but not small enough!
;WE NEED A KABS LESS THAN UNITY!
;SOS check pacholzyk for units of kabs SOS
;IDL> print, max((c6_pachz(gammac))*rho_4d*( ( bfield_4d*sinlosb_4d )^( (gammac+2.)/2.) )*((nobs/2.)/c1_pachz)^((-4.-gammac)/2.)   )
       ;167.14886
;
; print, max(( ( bfield_4d*sinlosb_4d )^( (gammac+2.)/2.) )   )
;       539.42527
;IDL> print, max(( ( bfield_4d*sinlosb_4d ) )   )
;       27.406816
;IDL> print, max((c6_pachz(gammac))*rho_4d*( ( bfield_4d*sinlosb_4d )^( (gammac+2.)/2.) )   )
;  3.6057914e-025
;% Program caused arithmetic error: Floating underflow
;IDL> print, max(((nobs/2.)/c1_pachz)^((-4.-gammac)/2.)   )
;  4.6355665e+026
;IDL> print, max(((nobs/2.)/c1_pachz)^((-4.-gammac)/2.)   )
;  4.6355665e+026
;IDL> print, max(((nobs/2.)/c1_pachz)^((-4.-gammac)/2.)   )
;  4.6355665e+026
;IDL> print, max(((nobs/2.)/c1_pachz)  )
  ;6.3795851e-010
;;
;
;
;130722 SOS did the emission coeff from pachz, was actually there all the time. Also, we did the nested routines var calls for the bfield, the sinlob, along with 
;basic checks for their behaviour and performance. Seems ok at first look at least. 
;Now, do the aborb coeff similarly, and then put in the units and make it full specific. Only then try to refix the hydromodel a bit. 
;110722 SUPERSOS we typed in pachz functions, from c1 to c6. Then, in order to implement those, we need to 
;1. Do the first four in a row after all fns and pros have been declared
;2. Then do the c5 and c6 as functions earlier, in the fn definition part of rlos2. 
;3. Do compare them with already existing fns, cp1 to cp6. Those are commented out for now, but do exist in the code. 
;4. Compare both ci fn groups to tabulated version of those, in Pachz appendix!
;
;
;090722 SOS added a function called coslosb, which is supposed to calc angle between LOS and Bfield. IT is modeled after coslosu. 
;Had to carefully add parameter passing variables  to nested functions in order to make it work! 
;SOS requires verification. Then, we add pachz coeffs, and good to go. 
;
;
;
;;080722 added this assignement, v_4d was declared but unused anyway, so no RAM use increase here
;v_4d=ccccc
;
;if(neutrino_emissivity_switch eq 1.0) then(emiss_4d=dummy3_4d_fun)
;290622 SOS we now do pachz sync emiss, so add a new switch here SOS
;290622 SOS  WTF IS THAT THETA THINGY??? CLARIFY SUPER SOS DO IT SOS ALSO DOES temperature and velocity play a role here? VERIFY SOS
;;080722 theta is angle between bfield and LOS must be calced similar to coslosu
;040722 SOS alphaindex_external is the spectral index as set in the data file.
;gammac=alphaindex_external
;080722 as a test, we assign velocity factor to synchr emissivity as well. SOS also, mind self absorption
;if(synchrotron_emissivity_switch eq 1.0) then(         emiss_4d=v_4d*rho_4d*((( sin(0.8) )^( (gammac+1.)/2.) ))*(dopplerfactor_4d*dopplerfactor_4d)   )
;040722 SOS old one to be remedied SOS if(synchrotron_emissivity_switch eq 1.0) then(         emiss_4d=cp5(0.)*rho_4d*( bfield_4d*sin(0.8) )^( (gammac+1.)/2.) )
;*( (ng/2.)/cp1(0.) )^( (1.-gammac)/2.) )            )
;040722 SOS 0.8 is the angle, from pachz, betwseem B field and LOS. So we shall calc it, 
;same way we did with (los,u)-coslosu.  let us call the angle losb
;040722 Also, we shall know that gammac is the spectral index of the p-law HE electron distro, already a model param. A function of
;that, and of some constants are the Pachz ci fns. SOS dig em up or recalc em. 
;040722 SOS verify Bfield is read OK from data and then proceed.
;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.)) 


;070722 SOS we play with dfactor, emiss_4d for synchr, etc and try to get a match with pics. almost there...
;
;;040722 SOS 0.8 is the angle, from pachz, betwseem B field and LOS. So we shall calc it, 
;same way we did with (los,u)-coslosu.  let us call the angle losb
;040722 Also, we shall know that gammac is the spectral index of the p-law HE electron distro, already a model param. A function of
;that, and of some constants are the Pachz ci fns. SOS dig em up or recalc em. 
;040722 SOS verify Bfield is read OK from data and then proceed.
;
;*****************************************************
;SOS 290622 EDO ETOIMO SOS to sync emiis apo palia, to theloume kai pali SUPER SOS this and pacholczyk SOS
; 
;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.))
;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.))
;
;
;************************************************************
;
;
;
;************************************************
;140720 dummy production in pluto.ini has now been corrected. 
;No dummy1 overwrite now. also, pload has been 
;re-fixed(was previously overwritten). Now, they work all together.
;Now, do a selection between 3 dummies and 18 dummies, both in rlos and nemiss. 
;Also, DO DOCUMENT codes.
;*************************************************
;
;
;
;;********************************************
;100720b SOS for some reason, even though it pretends to read beyond dummy 12, it still does not pload dummy>=13 
;as an array. it shows up as a scalar instead! Maybe 
;it is pluto? SOS check! dummy1/dummy15 thingy!maybe it is the flt.out file?  check!
;********************************************
;100720 SOLVED! EASY PEASY! dummy15 in pluto, overwrites the same name as dummy1. 
;So, when dumping, first it writes coslosu on dummy1, then overwrites that with 
;no 15's contents. Therefore, we see at dummy 1 the same stuff as in dummy15, 
;i.e. at nvar=11 the same as nvar=25! SOLVED! Just do the following :
;1. fix dummy1/15 thing in pluto. 
;2. make rlos read all of them dummies (not just 12 as is the case now), 
;preferably as a function of maxdummy. MAYBE add maxdummy to rlos, 
;or else read it from nemiss param file into rlos.  
;
;**************************************
;090720
;SOS dummy1 is dumped and then read from rlos at distorted values vs either dummy1 of nemiss or rlos's own coslosu.
;coslosu still matches nemiss's nvar11, or dummy1. Still, dummy1 is not read well from rlos. Just fix it! 
;maybe has to do with dummy1 appearing twice, or what? CHECK!!!
;
;090720 SOS when no arg2str error appears in pload, it 
;SIMPLY MEANS YOU NEED THE pluto's IDL routines in your dir! SOS!!!~!
;*******************************************
;FROM RLOS: 
;090720 SOS coslosu in idl is same as dummy1, nvar=11, in neniss. BUT 
;BUT dummy1 in rlos, as read from dumped data, is SMALLER thjan coslosu/dummy1 in nemiss. 
;HOW COME? check it out! is it divided by something dummy1 in rlos? OR WHAT? CHECK!
;L> print, max(coslosu_4d)
;      0.99987126
;IDL> print, max(dummy1_4d)
;    0.0043484597
;IDL> print,(dummy1_4d(1,30,42,29))
;    0.0042890380
;IDL> print,(coslosu_4d(1,30,42,29))
;      0.12167983
;********************************************
;from nenmiss! remember mathem is one indice up from rlos  
;    
;      Print[databig[[2, 11, 31, 43, 30]]  ] ;
;0.12168 (same as coslosu above)
;*********************************************
;
;
;
;080720 SOS coslosu_4d is same as mathemaTICa, BUT DUMMY1 IS OTHER THAN NVAR=11 OR DUMMY1 IN MATHEMATICA, IT IS SMALLER! sos CHECK HOW IT IS WRITTEN
;FOR NOW, USE COSLOSU4D HERE, NOT DUMMY1! OTHER DUMMIES ARE THE SAME, DUMMY IS IS NOTABLY SMALLER THAN EITHER COSLOSU, OR MATHEM DUMMY1! 
;DUMMY1 AND COSLOSU DISAGREEMENT FOUND! in SPECTRAL THINGY!
;*************************************************************
;060720 SOS THIS DATASET AND ALSO nemiss and rlos both 
;;param files give the nice result of JUST four points to be calced, ;
;out of many thousands! (filter setup, jet setup). SO it is easy to work with, 
;;so do save the whole setup (PLUTO 3 files, both param files, both codes!)
;
;***************************************************************
;060720 ALIGNED! VERIFIED VS NEMISS OF SAME DATE! dummy3, nvar=13, vortz nvar=10, dummy6 nvar=16!
;
;IDL> print,    dummy10_4d(4,13,14,15),   vortz_4d(4,7,8,3), dummy8_4d(3,31,47,30)     
;       14015.160       8009.0400  1.5394266e+009
;IDL> print,    dummy10_4d(4,13,14,15),   vortz_4d(4,7,8,3), dummy6_4d(3,31,47,30)     
;       14015.160       8009.0400  1.8477008e+013
;IDL> print,    dummy10_4d(4,13,14,15),   vortz_4d(4,7,8,3), dummy7_4d(3,31,47,30)     
;       14015.160       8009.0400  1.4241461e+011
;***********************************************************
;;060720 SAMLPE FROM NEMISS OUTPUT from big calc loop!
;
; tempovalue= {1.8477*10^13}

; lemissionspectrale[[tt,ii,jj,kk,i1]] (PRIN)= {1.8477*10^13}
;
;protou ayto! nvar,maxdummy,i1,tt,ii,jj,kk,(nvar-1)-maxdummy+i1= 29 15. 3. 4 32 48 31 16.
;Dimensions[databig[[tt,(nvar-1)-maxdummy+i1,ii,jj,kk]] ] = {1}
;Dimensions[databig[[tt,15.0,ii,jj,kk]] ] = {1}
;*******************************************************************
;
;
;;030720 IT now WORKS! with 12 dummies insofar! WE SHAL LTHEN TEST IT VS NEMISS and then add the rest of the dummies, IN CONNECTION TO THE maxdummy thingy! 

;
;
;300620 SOS this is to allow dummy15 spectrum stuff!
;maxdummy to be read from nemiss param file! SOS! FOR NOW WE JUST ASSIGN IT HERE!
;SOS we need to somehow make dependent how many of those 4D dummies it defines a function of maxdully! SOS DO IT SOMEHOW!
;maxdummy=15.0
;
;
;210320B it seems to actually WORK! using sample data points from all 4 geometry cases!
;
;;210320 seems to work for sample data point, especially using array indices, as above! vs the mathematica corresponding result!
;************************************************************
;;
;;IDL> print, where(  (dummy1_4d) eq .60477006)
;     1127305
;IDL> print, where(  abs(  (dummy1_4d)-0.65512) le 0.000001)
;          -1
;IDL> print, where(  abs(  (dummy1_4d)-0.65512) le 0.0001)
;     1558825
;IDL> print, coslosu_4d(1558825)
;      0.65512568
;IDL> print, dummy1_4d(1558825)
;      0.65512568
;IDL> print, dummy2_4d(1558825)
;  1.1470677e+012
;IDL> print, where(  abs(  (dummy1_4d)-0.65512) le 0.0001,/location)
;% Keyword LOCATION not allowed in call to: WHERE
;% Execution halted at: $MAIN$           7398
;  Q:\gitstuff\PAPER8_21dummies\rlos210_210320.pro
;IDL> location= where(  abs(  (dummy1_4d)-0.65512) le 0.0001)
;IDL> print, location
;     1558825
;IDL> indyc=array_indices(dimms,location,/dimensions)
;IDL> print, indyc
;           1          33          47          32
;;
;
;;***********************************************************************
;;200320B these three here! REPLACE VALUE OF COSLOSU with one read from mathematica output! 
;;then pinpoint if resulting coord is same as from mathem!
;importiva_coslosu=0.771628
;
;location= where(  abs(  (coslosu_4d)-importiva_coslosu) le 0.000001)
;locatione= where(  abs(  (dummy1_4d)-importiva_coslosu) le 0.000001)
;
;print, locatione
;print, location
;
;dimms=size(dummy1_4d,/dimensions)
;indya=array_indices(dimms,locatione,/dimensions)
;indyc=array_indices(dimms,location,/dimensions)
;
;print,dummy1_4d(indya(0),indya(1),indya(2),indya(3))
;print,coslosu_4d(indya(0),indya(1),indya(2),indya(3))
;
;print,dummy1_4d(indyc(0),indyc(1),indyc(2),indyc(3))
;print,coslosu_4d(indyc(0),indyc(1),indyc(2),indyc(3))
;;SOS 200320B diagreement here between coords output from above 3 lines(correct)
;;and mathem coords! whereas other arrays are OK! SOS dummy 1, what is wrong? 
;;SOS CHECK IT, trnsposing etc. vs huraah, etc!!! dummy1! SOS!
;;next we insert mathem coords! they are different than the ones we find here! 
;;200320B SOS CORRECT THIS!
;print, coslosu_4d(1,33,48,23)


;**********;***********************************************************
;
;
;
;
;
;
;
;
;
;************************************************
;;190320 SOS these allow to begin exploration of dummy 1 (read from mathem output) versus coslos4_4d (calced here!)
;190320 1-2 comparison of random datapoints between rlos and 
;nemiss_pload4d seem to achieve a macth by now, for all the 4 imaging geometries!
;But we need to work with how data are read from rlos, and how they are output from mathem!
;Achieve the final match of dummy 1 and coslosu here!
;print, min(coslosu_4d/ dummy1_4d,/NaN)
;print, max(coslosu_4d/ dummy1_4d,/NaN)
;****************************************************
;
;
;
;
;
;
;
;
;
;
;***********************************************************
;190320B
; find out non-zero elements of dummy1_4d array (depends on how much wee left it running in mathem!)
;print, dummy1_4d(where(  (dummy1_4d) ne 0))
;print, where(  (dummy1_4d) ne 0)
;print, where(  (dummy1_4d) eq .60477006)
;
;;190320 SOS these give the same position for the same value, for the same array! 
;;THIS SUPPOSEDLY SOLVES THE PROBLEM! BUT: why dont those numbers ;
;;appear at the sME 3D position?
;;190320 1D positioning is the same, but NOT 3D one! 
;;CAREFUL HERE, check also with huraaah, how is in 1D terms? TRY to 
;;understand how data are transfereed between the codes, 
;;based on both hurah and this 1D thingy!!
;print, where(  abs(  (coslosu_4d)-0.6047700) le 0.000001)
;print, where(  abs(  (dummy1_4d)-0.6047700) le 0.000001)
;*****************************************************

;
;
;
;
;
;
;;
;;190320 need to achieve match between dummy1 and coslosu here! find out non-zero elements of dummy1_4d array (depends on how much wee left it running in mathem!)
;print, dummy1_4d(where(  (dummy1_4d) ne 0))
;print, where(  (dummy1_4d) ne 0)
;print, where(  (dummy1_4d) eq .60477006)
;;
;
;180320 case 1 agrees with nemiss_pload4d, 
;but look into how case 1 (here in RLOS2) works at the moment, phi1fixed vs phi1, 
;cause resulting pic looks kinda weird, fo case 1 with both angles near zero!
;
;160320 CASE GEOM 4 (jet from the side, focused beam) lxi's and vxi;s are the same among rlos and pload4D, ands also rlos is consistenty in calcing aaaaa.
; Yet, pload4d gets aaaaa alitlle higher, for some reason! Angles also seem to be the same, but due to differing aaaaa, coslosu
;  for this case 4 appears alittle different! just 1-2 percent. 
;  Why aaaaa from pload is different? perhaps it is calced based of ii instead of iii, etc
;; i.e. not decreasing by one the coords to make same as idl rlos? PERHAPS CHECK aaaaa calc details in pload4d in mathem!
;
; print, lx1_fun(2,21,51,43)*vx1_4d_fun(2,21,51,43)+lx2_fun(2,21,51,43)*vx2_4d_fun(2,21,51,43)+lx3_fun(2,21,51,43)*vx3_4d_fun(2,21,51,43),aaaaa_fun(2,21,51,43),bbbbb_fun(2,21,51,43),ccccc_fun(2,21,51,43)
;   0.00017256760   0.00017256760       1.0000000   0.00027604559
;
;
;150320 vxi's look ok, only some slight (3rd -4th digit) misalignement in coslosu result! hopefully something minor left somewhere in the calcs!
;;keep checking, just bear in mind not 1000%o perfect data and results alignement! also, keep in mind that idl has minus one counting,. Maybe this vs 
;;counting tanphi loops from one, inst. of zero cause of slight misalignement? we'll see! JUST KEEP THIS NOTE!
;
;;Now attempt to check for rest of geometries! (this was for case 3 so far!)
;
;;140320 some verification stuff here, vs pload4d in mathem.
;140320 vxi are now DISALIGNED vs mathematica all else look OK
;
;
;140320 After setting tanphi12 margin to six, down from seven, 
;here it eems to work from zero to plus five, inst. of plus six! Wont work from one to plus six though!
;for zindigo=0.0,nz1_fun+5 do begin
; for yindigo=0.0,ny1_fun+5 do begin
;  for xindigo=0.0,nx_1_fun+5 do begins
;;
;**************************************************************************************************
;
;;130320 SOS this def from ZERO to plus six, makes necessary defining tanphi with plus SEVEN, thus ruining later on the size equality for lx1,2,3, aaaaa, bbbbb messed up, etc. 
;SO FIND A WAY AROUND THIS REQUIREMENT FOR A SEVEN in tan phi! e.g. these loops may go from ONE till the end, not from ZERO. OR SOMETHING ELSE SIMILAR SOS WORK AT THIS POINT NOW!!!
;130320 CARRY ON FROM HERE! SOS! 
;WHEN CORRECTED, REPEAT FOR REST THREE tanphi subroutines! SOS! here is tan phi1 XZ
;
;
;
;
;tan_phi1=dblarr(shotmax+ttt-shotmin,nx_1+7,ny1+7,nz1+7)
;tan_phi2=dblarr(shotmax+ttt-shotmin,nx_1+7,ny1+7,nz1+7)
;130320 SOS the above definition has plus seven, whereas others have plus six! this difference wreaks havoc in the sizes of lxi's 
;and cascades to the sizes of bbbbb! check how to do plus six, mabe using ttt param which os set to six! SOS!
;pload4d from mathem showed us this size must be the same across the board!
;130320 SUPER SOS plus six is NOT a PARAM, it is HARDWIRED as a precaution! KINDA safety margin
;IT IS INCLUDED ALSO IN PLOAD4D in mathematica!

;
;ccc, aaa are defined as a mixture of vxi and lxi, therefore have correct dims. bbb is sole lxi, so it has wring sozee!
;check bbbbb and also aaa mix for errors from mixed definition!
;
;;TRACK bbbbb and lxi definitions and CORRECT THEIR SIZE!!

;
;;130320 SOS following shows how array op for aaaaafun is OTHER THAN element-wise op for a 
;given element coordinate! W..F! check array-op for aaaaafun again!
;print, lx1_fun(2,27,71,33)*vx1_4d_fun(2,27,71,33)+lx2_fun(2,27,71,33)*vx2_4d_fun(2,27,71,33)+lx3_fun(2,27,71,33)*vx3_4d_fun(2,27,71,33),aaaaa_fun(2,27,71,33),bbbbb_fun(2,27,71,33),ccccc_fun(2,27,71,33)
;
;;SOS 130320 FOUND PROBLEM!!!  ARRAYS DIFFER IN SIZES SOS ELSE CALCS ARE GOOD!!! CORRECT ZE SIZES AND SHOULD BE FINE SOS!!!
;;nemiss in mathem helped us unveil bug in RLOS!! DO IT CORRECT SIZES OF ARRAYS TO BE THE SAME SOS!
;print, size(aaaaa_fun),size(lx1_fun),size(lx2_fun),size(lx3_fun)
;;vx1,2,3 seem of correct size
;print, size(aaaaa_fun),size(vx1_4d_fun),size(vx2_4d_fun),size(vx3_4d_fun),size(lx1_fun),size(lx2_fun),size(lx3_fun)
;;SOS 130320 bbbbb is also of the wrong size, like the lx1,2,3. 
;TRACK bbbbb and lxi definitions and CORRECT THEIR SIZE!!
;print, size(aaaaa_fun),size(bbbbb_fun),size(ccccc_fun),size(vx1_4d_fun),size(vx2_4d_fun),size(vx3_4d_fun),size(lx1_fun),size(lx2_fun),size(lx3_fun)

;**************************************************************************************************

;
;
;
;
;
;
;
;
;*********************************************************
;;120320  SUPER SOS

;the following 
;print, lx1_fun(3,29,68,33)*vx1_4d_fun(3,29,68,33)+lx2_fun(3,29,68,33)*vx2_4d_fun(3,29,68,33)+lx3_fun(3,29,68,33)*vx3_4d_fun(3,29,68,33),aaaaa_fun(3,29,68,33),bbbbb_fun(3,29,68,33),ccccc_fun(3,29,68,33)
;is DIFFERENT than the result of aaaaa_fun array op right below! WTF! INVESTIGATE!!!
;the analytical result is SAME as mathematica! but NOT the following array op output! sometimes
;it messes up the sign and slightly the value as well!!! IN here, not in mathem it seems! FIND OUT!
;
;print, lx1_fun(3,29,68,33)*vx1_4d_fun(3,29,68,33),lx2_fun(3,29,68,33)*vx2_4d_fun(3,29,68,33),lx3_fun(3,29,68,33)*vx3_4d_fun(3,29,68,33),aaaaa_fun(3,29,68,33),bbbbb_fun(3,29,68,33),ccccc_fun(3,29,68,33)


;aaaaa_fun=(lx1_fun*vx1_4d_fun+lx2_fun*vx2_4d_fun+lx3_fun*vx3_4d_fun)
;
;************************************************************
;
;
;
;
;
;
;
;
;
;080320 SOS KEEP THIS added bit EXACT to 0.0001 three zeros, TO BE THE SAME AS IN NEMISS_PLOAD4D SOS require consistency! !!! 


;080320 FROM NEMISS_PLOAD4d: 
;(*080320 SOS IN RLOS WE HAVE phi1, phi2 
;calcs in SEPARATE routines, THEREFORE SEPARATE CRITEWRIA ARE 
;NEEDED FOR EACH CASE! ALSO, REPEAT THE FOLLOWING CRITERIA 
;SEPARATION FOR ALL FOUR CASES OF GEOMETRY AND REVERSIBILITY 
;!! SOS DO IT 080320 ! CASE SOLVED !!! two criteria, one for each angle
; (cause when fixed phi23, the n phi1 was slightly off, whereas before it was
; the opposite! so, SEPARATE CRITERIA SOS!)

;**********************************************************************************************
;;SOS 070320 case closed, FOR NOW with phi2 slight discrepancy. Run moar tests plus PREP IT!
;;  ;070320 hereby define temp array ib order to compare sub-terms  from within tans_phi2_XZ
;       ;aim is to compare with nemiss corresponding terms. These arrays are local, so shall cease to 
;       ;;exist after this subroutine exits!
;       ;first define size-wise
;       tempyg1=tan_phi2_fun
;          tempyg2=tan_phi2_fun
;             tempyg3=tan_phi2_fun
;       ;rest within the loop that follows!
;
;
;
;140320 After setting tanphi12 margin to six, down from seven, 
;here it eems to work from zero to plus five, inst. of plus six! Wont work from one to plus six though!
;
;for zindigo=0.0,nz1_fun+5 do begin
; for yindigo=0.0,ny1_fun+5 do begin
;  for xindigo=0.0,nx_1_fun+5 do begin
;;      xcoord(xindigo,yindigo,zindigo)=xindigo
;;      ycoord(xindigo,yindigo,zindigo)=yindigo
;;      zcoord(xindigo,yindigo,zindigo)=zindigo
;;careful 170419 for neg tans wtf
;;add a bit to denom no zero div
;;       tan_phi1_fun(xindigo,yindigo,zindigo)=(yindigo-focal_point_y_fun)/(xindigo-focal_point_x_fun+0.0001)
;        
;       
;        tan_phi2_fun(*,xindigo,yindigo,zindigo)=(zindigo-focal_pointXZ_z_fun)/(sqrt( (xindigo-focal_pointXZ_x_fun)^2.0 + (yindigo-focal_pointXZ_y_fun)^2.0 ) +0.0001)
;         
;          ;070320  test stuff  here
;          tempyg1(*,xindigo,yindigo,zindigo)=(zindigo-focal_pointXZ_z_fun)
;           tempyg2(*,xindigo,yindigo,zindigo)=(xindigo-focal_pointXZ_x_fun)
;            tempyg3(*,xindigo,yindigo,zindigo)=(yindigo-focal_pointXZ_y_fun)
;          

;          
;          ;070819 added one to the denom to avoid div by huge number when zero denom!          
;
;;commented this out! 070819 only activate if abs. necessary!    
;
;         if (sqrt( (xindigo-focal_pointXZ_x_fun)^2.0 + (yindigo-focal_pointXZ_y_fun)^2.0 ) eq 0.0) then begin
;             tan_phi2_fun(*,xindigo,yindigo,zindigo)=(zindigo-focal_pointXZ_z_fun)/(sqrt( (xindigo-focal_pointXZ_x_fun)^2.0 + (yindigo-focal_pointXZ_y_fun)^2.0 ) +1.0)
;         
;       
;         
;;        if (debug_comments_fun eq 1) then print,'plus one denom in tan phi activated avoid div by zero!'
;        endif 
;
;;170419 for comparison with main loop ones!
;;       deltay_interim_signed_fun=double((ny10_fun-focal_point_y_fun))
;;deltax_interim_signed_fun=double((slice_x_location_fun-focal_point_x_fun));xfocalpoint is now default to zero probably anyway
;;deltaz_interim_signed_fun=double((nz10_fun-focal_point_z_fun)) 
;     
;  endfor
; endfor     
;endfor
;STOP
;;070320 SOS conclusion: sample sub-term numbers look the same among bot codes, yet for some reason mathematica calcs phi2 at a slightly different
;;value than idl!  unitadded looks the same, three zeros after decimal then unity! WTF! maybe something with the accuracy? leave it as is, use it like that. CAREFUL at extreme phi2 cases, though!
;print, focal_pointXZ_x_fun,focal_pointXZ_y_fun, focal_pointXZ_z_fun
;print,tempyg1(3,30,70,36),tempyg2(3,30,70,36),tempyg3(3,30,70,36),tan_phi2_fun(3,30,70,36),atan(tan_phi2_fun(3,27,73,31))
;RETURN, tan_phi2_fun

;**********************************************************************************************

;          
;
;
;
;
;
;
;ALSO 020320 we altered in nemiss, inter0,1,3 to inter minus 6, to make same as rlos, APART FROM FPOINT CALCS THAT IS! FOR THAT, the opposite happened, RLOS WAS CORRECTED, see next comment this space!
;
;;circa line 5370: 020320 ;ULTRA SOS WE USED nx_1, etc: NO!! nx1, needed! else: ERROR!!! SOS!!! KEEP OLD ONES, only alter intr0,1,3 in neniss! SOS!!

;141119
;figures do: a 3D-geometry figure (inclluding the screen, the focal point, the focused beam, the old one is radiograph, this one shall be fbeam, also XZ vs YZ basics do include in this fig), a back-in-time figure, a block diagram of the functions SOS important.

;SOS also do a figure for rlos2.10 externalparams like a table not figure

;ADD THE FIGURES INTO THE PAPER OF RLOS1 but do a new section and a new version of the paper. From that paPER, draw and do the 3-4 pgs new paper. 


;TBContinued







;****************************************************************************
;****************************************************************************
;100220 SOS here example 2 from idl help, shows how to transpose an array SPECIFICALLY swapping dims!

;Example 2 
;This example demonstrates multi-dimensional transposition: 
;
;; Create the array:
;A = INDGEN(2, 3, 4)
;
;; Take the transpose, reversing the order of the indices:
;B = TRANSPOSE(A)
;
;; Re-order the dimensions of A, so that the second dimension
;; becomes the first, the third becomes the second, and the first
;; becomes the third:
;C = TRANSPOSE(A, [1, 2, 0])
;
;; View the sizes of the three arrays:
;HELP, A, B, C 
;
;IDL prints: 
;
;A   INT  = Array[2, 3, 4] 
;B   INT  = Array[4, 3, 2] 
;C   INT  = Array[3, 4, 2] 

; 100220 based on the above example, we do the following AND IT FRIGGIN WORKS! vortz is tyransposed so as to only swap x,z. WE GOTTA DO THIS HERE, leave MATHEM out and PERHAPS TURN OFF THOSE xz reversal params in nemiss param file SOS!
;trans_vortz_4d=transpose(vortz_4d,[0,3,2,1])
;print, size(trans_vortz_4d,/dimensions),size(vortz_4d,/dimensions),size(dummy1_4d,/dimensions)
;SOS it works! from mathem array huraaaah, increasing x increments value in thousands, y in units and z in percents. 
;When checking that with the transpose above, it WORKS THE SAME AS IN MATHEM!!! 
;SO WE GOT IT! 
;(FOR some reason swap x,z not worked in mathem, perhaps huraaaah was BE4 loop, in array op! SO switch off x,z reversal, for starters. 
;;ALSO: DO CHECK REST OF DUMMIES, IN RELATION TO XZ REVERSAL IN MATHEM MEDDLING WITH IDL TRANSPOSITION EFFECTS! 
;ALIGN EVERYTHING, NOW ALMOST THERE!)
;ALL DUMMIES MUST NOW BE TRANSPOSED
;in rlos, after being re-read, after having being influenced from mathem, in order to have
;the correct form! FRom then on, we may employ em as intented, aka emission coeff, cell metrics, etc!






;****************************************************************************
;****************************************************************************
;010220 SOS adapted pload to accomodate 4 dummies total:dummies 1 to 3 AND vortz, also used as a dummy!
;ALSO COMPARED TO IDL AND DUMMIES ARE READ OK, JUST MINUS 1 in every dim, when 
;comparing, as idl begins array size at ZERO, whereas MAthemca BEGINS AT ONE!
;DO BACKUP PLOAD AS WELL! 
;REMAINS TO SEE COMPARISON OF coslosu etc as calced from IDL and also as calcxed in mathem and then dumped to dummies SOS cpmparison DO!
 

;***********************************************************************************
;***********************************************************************************
;171119 tpicked is a param read from external param file! CAREFUL HERE! clarify we now set tpicked to t(shotmax)! We hare NOT USING tpicked any moar! SOS!
;tpicked_fun=t_fun(shotmax_fun)






;161119 SOS erased, circa line 5000, the re-opening of param file!

;***********************************************************************************
;NEMISS comment, also applies here indorectly!
;
;071119 it works!!! we have the following: 
;lxi's: the same x and z between rlos and nemiss.
;vi's:  OPPOSITE (x of rlos is z of nemiss and vice-versa)
;phi1,phi2 the same from rlos to nemiss
;aaaaafun: OPPOSITE z and z among rlos and nemiss
;cccccfun:the same
;SO we did a 'reverse' function for aaaaa and there we reversed vx and vz. We then also created a cososureverse which finally MATCHED rlos's one!
;
;Do tidy up here and make it nice 
;
;SOS after some points here, uanl gives ZERO! WTF? CHECK it out, esp. vs above loops! What vould it be?
;;
;*********************************************************************************8
;
;
;061119 yesteray at laptop VM it wokred!!! rlos matchhed speeds and lxi's, so it was an rlos later than toay's!!! SOS check andd compare laptop's latest in VM with this one!!! 
;it worked there, only asaaaa,bbbbb,cccc were left out! Now speeds wont match here!!!! ALMPOST THERE, do it!!!


; sos 3001019 we re-instated vx1,vx2,vx3 being calced properly, they were not! vx2,vx3 WTF!
; ALSO, we deducted shot min from sthotnumberfun for back in time calcs, now those may start later than shotmin=1,i.e.cam obsc may have shotmin  moar than 1!
;  
;  210919 this append to the match varname pro within   "dummy1":  BEGIN dummy1  = vpt & match = 1 & END
;     "dummy2":  BEGIN dummy2  = vpt & match = 1 & END
;          "vortz":  BEGIN vortz  = vpt & match = 1 & END











;;*****************************
;221019 this portion between the lines!
;;191019 now attempting to allow for negative FPoints!
;;211019 we were wrong the other day! los originx if fpoint is neg is not zero! same triangles. One calc for y, one for z! 
;;We put neg coord of fpoint stuff AFTER, to avoid messing settingsif pos or neg fpoint coord!CRUCIAL use LT not le! zero case later on!
;if (focal_pointXZ_y_fun lt 0.0) then begin 
;;221019 now we use 1.0, since los is definately entering the grid somewhere at the slice one step after the boundary!
;los_origin_y_fun=1.0
;;*******************************************************************
;;221019 these two for x coord.
;if (focal_pointXZ_x_fun gt second_coord_fun) then begin 
;los_origin_x_fun=second_coord_fun+ ((sliceXZ_y_location_fun)/(sliceXZ_y_location_fun+abs(focal_pointXZ_y_fun)))*(focal_pointXZ_x_fun-second_coord_fun)
;endif
;
;if (focal_pointXZ_x_fun le second_coord_fun) then begin 
;los_origin_x_fun=focal_pointXZ_x_fun+ ((abs(focal_pointXZ_y_fun))/(sliceXZ_y_location_fun+abs(focal_pointXZ_y_fun)))*(second_coord_fun-focal_pointXZ_x_fun)
;endif
;;*******************************************************************
;;next twofor z coord!221019
;if (focal_pointXZ_z_fun gt nz10_fun) then begin 
;los_origin_z_fun=nz10_fun+ ((sliceXZ_y_location_fun)/(sliceXZ_y_location_fun+abs(focal_pointXZ_y_fun)))*(focal_pointXZ_z_fun-nz10_fun)
;endif
;
;if (focal_pointXZ_z_fun le nz10_fun) then begin 
;los_origin_z_fun=focal_pointXZ_z_fun+ ((abs(focal_pointXZ_y_fun))/(sliceXZ_y_location_fun+abs(focal_pointXZ_y_fun)))*(nz10_fun-focal_pointXZ_z_fun)
;endif
;**********************************************************************************************************************

;221019 now REPEAT THAT FOR imaging case 4 as well! also comment out previous version of those below!



;*******************************





;211019
;SOS neg fpoints do work, nut fpoints also have non zero y,x etc, they do not lie on central axis! so, they must account for thst difdference as well, i.e. subtract from vertical coord as well, but in a signed manner! 
;;we must ADD the difference to fpointx if second coord is larger and subtract it if it is smaller THINK it! SOS ! 
;incomplete as yet, but almost there!!! geometry and calc! abs(A)/A gives sign, then multiply by self, something like that do!

;191019 now attempting to allow for negative FPoints!
;211019 we were wrong the other day! los originx if fpoint is neg is not zero! same triangles. One calc for y, one for z! 
;We put neg coord of fpoint stuff AFTER, to avoid messing settingsif pos or neg fpoint coord!CRUCIAL use LT not le! zero case later on!
;if (focal_pointXZ_y_fun lt 0.0) then begin 
;
;los_origin_y_fun=1.0
;;211019 we must ADD the difference to fpointx if second coord is larger and subtract it if it is smaller THINK it! SOS ! incomplete as yet, but almost there!!! geometry and calc! abs(A)/A gives sign, then multiply by self, something like that do!
;los_origin_x_fun=fix( ( (abs(focal_pointXZ_y_fun))/(abs(focal_pointXZ_y_fun)+sliceXZ_y_location_fun)   )*(focal_pointXZ_x_fun+(second_coord_fun-focal_pointXZ_x_fun))   )
;los_origin_z_fun=fix(   ( (abs(focal_pointXZ_y_fun))/(abs(focal_pointXZ_y_fun)+sliceXZ_y_location_fun)   )*(nz10_fun-focal_pointXZ_z_fun)     )
;endif





;211019 SOS we were wrong the other day! los originx if fpoint is neg is not zero! Employ 'same triangles geometry'!. One calc for y, one for z! 
;
;imaging case 4: 
;if (focal_point_x_fun le 0.0) then begin
;los_origin_x_fun=0.0
;los_origin_y_fun=( (abs(focal_point_x_fun))/(abs(focal_point_x_fun)+slice_x_location)   )*second_coord_fun
;los_origin_z_fun=( (abs(focal_point_x_fun))/(abs(focal_point_x_fun)+slice_x_location)   )*nz10_fun
;endif


; 191019 now attempting to allow for negative FPoints!
; SOS lines circa 2350 AND 2375
;if (focal_point_x_fun le 0.0) then los_origin_x_fun=0.0
;
;
;210919 dummies are now read, from a MOdIFIE PLAOD.pro, we added dummies to the common block AND to another suitable place SOS within pload! 
;SAVED it with a backup name and also to pload.pro itself. SOS also x is z and z is x between IDL /GDL and mathem/octave. 
;SOS AN ALSO idl begins arrays at 0, mathem at 1! Else, they match element value to element value. SOS!


;170819 for now do some tests and a lot of commenting, de-centralized, within pro's and functions!
;160819 re-check how emission is if we do not account for spectral effect from formula!! We take density as the base for emission coefficient, and then add, on top of that, special relativistic effects!
;160819 the influence of freqfactor is included within jet_spectrum108, in ncalc specifically
;;if (spectrum_direct_switch eq 1.0) then (  emiss_4d=rho_4d*(dopplerfactor_4d)*jet_spectrum_built_in_function_test_unity_at_108  ) else (  emiss_4d=rho_4d*((dopplerfactor_4d)^(3.0+alphaindex))*freqfactor  )
;;170819 yes to the above line, but then we also have in the implied case the presence of both nu^alpha ANDfreqfactor!! REDUNDANT? APPLES TO APPLES BABY! DOIT FOR COMPARISON's sake!
;170819 two Dfactors from time dilations, 1 plus alpha from freq shift! so we miss one Dfactor here:
;keep old one for backup! if (spectrum_direct_switch eq 1.0) then (  emiss_4d=rho_4d*(dopplerfactor_4d)*jet_spectrum_built_in_function_test_unity_at_108  ) else (  emiss_4d=rho_4d*((dopplerfactor_4d)^(3.0+alphaindex))*freqfactor  )
;170819 in the old one, no need for freqfactor in implied form!!! CAREFUL! 

;160819 series of calling: 
;1.global calculations 
;2. freq shift effect
;3. emiss global calc

;160819 we have a direct spectrum (unity at 10^8 calibrated formula, etc: SOS VERIFY how many dfactors are actually needed here SOS) and an implied spectrum (the old existing one).
;160819 remain to better compare the two spectra. SOS


;160819 re-check how emission is if we do not account for spectral effect from formula!! We take density as the base for emission coefficient, and then add, on top of that, special relativistic effects!
;160819 the influence of freqfactor is included within jet_spectrum108, in ncalc specifically




;160819 this whole freq thing is the user input spectrum. 
;160819 There is also the ability to employ the freq effect directly into the DFACTOR formula, as an additional dfactor raised to a suitable power of alphaindex.
;160819 Arrange for a selection between the two, and prove they work by matching them to a calibration.

;*********************************************************************************************************
;*********************************************************************************************************
;150819B SOS here it is, globally apply that spectrum and see what happens! 
;150819B Pick random points, and test: print their dfactor, their ng, their ncalc and their spectrum. And that spectral value is your moderation, modulo(unity at 8GHz))
;jet_spectrum_built_in_function_test_unity_at_108=kappa_spectr2*ng^(-alphaindex_external)
;150819 actually, we should apply the spectrum moderation to the intensity, on the ncalc freq!
;150819 ncalc is taken from the following 4d array formula, from the procedure global_calculations.
;
;;SOS 150819 MAKE SURE THIS ONE PRO IS CALLEDAFTER  GLOBAL_CALCS!!! SOS
;ncalc=ng*(1/(dopplerfactor_4d+0.000000001))

;THIS ONE UNCOMMENT IN THE RELEVANT PRO: jet_spectrum_built_in_function_test_unity_at_108=kappa_spectr2*ncalc^(-alphaindex_external)
;*********************************************************************************************************
;*********************************************************************************************************











;**************************************************************************************************************************************************************************************************************

;140819 Mathematica needs a second dummy var as well, in order to include the different angles phi1, phi2 (ratio1f, ratio2f), which are different per LOS, for camera obscura. 
;140819 For radiograph, its OK, as the two angles are the same all over!
;;**************************************************************************************************************************************************************************************************************

;
;
;**************************************************************************************************************************************************************************************************************
;**************************************************************************************************************************************************************************************************************
;140819 SOS circa line 750 do finish the stuff related to spectrum! put it as an option, AFTER it is finished!
;140819 We now attempt to do the color map, i.e. eikona_color (2d array), which is an image of frequency shift NONONO! 
;per cell, there is a separate color shift!!! How to do the grand total!! difficult!!! so, need to select an observing frequency, and also to bear in mind a
;jet-frame spectrum!!! Then, shift differently per voxel, then find out, going backwards in freq, from where in the original spectrum does it originate, so that it ends up at the obs. freq!
;from that, deduce its intensity! OOPS!!! voila!!! DO THAT NOW!!! SOS per cell, it affects the intensity!!! when FS is turned ON. 

;140819 start by doing a global 4d calc, whereby each voxel is assigned a simple scalar freq shift. From that, based on a given spectrum, we go backwards and deduce emitting intensity, i.e. distort the base intensity (e.g.)
; at a given freq, i.e. point along the base spectrum, we got 100 percent. The relative intensity percentage then multiplies the density, or whatever intensity we get from the rest of the model.

;140819 for e.g.: say we have a freq redshift, by 50 percent. So: if e.e. nobs=8 GHz, then we see that as 4GHz. Now, back to the base spectrum. There, say we have 100 units at 10 GHz, 80 at 8GHz, 50 at 4GHz. Then, we get 5/8 of 
;the intensity, since we get to see what inherenlty is emitted at 4 GHz, at 8GHz. Just an example really.

;140819 So we need: a global freq shift calc (probably already there somehow) and a link between that and the intensities, trhrough a base spectrum!!
;**************************************************************************************************************************************************************************************************************
;**************************************************************************************************************************************************************************************************************







;
;
;
;
;







;130819 example 2 now seems to only require fine tuning to get ze image out of it. ALSO PUT IN MANUALLY THE LENGTH CONTRACTION, like the source did!
;130819 example 2, planar beam, is OK in terms of x coverage! Max alongx is nx_1-2, i.e. 22! since nx_1  is already at minus 6!

;100819 CAREFUL now reverse pic seems to work, nut! endlosloop is around 60 percent of los length, then reverse only gets thet6ailofthelos, straight only gets the beginning half or so!
; 100819 need endlosloop  to exactly equal eachj los's cell count length, in order to preserve symmetry, when no absroption. 
;100819 ALSO: try testing WITH FAST clight=400 etc. ELSE, asymmetry occurs due to meeting earlier snapshots from one of from the other side of the grid!


;090819 reverrse went at the end of losloop, after the endfor, before the end of the mother pro. Still needs clearing out what the reverse does, at the beginning!!! SOS!!!
;090819 INVERSE los is the name of the day's work today! SEt it up!
;080819  SOS for absorption when goig back in time in drawing the los, i.e. cam obsc, then we need to re-activate REVERSE LOS SOS DO IT!!! rev. los already exists, but has not been added to the unified scheme as yet!
;080819 do add the existing reverse los to the uni scheme, for completeness!







;080819  SOS for absorption when goig back in time in drawing the los, i.e. cam obsc, then we need to re-activate REVERSE LOS SOS DO IT!!! rev. los already exists, but has not been added to the unified scheme as yet!
;080819 do add the existing reverse los to the uni scheme, for completeness!


;***************************************************************
;;070819 IT WORKS!!! 
;if (abs(ratio1c) ge abs(ratio1f_fun)) then (rc_fun=rc_fun+fix((ratio1c-ratio1f_fun+0.01)/abs(ratio1c-ratio1f_fun+0.01)) ) else  begin
; uc_fun=uc_fun+1 
 ;070819 if inside cone, if correct side, it raises one, else towards axis! hope behaves well at crossing zero! well see!
 ;070819 no,this ruins things!  if (ratio1c lt ratio1f_fun ) then (rc_fun=rc_fun+1) else (rc_fun=rc_fun-1) 
 
 ;070819 keep old one for ref if (ratio1c*ratio1f_fun lt 0.0) then (rc_fun=rc_fun+fix((ratio1c*ratio1f_fun)/abs(ratio1c*ratio1f_fun) +0.001 )  )
 ;070819 some drastic action! It should only have to work just once, to take us across the zero, hopefully early on along the LOS! AND IT WORKS!!! OK!
 ;
; THIS WORKS HIHIHI! DRASTIC, ACROSS THE ZERO!  if (ratio1c*ratio1f_fun lt 0.0) then (rc_fun=-rc_fun )
; endelse 
;****************************************************************



;070819 criterion is ok, we simply need to play with rc for xz ratio1, not touch uc! uc cant go neg in xz mode!!!
;070819 IT IS OK!! SOS this break criterion is totally bogus! it does not allow progress below zero!!! quax quax quax!!! CORRECT IT! ALSO ITSCOUNTERPART FOR RATIO2???though that seems to work OK, so just compare with it!
;if ( (uc_fun+los_origin_y) lt 1.0 ) then break

;060819 kicked the hornet's nest: 1. we fixed back in time to start from less than maximum. 2. detected problem with ratio1 (not ratio2) in back in time, when ratio1 is neg! 
;060819 CAREFUL . Also check it for fwd in time!!!
;******************************************************************
;
;
;
;
;030819 Both redecing and approaching rods now operate. Ruler exists, but still uncut! TBC!

;020819 line 2395 circa set receding centre etc do that!
;010819 We now have it working for 4 geometries and also for both data and steady-state. 1st test works, we also need its freq-color variant. We now do test no 2.
;010819 New function copy for planar beam, using yesterday;s newly defined object. The latter woked today!



;300719 freqfactor is the ratio of shifted freq to jet frame frequency 

;290719 CAREFULsteady-state jet lies along Y axis!
;290719 invalid this comment! so we image it opposite ways! i.e. DBOOSTED when YZ, laterally when XZ!!

;270719 Density rho is overwritten on perp beam, so: moved per_beam and planar beam function calls OUT AND AFTER data-loading time loop. Reason: they already have their own time loop!
;270719 Also: raised perp beam density way higher than usual jet density, so even in case jet is overwritten on it, it still stands out like a sore thumb, if perp beam is called.
;;270719 ALSO: make two moar functions: One for rod_test_switch,  and one for steady_state_switch. 
;
;;260719 Introdyucing OLD_ONE pathfinding algo pros for the raqtio1 and ratio2, solely for imaging case 4. We add yet another param to the param file, in order to select this one thing. 
;260719 Our aim is to be able to directly compare to the older one, esp. in the relat. test runs!

;260719 this one does NOT reproduce old one,yet superpositions the intended oval shape on a background, that shouldnt be there in the first place. 

;260719 SOLVED MAYBE!!!!  circa line 4177 !!SOS potentially call test fn's AFTER main ASSIGNment of density! Else AFTER the test's rho assignement, it gets OVERWRITTEN by main density!!! ET VOILA ZE OVAL SUPERSEDED ON BACKGROUND!






;250719 The old one, as is now, works fine at high res, COMENTS OFF! The new one, i.e. this one, nneds nx1*0.9 in perp fn and then prints sort of a negative of the oval output. 
;250719 MAYBE IT IS THE PATHFIND ALGOS FOR CASE 4!!! TRY ZE OLD ONES IN A SUITABLE FUNCTION, need to match the old and the new one!!! FIRST TEST WORKED IN OLD! MUST WORK HERE AS WELL!!!
; 
;
;240719 DID pros for pathfinder algos.case 4 is NOT what we had a few days ago, prob affected by case 3 work. BRING TAT BACK ASANOPTION, keep all as options somehow! 
; 240719b UPDATE: CASE 4 WORKS CURRENTLY AS INTENDED SOS. But there was a case 3 yesterday that looked a tad bit better. Check that out as well!

;1st criterion: if (abs(ratio1c) gt abs(ratio1f_fun)) then (rc_fun=rc_fun+1) else (  uc_fun=uc_fun+fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))  ) 
 
;2nd criterion: if (abs(ratio2c) gt abs(ratio2f_fun)) then (rc_fun=rc_fun+1) else (  cc_fun=cc_fun+fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c))  ) 
;in case of ratio2c stagnation, we add the folowing: cc_fun=cc_fun+1. Thiis erases the valley, but adds faults at the bottom ohalf of y axis. Else, we get the valley!


;stagnation criterion of ratio1: if ((abs(ratio1c) gt abs(ratio1f_fun)) eq 0) and ( fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01)) eq 0  ) 

;stagnation criterion of ratio2: if ((abs(ratio2c) gt abs(ratio2f_fun)) eq 0) and ( fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c)) eq 0  )  then begin


;200719 PRINT and think the above on paper!

;200719 perhaps do a separate stagnation image for each ratio 1 or 2!
;200719 WE CAN DO surface imaging ta counterlast kai loslength! einai both 2D arrays!
;loslengths along the jet are very small now!!! Check stagnation count image!
;200729 Did it! stagnation count along jet is TOO high! in the thirties! So los runs out of count, due to zig zag factor cutting in! Must correct algo!
;circa the very end of this file! surface, (stagnant_signal), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4
;200719 Suggested solution. When stagnation is detected along the los (within stagnant's  if then begin endif structure), do apply a costlier quantitative criterion (such as which ratio, 1 or2, deviated the most, alter that one, etc).
;
;;try this! 200719SOS THIS REMOVED THE VALLEY, but blurred the left side! SECOND PART OF LOS LOOP! ratio2f was causing the problems!
;cc_fun=cc_fun+1
;
;190719 We have now tested it vs 3 single versions. Both radiograph versions give largely the same result. Cam obsc YZ gives something BETTER! Probably outr corrections to llos calcs etc yesterday fixed something there!
;190719 Only a valley along the jet seems unreal,  MUST check pathfinder algos! Yet, looks better than cam obsc YZ's result! 
;190719 Did not have somethinh to cpmpare to for the cam obsc XZ version. Yet, its pathfinder algos fail for neg phi2 at least for that! 
;190719 MUST check pathfinder algos for COBSCura, to fix the valley in the YZ result and the nans in the XZ result!
;190719 SOS older versions work well with latest param file, since they simply wont read past their max line count, ignoring newer params!
;190719 So it is OK to have older versions read from the LATEST param file, for easier comparison!
;
;
;180719  UF!we had fpXZ'x determined by a percentage of ny, which was larger than the correct nx, thus triggering the breaks! All day was looking for it!
;180719 XZ camera obscura seems to NOT work well with negative angles for ratio1f! look to it! Notnow! (pathfinder algos check! Alsao compare to relevant behavior for YZ version). DID ALL 4 CASES!!! 



;170719 SOS must alter the  ratio functions! tanphis global  maybe not employed after all! MUST tidy thing up!;180719 tan phis are USED IN CALCING ze lx1,2,3 in ze cam obsc  case! prominently! So keep them!

;180719 tan phis are USED IN CALCING ze lx1,2,3 in ze cam obsc  case! prominently! So keep them!
;170719 SOS MUST ALSO ADIT tanphi global calc function for XZ cam obscura!! SOS!!! DO IT!!!
;
;150719 idl array index begins at zero
;170719 yes, but this messes all others now! Now we must load till the one shot before the last one! In the param file!
;;170719 Please note that  relativistic imaging tests are meant to work with camera obscura YZ version only!

;170719: FROM line circa 4341: WTF is this relic? This only works for YZ cam obscura!
;llos=sqrt( ((dlc*(nz1currentlast-focal_point_x))^2)+((dlu*(ny1currentlast-focal_point_y))^2)+((dlr*(nx1currentlast-focal_point_z))^2) )
;170719 Try this:
;llos=sqrt( ((dlc*(nz1currentlast-los_origin_x))^2)+((dlu*(ny1currentlast-los_origin_y))^2)+((dlr*(nx1currentlast-los_origin_z))^2) )
;170719 SOS all adjustements for length from zig zag were wrong for radiograph cases! SOS!!!
;
;
;160719b IT WORKS FOR XZ RADIOGRAPH AS WELL!!! So we now got all three existing cases: cam obsc yz, radiograph yz, radiograph xz! Remains to do, the cam obsc xz, which is not there yet in an older version!
;
;
;160719 so far: worked for both yz cases, using the same code! But: still to do ze xz radiograph case correctly!
;160719 erased a silly t0los=0 assignement, was messing timing calcs along LOS!
;160719 nx1current, ny1current. nz1current assignements differentiate between xz and yz at the end of the day!
;
;130719 At long last, seems to work for radiograph yz (Already worked on camera obscura yz). Now remains to check lx1,lx2,lx3 for DB calc, and also to do the xz radiograph case. 
;130719 Then, we must also define the camera obscura xz case. 
;130719 Then, also SOS COMPARE DIFFERENT PATHFINDER ALGOS, ON THE SAME PROBLEM OF RADIOGRAPH (cam obsc only accepts the more complex algos)

;130719 please note how in case of cam obsc, ratios are re-calced in the imaging loops, not based on globAL tan phi calcs in the angles initialization procedure!
;130719 MIGHT BE A GOOD IDEA TO COMPARE THE TWO, i.e. ratios from here and ratios from there! TRY IT SOS!!!

;120719 SOS ratio1c, for radiograph case, seems to work ok. not so much ratio2c. BUT ratio1f still changes between LOSes? WTF! 
;In radiograph, ratio1f, ratio2f must be constant all over! SOS find out about ratio2c and ratio1f, ratio2f in radiograph!
;
;
;110719 debug the ratios and also after that do a procedure for the pathfinding algos for each case! Closing on it!
;100719 fixerd it, works ok, yet gives a different result! MUST BE THE CRITERIA for selecting direction! SOS try using the old ones for the radiograph case and SEE!
;090719 SOS lloscurrent seems to have the problem in time location procedure, check it next!
;090719 time_location we adopt separate calcs for curtime for back in time and for fwd in time. Also, in main body of code,
;circa line 2668, we also adopt separate calcs for shotnumber initialization
;080719 focused beam switch is now determined at the imaging loop boundaries procedure, in relation to the selected geometry
;imaging_loop_boundaries,focused_beam_switch_fun
;050719 SOS time reversal in cam obscura, Normal time direction in radiograph.SOS arrange for a selection! 
;050719 circa line 1485: shotnumberfun=.... SOS generalize that!
;040719 there is also the switch: focused_beam_switch, 1 is camera obscura, either xz or yz, other (false) is radiogtaph. 
;030719 We got it working for both camera obscura and radiograph for yz (cases 2 and 4) so far. BUT, it bring up some kind of test, not the actual data! See to it!
;030719 And try to add radiograph xz (already done as standalone) and then also prepare a cam obsc xz (not yet implemented!) as well!
;020719 SOS major overhaul today! before imaging loop, imaging_loop_boundaries procedure is called! Sets image boundaries.
;020719 Within imaging 2D loop, and before losloop, the los_origings pro is called, sets the los origins, in relation to the current   
;imaging loop indices(imaging pixel coords).
;;020719 SUGGESTED: set it up at case 4, re-run it, and when it works OK with that, THEN proceed to do the rest!
;ALSO, must do a xz cam obsc focal point, on the other side of the box!
;
;290619 IT FINALLY WORKS, just for case 4 at the moment, that is camera obscura YZ. But it shows what also the version 
;of 010619 shows, which is not the same as 180619, so please check their relevant setups! which test is shown here? perhaps we still
;have hardwired params in relation to tests hardwired in the code, which are not present in the param file. 
;290619 WE ALSO need to do a camera obsc. XZ case. ALSO: do incorpotrate now both radiograph versions. START WITH YZ maybe!
;280619 line 3796 counterlast, loslength give infinites to the in final calc! COUNTERLAST DOES NOT PASS FROM 
;LOSLOOP TO PARENT IMAGING LOOP sos MAKE SURE IT DOES!
;270619 nx10,ny10,nz10 do NOT change from los to los! for cam obsc! but they must change! they are screen pixel coords, they loop!
;270619 we have now replaced ny10 with second_coord in 2D imaging loop, especially in ratios function calls.
;260619 works, but does not give full image, only first line! run it side by side with 010619 version, on the same data. Find out what is wrong!
;we did some initializations and also included lloscurrent a s a function. SOS it seema to be about param passing among nested functions. Avoid complexity everywhere! Simplify at max!
;180619 MAJOR OVERHAUL AHEAD: replace x,y,z with second/third or third/second coord, and z always first coord. (first is outer coord). Also do overhaul functions. JUST do it, then we got generality, as desired. 
;;Take a working version, such as only losloop active, no camera obscura outer function! circa 5-6 june 19, perhaps!. To that, add all functions, first working JUST for the camera obscura version. 
;SO ALTER IT STEP BY STEP DO IT! change and re-check! 


;170619 remains to connect los origins quad function to the 2D imaging loop in cam obsc/older main loop or middle version with losloop only as a fnion.

;160619 SO SDO RELATE AS A NEXT STEP, los origins  procedure output to llos parameterizerd calculation SOS DO IT NEXT!!!

;***********************************************************************************************************************
;;200619 ULTRA SOS: 150619 MUST MOVE params INTO THE PARAM FILE SOS!!!! () they exist in code circa line 2000 plus . DO it like 180619 camera_obscura!
;200619  do v22 paraM FILE compare it to v21 and re-dig up all the new params and move them out of the code (already there in param file)
;200619 NO MORE CAM OBSC SEPArate procedure, eliminate that nesting cause too complicated, runs but no In result!







;;090519 SOLUTION: make an ALIAS funtction for pload, where we call it, with arhument the number, either with or without the float kword sos do it else nedd to alter manually float kword in many places sos do it!
;090519 pload loads flt only after we rename or erase dbl.out, else dbl has some kind of priority vs flt, and then if both .out are present
;it missesd the flt's!so rename dbl.out remnant first, in order to use flt for the perp beam for eg.
;080519 THESE LINES PASTE AND RUN, UNCOMMENTED BEFORE ALL ELSE! they also exist later on in the program, apart from sfactor! MUSTR FIX OT TO BE NICE!


;100519 a blueprint for calling pload, but we use if then else instead
pro pload_float,shot_number_fun, sfactor_fun,datapath_fun
;BLUEPRINT FINALLY EMPLOYED: 100519
;******************************************************************
if (pload_float_factor eq 1.0) then begin
pload, 21,shrink =sfactor, dir=datapath,/float
endif else begin
pload, 21,shrink =sfactor, dir=datapath
endelse
;******************************************************************

;pload, shot_number_fun, shrink=sfactor_fun,dir=datapath_fun,/float
if (pload_float_factor eq 1.0) then begin
pload, shot_number_fun, shrink=sfactor_fun,dir=datapath,/float
endif else begin
pload,shotmin+shotind, shrink=sfactor, dir=datapath
endelse


return
end


;;SUPER SOS 080519 WE NOW MUST CALL FIST TIME THE FILE IO AND THEN THE PLOAD, ELSE NX1,nx2,nx3 go all wrongf! SOS! when runnin gat 
;reduced resolution! SOS!
;080519B SOS FIXED IT! within perp function, insert break stsatement TO ALOOW BEAM TO 'EXIT' GRID: BREAK RHO ASSIGNEMENTS AFTER REACH END OF GRID.
;SO IMAGING GOES SMOOTH! no need to fiddle then! SOS! NOW TRY TO DO RELAT EFFECT BY MICRO TIMING! SOS !
;;080519 OR SIMPLY ASSIGN DESIRE SFaCTOR MANUALLY BEFORE EVERYTHING ELSE! SOS!
;020519 put rlosparam file input read near beginning, cause perp beam example uses those early calls to determine dimensions of nx1, ny1, nz1 SOS!
;010519 perp beam function works, but requires careful timing setup among components. 
;010519 remains to be seen if we can make it produce the relativistic approaching perpendicular beam relat. visual distortion
;i.e. sides appear thinner, due to their meeting the rays farther than the beam centre. TRY IT!
;290419 SOS eikona_reverse calc: after setting many things to double precision, we got errors down. PRob they asre inherent to ze incremental nature of 
;rad transfer calc. So reverse now is only some percent, on a median basis, deviation. good enough?

;
;
;
;
;
;
;FINALLY! line 2697 solution for non abs! check vs loop ! array op for abs then? 
;line 2680 check again for reverese first make it work symmetrically, then
;functionalize it with modules SO Sdo it slowly!
;270919 SOS LITMUS TEST IF ALL WORK OK AND THEY DONT!
;SOS 270419 here lies asymmetry SOS find out why!
;
;250419 SOS corrected imaging loop y z .. image:: image_file_namelower voxel upper voxel SOS yz was inverted!
;250419B SOS must inverse LOS calc, when we got absorption, cause we now move back in time!!! so save it up to a vector array ans use an option to choose fwd or
;; backward calc along ze vector. Just an extra loop, allows better functionization and modularity anyhow!
;250419 this profiler thing shows which part of code is slowing things down
;IDL> Profiler, /SYSTEM & Profiler
   ;IDL> run_your_code_here
   ;IDL> Profiler, /REPORT
;
;
;;230419 SEEMS to do it, calc back in time from camera trigger time tpicked. remains to be tested SOS DO TEST IT SOS. 
;;230419 ALSO: make it work as a separate module from the old one, i.e. do rlos modular allow user to select modules. 
;190419 we defined angles as 4D arrays. And: did 3D calcs within the functions that calculate the tans of them. 
;;190419 now ready to do ze calcs of DB etc using 4D_ARRAYS of angles phi1 and phi2.
;170419 tanphi1 misses ratio1c by a few percent, tanphi2 gets ratio2c much better. we will see again about this.
;170419 now put in all array calcs the arctanphi1,2 in place of phi1, phi2 also, do all in function mode, so that to keep both versions. SOS doit !
;060419 LEFT TO DO FOR NON-R focused LOS;s: resolve issue of POSSIBLE NEGATIVE ratiof's: beginning of loops SOS:  
;060419 THEN must also do back in time advance. From camera trigger time instant. 
;*************************************************************************************************************************************************************************************





;030819 here test the receding centre of example planar rod
pro testitall
;030819 here test the receding centre of example planar rod
;170819 This pro is a proving ground for the creation of a moving rod, WITH DENSITY, BUT WITHOUT VELOCITY AS OF NOW, to do list!. 
;170819 Such a rod (but not this one!) is called in the example of planar beam, august 2019 version!
clight_local=0.8
nx_1_fun_test=24
u_beam_x_test=-0.8905*clight_local
shotmax_fun_test=203

time_index_test=200.0

for time_index_test=200.0,1.0,-1.0 do begin

centre_x_approaching_test=fix(     (shotmax_fun_test-time_index_test+1)*double(abs(u_beam_x_test))      )

centre_x_receding_test_b=nx_1_fun_test-centre_x_approaching_test
print, ' time_index_test,centre_x_receding_test,centre_x_receding_test_b,centre_x_approaching_test',time_index_test,centre_x_receding_test,centre_x_receding_test_b,centre_x_approaching_test
;print, time_index_test
endfor


end








pro jet_profile_function_steady_state, eikonaread,jetprofile,indexed,indexedmas,deltazmas,eikona,eikona2
;170819 This procedure aims to reproduce the jet profile function of the steady-stade model jet of Hjellming-Johnston 1988 type. It is the same model used in the author's PhD, back in the year 2K.
;310719 This pro, in order to be called, requires that the steady-state jet has been invoked. Therefore, it is only called when steady_state_switch eq 1, i.e. when hj88 jet,along y-axis, has been invoked.
;tvscl wont work in 3D: try 2dslices, or 3d drawing instead
;161218TILL HERE DIFFERENTIATION BETWEEN xz AND yz VERSIONS.
;openr, 16, 'f:eikonafile.txt'
;readf, 16, ny10,nz10,eikonaread
;close, 16
;tvscl, eikonaread
jetprofile=TOTAL(eikonaread, 1)
indexedmas=indexed*deltazmas
eikona2=eikona
;plot, indexedmas, jetprofile*1e+18,xtitle='z(mas)', ytitle='Flux density per beam'
;if (debug_comments eq 1) then print,'total flux in SI *10^(-18):', total(eikonaread), total(jetprofile)
;plot, jetprofile*1e+18
end




pro rod_test_perform_before_time_read_loop, rho_4d_tempy,rho_4d,nx_1,ny1,nz1,bfield, ndens,denshj88,densrod,gammac,debug_comments,deltazmas
;170819 This pro helps perform a rod test of sorts, which comes in two parts. This is part No 1, before the time reading loop. Part No 2 foolws in another pro. 
;;170819 This is NOT the main st. state jet thing, it merely uses the st.st. jet a a tool, to cut a piece of it and present it as 'rod'. 
;170819 This whole method precedes the planar rod and then the x-rod thingy, in evolutionary order, both of which look better than this one really!
;
;300719 some stuff of this rod test switch go here, some in another pro, which runs within the time reading loop.

;300319 SOS HERE WE MUST IMPLEMENT A SUITABLE BOXCAR FUNCTION THAT IS NON ZERO ONLY AROUND A MARKER TRAVELING AHEAD IN TIME, i.e.
;is non-zero in a region where shotind times time unit times light speed in grid etc write it by hand.

;300319 we select the elemetns along y axis hwidth before and hwidth after the t-related location of the rod centre along y. 

rho_4d_tempy=rho_4d
;300419 make it zero for now, but keep its sizes!
rho_4d_tempy=rho_4d_tempy-rho_4d_tempy
; 300319 already taken the value of denshj88!
steady_state_jet,nx_1,ny1,nz1,bfield, ndens,denshj88,densrod,gammac,debug_comments,deltazmas

end








pro rod_test_perform_after_time_read_loop, rho_4d,shotind,denshj88,rod_test_switch,clight,t,NX2,halfwidth,rho_4d_tempy
;170819 Second part of the st. st. jeat turned rod ruse, mentioned in the rod_test_perform_before_time_read_loop pro. This one bit runs (is called) within the time-reading loop (snapshot reading loop: shotmin to shotmax).

;300719 this pro constitutes the second part of the rod test switch material, and this one goes WITHIN the time read loop, i.e. it runs 
;repeatively, again and again.

;300319 SOS HERE WE MUST IMPLEMENT A SUITABLE BOXCAR FUNCTION THAT IS NON ZERO ONLY AROUND A MARKER TRAVELING AHEAD IN TIME, i.e.
;is non-zero in a region where shotind times time unit times light speed in grid etc write it by hand.

;300319 we select the elemetns along y axis hwidth before and hwidth after the t-related location of the rod centre along y. 

rho_4d[shotind,*,*,*]=denshj88

;if (rod_test_switch eq 1)and (0.8*clight*t(shotind) lt (NX2-halfwidth-1) ) then (rho_4d[shotind,*,[(fix(0.8*clight*t(shotind)))-halfwidth,(fix(0.8*clight*t(shotind)))+halfwidth],*]=characteristic_value_unique)
;SOS 300319 select the slice of the hj88 jet, to be a rod!

;300719 imitate this, from perp_beam, to treat ends of domain problems here!
;;******************************************************************
;if ((x_base+x_length_half_count) ge nx1_fun*0.9) then break
;;020519 SOS do not allow beam to exit 150519 left!
;if (x_base lt x_length_half_count) then (x_base = fix(1.1D*x_length_half_count))
;;******************************************************************




;300319 only make unity the slice elements, i.e. rod length along y
;300719 added shifter thing! To avoid touching ends of nx2 domain!
;300719 initialize auxiliary params
shifter=0.0
dropper=0.0
;
;300719 bottom end treatment:
if (fix(0.8*clight*t(shotind))-halfwidth le 1) then (shifter =abs( abs(fix(0.8*clight*t(shotind)))-halfwidth) +1)

;300719 bottom end treatment:
if ((fix(0.8*clight*t(shotind)))+halfwidth ge (nx2-7)) then ( dropper= halfwidth)


 if (rod_test_switch eq 1) and (0.8*clight*t(shotind) lt (NX2-halfwidth+1) ) then (rho_4d_tempy[shotind,*,(fix(0.8*clight*t(shotind)))-halfwidth+shifter:(fix(0.8*clight*t(shotind)))+halfwidth-dropper,*]=1.0D)
 ;300719 oops here we are! -2 to plus 2? wtf?
 print,'(fix(0.8*clight*t(shotind)))-halfwidth+shifter ,  (fix(0.8*clight*t(shotind)))+halfwidth-dropper',(fix(0.8*clight*t(shotind)))-halfwidth+shifter,(fix(0.8*clight*t(shotind)))+halfwidth-dropper
 print, 'shotind,halfwidth',shotind,halfwidth
 print, 'shifter,dropper',shifter,dropper
;300319 et voila, multiply SOS keep it to current shotind time slice ONLY! SOSARA:
;270719 this messes  with density  if clight is above a certain level OVERWRites it!!! sos
if (rod_test_switch eq 1) and (0.8*clight*t(shotind) lt (NX2-halfwidth+1) ) then (rho_4d[shotind,*,*,*]=rho_4d[shotind,*,*,*]*rho_4d_tempy[shotind,*,*,*])
;SOSARA 300319 PUT ALL THIS IN if then condition, or it will ruin normal hydrocode execution SOS do it now! 
;SOS 300319 next thing proves we affected only the rod y-slice. Since we got a st state conical like hj88 type jet, the slice of a jet is a roddy!.
 dfeeee=0.0
 if (rod_test_switch eq 1) and (0.8*clight*t(shotind) lt (NX2-halfwidth+1) ) then (dfeeee= size(rho_4d_tempy[shotind,*,(fix(0.8*clight*t(shotind)))-halfwidth+shifter:(fix(0.8*clight*t(shotind)))+halfwidth-dropper,*],/dimensions))
print, dfeeee
;SOS print the following to final check
;print, rho_4d[shotind,*,(fix(0.8*clight*t(shotind)))-halfwidth:(fix(0.8*clight*t(shotind)))+halfwidth,*]
end

















pro global_absorption,Temp_4d,PRS_4d,RHO_4d,dopplerfactordivisor,xfrac,gaunt,ng,emiss,resultvectorfinal,kabs_4d,kabs2_4d,NLONG, synchrotron_emissivity_switch, alphaindex_external, v_4d, sinlosb_4d, bx1_4d, bx2_4d, bx3_4d, bfield_4d, nobs, c1_pachz, c2_pachz, c3_pachz, c4_pachz,dopplerfactor_4d, kappa_value_power_law, fast_proton_profile_4d

;170819 This pro is to include (NOT YET COMPLETE CAREFUL! EXPERIMENTAL MATERIAL HERE!) thermal, synchrotron and gamma rays (the latter probably through calling an external routine).

;300719 a collection of global abs calculations, including thermal emission. 
;300719 CAREFUL temp_4d here is defined directly from read density and pressure. 
;300719 But, elsewhere in this program, it is also read directly, under the name temp4d, from PLUTO, where it might have been calced at a differerent units' system. 
;Bottom end, DONT MESS temp_4d (from here) with temp4d (to be read from PLUTO).

Temp_4d=(PRS_4d/RHO_4d)*10000000000000.0


;;ionization fraction is called xfrac
;xfrac=((T/1.0e+8)<1.0)
;more proof to the high densities of the disk:
;extended disk should give NO EMISSION IF COLD ENOUGH,
;NO MATTER HOW HIGH ITS DENSITY IS!!
xfrac=(((Temp_4d^3)/1.0e+14)<1.0)
gaunt=10.0*(1.0+0.1*alog((Temp_4d^(1.5))/ng))
gaunt=gaunt>0
;emiss is over volume to the third for volume NLONG etc
;-39 from formulae plus 42 from volume of cell makes plus 3 power of ten
;also over steradian
;e^(-hn/kt) is one for radio
;emiss times dV=dl*dl*dl
;times 0.01 for sr to the next cell SOS
;10^-39 from emissivity formula  times cell volume of 10^42 makes 10^3 thus the 10^3 factor here SOS
;last 10^-6 is the solid angle of a los this is to be altered for HIGHER RESOLUTION SOS

; now we put in ze gamma ray emission coefficient
;emiss=emiss*0.0
;emiss=5.44*(1.0e+3)*((100.0/NLONG)^3)*RHO*RHO*(T^(-0.5))*gaunt*(xfrac^2)*lossolidangle
;emiss=RHO
;emiss=emiss*0.0

;020815 consider moving here the D. boosting stuff? or check anyway 

;gammarayemissiongromresultvectorfinal,coming from the mathematica code
emiss=resultvectorfinal/(1000000000000000000000000000000.0)
;emiss=T/RHO
;emiss=0.0*emiss



;absorption coefficient


;kabs=cp6(0.)*ndens*(bfield*sin(theta))^((gammac+2.)/2.)*((ng/2.)/cp1(0.))^(-(gammac+4.)/2.)
; kabs is over length, not over volume, like emiss!!)^14 cm is pluto code unit for length 100/nlong times
;that is length of los thru this cell
;kabs times dl

; we set ll equal to 1.5 to account for angles diagonal through cell
;so here leave (100/nlong) OK for length
;and it works ok for cell volume too, which has nothing to do with the LOS angle!

; we set the gamma ray absorption coefficient to zero
; 060815 ng is also a 4d array now BUT ng is only assigned along the LOS the rest is wrong
;so we must move these assignements inside the LOS loop, but only as correctuons to
;their presently assigned values
;within the loop we select and correct
;only way to do it efficiently,
;if there must be a loop 



kabs_4d=0.018*gaunt*RHO_4d*RHO_4d*(1/ng)*(1/ng)*(Temp_4d^(-1.5))*(100.0/NLONG)*(1.0e+14)*(xfrac^2)
;300719 corrected: T to Temp_4d
kabs2_4d=0.08235*RHO_4d*RHO_4d*(Temp_4d^(-1.35))*0.01*(((Temp_4d/1.0e+8)>1.0)^2)*(100.0/NLONG)

;251218 here no aborption for time being, but next time may be used.
kabs_4d=0.0*RHO_4d
kabs2_4d=0.0*RHO_4d

;140722 synchrotron switch triggers kabs calc based on pachz 
gammac=alphaindex_external
;if(synchrotron_emissivity_switch eq 1.0) then(  kabs_4d=(c6_pachz(gammac))*rho_4d*( ( bfield_4d*sinlosb_4d )^( (gammac+2.)/2.) )*((nobs/2.)/c1_pachz)^((-4.-gammac)/2.)   )

;290722 sos added fast_proton_profile_4d to sync absorpt as well, since it defines fast proton density, which also matter for sync absorb coeff.
if(synchrotron_emissivity_switch eq 1.0) then(  kabs_4d=fast_proton_profile_4d*kappa_value_power_law*(c6_pachz(gammac))*rho_4d*( ( bfield_4d*sinlosb_4d )^( (gammac+2.)/2.) )*((nobs/2.)/c1_pachz)^((-4.-gammac)/2.)   )
;;kabs=cp6(0.)*ndens*(bfield*sin(theta))^((gammac+2.)/2.)*((ng/2.)/cp1(0.))^(-(gammac+4.)/2.)

;kabs
; HERE PUT IN VALUE FOR KAPPA ABS COEFFICIENT
; DO IT!
; 190518 HERE ADD KAPPA ABS BASED ON REYNOSO ROMERO 2008, eqn 16, based on atoyan drmer 2003: also from our paper 2014 
;Note: no optical depth here: It is meaningless without a LOS, and a LOS is only for a small line throught the grid, not all over it!
;an array to enable picking out the los points within the 3D grid


;%OMIT GHZ-2 for the time being SOS

; 050815 nobs=ndash*Dfactor therefore for each cell we have
;a different ndash=nobs/Dfactor (nobs is fixed for each run)
;therefore we calc ze radio emiss(e.g. thermal) at each cell
; this calc, each time it is done,
;is heavily dependent on the dfactor, i.e. on
;angle and velocity at that cell. IT IS 
;NOT A ONE OFF UNIVERSAL CALCULATION NOT AT ALL SOS

; 060815 interim variable for dopplerfactor = dopplerfactordivisor
;we set dfdivisor, for now, to a starter value 
;for the hj88 model, but assign it to 
;cell's dopplerfactor array value later on, before the main loop;
; assignements of emiss and kabs values are performed
;before the los loops. Everything is done 
;in array format before the loops.
;The loops merely draw a path through ready results.
dopplerfactordivisor=1.2
; 060815 nobs is set here once and for all, perhaps 
;move it to start of code as well 



end








;OS EDO COMMENTED 170819 









;090722 SOS added bxis and also COSLOSB SOS this was needed coslosb SOS
pro global_calculations,phi1_fun,phi2_fun,vx1_4d,vx2_4d,vx3_4d,v_4d,speedtweakfactor,coslosu_4d,lx1,lx2,lx3,thetau_4d,gammalorentz_4d,ccccc,ncalc,ng,dopplerfactor_4d,debug_comments_fun,focused_beam_switch_fun, bx1_4d, bx2_4d, bx3_4d, coslosb_4d, sinlosb_4d, bfield_4d
;180819 This pro performs a series of array operations, which include the calculation of crucial quantities, such as dopplefactor,(171119 NO! FSHIFT iS NOW a bit later, in a separate procedure! frequency shift) and angle between LOS and local cell velocity.


;;160819 series of calling: 
;1.global calculations 
;2. freq shift effect
;3. emiss global calc
;
;300719 we wrap up those important global calcs into one single procedure. Careful, some of those may have to be outsourced to external programs (sintheta PS2001, etc), then we
;need a different, additionasl copoy of this pro , and a selection param or something. 

;******************************
;190819 vintage comment from the past:
; 090816 HERE WE HOMOGENEOUSLY TWEAK SPEEDS TEMPORARILLY, ALL OVER THE GRID!
;MUST AVOID THIS IN NORMAL RUNS SOS
; *** THIS IS TEMP FIX DO NOT KEEP IT SOS!!!!!!!!!
;*******************************
;241218 ended up being ts factor so we kept it!

;280722 here calc v_4d, BEFORE TWEAKING, so we then multiply something non-zero with tewakspeed factor!(array-op!)
v_4d=sqrt(vx1_4d*vx1_4d+vx2_4d*vx2_4d+vx3_4d*vx3_4d)

vx1_4d=vx1_4d*speedtweakfactor
vx2_4d=vx2_4d*speedtweakfactor
vx3_4d=vx3_4d*speedtweakfactor
;280722 sos we never assign the value to v_4d, thus it remains zero so far!
v_4d=v_4d*speedtweakfactor

;******************
; 110816 check this calc hereafter, why even at very high speed(tweaked) dopplerfactor never exceeds 1.15 max? 
;shouldnt it reach to infinity or something? check! testing extremes!
; it is a matter of angles also!!! check right above, lx1,lx2,lx33 SOS!


;200419 careful these lx1,2,3 are now 4D arrays. so bbbbb,ccccc, for e.g., should be a 4D array of ones!

;190819 cos of the angle between LOS and local velocity, is outsourced to another procedure. SOS check that lx1,lx2,lx3 are dependent on focal point, for cam obsc case! SOS CHECK IT!!!
coslosu_4d=coslosu_4d_calc(phi1_fun,phi2_fun,lx1,lx2,lx3,vx1_4d,vx2_4d,vx3_4d,debug_comments_fun,focused_beam_switch_fun)
print, "here test for coslosb4d"
if (debug_comments_fun eq 1) then stop  
;090722 cos of the angle between LOS and local magnetic field, is outsourced to another procedure. SOS check that lx1,lx2,lx3 are dependent on focal point, for cam obsc case! SOS CHECK IT!!!
coslosb_4d=coslosb_4d_calc(phi1_fun,phi2_fun,lx1,lx2,lx3,bx1_4d,bx2_4d,bx3_4d,debug_comments_fun,focused_beam_switch_fun)
print, "here test again for coslosb_4d"
if (debug_comments_fun eq 1) then stop  
;130722 here call sinlosb, in order to calc globally the sin of twisted theta, 2 b used in pachz synchrotron calcs 
sinlosb_4d=sinlosb_4d_calc(coslosb_4d)
if (debug_comments_fun eq 1) then STOP
;190819 now we calc the gamma lorentz factor, all over the grid.
ccccc=sqrt( (vx1_4d*vx1_4d) + (vx2_4d*vx2_4d) + (vx3_4d*vx3_4d) )

;080722 added this assignement, v_4d was declared but unused anyway, so no RAM use increase here
v_4d=ccccc

;190819 globally calc the angle of local velocity and line of sight crossing that cell. 
thetau_4d=acos(coslosu_4d)
; 291217 FOLLOWING IS FULL OF NANS, NONE OF THE ABOVE aaaaa, bbbbbb, ccccc, theta
;PROBLEM LOCATED ON (sqrt(1-ccccc*ccccc)), has NANS
gammalorentz_4d=1/(sqrt(1-ccccc*ccccc))

; screws the calculation DROP IT SOS!!! sets the minimum zero, while it must be one!!
;120816 this is meant to remove naNs and replace em with zeros SOS
;gammalorentz_4d=gammalorentz_4d*(finite(gammalorentz_4d))

;dopplerfactor_4d=(sqrt(1-ccccc*ccccc))/(1-ccccc*coslosu_4d)
;270718 changed the expression, for optimization reasons
dopplerfactor_4d=1/( gammalorentz_4d*(1-ccccc*coslosu_4d) )

;110816 this is meant to remove naNs and replace em with zeros SOS
;dopplerfactor_4d=dopplerfactor_4d*(finite(dopplerfactor_4d))

;define theta_4d=
; 170915 ncalc=ng/dopplerfactor_4d
;we use the 0.0000000001 factor in order to avoid division by zero
;if an element of the dfactor array turns out
;for some reason to be zero SOS
;190819 here we calc freq. shift, separate for each cell, depending on the Dfactor.
ncalc=ng*(1/(dopplerfactor_4d+0.000000001))
if (debug_comments_fun eq 1) then print,'inside GLOBA calcs here'
if (debug_comments_fun eq 1) then stop

end








pro frequency_shift_effect,freqshiftchoice,freqfactor,ncalc,ng,alphaindex_external,kappa_spectr,kappa_spectr2,jet_spectrum_built_in_function_test_unity_at_108
;190819 This pro calculates the quantity of freqfactor and also the explicit spectrum for a jet element.


;160819 series of calling: 
;1.global calculations 
;2. freq shift effect
;3. emiss global calc
;
;;160819 this whole freq thing is the user inut spectrum. 
;160819 There is also the ability to employ the freq effect directly into the DFACTOR formula, as an additional dfactor raised to a suitable power of alphaindex.
;160819 Arrange for a selection between the two, and prove they work by matching them to a calibration.
;
;
;**************************************************************************************************************************************************************************************************************
;**************************************************************************************************************************************************************************************************************

;140819 We now attempt to do the color map, i.e. eikona_color (2d array), which is an image of frequency shift NONONO! 
;per cell, there is a separate color shift!!! How to do the grand total!! difficult!!! so, need to select an observing frequency, and also to bear in mind a
;jet-frame spectrum!!! Then, shift differently per voxel, then find out, going backwards in freq, from where in the original spectrum does it originate, so that it ends up at the obs. freq!
;from that, deduce its intensity! OOPS!!! voila!!! DO THAT NOW!!! SOS per cell, it affects the intensity!!! when FS is turned ON. 

;140819 start by doing a global 4d calc, whereby each voxel is assigned a simple scalar freq shift. From that, based on a given spectrum, we go backwards and deduce emitting intensity, i.e. distort the base intensity (e.g.)
; at a given freq, i.e. point along the base spectrum, we got 100 percent. The relative intensity percentage then multiplies the density, or whatever intensity we get from the rest of the model.

;140819 for e.g.: say we have a freq redshift, by 50 percent. So: if e.e. nobs=8 GHz, then we see that as 4GHz. Now, back to the base spectrum. There, say we have 100 units at 10 GHz, 80 at 8GHz, 50 at 4GHz. Then, we get 5/8 of 
;the intensity, since we get to see what inherenlty is emitted at 4 GHz, at 8GHz. Just an example really.

;140819 So we need: a global freq shift calc (probably already there somehow) and a link between that and the intensities, trhrough a base spectrum!!
;**************************************************************************************************************************************************************************************************************
;**************************************************************************************************************************************************************************************************************






;300719 a pro to calc freq shift. ng is a global (4D) array. 

;300719 try to link fshift to color of an image, in order to do relat color shift for the first example SOS. 
  
  
;SO 290618 freqfactor is not FS on off, it is something else. alpha is already there in Db choice SOS

;140819 freqfactor is our color function!

;140819 now define the base spectrum, either analytically, or byfitting data!
;LETS USE, FOR STARTERS, S propto nu^(alpha), setting alpha=(-2).

;140819 bear in mind that(from procedure 'global calcs', above): ncalc=ng*(1/(dopplerfactor_4d+0.000000001)) 


;150819 kappa_spectr is for now set to unity,it is a calibration constant for the spectrum intensity!

;150819 lets do this (spectrum)!
;jet_spectrum_built_in_function=kappa_spectr*ng^(-alphaindex_external)

;;****************************************************************************************************************
;****************************************************************************************************************
;150819 here we test the spectrum by plotting it in a small lopp, of user-set length. Also present in relevant separate file, as a standalone spectrum plotting facility!
;max_plot_length=8000
;ng_test=dblarr(max_plot_length+1)
;jet_spectrum_built_in_function_test=dblarr(max_plot_length+1)
;print, size(ng_test,/dimensions),size(jet_spectrum_built_in_function_test,/dimensions)
;
;for indicy=1,(max_plot_length-1),1 do begin
;print, 'indicy',indicy
;print, '100*2.0*indicy',8000*2.0*indicy
;;150819 add some log flavor to the spectrum plot!
;ng_test(indicy)=8000*2.0*(indicy^1.4)
;jet_spectrum_built_in_function_test(indicy)=kappa_spectr*ng_test(indicy)^(-alphaindex_external)
;endfor
;
;plot, ng_test,jet_spectrum_built_in_function_test,/ylog,/xlog,yrange=[0.000001,100.0],xrange=[1000000,1000000000]
;****************************************************************************************************************
;****************************************************************************************************************


  ;150819 here we go: alter freqfactor to agrree with assumed spectral infuence! Do it here, into the freqfactor quantity! Zeroing on it!
if (freqshiftchoice eq 1.0) then (  freqfactor=(ncalc/ng)^(2))  else  (  freqfactor=(ng/ng)^(2));i.e. ng/ng=1


;150819b
;jet spectrum is a global 4D array here! ta-tarara! Even more RAM required! 
;150819 spectrum is relative value if intensity!;We preset things so that at 8GHz we have ref value of unity!
;backup: kappa_spectr2=10000000000000000.0

;150819 later on, we may put this value to the relevant constant in the param file
;160819 Moved to param file
;kappa_spectr2=10000000000000000.0



;*********************************************************************************************************
;*********************************************************************************************************
;150819B SOS here it is, globally apply that spectrum and see what happens! 
;150819B Pick random points, and test: print their dfactor, their ng, their ncalc and their spectrum. And that spectral value is your moderation, modulo(unity at 8GHz))
;jet_spectrum_built_in_function_test_unity_at_108=kappa_spectr2*ng^(-alphaindex_external)
;150819 actually, we should apply the spectrum moderation to the intensity, on the ncalc freq!
;150819 ncalc is taken from the following 4d array formula, from the procedure global_calculations.
;
;;SOS 150819 MAKE SURE THIS ONE PRO IS CALLEDAFTER  GLOBAL_CALCS!!! SOS
;ncalc=ng*(1/(dopplerfactor_4d+0.000000001))

jet_spectrum_built_in_function_test_unity_at_108=kappa_spectr2*ncalc^(-alphaindex_external)
;*********************************************************************************************************
;*********************************************************************************************************




;140819 all three are 4d global arrays! no wonder from 3gb of data 30 gb of ram! thanks page file! 
;print, size(freqfactor),size(ncalc),size(ng)
;           4         146          30          50          30           5     6570000
;           4         146          30          50          30           5     6570000
;           4         146          30          50          30           5     6570000

end







pro emiss_global_calc, dopplerchoice,emiss_4d,rho_4d,dopplerfactor_4d,alphaindex,freqfactor,spectrum_direct_switch,jet_spectrum_built_in_function_test_unity_at_108,debug_spectra_switch,emiss_4d_implied,emiss_4d_direct,dummy2_4d_fun,dummy3_4d_fun,pion_emissivity_switch,neutrino_emissivity_switch, synchrotron_emissivity_switch, alphaindex_external, v_4d, sinlosb_4d, bx1_4d, bx2_4d, bx3_4d, bfield_4d, nobs, c1_pachz, c2_pachz, c3_pachz, c4_pachz, fast_proton_profile_4d, kappa_value_power_law, neutrino_energy_selector, dummy4_4d_fun, dummy5_4d_fun, dummy6_4d_fun, dummy7_4d_fun, dummy8_4d_fun, dummy9_4d_fun, dummy10_4d_fun,dummy11_4d_fun,dummy12_4d_fun, dummy13_4d_fun, dummy14_4d_fun, dummy15_4d_fun 

;290722 Added fast_proton_profile as a multiplication factor, to distinguish fast matter from slow matter, for syc emiss purposes. 
;290722 Can be used elsewhere too!
;220320 added dummy2,3 here as passed arrays, in order to conditionally assign them 
;to emissivity!  SOS for specific angles etv params, must run nemiss_pload4d using
;; same params as rlos for correct result!
;
;;190819 SOS EXPLICIT SPECTRUM IS STILL EXPERIMENTAL, it works but requires finalcheck of how many dfactors, freqfactor usage or not per each case AND last but not least: kappa constant of expl. spectrum 10^8 unity, etc.
;
;
;190819 This pro calculates the emission from each cell, using either doppler factor or not. Also, it either uses explicit spectrum, or implicit. 
;190819 Both choices are arrangedwithin two nested if then else. These two choices are dependent on switches from the parameter file.
;190819 Another switch potentially allows the creation of two separate 4d arrays for emiison, either implicit or explicit spectrum. 
;190819 SOS EXPLICIT SPECTRUM IS STILL EXPERIMENTAL

;290719 a procedure to globally calculate emission

;160819 series of calling: 
;1.global calculations 
;2. freq shift effect
;3. emiss global calc

;160819 here we need to choose, either using directly, as is now the case, the spectrum effect FSIFT, or employ the spectral formula in the previous procedure!
if (dopplerchoice eq 1.0) then begin

;160819 re-check how emission is if we do not account for spectral effect from formula!! We take density as the base for emission coefficient, and then add, on top of that, special relativistic effects!
;160819 the influence of freqfactor is included within jet_spectrum108, in ncalc specifically
;;170819 yes to the above line, but then we also have in the implied case the presence of both nu^alpha ANDfreqfactor!! REDUNDANT? APPLES TO APPLES BABY! DOIT FOR COMPARISON's sake!
;170819 two Dfactors from time dilations, 1 plus alpha from freq shift! so we miss one Dfactor here:
;keep old one for backup! if (spectrum_direct_switch eq 1.0) then (  emiss_4d=rho_4d*(dopplerfactor_4d)*jet_spectrum_built_in_function_test_unity_at_108  ) else (  emiss_4d=rho_4d*((dopplerfactor_4d)^(3.0+alphaindex))*freqfactor  )
;170819 in the old one, no need for freqfactor in implied form!!! CAREFUL! 
if (spectrum_direct_switch eq 1.0) then (  emiss_4d=rho_4d*(dopplerfactor_4d*dopplerfactor_4d)*jet_spectrum_built_in_function_test_unity_at_108*freqfactor  ) else (  emiss_4d=rho_4d*((dopplerfactor_4d)^(3.0+alphaindex))*freqfactor  )

endif else begin
emiss_4d=rho_4d*freqfactor;; THIS IS THE ONE USED so farSOS
endelse

if (debug_spectra_switch eq 1.0) then begin
 
 emiss_4d_implied=rho_4d*((dopplerfactor_4d)^(3.0+alphaindex))*freqfactor 
 ;;170819 yes to the above line, but then we also have in the implied case the presence of both nu^alpha ANDfreqfactor!! REDUNDANT? APPLES TO APPLES BABY! DOIT FOR COMPARISON's sake!
;170819 two Dfactors from time dilations, 1 plus alpha from freq shift! so we miss one Dfactor here:
 emiss_4d_direct=rho_4d*(dopplerfactor_4d*dopplerfactor_4d)*jet_spectrum_built_in_function_test_unity_at_108*freqfactor

endif
;220320 here we conditionally assign to emissivity either pion
;(fast, precursor quantity)
;or neutrino (slow, final result) emissivity, as having been 
;read from dummy files on disk. nemiss_pload4d is suppposed to 
;having calced this stuff, FOR SPECIFIC ANGLES ETC SETTINGS.
;SOS DO NOT ALTER THW TWO PARAM FILES BETWEEN RUNNING 
;nemiss_pload4d and this one here!
if(pion_emissivity_switch eq 1.0) then(emiss_4d=dummy2_4d_fun)
if(neutrino_emissivity_switch eq 1.0) then begin

;if(neutrino_energy_selector eq 1.0)then(emiss_4d=dummy3_4d_fun)
CASE neutrino_energy_selector OF 
   1.0:  (emiss_4d=dummy3_4d_fun)
    2.0:  (emiss_4d=dummy4_4d_fun)
     3.0:  (emiss_4d=dummy5_4d_fun)
      4.0:  (emiss_4d=dummy6_4d_fun)
       5.0:  (emiss_4d=dummy7_4d_fun)
        6.0:  (emiss_4d=dummy8_4d_fun)
         7.0:  (emiss_4d=dummy9_4d_fun)
          8.0:  (emiss_4d=dummy10_4d_fun)
           9.0:  (emiss_4d=dummy11_4d_fun)
            10.0:  (emiss_4d=dummy12_4d_fun)
             11.0:  (emiss_4d=dummy13_4d_fun)
              12.0:  (emiss_4d=dummy14_4d_fun)
               13.0:  (emiss_4d=dummy15_4d_fun)
    
ELSE: PRINT, 'neutrino_energy_selector has an illegal value.' 
ENDCASE

;
;
endif
;290622 SOS we now do pachz sync emiss, so add a new switch here SOS
;290622 SOS  WTF IS THAT THETA THINGY??? CLARIFY SUPER SOS DO IT SOS ALSO DOES temperature and velocity play a role here? VERIFY SOS
;;080722 theta is angle between bfield and LOS must be calced similar to coslosu
;040722 SOS alphaindex_external is the spectral index as set in the data file.
gammac=alphaindex_external
;080722 as a test, we assign velocity factor to synchr emissivity as well. SOS also, mind self absorption
;130722 here assign value to bfield_4d
bfield_4d=sqrt(bx1_4d*bx1_4d+bx2_4d*bx2_4d+bx3_4d*bx3_4d)
;280722 SUPERSOS we add kappa value power law, cause N0 density in pachz is FAST proton density,
;i.e. slow proton density times kappa_value_power_law. Latter is portion of fast protons out of slow proton total
;Change should olny affect scale of the result, as kabs remains a percentage-style thingy, whereas emiss result just jets multiplied now by a 10^-6 -type facxtor.
;SOS 280722 we now remain to add fast proton profile factor function in front of both here and in kabs.
if(synchrotron_emissivity_switch eq 1.0) then(  emiss_4d=(kappa_value_power_law*c5_pachz(gammac))*rho_4d*( ( bfield_4d*sinlosb_4d )^( (gammac+1.)/2.) )*((nobs/2.)/c1_pachz)^((1.-gammac)/2.)*(dopplerfactor_4d*dopplerfactor_4d)   )
;170722 testing proved dfactor squared has many nans, so drop it for now then fix it
;180722 sos no problem, we just had the speed multiplier too high, 2.88. set it back to unity, and guess what, it now works like a charm!
;290722 added fast_proton_profile_4d factor, which distinguishes fast matter from slow matter, in a profiles manner, not abruptly
if(synchrotron_emissivity_switch eq 1.0) then(  emiss_4d=fast_proton_profile_4d*(kappa_value_power_law*c5_pachz(gammac))*rho_4d*( ( bfield_4d*sinlosb_4d )^( (gammac+1.)/2.) )*((nobs/2.)/c1_pachz)^((1.-gammac)/2.)*(dopplerfactor_4d*dopplerfactor_4d))

;130722 emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.)    )
;040722 SOS old one to be remedied SOS if(synchrotron_emissivity_switch eq 1.0) then(         emiss_4d=cp5(0.)*rho_4d*( bfield_4d*sin(0.8) )^( (gammac+1.)/2.) )
;*( (ng/2.)/cp1(0.) )^( (1.-gammac)/2.) )            )
;040722 SOS 0.8 is the angle, from pachz, betwseem B field and LOS. So we shall calc it, 
;same way we did with (los,u)-coslosu.  let us call the angle losb
;040722 Also, we shall know that gammac is the spectral index of the p-law HE electron distro, already a model param. A function of
;that, and of some constants are the Pachz ci fns. SOS dig em up or recalc em. 
;040722 SOS verify Bfield is read OK from data and then proceed.
;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.)) 



end







pro steady_state_jet, nx_1,ny1,nz1,bfield, ndens,denshj88,densrod,gammac,debug_comments_fun,deltazmas

;190819 This pro sets up the HJ88 steady-state jet, in 3D cartesian coords. Careful: HJ88 jet params are set up LOCALLY, NOT in the external parameter file!

;290719 CAREFUL steady-state jet lies along Y axis
;290719 invalid comment so we image it opposite ways! i.e. DBOOSTED when YZ, laterally when XZ!!

;290719 MUST ALSO CHANGE UX UY UZ,else only density wont affect the emission calculations, we are only using density now!!!
;no big deal, only employing jet speed along y axis in the steady model!!HJ88 ref re-check though, maybe there also is a smaller lateral component

;HERE HJ88 GEOMETRY DEFINED
;
;

; 'factor' affects the scale of the jet width. Can be set to set up a narrower or wider jet.
;Just a scale of the jet narrowness. This should affect the jet opening angle and the jet mach number, normally.
factor=1.0
;************************************************* BEGINNING OF HJ88 type jet MODEL (NO HYDRO) 251218
;************************************************* NOT USED AT THE MOMENT (still here, yet inactive)

r1=3
;print, 'tracker pos 0.1'
r2=14

;print, 'tracker pos 0.2'
x1=15
;CAREFUL WITH z1, z2
; it must be: (1<y1<yind<y2<ny1+1)
y1=1
y2=ny1-1

;print, 'tracker pos 0.4'
z1=1
z2=nz1+5
;print, 'tracker pos 0.6'


;we assume that initially, bfield array was set to zeros
;parameters to adjust bending of the axis of the cone of the jet:
;check by 2d plots of jet axis

a1m=0.0
b1m=0.0
a1exp=1.4
b1exp=1.3


;
;
;
;define in radians, angle between bfield and velocity of particles just a parameter really
theta=1.0


;distance in kpc
dkpc=3.0
;Some HJ88 model specifics
;p is parameter of HJ88 jet lateral expansion
p=0.5

; Z HAS NOW BECOME Y JET ALONG Y SOS
;length, in SI m, of grid cell along z
;length, along z, of computational domain, in mas
zmaxmas=20.
deltazmas=(zmaxmas/float(nz1))
zamas=1.0
dzbmas=0.5
zadeg=((zamas)/(3600.*1000.))
dzbdeg=((dzbmas)/(3600.*1000.))
zmaxdeg=((zmaxmas)/(3600.*1000.))
; CGS UNITS FOR LENGTH, cm i.e. cm in HJ88 model all the time (cos of the *100. factor)
za=(sin(2.*!pi*zadeg/360.))*dkpc*(3.1E+19)*100.
dzb=sin(2.*!pi*dzbdeg/360.)*dkpc*(3.1E+19)*100.
zmax=sin(2.*!pi*zmaxdeg/360.)*dkpc*(3.1E+19)*100.
;deltaz is voxel length along z axis
deltaz=zmax/(float(ny1))
;magnetic field at za, in tesla SI
cha=0.1
;particle density at ref. point, SI m^-3
cka=10.0
;initial mach number at ref point. HAs to do with initial opening angle, therefore
;defined in relation to the cone shape define before
;print, deltaz, zmax, nz1
;link those with above


;print, 'tracker pos 1'
;Mach number definition here.
;so far valid for a conical jet
abcde=double(r2-r1)
wwwww=double(y2-y1)
cma=1./(sin(abcde/wwwww))

;print, 'tracker pos 2'
;print, ((r2-r1)/(z2-z1)),sin((r2-r1)/(z2-z1)),cma,1./(sin((r2-r1)/(z2-z1)))
;print, 'tracker pos 3'


;print, 'tracker pos 4'
lamda1=(cha)*(za^p)
tau1=(cka)*(za^(2.*p*(gammac+2.)/3.))
ni1=(1.)/((za^(p-1.))*cma)
;print, 'tracker pos 4.1'
;define b, n arrays for HJ88 model
bfield=dblarr(nx_1+6,ny1+6, nz1+6)
;print, 'tracker pos 4.2'
ndens=dblarr(nx_1+6,ny1+6, nz1+6)
denshj88=fltarr(nx_1+6,ny1+6, nz1+6)

densrod=dblarr(nx_1+6,ny1+6, nz1+6)
denshj880=10000000000000
;print, 'tracker pos 5'
;ndens=ndens+3.0
if (debug_comments_fun eq 1) then print, nx_1

; hj88 has z axis along z. whereas PLUTO has jet along the X axis. we must therefore align one with the other. Easier seems to be to align the
;hj88 jet with the y axis. do it therefore.
for zind=1,nz1+5 do begin
 for yind=1,ny1+1 do begin
  for xind=1,nx_1+1 do begin
  ; rz must be a function of p in HJ88 SOS
  ;only in HJ88 though
  ;r(z)=r1*((z/z1)^p)
;better to avoid too small r1 ruining the ratio
  ;r(z)=r2*((z/z2)^p)
x1m=x1+a1m*(yind^a1exp)
z1m=z1+b1m*(yind^b1exp)
;za1=(r2-r1)
;za2=(z2-z1)
;za3=(zind-z1)
;za4=r1
;za5=((za1*za3)/za2)+r1

;rz=za5

rz=factor*r2*((double(yind)/double(y2))^p)
;SOS
; this is for the p-HJ88 model


;MUST HAVE NON BEMT JET FOR THE FOLLOWING B and N TO BE VALID ELSE, USE BENT AXIS AS A JET CONE AXIS

;020815 group the following if's, optimize'em



if (((xind-x1m)^2+(zind-z1m)^2) lt rz*rz) then Bfield(xind,yind,zind)=lamda1*(1./((yind*deltaz)^p))
k123=2.*p*(gammac+2.)/3.
if (((xind-x1m)^2+(zind-z1m)^2) lt rz*rz) then ndens(xind,yind,zind)=tau1*(1./((yind*deltaz)^k123))
; HERE add a HJ88 density, compared to a relative value at z0.
if (((xind-x1m)^2+(zind-z1m)^2) lt rz*rz) then denshj88(xind,yind,zind)=denshj880*(1./((yind*deltaz)^p))
;JUST TO CHECK IF CONSTANT DENS GIVES CONSTANT GAMMA RAYS EMISSION CHECK
;if (((xind-x1m)^2+(zind-z1m)^2) lt rz*rz) then denshj88(xind,yind,zind)=denshj880*(1./((10.0*deltaz)^p))
 ;if (((xind-x1)^2+(yind-y1)^2) lt rz*rz) then ;print, za1, za2, za3, za4, za5, rz*rz, ((xind-x1)^2+(yind-y1)^2), xind,yind,zind,(xind-x1)^2

  endfor
 endfor

endfor
slicenumber=15
; HERE WE SELECT THE SLICE OF densHJ88 to plot
;qwerty=REFORM(denshj88[*,slicenumber,*],100,100)
;zita is the lenth along the jet axis. It is not valid foe a bent jet. It is an HJ88 model parameter.
;zita=z-z1
surface, denshj88[2,*,*], ax=64, az=102

;HERE ENDS THE DEFINITION OF MODEL GEOMETRY

 ;surface, REFORM(denshj88[15,*,*],100,100), ax=64, az=102
 
;*********************************************END OF HJJ88 type jet geometry 251218



end




















pro pathfinder_algo_for_ratio1_radiograph_case_1_and_2, ratio1c,ratio1f_fun,uc_fun,rc_fun,debug_comments_fun
;190819 This pro calcs the azimuth pathfinding for each pair of steps along the LOS, FOR RADIOGRAPH CASES 1 AND TWO (RADIOGRAPH: case1: xz plane imaging, case 2: yz plane imaging). 
;190819 RADIOGRAPH MEANS NO FOCAL POINT, ALL LOS'S ARE PARALLEL TO EACH OTHER. FILM IS THE SIZE OF THE SCENE! LIKE AN X-RAY!
;198019 ratio1: tan(phi1). phi1=azimuth (xz plane)
;190819 ratio2: tan(phi2). phi2=elevation (z and a line perp. to xy-plane )

;240719 A function for radiograph case 1 or 2. Apparently we had no decreasing at any axis here! 
;110719 in case of radiograph problem geometry, then fall back to simpler pathfinder
 
 
 ;080819 temp comment this out! 
 if (ratio1c lt ratio1f_fun) then (uc_fun=uc_fun+1)  else (rc_fun=rc_fun+1)
 ;080819 the above only work for positive phi1 angles! 
 ;now we try the cam obsc criterion, hope not too rough image! No failed! too soon! so only for less than pi/2, in rad, angle phi1 does it work case 2!ok! keep it as such!
 
 
 if (debug_comments_fun eq 1) then print,'radiograph!,ratio1c,ratio1f_fun,uc_fun,rc_fun',ratio1c,ratio1f_fun,uc_fun,rc_fun

end











pro pathfinder_algo_for_ratio2_radiograph_case_1_and_2,ratio2c,ratio2f_fun,cc_fun,ratio1c,ratio1f_fun,uc_fun,rc_fun,debug_comments_fun
;190819 This pro calcs the ELEVATION pathfinding for each pair of steps along the LOS, FOR RADIOGRAPH CASES 1 AND TWO(RADIOGRAPH: case1: xz plane imaging, case 2: yz plane imaging).  
;190819 RADIOGRAPH MEANS NO FOCAL POINT, ALL LOS'S ARE PARALLEL TO EACH OTHER. FILM IS THE SIZE OF THE SCENE! LIKE AN X-RAY!
;198019 ratio1: tan(phi1). phi1=azimuth (xz plane)
;190819 ratio2: tan(phi2). phi2=elevation (z and a line perp. to xy-plane )
;
;240719 A pro for this case as well.

if (ratio2c lt ratio2f_fun) then (cc_fun=cc_fun+1)  else if (ratio1c lt ratio1f_fun) then (uc_fun=uc_fun+1)  else (rc_fun=rc_fun+1)

if (debug_comments_fun eq 1) then print,'radiograph!,ratio2c,ratio2f_fun,cc_fun,ratio1c,ratio1f_fun,uc_fun,rc_fun',ratio2c,ratio2f_fun,cc_fun,ratio1c,ratio1f_fun,uc_fun,rc_fun
if (debug_comments_fun eq 1) then print,'radiograph!,(ratio2c lt ratio2f_fun),(ratio1c lt ratio1f_fun)',(ratio2c lt ratio2f_fun),(ratio1c lt ratio1f_fun)

end










pro pathfinder_algo_and_stagnation_detector_for_ratio1_camera_obscura_case_3,ratio1c,ratio1f_fun,debug_comments_fun,rc_fun,uc_fun,stagnant_signal_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun
;;190819 This pro calcs the azimuth pathfinding for each pair of steps along the LOS, FOR CAMERA OBSCURA CASE 3 (CAMERA OBSCURA: case3: xz plane imaging, case 4: yz plane imaging).  
;190819 There is also a stagnation detection portion, whereby detection of stagnant LOS steps is attempted (i.e. pathfinding criteria leading to no progress at all)
;;198019 ratio1: tan(phi1). phi1=azimuth (xz plane)
;190819 ratio2: tan(phi2). phi2=elevation (z and a line perp. to xy-plane )
;
;230719 we changed gt to ge here. Hope this does not mess with case 4. (Now working on case 3, XZ cam obsc!)
;230719 seems to have a problem when ratio1f goes above 90 degrees, i.e. for FP_XZ centered, then for the left half (roughly) of x, we have a problem in the final image 
;FOLLOWING WORKS FOR yz CAQM OBSC! NOT FOR xz CAM OBSC!
;230719 made it le!!! trying it to stay in the prism cone within ratio1f! ALSO PUT A MINUS BEFORE THE FIX! NOTE REVERSAL OF RATIO1F aND 1C versus case 4 YZ above!!! SOS! 

;060819 this does not switch sides, if signs are opposite! SOS!!! correct that!!!! Else, it tends to the opposite of it!!! Introduce a  ALSO DO IT TO THE NEXT CRITERION, which also includes this one!
;070819 in order to switch sides in ratio1=y/x, in xz mode, , you need to alter x, i.e. rc, NOT uc 
;060819 OLD ONE, working only for positive ratio1f! KEEP IT FOR SAFETY! if (abs(ratio1c) ge abs(ratio1f_fun)) then (rc_fun=rc_fun+fix((ratio1c-ratio1f_fun+0.01)/abs(ratio1c-ratio1f_fun+0.01)) ) else (  uc_fun=uc_fun+1 ) 

;060819 CAREFUL we now add that!it matters the order of ratios in the fraction to the righ of the RHS!Also, nom and denom must be the same!
;;070819 in order to switch sides in ratio1=y/x, in xz mode, , you need to alter x, i.e. rc, NOT uc 
;070819 silly adition! uc_fun cant go neg in XZ mode here! LOS can turn, but cant go backwards! 
;070819 in order to switch sides in ratio1=y/x, in xz mode, , you need to alter x, i.e. rc, NOT uc 
if (abs(ratio1c) ge abs(ratio1f_fun)) then (rc_fun=rc_fun+fix((ratio1c-ratio1f_fun+0.01)/abs(ratio1c-ratio1f_fun+0.01)) ) else  begin
 uc_fun=uc_fun+1 
 ;070819 if inside cone, if correct side, it raises one, else towards axis! hope behaves well at crossing zero! well see!
 ;070819 no,this ruins things!  if (ratio1c lt ratio1f_fun ) then (rc_fun=rc_fun+1) else (rc_fun=rc_fun-1) 
 
 ;070819 keep old one for ref if (ratio1c*ratio1f_fun lt 0.0) then (rc_fun=rc_fun+fix((ratio1c*ratio1f_fun)/abs(ratio1c*ratio1f_fun) +0.001 )  )
 ;070819 some drastic action! It should only have to work just once, to take us across the zero, hopefully early on along the LOS! AND IT WORKS!!! OK!
 if (ratio1c*ratio1f_fun lt 0.0) then (rc_fun=-rc_fun )
 endelse 
;  if (abs(ratio1c) ge abs(ratio1f_fun)) then (rc_fun=rc_fun+fix((ratio1c-ratio1f_fun+0.01)/abs(ratio1c-ratio1f_fun+0.01)) ) else (uc_fun=uc_fun+(ratio1f_fun-ratio1c)/(abs(ratio1f_fun-ratio1c)))

;070819 in order to switch sides in ratio1=y/x, in xz mode, , you need to alter x, i.e. rc, NOT uc 
;060819 CAREFUL we now add that!it matters the order of ratios in the fraction to the righ of the RHS!Also, nom and denom must be the same!
;if (ratio1c*ratio1f_fun lt 0.0) then uc_fun=uc_fun+(ratio1f_fun-ratio1c)/(abs(ratio1f_fun-ratio1c))

 if (debug_comments_fun eq 1) then print,'camera obscura XZ STAGE 1!,abs(ratio1c),abs(ratio1f_fun),rc_fun,uc_fun,fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))',abs(ratio1c),abs(ratio1f_fun),rc_fun,uc_fun,fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))



;**********************************************************************
;240719 Stagnation detection portion for case 3 ratio1.

;
;;230719stagnation count for case 3 only


;230719 this is for imaging case 3, i.e. XZ cam obsc!
if ((abs(ratio1c) ge abs(ratio1f_fun)) eq 0) and ( fix((ratio1c-ratio1f_fun+0.01)/abs(ratio1c-ratio1f_fun+0.01)) eq 0  )  then begin

;200719 This, already there, simply increases a global count of stagnant steps.
error_stagnant_los_fun=error_stagnant_los_fun+1.0
;200719 This one draws a 2D stagnation image, each LOS's stagnation count summing up to such an image element!
;220719 employ first strragnation imaging here!
stagnant_signal_fun(second_coord_fun,nz10_fun)= stagnant_signal_fun(second_coord_fun,nz10_fun)+1.0

 if (debug_comments_fun eq 1) then print,'one more stagnation added here!','stagnant_signal_fun(second_coord_fun,nz10_fun)',stagnant_signal_fun(second_coord_fun,nz10_fun)

;200719 try this:
;rc_fun=rc_fun+1 
 if (debug_comments_fun eq 1) then print,'rc_fun=rc_fun+1 , override activated'

endif


;*****************
;240719 Some older comments on the issue.
;
;200719 now add a quantitative criterion to break over the stagnation and advance the los in space, quantitatively select which of the two directions!
;do it only for camera obscura!!! no stagnation bothering us in radiograph! (Check it up though sometime!)

;if ((imaging_geometry_selector_fun eq 3) or (imaging_geometry_selector_fun eq 4)) then begin

;200719 beefed up the else part, for the current nested case of stagnation! Try a move right precedence for angle 1! i.e. advance along x, instead of doing nothing!
;(rc_fun=rc_fun+1) 

;200719 re-check if it worked! ADD THE BEEFED UP COMPONENT AT THE SECONDPART SOS!
;if ( ((abs(ratio1c) gt abs(ratio1f_fun)) eq 0) and ((rc_fun=rc_fun+1) ) eq 1  )  then begin
;;stagnant_signal_override_fun(second_coord_fun,nz10_fun)=stagnant_signal_override_fun(second_coord_fun,nz10_fun)+1.0
; if (debug_comments_fun eq 1) then print,'************stagnantion override failed******'
;endif

;endif
;****************


;*****************************************************
end








pro pathfinder_algo_and_stagnation_detector_for_ratio2_camera_obscura_case_3,uc_fun,ratio2c,ratio2f_fun,rc_fun,cc_fun,debug_comments_fun,ratio1f_fun,ratio1c,stagnant_signal2_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun
;;190819 This pro calcs the elevation pathfinding for each pair of steps along the LOS, FOR CAMERA OBSCURA CASE 3 (CAMERA OBSCURA: case3: xz plane imaging, case 4: yz plane imaging).  
;190819 There is also a stagnation detection portion, whereby detection of stagnant LOS steps is attempted (i.e. pathfinding criteria leading to no progress at all)
;;198019 ratio1: tan(phi1). phi1=azimuth (xz plane)
;190819 ratio2: tan(phi2). phi2=elevation (z and a line perp. to xy-plane )


;last working one: 220719 if (abs(ratio2c) ge abs(ratio2f_fun)) then (rc_fun=rc_fun+1) else (  cc_fun=cc_fun+fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c))  ) 
;220719 we now have zero stagnation2, and also zero stagnation 1 of course. So, in total, no stagnation. Still, no symmetry and 'some' valley persists!
;220719 So we aim for a symmetric output, as a final step! At such small size for both angles, this means that above and below ratio2f we got slightly different behavior!
;220719 Note that calling again criterion1, aka radiograph,  did not work well, caused major ripples!

;220719 second portion of the following, is NOT symmetric!FIX[(a-b)/abs(a-b)] is different for a>b and for a<b! SOS make it symmetric somehow! dO IT! 

;230719 for case imaging 3 (cam obsc XZ) we advance UP(y) instead of RIGHT (x), whenever ratio2c falls above ratio2f target. That is because primary LOS direction is along Y, not x, in case 3!
;LAST KNOWN TO WORK FOR XZ CAM OBSC 230719: if (abs(ratio2c) ge abs(ratio2f_fun)) then (uc_fun=uc_fun+1) else (  cc_fun=cc_fun+fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c))  ) 

;230719 experimental 
 if (abs(ratio2c) ge abs(ratio2f_fun)) then begin
 
  if (abs(ratio1c) ge abs(ratio1f_fun)) then rc_fun=rc_fun+fix((ratio1c-ratio1f_fun+0.01)/abs(ratio1c-ratio1f_fun+0.01))  else   uc_fun=uc_fun+1 
  
  ;060819 CAREFUL we now add that!it matters the order of ratios in the fraction to the righ of the RHS!Also, nom and denom must be the same!
;060819 temp comment out, till it works!
;if (ratio1c*ratio1f_fun lt 0.0) then uc_fun=uc_fun+(ratio1f_fun-ratio1c)/(abs(ratio1f_fun-ratio1c))
 endif else begin 
 (  cc_fun=cc_fun+fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c))  ) 
endelse

;CRITERION 1 FOR XZ: if (abs(ratio1c) ge abs(ratio1f_fun)) then (rc_fun=rc_fun+fix((ratio1c-ratio1f_fun+0.01)/abs(ratio1c-ratio1f_fun+0.01)) ) else (  uc_fun=uc_fun+1 ) 

;if (abs(ratio1c) gt abs(ratio1f_fun)) then (rc_fun=rc_fun+1)  else  (  uc_fun=uc_fun+fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))  )  

;if (ratio2c lt ratio2f_fun) then (cc_fun=cc_fun+1)  else if (ratio1c lt ratio1f_fun) then (uc_fun=uc_fun+1)  else (rc_fun=rc_fun+1)
 
;if (abs(ratio1c) gt abs(ratio1f_fun)) then (rc_fun=rc_fun+1)  else  (  uc_fun=uc_fun+fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))  ) 
 
;if (ratio1c lt ratio1f_fun) then (uc_fun=uc_fun+1)  else (rc_fun=rc_fun+1)


;
;IF (A EQ 2) AND (B EQ 3) THEN BEGIN 
;   PRINT, 'A = ', A 
;   PRINT, 'B = ', B 
;ENDIF ELSE BEGIN 
;   IF A NE 2 THEN PRINT, 'A <> 2' ELSE PRINT, 'B <> 3' 
;ENDELSE 
;first criterion :if (abs(ratio1c) gt abs(ratio1f_fun)) then (rc_fun=rc_fun+1) else (  uc_fun=uc_fun+fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))  ) 

if (debug_comments_fun eq 1) then print,'camera obscuraYZ !,(abs(ratio2c),abs(ratio2f_fun),rc_fun,cc_fun,fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c))',abs(ratio2c),abs(ratio2f_fun),rc_fun,cc_fun,fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c))





;;200719 attempt to use first criterion, failed, gives not good pic
;if (abs(ratio2c) lt abs(ratio2f_fun)) then (cc_fun=cc_fun+1) else begin
; if (abs(ratio1c) gt abs(ratio1f_fun)) then (rc_fun=rc_fun+1) else (  uc_fun=uc_fun+fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))  )  
;;
;endelse


;********************************************
;240719 stagnation detection portion for criterion2. Means when the LOS advances nil after a criterion is applied.
;
;150419 want no stagnant los!
;220719 we also added here ge instead of gt:
if ((abs(ratio2c) ge abs(ratio2f_fun)) eq 0) and ( fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c)) eq 0  )  then begin
;try this! 200719SOS THIS REMOVED THE VALLEY, but blurred the left side!
;220719 CAREFUL! We cut this off temporarily, and also out ge in place of gt for the ratio2c criterio above!
;cc_fun=cc_fun+1
;200719 we add one to the denom no it increases cc out of proportion!
 ;cc_fun=cc_fun+fix((1+ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c+0.001))
;200719 This, already there, simply increases a global count of stagnant steps.
error_stagnant_los_fun=error_stagnant_los_fun+1.0
;200719 This one draws a 2D stagnation image, each LOS's stagnation count summing up to such an image element!
;220719 Now it is the second criterion here!
stagnant_signal2_fun(second_coord_fun,nz10_fun)= stagnant_signal2_fun(second_coord_fun,nz10_fun)+1.0
 if (debug_comments_fun eq 1) then print,'one more stagnation added here criterion2 do a sep array for each ratio stagnation!!','stagnant_signal2_fun(second_coord_fun,nz10_fun)',stagnant_signal2_fun(second_coord_fun,nz10_fun)
endif

;*****************
;;240719 older comments here
;200719 now add a quantitative criterion to break over the stagnation and advance the los in space, quantitatively select which of the two directions!
;do it only for camera obscura!!! no stagnation bothering us in radiograph! (Check it up though sometime!)

;if ((imaging_geometry_selector_fun eq 3) or (imaging_geometry_selector_fun eq 4)) then begin
;
;;200719 beefed up the else part, for the current nested case of stagnation! Try a climb move precedence for angle 2! i.e. advance along z, instead of doing nothing!
;;(cc_fun=cc_fun+1) 
;
;;200719 re-check if it worked! ADD THE BEEFED UP COMPONENT AT THE SECONDPART SOS!
;if ( ((abs(ratio2c) gt abs(ratio2f_fun)) eq 0) and ((cc_fun=cc_fun+1) ) eq 1  )  then begin
;;stagnant_signal_override_fun(second_coord_fun,nz10_fun)=stagnant_signal_override_fun(second_coord_fun,nz10_fun)+1.0
; if (debug_comments_fun eq 1) then print,'************stagnantion override failed******'
;endif
;
;endif
;*****************
;
;********************************************

end











pro OLD_ONE_pathfinder_algo_and_stagnation_detector_for_ratio1_camera_obscura_case_4, ratio1c,ratio1f_fun,debug_comments_fun,rc_fun,uc_fun,stagnant_signal_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun
;;190819 This OLDER (not called at the momnet, but still here as a backup) pro calcs the azimuth pathfinding for each pair of steps along the LOS, FOR CAMERA OBSCURA CASE 4 (CAMERA OBSCURA: case3: xz plane imaging, case 4: yz plane imaging).  
;190819 There is also a stagnation detection portion, whereby detection of stagnant LOS steps is attempted (i.e. pathfinding criteria leading to no progress at all)
;;198019 ratio1: tan(phi1). phi1=azimuth (xz plane)
;190819 ratio2: tan(phi2). phi2=elevation (z and a line perp. to xy-plane )


;080419 this twin if setup allows for negative ratio1f, aka tan of phi1!
;110419 FINALLY imitate the original criterion, in a symmetric manner, using abs function.
;130419 edited right hand stuff here... seems to hold ratio1c close to ratio1f now, must test with many cases though.... SOS do ze tests! 

;130419 SOS some combos stuck at the same point so no LOS advance, careful! must put a way out if no move at all SOS!
;130419 CAREFUL the added 0.01 at the DENOM prevents LOS from getting STUICK at same place for a given combo of numbers !

if (abs(ratio1c) gt abs(ratio1f_fun)) then (rc_fun=rc_fun+1) else (  uc_fun=uc_fun+fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))  ) 

;150419 want no stagnant los!

if ((abs(ratio1c) gt abs(ratio1f_fun)) eq 0) and ( fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01)) eq 0  )  then begin

(error_stagnant_los_fun=error_stagnant_los_fun+1.0)

;200719 This one draws a 2D stagnation image, each LOS's stagnation count summing up to such an image element!
;220719 employ first strragnation imaging here!
stagnant_signal_fun(second_coord_fun,nz10_fun)= stagnant_signal_fun(second_coord_fun,nz10_fun)+1.0

 if (debug_comments_fun eq 1) then print,'one more stagnation added here!','stagnant_signal_fun(second_coord_fun,nz10_fun)',stagnant_signal_fun(second_coord_fun,nz10_fun)

endif


end









pro pathfinder_algo_and_stagnation_detector_for_ratio1_camera_obscura_case_4,ratio1c,ratio1f_fun,debug_comments_fun,rc_fun,uc_fun,stagnant_signal_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun
;;190819 This pro calcs the azimuth pathfinding for each pair of steps along the LOS, FOR CAMERA OBSCURA CASE 4 (CAMERA OBSCURA: case3: xz plane imaging, case 4: yz plane imaging).  
;190819 There is also a stagnation detection portion, whereby detection of stagnant LOS steps is attempted (i.e. pathfinding criteria leading to no progress at all)
;;198019 ratio1: tan(phi1). phi1=azimuth (xz plane)
;190819 ratio2: tan(phi2). phi2=elevation (z and a line perp. to xy-plane )


;240719 Active stagnation detector must be the same as pathfinder algo employed.

;*******************************
;240719 Relevant older comments here.
;
;080419 this twin if setup allows for negative ratio1f, aka tan of phi1!
;110419 FINALLY imitate the original criterion, in a symmetric manner, using abs function.
;130419 edited right hand stuff here... seems to hold ratio1c close to ratio1f now, must test with many cases though.... SOS do ze tests! 


;130419 SOS some combos stuck at the same point so no LOS advance, careful! must put a way out if no move at all SOS!
;130419 CAREFUL the added 0.01 at the DENOM prevents LOS from getting STUICK at same place for a given combo of numbers !

;110719 in case of cam obsc geometry, then use comlicated pathfinding.
;*******************************



;*************************************************
;240719 Pathfinder algo portion here. Currently employing the given non-commented version of it

;230719 we changed gt to ge here. Hope this does not mess with case 4. (Now working on case 3, XZ cam obsc!)
;230719 seems to have a problem when ratio1f goes above 90 degrees, i.e. for FP_XZ centered, then for the left half (roughly) of x, we have a problem in the final image 
;FOLOWING WORKS FOR yz CAQM OBSC! NOT FOR xz CAM OBSC!
if (abs(ratio1c) ge abs(ratio1f_fun)) then (rc_fun=rc_fun+1) else (  uc_fun=uc_fun+fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))  ) 

 if (debug_comments_fun eq 1) then print,'camera obscura YZ STAGE 1!,abs(ratio1c),abs(ratio1f_fun),rc_fun,uc_fun,fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))',abs(ratio1c),abs(ratio1f_fun),rc_fun,uc_fun,fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))

;*************************************************





;*******************************
;240719 Stagnation detector portion here. I.e. incremental detection of when a LOS not advancing at all after a pathfinder criterion has been applied.
if ((abs(ratio1c) ge abs(ratio1f_fun)) eq 0) and ( fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01)) eq 0  )  then begin

;200719 This, already there, simply increases a global count of stagnant steps.
error_stagnant_los_fun=error_stagnant_los_fun+1.0
;200719 This one draws a 2D stagnation image, each LOS's stagnation count summing up to such an image element!
;220719 employ first strragnation imaging here!
stagnant_signal_fun(second_coord_fun,nz10_fun)= stagnant_signal_fun(second_coord_fun,nz10_fun)+1.0

 if (debug_comments_fun eq 1) then print,'one more stagnation added here!','stagnant_signal_fun(second_coord_fun,nz10_fun)',stagnant_signal_fun(second_coord_fun,nz10_fun)

;200719 try this: potentially employ this only in case of stagnation detection
;rc_fun=rc_fun+1 
 ;if (debug_comments_fun eq 1) then print,'rc_fun=rc_fun+1 , override activated'

 endif
;******************************


end












pro OLD_ONE_pathfinder_algo_and_stagnation_detector_for_ratio2_camera_obscura_case_4,ratio2c,ratio2f_fun,rc_fun,cc_fun,debug_comments_fun,ratio1f_fun,ratio1c,stagnant_signal2_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun
;;190819 This pro (OLD ONE: PRESENT HERE ONLY AS A BACKUP, not currently called!) calcs the elevation pathfinding for each pair of steps along the LOS, FOR CAMERA OBSCURA CASE 4 (CAMERA OBSCURA: case3: xz plane imaging, case 4: yz plane imaging).  
;190819 There is also a stagnation detection portion, whereby detection of stagnant LOS steps is attempted (i.e. pathfinding criteria leading to no progress at all)

;198019 ratio1: tan(phi1). phi1=azimuth (xz plane)
;190819 ratio2: tan(phi2). phi2=elevation (z and a line perp. to xy-plane )


;***080419 this twin if setup allows for negative ratio2f, aka tan of phi2!
;if (ratio2c lt ratio2f) then (cc=cc+1)  else  (cc=cc-1)
;***08419 NEXT: if outside screen FOV in terms of phi2, then move a step right, towards intercepting the FOV cone.
;if (abs(ratio1c) gt abs(ratio1f)) then (rc=rc+1) else (  uc=uc+fix((ratio1f-ratio1c)/abs(ratio1f-ratio1c))  ) 
;130419 CAREFUL the added 0.01 at the DENOM prevents LOS from getting STUICK at same place for a given combo of numbers !
;only for ratio1 not here SOS! 
if (abs(ratio2c) gt abs(ratio2f_fun)) then (rc_fun=rc_fun+1) else (  cc_fun=cc_fun+fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c))  ) 




;***************************************************************************************************************************
;260719 stagnation detection part here

;150419 want no stagnant los!

;old one! if ( abs(ratio2c) lt abs(ratio2f) ) then (cc=cc+((cc+1)/abs(cc+1))) else  (rc=rc+1) 
;uc=uc+((uc+1)/abs(uc+1)) 

if ((abs(ratio2c) gt abs(ratio2f_fun)) eq 0) and ( fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c)) eq 0  )   then begin
;try this! 200719SOS THIS REMOVED THE VALLEY, but blurred the left side!
;220719 CAREFUL! We cut this off temporarily, and also out ge in place of gt for the ratio2c criterio above!
;cc_fun=cc_fun+1
;200719 we add one to the denom no it increases cc out of proportion!
 ;cc_fun=cc_fun+fix((1+ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c+0.001))
;200719 This, already there, simply increases a global count of stagnant steps.
error_stagnant_los_fun=error_stagnant_los_fun+1.0
;200719 This one draws a 2D stagnation image, each LOS's stagnation count summing up to such an image element!
;220719 Now it is the second criterion here!
stagnant_signal2_fun(second_coord_fun,nz10_fun)= stagnant_signal2_fun(second_coord_fun,nz10_fun)+1.0
 if (debug_comments_fun eq 1) then print,'one more stagnation added here criterion2 do a sep array for each ratio stagnation!!','stagnant_signal2_fun(second_coord_fun,nz10_fun)',stagnant_signal2_fun(second_coord_fun,nz10_fun)
endif
;***************************************************************************************************************************



end
















pro pathfinder_algo_and_stagnation_detector_for_ratio2_camera_obscura_case_4,ratio2c,ratio2f_fun,rc_fun,cc_fun,debug_comments_fun,ratio1f_fun,ratio1c,stagnant_signal2_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun
;;190819 This pro calcs the elevation pathfinding for each pair of steps along the LOS, FOR CAMERA OBSCURA CASE 4 (CAMERA OBSCURA: case3: xz plane imaging, case 4: yz plane imaging).  
;190819 There is also a stagnation detection portion, whereby detection of stagnant LOS steps is attempted (i.e. pathfinding criteria leading to no progress at all)

;198019 ratio1: tan(phi1). phi1=azimuth (xz plane)
;190819 ratio2: tan(phi2). phi2=elevation (z and a line perp. to xy-plane )

;
;
;
;
;240719 stagnant_signal2 means ratio2, for both caes 3 and 4. It detects, through an image, stagnation when criterion 2 (examining ratio 2) is applied.
;last working one: 220719 if (abs(ratio2c) ge abs(ratio2f_fun)) then (rc_fun=rc_fun+1) else (  cc_fun=cc_fun+fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c))  ) 
;220719 we now have zero stagnation2, and also zero stagnation 1 of course. So, in total, no stagnation. Still, no symmetry and 'some' valley persists!
;220719 So we aim for a symmetric output, as a final step! At such small size for both angles, this means that above and below ratio2f we got slightly different behavior!
;220719 Note that calling again criterion1, aka radiograph,  did not work well, caused major ripples!

;220719 second portion of the following, is NOT symmetric!FIX[(a-b)/abs(a-b)] is different for a>b and for a<b! SOS make it symmetric somehow! dO IT! 
if (abs(ratio2c) ge abs(ratio2f_fun)) then (rc_fun=rc_fun+1) else (  cc_fun=cc_fun+fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c))  ) 


;if (abs(ratio1c) gt abs(ratio1f_fun)) then (rc_fun=rc_fun+1)  else  (  uc_fun=uc_fun+fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))  )  

;if (ratio2c lt ratio2f_fun) then (cc_fun=cc_fun+1)  else if (ratio1c lt ratio1f_fun) then (uc_fun=uc_fun+1)  else (rc_fun=rc_fun+1)
 
;if (abs(ratio1c) gt abs(ratio1f_fun)) then (rc_fun=rc_fun+1)  else  (  uc_fun=uc_fun+fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))  ) 
 
;if (ratio1c lt ratio1f_fun) then (uc_fun=uc_fun+1)  else (rc_fun=rc_fun+1)


;
;IF (A EQ 2) AND (B EQ 3) THEN BEGIN 
;   PRINT, 'A = ', A 
;   PRINT, 'B = ', B 
;ENDIF ELSE BEGIN 
;   IF A NE 2 THEN PRINT, 'A <> 2' ELSE PRINT, 'B <> 3' 
;ENDELSE 
;first criterion :if (abs(ratio1c) gt abs(ratio1f_fun)) then (rc_fun=rc_fun+1) else (  uc_fun=uc_fun+fix((ratio1f_fun-ratio1c+0.01)/abs(ratio1f_fun-ratio1c+0.01))  ) 

if (debug_comments_fun eq 1) then print,'camera obscuraYZ !,(abs(ratio2c),abs(ratio2f_fun),rc_fun,cc_fun,fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c))',abs(ratio2c),abs(ratio2f_fun),rc_fun,cc_fun,fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c))







;********************************************
;240719 stagnation detection portion for criterion2. Means when the LOS advances nil after a criterion is applied.
;
;150419 want no stagnant los!
;220719 we also added here ge instead of gt:
if ((abs(ratio2c) ge abs(ratio2f_fun)) eq 0) and ( fix((ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c)) eq 0  )  then begin
;try this! 200719SOS THIS REMOVED THE VALLEY, but blurred the left side!
;220719 CAREFUL! We cut this off temporarily, and also out ge in place of gt for the ratio2c criterio above!
;cc_fun=cc_fun+1
;200719 we add one to the denom no it increases cc out of proportion!
 ;cc_fun=cc_fun+fix((1+ratio2f_fun-ratio2c)/abs(ratio2f_fun-ratio2c+0.001))
;200719 This, already there, simply increases a global count of stagnant steps.
error_stagnant_los_fun=error_stagnant_los_fun+1.0
;200719 This one draws a 2D stagnation image, each LOS's stagnation count summing up to such an image element!
;220719 Now it is the second criterion here!
stagnant_signal2_fun(second_coord_fun,nz10_fun)= stagnant_signal2_fun(second_coord_fun,nz10_fun)+1.0
 if (debug_comments_fun eq 1) then print,'one more stagnation added here criterion2 do a sep array for each ratio stagnation!!','stagnant_signal2_fun(second_coord_fun,nz10_fun)',stagnant_signal2_fun(second_coord_fun,nz10_fun)
endif

;*****************
;;240719 older comments here
;200719 now add a quantitative criterion to break over the stagnation and advance the los in space, quantitatively select which of the two directions!
;do it only for camera obscura!!! no stagnation bothering us in radiograph! (Check it up though sometime!)

;if ((imaging_geometry_selector_fun eq 3) or (imaging_geometry_selector_fun eq 4)) then begin
;
;;200719 beefed up the else part, for the current nested case of stagnation! Try a climb move precedence for angle 2! i.e. advance along z, instead of doing nothing!
;;(cc_fun=cc_fun+1) 
;
;;200719 re-check if it worked! ADD THE BEEFED UP COMPONENT AT THE SECONDPART SOS!
;if ( ((abs(ratio2c) gt abs(ratio2f_fun)) eq 0) and ((cc_fun=cc_fun+1) ) eq 1  )  then begin
;;stagnant_signal_override_fun(second_coord_fun,nz10_fun)=stagnant_signal_override_fun(second_coord_fun,nz10_fun)+1.0
; if (debug_comments_fun eq 1) then print,'************stagnantion override failed******'
;endif
;
;endif
;*****************
;
;********************************************

end







pro t_picked_definition,debug_comments_fun,count_tpicked_fun, backwards_in_time_fun,tpicked_fun,t_fun,shotmax_fun,shotnumber_tpicked_fun,shotmin_fun,t0LOS_fun
;190819 This pro calcs tpicked, i.e. from where we begin the calculation backwards in time, in camera obscura mode, or in radiograph.
;190819 There is a relevant parameter in the param file, for 'backwards in time'. tpicked is really useful when that param is turned on.
;190819 Radiograph may operate either ahead in time or back in time, back from tpicked.  Camera obscura is meant to only use back in time, i.e. setting tpicked is required there. 
;
;
;150719 this procedure sets up the time stuff before the execution of the imaging part


if (backwards_in_time_fun eq 1.0) then begin

;230419 tpicked is the camera trigger time. It should be selected to lie towards the end of the temporal domain, 
;since we move backwards in time from it. We now replace (tpicked-t) in place of t, at the where function. Also, we should not set a 
; a time span larger than tpicked. also employ a break construct, to prevent from attempting to reach negative times.
tpicked_fun=t_fun(shotmax_fun)

;020519 may adjust tpicked by hand at an earlier time, but careful with those settings!
;tpicked=310
;230419 this defines the time snapshot corresponding (larger or equal time) to the selected camera trigger time tpicked. It is a 
;moment towards the end of the data normally, from where we move back in time through the data. 


;201222 Since tpicked_fun is now set above to t_fun(shotmax_fun) then we now do a cyclical calc (next TWO lines of code), 
;re-calcing t_fun(shotmax_fun) here. 
;But we do keep generality this way, in case tpicked becomes smaller that tshotmax in the future...

aaaa_tpicked=where(((tpicked_fun-t_fun))<0.0,count_tpicked_fun)

if (debug_comments_fun eq 1) then print,'tracker 1'
;150719 SOS something is WRONG with the fololowinbg! WHY DO WE NEED TO DEDUCT shotmin ? tpicked is at shotmax here, period!
;
;OLD ONE VERSION: shotnumber_tpicked_fun=size(t_fun, /n_elements)-count_tpicked_fun-shotmin_fun

;updated version 150719!
shotnumber_tpicked_fun=size(t_fun, /n_elements)-count_tpicked_fun
if (debug_comments_fun eq 1) then print,'tracker 2'
if (debug_comments_fun eq 1) then print,'EDO ORE PAIDI! t_fun(shotnumber_tpicked_fun),shotnumber_tpicked_fun,size(t_fun, /n_elements),count_tpicked_fun,shotmin_fun', t_fun(shotnumber_tpicked_fun),shotnumber_tpicked_fun,size(t_fun, /n_elements),count_tpicked_fun,shotmin_fun
print, 'shotnumber_tpicked_fun',shotnumber_tpicked_fun;230419 initialize shotnumber to that of tpicked. This is the actual LOS origin time. NOT tpicked, but the snapshot directly before that. Only way to do it.
;301019 NONONO!!! we need shotnumber_tpicked_fun-shotmin-fun!! we may start at a later timeshot than the first !!! wait 1 correct this point!
shotnumber_fun=shotnumber_tpicked_fun-shotmin_fun
if (debug_comments_fun eq 1) then stop
;SOS 230419  shotmin and shotmax now take a whole new meaning! Shotmax should be larger than the minimum time shot exceeding the time of beginning of the back-in-time 
;calc, tpicked. aND SHOTMIN SHOULD BE SMALL ENOUGH TO ALLOW FOR THE CALC TO ADVANCE.
;*****************************************************

;SOS 230419  shotmin and shotmax now take a whole new meaning! Shotmax should be larger than the minimum time shot exceeding the time of beginning of the back-in-time 
;calc, tpicked. aND SHOTMIN SHOULD BE SMALL ENOUGH TO ALLOW FOR THE CALC TO ADVANCE.
;****************************************************************************************
;150719 idl array index begins at zero
;170719 yes, but this messes all others now! Now we must load till the one shot before the last one!
t0LOS_fun=double(t_fun(shotnumber_tpicked_fun-1))
if (debug_comments_fun eq 1) then print,'EDO ORE to t0los',t0LOS_fun
;***************************************************************************************
endif   ELSE BEGIN 

;OLD WAY: t0LOS=t(shotmin)
t0LOS_fun=t_fun(shotmin_fun)
shotnumber_tpicked_fun=shotmin_fun
endelse

end






pro time_location,backwards_in_time_fun,debug_comments_fun,shotnumber_tpicked_fun,t_fun,curtime_fun,t0LOS_fun,lloscurrent_fun,clight_fun,count_fun,shotnumber_fun,shotmin_fun,shotmax_fun,count
;190819 This pro finds where we are along the LOS. Very important! Either for back in time, or ahead in time mode (there is a selection, depending on the value of the relevant 'back in time' param, from the param file!)
;190819 This pro is called within the losloop pro, the one that calcs along a LOS!

;060719 SOS the following should be kept in non-fun mode during the function call!  t0LOS,lloscurrent,curtime,count

;060719 here we have a new pro that determines where we are in the timeline, depending on the problem setup, i.e. either marching ahead in time, or 
;going back. We also try a test case here, like we did in the original rlos code. 

;SUPER SOS: curtime, begins when tlos=0, NOT when T=0 (in t array). MUST account for ze difference SOS!

; 260514 CHECK time units used here vs PLUTO time units used in the dbl.out file (and in the code of course)
;in current dbl.out, there are 21 snapshots and max time is less than 0.5
;SUPER 070915 we must initialize the los time 
;at when it begins, i.e. t(shotmin).time of the first snapshot else it resets to zero
;and we cannot catch the time delays along the los t0LOS


;**************************************************************
;230419 now the LOS does not begin a t(shotmin, but at t(shotnumber_tpicked), and it goes back in time)

;070519 do a procedure with all these here, cos there are some!
;OLD WAY: t0LOS=t(shotmin)
;
;060719 SUPER SOS here ALSO add an if then else for bak in time! SOSARA SOS!

;090719 SOS the following only goes for back in time LOS drawing. 

;*********************************************************************




if (backwards_in_time_fun eq 1) then begin

curtime_fun=double(t0LOS_fun-(lloscurrent_fun/double(clight_fun)))
if (debug_comments_fun eq 1) then print,'curtime_fun,t0LOS_fun,lloscurrent_fun,clight_fun',curtime_fun,t0LOS_fun,lloscurrent_fun,clight_fun
if (debug_comments_fun eq 1) then print,'back in time los draw'
;150719 tolos is now defined at the prodecure t_picked_definition
;230419 we now edit this to go back in time from camerra trigger time: tpicked.
;tpicked_array=
;aaaa=where(((curtime-t))<0,count)


;********************************************************************************************
;;060819 we adapt,for here, this snippet from tpicked def in relevant procedure!!
;230419 tpicked is the camera trigger time. It should be selected to lie towards the end of the temporal domain, 
;since we move backwards in time from it. We now replace (tpicked-t) in place of t, at the where function. Also, we should not set a 
; a time span larger than tpicked. also employ a break construct, to prevent from attempting to reach negative times.

;171119 tpicked is a param read from external param file! CAREFUL HERE! clarify we now set tpicked to t(shotmax)! We hare NOT USING tpicked any moar! SOS!
tpicked_fun=t_fun(shotmax_fun)

;020519 may adjust tpicked by hand at an earlier time, but careful with those settings!
;tpicked=310
;230419 this defines the time snapshot corresponding (larger or equal time) to the selected camera trigger time tpicked. It is a 
;moment towards the end of the data normally, from where we move back in time through the data. 
;aaaa_tpicked=where(((tpicked_fun-t_fun))<0,count_tpicked_fun)
;*********************************************************************************************



;060819 latest effort to do this right!
;201222 something fishy here: count_tpicked_fun revolves in a narrow range, maybe below calc is a bit wrong
;we add twice t_fun below??? no! re-think it!
;old way from 2019 aaaa_back_in_time_tpicked=where((   (curtime_fun +t_fun)  -   (tpicked_fun-t_fun))<0,count_tpicked_fun)
;201222 now replace tpicked_fun calculation  with tshotmax directly loss of generality here SOS 
;201222  aaaa_back_in_time_tpicked=where((   (curtime_fun +t_fun)  -   (t_fun(shotmax_fun)))<0,count_tpicked_fun)
;020123 SOS this keeps count_tpicked_fun at one step before end of times, so always tmax-1 snapshot being used. NO!!! CORRECT IT!
;aaaa_back_in_time_tpicked=where((   (curtime_fun)  -   (t_fun(shotmax_fun)))<0,count_tpicked_fun)
;SOS 020123 t_fun is meant to be an array!!! SOS the trick below aims to locate current time's position in the array t_fun. 
;020123 SOS now seems to work! lts check though!!;040123 this gives pos backwards from tlast. NOT from shotmax, which in general is smaller than tmax.
;aaaa_back_in_time_tpicked=where((   (curtime_fun)  -   (t_fun))<0,count_tpicked_fun)
;so we amend it. 040123
aaaa_back_in_time_tpicked=where((   (curtime_fun)  -   (t_fun))<0,count_tpicked_fun)
;;040123 count_tpicked_fun extracted above, is number of timeshots from tmax backwards to current timeshot

;201222 no! try same as fwd in time, since curtime_fun calc includes the reverse calc, then the following merely temporally locates the result of curtime calc.
; this wont cut it!aaaa_back_in_time_tpicked=where(((curtime_fun-t_fun))<0,count_tpicked_fun)

;fwd time calc here aaaa=where(((curtime_fun-t_fun))<0,count_fun)
;bwd time calc here curtime_fun=double(t0LOS_fun-(lloscurrent_fun/double(clight_fun)))

;aaaa_tpicked=where(((tpicked_fun-t_fun))<0,count_tpicked_fun)

;print, where((curtime-t)<0,count)
if (debug_comments_fun eq 1) then print, 'shotmax_fun, shotmin_fun, count_tpicked_fun, shotmax_fun-count_tpicked_fun-shotmin_fun',shotmax_fun, shotmin_fun, count_tpicked_fun,shotmax_fun-count_tpicked_fun-shotmin_fun
if (debug_comments_fun eq 1) then print,'size(t_fun, /n_elements)-count_tpicked_fun,(size(t_fun, /n_elements)-count_tpicked_fun)-shotmin_fun', size(t_fun, /n_elements)-count_tpicked_fun,(size(t_fun, /n_elements)-count_tpicked_fun)-shotmin_fun
if (debug_comments_fun eq 1) then print,'1a2b,aaaa_back_in_time_tpicked=where((   (curtime_fun +t_fun)  -   (t_fun))<0,count_tpicked_fun)'
if (debug_comments_fun eq 1) then print,aaaa_back_in_time_tpicked,curtime_fun,t_fun,tpicked_fun,count_tpicked_fun,'aaaa_back_in_time_tpicked,curtime_fun,t_fun,tpicked_fun,count_tpicked_fun,'
if (debug_comments_fun eq 1) then print,'size(t_fun, /n_elements)', size(t_fun, /n_elements)
if (debug_comments_fun eq 1) then print,'testing:count_tpicked_fun,curtime_fun',count_tpicked_fun,curtime_fun
;next is snapshot number, give or take 1, for a current time of curtime.
if (debug_comments_fun eq 1) then print,'size(t_fun, /n_elements)-count_tpicked_fun', size(t_fun, /n_elements)-count_tpicked_fun
;ultra 080915 we add minus shotmin here, else it counts shotnumber
;over the whole time since the beginning of the sim,
;while we just want since the beginning of the LOS, i.e. -shotmin

; 110915 check shotnumber in time pluto units 
;is it advancing too slow perhap[s? check!!
;counter means how many loop steps, NOT count, count is ok!! see above!!
;but each loop step
;los advances by clight, which is not 1 in general sos!!
;230419 changed this to go back in time: shotnumber is now set by the tpicked
;230419 changed it again, cos we did more than necessary (overcalced!)
;SOS 230419 NO NEED TO deduct again shotmin, already accounted for that when defining shotnumber_tpicked. ONLY DEDUCT count from tpicked, i.e. tpicked-tloc_crossingtime.
;shotnumber= shotnumber_tpicked-count


;060819 this is a problematic definition for back in time (camera obscura): If tpicked is max, i.e. shotmax, then it works! But 
;060819 if tpicked is less than max, then it gives neg shotnumber!! Must correct, in connection to count definition above!!
;shotnumber_fun= shotnumber_tpicked_fun
;060819 try this! i.e. only employ tpicked for defining the start of the calc in time, but then use shotmax for finding where we are!
;301019 we deduct shotmin from shotnumber, to allow starting from later  snapshot for camera obscura!
; 2019 version shotnumber_fun= shotnumber_tpicked_fun-count_tpicked_fun-shotmin_fun
;040123 SOS the following only works when we begin at shotmin=1
;shotnumber_fun= shotmax_fun-count_tpicked_fun-shotmin_fun

;040123 lets make it work with moar shotmin choices!
;040123 count_tpicked_fun is number of timeshots from tmax backwards to current timeshot
;shotnumber_fun= (size(t_fun, /n_elements)-count_tpicked_fun) 
;shotnumber_fun= shotmax_fun-count_tpicked_fun-shotmin_fun
;Absolute number of timeshot, in overalll time array!(size(t_fun, /n_elements)-count_tpicked_fun)
;shotnumber, though is measured from shot(tmin) to shot(tmax), in timeshot steps
;seems to work for both tshotmax<tmax and tshotmin > tmin=0
shotnumber_fun=(size(t_fun, /n_elements)-count_tpicked_fun)-shotmin_fun
;201222 fwd in time version: shotnumber_fun=size(t_fun, /n_elements)-count_fun-shotmin_fun 

;060819 even better:define count relative to tpicked, not shotmax!but then need a new array tpicked_somerthing, in place of t, etc etc! So for simplicxity try the above for starters!

if (debug_comments_fun eq 1) then print,'shotnumber_fun,(shotnumber_tpicked_fun-count_tpicked_fun),shotnumber_tpicked_fun,count_tpicked_fun',shotnumber_fun,(shotnumber_tpicked_fun-count_tpicked_fun),shotnumber_tpicked_fun,count_tpicked_fun
;shotnumber_tpicked_fun=size(t_fun, /n_elements)-count_tpicked_fun

;shotnumber= (count-shotmin)
if (debug_comments_fun eq 1) then print, 'count_tpicked_fun, count_tpicked_fun-shotmin,shotnumber_fun',count_tpicked_fun, count_tpicked_fun-shotmin_fun,shotnumber_fun

endif ELSE BEGIN 

;150719 tolos is now set in the previous procedure tpicked definition

;090919 in case of radiogragh, then curtime is calced the OLD WAY!
;curtime=lloscurrent/clight+t0LOS
curtime_fun=lloscurrent_fun/clight_fun+t0LOS_fun
if (debug_comments_fun eq 1) then print,'ahead in time los draw'

;150719 initialize it to shotmin!
;shotnumber=shotmin



;******************************************************
;aaaa=where((curtime-t)<0,count)

aaaa=where(((curtime_fun-t_fun))<0,count_fun)
;print, where((curtime-t)<0,count)
if (debug_comments_fun eq 1) then print, size(t_fun, /n_elements)
if (debug_comments_fun eq 1) then print,'testing',count_fun,curtime_fun
;next is snapshot number, give or take 1, for a current time of curtime.
if (debug_comments_fun eq 1) then print, size(t_fun, /n_elements)-count_fun

;******************************************
;060719 SOS next line is copy pasted from the radiograph code of 251218 sos
;shotnumber=size(t, /n_elements)-count-shotmin
shotnumber_fun=size(t_fun, /n_elements)-count_fun-shotmin_fun 
;301019 try this one!  shotmin plus progress so far, i.e. count!
;shotnumber_fun=count_fun+shotmin_fun 
if (debug_comments_fun eq 1) then print,'fwd in time!,shotnumber_fun,shotnumber_tpicked_fun,count_fun',shotnumber_fun,shotnumber_tpicked_fun,count_fun

;shotnumber= (count-shotmin)

ENDELSE 

if (debug_comments_fun eq 1) then print,'curtime_fun,t0LOS_fun,lloscurrent_fun,clight_fun',curtime_fun,t0LOS_fun,lloscurrent_fun,clight_fun

;if (debug_comments_fun eq 1) then print, 'count_tpicked_fun,count_fun, count_fun-shotmin',count_tpicked_fun,count_fun, count_fun-shotmin_fun
 
;***********************************************************

;if (debug_comments_fun eq 1) then print, 'shotnumber_tpicked - ( size(t, /n_elements)-count-shotmin  ),( size(t, /n_elements)-count-shotmin  )',shotnumber_tpicked_fun - ( size(t, /n_elements)-count_fun-shotmin_fun  ),( size(t, /n_elements)-count_fun-shotmin_fun  )


; 090915 here is key determining calculation of current time
;along the los. so far, llos is in 10^10 cm, i.e. in pluto l units
;furthermore, clight is in adjusted for shrink sfactor cells per second 
;(not per time unit of pluto) re-check this point !!
;t0los should be in pluto time units.
; check in what time units is actually the calculation of clight.
;pluto time units, or seconds? 
;
; 050815 check if t0los should be somenthing else at 
;the beginning of the LOS. but perhaps not

; 050815 the following curtimepluto thing is based on shotind
;but shotind is only used in the initial assignement loop
;for temporal data array 
;shotind should not be used any more in the code correct it here.
;shotnumber should be used in its place sos

; 050815 what is the meaning of curtimepluto? none! it is not used anywhere! 
;just a leftover it seems

;curtimepluto=t(shotind)

;280514
;tlos to tpluto, i.e. from sec to pluto time units, which are here 0.333 sec
; tlos in sec tpluto in 0.333 sec from units

; 070915 we deal in secs from pluto, not in 'pluto time units=0.333sec'. Therefore 
;we revert back to factort=1!!

; we uncomment this in order to avoid using factort=0.333 for time units, 
;stick with cgs units for now 

;080915  SUPER pluto maNuAL says time units are in L0/U0 units, therefore 
;we should use factort indeed! it is not in cgs seconds, it is in 
;pluto units, which in our case (see init.c for units) ends up 1/3 of a sec.


;080915b Above definition of curtime os based on clight and on pluto time index
;it is therefore in pluto time units already, as both its terms are
;in such units. No need for factort here it seems. need to recheck, though SOS
;curtime=curtime*factort

; 070915 NA ELEGXTHEI o xronos shotnumber vs count etc
;SOSARA! fix it

;********************************************************************************************


 ;*****************************************************************
;200419 brought earlier, but still within LOS loop (SOS LOS LOOP, not just image screen loop as tried before!), the calculation of current time position. Reason: tan_phi now is 4D. No big deal, but used for debugging (at least).

 ;070419 SOS HERE we must set time to go backwards from camera trigger (spacially referring to focal point location), back in time.


;230419 we now edit this to go back in time from camerra trigger time: tpicked.

;ultra 080915 we add minus shotmin here, else it counts shotnumber
;over the whole time since the beginning of the sim,
;while we just want since the beginning of the LOS, i.e. -shotmin

; 110915 check shotnumber in time pluto units 
;is it advancing too slow perhap[s? check!!
;counter means how many loop steps, NOT count, count is ok!! see above!!
;but each loop step
;los advances by clight, which is not 1 in general sos!!
;230419 changed this to go back in time: shotnumber is now set by the tpicked
;230419 changed it again, cos we did more than necessary (overcalced!)
;SOS 230419 NO NEED TO deduct again shotmin, already accounted for that when defining shotnumber_tpicked. ONLY DEDUCT count from tpicked, i.e. tpicked-tloc_crossingtime.

;050719 following works for camera obscura, i.e. back in time. But, so far, for radiograph
;we had ahead in time, from shotmin! Consequently, let us do it general, using a procedure! Nicely do it!

end











pro problem_geometry_selection, imaging_geometry_selector, camera_obscura_xz_fun,camera_obscura_yz_fun,radiograph_xz_fun,radiograph_yz_fun
;190419 This pro selects which problem geometry we have! It ended up not being particularly used somewhere, but it is kind of emblematic, so here it is!


; attention circa code line 1863
; SOS 150619 a very important procedure, one that selects problem geomentry, out of currenty four cases. These are assigned
;to a single variable called imaging_geometry_selector, whose values from one to four each define a different setup. 

;150619 SOS imaging_geometry_selector should go into the param file, along with suitable comments, c-pasted from the comments of this procedure!

;150619 we must also initialize the four params (fifth one os read from the file) in the main body of code!
;150619 first, reset all defining params to zero. Then, geometry selector shall raise, to the value of one, only one among them. The
;rest shall remain to zero. This will be our guide all over the code, helping us select the appropriate format for each code part. 

radiograph_xz_fun=0.0
radiograph_yz_fun=0.0
camera_obscura_xz_fun=0.0
camera_obscura_yz_fun=0.0

;150619 SOS READ imaging_geometry_selector from the param file! SOSARA DO IT!
 
CASE imaging_geometry_selector OF 
;150619 we use integer 1 here, since when checking for this value later on in the code, we mught accidentally 
;employ integer or float or double. So this shall either be used as is, or upgraded auto to float etc, by idl.
;On the other hand, if here was float, then if there integer, then peut-etre no equality, while in reality 
;there is equality! 
;SOS THIS ORDER IS IMPORTANT! KEEP TO IT!
;040719 there is also the switch: focused_beam_switch, 1 is camera obscura, either xz or yz, other (false) is radiogtaph. 
   1: radiograph_xz_fun=1 
   2: radiograph_yz_fun=1
   3: camera_obscura_xz_fun=1
   4: camera_obscura_yz_fun=1 
   ELSE: PRINT, 'Not one through four, *** CANT GO ON LIKE THAT ****!' 
ENDCASE
print, 'imaging_geometry_selector=', imaging_geometry_selector

end








pro imaging_loop_boundaries,sliceXZ_y_location_fun,screenXZ_z_lower_voxel_fun,screenXZ_z_upper_voxel_fun,screenXZ_x_lower_voxel_fun,screenXZ_x_upper_voxel_fun,focused_beam_switch_fun,los_origin_x_fun,los_origin_y_fun,los_origin_z_fun, focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,slice_x_location_fun,screen_y_lower_voxel_fun,screen_y_upper_voxel_fun,screen_z_lower_voxel_fun,screen_z_upper_voxel_fun,nx_1,ny1,nz1, imaging_geometry_selector_fun,first_coord_min_fun,first_coord_max_fun,second_coord_min_fun,second_coord_max_fun,first_coord_fun,second_coord_fun,third_coord_fun,debug_comments_fun
;190819 This pro calcs image boundaries, either on the grid box side planes (radiograph cases, XZ or YZ), or on the fiducial imaging screen (camera obscura mode, either XZ or YZ mode)

;020719 SOS a new procedure for imaging boundaries, called BEFORE imaging 2D loops. Whereas los_origins must be called
;once for every LOS, i.e. within the imaging loops.SOS two different things! One is for imaging boundaries, the 
;other for los origins, nx1current,ny1current, nz1current.SOS HERE ONLY IMAGE BOUNDARIES KEEP, NEXT PRO ONLY THE LOS ORIGINS


;***************************
;;170719 add ze following (XZ ONES only needed!) to the above procedure param list! SOS ADD THEM TO THE procedure CALL AS WELL (without the _fun ending), later in the code!
;first_coord_min_fun=screenXZ_z_lower_voxel_fun
;first_coord_max_fun=screenXZ_z_upper_voxel_fun
;
;second_coord_min_fun=screenXZ_x_lower_voxel_fun
;second_coord_max_fun=screenXZ_x_upper_voxel_fun
;
;;SOS 170619 third coord is just location of imaging plane, be it the fiducial screen (camera obscura) or the box side (radiograph).
;third_coord_fun=sliceXZ_y_location_fun ;screen's slice location along y axis!
;****************************

if (imaging_geometry_selector_fun eq 1) then begin
print, 'radiograph_xz_fun case'

;080719 SOS we must also change the following param, affecting the ratios area
;cam obsc means focused beam. Radiograph means unfocused beam.
focused_beam_switch_fun=0


;160619 SOS REQUIRED PARAM PASSING the first_coord, ny10, nz10 here! DO IT!

;*********************************
;160619 next commented out code snippet is from xz radiograph version (from rlos115_XZ_251218.pro file, lines 1305-1306), to serve as a guide for 
;correctly defining LOS origins for that case, i.e. xz radiograph.
;JET BASE  IMAGING 160818
;for nz10=1,nz1 do begin
;for nx10=1,nx_1 do begin
;*********************************

;first_coord_min_fun,first_coord_max_fun,second_coord_min_fun,second_coord_max_fun
;
;160619 from the above, we assert the y LOS origin coordinate to always take the value of
;unity, since XZ is the imaging plane, and on that plane y is always unity (check if zero or something, though!)

;180619 NO need AT ALL TO MESS WITH THE LOOP VARYING VARS! THEY ARE TAKEN CARE AUTOMATICALLY BY THE LOOP!
;ONLY SET THE THIRD VAR, THE LIMITS OF THE LOOP VARS AND WE ARE GOOD TO GO! VOILA! JUST DO IT!
;160619 ULTRA SOS LOOP HAS SECOND COORDINATE ON THE OUTSIDE SOS!!!!
;020719 image boundaries for current case here!

;030719 ORE to z einai to first coord! to y einai to second i to third (toi z panta to first!)
;030719 we cut some (minus 1) in order to hopefully prevent it from overshjooting coord
;030719b seems to work at minus 2. No need to exceed that.
first_coord_min_fun=1
first_coord_max_fun=nz1-2

;030719 we cut some (minus 1) in order to hopefully prevent it from overshjooting coord
second_coord_min_fun=1
second_coord_max_fun=nx_1-2

;SOS 170619 third coord is just location of imaging plane, be it the fiducial screen (camera obscura) or the box side (radiograph).
third_coord_fun=1 ;meaning y0=1!

;170619 here the trick! only one of the three is fixed here, rest two vary! either in screen (cam_obsc), or in box side (radgraph), as is the case here!

;First_coord_fun=loop_dependent_radio_xz
;econd_coord_fun=loop_dependent_radio_xz

 ;los_origin_x_fun=first_coord_fun
;los_origin_y_fun=third_coord_fun
;los_origin_z_fun=second_coord_fun

;nx1current=los_origin_x_fun;focal_point_x_fun
;;080419 each LOS now begins at the focal point, meant to go back in time!
;;080419 ny10, nz10 screen pixel's SOS! NOT focall poimt's SOS!

;THIS MOVE INTO THE LOOP!
;NO! must stay here, cause it goes cyclical: ny1, nx1 depending on wether it is xz or xz (regardless of cam obsc or radiograph). 
;ny1current_fun=third_coord_fun;focal_point_y_fun
;nz1current=los_origin_z_fun;focal_point_z_fun


;170619 SOS here we define los origins, which in case of radiograph are the same as imaging plane los crossing point. 
;BUT, for camera obscura, LOS origin is th focal point, NOT the screen crossing point! DO ARRANGE IT APPROPRIATELY FOR EACH PAIR OF CASES!

endif



;****************

;SOS 070419: all LOs's now start from focal point, NOT from screen pixel! Screen pixel only defines ratiof's now! SOS nx10, ny109, nz10 MUST
; BE ADJUSTED to focal point's ones, NOT to screen pixel;s onesyz and x zero NO!. DO IT NOW! 

;070419 nx10 NOT OK start from zero! nx10 is the screen slice's x coordinate, NOT zero any more! 

;220619 now starting point is defined within los origins function  NO! nx10 un cam obsc is NOT sttarting pointm it is screen aimed crossing point! sos! THIS HAS
;NOT BEEN TAKEN INTO ACCOUNT WITHIN LOSORIGINS SOS DO TAKE CARE OF IT!
;nx10=slice_x_location

;sos080419 nx1current is now initialized to the focal point's x, as opposed to the screen pixel's x.  
;220619 fpoint coord must be revised for cam obsc xz! SOS it must be set on another wall of the box!
;;250619 select cam obsc yz or xz, i.e. cam obsc, meaning all los begin from the focal point. 
;as opposed to radiograph case, where each los begins from an image pixel




if (imaging_geometry_selector_fun eq 2) then begin
print, 'radiograph_yz_fun case'

;080719 SOS we must also change the following param, affecting the ratios area
;cam obsc means focused beam. Radiograph means unfocused beam.
focused_beam_switch_fun=0

;160619 SOS REQUIRED PARAM PASSING the nx10, ny10, nz10 here! DO IT!

;*********************************
;160619 next commented out code snippet is from yz radiograph version (from rlos115_YZ_251218.pro file, lines 1306-1307), to serve as a guide for 
;correctly defining LOS origins for that case, i.e. yz radiograph.
;for nz10=1,nz1 do begin
;for ny10=1,ny1 do begin
;*********************************
;160619 


;030719 ORE to z einai to first coord! to y einai to second i to third (toi z panta to first!)
first_coord_min_fun=1
first_coord_max_fun=nz1-2

;030719 it seems to fail at the last one, so we reduce image size by 1, incrementally! (we lose some pic, but whatever)
;030719b seems to work at minus 2. No need to exceed that. 
second_coord_min_fun=1
second_coord_max_fun=ny1-2

;SOS 170619 third coord is just location of imaging plane, be it the fiducial screen (camera obscura) or the box side (radiograph).
;160719 corrected ze comment to say x0=1, inst. of y0.
third_coord_fun=1 ;meaning x0=1!

;170619 here the trick! only one of the three is fixed here, rest two vary! either in screen (cam_obsc), or in box side (radgraph), as is the case here!

endif


if (imaging_geometry_selector_fun eq 3) then begin
print, 'camera_obscura_xz_fun case'

;080719 SOS we must also change the following param, affecting the ratios area
;cam obsc means focused beam. Radiograph means unfocused beam.
focused_beam_switch_fun=1.0

;160619 SOS REQUIRED PARAM PASSING the nx10, ny10, nz10 here! DO IT!

;*********************************
;160619 next commented out code snippet is ASSERTED from xz camera_obscura version (NON_EXISTENT VERSION AS YET!), to serve as a guide for 
;correctly defining LOS origins for that case, i.e. xz cam obsc.
;for nz10_fun=screen_z_lower_voxel_fun,screen_z_upper_voxel_fun do begin
;for nx10_fun=screen_x_lower_voxel_fun,screen_x_upper_voxel_fun do begin
;
;*********************************

;first_coord_min_fun,first_coord_max_fun,second_coord_min_fun,second_coord_max_fun
;
;160619 from the above, we assert the y LOS origin coordinate to always take the value of
;unity, since XZ is the imaging plane, and on that plane y is always unity (check if zero or something, though!)

;160619 ULTRA SOS LOOP HAS SECOND COORDINATE ON THE OUTSIDE SOS!!!!


;290619 SOS SEE EIKONAMARGINS PROCEDURES BELOW SOS! 
;we need a different focal point for the XZ version of camera obscura! AND a different screen setup! SO we have to distinguish between two different pairs of FPoint and corresponding screen, one 
;for XZ and one for YZ versions of camera obscura. SOS!


;260619 was wrong assignement, reversed their order now! first coord is z, second is x, third is y here!
;170719 Also: New notation for cam obsc XZ: add XZ to all screen and FPoint_XZ related components
first_coord_min_fun=screenXZ_z_lower_voxel_fun
first_coord_max_fun=screenXZ_z_upper_voxel_fun

second_coord_min_fun=screenXZ_x_lower_voxel_fun
second_coord_max_fun=screenXZ_x_upper_voxel_fun

;SOS 170619 third coord is just location of imaging plane, be it the fiducial screen (camera obscura) or the box side (radiograph).
third_coord_fun=sliceXZ_y_location_fun ;screen's slice location along y axis!

;290619 SOS SEE EIKONAMARGINS PROCEDURES BELOW SOS! 
;we need a different focal point for the XZ version of camera obscura! AND a different screen setup! SO we have to distinguish between two different pairs of FPoint and corresponding screen, one 
;for XZ and one for YZ versions of camera obscura. SOS!
endif



if (imaging_geometry_selector_fun eq 4) then begin
print, 'camera_obscura_yz_fun case'

;080719 SOS we must also change the following param, affecting the ratios area
;cam obsc means focused beam. Radiograph means unfocused beam.
focused_beam_switch_fun=1.0

;160619 SOS REQUIRED PARAM PASSING the nx10, ny10, nz10 here! DO IT!

;*********************************
;160619 next commented out code snippet is ASSERTED from yz camera_obscura version (this file, lines circa 1120), to serve as a guide for 
;correctly defining LOS origins for that case, i.e. yz cam obsc.
;for nz10_fun=screen_z_lower_voxel_fun,screen_z_upper_voxel_fun do begin
;for ny10_fun=screen_y_lower_voxel_fun,screen_y_upper_voxel_fun do begin
;
;*********************************

;first_coord_min_fun,first_coord_max_fun,second_coord_min_fun,second_coord_max_fun
;
;160619 from the above, we assert the y LOS origin coordinate to always take the value of
;unity, since XZ is the imaging plane, and on that plane y is always unity (check if zero or something, though!)


;160619 ULTRA SOS LOOP HAS SECOND COORDINATE ON THE OUTSIDE SOS!!!!

;260619 WRONG ASSIGNEMENT OF COORDS!!! first is z in this case, second is y, third is x allright!
first_coord_min_fun=screen_z_lower_voxel_fun
first_coord_max_fun=screen_z_upper_voxel_fun

second_coord_min_fun=screen_y_lower_voxel_fun
second_coord_max_fun=screen_y_upper_voxel_fun

;SOS 170619 third coord is just location of imaging plane, be it the fiducial screen (camera obscura) or the box side (radiograph).
third_coord_fun=slice_x_location_fun ;screen's slice location along x axis!

endif

if (debug_comments_fun eq 1) then print, 'inside imaging loop boundaries'
if (debug_comments_fun eq 1) then stop


end







pro los_origins,focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun,nz10_fun,los_origin_x_fun,los_origin_y_fun,los_origin_z_fun, focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,slice_x_location_fun,screen_y_lower_voxel_fun,screen_y_upper_voxel_fun,screen_z_lower_voxel_fun,screen_z_upper_voxel_fun,nx_1,ny1,nz1, imaging_geometry_selector_fun,first_coord_min_fun,first_coord_max_fun,second_coord_min_fun,second_coord_max_fun,first_coord_fun,second_coord_fun,third_coord_fun,sliceXZ_y_location_fun
;190819 This pro calcs los origins for each of the four cases, and it is tailor-made to be called from within the 2D imaging loop, before calling the los-loop pro.


;020719 this procedure calcs the los origins , to be called from within the imaging loops! SEPARATE from the above image boundaries procedure!.


;170719 add the following (XZ ones only needed) to the param list of the current procedure! Also add them to the procedure call later in the code!
;los_origin_x_fun=focal_pointXZ_x_fun
;los_origin_y_fun=focal_pointXZ_y_fun
;los_origin_z_fun=focal_pointXZ_z_fun

;160619 SO SDO RELATE AS A NEXT STEP, current procedure output to llos parameterizerd calcxulation SOS DO IT NEXT!!!
;160619 use the following as a guide to coordinates!
;lloscurrent_local=sqrt( ((dlc_fun*(nz1current_fun-los_origin_z_fun))^2)+((dlu_fun*(ny1current_fun-los_origin_y_fun))^2)+((dlr_fun*(nx1current_fun-los_origin_x_fun))^2) )

;160619 reset those coordinate origins here AND early on in the main body of code!!!
los_origin_x_fun=0.0D
los_origin_y_fun=0.0D
los_origin_z_fun=0.0D

;********************************
;170619 this bit (from cam obsc YZ, this file) acts as a guide for los coords generalized
;170619 LOS loop initialization of los origins
;nx1current=los_origin_x_fun;focal_point_x_fun
;;080419 each LOS now begins at the focal point, meant to go back in time!
;;080419 ny10, nz10 screen pixel's SOS! NOT focall poimt's SOS!
;ny1current=los_origin_y_fun;focal_point_y_fun
;nz1current=los_origin_z_fun;focal_point_z_fun
;**************************************


;150619 SOS here now define LOS origins, based on the value of geometry selector set in the procedure above 
;160619 do it at long last!
;****************************************************
;180619 copied from cam obsc loop
;
;for nz10_fun=first_coord_min_fun,first_coord_max_fun do begin
;for ny10_fun=second_coord_min_fun,second_coord_max_fun do begin

;180619 replace with these, all over the loop, no more nx1o etc, just first, second, third coord !
;for first_coord_fun=first_coord_min_fun,first_coord_max_fun do begin
;for second_coord_fun=second_coord_min_fun,second_coord_max_fun do begin

;here just read third_coord_fun 

 
;****************************************************


if (imaging_geometry_selector_fun eq 1) then begin
print, 'radiograph_xz_fun case'


;careful with which coord is which!
los_origin_x_fun=second_coord_fun
los_origin_y_fun=1
los_origin_z_fun=nz10_fun


endif

if (imaging_geometry_selector_fun eq 2) then begin
print, 'radiograph_yz_fun case'

los_origin_x_fun=1
los_origin_y_fun=second_coord_fun
;030719 we added nz10_fun at the beginning of the param list of this pro!
los_origin_z_fun=nz10_fun

endif


if (imaging_geometry_selector_fun eq 3) then begin
print, 'camera_obscura_xz_fun case'


;;260619 for cases 3 and four, los origins are preset to the FPoint. 211019 not anymoar! can be neg out of grid fpoint now! 
;We put it before, to avoid messing with setting it to one later in the zerocase!
;
; For 1 and 2, they are the imaging loop current values.
;lloscurrent=sqrt( ((dlc_fun*(nz1current_fun-focal_point_z_fun))^2)+((dlu_fun*(ny1current_fun-focal_point_y_fun))^2)+((dlr_fun*(nx1current_fun-focal_point_x_fun))^2) )

;020719 NEED A DIFFERENT FPOINT FOR CAM OBSCURA XZ THAN CAM OBSC YZ. DO THAT XZ FPOINT AND IMPLEMENT IT HERE!
;020719 WE CALL IT FOCALPOINTXZ, other than focal point!
;170719 add the following (XZ ones only needed) to the param list of the current procedure! Also add them to the procedure call later in the code!

los_origin_x_fun=focal_pointXZ_x_fun
los_origin_y_fun=focal_pointXZ_y_fun
;070819 allows moar los to start (make it beyond point 1, for neg ratio1f!)
if (focal_pointXZ_y_fun eq 0.0) then focal_pointXZ_y_fun=1.0
los_origin_z_fun=focal_pointXZ_z_fun


;191019 now attempting to allow for negative FPoints!
;211019 we were wrong the other day! los originx if fpoint is neg is not zero! same triangles. One calc for y, one for z! 
;We put neg coord of fpoint stuff AFTER, to avoid messing settingsif pos or neg fpoint coord!CRUCIAL use LT not le! zero case later on!
if (focal_pointXZ_y_fun lt 0.0) then begin 
;221019 now we use 1.0, since los is definately entering the grid somewhere at the slice one step after the boundary!
los_origin_y_fun=1.0
;*******************************************************************
;221019 these two for x coord.
if (focal_pointXZ_x_fun gt second_coord_fun) then begin 
los_origin_x_fun=fix(   second_coord_fun+ ((sliceXZ_y_location_fun)/(sliceXZ_y_location_fun+abs(focal_pointXZ_y_fun)))*(focal_pointXZ_x_fun-second_coord_fun)    )
endif

if (focal_pointXZ_x_fun le second_coord_fun) then begin 
los_origin_x_fun=fix(   focal_pointXZ_x_fun+ ((abs(focal_pointXZ_y_fun))/(sliceXZ_y_location_fun+abs(focal_pointXZ_y_fun)))*(second_coord_fun-focal_pointXZ_x_fun)   )
endif
;*******************************************************************
;next twofor z coord!221019
if (focal_pointXZ_z_fun gt nz10_fun) then begin 
los_origin_z_fun=fix(   nz10_fun+ ((sliceXZ_y_location_fun)/(sliceXZ_y_location_fun+abs(focal_pointXZ_y_fun)))*(focal_pointXZ_z_fun-nz10_fun)  )
endif

if (focal_pointXZ_z_fun le nz10_fun) then begin 
los_origin_z_fun=fix(  focal_pointXZ_z_fun+ ((abs(focal_pointXZ_y_fun))/(sliceXZ_y_location_fun+abs(focal_pointXZ_y_fun)))*(nz10_fun-focal_pointXZ_z_fun)  )
endif
;**********************************************************************************************************************

;221019 now REPEAT THAT FOR imaging case 4 as well! also comment out previous version of those below!



;*******************************************************************


;;211019 we must ADD the difference to fpointx if second coord is larger and subtract it if it is smaller THINK it! SOS ! incomplete as yet, but almost there!!! geometry and calc! abs(A)/A gives sign, then multiply by self, something like that do!
;los_origin_x_fun=fix( ( (abs(focal_pointXZ_y_fun))/(abs(focal_pointXZ_y_fun)+sliceXZ_y_location_fun)   )*(focal_pointXZ_x_fun+(second_coord_fun-focal_pointXZ_x_fun))   )
;los_origin_z_fun=fix(   ( (abs(focal_pointXZ_y_fun))/(abs(focal_pointXZ_y_fun)+sliceXZ_y_location_fun)   )*(nz10_fun-focal_pointXZ_z_fun)     )
;endif
endif
endif

if (imaging_geometry_selector_fun eq 4) then begin
print, 'camera_obscura_yz_fun case'


;*********************************
;160619 next commented out code snippet is ASSERTED from yz camera_obscura version (this file, lines circa 1120), to serve as a guide for 
;correctly defining LOS origins for that case, i.e. yz cam obsc.
;for nz10_fun=screen_z_lower_voxel_fun,screen_z_upper_voxel_fun do begin
;for ny10_fun=screen_y_lower_voxel_fun,screen_y_upper_voxel_fun do begin
;
;*********************************





;;260619 for cases 3 and four, los origins are preset to the FPoint. 211019 not anymoar! can be neg out of grid fpoint now! 
;
;For 1 and 2, they are the imaging loop current values.
;lloscurrent=sqrt( ((dlc_fun*(nz1current_fun-focal_point_z_fun))^2)+((dlu_fun*(ny1current_fun-focal_point_y_fun))^2)+((dlr_fun*(nx1current_fun-focal_point_x_fun))^2) )

los_origin_x_fun=focal_point_x_fun

los_origin_y_fun=focal_point_y_fun
los_origin_z_fun=focal_point_z_fun

;2211019 SOS after, not before the neg coord if then calc! else it gets overwritten!
;191019 now attempting to allow for negative FPoints!
;211019 we were wrong the other day! los originx if fpoint is neg is not zero! same triangles. One calc for y, one for z! 
if (focal_point_x_fun le 0.0) then begin
los_origin_x_fun=0.0


;*******************************************************************
;221019 these two for x coord.SOS CONVERT THOSE TO IMAGING CASE 4!
if (focal_point_y_fun gt second_coord_fun) then begin 
los_origin_y_fun=fix(   second_coord_fun+ ((slice_x_location_fun)/(slice_x_location_fun+abs(focal_point_x_fun)))*(focal_point_y_fun-second_coord_fun)   )
endif

if (focal_point_y_fun le second_coord_fun) then begin 
los_origin_y_fun=fix(  focal_point_y_fun+ ((abs(focal_point_x_fun))/(slice_x_location_fun+abs(focal_point_x_fun)))*(second_coord_fun-focal_point_y_fun)   )
endif
;*******************************************************************
;next twofor z coord!221019
if (focal_point_z_fun gt nz10_fun) then begin 
los_origin_z_fun=fix(nz10_fun+ ((slice_x_location_fun)/(slice_x_location_fun+abs(focal_point_x_fun)))*(focal_point_z_fun-nz10_fun)   )
endif

if (focal_point_z_fun le nz10_fun) then begin 
los_origin_z_fun=fix(   focal_point_z_fun+ ((abs(focal_point_x_fun))/(slice_x_location_fun+abs(focal_point_x_fun)))*(nz10_fun-focal_point_z_fun))  
endif
;**********************************************************************************************************************
;
;
;
;
;;231019 comment out old attempt, whichh was only good for on-axis FPoint!
;los_origin_y_fun=fix(     ( (abs(focal_point_x_fun))/(abs(focal_point_x_fun)+slice_x_location_fun)   )*(second_coord_fun-focal_point_y_fun)      )
;los_origin_z_fun=fix(     ( (abs(focal_point_x_fun))/(abs(focal_point_x_fun)+slice_x_location_fun)   )*(nz10_fun-focal_point_z_fun)              )




endif



endif


end













function fast_proton_profile_function, u_fast_proton_lower_threshold, v_4d, z_fast_proton_threshold_index, returnski_4d
;280722 this function presents a multiplication factor for the fraction of slow protons that
;become fast protons. This fraction is an extewrnal parameter of the model, adjusted by the user 
;in the external param file of rlos.  
;This function represents the relation: 
;f(u)=1, abs(velocity)>= abs(threshold_velocity)) (f(u)=1, abs(u/u0)>=1 ). 
;f(u)=(abs(u/u0))^z, abs(u/u0)<1.0.
;if z=1, then it goes linear from zero to threshold, then stays put at unity. 
;If z=2, then it rises slowly from zero and only reaches unity when approaching threshold.
;If z=0.5, then it quickly moves towards unity, even from when u is less than half that of threshold. 
;THe philosophy behind this function is thatthe production of a meaningful number of fast protons requires local 
;jet matter to be a. that, i.e. jet matter (not so much ambient matter), and b. to be moving at a given velocity, regardless
;of direction. Thus, we may consider a threshold, above which the preset percentage of fast to slow protons applies, while for lower 
;local velocities, wee get less fast protons than the user preset. 
;Thus, we avoid synchrotron emission artefacts from slow dense matter that are not part of the jet activity, but may be dormant at thw side of the grid.
;
;array-op if then else a111_aplo=(a111_aplo gt 1.0)*1.0+(a111_aplo le 1.0)*a111_special
;
a111_aplo=( abs(v_4d/u_fast_proton_lower_threshold)  )
a111_special=( abs(v_4d/u_fast_proton_lower_threshold)  )^z_fast_proton_threshold_index

;280722 SUPERSOS need to refit this function in order to work with array-op! SOS!
;if (a111_aplo ge 1.0) then (returnski_4d=1.0)
;if (a111_aplo lt 1.0) and (a111_aplo gt 0.0) then (returnski=a111_special)
;280722 sos array-oriented if then else replacement
;a111_aplo=(a111_aplo gt 1.0)*1.0+(a111_aplo le 1.0)*a111_special
returnski_4d=(a111_aplo ge 1.0)*1.0+(a111_aplo lt 1.0)*a111_special
;
print, size(a111_aplo)
;stop

return, returnski_4d

end


function lloscurrent_function, debug_comments_fun, dlr_fun,dlu_fun,dlc_fun,nx1current_fun,ny1current_fun,nz1current_fun,los_origin_x_fun,los_origin_y_fun,los_origin_z_fun
;190819 This pro calcs current los length along a los.

;290619 this lloscurrent  function is aimed to operate for all cases of system geometry, as it simply employs start and end of current los position in the grid.

;sos 140619 here add a function to calc llos, one for radiograph, another for camera obscure, such as the case here!

;perhaps define a point of origin coordinate trio, either for radiograph, or for cam_obscura, then do the rest the same, only the coords differ!

;cam_obscura llos:lloscurrent_local=sqrt( ((dlc_fun*(nz1current_fun-focal_point_z_fun))^2)+((dlu_fun*(ny1current_fun-focal_point_y_fun))^2)+((dlr_fun*(nx1current_fun-focal_point_x_fun))^2) )
lloscurrent_local=sqrt( ((dlc_fun*(nz1current_fun-los_origin_z_fun))^2)+((dlu_fun*(ny1current_fun-los_origin_y_fun))^2)+((dlc_fun*(nx1current_fun-los_origin_x_fun))^2) )

lloscurrent_local=double(lloscurrent_local)
if (debug_comments_fun eq 1) then  print, 'lloscurrent_local,nx1current_fun,ny1current_fun,nz1current_fun,dlr_fun,dlu_fun,dlc_fun,los_origin_x_fun,los_origin_y_fun,los_origin_z_fun',lloscurrent_local,nx1current_fun,ny1current_fun,nz1current_fun,dlr_fun,dlu_fun,dlc_fun,los_origin_x_fun,los_origin_y_fun,los_origin_z_fun
if (debug_comments_fun eq 1) then  print,'nz1current_fun-los_origin_z_fun,ny1current_fun-los_origin_y_fun,nx1current_fun-los_origin_x_fun',nz1current_fun-los_origin_z_fun,ny1current_fun-los_origin_y_fun,nx1current_fun-los_origin_x_fun
return, lloscurrent_local
end



pro eikona_margins_camera_obscura_YZ, z_lower_limit,z_upper_limit,x_lower_limit,x_upper_limit,y_lower_limit,y_upper_limit,screen_z_lower_voxel_fun,screen_z_upper_voxel_fun,screen_y_lower_voxel_fun,screen_y_upper_voxel_fun
;190819 This eikona_margins pro, and the rest of its quartet, are meant to calc image margins. They are superseded by the imaging_loop_boundaries pro, so these here are not used. Still, they are kept for possible future use.
;*******************************************************************************************************************
;130619 sos put following code part in a procedure to be called upon beginning the loops! 

;
;130619 this only works for YZ perspective, only available for camera obscura version.
;Radiograph version also has XZ version, i.e. jet-head-on.
z_lower_limit = screen_z_lower_voxel_fun
z_upper_limit = screen_z_upper_voxel_fun

y_lower_limit = screen_y_lower_voxel_fun
y_upper_limit = screen_y_upper_voxel_fun

end




pro eikona_margins_camera_obscura_XZ, z_lower_limit,z_upper_limit,x_lower_limit,x_upper_limit,y_lower_limit,y_upper_limit,screen_z_lower_voxel_fun,screen_z_upper_voxel_fun,screen_y_lower_voxel_fun,screen_y_upper_voxel_fun
;190819 This eikona_margins pro, and the rest of its quartet, are meant to calc image margins. They are superseded by the imaging_loop_boundaries pro, so these here are not used. Still, they are kept for possible future use.


;sos 150619 these x limits do not yet exist, as a XZ version of rlos is not yet available for 
; camera obscure setup, but it shall become!
;190819 XZ cam obsc now works like a charm, yet this pro is not called any more!

;170719 probably these four pros are not used after all, superseded by the ones above! let's see!
z_lower_limit = screen_z_lower_voxel_fun
z_upper_limit = screen_z_upper_voxel_fun

x_lower_limit = screen_x_lower_voxel_fun
x_upper_limit = screen_x_upper_voxel_fun


end


;**************************************************

pro eikona_margins_radiograph_YZ, z_lower_limit,z_upper_limit,x_lower_limit,x_upper_limit,y_lower_limit,y_upper_limit,screen_z_lower_voxel_fun,screen_z_upper_voxel_fun,screen_y_lower_voxel_fun,screen_y_upper_voxel_fun
;190819 This eikona_margins pro, and the rest of its quartet, are meant to calc image margins. They are superseded by the imaging_loop_boundaries pro, so these here are not used. Still, they are kept for possible future use.


;130619 this only works for YZ perspective,  available for camera obscura nad for radiograph versions.
;Radiograph version also has XZ version, i.e. jet-head-on.
;130619 SOS VERIFY THESE VALUES OF BOUNDARIES FROM RADIOGRAPH OLDER VERSION OF CODE !SOS DO IT!
;simply copoy paste image loop boundaries uppers lowers! 
z_lower_limit = 1
z_upper_limit = nz1

y_lower_limit = 1
y_upper_limit = ny1
 

end


;150619 code snippet from XZ rlos115 dec18
;for nz10=1,nz1 do begin
;for nx10=1,nx_1 do begin
;
;;print, 'ok os edo 1'
;nx1current=nx10
;ny1current=ny10
;nz1current=nz10

pro eikona_margins_radiograph_XZ, z_lower_limit,z_upper_limit,x_lower_limit,x_upper_limit,y_lower_limit,y_upper_limit,screen_z_lower_voxel_fun,screen_z_upper_voxel_fun,screen_y_lower_voxel_fun,screen_y_upper_voxel_fun
;190819 This eikona_margins pro, and the rest of its quartet, are meant to calc image margins. They are superseded by the imaging_loop_boundaries pro, so these here are not used. Still, they are kept for possible future use.


;130619 this only works for XZ perspective, only available for radiographa version.
;Radiograph version also has XZ version, i.e. jet-head-on.
;130619 SOS VERIFY THESE VALUES OF BOUNDARIES FROM RADIOGRAPH OLDER VERSION OF CODE !SOS DO IT!
;simply copoy paste image loop boundaries uppers lowers! 
z_lower_limit = 1
z_upper_limit = nz1

x_lower_limit = 1
x_upper_limit = nx_1
      
END


;OS EDO COMMENTED 190819

pro pload_float,shot_number_fun, sfactor_fun,datapath_fun
;200819 This pro does the job, not tried herre though, but it is replicated in the main body of code!
;BLUEPRINT FINALLY EMPLOYED: 100519
;******************************************************************
if (pload_float_factor eq 1.0) then begin
pload, 21,shrink =sfactor, dir=datapath,/float
endif else begin
pload, 21,shrink =sfactor, dir=datapath
endelse
;******************************************************************

;pload, shot_number_fun, shrink=sfactor_fun,dir=datapath_fun,/float
if (pload_float_factor eq 1.0) then begin
pload, shot_number_fun, shrink=sfactor_fun,dir=datapath,/float
endif else begin
pload,shotmin+shotind, shrink=sfactor, dir=datapath
endelse


return
end




;170719 these relativistic imaging tests are meant to work with camera obscura YZ version only!   
function planar_beam_rod_test, rho_4d_fun,clight_fun,nx_1_fun,ny1_fun,nz1_fun,focal_point_y_fun,focal_point_z_fun,shotmin_fun,shotmax_fun,debug_comments_fun,t_fun
;200819 This OLD ONE (LOOK AT THE FOLLOWING AUGUST 2019 VERSION OF THIS SAME FUNCTION, THIS IS JUST AN OLD VERSION) function 
;200819 constitutes the implemantation of an example test of this program, namely to visualize relativistic effect on an approaching rod-like object. There are 


;170719 these relativistic imaging tests are meant to work with camera obscura YZ version only!   
;200819 Those tests are activated by the relevant switch in the rlos param external file.


;310719 idea: make 3 PLANAR beams: 1 approaching, one receding, and one stationary!!. Then, visually compare the lengths of the three, et voila!

;310719 make them thin and long along x axis!!!. Not as they are now!!! (remnants from previous test?) Consider parametric object definition in 3D, makes life MUCH easier!!!

;310719SOS must also account for length contraction, but we can do that when calcing!! 

;200519 got to mannually account for length contraction beforehand any relat imaging stuff!. Alt, make it the sum of point cell sources.

;also make plate narrow and long, long along motion direction.

;also make it massif, not cadre!

;sos 160519 make it slower in order to appear, cos it is lower than fpoint and 
;delay makes rays from fp backwards in time totally MISS it! at 0.89c sos! work out details! trial error!

;150519 Now work out the details of the plane aproaching side-on, like a frisbee or something, and appearing to be shorter along the LOS. Length contraction example test!
;
;030519 idea: make the loop work back in time as well! More natural way of doing it!

;beam constant density
beam_density=1000000000.0D
;010519 shotmax_margin is decreased from shotmax, in order to allow for our setup to work for high clight!. i.e. 
;beam can be very fast yet enough time span exists for rays , traveling at clight, to cross the domain without running out of 
;snapshots to access! beam and rays, two different stories but taking place on the same temporal range!
;margin_fraction is ADDED TO SHOTMIN deducted from shotmax, a fraction of the time span, the multiplicatrion result is integered and then deducted from shotmax
;margin_fraction=0.68
;030519 make margin large enough so that it stops going back in time BEFORE it reaches spatially the end of the domain SOS
;SOS this para shortens duration, when domain is smallish for the rays to cover time span. Either cut the duration here, 
;or else bring in a larger domain.
;080519 BREAK introduced below may render this frsaction a bit less nedded perhaps!
margin_fraction=0.00
shotmin_margin=fix((shotmax_fun-shotmin_fun)*margin_fraction)

if (debug_comments_fun eq 1) then  print,'margin_fraction,shotmin_fun,shotmax_fun,shotmin_margin,shotmax_fun-shotmin_margin',margin_fraction,shotmin_fun,shotmax_fun,shotmin_margin,shotmax_fun-shotmin_margin

;010519 set dens zero everywhere,  AND AT ANYTIME, prepare density tabula rasa for the beam definition to follow!
(rho_4d_fun[*,*,*,*]=0.0)
;010519 here we define the beam velocity along the x-axis, best neg for naw, in order to approach the Fpoint from the right
u_beam_x=-0.8905*clight_fun
;010519 (x_base, y_base, z_base) are the 3D spatial coordinates of the central point of the beam. Since the beam is taken to move along the negative 
;x direction for now, approaching the focal point, we got: (CAREFUL do not exceed domain size x_base+xsize!)
;ALSO CAREFUL to set x_base at first beyond the screen!
;030519 now we reverse the dens definition, going now backwards timewise. So that shotmax_fun is when it begins rolling back in time!

;180519 we now introduce NEG final fracxtion, so it starts to the left of zero, allowing for very fast incoming stuff to be captured!


;130819 comment this fraction out!
;x_final_fraction=0.11; (SOS: conceptually should be MORE than screen, and less than domain-x-half-length!MAY AUTOMATE LATER ON! 

;
;
;010519 SOS also consider margin of six in nx_1 vs nx1! MAX=0.88 for 60-100-60 resolution!) 
;xbase must be set within time loop below!

;x_base=fix(nx1*x_init_fraction)+u_beam_x*t(shotind)


;150519 here ze differences with perp plane beam! Also on x final fraction above! sos! now z is small and x is long! 
;Careful alter xmin above (make it bigger)
x_fractional_half_size=0.05;suggested:0.05
y_fractional_half_size=0.3;suggested:0.3
z_fractional_half_size=0.3;suggested:0.04

;010519 rest of coords set to FPoints' ones, so that beam's centre is moving towards FP, along x axis

;150519 also alter base coordinates, since we need to look upon it from kinda above!
;so make z base LESS than focal points' one!
y_base=focal_point_y_fun
z_base=fix(focal_point_z_fun*1.0D)

;010519 ULTRA SOS we must avoid beam size exceeding domain limits, therefore: define half counts at less than half of 
;x_length_count=x-dimension-half-size or beam, IN CELLS, calced as a percentage of x-grid size, i.e. independent on resolution!
x_length_half_count=fix(nx_1_fun*x_fractional_half_size)
;y_length_count=y-dimension-half-size or beam, IN CELLS, calced as a percentage of y-grid size, i.e. dependent on resolution!
y_length_half_count=fix(ny1_fun*y_fractional_half_size)
;z_length_count=z-dimension-half-size or beam, IN CELLS, calced as a percentage of z-grid size, i.e. dependent on resolution!
z_length_half_count=fix(nz1_fun*z_fractional_half_size)

if (debug_comments_fun eq 1) then  print,'PERP. ROD DIMS IN CELLS: x_length_half_count,y_length_half_count,z_length_half_count',x_length_half_count,y_length_half_count,z_length_half_count

;020519 SOS must adjust tpicked to begin near shotmax, but not above. Then, adjust here to END at shotmax, but bEGIN at shotmin plus some margin!
;Not as it is now, cos we shoot and miss to get the beam to exit right on the interval end!
;so we change it now! 020519!
;BEAM NOW APPEARS MAGICALLY IN 4D DATA at time (shotmin_fun+shotmax_margin)!
for time_index=(shotmax_fun),(shotmin_fun+shotmin_margin),-1 do begin
;030519 SOS now 
x_base=fix(nx_1_fun*x_final_fraction)-fix(u_beam_x*t_fun(shotmax_fun-time_index))
;230519 add another xbase to have a dash line of sorts SEPARATION is the GAP between rods
separation=2.0D
x_base2=x_base+fix(separation*x_length_half_count)
;080519 SOS we add this break statement, to allow beam to EXIT grid if very quck, without error. ELSE, we get en error 
;later on on the  program ,when not enough time available! SUPER SOS HERE TIMING STUFF!!
if ((x_base+x_length_half_count) ge nx_1_fun) then break
;230519 add another criterion, about xbase2, makes things longer and wastes moar x space though!
if ((x_base2+x_length_half_count) ge nx_1_fun) then break

;020519 SOS do not allow beam to exit 150519 left!
if (x_base lt x_length_half_count) then (x_base = fix(1.1D*x_length_half_count))

if (debug_comments_fun eq 1) then  print,'time_index aka local shotind',time_index 
if (debug_comments_fun eq 1) then  print,'x_base,x_base-x_length_half_count,x_base+x_length_half_count',x_base,x_base-x_length_half_count,x_base+x_length_half_count 
if (debug_comments_fun eq 1) then  print,'y_base,y_base-y_length_half_count,y_base+y_length_half_count',y_base,y_base-y_length_half_count,y_base+y_length_half_count
if (debug_comments_fun eq 1) then  print,'z_base,z_base-z_length_half_count,z_base+z_length_half_count',z_base,z_base-z_length_half_count,z_base+z_length_half_count

;030519 edited this to begin backwards in time from shotmax, at xinit near x=0

;180519 SOS in case we begin below zero along x axis, we can still have a go using this arrangement
if (x_base le 0.0) then (rho_4d_fun[time_index,*,*,*]=rho_4d_fun[time_index,*,*,*]*0.0D)

;020519 then again, if it reaches beginning of grid, it must disappear! Else it will constantly affect all rays!
if (x_base le x_length_half_count) then (rho_4d_fun[time_index,*,*,*]=rho_4d_fun[time_index,*,*,*]*0.0D)

;190519 set it non zero when appropriate
if (x_base gt x_length_half_count) then begin

;310719 BEAM 1  crashing on us! (approaching fast)
;**************************************************************************************************************************
rho_4d_fun[time_index,x_base-x_length_half_count:x_base+x_length_half_count,y_base-y_length_half_count:y_base+y_length_half_count,z_base-z_length_half_count:z_base+z_length_half_count]=beam_density
;230519 add second part
rho_4d_fun[time_index,x_base2-x_length_half_count:x_base2+x_length_half_count,y_base-y_length_half_count:y_base+y_length_half_count,z_base-z_length_half_count:z_base+z_length_half_count]=beam_density
;**************************************************************************************************************************

;310719 BEAM 2  receding!
;
;310719 ; time_base_reversal represents crossing time for the y-length of the domain. 
;310719 Helps with an easy definition of the receding beam, based on the approaching one.
time_base_reversal=(double(ny1_fun)/u_beam_x)
;**************************************************************************************************************************
;temporary comment out both of these here, in order to try out the next one, the 'procedured' (though non-parameterized) rod!
;rho_4d_fun[time_index,x_base-x_length_half_count:x_base+x_length_half_count,y_base-y_length_half_count:y_base+y_length_half_count,z_base-z_length_half_count:z_base+z_length_half_count]=beam_density
;230519 add second part
;rho_4d_fun[time_index,x_base2-x_length_half_count:x_base2+x_length_half_count,y_base-y_length_half_count:y_base+y_length_half_count,z_base-z_length_half_count:z_base+z_length_half_count]=beam_density
;**************************************************************************************************************************

;310719 here we MUST adopt parametric definition of the rods, relative to a moving centre of the object. This is supposed to be the way to go with those things, not as NOW, which IS too complex!
;pro call here!
;****************************************************************************************************************************************************
;310719 here lies the art, in defining centre_x location in relation to current time in the loop, since we are moving along x in this mother function.
;310719 we use the absolute value of the given beam velocity
centre_x=fix(     (shotmax_fun-time_index+1)*double(abs(u_beam_x))      )


print,'centre_x,shotmax_fun,time_index,u_beam_x,double(abs(u_beam_x))  ', centre_x,shotmax_fun,time_index,u_beam_x,double(abs(u_beam_x))  

if (centre_x ge (nx_1_fun-7)) then centre_x= (nx_1_fun-7)

if (centre_x le 3) then centre_x= 3

print,'centre_x,shotmax_fun,time_index,u_beam_x,double(abs(u_beam_x))  ', centre_x,shotmax_fun,time_index,u_beam_x,double(abs(u_beam_x))  

if (debug_comments_fun eq 1) then stop
;310719 do y,z centres kinda off-centre, in relation to FPoint, in order to avoid overlaps with the central approaching rod! 
centre_y=fix(0.85*(ny1_fun-6))

centre_z=fix(0.85*(nz1_fun-6))


size_x=fix(0.25*(nx_1_fun-6))

size_y=fix(0.13*(ny1_fun-6))

size_z=fix(0.13*(nz1_fun-6))

;310719 funny, we call it with_fun suffix for nx_1, etc, yet the pro called does not employ those!!!(nx_i _funned present in the call, but not in the definition, while the opposite is usually true!)
x_rod_receding,centre_x,centre_y,centre_z,size_x,size_y,size_z,rho_d_3d,x_velocity,time_index,nx_1_fun,ny1_fun,nz1_fun,rho_4d_fun,beam_density
;****************************************************************************************************************************************************



endif


endfor

return, rho_4d_fun
;these stuff uncomment and use at end, to check the results 
;SOS use the filter-> smooth function after rendering with ivolume, in order to see the beam (actually looks more like a brick)

;for iggy=1,10,1 do begin 
;ivolume, reform(rho_4d[iggy,*,*,*])
;endfor
;
;print, where(reform(rho_4d[1,*,*,*]) ge 1000000000)
;010519 SOS DO SMOOTH AFTER RENDERING ET VOILA!
;ivolume, reform(rho_4d[3,*,*,*])
;010519 SOS!
;print, where(reform(rho_4d[1,*,focal_point_y,focal_point_z])) ge 10.0)
;end
end






;function fast_proton_profile_function, u_fast_proton_lower_threshold, velocity, kappa_zero, z_fast_proton_threshold_index, returnski_4d
;
;a111_aplo=( abs(velocity/u_fast_proton_lower_threshold)  )
;a111_special=( abs(velocity/u_fast_proton_lower_threshold)  )^z_fast_proton_threshold_index
;returnski=1.0
;if (a111_aplo ge 1.0) then (returnski=1.0)
;if (a111_aplo lt 1.0) and (a111_aplo gt 0.0) then (returnski=a111_special)
;
;return, returnski
;end







pro planar_beam_rod_test_version_august_2019, vx1_4d_fun,rho_4d_fun,clight_fun,nx_1_fun,ny1_fun,nz1_fun,focal_point_y_fun,focal_point_z_fun,shotmin_fun,shotmax_fun,debug_comments_fun,t_fun
;200819 This PROCEDURE constitutes the implemantation of an example test of this program, namely to visualize relativistic effect on an approaching rod-like object. There are 
;200819 actually three objects: An approaching rod, a receding one, and a stationary ruler in the middle.
;200819 IT REQUIRES SOME FINE TUNING TO GIVE THE RESULT, UNFINISHED BUT NEARLY THERE! 

;170719 these relativistic imaging tests are meant to work with camera obscura YZ version only!   
;200819 Those tests are activated by the relevant switch in the rlos param external file.
;
;050819 now this one is a pro, cause it also affects  velocity, therefore it has to affect more than one variable!

;120819 SOS we have now removed the old temp construct from this function, as it made our ruler vanish when the old 'rod' exited the grid! 
;120819 We now call xrod procedure to in order to make rods etc. Careful!
;010819 this version of the current function is meant to only employ the new moving object defining procedure. 
;010819 we also keep the old version of this, above, as a backup

;310719 idea: make 3 PLANAR beams: 1 approaching, one receding, and one stationary!!. Then, visually compare the lengths of the three, et voila!

;310719 make them thin and long along x axis!!!. Not as they are now!!! (remnants from previous test?) Consider parametric object definition in 3D, makes life MUCH easier!!!

;310719SOS must also account for length contraction, but we can do that when calcing!! 

;200519 got to mannually account for length contraction beforehand any relat imaging stuff!. Alt, make it the sum of point cell sources.

;also make plate narrow and long, long along motion direction.

;also make it massif, not cadre!

;sos 160519 make it slower in order to appear, cos it is lower than fpoint and 
;delay makes rays from fp backwards in time totally MISS it! at 0.89c sos! work out details! trial error!

;150519 Now work out the details of the plane aproaching side-on, like a frisbee or something, and appearing to be shorter along the LOS. Length contraction example test!
;
;030519 idea: make the loop work back in time as well! More natural way of doing it!

;beam constant density
beam_density=1000000000.0D
;010519 shotmax_margin is decreased from shotmax, in order to allow for our setup to work for high clight!. i.e. 
;beam can be very fast yet enough time span exists for rays , traveling at clight, to cross the domain without running out of 
;snapshots to access! beam and rays, two different stories but taking place on the same temporal range!
;margin_fraction is ADDED TO SHOTMIN deducted from shotmax, a fraction of the time span, the multiplicatrion result is integered and then deducted from shotmax
;margin_fraction=0.68
;030519 make margin large enough so that it stops going back in time BEFORE it reaches spatially the end of the domain SOS
;SOS this para shortens duration, when domain is smallish for the rays to cover time span. Either cut the duration here, 
;or else bring in a larger domain.
;080519 BREAK introduced below may render this frsaction a bit less nedded perhaps!
margin_fraction=0.00
shotmin_margin=fix((shotmax_fun-shotmin_fun)*margin_fraction)

if (debug_comments_fun eq 1) then  print,'margin_fraction,shotmin_fun,shotmax_fun,shotmin_margin,shotmax_fun-shotmin_margin',margin_fraction,shotmin_fun,shotmax_fun,shotmin_margin,shotmax_fun-shotmin_margin

;010519 set dens zero everywhere,  AND AT ANYTIME, prepare density tabula rasa for the beam definition to follow!
(rho_4d_fun[*,*,*,*]=0.0)
;010519 here we define the beam velocity along the x-axis, best neg for naw, in order to approach the Fpoint from the right
u_beam_x=-0.6905*clight_fun
;010519 (x_base, y_base, z_base) are the 3D spatial coordinates of the central point of the beam. Since the beam is taken to move along the negative 
;x direction for now, approaching the focal point, we got: (CAREFUL do not exceed domain size x_base+xsize!)
;ALSO CAREFUL to set x_base at first beyond the screen!
;030519 now we reverse the dens definition, going now backwards timewise. So that shotmax_fun is when it begins rolling back in time!

;180519 we now introduce NEG final fracxtion, so it starts to the left of zero, allowing for very fast incoming stuff to be captured!
x_final_fraction=0.11; (SOS: conceptually should be MORE than screen, and less than domain-x-half-length!MAY AUTOMATE LATER ON! 
;010519 SOS also consider margin of six in nx_1 vs nx1! MAX=0.88 for 60-100-60 resolution!) 
;xbase must be set within time loop below!

;x_base=fix(nx1*x_init_fraction)+u_beam_x*t(shotind)


;150519 here ze differences with perp plane beam! Also on x final fraction above! sos! now z is small and x is long! 
;Careful alter xmin above (make it bigger)

;130819 comment these out as well!
;x_fractional_half_size=0.05;suggested:0.05
;y_fractional_half_size=0.3;suggested:0.3
;z_fractional_half_size=0.3;suggested:0.04

;010519 rest of coords set to FPoints' ones, so that beam's centre is moving towards FP, along x axis

;150519 also alter base coordinates, since we need to look upon it from kinda above!
;so make z base LESS than focal points' one!

;130819 comment these out!
;y_base=focal_point_y_fun
;z_base=fix(focal_point_z_fun*1.0D)


;130819 comment these out!

;010519 ULTRA SOS we must avoid beam size exceeding domain limits, therefore: define half counts at less than half of 
;x_length_count=x-dimension-half-size or beam, IN CELLS, calced as a percentage of x-grid size, i.e. independent on resolution!
;x_length_half_count=fix(nx_1_fun*x_fractional_half_size)
;;y_length_count=y-dimension-half-size or beam, IN CELLS, calced as a percentage of y-grid size, i.e. dependent on resolution!
;y_length_half_count=fix(ny1_fun*y_fractional_half_size)
;;z_length_count=z-dimension-half-size or beam, IN CELLS, calced as a percentage of z-grid size, i.e. dependent on resolution!
;z_length_half_count=fix(nz1_fun*z_fractional_half_size)
;
;if (debug_comments_fun eq 1) then  print,'PERP. ROD DIMS IN CELLS: x_length_half_count,y_length_half_count,z_length_half_count',x_length_half_count,y_length_half_count,z_length_half_count




;020519 SOS must adjust tpicked to begin near shotmax, but not above. Then, adjust here to END at shotmax, but bEGIN at shotmin plus some margin!
;Not as it is now, cos we shoot and miss to get the beam to exit right on the interval end!
;so we change it now! 020519!
;BEAM NOW APPEARS MAGICALLY IN 4D DATA at time (shotmin_fun+shotmax_margin)!

for time_index=(shotmax_fun),(shotmin_fun+shotmin_margin),-1 do begin
;200819 A time loop, affecting and redefining the densities all along the timeline of data snapshots, for the needs of the current test/example.


;120819 we hereby adjust xbase's speed to ultra low, inorder to keep the construct as is, yet we define centres'positions below separately! 

;130819 start by commenting these two lines out! our aim is to finally allow the blobs to cover the whole x-length of the grid, (when we count it by going back in time)! Cos they tend to stop before reaching max x!
;xbase_slowdown_factor=0.001D
;x_base=fix(nx_1_fun*x_final_fraction)-fix(xbase_slowdown_factor*u_beam_x*t_fun(shotmax_fun-time_index))



;230519 add another xbase to have a dash line of sorts SEPARATION is the GAP between rods

;120819 commented these two out!
;separation=2.0D
;x_base2=x_base+fix(separation*x_length_half_count)

;080519 SOS we add this break statement, to allow beam to EXIT grid if very quck, without error. ELSE, we get en error 
;later on on the  program ,when not enough time available! SUPER SOS HERE TIMING STUFF!!
;;120819 nothing moving here , we now set those later! so, no nbreak statements, based on non-existing moving rods!!
;if ((x_base+x_length_half_count) ge nx_1_fun) then break
;230519 add another criterion, about xbase2, makes things longer and wastes moar x space though!
;if ((x_base2+x_length_half_count) ge nx_1_fun) then break

;020519 SOS do not allow beam to exit 150519 left!
;if (x_base lt x_length_half_count) then (x_base = fix(1.1D*x_length_half_count))


;130819 also commenting three comment lines out, to avoid dealing with now commented out (above) xbase!
if (debug_comments_fun eq 1) then  print,'time_index aka local shotind',time_index 
;if (debug_comments_fun eq 1) then  print,'x_base,x_base-x_length_half_count,x_base+x_length_half_count',x_base,x_base-x_length_half_count,x_base+x_length_half_count 
;if (debug_comments_fun eq 1) then  print,'y_base,y_base-y_length_half_count,y_base+y_length_half_count',y_base,y_base-y_length_half_count,y_base+y_length_half_count
;if (debug_comments_fun eq 1) then  print,'z_base,z_base-z_length_half_count,z_base+z_length_half_count',z_base,z_base-z_length_half_count,z_base+z_length_half_count

;030519 edited this to begin backwards in time from shotmax, at xinit near x=0

;180519 SOS in case we begin below zero along x axis, we can still have a go using this arrangement
;if (x_base le 0.0) then (rho_4d_fun[time_index,*,*,*]=rho_4d_fun[time_index,*,*,*]*0.0D)

;020519 then again, if it reaches beginning of grid, it must disappear! Else it will constantly affect all rays!
;if (x_base le x_length_half_count) then (rho_4d_fun[time_index,*,*,*]=rho_4d_fun[time_index,*,*,*]*0.0D)

;190519 set it non zero when appropriate
;if (x_base gt x_length_half_count) then begin

;310719 BEAM 1  crashing on us! (approaching fast)
;0101819 we comment these out for now (they are still active on the above sister of this function). WE want to just employ the following on moving object!
;**************************************************************************************************************************
;rho_4d_fun[time_index,x_base-x_length_half_count:x_base+x_length_half_count,y_base-y_length_half_count:y_base+y_length_half_count,z_base-z_length_half_count:z_base+z_length_half_count]=beam_density
;230519 add second part
;rho_4d_fun[time_index,x_base2-x_length_half_count:x_base2+x_length_half_count,y_base-y_length_half_count:y_base+y_length_half_count,z_base-z_length_half_count:z_base+z_length_half_count]=beam_density
;**************************************************************************************************************************

;310719 BEAM 2  receding!
;
;310719 ; time_base_reversal represents crossing time for the y-length of the domain. 
;310719 Helps with an easy definition of the receding beam, based on the approaching one.
time_base_reversal=(double(ny1_fun)/u_beam_x)
;**************************************************************************************************************************
;temporary comment out both of these here, in order to try out the next one, the 'procedured' (though non-parameterized) rod!
;rho_4d_fun[time_index,x_base-x_length_half_count:x_base+x_length_half_count,y_base-y_length_half_count:y_base+y_length_half_count,z_base-z_length_half_count:z_base+z_length_half_count]=beam_density
;230519 add second part
;rho_4d_fun[time_index,x_base2-x_length_half_count:x_base2+x_length_half_count,y_base-y_length_half_count:y_base+y_length_half_count,z_base-z_length_half_count:z_base+z_length_half_count]=beam_density
;**************************************************************************************************************************

;310719 here we MUST adopt parametric definition of the rods, relative to a moving centre of the object. This is supposed to be the way to go with those things, not as NOW, which IS too complex!
;pro call here!
;****************************************************************************************************************************************************
;310719 here lies the art, in defining centre_x location in relation to current time in the loop, since we are moving along x in this mother function.
;310719 we use the absolute value of the given beam velocity
centre_x_approaching=fix(     (shotmax_fun-time_index+1)*double(abs(u_beam_x))      )

;020819 now we do a receding one, the first one was actually approaching OK!
;020819 THIS MUST SET TO RECEDING MODE NOW!!! SO
;centre_x_receding=fix(   (  (shotmax_fun-   ((nx_1_fun-6)/u_beam_x)   )+time_index+1)*double(abs(u_beam_x))      )

;030819 just try the reverse of approaching! easy abnd works!
centre_x_receding=fix(nx_1_fun-centre_x_approaching)
;030819 First attempt to define a stationary ruler in the middle
centre_x_ruler=[fix(nx_1_fun*0.20),fix(nx_1_fun*0.50),fix(3.*nx_1_fun*0.80)]

print,'centre_x_ruler',centre_x_ruler
print, ' time_index,centre_x_receding,centre_x,centre_x_approaching',time_index,centre_x_receding,centre_x_receding,centre_x_approaching
;print, time_index_test

print,'centre_x_approaching,centre_x_receding,shotmax_fun,time_index,u_beam_x,double(abs(u_beam_x))  ', centre_x_approaching,centre_x_receding,shotmax_fun,time_index,u_beam_x,double(abs(u_beam_x))  
;020819 here we avoid exiting the grid which leads to errors! 
;020819 must do for both cases!



;130819 brought these back here, to use them earlier, right below!
;130819 try smaller sizes perhaps?
size_x=fix(0.20*(nx_1_fun-2))

size_y=fix(0.05*(ny1_fun-2))

size_z=fix(0.05*(nz1_fun-2))



;130819 HERE FOUND IT! make safety margin variable, not a number 7!

if (centre_x_approaching ge (nx_1_fun-size_x-2)) then centre_x_approaching= (nx_1_fun-size_x-2)

if (centre_x_approaching le (size_x+2)) then centre_x_approaching= (size_x+2)
 
 
 if (centre_x_receding ge (nx_1_fun-size_x-2)) then centre_x_receding= (nx_1_fun-size_x-2)

if (centre_x_receding le (size_x+2)) then centre_x_receding= (size_x+2)
 

;if (centre_x_approaching ge (nx_1_fun-7)) then centre_x_approaching= (nx_1_fun-7)
;130819 keep old ones for safety!
;if (centre_x_approaching le 3) then centre_x_approaching= 3



;if (centre_x_receding ge (nx_1_fun-7)) then centre_x_receding= (nx_1_fun-7)
;;130819 keep old ones for safety!
;if (centre_x_receding le 3) then centre_x_receding= 3


;030819 had to postpone limiting this, as it is a 3-element array for now! well see!
;



print,'centre_x_approaching,centre_x_receding,shotmax_fun,time_index,u_beam_x,double(abs(u_beam_x))  ', centre_x_approaching,centre_x_receding,shotmax_fun,time_index,u_beam_x,double(abs(u_beam_x))  

;if (debug_comments_fun eq 1) then print, 'now a stoppie!'
;stop
;310719 do y,z centres kinda off-centre, in relation to FPoint, in order to avoid overlaps with the central approaching rod! 
;120819 try spreading those a bit!
centre_y_first=fix(0.89*(ny1_fun-2))

centre_y_second=fix(0.11*(ny1_fun-2))

centre_y_ruler=fix(0.5*(ny1_fun-2))



;120819 make these the same, in order to have them all at the same plane. For starters, try a level below. 
;;keep the old ones for backup!
;centre_z_first=fix(0.84*(nz1_fun-2))
;
;centre_z_second=fix(0.21*(nz1_fun-2))
;
;centre_z_ruler=fix(0.5*(nz1_fun-2))
centre_z_first=fix(0.44*(nz1_fun-2))
;
centre_z_second=fix(0.44*(nz1_fun-2))
;
centre_z_ruler=fix(0.44*(nz1_fun-2))




;120819 ruler blob centre's definition!
;120819 alter this code snippet to depend on size_x, which is added to the right of centrexruler! That sum must not exceed the grid edge!.
for iii=0,2,1 do begin

if (centre_x_ruler[iii] ge (nx_1_fun-size_x-2)) then centre_x_ruler[iii]= (nx_1_fun-size_x-2)

if (centre_x_ruler[iii] le (size_x+2)) then centre_x_ruler[iii]= (size_x+2)

endfor

;310719 funny, we call it with_fun suffix for nx_1, etc, yet the pro called does not employ those!!!(nx_i _funned present in the call, but not in the definition, while the opposite is usually true!)
;020819 changed centre_x to centre_x_receding
;
;050819 CAREFUL! HERE we must make sure that the velocity in the x_rod procedure call actually matches the one calced above for the moving centre of the object. There is no direct auto match at the moment!
;050819 use abs cause only the abs value is  used in the centre_x speed calc above! Approach means moving towards negative X!

;200819 calling x_rod, a pro that creates a rod along the x axis
x_rod,centre_x_approaching,centre_y_first,centre_z_first,size_x,size_y,size_z,rho_d_3d,-abs(u_beam_x),time_index,nx_1_fun,ny1_fun,nz1_fun,rho_4d_fun,beam_density,vx1_4d_fun
;130819 try keeping only the ruler, till you see it clearly!

;200819 calling x_rod, a pro that creates a rod along the x axis
x_rod,centre_x_receding,centre_y_second,centre_z_second,size_x,size_y,size_z,rho_d_3d,abs(u_beam_x),time_index,nx_1_fun,ny1_fun,nz1_fun,rho_4d_fun,beam_density,vx1_4d_fun

for iii=0,2,1 do begin
;200819 A loop to define the ruler here!

;200819 calling x_rod, a pro that creates a rod along the x axis
x_rod,centre_x_ruler[iii],centre_y_ruler,centre_z_ruler,size_x,size_y,size_z,rho_d_3d,0.0D,time_index,nx_1_fun,ny1_fun,nz1_fun,rho_4d_fun,beam_density,vx1_4d_fun
;pro x_rod,centre_x,centre_y,centre_z,size_x,size_y,size_z,rho_d_3d,x_velocity,time_index,nx_1,ny1,nz1,rho_4d_fun,beam_density,ux1_4d_fun
;200819 end of ruler loop
endfor
;pro x_rod,centre_x,centre_y,centre_z,size_x,size_y,size_z,rho_d_3d,x_velocity,time_index,nx_1,ny1,nz1,rho_4d_fun,beam_density

;ENDIF
;****************************************************************************************************************************************************

;200819 end of time loop, affecting and redefining the densities all along the timeline of data snapshots.
endfor
;these stuff uncomment and use at end, to check the results 
;SOS use the filter-> smooth function after rendering with ivolume, in order to see the beam (actually looks more like a brick)

;for iggy=1,10,1 do begin 
;ivolume, reform(rho_4d[iggy,*,*,*])
;endfor
;
;print, where(reform(rho_4d[1,*,*,*]) ge 1000000000)
;010519 SOS DO SMOOTH AFTER RENDERING ET VOILA!
;ivolume, reform(rho_4d[3,*,*,*])
;010519 SOS!
;print, where(reform(rho_4d[1,*,focal_point_y,focal_point_z])) ge 10.0)
;end
end





pro x_rod,centre_x,centre_y,centre_z,size_x,size_y,size_z,rho_d_3d,x_velocity,time_index,nx_1,ny1,nz1,rho_4d_fun,beam_density,vx1_4d_fun
;200819 A pro to define a moving rod along the x-axis. IT ASSIGNS BOTH DENSITY AND VELOCITY TO CELLS WITHIN THE ROD! AS OPPOSED TO SOME OLDER EXAMPLE TESTS, where only density is assigned!

;050819 CAREFUL! HERE we must make sure that the velocity in the x_rod procedure call actually matches the one calced above for the moving centre of the object. There is no direct auto match at the moment!
;020819 had to rename this, appr. or rec. is definded by the previous definition of centre_x param call!
;030819 reduced error margin from -7 to -2

;130819 perhaps in these lies our problem, of insufficient x coverage at high x's!
x_low=centre_x-(size_x/2.0)

if (x_low lt 1) then (x_low=1)

x_high=centre_x+(size_x/2.0)

if (x_high gt nx_1-2) then (x_high=nx_1-2)

y_low=centre_y-(size_y/2.0)

if (y_low lt 1) then (y_low=1)

y_high=centre_y+(size_y/2.0)

if (y_high gt ny1-2) then (y_high=ny1-2)


z_low=centre_z-(size_z/2.0)

if (z_low lt 1) then (z_low=1)

z_high=centre_z+(size_z/2.0) 

if (z_high gt nz1-2) then (z_high=nz1-2)

print,'x_low, x_high , y_low, y_high, z_low , z_high,time_index', x_low, x_high , y_low, y_high, z_low , z_high,time_index

rho_4d_fun[time_index,x_low: x_high , y_low: y_high, z_low : z_high]=beam_density
;050819 also added x-velocity (since it is moving along the x-axis) to moving thing!
vx1_4d_fun[time_index,x_low: x_high , y_low: y_high, z_low : z_high]=x_velocity
end































function perp_beam_rod_test, rho_4d_fun,clight_fun,nx_1_fun,ny1_fun,nz1_fun,focal_point_y_fun,focal_point_z_fun,shotmin_fun,shotmax_fun,debug_comments_fun,t_fun
;200819 This function constitutes the implementation of an example test of this program, namely to visualize relativistic effect on an approaching rectangular planar object. For a rectangular shape, 
;200819 its corners shall appear rounded, the faster it is approaching us!
;200819 Probably this only assigns density, not velocity to the approaching object. IT WORKS FOR GEOMETRICAL RELAT. EFFECT.
;200819 IF WE ARE TO GET FREQ. SHIFT EFFECTS AS WELL ON THE OBJECT, THEN WE MUST ALSO ASSIGN VELOCITY HERE. TO BE DONE!

;SOS DO FUNCTIONALIZE THIS EXAMPLE SOS!

;030519 idea: make the loop work back in time as well! More natural way of doing it!

;beam constant density
beam_density=1000000000000000.0D
;010519 shotmax_margin is decreased from shotmax, in order to allow for our setup to work for high clight!. i.e. 
;beam can be very fast yet enough time span exists for rays , traveling at clight, to cross the domain without running out of 
;snapshots to access! beam and rays, two different stories but taking place on the same temporal range!
;margin_fraction is ADDED TO SHOTMIN deducted from shotmax, a fraction of the time span, the multiplicatrion result is integered and then deducted from shotmax
;margin_fraction=0.68
;030519 make margin large enough so that it stops going back in time BEFORE it reaches spatially the end of the domain SOS
;SOS this para shortens duration, when domain is smallish for the rays to cover time span. Either cut the duration here, 
;or else bring in a larger domain.
;080519 BREAK introduced below may render this frsaction a bit less nedded perhaps!
margin_fraction=0.00
shotmin_margin=fix((shotmax_fun-shotmin_fun)*margin_fraction)

if (debug_comments_fun eq 1) then  print,'margin_fraction,shotmin_fun,shotmax_fun,shotmin_margin,shotmax_fun-shotmin_margin',margin_fraction,shotmin_fun,shotmax_fun,shotmin_margin,shotmax_fun-shotmin_margin

;010519 set dens zero everywhere,  AND AT ANYTIME, prepare density tabula rasa for the beam definition to follow!
rho_4d_fun[*,*,*,*]=0.0
;010519 here we define the beam velocity along the x-axis, best neg for naw, in order to approach the Fpoint from the right
u_beam_x=-0.8905*clight_fun
;010519 (x_base, y_base, z_base) are the 3D spatial coordinates of the central point of the beam. Since the beam is taken to move along the negative 
;x direction for now, approaching the focal point, we got: (CAREFUL do not exceed domain size x_base+xsize!)
;ALSO CAREFUL to set x_base at first beyond the screen!
;030519 now we reverse the dens definition, going now backwards timewise. So that shotmax_fun is when it begins rolling back in time!
x_final_fraction=-0.1; (SOS: conceptually should be more than screen, and less than domain-x-half-length!MAY AUTOMATE LATER ON! 
;010519 SOS also consider margin of six in nx_1 vs nx1! MAX=0.88 for 60-100-60 resolution!) 
;xbase must be set within time loop below!

;x_base=fix(nx1*x_init_fraction)+u_beam_x*t(shotind)


x_fractional_half_size=0.05;suggested:0.05
y_fractional_half_size=0.4;suggested:0.3
z_fractional_half_size=0.4;suggested:0.3

;010519 rest of coords set to FPoints' ones, so that beam's centre is moving towards FP, along x axis
y_base=focal_point_y_fun
z_base=focal_point_z_fun
;010519 ULTRA SOS we must avoid beam size exceeding domain limits, therefore: define half counts at less than half of 
;x_length_count=x-dimension-half-size or beam, IN CELLS, calced as a percentage of x-grid size, i.e. independent on resolution!
x_length_half_count=fix(nx_1_fun*x_fractional_half_size)
;y_length_count=y-dimension-half-size or beam, IN CELLS, calced as a percentage of y-grid size, i.e. dependent on resolution!
y_length_half_count=fix(ny1_fun*y_fractional_half_size)
;z_length_count=z-dimension-half-size or beam, IN CELLS, calced as a percentage of z-grid size, i.e. dependent on resolution!
z_length_half_count=fix(nz1_fun*z_fractional_half_size)

if (debug_comments_fun eq 1) then  print,'PERP. ROD DIMS IN CELLS: x_length_half_count,y_length_half_count,z_length_half_count',x_length_half_count,y_length_half_count,z_length_half_count

;020519 SOS must adjust tpicked to begin near shotmax, but not above. Then, adjust here to END at shotmax, but bEGIN at shotmin plus some margin!
;Not as it is now, cos we shoot and miss to get the beam to exit right on the interval end!
;so we change it now! 020519!
;BEAM NOW APPEARS MAGICALLY IN 4D DATA at time (shotmin_fun+shotmax_margin)!



;200819 a loop to define the test's own density, all along the time line of the data snapshots.
for time_index=(shotmax_fun),(shotmin_fun+shotmin_margin),-1 do begin
;030519 SOS now 
;200819 xbase is the centre of the object, as far as the x-axis is concerned.
x_base=fix(nx_1_fun*x_final_fraction)-fix(u_beam_x*t_fun(shotmax_fun-time_index))

;080519 SOS we add this break statement, to allow beam to EXIT grid if very quck, without error. ELSE, we get en error 
;later on on the  program ,when not enough time available! SUPER SOS HERE TIMING STUFF!!
;250719 reduced used nx1 by 10 percent here
if ((x_base+x_length_half_count) ge nx_1_fun*0.9) then break
;020519 SOS do not allow beam to exit 150519 left!
if (x_base lt x_length_half_count) then (x_base = fix(1.1D*x_length_half_count))

if (debug_comments_fun eq 1) then  print,'time_index aka local shotind',time_index 
if (debug_comments_fun eq 1) then  print,'x_base,x_base-x_length_half_count,x_base+x_length_half_count',x_base,x_base-x_length_half_count,x_base+x_length_half_count 
if (debug_comments_fun eq 1) then  print,'y_base,y_base-y_length_half_count,y_base+y_length_half_count',y_base,y_base-y_length_half_count,y_base+y_length_half_count
if (debug_comments_fun eq 1) then  print,'z_base,z_base-z_length_half_count,z_base+z_length_half_count',z_base,z_base-z_length_half_count,z_base+z_length_half_count

print ,x_base-x_length_half_count,x_base+x_length_half_count,y_base-y_length_half_count,y_base+y_length_half_count,z_base-z_length_half_count,z_base+z_length_half_count

;030519 edited this to begin backwards in time from shotmax, at xinit near x=0
rho_4d_fun[time_index,x_base-x_length_half_count:x_base+x_length_half_count,y_base-y_length_half_count:y_base+y_length_half_count,z_base-z_length_half_count:z_base+z_length_half_count]=beam_density
;020519 then again, if it reaches beginning of grid, it must disappear! Else it will constantly affect all rays!
if (x_base lt x_length_half_count) then (rho_4d_fun[time_index,*,*,*]=rho_4d_fun[time_index,*,*,*]*0.0D)

endfor

return, rho_4d_fun

;these stuff uncomment and use at end, to check the results 
;SOS use the filter-> smooth function after rendering with ivolume, in order to see the beam (actually looks more like a brick)

;for iggy=1,10,1 do begin 
;ivolume, reform(rho_4d[iggy,*,*,*])
;endfor


;print, where(reform(rho_4d[1,*,*,*]) ge 1000000000)
;010519 SOS DO SMOOTH AFTER RENDERING ET VOILA!
;ivolume, reform(rho_4d[3,*,*,*])
;010519 SOS!
;print, where(reform(rho_4d[1,*,focal_point_y,focal_point_z])) ge 10.0)
;end
end


;coslosu_4d=coslosu_4d_calc(lx1,lx2,lx3,vx1_4d,vx2_4d,vx3_4d,debug_comments_fun)
function coslosu_4d_calc, phi1_fun,phi2_fun,lx1_fun,lx2_fun,lx3_fun,vx1_4d_fun,vx2_4d_fun,vx3_4d_fun,debug_comments_fun,focused_beam_switch_fun
;200819 This function GLOBALLY (ARRAY-OPS) calculates the cosine of the angle between the local cell velocity and the line of sight. 
;200819 Please notice that for camera obscura (imaging cases 3, 4) not all LOS's are parallel, but they meet at a focal point! This is accounted for in the relevant calcs!
;120320  SUPER SOS

;the following 
;print, lx1_fun(3,29,68,33)*vx1_4d_fun(3,29,68,33)+lx2_fun(3,29,68,33)*vx2_4d_fun(3,29,68,33)+lx3_fun(3,29,68,33)*vx3_4d_fun(3,29,68,33),aaaaa_fun(3,29,68,33),bbbbb_fun(3,29,68,33),ccccc_fun(3,29,68,33)
;is DIFFERENT than the result of aaaaa_fun array op right below! WTF! INVESTIGATE!!!
;the analytical result is SAME as mathematica! but NOT the following array op output! sometimes
;it messes up the sign and slightly the value as well!!! IN here, not in mathem it seems! FIND OUT!
;
;print, lx1_fun(3,29,68,33)*vx1_4d_fun(3,29,68,33),lx2_fun(3,29,68,33)*vx2_4d_fun(3,29,68,33),lx3_fun(3,29,68,33)*vx3_4d_fun(3,29,68,33),aaaaa_fun(3,29,68,33),bbbbb_fun(3,29,68,33),ccccc_fun(3,29,68,33)

;311222 SOS alter sign of lxi here in order to account for an approaching LOS, not a receding one! 
;Alternatively, directly alter the sign of lxi's!
aaaaa_fun=(lx1_fun*vx1_4d_fun+lx2_fun*vx2_4d_fun+lx3_fun*vx3_4d_fun)
;210419 next is unitary los vector length, can be also directly assigned to one, spare the calcs
bbbbb_fun=sqrt(lx1_fun*lx1_fun+lx2_fun*lx2_fun+lx3_fun*lx3_fun)
ccccc_fun=sqrt( (vx1_4d_fun*vx1_4d_fun) + (vx2_4d_fun*vx2_4d_fun) + (vx3_4d_fun*vx3_4d_fun) )
coslosu_4d_fun = aaaaa_fun/(bbbbb_fun*ccccc_fun+0.000001)
if (debug_comments_fun eq 1) then print,'inside COSLOSU here'

if (debug_comments_fun eq 1) then begin 
if (focused_beam_switch_fun eq 1.0) then begin 

;print,'  lx1_fun(2,15,8,36),lx2_fun(2,15,8,36),lx3_fun(2,15,8,36),aaaaa_fun(2,15,8,36),coslosu_4d_fun(2,15,8,36),vx1_4d_fun(2,15,8,36),vx2_4d_fun(2,15,8,36),vx3_4d_fun(2,15,8,36)', lx1_fun(2,15,8,36),lx2_fun(2,15,8,36),lx3_fun(2,15,8,36),aaaaa_fun(2,15,8,36),coslosu_4d_fun(2,15,8,36),vx1_4d_fun(2,15,8,36),vx2_4d_fun(2,15,8,36),vx3_4d_fun(2,15,8,36)
;print,'aaaaa_fun(2,15,8,36),bbbbb_fun(2,15,8,36),ccccc_fun(2,15,8,36) ', aaaaa_fun(2,15,8,36),bbbbb_fun(2,15,8,36),ccccc_fun(2,15,8,36)
;print, lx1_fun(1,0,3,26),lx2_fun(1,0,3,26),lx3_fun(1,0,3,26),aaaaa_fun(1,0,3,26),bbbbb_fun(1,0,3,26),ccccc_fun(1,0,3,26)
;print, vx1_4d_fun(2,26,18,30),vx2_4d_fun(2,26,18,30),vx3_4d_fun(2,26,18,30),aaaaa_fun(2,26,18,30),bbbbb_fun(2,26,18,30),ccccc_fun(2,26,18,30)
;130320 SOS following shows how array op for aaaaafun is OTHER THAN element-wise op for a 
;given element coordinate! W..F! check array-op for aaaaafun again!
;print, lx1_fun(2,27,71,33)*vx1_4d_fun(2,27,71,33)+lx2_fun(2,27,71,33)*vx2_4d_fun(2,27,71,33)+lx3_fun(2,27,71,33)*vx3_4d_fun(2,27,71,33),aaaaa_fun(2,27,71,33),bbbbb_fun(2,27,71,33),ccccc_fun(2,27,71,33)
;print, lx1_fun(2,26,12,28),vx1_4d_fun(2,26,12,28),lx2_fun(2,26,12,28),vx2_4d_fun(2,26,12,28),lx3_fun(2,26,12,28),vx3_4d_fun(2,26,12,28),aaaaa_fun(2,26,12,28),bbbbb_fun(2,26,12,28),ccccc_fun(2,26,12,28)
;180320 SOS this ine for case geom 1 or 2! some are scalar then!
;print, lx1_fun,vx1_4d_fun(2,26,12,28),lx2_fun,vx2_4d_fun(2,26,12,28),lx3_fun,vx3_4d_fun(2,26,12,28),aaaaa_fun(2,26,12,28),bbbbb_fun,ccccc_fun(2,26,12,28)
;
;SOS 130320 FOUND PROBLEM!!!  ARRAYS DIFFER IN SIZES SOS ELSE CALCS ARE GOOD!!! CORRECT ZE SIZES AND SHOULD BE FINE SOS!!!
;nemiss in mathem helped us unveil bug in RLOS!! DO IT CORRECT SIZES OF ARRAYS TO BE THE SAME SOS!



;160320 CASE GEOM 4 (jet from the side, focused beam) lxi's and vxi;s are the same among rlos and pload4D, ands also rlos is consistenty in calcing aaaaa.
; Yet, pload4d gets aaaaa alitlle higher, for some reason! Angles also seem to be the same, but due to differing aaaaa, coslosu
;  for this case 4 appears alittle different! just 1-2 percent. 
;  Why aaaaa from pload is different? perhaps it is calced based of ii instead of iii, etc
;; i.e. not decreasing by one the coords to make same as idl rlos? PERHAPS CHECK aaaaa calc details in pload4d in mathem!

;
;
;
;ccc, aaa are defined as a mixture of vxi and lxi, therefore have correct dims. bbb is sole lxi, so it has wring sozee!
;check bbbbb and also aaa mix for errors from mixed definition!
;lxi are defined based on tanphi1, which also has the wrong size! keep tracking!



print, size(aaaaa_fun),size(lx1_fun),size(lx2_fun),size(lx3_fun)
;vx1,2,3 seem of correct size
print, size(aaaaa_fun),size(vx1_4d_fun),size(vx2_4d_fun),size(vx3_4d_fun),size(lx1_fun),size(lx2_fun),size(lx3_fun)
;SOS 130320 bbbbb is also of the wrong size, like the lx1,2,3. TRACK bbbbb and lxi definitions and CORRECT THEIR SIZE!!
print, size(aaaaa_fun),size(bbbbb_fun),size(ccccc_fun),size(vx1_4d_fun),size(vx2_4d_fun),size(vx3_4d_fun),size(lx1_fun),size(lx2_fun),size(lx3_fun)
endif
endif
if (debug_comments_fun eq 1) then stop
if (debug_comments_fun eq 1) then stop
return, coslosu_4d_fun

end




;coslosu_4d=coslosb_4d_calc(lx1,lx2,lx3,vx1_4d,vx2_4d,vx3_4d,debug_comments_fun)
function coslosb_4d_calc, phi1_fun,phi2_fun,lx1_fun,lx2_fun,lx3_fun,bx1_4d_fun,bx2_4d_fun,bx3_4d_fun,debug_comments_fun,focused_beam_switch_fun
;090822 SOS we copy the coslosu function to coslosB. coslosb is the cosine of the angle between los and bfield. It is the twisted theta angle in Pacholchzyk synchrotron emission calculation. 
;;090822 Thus we need that angle everywhere, aka globally, in order to calc sync emiss globalloy. It is also required for sync absorption globally. 
;090822 This function GLOBALLY (ARRAY-OPS) calculates the cosine of the angle between the local cell magnetic field the line of sight. 
;200819 Please notice that for camera obscura (imaging cases 3, 4) not all LOS's are parallel, but they meet at a focal point! This is accounted for in the relevant calcs!
;120320  SUPER SOS

;the following 
;print, lx1_fun(3,29,68,33)*vx1_4d_fun(3,29,68,33)+lx2_fun(3,29,68,33)*vx2_4d_fun(3,29,68,33)+lx3_fun(3,29,68,33)*vx3_4d_fun(3,29,68,33),aaaaa_fun(3,29,68,33),bbbbb_fun(3,29,68,33),ccccc_fun(3,29,68,33)
;is DIFFERENT than the result of aaaaa_fun array op right below! WTF! INVESTIGATE!!!
;the analytical result is SAME as mathematica! but NOT the following array op output! sometimes
;it messes up the sign and slightly the value as well!!! IN here, not in mathem it seems! FIND OUT!
;
;print, lx1_fun(3,29,68,33)*vx1_4d_fun(3,29,68,33),lx2_fun(3,29,68,33)*vx2_4d_fun(3,29,68,33),lx3_fun(3,29,68,33)*vx3_4d_fun(3,29,68,33),aaaaa_fun(3,29,68,33),bbbbb_fun(3,29,68,33),ccccc_fun(3,29,68,33)


aaaaa_coslosb_fun=(lx1_fun*bx1_4d_fun+lx2_fun*bx2_4d_fun+lx3_fun*bx3_4d_fun)
;210419 next is unitary los vector length, can be also directly assigned to one, spare the calcs
;
;090822 fur coslosb, we keep bbbbb the same, saves one array, as it is the same as for coslosu
bbbbb_fun=sqrt(lx1_fun*lx1_fun+lx2_fun*lx2_fun+lx3_fun*lx3_fun)
ccccc_coslosb_fun=sqrt( (bx1_4d_fun*bx1_4d_fun) + (bx2_4d_fun*bx2_4d_fun) + (bx3_4d_fun*bx3_4d_fun) )
coslosb_4d_fun = aaaaa_coslosb_fun/(bbbbb_fun*ccccc_coslosb_fun+0.000001)
if (debug_comments_fun eq 1) then print,'inside COSLOSB here'

if (debug_comments_fun eq 1) then begin 
if (focused_beam_switch_fun eq 1.0) then begin 

;print,'  lx1_fun(2,15,8,36),lx2_fun(2,15,8,36),lx3_fun(2,15,8,36),aaaaa_coslosb_fun(2,15,8,36),coslosb_4d_fun(2,15,8,36),bx1_4d_fun(2,15,8,36),bx2_4d_fun(2,15,8,36),bx3_4d_fun(2,15,8,36)', lx1_fun(2,15,8,36),lx2_fun(2,15,8,36),lx3_fun(2,15,8,36),aaaaa_fun(2,15,8,36),coslosb_4d_fun(2,15,8,36),bx1_4d_fun(2,15,8,36),bx2_4d_fun(2,15,8,36),bx3_4d_fun(2,15,8,36)
;print,'aaaaa_coslosb_fun(2,15,8,36),bbbbb_fun(2,15,8,36),ccccc_coslosb_fun(2,15,8,36) ', aaaaa_coslosb_fun(2,15,8,36),bbbbb_fun(2,15,8,36),ccccc_coslosb_fun(2,15,8,36)
;print, lx1_fun(1,0,3,26),lx2_fun(1,0,3,26),lx3_fun(1,0,3,26),aaaaa_coslosb_fun(1,0,3,26),bbbbb_fun(1,0,3,26),ccccc_coslosb_fun(1,0,3,26)
;print, bx1_4d_fun(2,26,18,30),bx2_4d_fun(2,26,18,30),bx3_4d_fun(2,26,18,30),aaaaa_coslosb_fun(2,26,18,30),bbbbb_fun(2,26,18,30),ccccc_coslosb_fun(2,26,18,30)
;130320 SOS following shows how array op for aaaaafun is OTHER THAN element-wise op for a 
;given element coordinate! W..F! check array-op for aaaaafun again!
;print, lx1_fun(2,27,71,33)*bx1_4d_fun(2,27,71,33)+lx2_fun(2,27,71,33)*bx2_4d_fun(2,27,71,33)+lx3_fun(2,27,71,33)*bx3_4d_fun(2,27,71,33),aaaaa_coslosb_fun(2,27,71,33),bbbbb_fun(2,27,71,33),ccccc_coslosb_fun(2,27,71,33)
;print, lx1_fun(2,26,12,28),bx1_4d_fun(2,26,12,28),lx2_fun(2,26,12,28),bx2_4d_fun(2,26,12,28),lx3_fun(2,26,12,28),bx3_4d_fun(2,26,12,28),aaaaa_coslosb_fun(2,26,12,28),bbbbb_fun(2,26,12,28),ccccc_coslosb_fun(2,26,12,28)
;180320 SOS this ine for case geom 1 or 2! some are scalar then!
;print, lx1_fun,bx1_4d_fun(2,26,12,28),lx2_fun,bx2_4d_fun(2,26,12,28),lx3_fun,bx3_4d_fun(2,26,12,28),aaaaa_coslosb_fun(2,26,12,28),bbbbb_fun,ccccc_coslosb_fun(2,26,12,28)
;
;SOS 130320 FOUND PROBLEM!!!  ARRAYS DIFFER IN SIZES SOS ELSE CALCS ARE GOOD!!! CORRECT ZE SIZES AND SHOULD BE FINE SOS!!!
;nemiss in mathem helped us unveil bug in RLOS!! DO IT CORRECT SIZES OF ARRAYS TO BE THE SAME SOS!



;160320 CASE GEOM 4 (jet from the side, focused beam) lxi's and vxi;s are the same among rlos and pload4D, ands also rlos is consistenty in calcing aaaaa.
; Yet, pload4d gets aaaaa alitlle higher, for some reason! Angles also seem to be the same, but due to differing aaaaa, coslosu
;  for this case 4 appears alittle different! just 1-2 percent. 
;  Why aaaaa from pload is different? perhaps it is calced based of ii instead of iii, etc
;; i.e. not decreasing by one the coords to make same as idl rlos? PERHAPS CHECK aaaaa calc details in pload4d in mathem!

;
;
;
;ccc, aaa are defined as a mixture of vxi and lxi, therefore have correct dims. bbb is sole lxi, so it has wring sozee!
;check bbbbb and also aaa mix for errors from mixed definition!
;lxi are defined based on tanphi1, which also has the wrong size! keep tracking!



print, size(aaaaa_coslosb_fun),size(lx1_fun),size(lx2_fun),size(lx3_fun)
;vx1,2,3 seem of correct size
print, size(aaaaa_coslosb_fun),size(bx1_4d_fun),size(bx2_4d_fun),size(bx3_4d_fun),size(lx1_fun),size(lx2_fun),size(lx3_fun)
;SOS 130320 bbbbb is also of the wrong size, like the lx1,2,3. TRACK bbbbb and lxi definitions and CORRECT THEIR SIZE!!
print, size(aaaaa_coslosb_fun),size(bbbbb_fun),size(ccccc_coslosb_fun),size(bx1_4d_fun),size(bx2_4d_fun),size(bx3_4d_fun),size(lx1_fun),size(lx2_fun),size(lx3_fun)
endif
endif
if (debug_comments_fun eq 1) then stop
if (debug_comments_fun eq 1) then stop
return, coslosb_4d_fun

end

;130722 calc sinlosb_4d from coslosb_4d
function sinlosb_4d_calc, coslosb_4d
endiameso=sqrt(1.0-coslosb_4d*coslosb_4d)
;stop
return, endiameso
end














function tanphi1_global_calc, tan_phi1_fun,focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,nx_1_fun,ny1_fun,nz1_fun
;200819 Globally calc tan of phi1. For the YZ case!
;
;180419 calculates tanphi1 tan of azimuth angle:Dy/Dx. Distances are in cells, measured from the focal point. 


;140320 After setting tanphi12 margin to six, down from seven, 
;here it eems to work from zero to plus five, inst. of plus six! Wont work from one to plus six though!
for zindigo=0.0,nz1_fun+5 do begin
 for yindigo=0.0,ny1_fun+5 do begin
  for xindigo=0.0,nx_1_fun+5 do begin
;      xcoord(xindigo,yindigo,zindigo)=xindigo
;      ycoord(xindigo,yindigo,zindigo)=yindigo
;      zcoord(xindigo,yindigo,zindigo)=zindigo
;careful 170419 for neg tans wtf
;add a bit to denom no zero div

;080320 SOS KEEP THIS added bit EXACT to 0.0001 three zeros, TO BE THE SAME AS IN NEMISS_PLOAD4D SOS require consistency! !!! 

       tan_phi1_fun(*,xindigo,yindigo,zindigo)=(yindigo-focal_point_y_fun)/(xindigo-focal_point_x_fun+0.0001)
        ;070819 SOS when denom=0, try this:
;commented this out! 070819 only activate if abs. necessary!    

         if ((xindigo-focal_point_x_fun) eq 0.0) then begin
         tan_phi1_fun(*,xindigo,yindigo,zindigo)=(yindigo-focal_point_y_fun)/(xindigo-focal_point_x_fun+1.0)
;        if (debug_comments_fun eq 1) then print,'plus one denom in tan phi activated avoid div by zero!'
        endif
       
;       tan_phi2(xindigo,yindigo,zindigo)=(zindigo-focal_point_z_fun)/(sqrt( (xindigo-focal_point_x_fun)^2.0 + (yindigo-focal_point_y_fun)^2.0 ) +0.0001)

;170419 for comparison with main loop ones!
;       deltay_interim_signed_fun=double((ny10_fun-focal_point_y_fun))
;deltax_interim_signed_fun=double((slice_x_location_fun-focal_point_x_fun));xfocalpoint is now default to zero probably anyway
;deltaz_interim_signed_fun=double((nz10_fun-focal_point_z_fun)) 
     
  endfor
 endfor     
endfor

RETURN, tan_phi1_fun

END




function tanphi1_global_calcXZ, tan_phi1_fun,focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun,nx_1_fun,ny1_fun,nz1_fun
;200819 Globally calc tan of phi1. For the XZ case!

;180419 calculates tanphi1 tan of azimuth angle:Dy/Dx. Distances are in cells, measured from the focal point. 


;130320 SOS this def from ZERO to plus six, makes necessary defining tanphi with plus SEVEN, thus ruining later on the size equality for lx1,2,3, aaaaa, bbbbb messed up, etc. 
;SO FIND A WAY AROUND THIS REQUIREMENT FOR A SEVEN in tan phi! e.g. these loops may go from ONE till the end, not from ZERO. OR SOMETHING ELSE SIMILAR SOS WORK AT THIS POINT NOW!!!
;130320 CARRY ON FROM HERE! SOS! 
;WHEN CORRECTED, REPEAT FOR REST THREE tanphi subroutines! SOS! here is tan phi1 XZ
;
;140320 try ftarting loops from 1 inst. of zero! IF GOOD, then repeat to all other tanphi routines!


;140320 After setting tanphi12 margin to six, down from seven, 
;here it seems to work from zero to plus five, inst. of plus six! Wont work from one to plus six though!
for zindigo=0.0,nz1_fun+5 do begin
 for yindigo=0.0,ny1_fun+5 do begin
  for xindigo=0.0,nx_1_fun+5 do begin
;      xcoord(xindigo,yindigo,zindigo)=xindigo
;      ycoord(xindigo,yindigo,zindigo)=yindigo
;      zcoord(xindigo,yindigo,zindigo)=zindigo
;careful 170419 for neg tans wtf
;add a bit to denom no zero div

;080320 SOS KEEP THIS added bit EXACT to 0.0001 three zeros, TO BE THE SAME AS IN NEMISS_PLOAD4D SOS require consistency! !!! 
       tan_phi1_fun(*,xindigo,yindigo,zindigo)=(yindigo-focal_pointXZ_y_fun)/(xindigo-focal_pointXZ_x_fun+0.0001)
 
;commented this out! 070819 only activate if abs. necessary!    
       
         if ((xindigo-focal_pointXZ_x_fun) eq 0.0) then begin
          tan_phi1_fun(*,xindigo,yindigo,zindigo)=(yindigo-focal_pointXZ_y_fun)/(xindigo-focal_pointXZ_x_fun+1.0)
;        if (debug_comments_fun eq 1) then print,'plus one denom in tan phi activated avoid div by zero!'
        endif 
       
       
;       tan_phi2(xindigo,yindigo,zindigo)=(zindigo-focal_point_z_fun)/(sqrt( (xindigo-focal_point_x_fun)^2.0 + (yindigo-focal_point_y_fun)^2.0 ) +0.0001)

;170419 for comparison with main loop ones!
;       deltay_interim_signed_fun=double((ny10_fun-focal_point_y_fun))
;deltax_interim_signed_fun=double((slice_x_location_fun-focal_point_x_fun));xfocalpoint is now default to zero probably anyway
;deltaz_interim_signed_fun=double((nz10_fun-focal_point_z_fun)) 
     
  endfor
 endfor     
endfor

RETURN, tan_phi1_fun

END



function tanphi2_global_calc, tan_phi2_fun,focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,nx_1_fun,ny1_fun,nz1_fun
;200819 Globally calc tan of phi2. For the YZ case!

;180419 calculates tanphi2 tan of elevation angle: Dz/(sqrt(Dx^2+Dy^2)). Distances are in cells, measured from the focal point. 


;140320 After setting tanphi12 margin to six, down from seven, 
;here it eems to work from zero to plus five, inst. of plus six! Wont work from one to plus six though!
for zindigo=0.0,nz1_fun+5 do begin
 for yindigo=0.0,ny1_fun+5 do begin
  for xindigo=0.0,nx_1_fun+5 do begin
;      xcoord(xindigo,yindigo,zindigo)=xindigo
;      ycoord(xindigo,yindigo,zindigo)=yindigo
;      zcoord(xindigo,yindigo,zindigo)=zindigo
;careful 170419 for neg tans wtf
;add a bit to denom no zero div
;       tan_phi1_fun(xindigo,yindigo,zindigo)=(yindigo-focal_point_y_fun)/(xindigo-focal_point_x_fun+0.0001)
       ;080320 SOS KEEP THIS added bit EXACT to 0.0001 three zeros, TO BE THE SAME AS IN NEMISS_PLOAD4D SOS require consistency! !!! 
       
        tan_phi2_fun(*,xindigo,yindigo,zindigo)=(zindigo-focal_point_z_fun)/(sqrt( (xindigo-focal_point_x_fun)^2.0 + (yindigo-focal_point_y_fun)^2.0 ) +0.0001)
          ;070819 added one to the denom to avoid div by huge number when zero denom!

;commented this out! 070819 only activate if abs. necessary!    

         if ((sqrt( (xindigo-focal_point_x_fun)^2.0 + (yindigo-focal_point_y_fun)^2.0 )) eq 0.0) then begin
                 tan_phi2_fun(*,xindigo,yindigo,zindigo)=(zindigo-focal_point_z_fun)/(sqrt( (xindigo-focal_point_x_fun)^2.0 + (yindigo-focal_point_y_fun)^2.0 ) +1.0)
         
;        if (debug_comments_fun eq 1) then print,'plus one denom in tan phi activated avoid div by zero!'
        endif 

        
;170419 for comparison with main loop ones!
;       deltay_interim_signed_fun=double((ny10_fun-focal_point_y_fun))
;deltax_interim_signed_fun=double((slice_x_location_fun-focal_point_x_fun));xfocalpoint is now default to zero probably anyway
;deltaz_interim_signed_fun=double((nz10_fun-focal_point_z_fun)) 
     
  endfor
 endfor     
endfor

RETURN, tan_phi2_fun

END



function tanphi2_global_calcXZ, tan_phi2_fun,focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun,nx_1_fun,ny1_fun,nz1_fun
;200819 Globally calc tan of phi2. For the XZ case!

;180419 calculates tanphi2 tan of elevation angle: Dz/(sqrt(Dx^2+Dy^2)). Distances are in cells, measured from the focal point. 

  ;070320 hereby define temp array ib order to compare sub-terms  from within tans_phi2_XZ
       ;aim is to compare with nemiss corresponding terms. These arrays are local, so shall cease to 
       ;;exist after this subroutine exits!
       ;first define size-wise
       tempyg1=tan_phi2_fun
          tempyg2=tan_phi2_fun
             tempyg3=tan_phi2_fun
       ;rest within the loop that follows!

for zindigo=0.0,nz1_fun+5 do begin
 for yindigo=0.0,ny1_fun+5 do begin
  for xindigo=0.0,nx_1_fun+5 do begin
;      xcoord(xindigo,yindigo,zindigo)=xindigo
;      ycoord(xindigo,yindigo,zindigo)=yindigo
;      zcoord(xindigo,yindigo,zindigo)=zindigo
;careful 170419 for neg tans wtf
;add a bit to denom no zero div
;       tan_phi1_fun(xindigo,yindigo,zindigo)=(yindigo-focal_point_y_fun)/(xindigo-focal_point_x_fun+0.0001)
     
       ;080320 SOS KEEP THIS added bit EXACT to 0.0001 three zeros, TO BE THE SAME AS IN NEMISS_PLOAD4D SOS require consistency! !!! 
       
       
        tan_phi2_fun(*,xindigo,yindigo,zindigo)=(zindigo-focal_pointXZ_z_fun)/(sqrt( (xindigo-focal_pointXZ_x_fun)^2.0 + (yindigo-focal_pointXZ_y_fun)^2.0 ) +0.0001)
        
        
          ;070320  test stuff  here
          tempyg1(*,xindigo,yindigo,zindigo)=(zindigo-focal_pointXZ_z_fun)
           tempyg2(*,xindigo,yindigo,zindigo)=(xindigo-focal_pointXZ_x_fun)
            tempyg3(*,xindigo,yindigo,zindigo)=(yindigo-focal_pointXZ_y_fun)
          
          ;070819 added one to the denom to avoid div by huge number when zero denom!          

;commented this out! 070819 only activate if abs. necessary!    

         if (sqrt( (xindigo-focal_pointXZ_x_fun)^2.0 + (yindigo-focal_pointXZ_y_fun)^2.0 ) eq 0.0) then begin
             tan_phi2_fun(*,xindigo,yindigo,zindigo)=(zindigo-focal_pointXZ_z_fun)/(sqrt( (xindigo-focal_pointXZ_x_fun)^2.0 + (yindigo-focal_pointXZ_y_fun)^2.0 ) +1.0)
         
       
         
;        if (debug_comments_fun eq 1) then print,'plus one denom in tan phi activated avoid div by zero!'
        endif 

;170419 for comparison with main loop ones!
;       deltay_interim_signed_fun=double((ny10_fun-focal_point_y_fun))
;deltax_interim_signed_fun=double((slice_x_location_fun-focal_point_x_fun));xfocalpoint is now default to zero probably anyway
;deltaz_interim_signed_fun=double((nz10_fun-focal_point_z_fun)) 
     
  endfor
 endfor     
endfor
STOP
;070320 SOS conclusion: sample sub-term numbers look the same among bot codes, yet for some reason mathematica calcs phi2 at a slightly different
;value than idl!  unitadded looks the same, three zeros after decimal then unity! WTF! maybe something with the accuracy? leave it as is, use it like that. CAREFUL at extreme phi2 cases, though!
print, focal_pointXZ_x_fun,focal_pointXZ_y_fun, focal_pointXZ_z_fun
print,tempyg1(3,30,70,36),tempyg2(3,30,70,36),tempyg3(3,30,70,36),tan_phi2_fun(3,30,70,36),atan(tan_phi2_fun(3,27,73,31))
RETURN, tan_phi2_fun

END



;010719 functions for radiograph version ratio1f, ratio2f
function ratio1f_radiograph, tanphi1
;200819 For radiograph cases (imaging cases 1 and 2) things are quite simple, as all LOS's are parallel to each other!
return, tanphi1
end


;010719 functions for radiograph version ratio1f, ratio2f
function ratio2f_radiograph, tanphi2
;200819 For radiograph cases (imaging cases 1 and 2) things are quite simple, as all LOS's are parallel to each other!
return, tanphi2
end



;130719 SOS must employ first coord, second coord, etc, NOT ny10 etc!!! SOSOSOSOS !!! CORRECT THIS!!!! 
;130719 SOS ALREADY CORRECTED IN THE FUNCTION CALL !!! ny10 nhas been REPLACED by the corresponding coord of the imaging loop SOS DONE!!!!
;180719 corrected: ny10, not nx10! in this function!!! it is actually nx10 in the XZ version of this funcvtion!WE messed, in 1707, the YZ case;s function!!! FIXED IT NOW! CAREFUL!
FUNCTION ratio1f_function,  slice_x_location_fun,ny10_fun,nz10_fun,focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,debug_comments_fun
;200819 This function calcs ratio1, which is the same as the tanphi1 thing. Apparently not both, this and tanphi above, are employed! But both are kept available!

;130719 this function to be called for the case of camera obscura, apparently YZ case (must be checked if it works for the xz case of camera obscura!)

   ;130419 '_fun' ending means a quantity intended for local use only.
   
    ;SUPER_SOS 060419 MUST DEAL WITH POSSIBLE NEGATIVE RATIOS HERE! NOT METEM BEFORE SUPER SOS! NEED PAPER! SOS 

;SOS 070419 NEG ratios are probably OK.

deltay_interim_signed_fun=double((ny10_fun-focal_point_y_fun))
deltax_interim_signed_fun=double((slice_x_location_fun-focal_point_x_fun));xfocalpoint is now default to zero probably anyway
deltaz_interim_signed_fun=double((nz10_fun-focal_point_z_fun)) 
    
    
    xy_length_unsigned_no_sign_needed_here_fun = double(sqrt( deltay_interim_signed_fun^2 + deltax_interim_signed_fun^2  )) 
    ;060819 added some to the denom, no infs!
    ratio1_value_fun=double(deltay_interim_signed_fun)/double((slice_x_location_fun-focal_point_x_fun+0.0001)) 
    ;070819 avoid huge values!
    if ((slice_x_location_fun-focal_point_x_fun) eq 0.0) then begin
      ratio1_value_fun=double(deltay_interim_signed_fun)/double((slice_x_location_fun-focal_point_x_fun+1.0))   
                 if (debug_comments_fun eq 1) then print,'plus one denom in tan phi activated avoid div by zero!'
        endif 
    
    
    
    ;only uncomment the following prints for debug purposes! 130419
    if (debug_comments_fun eq 1) then print,'xy_length_unsigned_no_sign_needed_here_fun,deltay_interim_signed_fun,deltax_interim_signed_fun',xy_length_unsigned_no_sign_needed_here_fun,deltay_interim_signed_fun,deltax_interim_signed_fun
    if (debug_comments_fun eq 1) then print,'double(deltay_interim_signed_fun), double(deltay_interim_signed_fun)',double(deltay_interim_signed_fun), double(deltay_interim_signed_fun)
    if (debug_comments_fun eq 1) then print, 'deltaz_interim_signed_fun',deltaz_interim_signed_fun
    RETURN, ratio1_value_fun
END


;os edo commented 200819


FUNCTION ratio1f_functionXZ,  sliceXZ_y_location_fun,nx10_fun,nz10_fun,focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun,debug_comments_fun
;210819 This function calcs the tan of the azimuth, at a given point, along a los, for the XZ case of camera obscura problem geometry.


;130719 this function to be called for the case of camera obscura, apparently YZ case (must be checked if it works for the xz case of camera obscura!)

   ;130419 '_fun' ending means a quantity intended for local use only.
   
    ;SUPER_SOS 060419 MUST DEAL WITH POSSIBLE NEGATIVE RATIOS HERE! NOT METEM BEFORE SUPER SOS! NEED PAPER! SOS 

;SOS 070419 NEG ratios are probably OK.

deltax_interim_signed_fun=double((nx10_fun-focal_pointXZ_x_fun))
deltay_interim_signed_fun=double((sliceXZ_y_location_fun-focal_pointXZ_y_fun));xfocalpoint is now default to zero probably anyway
deltaz_interim_signed_fun=double((nz10_fun-focal_pointXZ_z_fun)) 
    
    
    xy_length_unsigned_no_sign_needed_here_fun = double(sqrt( deltay_interim_signed_fun^2 + deltax_interim_signed_fun^2  )) 
        ;060819 added some to the denom, no infs!
    
    ratio1XZ_value_fun=double(deltay_interim_signed_fun)/double((deltax_interim_signed_fun+0.0001)) 
    ;070819 SOS when denom=0, try this:
    if (deltax_interim_signed_fun eq 0.0) then begin
    ratio1XZ_value_fun=double(deltay_interim_signed_fun)/double((deltax_interim_signed_fun+1.0)) 
        if (debug_comments_fun eq 1) then print,'plus one denom activated avoid div by zero!'
    endif
    ;
    ;only uncomment the following prints for debug purposes! 130419
    if (debug_comments_fun eq 1) then print,'xy_length_unsigned_no_sign_needed_here_fun,deltay_interim_signed_fun,deltax_interim_signed_fun',xy_length_unsigned_no_sign_needed_here_fun,deltay_interim_signed_fun,deltax_interim_signed_fun
    if (debug_comments_fun eq 1) then print,'double(deltay_interim_signed_fun), double(deltay_interim_signed_fun)',double(deltay_interim_signed_fun), double(deltay_interim_signed_fun)
    if (debug_comments_fun eq 1) then print, 'deltaz_interim_signed_fun',deltaz_interim_signed_fun
    RETURN, ratio1XZ_value_fun
END








function ratio1f_fixed_function,ny10_fun,nz10_fun,debug_comments_fun
;210819 This one function calcs the azimuth ta, for the radiograph case.
;130719 this function to be called in the case of radiograph, yz version. must be checked if it works for radiograph xz, or if it needs modifa for that!


  RETURN, ratio1_fixed_value_fun
end



;ratio1f=double(ratio1f_function(slice_x_location,second_coord,nz10,focal_point_x,focal_point_y,focal_point_z,debug_comments))

;ratio2f=double(ratio2f_function(slice_x_location,second_coord,nz10,focal_point_x,focal_point_y,focal_point_z,debug_comments))


;130719 SOS ALREADY CORRECTED IN THE FUNCTION CALL !!! ny10 nhas been REPLACED by the corresponding coord of the imaging loop SOS DONE!!!!
FUNCTION ratio2f_function,slice_x_location_fun,ny10_fun,nz10_fun,focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,debug_comments_fun
;210819 This one calcs tan of elevation for YZ cam obscura case.
;130719 this function is to be called in the case of camera obscura yz. Must check wether it also works for cam obsc xz case!

   ;130419 '_fun' ending means a quantity intended for local use only.
   
    ;SUPER_SOS 060419 MUST DEAL WITH POSSIBLE NEGATIVE RATIOS HERE! NOT METEM BEFORE SUPER SOS! NEED PAPER! SOS 

;SOS 070419 NEG ratios are probably OK.

deltay_interim_signed_fun=double((ny10_fun-focal_point_y_fun))
deltax_interim_signed_fun=double((slice_x_location_fun-focal_point_x_fun));xfocalpoint is now default to zero probably anyway
deltaz_interim_signed_fun=double((nz10_fun-focal_point_z_fun)) 
    
    
    xy_length_unsigned_no_sign_needed_here_fun = double(sqrt( deltay_interim_signed_fun^2 + deltax_interim_signed_fun^2  )) 
    
    
    ;next comment line for help only
;ratio2f=double(deltaz_interim_signed)/double( ( xy_length_unsigned_no_sign_needed_here  )  ) 
    
    
    ratio2_value_fun=double(deltaz_interim_signed_fun)/double( ( xy_length_unsigned_no_sign_needed_here_fun  )  ) 
    
;commented this out! 070819 only activate if abs. necessary!    
;     if (xy_length_unsigned_no_sign_needed_here_fun eq 0.0) then begin
;          ratio2_value_fun=double(deltaz_interim_signed_fun)/double( ( xy_length_unsigned_no_sign_needed_here_fun +1.0  )  ) 
 
         ;        if (debug_comments_fun eq 1) then print,'plus one denom in tan phi activated avoid div by zero!'
;        endif 
    
    
    ;only uncomment the following prints for debug purposes! 130419
    if (debug_comments_fun eq 1) then print,'xy_length_unsigned_no_sign_needed_here_fun,deltay_interim_signed_fun,deltax_interim_signed_fun',xy_length_unsigned_no_sign_needed_here_fun,deltay_interim_signed_fun,deltax_interim_signed_fun
    if (debug_comments_fun eq 1) then print,'double(deltay_interim_signed_fun), double(deltay_interim_signed_fun)',double(deltay_interim_signed_fun), double(deltay_interim_signed_fun)
    if (debug_comments_fun eq 1) then print, 'deltaz_interim_signed_fun',deltaz_interim_signed_fun
    RETURN, ratio2_value_fun
END


FUNCTION ratio2f_functionXZ,sliceXZ_y_location_fun,nx10_fun,nz10_fun,focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun,debug_comments_fun
;201819 This one calcs tan of elevation for XZ cam. obscura case.
;130719 this function is to be called in the case of camera obscura yz. Must check wether it also works for cam obsc xz case!

   ;130419 '_fun' ending means a quantity intended for local use only.
   
    ;SUPER_SOS 060419 MUST DEAL WITH POSSIBLE NEGATIVE RATIOS HERE! NOT METEM BEFORE SUPER SOS! NEED PAPER! SOS 

;SOS 070419 NEG ratios are probably OK.
    
deltax_interim_signed_fun=double((nx10_fun-focal_pointXZ_x_fun))
deltay_interim_signed_fun=double((sliceXZ_y_location_fun-focal_pointXZ_y_fun));xfocalpoint is now default to zero probably anyway
deltaz_interim_signed_fun=double((nz10_fun-focal_pointXZ_z_fun)) 
    
    xy_length_unsigned_no_sign_needed_here_fun = double(sqrt( deltay_interim_signed_fun^2 + deltax_interim_signed_fun^2  )) 
    
    
    ;next comment line for help only
;ratio2f=double(deltaz_interim_signed)/double( ( xy_length_unsigned_no_sign_needed_here  )  ) 
    
    
    ratio2XZ_value_fun=double(deltaz_interim_signed_fun)/double( ( xy_length_unsigned_no_sign_needed_here_fun  )  ) 
;commented this out! 070819 only activate if abs. necessary!    
;       if (xy_length_unsigned_no_sign_needed_here_fun eq 0.0) then begin
;             ratio2XZ_value_fun=double(deltaz_interim_signed_fun)/double( ( xy_length_unsigned_no_sign_needed_here_fun +1.0  )  ) 
 
         ;        if (debug_comments_fun eq 1) then print,'plus one denom in tan phi activated avoid div by zero!'
;        endif 
    
    
    ;only uncomment the following prints for debug purposes! 130419
    if (debug_comments_fun eq 1) then print,'xy_length_unsigned_no_sign_needed_here_fun,deltay_interim_signed_fun,deltax_interim_signed_fun',xy_length_unsigned_no_sign_needed_here_fun,deltay_interim_signed_fun,deltax_interim_signed_fun
    if (debug_comments_fun eq 1) then print,'double(deltay_interim_signed_fun), double(deltay_interim_signed_fun)',double(deltay_interim_signed_fun), double(deltay_interim_signed_fun)
    if (debug_comments_fun eq 1) then print, 'deltaz_interim_signed_fun',deltaz_interim_signed_fun
    RETURN, ratio2XZ_value_fun
END



function c5_pachz, gammac

;120722 here we define pachz constants.


;110722 SOS here we do pachz coeffs SOS may be compared to old ones from phd days.
c1_pachz=6.27*(10.0^18) 
;print, 6.27*(10.0^18)
;print, "c1_pachz",c1_pachz

c2_pachz=2.37*(10.0^(-3))
;print, 2.37*(10.0^(-3))
;print, "c2_pachz",c2_pachz

c3_pachz=1.87*(10.0^(-23))
;print, 1.87*(10.0^(-23))
;print, "c3_pachz",c3_pachz

c4_pachz=4.20*(10.0^7)
;print, 4.20*(10.0^7)
;print, "c4_pachz",c4_pachz
apotelesma= 0.25*c3_pachz*(gamma( (3.0*gammac-1.0)/12.0    ) )*(gamma( (3.0*gammac+7.0)/12.0    ) )*( (gammac+(7.0/3.0))/(gammac+1.0)  )

return, apotelesma
end

;ttttest1=c5_pachz(2.0, c3_pachz)
;print, ttttest1, "c5_pachz(2.0, c3_pachz)"
function c6_pachz, gammac

c1_pachz=6.27*(10.0^18) 
;print, 6.27*(10.0^18)
;print, "c1_pachz",c1_pachz

c2_pachz=2.37*(10.0^(-3))
;print, 2.37*(10.0^(-3))
;print, "c2_pachz",c2_pachz

c3_pachz=1.87*(10.0^(-23))
;print, 1.87*(10.0^(-23))
;print, "c3_pachz",c3_pachz

c4_pachz=4.20*(10.0^7)
;print, 4.20*(10.0^7)
;print, "c4_pachz",c4_pachz

apotelesma2=(1/32.0)*(   ( 3.0*(10.0^10)/c1_pachz )^(2.0) )*c3_pachz*( gammac+(10.0/3.0) )*(gamma( (3.0*gammac+2.0)/12.0    ) )*(gamma( (3.0*gammac+10.0)/12.0    ) )

return, apotelesma2
end
;print, c6_pachz(2.0, c1_pachz, c3_pachz)



pro angles_calculation, focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun,ratio1f_comparison_fun,ratio2f_comparison_fun,lx1_fixed_fun,lx2_fixed_fun,lx3_fixed_fun,lx1_fun,lx2_fun,lx3_fun,focused_beam_switch_fun,phi1_fun,phi2_fun,tan_phi1_fun,tan_phi2_fun,phi1_external_fun,phi2_external_fun,ratio1f_fun,ratio2f_fun,phi1_fixed_fun,phi2_fixed_fun,debug_comments_fun,nx_1_fun,ny1_fun,nz1_fun,focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,IMAGING_GEOMETRY_SELECTOR
;210819 This one globally calcs los angles, using either fixed angles, for radiograph, or angles to the corresponding focal point, for cam. obscura Latter through calling tanphi(i)globalcalc routines). 

;170719 SOS must alter the above ratio functions! these maybe not employed after all! MUST tidy thing up!
;170719 add the following to the above param list and to the current procedure's  call, later in the code,  as well! ;focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun
;130719 we only  keep this function for calcing the lxis vectors and also for the fixed angles, in the radiograph case. We also call this 
;within the imaging loops now SOS. No more calcing the cam obsc ratios here, we do that using ratio1f, ratio2f. SO the current
;;procedure is for the vectors only. move calcing the fixed ratios to another functiuon



;130719 SOS this procedure must NOT contradict the calls to ratio1(or 2)f_function, INSIDE the nested imaging LOOPS! SOS!
;130719 This one does the same tyhing OUTSIDE the loops, by calling tan_PHI global calc and doing it beforehand!!! 
;SOS WE DID THE SAME THING F....ING TWICE!!! THIS ONE ALSO TAKES CARE OF THE VECTORS!!! SOS!!! 

;This is a code of line-of-sight (hereafter LOS) calculations of radiative transfer.
;It presumes that there is only emission and absorbtion along the LOS, not scattering in different directions (CHECK AGAIN REFS)
;This version is three dimensional (hereafter 3D).
;It works as follows: We  start at a point to the 'left' of a rectilinear grid, i.e. x=x0, y=y0, z=z0.
;Then, we define 2 desired tan(phi(i)f), i=1,2 as a target for the LOS.
;Then the code starts advancing in the discrete grid, either in x (r) (right) or in y (u) (up), or in z (c) (climb).
;There are two  if then else criteria:
;For tan(phi1):
;If current tan(phi(i))<tan(phi(1)f) then (i=1) step up (u), else step right (r) (SOS) (i=2:c-r, i=3:c-u).
;the 2nd angle is tans phi 2 =z/sqrt(x^2+y^2) if lt ratio2f then icrease z, else increase x or y. Which one, use again the 1st criterion to decide.
;always update the los arrays after every grid step.
;2 angles phi1, phi 2 like in spherical coordinates. Not a 3rd variable!
;This way, we get closer to the desired LOS, kind of 'asymptotically'.
; Carry on till you reach the ends of the grid. Do not forget to save your results.
;angles of LOS, in radians
;tan(phi1)=y/x, tan(phi2)=z/sqrt(x^2+y^2)
;
;****************************************************************
;200815 phi1 is azimuth from x to y, on xy plane
;200815 phi2 is elevation, from xy plane towards z

;phi1=0.590D
;phi2=0.20D

; 060815 a few lines above it shows the definition of phi1, phi2 in terms of (x,y,z). phi1
;lies on xy plane, while phi2 is on a
;plane perpendicular to the xy but at an angle phi1 
;(different anyway than xz plane) to the xz plane.


;251218 ANGLES GIVEN IN RADIANS 
;phi1=1.57D
;phi1=1.6D
;phi1=1.682D
phi2_fixed_fun=0.005D
phi2_fixed_fun=phi2_external_fun;d

;phi1=1.57D
;phi1=1.6D

phi1_fixed_fun=1.6814D
phi1_fixed_fun=phi1_external_fun;d


;******************************

;110719 perhaps this is an attempt to make an array? pointless after all? whatever!
if (focused_beam_switch_fun eq 0.0) then begin 
;130719 this is the sAME AS NEXT IF THEN ....
;MOVED TO THE NEXT IF THEN....



endif
;;SOS PUT an if in the following SOS 180419
;040719 there is also the switch: focused_beam_switch, 1 is camera obscura, either xz or yz, other (false) is radiogtaph. 
if (focused_beam_switch_fun ne 1) then begin
;130719 this one keep!



;SUPER SOS 311222 we alter sign of lxi's, 
;because LOS is APPROACHING, not receding! SOS!
;lx1_fun=cos(phi2_fixed_fun)*cos(phi1_fixed_fun)
;lx2_fun=cos(phi2_fixed_fun)*sin(phi1_fixed_fun)
;lx3_fun=sin(phi2_fixed_fun)

lx1_fun=-cos(phi2_fixed_fun)*cos(phi1_fixed_fun)
lx2_fun=-cos(phi2_fixed_fun)*sin(phi1_fixed_fun)
lx3_fun=-sin(phi2_fixed_fun)

endif

;170419 HERE we define phi1, phi2 for focused beam this should be a function 
;and previous def should also bew a fucntion and we should call the suitable one SOS

;define tans of angles array for 1. use with global calc of coslosu, etc and then for verification through comparison
;with ratio1f of a los within LOS loops. SOS do that verification! 


;190419 fixed means unfocused beam, radiograph. fixed are scalars, phi1,2 simply, are large 4D arrays.


; tanphi1,2 global calcs are initialized in the main part of code!
;SOS following tan functions better run just once? verify! 180419
if (focused_beam_switch_fun eq 1.0) then begin 
;130719 we only  keep this function for calcing the lxis vectors and also for the fixed angles, in the radiograph case. We also call this 
;within the imaging loops now SOS. No more calcing the cam obsc ratios here, we do that using ratio1f, ratio2f. SO the current
;;procedure is for the vectors only.UST BE CALLED AFTER THE RATIOSF SOS!!!
;130719 calls to these tan phi functions is redundant, since we already calc the ratios for cam obsc WITHIN the loops , once per LOS! AND IT WORKS !
;yet we may keep those tanphi global calcs, for future possible need! SOS!

;tan_phi1_fun=(tan_phi1_fun-tan_phi1_fun+tan(phi1_fixed_fun) )
;tan_phi2_fun=(tan_phi2_fun-tan_phi2_fun+tan(phi2_fixed_fun) )


if (imaging_geometry_selector eq 4) then begin

tan_phi1_fun=tanphi1_global_calc(tan_phi1_fun,focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,nx_1_fun,ny1_fun,nz1_fun)

tan_phi2_fun=tanphi2_global_calc(tan_phi2_fun,focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,nx_1_fun,ny1_fun,nz1_fun)
;
;lx1_fun=cos(phi2_fun)*cos(phi1_fun)
;lx2_fun=cos(phi2_fun)*sin(phi1_fun)
;lx3_fun=sin(phi2_fun)

endif

;170719 added nested ifs, in order to differentiate between XZ and YZ camera obscura cases! 
;SOS ALSO ADD THE XZ related new params to the mother procedures param list! ALSO to its call as well!
if (imaging_geometry_selector eq 3) then begin

tan_phi1_fun=tanphi1_global_calcXZ(tan_phi1_fun,focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun,nx_1_fun,ny1_fun,nz1_fun)

tan_phi2_fun=tanphi2_global_calcXZ(tan_phi2_fun,focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun,nx_1_fun,ny1_fun,nz1_fun)

endif
;110719 SOS we added this portion, else we do nothing here with ratios!
;130719 we altered tans to agree with above definitions!
ratio1f_comparison_fun=tan_phi1_fun
ratio2f_comparison_fun=tan_phi2_fun

;190419 now calculate the arrays of the angles
;;SOS! 091119 moved this inverse calc above!!! before the lxi's!!! Else non-local calc??
;;OK just commented out above lxi in -if calc!
phi1_fun=atan(tan_phi1_fun)
phi2_fun=atan(tan_phi2_fun)



;SUPER SOS 311222 we alter sign of lxi's, 
;because LOS is APPROACHING, not receding! SOS!
;lx1_fun=cos(phi2_fun)*cos(phi1_fun)
;lx2_fun=cos(phi2_fun)*sin(phi1_fun)
;lx3_fun=sin(phi2_fun)

lx1_fun=-cos(phi2_fun)*cos(phi1_fun)
lx2_fun=-cos(phi2_fun)*sin(phi1_fun)
lx3_fun=-sin(phi2_fun)

endif


;IF expression THEN BEGIN 
 ;  statements 
;ENDIF 


if (debug_comments_fun eq 1) then stop
;*****************************

end






pro los_reverse,in_reverse, taun_reverse,llos_2d_fun,lose_reverse,losk_reverse,endlosloop,loslength_fun,nz10_fun,second_coord_fun,lose_fun,losk_fun,nx_1_fun,ny1_fun,nz1_fun,ll_fun,endlosloop_fun,mikoscell_fun,debug_comments_fun
;;210819 This procedure integrates along the los, INVERSELY, i.e. from the scene towards the focal point (or from the scene towards the side of the plane, for radiograph; but it is meant for cam obscura geometries!). 
;210819 This inverse thing is required for ABSORPTION when doing cam obscura, because it is not symmetric, and time-dependent means different result for each way along the LOS. 
;;210819 For e.g., we meet first the cloud, then the void. The opposite gives different results when there is absoprtion! Absorption eats a given percentage, while emission adds its own thing! 
;;210819 Not symmetric in opposite ways absorption, let alone with time-dependency!
;;210819 For radiograph, there is symmetry, in the sense that the screen may be taken at the other end of the grid, and we may take the system rotated 180 degs around an axis, so we got a same result under those conditions. 
;210819 Anyhow, now we can opt for inverse pic, and also compare that to the 'straight' one!
;
;110819 ok now it works like a charm, in absence of absorption, eikona and eikona_reverse are almost identical, even with normal clight!
;
;;100819 CAREFUL now reverse pic seems to work, nut! endlosloop is around 60 percent of los length, then reverse only gets thet6ailofthelos, straight only gets the beginning half or so!
; 100819 need endlosloop  to exactly equal eachj los's cell count length, in order to preserve symmetry, when no absroption. 
;100819 ALSO: try testing WITH FAST clight=400 etc. ELSE, asymmetry occurs due to meeting earlier snapshots from one of from the other side of the grid!

;
;
;ll_fun,din_fun,dtaun_fun,tauncurrent_fun,in_fun,
;090819 why looptwice? maybe some careless double copy-paste? CAREFUL!!!
;**********************************************************************************************
;;260419 SOS edit this section of reverse LOS INTEGRATION and also edit the llos calculation SOS DO IT! reverse LOS self absorb. calc
;250419B SOS here we must put the option to INVERT the lose and losk vectors, in order to redo ze counter loop solving the eqn of rad transfer. 
;270419 do two functions a straight module and a reverse modlule. So we can select which one to use. 
;270419 here lies assymetry problem, cosing reverse image, without any k, to be dissimilar from straight! sos
lose_reverse=double(reverse(lose_fun))
losk_reverse=double(reverse(losk_fun))

;********************************************
;110819 los_sizy= exact length of current los, i.e. lose, losk
lose_sizy=size(lose_fun,/dimensions)
losk_sizy=size(losk_fun,/dimensions)

;110819 for some reason, it is an array of 1: We hereby select its sole element!
lose_sizy_scalar=lose_sizy(0)
losk_sizy_scalar=losk_sizy(0)

if (debug_comments_fun eq 1) then print,'lose length: lose_sizy,',lose_sizy,lose_sizy_scalar
if (debug_comments_fun eq 1) then print,'losk length: losk_sizy,losk_sizy_scalar (**should be same as lose_sizy and its scalar sibling!**)',losk_sizy,losk_sizy_scalar

;********************************************

if (debug_comments_fun eq 1) then print,'endlosloop',endlosloop
if (debug_comments_fun eq 1) then print,'lose_reverse,lose_fun',lose_reverse,lose_fun
if (debug_comments_fun eq 1) then print,'losk_reverse,losk_fun',losk_reverse,losk_fun

;define some local stuff 010619
;;090819 DONT need most of these, since we now pass them from mother pro!!!
incurrent=dblarr(nx_1_fun+ny1_fun+nz1_fun)
din=dblarr(nx_1_fun+ny1_fun+nz1_fun)
dtaun=dblarr(nx_1_fun+ny1_fun+nz1_fun+8)
tauncurrent=dblarr(nx_1_fun+ny1_fun+nz1_fun+8)


;270419 reset to zero in reverse
in_reverse=0.0D 
taun_reverse=0.0D
;270419 this one is an array to be zeroed
incurrent_reverse=incurrent*0.0D

;270419 Next, reversing ll! NO NO NO ! it MUST be reversed, cos ll is calced on a per cell basis, and it is along the original LOs. 
;Therefore, reversed ll, containing exact cell lengths along the LOS, must, must be reversed in order to obtain the correct llos's. (This one, just for the dims!)
;270419 update we now use ll(counter=1) all over, in both straight and reverse, thererefore this construct is only kept for future-proof reasons (easier upgrades)

;270419 SOS we define ll reverse all ones, cos ll is zero towards end, where no calcs take place, depending on zig zag definition. SOS
;else we get zeroes where it counts the mostr, ruining ze reverse calc!
ll_reverse=ll_fun*1.0D

;270419 set all elements of the din_reverse array to zero, whilst keeping its dimensions as an array correct
din_reverse=din*0.0D
;**********************************************************************************************
;270419 similarly, do define dtaun_reverse: get the size from the original then set it to zero all over, but still a vector.
dtaun_reverse=dtaun*0.0D
tauncurrent_reverse=tauncurrent*0.0D

;************************************************************************************************************************************
;for counter_reverse=1,counterlast(ny10,nz10) do begin


;110819 replaced fix(endlosloop_fun) with the exact los length lse, since that is the necessity for reversing symmetry when calcing opposite ways.
; 110819 Also, need a clight=LARGE, else no time symmetry either in reverse calc!
;110819 keep the old one as backup: for counter_reverse=0,fix(endlosloop_fun),1 do begin
for counter_reverse=0,(lose_sizy_scalar-1),1 do begin

;260419 SOS HERE BACKWARDS SOLVE EQUATION of rad transfer
;only matters when ABSORPTION is present
din_reverse(counter_reverse)=-losk_reverse(counter_reverse)*ll_reverse(counter_reverse)*in_reverse+lose_reverse(counter_reverse)*ll_reverse(counter_reverse)
if (debug_comments_fun eq 1) then print,'counter_reverse,din_reverse(counter_reverse)',counter_reverse,din_reverse(counter_reverse)
if (debug_comments_fun eq 1) then print,'losk_reverse(counter_reverse),ll_reverse(counter_reverse),lose_reverse(counter_reverse),lose_reverse(counter_reverse)*ll_reverse(counter_reverse)',losk_reverse(counter_reverse),ll_reverse(counter_reverse),lose_reverse(counter_reverse),lose_reverse(counter_reverse)*ll_reverse(counter_reverse)

dtaun_reverse(counter_reverse)=losk_reverse(counter_reverse)*ll_reverse(counter_reverse)*double(mikoscell_fun)

if (debug_comments_fun eq 1) then print, 'EDOORETASO2_reverse',lose_reverse(counter_reverse),losk_reverse(counter_reverse),ll_reverse(counter_reverse),mikoscell_fun

in_reverse=in_reverse+din_reverse(counter_reverse)
incurrent_reverse(counter_reverse)=in_reverse
taun_reverse=taun_reverse+dtaun_reverse(counter_reverse)
tauncurrent_reverse(counter_reverse)=taun_reverse
if (debug_comments_fun eq 1) then print, 'EDO TO IN_reverse,', in_reverse, din_reverse(counter_reverse), taun_reverse


endfor

;no need for loop, just do array-op! cant do for absorption so easily! We need to advance the calc along los vector!
;;290419 SOS cant adnvance calc along LOS like that, need to locate each calc, DONE THAT in stady state ROD test, take it from there!
;din_reverse=-losk_reverse*ll_reverse*in_reverse+lose_reverse*ll_reverse
;dtaun_reverse=losk_reverse*ll_reverse*mikoscell
;in_reverse=in_reverse+din_reverse(counter_reverse)
;
;************************************************************************************************************************************
;*****************************************************************
;270419 SOS do these for the reverse then inc em into ze functionalization schema
;270419 loslength is the same in reverse so no need to define a reverse on this one
;in_reverse=in_reverse
;if (debug_comments eq 1) then print, in, 'INEDOORESOS
;if (debug_comments eq 1) then print, taun_reverse

;****************************************************
;090819 from main body: 
;loslength(second_coord,nz10)=(counterlast(second_coord,nz10)/llos)
;;llos=lloscurrent
;in=in/loslength(second_coord,nz10)
;****************************************************


;no need to mess with counterlast, cos the size of reverses is determined by counterlast 
;counterlast(ny10,nz10)=lastcounter

;270919 SOS LITMUS TEST IF ALL WORK OK AND THEY DONT!
;SOS 270419 here lies asymmetry SOS find out why!
;DO IT MANUAALYY!
;print, incurrent/reverse(incurrent_reverse)
;print, incurrent-reverse(incurrent_reverse)
;print, incurrent, incurrent_reverse
;270919 yet this works ok all zeros gives!
;print, lose-reverse(lose_reverse)
;;270419 HEY! the following works!!!!! this should do it FINALLY ! SOLUTION for non abs! look at it again!
;print, lose_Reverse*ll_reverse-reverse(lose*ll)
;*****************************************************************

;*****************************************************************
;270419 SOS do these for the reverse then inc em into ze functionalization schema
;270419 loslength_fun is the same in reverse so no need to define a reverse on this one
;in_reverse=in_reverse
;if (debug_comments eq 1) then print, in, 'INEDOORESOS
;if (debug_comments eq 1) then print, taun_reverse

;no need to mess with counterlast, cos the size of reverses is determined by counterlast 
;counterlast(ny10,nz10)=lastcounter

;270919 SOS LITMUS TEST IF ALL WORK OK AND THEY DONT!
;SOS 270419 here lies asymmetry SOS find out why!
;DO IT MANUAALYY!
;print, incurrent/reverse(incurrent_reverse)
;print, incurrent-reverse(incurrent_reverse)
;print, incurrent, incurrent_reverse
;270919 yet this works ok all zeros gives!
;print, lose-reverse(lose_reverse)
;;270419 HEY! the following works!!!!! this should do it FINALLY ! SOLUTION for non abs! look at it again!
;print, lose_Reverse*ll_reverse-reverse(lose*ll)
;*****************************************************************
END










;090722 SOS added bfield to this big pro, in order to use it in coslosb calculation. losloop calc global_calcs, which in turn calls coslosb
pro losloop_fun,loslength_fun ,path_algo_set,llos_2d_fun,stagnant_signal_fun,stagnant_signal2_fun,lloscurrent_fun, t0los_fun,imaging_geometry_selector_fun, backwards_in_time_fun,los_origin_x,los_origin_y,los_origin_z,first_coord_fun,second_coord_fun,third_coord_fun,nx2_fun,nx3_fun,shotind_fun,tan_phi1_fun,tan_phi2_fun,error_stagnant_los_fun,lostdepth_fun,lose_fun,losk_fun,losdraw_fun, RHO_INDICATOR_VALUE_fun, nx10_fun, ny10_fun,nz10_fun, rho_indicator_fun, rho_indicator_total_fun, use_huge_indicator_array_fun, counterlast_fun, taun_fun,mikoscell_fun,in_fun,ll_fun, nx1currentlast_fun, ny1currentlast_fun, nz1currentlast_fun,debug_comments_fun, plutocelllength_fun,lcell_fun,shotnumber_fun,clight_fun, emiss_4d_fun,kabs_4d_fun, nx_1_fun,nx1_fun,ny1_fun,nz1_fun,ratio1f_fun,ratio2f_fun,cc_fun,rc_fun,uc_fun,shotmin_fun,shotmax_fun,t_fun,shotnumber_tpicked_fun,endlosloop_fun, nx1current_fun,ny1current_fun,nz1current_fun, focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,dlr_fun,dlc_fun,dlu_fun,focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun, bx1_4d, bx2_4d, bx3_4d 
;210819 This big pro does the LOS integration, along the ...LOS! It calls other routines as well. Lloscurrent for current los length, time location, and also the pathfinder algos, depending on the geometry case employed from the external param file.


;pro los_reverse,loslength_fun,nz10_fun,second_coord_fun,lose_fun,losk_fun,nx_1_fun,ny1_fun,nz1_fun,ll_fun,endlosloop_fun,mikoscell_fun,debug_comments_fun,eikona_fun, eikonatau_fun,eikona_reverse_unlengthed_fun,eikona_reverse_fun,eikonatau_reverse_fun

;
;020719 must now use the results, i.e. assign nx1current, ny1current, nz1current, from los_origins call, within 2D imaging loop, and this one procedure
;is also called within the 2D imaging loop.
;losloop_fun,  los_origin_x,los_origin_y,los_origin_z,first_coord,second_coord,third_coord, nx2,nx3,shotind ,tan_phi1 ,tan_phi2 ,error_stagnant_los ,lostdepth ,lose ,losk ,losdraw ,lastcounter , RHO_INDICATOR_VALUE , nx10 , ny10 ,nz10 , eikona , eikonatau , eikona_reverse , eikona_reverse_unlengthed , eikonatau_reverse , rho_indicator , rho_indicator_total , use_huge_indicator_array , counterlast , taun ,mikoscell ,in ,ll , nx1currentlast , ny1currentlast , nz1currentlast ,debug_comments , plutocelllength ,lcell ,shotnumber ,clight , emiss_4d ,kabs_4d , nx_1 ,nx1 ,ny1 ,nz1 ,ratio1f ,ratio2f ,cc ,rc ,uc ,shotmin ,shotmax ,t ,shotnumber_tpicked ,endlosloop , nx1current ,ny1current ,nz1current , focal_point_x ,focal_point_y ,focal_point_z ,dlr ,dlc ,dlu 


;
;;060819 SOS ERROR here! these initializations are ok for 
;initialize current (as opposed to final) 'up' step number, to one
uc_fun=1
; current 'right' step counter begins at 1, i.e. one first step right, saves div by 0! We presume that it is intrinsically
;accounted for in the loops later on in the code.
rc_fun=1
;similar for current 'climb' counter, z-dimension. (NOTE: This could be made to begin at zero, had we wished to do so.(SOS: AND HAD ACCOUNTED FOR IT IN THE LOOPS)
cc_fun=0
;Initialize current ratio of up to right steps, at zero (i.e. (uc/rc)=0).
;This way, 2nd step is always up (first was right), as it has to increase ratioc up from zero, in the if construct.
;similarly for second tan(phi2) ratio2c
ratio1c=0.0
ratio2c=0.0

;
;





;; HERE SET t0LOS when the LOS begins (time of begining snapshot sos).  SHOULD DO IT SYSTEMTICALLY
;;160719 drop it, messed the calc of llos! 
;t0los_fun=0.0D
;200419 counter set to 1.0 here, else ti starts at 52 or 55, no matter what! crazy!
counter=1.0
;
;
;array of dIn, voxel intensities
incurrent=dblarr(nx_1_fun+ny1_fun+nz1_fun)

din=dblarr(nx_1_fun+ny1_fun+nz1_fun)
dtaun=dblarr(nx_1_fun+ny1_fun+nz1_fun)
tauncurrent=dblarr(nx_1_fun+ny1_fun+nz1_fun)

;200719 set stagnant signal of the current los to zero
;200719 BEFORE THE LOOP U BLOODY i...ot!
stagnant_signal_fun(second_coord_fun,nz10_fun)=0.0
stagnant_signal2_fun(second_coord_fun,nz10_fun)=0.0
;180719 set counter to begin from 2. else counter goes to zero at first step and inf  emerges
for counter=2, fix(endlosloop_fun),2  do begin

if (debug_comments_fun eq 1) then print,'in-los-loop SCREEN POINT FORCAMOBSC,OR ELSE RADIOGR. LOSORIGIN: second_coord_fun,nz10_fun',second_coord_fun,nz10_fun


 
testone=1.0D

if (debug_comments_fun eq 1) then print, 'counter, endlosloop, fix(endlosloop)',counter, endlosloop_fun, fix(endlosloop_fun)


if (debug_comments_fun eq 1) then print, 'counter arxi inner loop ', counter,' tsa'
;sqrt(nx_1*nx_1 + ny1*ny1 + nz1*nz1) + 1

;print, 'ok os edo 3'
;model grid size:x,y,zprint, za1, za2, za3, za4, za5, za6, rz, ((xind-x1)^2+(yind-y1)^2)




;********************************************************************************************
;200419 HAD TO BRING EARLIER, STILL WITHIN LOS LOOP, THIS STUFF, since it calcs temporal stuff, in order to 
;index the -now 4D- angles' arrays.




;********************************************************************************************************

;determine time shot
;dtllos=(dl/clight)
;080419 edited llos to FPoint beginning
;lloscurrent=sqrt( ((dlc_fun*(nz1current_fun-focal_point_z_fun))^2)+((dlu_fun*(ny1current_fun-focal_point_y_fun))^2)+((dlr_fun*(nx1current_fun-focal_point_x_fun))^2) )
;lloscurrent=double(lloscurrent)

;function lloscurrent_function, dlr_fun,dlu_fun,dlc_fun,nx1current_fun,ny1current_fun,nz1current_fun,los_origin_x_fun,los_origin_y_fun,los_origin_z_fun
;y,los_origin_z

lloscurrent_fun=lloscurrent_function(debug_comments_fun, dlr_fun,dlu_fun,dlc_fun,nx1current_fun,ny1current_fun,nz1current_fun,los_origin_x,los_origin_y,los_origin_z)
;200719 we add a total geometric los length image here!
llos_2d_fun(second_coord_fun,nz10_fun)=lloscurrent_fun
if (debug_comments_fun eq 1) then print, lloscurrent_fun, 'proto deytero trito meros entos rizas llos', ((dlc_fun*(nz1current_fun-focal_point_x_fun))^2),((dlu_fun*(ny1current_fun-focal_point_y_fun))^2), ((dlr_fun*(nx1current_fun-focal_point_x_fun))^2) 


;********************************************************
;060719 SOS here do call the time marching procedure SOS!
;160719 we added t0los_fun to the call list, as it is nested, and t0los to the mother pro. Else it misses the t0los param insode time_location!
 time_location, backwards_in_time_fun,debug_comments_fun,shotnumber_tpicked_fun,t_fun,curtime,t0LOS_fun,lloscurrent_fun,clight_fun,count,shotnumber_fun,shotmin_fun,shotmax_fun,count


;if (debug_comments_fun eq 1) then print,'(sqrt(nx_1*nx_1 + ny1*ny1 + nz1*nz1) + 1), shotnumber, COUNER WITHIN LOOP', (sqrt(nx_1_fun*nx_1_fun + ny1_fun*ny1_fun + nz1_fun*nz1_fun) + 1), shotnumber_fun,shotnumber_tpicked_fun - ( size(t, /n_elements)-count-shotmin_fun  ), counter

;060719SOS the following should be kept in non-fun mode during the function call!  t0LOS,lloscurrent,curtime,count 
;;they are non- _fun in the current procedure's main body! 
;REST ARE IN FUN MODE, cos this is nested inside another procedure, where s..t is already _funned!

;ELSE we must go into double mode: _fun_fun frothose that are from the main code and for single _fun for tyhose local to the mother procedure


;********************************************************














;print, 'tsa1', counter
;temp decr counter to do 1st grid step
counter=counter-1
if (debug_comments_fun eq 1) then print, 'counter after -1 ',counter
;print, 'tsa2', counter
ratio1c=(double(uc_fun)/double(rc_fun))
 if (debug_comments_fun eq 1) then print,'early on in LOS-procedure: ratio1c,uc_fun,rc_fun,double(uc_fun)/double(rc_fun)',ratio1c,uc_fun,rc_fun,double(uc_fun)/double(rc_fun)

; HERE MUST RESET rc,uc,cc to initial values,
;else they retain old values, before breaking out of the inner loop,
;and they mess our loops


a1=double(cc_fun)
b1=double(rc_fun)
c1=double(uc_fun)
d1=sqrt(b1*b1+c1*c1)
e1=a1/d1
ratio2c=double(e1)



 ;********************************************************************************************************************************************

if ((imaging_geometry_selector_fun eq 1) or (imaging_geometry_selector_fun eq 2)) then begin

pathfinder_algo_for_ratio1_radiograph_case_1_and_2, ratio1c,ratio1f_fun,uc_fun,rc_fun,debug_comments_fun
;240719 A function for radiograph case 1 or 2. Apparently we had no decreasing at any axis here! 

 endif
 ;********************************************************************************************************************************************



 ;********************************************************************************************************************************************
 if (imaging_geometry_selector_fun eq 3) then begin
;240719 We now call a function for pathfinding!
pathfinder_algo_and_stagnation_detector_for_ratio1_camera_obscura_case_3,ratio1c,ratio1f_fun,debug_comments_fun,rc_fun,uc_fun,stagnant_signal_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun
endif 
 ;********************************************************************************************************************************************
 
;150419 want no stagnant los!

;200719 we added a new array element here, in order to have stagnation data for each los separately(forming a stagnation image!)
;230719 we changed gt to ge here. Hope this does not mess with case 4. (Now working on case 3, XZ cam obsc!)

;230719stagnation count for case 4 only

 ;********************************************************************************************************************************************

if (imaging_geometry_selector_fun eq 4) then begin

  ;260719 path algo set =0 means use the standard pathfinder algo for this code version
  if (path_algo_set eq 0) then begin 
   pathfinder_algo_and_stagnation_detector_for_ratio1_camera_obscura_case_4,ratio1c,ratio1f_fun,debug_comments_fun,rc_fun,uc_fun,stagnant_signal_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun
  endif

  ;260719 path algo set=1 brings up the algos from last autonomous case 4 cam obsc YZ working version
  if (path_algo_set eq 1) then begin 
   OLD_ONE_pathfinder_algo_and_stagnation_detector_for_ratio1_camera_obscura_case_4,ratio1c,ratio1f_fun,debug_comments_fun,rc_fun,uc_fun,stagnant_signal_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun
  endif

endif
;
 ;********************************************************************************************************************************************






;100719 RADIOGRAPH OLD ONE first one ORIGINAL: if (ratio1c lt ratio1f) then (uc=uc+1)  else (rc=rc+1)

;08419 NEXT: if outside screen FOV in terms of phi1, then move a step right, towards intercepting the FOV cone.


;This was WRONG!! We must divide and then limit it CUT it use approximate ll =1.5 times
;cell side length and assume equal cell sizes x, y, z 
;if (ratio1c lt  ratio1f) then (ll(counter)=dlu*sf1*cf2)  else (ll(counter)=dlr*cf1*cf2)
ll_fun(counter)=1.0D

ratio1c=(double(uc_fun)/double(rc_fun))
if (debug_comments_fun eq 1) then print, ' REENACTED THAT FOR LARGE ANGLES 140818', rc_fun, uc_fun, cc_fun, ratio1c, ratio2c, counter, ll_fun(counter)
if (debug_comments_fun eq 1) then print, 'ratio1c,ratio1f,ratio2c,ratio2f,uc,uc+fix((ratio1f-ratio1c)/abs(ratio1f-ratio1c)),(ratio1f-ratio1c),abs(ratio1f-ratio1c),fix((ratio1f-ratio1c)/abs(ratio1f-ratio1c))',ratio1c,ratio1f_fun,ratio2c,ratio2f_fun,uc_fun,uc_fun+fix((ratio1f_fun-ratio1c)/abs(ratio1f_fun-ratio1c)),(ratio1f_fun-ratio1c),abs(ratio1f_fun-ratio1c),fix((ratio1f_fun-ratio1c)/abs(ratio1f_fun-ratio1c))
;if (debug_comments_fun eq 1) then print, 'local tanphi1, tanphi2 compare to ratio1c, ratio2c current not f! (should be same)!', tan_phi1_fun(shotnumber_fun,nx1current_fun,ny1current_fun,nz1current_fun),tan_phi2_fun(shotnumber_fun,nx1current_fun,ny1current_fun,nz1current_fun)
;update LOS arrays.
;Basically, every time we advance one grid point, we update the LOS arrays,
;in order to properly keep track of all LOS points.
;So we do this twice, as we have two ifs, each of which definately advances the LOS by exactly one grid point.

;find where we are in the 3D grid
;;020719 now alter this one to work with los_origins! sos do it!
;calculate current x-position in the grid
nx1current_fun=los_origin_x+rc_fun
;same for current y-pos in the 3D grid
ny1current_fun=los_origin_y+uc_fun
;same for z
nz1current_fun=los_origin_z+cc_fun

if (debug_comments_fun eq 1) then print,'los_origin_x,los_origin_y,los_origin_z',los_origin_x,los_origin_y,los_origin_z

if( (debug_comments_fun eq 1)  and (imaging_geometry_selector_fun eq 4.0)) then print,'works for imaging case 4: focal_point_x_fun,nz10_fun,second_coord_fun',focal_point_x_fun,nz10_fun,second_coord_fun

if( (debug_comments_fun eq 1)  and (imaging_geometry_selector_fun eq 3.0))then print,'works for imaging case 3: focal_pointXZ_y_fun,focal_pointXZ_x_fun,focal_pointXZ_z_fun,nz10_fun,first_coord_fun,second_coord_fun,',focal_pointXZ_y_fun,focal_pointXZ_x_fun,focal_pointXZ_z_fun,nz10_fun,second_coord_fun

if (debug_comments_fun eq 1) then print,'nx1current,focal_point_x,rc,focal_point_x_fun+rc_fun',nx1current_fun,focal_point_x_fun,rc_fun,focal_point_x_fun+rc_fun
if (debug_comments_fun eq 1) then print,'ny1current,focal_point_y,uc,foal_point_y_fun+uc_fun',ny1current_fun,focal_point_y_fun,uc_fun,focal_point_y_fun+uc_fun
if (debug_comments_fun eq 1) then print,'nz1current,focal_point_z,ccfocal_point_z_fun+cc_fun',nz1current_fun,focal_point_z_fun,cc_fun,focal_point_z_fun+cc_fun
; edo repeat the calc of angle and dboosting check

; 060515 we finally add aberration stuff here for rlos

;declare vars for calculating angle between LOS direction in 3D space and
;local velocity direction.

;080419 edited those to FPoint's ones, new beginning of LOS!
;020719 these now point to the more general los origins, aimed to work with all problem geometries.
xx0=los_origin_x
yy0=los_origin_y
zz0=los_origin_z


;we assign the following, in order to avoid confusions from using x,y,z
;which are very simple and might have been used elsewere in the code
;xx=x
;yy=y
;zz=z
;SUPER 25-05-15
;DIORTHOSI xx=xx0+rc, klp y, z. OSTE x-x0=rc=Dx. Allios bgazei o,ti na'nai arnitika
; SIMEIOSEIS LATHOS TO EIXAN 
xx=rc_fun+xx0
yy=uc_fun+yy0
zz=cc_fun+zz0
;SOS OS EDO 240519 SOS
; 040815 WE m
;just implement frequency shift, aka 1st model of hj88 from back in the day
;in order to achieve that, we first need to re-implement 
;the radio emission stuff from paper1, e.g. thermal 


; 040815 moved this bit a tad earlier in order to calculate shotnumber before 
;ze assignements of dopplerboosting related stuff 


;080915 check length llos units, and time units vs sfactor etc
if (debug_comments_fun eq 1) then print, clight_fun, shotnumber_fun, curtime,lloscurrent_fun, 'tsikabum',counter, nx10_fun,nz10_fun,nx1current_fun,ny1current_fun,nz1current_fun, plutocelllength_fun, lcell_fun
; factors, factort
if (debug_comments_fun eq 1) then print, 'shotind in curtime definition,curtime,shotnumber',shotind_fun,curtime,shotnumber_fun
if (debug_comments_fun eq 1) then print, 'los time  ', 't0los',lloscurrent_fun/clight_fun,t0LOS_fun
if (debug_comments_fun eq 1) then print, 'counter ',counter
if (debug_comments_fun eq 1) then print, 'nx10 ny10 nz10 ',nx10_fun, ny10_fun,nz10_fun
if (debug_comments_fun eq 1) then print, 'now are these the screen coords? or radiograph coords (imaging loop indices)?:first_coord_fun, second_coord_fun,nz10_fun ',first_coord_fun, second_coord_fun,nz10_fun

if (debug_comments_fun eq 1) then print, 'nx1 nx_1 nx2 nx3 ',nx1_fun,nx_1_fun, nx2_fun,nx3_fun
if (debug_comments_fun eq 1) then print, 'nx1current ny1current nz1current ', nx1current_fun, ny1current_fun, nz1current_fun
if (debug_comments_fun eq 1) then print, 'focal_point_x focal_point_y focal_point_z  ', focal_point_x_fun, focal_point_y_fun, focal_point_z_fun
if ((debug_comments_fun eq 1) and ( (imaging_geometry_selector_fun eq 3) )) then print, 'lloscurrent cam obsc XZ calced', sqrt( ((dlc_fun*(nz1current_fun-los_origin_z))^2)+((dlu_fun*(ny1current_fun-los_origin_y))^2)+((dlr_fun*(nx1current_fun-los_origin_x))^2) )
if ((debug_comments_fun eq 1) and ( (imaging_geometry_selector_fun eq 4) )) then print, 'lloscurrent calced', sqrt( ((dlc_fun*(nz1current_fun-focal_point_z_fun))^2)+((dlu_fun*(ny1current_fun-focal_point_y_fun))^2)+((dlr_fun*(nx1current_fun-focal_point_x_fun))^2) )




;llos=sqrt( ((dlc*(nz1currentlast-los_origin_x))^2)+((dlu*(ny1currentlast-los_origin_y))^2)+((dlr*(nx1currentlast-los_origin_z))^2) )



;We now select and assign the suitable values of time tagged velocity
;(kind of 4D, probably not in the usual rel ativistic 4D sense)
;from the 4D array to the convenience vars ux,uy,uz
;in order to facilitate the (los,u) angle cos calculation
;We made sure this comes AFTER assignement of nx(i)current above.
;Else it doesn't work correctly!!

;ux=vx1_4d(shotnumber,nx1current,ny1current,nz1current)
;uy=vx2_4d(shotnumber,nx1current,ny1current,nz1current)
;uz=vx3_4d(shotnumber,nx1current,ny1current,nz1current)



;The following formula is used, from analytic geometry
;cos(los,u)=A/B, where
;A=[(xx-xx0)*ux+(yy-yy0)*uy+(zz-zz0)*uz]
;B=sqrt[(xx-xx0)^2 +(yy-yy0)^2 +(zz-zz0)^2 ]*sqrt[ux^2+uy^2+uz^2]

;aaa=(xx-xx0)*ux+(yy-yy0)*uy+(zz-zz0)*uz
;bbb=sqrt((xx-xx0)*(xx-xx0) +(yy-yy0)*(yy-yy0) +(zz-zz0)*(zz-zz0) )
;ccc=sqrt(ux*ux+uy*uy+uz*uz)
; 030815 this algo siimply calculates the coslosu angle according tocurent position
;along the los it does not use the original los aiming angle.
;it merly relies on the los aiming algo to do its job and then picks up on the latter's 
;result 
;coslosu=aaa/(bbb*ccc)
;310715 we define theta and also doppler boosting andf rel. beaming stuff
;also lorenrz factor
;bear in mind need to recheck left right oriented 3d xyz systems.
;thetau=acos(coslosu)
; DOUBLE CHECK IF ccc is indeed the relativistic velocity over c

; 220815 we commented out these two lines since we now use array operations
;gammalorentz(shotnumber,nx1current,ny1current,nz1current)=1/(sqrt(1-ccc*ccc))
;dopplerfactor(shotnumber,nx1current,ny1current,nz1current)=(sqrt(1-ccc*ccc))/(1-ccc*coslosu)
; check the above functions


;frequency

;SUPER 060815 here is the observing frequency
;consider moving it to a file ot to the beginning of the code,
;along with other params.
;in the relativistic version of the code
;where dopplerfactor is employed, ndash=nobs/dopplerfactor SOS
; set it up to change for each cell according to cell's 
;local dopplerfactor

;ng(shotnumber,nx1current,ny1current,nz1current)=nobs*(1/dopplerfactor(shotnumber,nx1current,ny1current,nz1current))
;ntesty=nobs*(1/dopplerfactor(shotnumber,nx1current,ny1current,nz1current))
;220815 we device a little test here in order to compare the array with the loop result for frequency SOS
;print, 'freq ntesty ',ng(shotnumber,nx1current,ny1current,nz1current),'dopplerfactor',dopplerfactor(shotnumber,nx1current,ny1current,nz1current), ntesty

;SUPER 070815 THE FOLLOWING DO WORK WE SET THEM OFF TEMPORARILY FOR EFFICIENCY
;WHILE DEVELOPING IT IS A SWITCH NOTHING MORE
;CASE emisselect OF 
;  1: PRINT, 'one' 
;  2: PRINT, 'two' 
;  3: PRINT, 'three' 
;  4: PRINT, 'four' 
;ENDCASE 
;kabs_4d(shotnumber,nx1current,ny1current,nz1current)=0.018*gaunt(shotnumber,nx1current,ny1current,nz1current)*RHO_4d(shotnumber,nx1current,ny1current,nz1current)*RHO_4d(shotnumber,nx1current,ny1current,nz1current)*(1/ng(shotnumber,nx1current,ny1current,nz1current))*(1/ng(shotnumber,nx1current,ny1current,nz1current))*(Temp_4d(shotnumber,nx1current,ny1current,nz1current)^(-1.5))*(100.0/NLONG)*(1.0e+14)*(xfrac(shotnumber,nx1current,ny1current,nz1current)^2)
;kabs2_4d=0.08235*RHO_4d*RHO_4d*(T^(-1.35))*0.01*(((Temp_4d(shotnumber,nx1current,ny1current,nz1current)/1.0e+8)>1.0)^2)*(100.0/NLONG)

; 070815 ALSO emiss have to be implemented 
;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.)
;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.)

;OR WAIT TILL WE DO  IT IN NEW GEOMETRIC FASHION USING
;RIGINAL DIRECTION ANGLES INSTEAD OF CURRENT LOS
;ANGLES 

;print, 'aaa,bbb,ccc',aaa,bbb,ccc
;print, 'edo ta ux,uy,uz,(xx-xx0),(yy-yy0),(zz-zz0),coslosu, gonia (los,u);
;print, ux,uy,uz,(xx-xx0),(yy-yy0),(zz-zz0), coslosu,acos(coslosu), thetau
;print,'xx,yy,zz,xx0,yy0,zz0', xx, yy, zz, xx0, yy0, zz0
; NA SYNEXISTEI!! 060515 OS EDO CODED
if (debug_comments_fun eq 1) then print,'nx10,ny10,nz10', nx10_fun,ny10_fun,nz10_fun
;print, 'ux^2,uy^2,uz^2',ux*ux,uy*uy,uz*uz
if (debug_comments_fun eq 1) then print, ' mipos EINAI ARISTEROSTROFO to XYZ, px tis LOS? CHECK IN PRACTICE AGAIN SOS'
if (debug_comments_fun eq 1) then print,'210515'
;print, 'gammalorentz',gammalorentz(shotnumber,nx1current,ny1current,nz1current)
;print, 'dopplerfactor',dopplerfactor(shotnumber,nx1current,ny1current,nz1current)
if (debug_comments_fun eq 1) then print, 'el stoppo 190515'
if (debug_comments_fun eq 1) then print, ' shotind vs shotnumber 020815 check before adding dopplerboosting factor SOS;
;SUPER 030815 fix shotnumber in lieu of shotind in recent dboosting and gamalorentz calculations 
;directly above DO NOT FORGET IT SOS
;test also by altering phi1, phi2 
; 030815try gathering althose params into one place with corresponding descriptions do it as welll.

;stop
; WE MAY REPEAT shotnumber check once more at second in-loop increment, for improved time -accuracy SOS
;140515 we shall not repeat shotnumber check since shots are
;much fewer than los voxels.

;IMPORTANT: FOLLOWING STUFF MUST GO AFTER INITIAL USE OF nx1current, ny1current, nz1current
;SUPERSOS
;next comes ALGO to find snapshot position, given current time curtime
;to be put suitably inside ze code, at algo location SOS


;**************************************************************************
;;251218 this portion of comments has the test of the time-tracking algorithm, used to calc current 
;time along the LOS, called curtime.
;
;
;next comes ALGO to find snapshot position, given current time curtime
;to be put suitably inside ze code, at algo location SOS
;curtime=40
;print, where((curtime-t)<0,count)
;print, size(t, /n_elements)
;next is snapshot number, give or take 1, for a current time of curtime.
;print, size(t, /n_elements)-count
;****************************************************************


;***********************************************************************************
;***********************************************************************************

;***********************************************************************************
;***********************************************************************************




;190419 assignements now differ? nx1current etch
;now that we know where we are in the 3D grid, assign to the current los point, the e and k values
;from the corresponding point of the 3D grid
if (debug_comments_fun eq 1) then print,los_origin_x,los_origin_y,los_origin_z
lose_fun(counter)=double(emiss_4d_fun(shotnumber_fun,nx1current_fun,ny1current_fun,nz1current_fun))
;using ratio1, ratio2 instead of coslosu
;lose(counter)=emiss_4d(shotnumber,nx1current,ny1current,nz1current)*((dopplerfactor(shotnumber,nx1current,ny1current,nz1current))^(2+alphaindex))
; k must be between 0 and 1, as it is an abs coefficient sosara!
losk_fun(counter)=double(kabs_4d_fun(shotnumber_fun,nx1current_fun,ny1current_fun,nz1current_fun))
losdraw_fun(nx1current_fun,ny1current_fun,nz1current_fun)=1.0D
;dIn=-kn*dl+en
din(counter)=-double(losk_fun(counter))*double(ll_fun(counter))*double(in_fun)+double(lose_fun(counter))*double(ll_fun(counter))
;in cgs the length!!!
; 301217 we now multiply dlu,
;  dlr, dlc by sfactor, in order to make up for less and larger cells when 
;shrinking the grid. But these do NOT affect the following mikoscell_fun etc. mikoscell_fun is set to 1 earlier, 
; no duplicate sfactor there.

dtaun(counter)=double(losk_fun(counter))*double(ll_fun(counter))*double(mikoscell_fun)
if (debug_comments_fun eq 1) then print, 'EDOORETASO',lose_fun(counter),losk_fun(counter),ll_fun(counter),mikoscell_fun
in_fun=double(in_fun)+double(din(counter))
incurrent(counter)=double(in_fun)
taun_fun=double(taun_fun)+double(dtaun(counter))
tauncurrent(counter)=double(taun_fun)

;print, rc, uc, cc, ratio1c, ratio2c
;exit loop on reaching either end of 3D grid


;250419 SOS these breaks NEED EDITING FOR THE FOCAL POINT THING SOS! they are wrong now!
;;250419 we edited them so that the case of neg cc, uc, is covered. (no neg rc, still we alao covered that as well.)
;print, 'PTPTPTP', 'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc

;010619 had to set to minus 2 ze margins below for break, miss some detail, yet more stable! wonder why now pop up this problem!

;020719 sos replaced with los origins the focsal point coords!
if (debug_comments_fun eq 1) then print,'made it to no 0!','rc_fun,los_origin_x,(abs(nx_1_fun)-2.0)',rc_fun,los_origin_x,(abs(nx_1_fun)-2.0)
if ( (rc_fun+los_origin_x) gt (abs(nx_1_fun)-2.0) ) then break
if (debug_comments_fun eq 1) then print,'made it to no 0.5!','rc_fun,los_origin_x,(abs(nx_1_fun)-2.0)',rc_fun,los_origin_x,(abs(nx_1_fun)-2.0)

;070819 what if rc_fun is below los origin x, for xz imaging?  WTF HERE?
if ( (rc_fun+los_origin_x) lt 1.0 ) then break
if (debug_comments_fun eq 1) then print,'made it to no1!'
;print, 'ssstdfklhv', 'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx_1-nx10)),'rc',rc,'uc',uc,'cc',cc
if ( (uc_fun+los_origin_y) gt (abs(ny1_fun) -2.0) ) then break
if (debug_comments_fun eq 1) then print,'made it to no2_A!,uc_fun,los_origin_y,ny1_fun,(uc_fun+los_origin_y),abs(ny1_fun) -2.0,( (uc_fun+los_origin_y) gt (abs(ny1_fun) -2.0) )',uc_fun,los_origin_y,ny1_fun,(uc_fun+los_origin_y),abs(ny1_fun) -2.0,( (uc_fun+los_origin_y) gt (abs(ny1_fun) -2.0) )

if (debug_comments_fun eq 1) then print,'uc_fun,los_origin_y, ( (uc_fun+los_origin_y) lt 1.0 )',uc_fun,los_origin_y, ( (uc_fun+los_origin_y) lt 1.0 )

;070819 SOS this break criterion is totally bogus! it does not allow progress below zero!!! quax quax quax!!! CORRECT IT! 
;0708190 no it is fine! the pathfinder has a problem, the above sum cant go neg! in xz imaging,cant go behind xz side of box! why is uc_fun=-1 at start?
if ( (uc_fun+los_origin_y) lt 1.0 ) then break
;if ( (uc_fun+los_origin_y) lt ny1_fun- ) then break
if (debug_comments_fun eq 1) then print,'made it to no2_B!,uc_fun,los_origin_y, ( (uc_fun+los_origin_y) lt 1.0 )',uc_fun,los_origin_y, ( (uc_fun+los_origin_y) lt 1.0 )

;print, 'TTTTTT',  'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc
if ( (cc_fun+los_origin_z) gt (abs(nz1_fun) -2.0) ) then break
if ( (cc_fun+los_origin_z) lt 1.0 ) then break
if (debug_comments_fun eq 1) then print,'made it to no3!'

;print, 'BINNN',  'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc
;restore counter to +2 value for second increment of grid. This way, we got all two grid
;increments recorded in counter.
;print, 'tsa3', counter
counter=counter +1
if (debug_comments_fun eq 1) then print, 'counter meta to syn 1 ', counter 
;print, 'tsa4', counter

;***080419 this twin if setup allows for negative ratio2f, aka tan of phi2!
;ntio2c lt ratio2f) then (cc=cc+1)  else  (cc=cc-1)
;***08419 NEXT: if outside screen FOV in terms of phi2, then move a step right, towards intercepting the FOV cone.
;if (abs(ratio1c) gt abs(ratio1f)) then (rc=rc+1) else (  uc=uc+fix((ratio1f-ratio1c)/abs(ratio1f-ratio1c))  ) 
;130419 CAREFUL the added 0.01 at the DENOM prevents LOS from getting STUICK at same place for a given combo of numbers !
;only for ratio1 not here SOS! 

;110719 in case of cam obsc then do complicated pathfinding algo!



if  (imaging_geometry_selector_fun eq 4) then begin
;
  ;260719 path algo set =0 means use the standard pathfinder algo for this code version
  if (path_algo_set eq 0) then begin 
   pathfinder_algo_and_stagnation_detector_for_ratio2_camera_obscura_case_4,ratio2c,ratio2f_fun,rc_fun,cc_fun,debug_comments_fun,ratio1f_fun,ratio1c,stagnant_signal2_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun
  endif

  ;260719 path algo set=1 brings up the algos from last autonomous case 4 cam obsc YZ working version
  if (path_algo_set eq 1) then begin 
   OLD_ONE_pathfinder_algo_and_stagnation_detector_for_ratio2_camera_obscura_case_4,ratio2c,ratio2f_fun,rc_fun,cc_fun,debug_comments_fun,ratio1f_fun,ratio1c,stagnant_signal2_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun
  endif

endif


;230719 A separate one for the XZ camera obscura case! Z below centre give zero now! FIX IT!
if  (imaging_geometry_selector_fun eq 3) then begin

pathfinder_algo_and_stagnation_detector_for_ratio2_camera_obscura_case_3,uc_fun,ratio2c,ratio2f_fun,rc_fun,cc_fun,debug_comments_fun,ratio1f_fun,ratio1c,stagnant_signal2_fun,error_stagnant_los_fun,second_coord_fun,nz10_fun

endif




;110719 in case of radiograph, use old one algo, simpler!
if ((imaging_geometry_selector_fun eq 1) or (imaging_geometry_selector_fun eq 2)) then begin

 pathfinder_algo_for_ratio2_radiograph_case_1_and_2,ratio2c,ratio2f_fun,cc_fun,ratio1c,ratio1f_fun,uc_fun,rc_fun,debug_comments_fun

endif



;RADIOGRAPH 2nd ONE ORIGINAL: if (ratio2c lt ratio2f) then (cc=cc+1)  else if (ratio1c lt ratio1f) then (uc=uc+1)  else (rc=rc+1)

;old one! if ( abs(ratio2c) lt abs(ratio2f) ) then (cc=cc+((cc+1)/abs(cc+1))) else  (rc=rc+1) 
;uc=uc+((uc+1)/abs(uc+1)) 

;130419 part of following calcs were moved into the functions calcing ratio1,2f.
if (debug_comments_fun eq 1) then print, '( abs(ratio2c) lt abs(ratio2f) ),abs(ratio2c),abs(ratio1c),cc+(cc/abs(cc))',( abs(ratio2c) lt abs(ratio2f_fun) ),abs(ratio2c),abs(ratio1c),cc_fun+(cc_fun/abs(cc_fun))
;if (debug_comments_fun eq 1) then print, 'deltax_interim_signed,deltay_interim_signed,deltaz_interim_signed', deltax_interim_signed,deltay_interim_signed,deltaz_interim_signed
if (debug_comments_fun eq 1) then print, 'this LOSs ratio1f, ratio2f', ratio1f_fun, ratio2f_fun
;if (debug_comments eq 1) then print,'xy_length_unsigned_no_sign_needed_here,deltax_interim_signed^2,deltay_interim_signed^2',xy_length_unsigned_no_sign_needed_here,deltax_interim_signed^2,deltay_interim_signed^2
;if (debug_comments eq 1) then print, 'double((slice_x_location-focal_point_x))',double((slice_x_location-focal_point_x))



;if (debug_comments eq 1) then print, 'this LOSs ratio1f, ratio2f', ratio1f, ratio2f
;if (debug_comments eq 1) then print, 'deltax_interim_signed,deltay_interim_signed,deltaz_interim_signed', deltax_interim_signed,deltay_interim_signed,deltaz_interim_signed
if (debug_comments_fun eq 1) then print, 'this LOSs ratio1f, ratio2f', ratio1f_fun, ratio2f_fun
;if (debug_comments eq 1) then print,'xy_length_unsigned_no_sign_needed_here,deltax_interim_signed^2,deltay_interim_signed^2',xy_length_unsigned_no_sign_needed_here,deltax_interim_signed^2,deltay_interim_signed^2
;if (debug_comments eq 1) then print, 'double((slice_x_location-focal_point_x))',double((slice_x_location-focal_point_x))



;likewise, this was wrong by a factor of two
;if (ratio2c lt ratio2f) then (ll(counter)=dlc*sf2)  else if (ratio1c lt ratio1f) then (ll(counter)=dlu*sf1*cf2)  else (ll(counter)=dlr*cf1*cf2)
ll_fun(counter)=1.0

ratio1c=(double(uc_fun)/double(rc_fun))

a1=double(cc_fun)
b1=double(rc_fun)
c1=double(uc_fun)
d1=double(sqrt(b1*b1+c1*c1))
e1=double(a1/d1)
ratio2c=double(e1)





;find where we are in the 3D grid
;calculate current x-position in the grid
;020719 now use los origins, for more generality!
nx1current_fun=los_origin_x+rc_fun
;same for current y-pos in the 3D grid
ny1current_fun=los_origin_y+uc_fun
;same for z
nz1current_fun=los_origin_z+cc_fun


;FROM HERE D. BOOSTING
; 140515 we ALSO add SECOND aberration stuff here for rlos
;REASON is that each and every cell must have its very own Doppler boosting factor
;SO we need to calculate D. boosting for the second counter loop as well
;(2nd step of pathfinding in 3D)

;declare vars for calculating angle between LOS direction in 3D space and
;local velocity direction.
;080419 edited these to FPoint;s ones 
xx0=los_origin_x
yy0=los_origin_y
zz0=los_origin_z


;we assign the following, in order to avoid confusions from using x,y,z
;which are very simple and might have been used elsewere in the code
;xx=x
;yy=y
;zz=z
;;080419 SOS the following correction from 2015 was only present in the first part of the loops!
;brought it here as well now!
;SUPER 25-05-15
;DIORTHOSI xx=xx0+rc, klp y, z. OSTE x-x0=rc=Dx. Allios bgazei o,ti na'nai arnitika
; SIMEIOSEIS LATHOS TO EIXAN 
xx=rc_fun+xx0
yy=uc_fun+yy0
zz=cc_fun+zz0

;We now select and assign the suitable values of time tagged velocity
;(kind of 4D, probably not in the usual relativistic 4D sense)
;from the 4D array to the convenience vars ux,uy,uz
;in order to facilitate the (los,u) angle cos calculation
;We made sure this comes AFTER assignement of nx(i)current above.
;Else it doesn't work correctly!!


;020815 may be shotnumber, not shotind here check sosara
;ux=vx1_4d(shotnumber,nx1current,ny1current,nz1current)
;uy=vx2_4d(shotnumber,nx1current,ny1current,nz1current)
;uz=vx3_4d(shotnumber,nx1current,ny1current,nz1current)


;*********************************************************
;251218 Following is a test of the coslsu algorithm
;
;The following formula is used, from analytic geometry
;cos(los,u)=A/B, where
;A=[(xx-xx0)*ux+(yy-yy0)*uy+(zz-zz0)*uz]
;B=sqrt[(xx-xx0)^2 +(yy-yy0)^2 +(zz-zz0)^2 ]*sqrt[ux^2+uy^2+uz^2]


;aaa=(xx-xx0)*ux+(yy-yy0)*uy+(zz-zz0)*uz
;bbb=sqrt((xx-xx0)*(xx-xx0) +(yy-yy0)*(yy-yy0) +(zz-zz0)*(zz-zz0) )
;ccc=sqrt(ux*ux+uy*uy+uz*uz)
;coslosu=aaa/(bbb*ccc)
;
;;310715 we define theta and also doppler boosting andf rel. beaming stuff
;also lorenrz factor
;bear in mind need to recheck left right oriented 3d xyz systems.
;thetau=acos(coslosu)
; DOUBLE CHECK IF ccc is indeed the relativistic velocity over c
;020815 may be shotnumber, not shotind here check sosara
;gammalorentz(shotnumber,nx1current,ny1current,nz1current)=1/(sqrt(1-ccc*ccc))
;dopplerfactor(shotnumber,nx1current,ny1current,nz1current)=(sqrt(1-ccc*ccc))/(1-ccc*coslosu)
; check the above functions
;***********************************************************************



;************************************************************
;251218 this segment: historical comments, nothing special
;ng(shotnumber,nx1current,ny1current,nz1current)=nobs*(1/dopplerfactor(shotnumber,nx1current,ny1current,nz1current))
; 2208145 this now comes from the array op before the loop, not from the in-loop calculation SOS
;print, 'freq',ng(shotnumber,nx1current,ny1current,nz1current),'dopplerfactor',dopplerfactor(shotnumber,nx1current,ny1current,nz1current)
; 070815 WE SET TEMPORARILY THOSE TO ZERO 
;FOR EFFICIENCY WHILE TESTING THE CODE GEOMETRICALLY
;MUST RESTORE LATER SOS
;EVEN BETTER, PUT IN A CASE SWITCH TO SELECT WETHER TO
;USE THESE OR NOT
;ALSO FINALLY DO THE ARRAY THING, DOPPLER =F(LOS ANGLE)
;NOT F(CURRENT ANGLE)

;********************************************************************
;
;kabs_4d(shotnumber,nx1current,ny1current,nz1current)=0.018*gaunt(shotnumber,nx1current,ny1current,nz1current)*RHO_4d(shotnumber,nx1current,ny1current,nz1current)*RHO_4d(shotnumber,nx1current,ny1current,nz1current)*(1/ng(shotnumber,nx1current,ny1current,nz1current))*(1/ng(shotnumber,nx1current,ny1current,nz1current))*(Temp_4d(shotnumber,nx1current,ny1current,nz1current)^(-1.5))*(100.0/NLONG)*(1.0e+14)*(xfrac(shotnumber,nx1current,ny1current,nz1current)^2)
;kabs2_4d=0.08235*RHO_4d*RHO_4d*(T^(-1.35))*0.01*(((Temp_4d(shotnumber,nx1current,ny1current,nz1current)/1.0e+8)>1.0)^2)*(100.0/NLONG)







;now that we know where we are in the 3D grid, assign to the current los point, the e and k values
;from the corresponding point of the 3D grid
; 251218 NO WE DONT! COS NOW ALL THAT IS DONE B4 the loops, in array-oriented ops.
; 050815 we now multiply by the dopplerfactor to account for D-boost/de-boost SOS
; temporarily disable dfactor.also consider doing it array oriented 251218 HERE WE GO!
;using ratio1, ratio2 instead of coslosu
;lose(counter)=emiss_4d_fun(shotnumber,nx1current,ny1current,nz1current)*((dopplerfactor(shotnumber,nx1current,ny1current,nz1current))^(2+alphaindex))
lose_fun(counter)=double(emiss_4d_fun(shotnumber_fun,nx1current_fun,ny1current_fun,nz1current_fun))
;k must be between 0 and 1 sos
losk_fun(counter)=double(kabs_4d_fun(shotnumber_fun,nx1current_fun,ny1current_fun,nz1current_fun))
losdraw_fun(nx1current_fun,ny1current_fun,nz1current_fun)=1.0D
;din=-k*dl*in +e*dl


din(counter)=-double(losk_fun(counter))*double(ll_fun(counter))*double(in_fun)+double(lose_fun(counter))*double(ll_fun(counter))
;in cgs the length!!!

dtaun(counter)=double(losk_fun(counter))*double(ll_fun(counter))*double(mikoscell_fun)

if (debug_comments_fun eq 1) then print, 'EDOORETASO2',lose_fun(counter),losk_fun(counter),ll_fun(counter),mikoscell_fun

in_fun=double(in_fun)+double(din(counter))
incurrent(counter)=double(in_fun)
taun_fun=double(taun_fun)+double(dtaun(counter))
tauncurrent(counter)=double(taun_fun)
if (debug_comments_fun eq 1) then print, 'EDO TO IN', in_fun, din(counter), taun_fun, -double(losk_fun(counter))*double(ll_fun(counter))*double(in_fun)
;print, rc, uc, cc, ratio1c, ratio2c, counter, ll(counter)
;exit loop on reaching either end of 3D grid
;270519 commented out k123
;k123=2.*p*(gammac+2.)/3.
;print, 'PTPTPTP', 'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc
;print, 'edo radstuff','��',in,'din',din(counter), $
;-losk(counter),ll(counter),lose(counter),'kabs',kabs(nx1current,ny1current,nz1current),'emiss',emiss(nx1current,ny1current,nz1current), $
;ndens(nx1current,ny1current,nz1current),(((nx1current-x1m)^2+(ny1current-y1m)^2) lt rz*rz)*tau1*(1./((nz1current*deltaz)^k123)), $
;'aloha',(((nx1current-x1m)^2+(ny1current-y1m)^2) lt rz*rz),rz,x1m,y1m,'dallas', $
;(((nx1current-x1m)^2+(ny1current-y1m)^2) lt rz*rz), 'bfield', $
;Bfield(nx1current,ny1current,nz1current),(((nx1current-x1m)^2+(ny1current-y1m)^2) lt rz*rz), $
;lamda1*(1./((nz1current*deltaz)^p)), $
;'emissbits',cp5(0.),ndens(nx1current,ny1current,nz1current),(bfield(nx1current,ny1current,nz1current)*$
;sin(theta))^((gammac+1.)/2.),((ng/2.)/cp1(0.))^((1.-gammac)/2.), $
;'absbits',cp6(0.)*ndens(nx1current,ny1current,nz1current),$
;(bfield(nx1current,ny1current,nz1current)*sin(theta))^((gammac+2.)/2.),$
;((ng/2.)/cp1(0.))^(-(gammac+4.)/2.)

; check emissbits, absbits vs model
;last abs bit is zero?? why?
;030719 try cutting off lastcounter, directly assing the value of counter to  the corresponding element of counterlast_fun() 2D array

;lastcounter_fun=counter
;lastcounter is now defunct and removed from the following debug print command
if (debug_comments_fun eq 1) then print,'here counter',counter
;260619 reverse here first and second, beacause z is the first now!

counterlast_fun(second_coord_fun,nz10_fun)=counter

if (debug_comments_fun eq 1) then print,'counterlast_fun(second_coord_fun,nz10_fun)',counterlast_fun(second_coord_fun,nz10_fun),'second_coord_fun',second_coord_fun,'nz10_fun',nz10_fun
nx1currentlast_fun=nx1current_fun
ny1currentlast_fun=ny1current_fun
nz1currentlast_fun=nz1current_fun
;130419 re-check limits, esp. x-axis cos fpoint begins at zero x. SOS stay within the domain!
;CAREFUL we dont have negatives nx1current, ny1current, nz1current SO we must add a break condition for those cases as well
;130419 ray cannot move onto negative 3D array indices! It must break before that! 
;130419 x will not go backwards! no need for x no neg break condition!
;if (fix(nx1current) eq 0)  then break
if ((ny1current_fun) lt 1)  then break
if ((nz1current_fun) lt 1)  then break
;010619 had to set to minus 2 ze margins below for break, miss some detail, yet more stable! wonder why now pop up this problem!
if ( (rc_fun+los_origin_x) gt (abs(nx_1_fun)-2.0) ) then break
if ( (rc_fun+los_origin_x) lt 1.0 ) then break
;print, 'ssstdfklhv', 'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx_1-nx10)),'rc',rc,'uc',uc,'cc',cc
if ( (uc_fun+los_origin_y) gt (abs(ny1_fun) -2.0) ) then break
if ( (uc_fun+los_origin_y) lt 1.0 ) then break
;print, 'TTTTTT',  'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc
if ( (cc_fun+los_origin_z) gt (abs(nz1_fun) -2.0) ) then break
if ( (cc_fun+los_origin_z) lt 1.0 ) then break
;print, 'BINNN',  'counter',counter,'nx1',nx1,'ny1',ny1,'nz1',nz1,'ny10',ny10,'nz10',nz10, $
;'nx1current',nx1current,'ny1current',ny1current,'nz1current',nz1current, $
;'T/F rc',(rc gt (nx1-nx10)),'rc',rc,'uc',uc,'cc',cc
;restore counter to +2 value for second increment of grid. This way, we got all two grid
;increments recorded in counter.
;print, 'tsa3', counter
if (debug_comments_fun eq 1) then print, 'counter prin telos inner loop los ', counter

;110519 SOS here paint current los voxel with indicator value!
rho_indicator_fun(nx1current_fun,ny1current_fun,nz1current_fun)=rho_indicator_value_fun
;110519 conditional huge array assignement (good for small resolution/high sfactor only) 
if (use_huge_indicator_array_fun eq 1) then begin
;110519 SOS we must deduct lowers, since it goes from lowers up, not from zero!
rho_indicator_total_fun((ny10_fun),(nz10_fun),nx1current_fun,ny1current_fun,nz1current_fun)=rho_indicator_value
endif

endfor




end










;210819 START OF MAIN PROGRAM! 
;
;290619 YO HO HO HERE WE GO!
;***************************************
;021218 B4 anything else, please execute the following six lines of code.
;210819 No need for that any more! Lines of code are now near the beginning, conveniently packaged in a dummy routine! They are even able to auto detect float or double data type!
; Make sure datapath is set to the location of both data and pload 
;(for convenience RLOS currenly runs when RLOS, pload and hydro data are located in the same directory, pointed to by datapath)
;;030419 please NOTE: loading data hore for the first time sets grid dimensions NX1, 2, 3 and these are taken by RLOS as stable. Therefore do NOT alter the grid dimensions throughout the ptogram, but 
; keep them the same (else it will crash or something). So we take NX1, NX2 and NX3 as fixed beginning from the first use of pload, which normally is right after this!

;***************************************

;201218 following file I/O loop adapted from Harris Geosp. website.
; Select a text file and open for reading
;file = DIALOG_PICKFILE(FILTER='*.txt')

;260519 define these here, then update em in above procedure losloop 
nx1currentlast=1.0D
ny1currentlast=1.0D
nz1currentlast=1.0D

;260619 initialize los origins
los_origin_x=0.0D
los_origin_y=0.0D
los_origin_z=0.0D
;171119 COMMENTED THIS OUT FOR NOW, as it *should* work from the earliest such definition! Double-check it though!
DATAPATH='E:\scrange\neutrino_scale\torblob18dummies\'
DATAPATH='E:\scrange\neutrino_scale\torblob18dummies\'
cd, datapath
GDL_DIR='C:\Program Files (x86)\gnudatalanguage\gdlde'
PATH=!PATH+'C:\Program Files (x86)\gnudatalanguage\gdlde\'
!PATH=!PATH+datapath
file_early =DATAPATH+'rlos_params_v254.txt'
CLOSE, 18
 OPENR, 18, file_early
 ;110819 this rewinds file to its beginning! also put it to  main reading part, later on in this code!
 POINT_LUN, 18, 0 
 
; Read one line at a time, saving the result into array
text_array_early = ''
line_early = ''
WHILE NOT EOF(18) DO BEGIN & $
  READF, 18, line_early & $
  text_array_early = [text_array_early, line_early] & $
  ;array_double = [array_double, line] & $
ENDWHILE
; Close the file and free the file unit
CLOSE, 18
print,text_array_early
sfactor_external=text_array_early(8)/1.0D
pload_float_factor=text_array_early(66)/1.0D

print,'pload_float_factor,sfactor', pload_float_factor,sfactor_external
if (pload_float_factor eq 1.0) then print,'tsi' else print,'tsa'
;110819 for copy-paste initially, begin endif construct wont work line by line! single line if then else thing seems to pull the trick nicely for initial manual copy-paste!
if (pload_float_factor eq 1.0) then pload, 10,shrink =sfactor_external, dir=datapath,/float else pload, 1,shrink =sfactor_external, dir=datapath,/double
;110819 till here copy paste at starters, in order to call pload for the first time, while automatically having read the value of sfactor and the float/double form of binary data, from the param file! 
;110819 no need to manually alter sfactor and (float or double) up here each time we change those!'
cd, datapath
GDL_DIR='C:\Program Files (x86)\gnudatalanguage\gdlde'
PATH=!PATH+'C:\Program Files (x86)\gnudatalanguage\gdlde\'

!PATH=!PATH+datapath
file ='rlos_params_v254.txt'
;;********************************
CLOSE, 8
 OPENR, 8, file
 ;110819 this rewinds file to its beginning, necessary now that we also open the file around the start of code!
  POINT_LUN, 8, 0 
 
; Read one line at a time, saving the result into array
text_array = ''
line = ''
WHILE NOT EOF(8) DO BEGIN & $
  READF, 8, line & $
  text_array = [text_array, line] & $
  ;array_double = [array_double, line] & $
ENDWHILE
; Close the file and free the file unit
CLOSE, 8

;********************************
;

;210819 Here we read the external parameter file.
;
;NEXT IS ASSIGNEMENT LIST, OF PARAMETER VALUES, the one that counts! Later on it may be read from an external file or something. 
;Here we may collectively edit parameters, without searching for them in the code. 
;Then, the corresponding param, is assigned the 'external' value set up in this list. for e.g later in the code, we read: sfactor=sfactor_external
;************************************************
; 191218 conditional_stop is a switch that turns on (1) or off (0) stops along the code execution line, aimed to assist with debugging.
conditional_stop=text_array(4)/1.0D
;191218 debug_comments is a switch that turns on (1) or off (0) comments that assist with debugging.
debug_comments=text_array(6)/1.0D
;sfactr is shrink factor used in pload. Full res is 1, half res is 2, etc.
sfactor_external=text_array(8)/1.0D
;ts param next, tewaks matter speed everywhere, and anytime, in data
speedtweakfactor_external=text_array(10)/1.0D
;select wether manually verride clight, else it keeps its natural value calced by the algo
clight_override_external=text_array(12)/1.0D
;in case we select override=1, then it is gets the clight_preset clight value.else it keeps the natural value, whatever that will be
clight_preset_external=text_array(14)/1.0D
; 140418 HERE INPUT NOMINAL JET VELOCITY in units of c, as set manually in the I/O of PLUTO
jet_norm_velocity_external=text_array(16)/1.0D
;270815 shotmin was the beginning shot it was set to zero, so we always went
;till no shotmax, i.e. 10 in our case!!
; it all begins at shotmin and goes till shotmax 
; 270618 in order to run till a given snapshot, set shotmin to zero 
shotmin_external=text_array(18)/1.0D
; 040816 MUST HAVE enough temporal span in the data to cover the jet time cross interval
;i.e. shotmax-shotmin=more than jet cross time!!! 
;else in runs out of time instants and gives error message!!s
shotmax_external=text_array(20)/1.0D
;aiming angles 
phi2_external=text_array(22)/1.0D
phi1_external=text_array(24)/1.0D
; 170915 the following factor, freqshiftchoice, is 1 for taking into account
;doppler shift of the frequency and other, e.g. 0, for not taking it into account
;for freqshiftchoice=1, we get for each cell a different ncalc, ncalc=nobs/Dfactor
;freqshiftchoice=1.0
freqshiftchoice_external=text_array(26)/1.0D
; 040915 this following string, called dopplerchoice, should equal exactly 1.0
;anything else and there is no dopplerboosting whatsoever.
dopplerchoice_external=text_array(28)/1.0D
;if no dopplerboosting then uncomment the following line SOS
; 050815 we now set the spectral index, larger 
;than zero, i.e. meaning
alphaindex_external=text_array(30)/1.0D

; 060815 assign an onbserving frequency, 
nobs_external=text_array(32)/1.0D
;NLONG is maximum cell number along jet SOS
;SCALING FACTOR IT IS FOR CASE OF MORE CELLS AND HIGHER RESILUTION
;TO AVOID REWRITING THE INSIDES OF THIS CODE SOS
; 060815 in case of higher res available,
;maybe change param long upwards of its currently assigned 
; value of 150.
NLONG_external=text_array(34)/1.0D
;UNITS FROM INIT.c
;RELEVANT PLUTO's time unit is obtained by division of length to speed unit, i.e. 1/3 of sec.
; 090915 copy these from pluto's init.c file of i/b conditions sos
plutolength_external=text_array(36)/1.0D
plutospeed_external=text_array(38)/1.0D
plutodensity_external=text_array(40)/1.0D
plutocelllength_external=text_array(42)/1.0D
LOOP_LENGTH_FACTOR_FOR_TIMING_RELATED_TESTS=text_array(44)/1.0D
show_loops_switch=text_array(46)/1.0D
show_loops_switch__screen_boundaries=text_array(48)/1.0D
zigzag_factor=text_array(50)/1.0D
focused_beam_switch=text_array(52)/1.0D
focal_point_x=text_array(54)/1.0D
slice_x_axis_percentage=text_array(56)/1.0D
slice_y_lower_percentage=text_array(58)/1.0D
slice_z_lower_percentage=text_array(60)/1.0D
slice_y_upper_percentage=text_array(62)/1.0D
slice_z_upper_percentage=text_array(64)/1.0D
pload_float_factor=text_array(66)/1.0D
ERROR_STAGNANT_LOS=text_array(68)/1.0D
rho_indicator_value=text_array(70)/1.0D
use_huge_indicator_array=text_array(72)/1.0D
focal_point_y_axis_percentage=text_array(74)/1.0D
focal_point_z_axis_percentage=text_array(76)/1.0D
imaging_geometry_selector=text_array(78)/1.0D
steady_state_switch=text_array(80)/1.0D
rod_test_switch=text_array(82)/1.0D
incoming_perpendicular_rod_test_switch=text_array(84)/1.0D
incoming_planar_rod_test_switch=text_array(86)/1.0D  
halfwidth=text_array(88)/1.0D
characteristic_value_unique=text_array(90)/1.0D
ujet_steady_state=text_array(92)/1.0D
lossolidangle=text_array(94)/1.0D
LOOP_LENGTH_FACTOR_FOR_TIMING_RELATED_TESTS=text_array(96)/1.0D
backwards_in_time=text_array(98)/1.0D
focal_pointXZ_y=text_array(100)/1.0D
sliceXZ_y_axis_percentage=text_array(102)/1.0D
focal_pointXZ_x_axis_percentage=text_array(104)/1.0D
focal_pointXZ_z_axis_percentage=text_array(106)/1.0D
sliceXZ_x_lower_percentage=text_array(108)/1.0D
sliceXZ_z_lower_percentage=text_array(110)/1.0D
sliceXZ_x_upper_percentage=text_array(112)/1.0D
sliceXZ_z_upper_percentage=text_array(114)/1.0D
path_algo_set=text_array(116)/1.0D
reverse_los_switch=text_array(118)/1.0D
spectrum_direct_switch =text_array(120)/1.0D;160819 this turns on the use of the spectral formula.0 is off, 1 is S propto nu^(-alphaindex), etc 2, etc
kappa_spectr=text_array(122)/1.0D
kappa_spectr2=text_array(124)/1.0D
debug_spectra_switch=text_array(126)/1.0D 
pion_emissivity_switch=text_array(128)/1.0D 
neutrino_emissivity_switch=text_array(130)/1.0D 
synchrotron_emissivity_switch=text_array(132)/1.0D 
kappa_value_power_law=text_array(134)/1.0D 
u_fast_proton_lower_threshold_external=text_array(136)/1.0D 
z_fast_proton_threshold_index_external=text_array(138)/1.0D 
neutrino_energy_selector=text_array(140)/1.0D 

print, focal_pointXZ_y,sliceXZ_y_axis_percentage,focal_pointXZ_x_axis_percentage,focal_pointXZ_z_axis_percentage,sliceXZ_x_lower_percentage,sliceXZ_z_lower_percentage,sliceXZ_x_upper_percentage,sliceXZ_z_upper_percentage

;focal_pointXZ_z_fun


;pload_float_factor=1.0


;slice_y_lower_percentage=0.2
;slice_z_lower_percentage=0.2
;slice_y_upper_percentage=0.2
;slice_z_upper_percentage=0.2


;focal_point_x=0.0


;PARAMS LIST ENDS HERE!
;************************************************
;100519 SOS move all the follwing to the param file and have them read from there! sos do it!
;
;151218 corrected recent artifact that prevented sfactor from working properly at more than 1 values. (must call pload with shrink=sfactor BEFORE 4D array definitions, in order for NX1,2,3 to get correct values)

;080519 next factor reduces overall LOS length, back in time as it progresses, in order to allow for smaller time spans to be used. LOSES FAR END OF DOMAIN THOUGH, only for 
;studiyng near effects, such as the perp. beam test!
;LOOP_LENGTH_FACTOR_FOR_TIMING_RELATED_TESTS=0.730




;**************************************************
;SOS 030419 now we setup the details in order to prepare for the focused version of RLOS
;print, sfactor,'sfactor read'
;*************************************** 

;210819 Here we do the same stuff as in the relevant procedure, pload_float etc. But inst. of calling that, we still havw this piece of code in the main program!
;


;161119 commented these out! ONLY at the beginning do we need those!
;161119 NO NO KEEP THEM FOR NOW!!!
;
DATAPATH='E:\scrange\neutrino_scale\torblob18dummies\'
DATAPATH='E:\scrange\neutrino_scale\torblob18dummies\'
cd, datapath
GDL_DIR='C:\Program Files (x86)\gnudatalanguage\gdlde'
PATH=!PATH+'C:\Program Files (x86)\gnudatalanguage\gdlde\'
!PATH=!PATH+datapath
file_early =DATAPATH+'rlos_params_v254.txt'
CLOSE, 18
 OPENR, 18, file_early
 ;110819 this rewinds file to its beginning! also put it to  main reading part, later on in this code!
 POINT_LUN, 18, 0 
 
; Read one line at a time, saving the result into array
text_array_early = ''
line_early = ''
WHILE NOT EOF(18) DO BEGIN & $
  READF, 18, line_early & $
  text_array_early = [text_array_early, line_early] & $
  ;array_double = [array_double, line] & $
ENDWHILE
; Close the file and free the file unit
CLOSE, 18
print,text_array_early
sfactor_external=text_array_early(8)/1.0D
pload_float_factor=text_array_early(66)/1.0D

print,'pload_float_factor,sfactor', pload_float_factor,sfactor_external
if (pload_float_factor eq 1.0) then print,'tsi' else print,'tsa'
;110819 for copy-paste initially, begin endif construct wont work line by line! single line if then else thing seems to pull the trick nicely for initial manual copy-paste!
if (pload_float_factor eq 1.0) then pload, 10,shrink =sfactor_external, dir=datapath,/float else pload, 1,shrink =sfactor_external, dir=datapath,/double
;110819 till here copy paste at starters, in order to call pload for the first time, while automatically having read the value of sfactor and the float/double form of binary data, from the param file! 
;110819 no need to manually alter sfactor and (float or double) up here each time we change those!'
GDL_DIR='C:\Program Files (x86)\gnudatalanguage\gdlde'
PATH=!PATH+'C:\Program Files (x86)\gnudatalanguage\gdlde\'
cd, datapath

;161119 SO Sthis shall NOT work if there is no 21 datafile!!! CAREFUL! changed it to 1!

!PATH=!PATH+datapath
if (pload_float_factor eq 1.0) then begin
pload, 1,shrink =sfactor, dir=datapath,/float
endif else begin
pload, 1,shrink =sfactor, dir=datapath
endelse
;060519 CRUCIAL to have NX1,2,3 defined AFTER the first call OF THE RUN (excluding the initial pre-compilation call) above, 
;which call occurs AFTER reading from the param file the sfactor shrink index. NEED be in correct order!
;060419 SOS brought those screen size decreases back here in order to calc sub-screen size based on these, not the original ones. 
;
;nx1 is also included in pload, but in 3d mode (non-R) pload was called before ze definition of nx1, therefore it was declared afterwards as nx1 and pload was not ever called again. but now, pload was
;called time and again, messing up with nx1 (it was set to pluto's x dimension, not minus 6.
;now we have nx_1 here and nx1 in pload. pload has nx1, nx2, nx3. We have now: nx_1, ny1, nz1
;nx_1=114
;ny_1=194
;nz_1=114

nx_1=NX1-6
ny_1=NX2-6
nz_1=NX3-6
; repeat this stuff 'cos we used both in different areas SOS
;if change one, then should change the other as well for sure.
;Else, recipe for disaster

ny1=NX2-6
nz1=NX3-6


print, NX1, NX2, NX3
;

;260619 INITIALIZE hereby  vars used within procedures!
;
;pro los_origins, ,,, ,,,,,,,

first_coord_min=0.0d
first_coord_max=0.0d
second_coord_min=0.0d
second_coord_max=0.0d
first_coord=0.0d
second_coord=0.0d
third_coord=0.0d







;030419 NEXT we set the position of the focal point, on the YZ plane, as a percentager of the NX2 and NX3 voxel dimensions (NX1=0, NX2, NX3) of the YZ plane.
;030419 SOS this param should be moved ASAP to the param file SOS!
;SOS 030419 for YZ plane fp_x is ZERO, not one, cos IDL starts arrays from zero, not one SOS

;*****************************************
;SOS 060419 MIND YOU, the LOS beginning form the focal point may not reach exactly the corresponding screen pixel (may miss it). Also, the 
; travel time to the corresponding pixel is an easy calculation, no need to run the sim for that! 
; Therefore, we may set up the following: 

; 1. For each LOS, we calc the travel time from FPoint to each screen pixel. 

; 2. Then, we find the minimum among FP to screen voxel travel times. 

; 3. Then we begin the imaging from the given (CAMERA TRIGGER) time from the pixel with the minimum, running BACKWARDS in time.
;    THE REST OF THE SCREEN PIXELS BEGIN THEIR LOS'S WITH A 'DELAY' (ACTUALLY ADVANCE, SINCE WO GO BACK IN TIME) 
;    THAT DELAY MAY BE CALCED, for each pixel, IN A SIMPLE ARRAY OPERATION. THEIR LOS's POINT TO THE pixel's DIRECTION, AS PRESCRIBED (PHI1, PHI2)
;    FROM THE LINE FPOINT TO CORRESPONDING VOXEL.
;   
; 4. BUT HOW SHALL THEY BEGIN THEIR JOURNEY WITH DELAY? 
; WE HAVE A DISCRETE GRID WITH MODEST TEMPORAL RESOLUTION! HOW? WTF? 
; LEAVE IT! DO INTRODUCE THE SECOND DIRECTIONAL CRITERION AND DO the FP thing AS PLANNED. 
; Just, if it misses, CORRECT IT TO THE ORIGINAL POINT AND ALSO CORRECT THE TRAVEL TIME. BUT THEN WHY NOT LEAVE IT THERE?   

; 5. DECISION:  First, we do as planned originally. Then, examine result: if it reaches the correct pixel, and if the time to do that is near the calced travel time from FP to each given pixel.
; If the differences are important, only then do consider adding corrections. COS AS IT IS NOW PLANNED IT IS SIMPLE!. Keep it SIMPLE!

; YET DO CHECK WHERE EACH LOS CROSSES SCREEN, VERSUS ITS PLANNED POINT THAT SHOULD BE EASY. DO IT AS A BUILT_IN CHECK

;******************************************
;100419 LATER ON IN THE LOOPS, WE MUST MAKE THOSE FLOAT OR DOUBLE, else it does int division for ratio1,2 calculation later on and if less than one then gives result ZERO!! 

;210819 Here are some rounding geometry stuff, from percentages and fractions to the discrete grid. Should have been in a pro, yet here they are.
;020320: ULTRA SOS WE USED nx_1, etc: NO!! nx1, needed! else: ERROR!!! SOS!!!
focal_point_y=(fix(focal_point_y_axis_percentage*ny_1))
;170719 here fixed error, was using y axis percentage for z as well, instead of z's own percentage!
focal_point_z=(fix(focal_point_z_axis_percentage*nz_1))
print, focal_point_x, focal_point_y, focal_point_z

;170719 now we do ze same for focal point XZ. MAYBE MOVE ALL THOSE AS WELL TO A PROCEDURE SOMEHOW! 
;170719 fpXZ's y is read from param file directly!
;180719 SOS we had fpXZ'x determined by a percentage of ny, which was larger, thus triggering the breaks!
focal_pointXZ_x=(fix(focal_pointXZ_x_axis_percentage*nx_1))
focal_pointXZ_z=(fix(focal_pointXZ_z_axis_percentage*nz_1))
;020320: ULTRA SOS WE USED nx_1, etc: NO!! nx1, needed! else: ERROR!!! SOS!!!
;SOS 030220 LEAVE AS IS WE DID IT ONLY IN NEMISS!!
print, 'SOS ORE  we had the x of fpXZ  determined by a percentage of ny, which was larger, thus triggering the breaks!',focal_pointXZ_x, focal_pointXZ_y, focal_pointXZ_z


 if (debug_comments eq 1) then stop

;next we set the location of the slice where the image shall ber formed. This screen may be smaller than the slice, or at most equal to it. 
;Smaller also means lower resolution. SOS DO A FIGURE RELEVANT.
;SOS 060419 following should be put in the params.txt file. 
;100419 CAREFUL the follwing must be float or double, NOT left integer!
;100419 slice cuts x axis at following location:

slice_x_location=double(fix(slice_x_axis_percentage*nx_1))
;170719 nx1 is moar than nx_1: nx1-6,nx2-6,nx3-6= nx_1, ny1, nz1=nx_1,ny_1,nz_1. So, ny1 equals ny_1 equals nx2 minus six, but nx_1 is six less than nx1. CAREFUL HERE!
sliceXZ_y_location=double(fix(sliceXZ_y_axis_percentage *ny_1))
if (debug_comments eq 1) then print,'sliceXZ_y_location,slice_x_location,slice_x_location,sliceXZ_y_axis_percentage', sliceXZ_y_location,slice_x_location,slice_x_location,sliceXZ_y_axis_percentage
;030419 now we set the dimensions of the screen relevant to the slice that hosts it. 
;030419 The way we do it is we define the margins from the host slice, to the four sides of the hosted screen (see figure). 
; CAREFUL PERCENTAGES MUST BE CLEARLY BELOW 50 percent, cos the yADD UP IN PAIRS. THESE ARE WHAT IS LEFT OUT, i.e. MARGINS. At MOST 50 percent.

; SOS 060419 following should eventually be placed in the params.txt file 



;030419 translate the above to margins 

;170719 removed from the following the definitions of the third dimension for each case, since we got 2D screens!
;screen_x_lower_voxel=fix(slice_x_lower_percentage*nx_1)
screen_y_lower_voxel=fix(slice_y_lower_percentage*ny_1)
screen_z_lower_voxel=fix(slice_z_lower_percentage*nz_1)
;170719 was y, corrected it now, to x_upper...! Also was ny_1, now nx_1
;screen_x_upper_voxel=fix( (1.0-slice_x_upper_percentage)*nx_1)
screen_y_upper_voxel=fix( (1.0-slice_y_upper_percentage)*ny_1)
screen_z_upper_voxel=fix( (1.0-slice_z_upper_percentage)*nz_1)
;SOS 030419 NOW PRINT AND verify the above and then carry on

print, screen_y_lower_voxel,screen_z_lower_voxel,screen_y_upper_voxel, screen_z_upper_voxel

;170719 We do the same for the XZ version:

screenXZ_x_lower_voxel=fix(sliceXZ_x_lower_percentage*nx_1)
;screenXZ_y_lower_voxel=fix(sliceXZ_y_lower_percentage*ny_1)
screenXZ_z_lower_voxel=fix(sliceXZ_z_lower_percentage*nz_1)
screenXZ_x_upper_voxel=fix( (1.0-sliceXZ_x_upper_percentage)*nx_1)
;screenXZ_y_upper_voxel=fix( (1.0-sliceXZ_y_upper_percentage)*ny_1)
screenXZ_z_upper_voxel=fix( (1.0-sliceXZ_z_upper_percentage)*nz_1)


if (debug_comments eq 1) then stop

;*****************************************************



;*********************************************
;170419 here we define a 3D grid of xyz indices. Could not do it isn array op, though i recall doing it ages ago. For now, a loop!
;SOS only this loopy. Rest remains array op
xcoord=dindgen(nx_1+8,ny1+8,nz1+8)
ycoord=dindgen(nx_1+8,ny1+8,nz1+8)
zcoord=dindgen(nx_1+8,ny1+8,nz1+8)


;140320 After setting tanphi12 margin to six, down from seven, 
;here it eems to work from zero to plus five, inst. of plus six! Wont work from one to plus six though!
;check  out xcoord plus eight above, though! 
for zindigo=0.0,nz1+5 do begin
 for yindigo=0.0,ny1+5 do begin
  for xindigo=0.0,nx_1+5 do begin
      xcoord(xindigo,yindigo,zindigo)=xindigo
      ycoord(xindigo,yindigo,zindigo)=yindigo
      zcoord(xindigo,yindigo,zindigo)=zindigo
  endfor
 endfor     
endfor
;*********************************************

;FOR variable = init, limit [, Increment] DO statement
;FOR variable = init, limit [, Increment] DO BEGIN
;statements
;ENDFOR







;200319 following switch SELECTS THE STEADY STATE = ON AND employs the DENSITY of the cone-like model instead of the hydrocode density.
;210619 now read from file
;steady_state_switch=0.
;300319 following switch (REQUIRES THE PREVIOUS 'steady state' SWITCH ALSO TO BE ON) 
;SELECTS THE rod test = ON AND employs the DENSITY of the cone-like model instead of the hydrocode density .

;210619 now read from filerod_test_switch=0.
;010519 select incoming perpendicular_rod_test, aka relativistic test of approaching rod, wider at centre, thiinber at edges
; 210619 now read from file incoming_perpendicular_rod_test_switch=0.0
;150519 similar to above, DO NOT SET BOTH TO ONE SOS MAKE IT FOOL PROOF SOON ENOUGH!!! SOS!!! 150519
; 210619 now read from file incoming_planar_rod_test_switch=1.0
;300319 halflength of rod
;210619 now read from file halfwidth=2
;300319 unique value to set rod values equal to in order to select them afterwards if equasl to that
; 210619 now set in file characteristic_value_unique=1.768657846578D
;200319 here define a steady-state jet velocity along y, the jet axis. no other velocity component is present is case the steady-state is ON.
; 210619 now set in file ujet_steady_state=0.8

;0208156 move to file or to beginning this param
; 210619 did it! now it resides in the param file! lossolidangle=0.000001

; 140818 phi1 MUST range from -90 degrees to plus 90 degrees (-1.57 to plus 1.57 RADS   RADS ARE USED HERE  .
; NOT inclusive the limits. Else, singularity so ratio1f goes 
;neg then ratio1f goes zero so we get cris image from the side, fully doppler boosted, but WRONG GEOMETRICALLY  

;  140818 in order to image a jet at high angles, i.e. bossted, maybe we should position it so as to emerge from the imaging plane  .

;SOLVED 120818 when phi1 goes to 90 degrees, i.e. 1.5 rad or so, then theta in doppler factor calc (IMPLIED IN THIS CODE) goes to zero and 
;therefore D factor is maximized. When it is neg, then Dfactor is opposite effect.   tested now.
;for positive boosting else we get de-boosting PUT NOTE IN GEOMETRY IN PAPER FIG. 1, 2  Also FS off, db on, phi1 neg, et voila!
; 260718 keep freqfactor off, else it decreases, as is, the emission, cause it divides ncalc with ng and then squares that. hust a soothing effect for the tests. But it can be modded to inc. a real complex spectrum, better that the D^a that is now incl. in the D
;D boosting factor. mention in paper DB on, FS off for now works best. If complex spectrum in future, edit FS (into a true shifting of freq) and also DB (remove alpha index) and then use both. SOS

;
; 230718 THIS HAS dlu, dlc, dlr corrected the sfactor ERROR. NOW WORKS OK WITH SFACTOR=2 or 4 etc
; 230618 clight=0.5 works OK with current temporal resolution. jumps every 2 or so. could use abit denser though. SUPER always SET SHOTMAX BELOW MAX SHOT, ELSE ERROR T=0, dt=0
; THIS HAS THE LOS LENGTH ADAPTATION
;; THIS HAS freqfactor to the alphaindex, not just to the second power 270618
;maxphi1 less thaan 1 (0.7 works for sure)) if phi2 very small! else need moar data! 240816
;190816 recheck dboosting calc. right now, ze db array has max of just 1.56?? should be 
;much higher than that with speedtweak used, which boosts speed  near ot over c!
;Is it just angles, or maybe an error of sorts? A subtle one perhaps? ZE RELEVANT CALC  is the one involving ze aaaaa's, bbbbb's, ccccc's, etc
;i.e. re-check ze coslosu calculation vs its implementation in here!


;nx1 is also included in pload, but in 3d mode (non-R) pload was called before ze definition of nx1, therefore it was declared afterwards as nx1 and pload was not ever called again. but now, pload was
;called time and again, messing up with nx1 (it was set to pluto's x dimension, not minus 6.
;now we have nx_1 here and nx1 in pload. pload has nx1, nx2, nx3. We have now: nx_1, ny1, nz1


;040816 MUST HAVE enough temporal span in the data to cover the jet time cross interval
;i.e. shotmax-shotmin=more than jet cross time!!! 
;else in runs out of time instants and gives error message!!






;190617

;some special effects used to pronounce certain relativistic effects in code results

;clight simply 'enlarges' artificially the voxels, as far as the light ray is concerned. Therefore it sets the light speed to 4 voxels per sec, thus making relativistic effects more profound (AFFECTS LIGHT RAY SPEED). 

;tweakspeed, on the other hand, means we multiply existing JET MATTER velocities by a tweakspeed factor, in order to emulate a faster jet, without actually re-running the simulation. This also helps to more directly compare with the corresponding non-tweaked version, all other factors kept the same. 

;freqshift choice is a factor setting on off the frequency shift. This only matters when there is some dependence of the emitted intensity on the actual frequency used (ncalc, vs ng) in its calculation. In our case, we use freq squared, no gamma dependence. thus, a quadratic dependence on frequency, for the sake mainly of investigating its effects on the code results. 


;*********************************************************************
;241218 inactive portion of cone-like model here 
;pro peol

;CPU, TPOOL_NTHREADS=3
;CPU, /VECTOR_ENABLE
;CPU, TPOOL_MAX_ELTS = 0, TPOOL_MIN_ELTS = 1
;common su37, gammac
;gammac=1.8
;print, 'ok edo'
;end

;function cp1, x
;common su37
;a=(6.27E+18)
;return, a
;end

;function cp3, x
;common su37
;a=(1.87E-23)
;return, a
;end

;function cp5, x
;common su37
;gammac=1.8
;a=(0.25)*cp3(0.)
;b=gamma((3.*gammac-1.)/12.)
;c=gamma((3.*gammac+7.)/12.)
;d1=((gammac+(7./3.))/(gammac+1.))
;return, a*b*c*d1
;end

;function cp6, x
;common su37
;gammac=1.8
;a=double((1./32.)*(((2.998E+10)/(cp1(0.)))^(2.)))
;b=double(cp3(0.))
;c=double((gammac+(10./3.)))
;d1=double(gamma((3.*gammac+2.)/12.))
;e=double(gamma((3.*gammac+10.)/12.))
;return, double(a*b*c*d1*e)
;end
;110722 SOS here we do pachz coeffs SOS may be compared to old ones from phd days.
;c1_pachz=6.27*(10.0^18) 
;print, 6.27*(10.0^18)
;print, "c1_pachz",c1_pachz

;c2_pachz=2.37*(10.0^(-3))
;print, 2.37*(10.0^(-3))
;print, "c2_pachz",c2_pachz

;c3_pachz=1.87*(10.0^(-23))
;print, 1.87*(10.0^(-23))
;print, "c3_pachz",c3_pachz

;c4_pachz=4.20*(10.0^7)
;print, 4.20*(10.0^7)
;print, "c4_pachz",c4_pachz


;c5_pachz=0.25*c3_pachz*(gamma( (3.0*gammac-1.0)/12.0    ) )*(gamma( (3.0*gammac+7.0)/12.0    ) )*( (gammac+(7.0/3.0))/(gammac+1.0)  )

;function pipes, ttrest
;apot1=ttrest*2.0
;return, apot1
;end

;print, pipes(2.0)


;function c5_pachz, gammac, c3_pachz
;apotelesma= 0.25*c3_pachz*(gamma( (3.0*gammac-1.0)/12.0    ) )*(gamma( (3.0*gammac+7.0)/12.0    ) )*( (gammac+(7.0/3.0))/(gammac+1.0)  )

;return, apotelesma
;end

;ttttest1=c5_pachz(2.0, c3_pachz)
;print, ttttest1, "c5_pachz(2.0, c3_pachz)"
;function c6_pachz, gammac, c1_pachz, c3_pachz

;apotelesma2=(1/32.0)*(   ( 3.0*(10.0^10)/c1_pachz )^(2.0) )*c3_pachz*( gammac+(10.0/3.0) )*(gamma( (3.0*gammac+2.0)/12.0    ) )*(gamma( (3.0*gammac+10.0)/12.0    ) )

;return, apotelesma2
;end
;print, c6_pachz(2.0, c1_pachz, c3_pachz)


;******************************************************************

;241218 some optimization here
CPU, TPOOL_MAX_ELTS = 0, TPOOL_MIN_ELTS = 1
;common su37, gammac
;210819 A lone param set here. This one NOT  in theparam file though!
; SPECIAL INDEX SET HERE gammac 070418
gammac=1.8

;210819 Params read in the external file are assigned 'typical' values here and then assigned the  externally read ones! 
;210819 Basically, here are read param values from the array read from external file, to the corresponding code variables!
;
;
; THE pload's SHRINK FACTOR IS SET HERE  
sfactor=2.0
sfactor=sfactor_external;d
;241218 CAREFUL HERE: CAREFUL!  IMPERATIVE to load some using the desired sfactor, in order to get the dims correct NX1, etc 
;for the array definitions shortly after
;030419 CAREFUL this last loading of data sets the NX1 etc dimensions of the grid. DATA LOADED HERE SHOULD BE OF SAME DIMS AS DATA LOADED LATER IN TEMPOORAL SEQUENCE!

;210819 AGAIN this pload float stuff. Second time in main rpogram! Maybe a little too much?
if (pload_float_factor eq 1.0) then begin
pload, 21,shrink =sfactor, dir=datapath,/float
endif else begin
pload, 21,shrink =sfactor, dir=datapath
endelse

;************************************************************************
;241218 here we assign the parameter values to the ones read from the param txt file. This step provides the ability to manually override in case 
;reading from the param file fails for some reason.
;
;
; 090816 HERE WE SETUP A FACTOR IN ORDER TO HOMOGENEOUSLY TWEAK SPEEDS TEMPORARILLY, ALL OVER THE GRID!
;MUST AVOID THIS IN NORMAL RUNS SOS
speedtweakfactor=1.0
speedtweakfactor=speedtweakfactor_external;d

;141218 Next is clight override switch. IF yes, then set to 1 (one). Else, if NOT USED, then set to zero, or any other! 1=on, other =off.
;when ON, i.e. 1, then clight takes the value set below, clight preset, OVERRIDING the naturally calced value of clight.
;WHEN OFF, then no override occurs and NATURAL value is employed. SO set override to 1 and use preset, or set it otherwise and let clght natural value be used.
clight_override=0.0
clight_override=clight_override_external;d
clight_preset=1.0
clight_preset=clight_preset_external;d

; 140418 HERE INPUT NOMINAL JET VELOCITY in units of c, as set manually in the I/O of PLUTO
jet_norm_velocity=0.8
jet_norm_velocity=jet_norm_velocity_external;d


;SUPER 270815 shotmin was the beginning shot it was set to zero, so we always went
;till no shotmax, i.e. 10 in our case!!
; it all begins at shotmin and goes till shotmax SOS
; 270618 in order to run till a given snapshot, set shotmin to zero 
shotmin=2
shotmin=shotmin_external;d


 
;SUPER 040816 MUST HAVE enough temporal span in the data to cover the jet time cross interval
;i.e. shotmax-shotmin=more than jet cross time!!! 
;else in runs out of time instants and gives error message!!

; 270815 shotmin, shotmax must be within available range of data sets
;now we set a default value for shotmax, nothing special, just an initial value before reading it
shotmax=22
shotmax=shotmax_external;d

;***********************************************************************************
;*****************************************************

;150719 from here to time procedure cutted stuff! 



;251218 following comments from original 2008 3D LOS code 
;;*****************************************************************
;final aiming of LOS
;15508 edited loop to increment by two steps and decrease one step temporarily, since there are two steps of ratiof check. Then , we stored only second step. SOS. must
;store two steps for every loop circle, along the LOS.
                                             

;*************ABABABABAB****************

; 170915 the following factor, freqshiftchoice, is 1 for taking into account
;doppler shift of the frequency and other, e.g. 0, for not taking it into account
;for freqshiftchoice=1, we get for each cell a different ncalc, ncalc=nobs/Dfactor
;freqshiftchoice=1.0
freqshiftchoice=0.0
freqshiftchoice=freqshiftchoice_external;d
; OS EDO 110418 3.38pm SOS

; 040915 this following string, called dopplerchoice, should equal exactly 1.0
;anything else and there is no dopplerboosting whatsoever.
dopplerchoice=1.0
dopplerchoice=dopplerchoice_external;d
;if no dopplerboosting then uncomment the following line SOS
;dopplerchoice=0.0
print, 'dopplerchoice=',dopplerchoice

; 050815 we now set the spectral index, larger 
;than zero, i.e. meaning

alphaindex=2.0
alphaindex=alphaindex_external;d

;nx1 is also included in pload, but in 3d mode (non-R) pload was called before ze definition of nx1, therefore it was declared afterwards as nx1 and pload was not ever called again. but now, pload was
;called time and again, messing up with nx1 (it was set to pluto's x dimension, not minus 6.
;now we have nx_1 here and nx1 in pload. pload has nx1, nx2, nx3. We have now: nx_1, ny1, nz1


; 060815 assign an onbserving frequency, 
;maybe move to beginning SOS
nobs=1000000000.*8.
nobs=nobs_external;d

;NLONG is maximum cell number along jet SOS
;SCALING FACTOR IT IS FOR CASE OF MORE CELLS AND HIGHER RESILUTION
;TO AVOID REWRITING THE INSIDES OF THIS CODE SOS

; 060815 in case of higher res available,
;maybe change param long upwards of its currently assigned 
; value of 150.

NLONG=150.0
NLONG=NLONG_external;d
;
;SOS230718here corrected sfactor bug SOS!
dlu=1.0*sfactor
;pluto grid not exactly isotropic
dlr=0.95*sfactor
dlc=1.0*sfactor
;020815 this 0.95 factor put in a file or at beginning and set as a param SOS
;some optimization, for the just-1-d loop though

; cell length is already included is kabs definition SOS
mikoscell=1

;020815 INCORPORATE FREQUENCY SHIFT, just like in original HJ88 model from JBank. DO IT IT

;UNITS FROM INIT.c
;RELEVANT PLUTO's time unit is obtained by division of length to speed unit, i.e. 1/3 of sec.
; 090915 copy these from pluto's init.c file of i/b conditions sos
plutolength=1.0*10.0^10.0
plutolength=plutolength_external;d
plutospeed=3.0*10.0^10.0
plutospeed=plutospeed_external;d
plutodensity=1.67*10.0^(-24.0)
plutodensity=plutodensity_external;d

plutotime=plutolength/plutospeed


;CGS value for cell length form PLUTO
; MUST multiply (NOT DIVIDE) BY pload's SHRINK SO, i.e. by sfactor

plutocelllength=1.0*10.0^10.0
plutocelllength=plutocelllength_external;d
lcell=plutocelllength*sfactor

;factorc converts from cgs to los cell length units

factorc=lcell
factort=1/plutotime
if (debug_comments eq 1) then print, factorc, factort, plutotime, lcell, ' .......... TSA11'
;CGS value for speed of light in medium, c, in cells per second 020815
; 110915 we corrected clight to use pluto time units instead of 
;seconds. time in pluto is measured in pluto time units
;and we use the pluto t array for calculations along the los etc.
;consequently, clight should be in cells per pluto time unit, 
;NOT in cells per sec, no.

; 301217 MUST CONSIDER SFACTOR IN CLIGHT SCALING 
clight=3.0*10000000000.0/(factorc*factort)
; next calculation assigns clight to its derived value, from PLUTO settings, plutotime, cell size, etc etc. Is 0.5 probably. 
print, 'original clight',clight

;temp stuff here 030816 cause we dont have that many snapshots also recheck dimensional stuff from
;PLUTO etc and also vs ours here!!! cm, sec, etc etc!
; 090816 clight only matters for time calculations. 
;NOT for dboosting calcs! there, all calcs take c=1 
;clight=400.0
; 141218 next we employ the override factor, in order 

if (clight_override eq 1) then (clight=clight_preset) 

if (conditional_stop eq 1) then stop


TPOOL_MAX_ELTS=1000
if (debug_comments eq 1) then print, nx_1, 'EDO ORE'
; array4d indexing begins at 0, whereas pload's snapshot indexing begins at 1. Therefore
;some RAM is wasted here (an extra snapshot is defined, just in case)
; shotmin is set at beginning of file. same as shotmax SUPER SOS

dimofvector=(nx_1+6.0)*(ny1+6.0)*(nz1+6.0)

arrayvector=dblarr(dimofvector,1)



;210819 Conditionally call the steady-state jet creation routine.
;290719 here we call the steady-state jet procedure, in order to create the model jet.
if (steady_state_switch eq 1) then begin
;290719 this bit also goes with the bit right after the time reading loop, where we set y-axis velocity for hj88 type jet!
 steady_state_jet,nx_1,ny1,nz1,bfield, ndens,denshj88,densrod,gammac,debug_comments,deltazmas

endif


;********************************************************************************************************
;210819
;fre=dblarr(14,10368)
;freavg=dblarr(1,10368)
;fre=dindgen(14,10368)
;freavg=dindgen(1,10368)
;freallyears=dblarr(14,10368,21)

;ttt is just a convenience parameter for defining the 4d arrays the bigger it is the more RAM we
;consume, yet it leaves more headroom for loose operations. ttt is now defined earlier, at the angles location 190419.
;040719 moved ttt back here, now that we dont have to do the outer loops as a separate procedure any more!
;ttt is just a convenience parameter for defining the 4d arrays the bigger it is the more RAM we
;consume, yet it leaves more headroom for loose operations
ttt=4


;210819 Here define big 4D arrays to include data. ttt param above, set to four (NOT in the param file!)  is important in terms of both preventing stalls and overloading RAM!
array_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
array=dblarr(nx_1+6,ny1+6,nz1+6)
emiss_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
kabs_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
kabs2_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
losdraw_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
;280722 SOS we add fast_proton_profile to allow separating agitated matter from stagnant matter, 
;so as to obtain synchrotron emission from agitated-activated  matter only.(no fast rpotons in bulk stagnant matter)
fast_proton_profile_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
;280722 returnski_4dauxiliary array to help calc where agitated matter is, with high content of fast protons
returnski_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
rho_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
prs_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
temp4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
bx1_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
bx2_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
bx3_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
vx1_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
vx2_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
vx3_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
v_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
; corrected sqrt in bfield, 020815 
bfield_4d=sqrt(bx1_4d*bx1_4d+bx2_4d*bx2_4d+bx3_4d*bx3_4d)
dopplerfactor_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
gammalorentz=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
ng=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
ncalc=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
coslosu_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
thetau_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
;010220 here define 4 dummy arrays, dummy1,2,3 and vortz, also used as a dummy!
dummy1_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy2_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy3_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
vortz_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)

;SOS 030720 we hereby do a conditional definition of the extra dummies! 
;SOS we further need to relate their number to maxdummy! ALSO SOS 
;maxdummy is read from the nemiss param file, so do 
;it, make it to be read from there! INTRODUCE that param file to rlos2!
if (maxdummy eq 15.0 ) then  begin 
dummy4_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy5_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy6_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy7_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy8_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy9_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy10_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy11_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy12_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy13_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy14_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy15_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy16_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy17_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
dummy18_4d=dblarr(shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
endif







; 060815 we do this assignement in order to keep ng an array sos
;220815 we keep it as is, it should work ok for the array calculation fo dopplerfactor
;
;CAREFUL 070418 ng is defined in PARAMETER REGION near the beginning of the code
ng=ng+nobs

;Now the following few comment lines are done globally! 
;
; {060815 later on we assign ng according to local doppler factor
;SUPER cannot do it globaly, as dopplerfactor is really assigned only 
;along the LOS
;therefore we can only assign ng along the LOS, not everywhere
;still we keep the assignement of the 4d ng array and doppler factor array,
;for convenience}


;array1 to b defined from string 0.
;write it down sos

;some reminder for concat here
;fluxidl=fluxdensitypachz(fre)
;14 columns, 0 to 13, 1st ans 2nd are coordinates, rest are data
;10368 rows, 0 to 10367, equal to the number  of grid points on the model global map.
; do this concat for a variable times of snapshots, i.e. for shotmax times. try 2 figure it out! e.g. write it so may times, up to a notional maximum, to be reminded above where shotmax is initialized,  then rest are zero!
;freavg=(fre[2,*]+fre[3,*]+fre[4,*]+fre[5,*]+fre[6,*]+fre[7,*]+fre[8,*]+fre[9,*]+fre[10,*]+fre[11,*]+fre[12,*]+fre[13,*])/12.;
;frepinakas=[fre[0,*],fre[1,*],freavg]

if (debug_comments eq 1) then print, nx_1,'No 1'
; this loop covers the whole lot SOS

;calculations based on pload's data 020815


v=sqrt(vx1*vx1+vx2*vx2+vx3*vx3)
;210819 again array=something??? WT... is this?
array=rho*v

;210819 Conditionally do the first part of the rod test here, second part lies on the time loop that follows shortly after!
if (rod_test_switch eq 1) then begin
;300719 wrap this rod test switch stuff, before the loop! There are some relevant things within the loop as well!
 rod_test_perform_before_time_read_loop, rho_4d_tempy,rho_4d,nx_1,ny1,nz1,bfield, ndens,denshj88,densrod,gammac,debug_comments,deltazmas

endif

;290719 MOVED THIS AFTER defining 4D stuff, BUT BEFORE TIME_READING LOOP!!! In order to use rho_4d, it must exist already! BUT NOT REPEAT THIS STUFF OVER TIME LOOP!
;Cone-like jet model also needed as a ready-made substrate for the rod_test: Again we call the steady-state jet procedure, in order to create the model jet.










;210819 here is the time-reading loop, which loads data from the data folder into the big 4D arrays defined above!
for shotindtemp=shotmin,(shotmax-1) do begin
;SUPER HERE WE SETUP TO BEGIN FROM A SHOTIND LARGER THAN ONE SOS
; 270815 we reset the following line, in order to start from shotmin
;not from 1 every time!!! SOS
;oldversion
shotind=shotindtemp+1.0-shotmin
; WE MUST PLOAD SHOTMIN+SHOTIND, BUTASSIGN JUST SHOTIND
;ASSIGNEMENT GOES FROM 0 TO SHOTMAX-SHOTMIN
;WHEREAS OPENING GOES FROM SHOTMIN TO SHOTMAX 
;SUPER HERE 270815 SOSARA
;...and new version
;shotind=shotindtemp+1.0
if (debug_comments eq 1) then print, 'lababula', shotind, shotindtemp, shotmin, shotmax
bd=string((shotind))
;ad='DLF.data'
if (debug_comments eq 1) then print, nx_1,'No 1.1'

bd=strcompress(bd,/remove_all)
;cd=bd+ad
if (debug_comments eq 1) then print, bd
; bd is string? we use shotind for ze shot number SOS

;HERE WAS THE ERROR ALL ALONG WE MUST PLOAD SHOTIND (INDEX OF 4D ARRAY)+SHOTMIN
;ELSE IT KEEPS LOADING THE FIRST SERIES OF DATA, E.G. FROM 1 TO 10 
;NEVER GOES TO HIGHER NUMBERS LATER ON,
;NO MATTER WHAT WE TELL IT TO DO 
; solved 270815



;100519 Also keep the old one without float!pload,shotmin+shotind, shrink=sfactor,/float

if (pload_float_factor eq 1.0) then begin
pload,shotmin+shotind, shrink=sfactor, dir=datapath,/float
endif else begin
pload,shotmin+shotind, shrink=sfactor, dir=datapath
endelse 

if (debug_comments eq 1) then print,'t prin', t
if (debug_comments eq 1) then print, 'dt prin',dt
if (debug_comments eq 1) then print, 'shotind loaded', shotind
; CORRECTION 20-may-14 shotind may now begin at more than 1
; RETHINK THIS INDEX -1, or plus 1? check !!
;SOS

;sos010815 test high velocity remove after testing sos
;vx1=3*vx1
;vx2=3*vx2
;vx3=3*vx3
;v=3*v



;assign current 3d arrays to corresponding slices of 4d arrays 020815

;150819 something with loading arrays, etc! please end 1st attempt by keeping copmressed ctrl shift F6. Then, rerun and it should work as normal.
print, size(array_4d,/dimensions),size(array,/dimensions)
array_4d[shotind,*,*,*]=array
rho_4d[shotind,*,*,*]=rho
;010220 SOS here we populate the newly created 4D dummy arrays (plus vortz, ALSO USED AS A DUMMY!) with their 3D counterparts read per step in the current loop!
dummy1_4d[shotind,*,*,*]=dummy1
dummy2_4d[shotind,*,*,*]=dummy2
dummy3_4d[shotind,*,*,*]=dummy3
vortz_4d[shotind,*,*,*]=vortz

;SOS 030720 we hereby do a conditional assignement of the extra dummies! 
;SOS we further need to relate their number to maxdummy! ALSO SOS 
;maxdummy is read from the nemiss param file, so do 
;it, make it to be read from there! INTRODUCE that param file to rlos2!
if (maxdummy eq 15.0) then begin

dummy4_4d[shotind,*,*,*]=dummy4
dummy5_4d[shotind,*,*,*]=dummy5
dummy6_4d[shotind,*,*,*]=dummy6
dummy7_4d[shotind,*,*,*]=dummy7
dummy8_4d[shotind,*,*,*]=dummy8
dummy9_4d[shotind,*,*,*]=dummy9
dummy10_4d[shotind,*,*,*]=dummy10
dummy11_4d[shotind,*,*,*]=dummy11
dummy12_4d[shotind,*,*,*]=dummy12
dummy13_4d[shotind,*,*,*]=dummy13
dummy14_4d[shotind,*,*,*]=dummy14
dummy15_4d[shotind,*,*,*]=dummy15
dummy16_4d[shotind,*,*,*]=dummy16
dummy17_4d[shotind,*,*,*]=dummy17
dummy18_4d[shotind,*,*,*]=dummy18

endif



;010220 SOS here the 4D dummy arrays, recently defined above, are populated! Also, WTF no dummy 3 readd from pload here?

;260719 SOS potentially call test fn's AFTER main ASSIGNment of density! Else AFTER the test's rho assignement, it gets OVERWRITTEN by main density!!! ET VOILA ZE OVAL SUPERSEDED ON BACKGROUND!


;****************************************************************************************************************
;010519 APPROACHING PERPENDICULAR ROD RELAT. IMAGING TEST. MUST WIDEN AT CENTRE, THIN AT EDGES SOS
;010519 THIS paragraph only affects the density. PLEASE adjust clight in order to make it run throughout available snapshots.
;The more temporal resolution we achieve, the better for this. For convenience, beam perp. speed is expressed as a fraction of clight here.

;200319 use the steady denshj88 cone-like model density instead of the temporal files hydro density
;210819 Conditionally use the st. state model density in place of the loaded data!
if (steady_state_switch  eq 1) then begin

rho_4d[shotind,*,*,*]=denshj88

;290719 here we adopt only axial velocity for the HJ88 model jet
;290719 had to do it AFTER time reading loop, else vy 4d would not have had existed in the first place!

;vx1_4d[shotind,*,*,*]=vx1_4d[shotind,*,*,*]*0.0d
;290719 hj88 jet goes along Y axis!!
;290719 we employ ujet_steady_state for hj88 jet, NOT clight
vx1_4d[shotind,*,*,*]=0.0D
;vx2_4d[shotind,*,*,*]=vx2_4d[shotind,*,*,*]*0.0D
vx2_4d[shotind,*,*,*]=ujet_steady_state
vx3_4d[shotind,*,*,*]=vx3_4d[shotind,*,*,*]*0.0D

;300719 here also do the magnetic field! Conival Model quotes a scalar? value for bfield? Just an approximation here pointing at a single direction
;300719 this area may be used as a basis for st. state model development!!!!
;311019 sos here added bfield stuff!!! was v merged with b, wtf!
bx1_4d[shotind,*,*,*]=sqrt(Bfield)
bx3_4d[shotind,*,*,*]=sqrt(Bfield)

;290719 OLDE STUFF HERE!!! for ref.
;if (steady_state_switch eq 1) then (vx1_4d[shotind,*,*,*]=ujet_steady_state)
;vx2_4d[shotind,*,*,*]=vx2
;;200319 if steady state is ON then only speed is along jet axis
;if (steady_state_switch eq 1) then (vx2_4d[shotind,*,*,*]= 0.0)
;vx3_4d[shotind,*,*,*]=vx3
;;200319 if steady state is ON then only speed is along jet axis
;if (steady_state_switch eq 1) then (vx3_4d[shotind,*,*,*]= 0.0)
;v_4d[shotind,*,*,*]=v
; b4d=? 020815

; v is not the same? SOMETHING CHANGES IT? SUPER CHECK IT SOS!!!
; ABOVE v value not same as corresponding v4d

endif


;210819 conditionally do second part of rod test here.(First was before current time loop)
if (rod_test_switch eq 1) then begin 
;300719 rod test switch pro that goes into time read loop.
rod_test_perform_after_time_read_loop, rho_4d,shotind,denshj88,rod_test_switch,clight,t,NX2,halfwidth,rho_4d_tempy


endif


;300319 some testing stuff
 
 ;test no 1
 array_test=[1,2,3,4,5,6,7,8,9]
if (debug_comments eq 1) then  print, (array_test gt 3)*array_test*(array_test lt 7)
  array_select = WHERE((array_test GT 4) and (array_test LT 7)) 
if (debug_comments eq 1) then  print, array_select
 
 
  ;test no 2
  aaawww=[[1,2,3],[4,5,6],[7,8,9]]
if (debug_comments eq 1) then  print, aaawww[2,1:2]
  
  
;end of testing stuff
;
;




if (debug_comments eq 1) then print, shotind
if (debug_comments eq 1) then print, rho(1,1,1), 'tsa11'
if (debug_comments eq 1) then print, rho_4d(shotind,1,1,1),'tsa22'
prs_4d[shotind,*,*,*]=prs
;temp4d[shotind-1,*,*,*]=temp
bx1_4d[shotind,*,*,*]=bx1
bx2_4d[shotind,*,*,*]=bx2
bx3_4d[shotind,*,*,*]=bx3
vx1_4d[shotind,*,*,*]=vx1
vx2_4d[shotind,*,*,*]=vx2
vx3_4d[shotind,*,*,*]=vx3
; new stuff added for rlos here
;2 B edited, they originated from clima thing i did 6 years ago.

;, shrink=2
;arrayshock=RHO*0.0
;arrayshock=shockfind(PR,V1,V2,eps_min=0.33,eps_max=5.0)
if (debug_comments eq 1) then print, shotind
if (debug_comments eq 1) THEN print,'t meta', t
if (debug_comments eq 1) then print, 'dt meta',dt
;210819 Here endof time data reading loop.
endfor









;27071 SOSPUTTHOSEAFTERTHE LOOP SOS!!!
;210819 Conditionally run a test.
if (incoming_perpendicular_rod_test_switch eq 1) then begin
;020519 we first erase density data from original CFD input data, in order to avoid 
;doing both, at separate intervals. This might xplain weird extra intensity ALONG BEAM CENTRAL LINE!
rho_4d=rho_4d*0.0D
rho_4d=perp_beam_rod_test(rho_4d,clight,nx1,ny1,nz1,focal_point_y,focal_point_z,shotmin,shotmax,debug_comments,t)
endif
;***********************************************************************************************************

;210819 Conditionally run a test.
if (incoming_planar_rod_test_switch eq 1) then begin
;020519 we first erase density data from original CFD input data, in order to avoid 
;doing both, at separate intervals. This might xplain weird extra intensity ALONG BEAM CENTRAL LINE!
rho_4d=rho_4d*0.0D
vx1_4d=vx1_4d*0.0D
;0101819 we temp use the experimental version,, which solely employs the new moving object defining procedure
;rho_4d=planar_beam_rod_test(rho_4d,clight,nx_1,ny1,nz1,focal_point_y,focal_point_z,shotmin,shotmax,debug_comments,t)
;210819 Conditionally run a test. this august 2019 thing is newer that the vanilla version of it.
planar_beam_rod_test_version_august_2019,vx1_4d,rho_4d,clight,nx_1,ny1,nz1,focal_point_y,focal_point_z,shotmin,shotmax,debug_comments,t

endif





if (debug_comments eq 1) then print, nx_1, 'No 2'
if (conditional_stop eq 1) then stop
if (debug_comments eq 1) then print, nx_1  , 'No 3'
;SUPERSOS
;next comes ALGO to find snapshot position, given current time curtime
;to be put suitably inside ze code, at algo location SOS

; 14-05-15 we do the following only every 2 los steps
;since the number of snapshots is much less than the
;number of los voxels. Therefore we do not repeat
;the curtime snapshot update check in the second0
;part of the counter los direction loop

;020815 the following has been added later on but there it interferes with the 
;algo assignement of time snapshot numbers
;consider moving to here? or if not possible, adjust the rest of relevant assignements



;040719 here initialize the tanphi arrays,, to be used in the relevant procedure!

;tan_phi1=dblarr(shotmax+ttt-shotmin,nx_1+7,ny1+7,nz1+7)
;tan_phi2=dblarr(shotmax+ttt-shotmin,nx_1+7,ny1+7,nz1+7)
;130320 SOS try reducing this to six inst. of seven! will it work? CHECKING...
;140320 setting up a margin for bot tans
tanphi12_margin=6
tan_phi1=dblarr(shotmax+ttt-shotmin,nx_1+tanphi12_margin,ny1+tanphi12_margin,nz1+tanphi12_margin)
tan_phi2=dblarr(shotmax+ttt-shotmin,nx_1+tanphi12_margin,ny1+tanphi12_margin,nz1+tanphi12_margin)

;130320 SOS the above definition has plus seven, whereas others have plus six! this difference wreaks havoc in the sizes of lxi's 
;and cascades to the sizes of bbbbb! check how to do plus six, mabe using ttt param which os set to six! SOS!
;pload4d from mathem showed us this size must be the same across the board!
;130320 SUPER SOS plus six is NOT a PARAM, it is HARDWIRED as a precaution! KINDA safety margin
;IT IS INCLUDED ALSO IN PLOAD4D in mathematica!


;**************************************
;120719 initialize lxis and phi1,phi2 to 4D array size. 
;Then, they are assigned values, conditionally, depending on the value of focused beam param, in the following called ratios_assignement procedure.
lx1_fixed=tan_phi1
lx2_fixed=tan_phi1
lx3_fixed=tan_phi1
lx1=tan_phi1
lx2=tan_phi1
lx3=tan_phi1
phi1=tan_phi1
phi2=tan_phi1


ratio1f_comparison=tan_phi1
ratio2f_comparison=tan_phi2
;**************************************
;301019 Supersos brought this call this early, cause else it would not affect lxi's and phi's globally! comparison vs nemiss saw discrepancies!
;los_origins
;160719 brought this call before 2D array definitions, cause they depend on imaging loop boundaries definitions, in the general case!
;2101819 call this routine, define image boundaries.
imaging_loop_boundaries,sliceXZ_y_location,screenXZ_z_lower_voxel,screenXZ_z_upper_voxel,screenXZ_x_lower_voxel,screenXZ_x_upper_voxel,focused_beam_switch,los_origin_x,los_origin_y,los_origin_z,focal_point_x,focal_point_y,focal_point_z,slice_x_location,screen_y_lower_voxel,screen_y_upper_voxel,screen_z_lower_voxel,screen_z_upper_voxel, nx_1,ny1,nz1, imaging_geometry_selector,first_coord_min,first_coord_max,second_coord_min,second_coord_max,first_coord,second_coord,third_coord,debug_comments
;pro imaging_loop_boundaries,sliceXZ_y_location_fun,screenXZ_z_lower_voxel_fun,screenXZ_z_upper_voxel_fun,screenXZ_x_lower_voxel_fun,screenXZ_x_upper_voxel_fun,focused_beam_switch_fun,los_origin_x_fun,los_origin_y_fun,los_origin_z_fun, focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,slice_x_location_fun,screen_y_lower_voxel_fun,screen_y_upper_voxel_fun,screen_z_lower_voxel_fun,screen_z_upper_voxel_fun,nx_1,ny1,nz1, imaging_geometry_selector_fun,first_coord_min_fun,first_coord_max_fun,second_coord_min_fun,second_coord_max_fun,first_coord_fun,second_coord_fun,third_coord_fun








;040719 SOS here we do the angles, right after reading the para file and loading ze data! So we can do the whole thing in relation to ratios here!
angles_calculation,  focal_pointXZ_x,focal_pointXZ_y,focal_pointXZ_z,ratio1f_comparison,ratio1f_comparison,lx1_fixed,lx2_fixed,lx3_fixed,lx1,lx2,lx3,focused_beam_switch,phi1,phi2,tan_phi1,tan_phi2,phi1_external,phi2_external,ratio1f,ratio2f,phi1_fixed,phi2_fixed,debug_comments,nx_1,ny1,nz1,focal_point_x,focal_point_y,focal_point_z,IMAGING_GEOMETRY_SELECTOR

;170719 added params! pro angles_calculation, focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun,ratio1f_comparison_fun,ratio2f_comparison_fun,lx1_fixed_fun,lx2_fixed_fun,lx3_fixed_fun,lx1_fun,lx2_fun,lx3_fun,focused_beam_switch_fun,phi1_fun,phi2_fun,tan_phi1_fun,tan_phi2_fun,phi1_external_fun,phi2_external_fun,ratio1f_fun,ratio2f_fun,phi1_fixed_fun,phi2_fixed_fun,debug_comments_fun,nx_1_fun,ny1_fun,nz1_fun,focal_point_x_fun,focal_point_y_fun,focal_point_z_fun

;161119 HERE TEST CURTIME CONSTRUCT!
curtime=140
if (debug_comments eq 1) then print, where((curtime-t)<0,count)
if (debug_comments eq 1) then print, 't',t
if (debug_comments eq 1) then print, size(t, /n_elements)
;next is snapshot number, give or take 1, for a current time of curtime.
if (debug_comments eq 1) then print, size(t, /n_elements)-count
if (debug_comments eq 1) then print, nx_1, 'No 4'
if (conditional_stop eq 1) then stop

;200419 we restore curtime, because now, in focused beam mode, it may be used earlier than the main loop
curtime=1




;200815 we define coordinates that point in the 
;direction of the two directional angles, as defined in the initialization sector, near the beginning of this code.
;241218 see paper for details on lx1, lx2, lx3 etc.

;200419 phi1,phi2 are now full blown 4D arrays, since in focused beam mode LOS's are NOT parallel any moar.
;but we do keep the 'fixed' angles for comparison, so as to allow use of the old 'radiograph' (parallel LOS's) method.


;if (debug_comments eq 1) then print, 'phi1_fixed phi2_fixed',phi1_fixed,phi2_fixed,'lx1_fixed lx2_fixed lx3_fixed',lx1_fixed,lx2_fixed,lx3_fixed
;ok compared lx1,lx2,lx3 vs sphericaltest file with same phi1, phi2
;and yields same coord results seems ok for phi's less than 90 degrees(verified)
;or angles less thaN 180 DEGREES (UNVERIFIED)

;;210815 we include here the previous formalism for comparison
;aaa=(xx-xx0)*ux+(yy-yy0)*uy+(zz-zz0)*uz
;bbb=sqrt((xx-xx0)*(xx-xx0) +(yy-yy0)*(yy-yy0) +(zz-zz0)*(zz-zz0) )
;ccc=sqrt(ux*ux+uy*uy+uz*uz)
;coslosu=aaa/(bbb*ccc)
;thetau=acos(coslosu)

; 060816 it seems as if at coslosu =cos (90 degrees)=0, dfactor is min (1-cccc*0)=1 =max denominator of dfactor 
;whereas at coslosu = cos(0)=1, denom is min: 1-cccc thus dfactor is maximum, as far as angle is concerned. Thus, dfactor maximizes at angles =0, 
;for given speed at each cell!
;try smallish angles for best visible dfactor effect results  therefore!


;210819 Call global calcs. Careful with the order of calling these routines here, cos they do important work!
global_calculations,phi1,phi2,vx1_4d,vx2_4d,vx3_4d,v_4d,speedtweakfactor,coslosu_4d,lx1,lx2,lx3,thetau_4d,gammalorentz_4d,ccccc,ncalc,ng,dopplerfactor_4d,debug_comments,focused_beam_switch, bx1_4d, bx2_4d, bx3_4d, coslosb_4d, sinlosb_4d, bfield_4d

;210819 Call global calcs. Careful with the order of calling these routines here, cos they do important work!
print, "here check sizes of bxi_4d global arrays AND OF COSLOSB SOS sos do it 090822"



;210819 Call global tpicked definition. Careful with the order of calling these routines here, cos they do important work!
 t_picked_definition, debug_comments,count_tpicked,backwards_in_time,tpicked,t,shotmax,shotnumber_tpicked,shotmin,t0LOS

if (backwards_in_time eq 1) then print,'backwards_in_time,t0los,t(shotmax),tpicked,shotnumber_tpicked,count_tpicked', backwards_in_time,t0los,t(shotmax),tpicked,shotnumber_tpicked,count_tpicked


;140722 here define pachz first four ci's. Meant to be constants. Perhaps consider defining them differently. 
c1_pachz=6.27*(10.0^18) 
;print, 6.27*(10.0^18)
;print, "c1_pachz",c1_pachz

c2_pachz=2.37*(10.0^(-3))
;print, 2.37*(10.0^(-3))
;print, "c2_pachz",c2_pachz

c3_pachz=1.87*(10.0^(-23))
;print, 1.87*(10.0^(-23))
;print, "c3_pachz",c3_pachz

c4_pachz=4.20*(10.0^7)
;print, 4.20*(10.0^7)
;print, "c4_pachz",c4_pachz


;stop

if (debug_comments eq 1) then print, 'max(vx1_4d)',max(vx1_4d)
if (debug_comments eq 1) then print, 'max(vx2_4d)',max(vx2_4d)
if (debug_comments eq 1) then print, 'max(vx3_4d)',max(vx3_4d)
if (debug_comments eq 1) then print, 'max(v_4d)',max(v_4d)

if (conditional_stop eq 1) then stop
;Final ratio of number of up steps to number of right steps. Final tan(phi), i.e. tan(phif). Our objective.
;100419 this must change now SOS focused beam


;model grid size:x,y,zprint, za1, za2, za3, za4, za5, za6, rz, ((xind-x1)^2+(yind-y1)^2)

;FROM NOW ON MATHEMATICA STUFF
;array = RANDOMU(seed, j, k,l)
;array=RHO
;  SO we assume only relativistic matter has
;shocks' emissions

;multiply by velocity just to see how things go



















if (conditional_stop eq 1) then stop


;print, 'tracker pos 7'



;stop



if (debug_comments eq 1) then print, nx_1











;print, array
;for jj=0,((j*j)-1) do begin
;print, jj
;arrayvector(jj)=ARRAY_INDICES(arrayc,jj)
;if (debug_comments eq 1) then print, arrayvector(jj)
;end
; print, ind, array[ind[0],ind[1]], format = '(%"Value at [%d, %d] is %f")'
;if (debug_comments eq 1) then    print, ind
;if (debug_comments eq 1) then print, ARRAY_INDICES(array, 6)
;if (debug_comments eq 1) then print, array
;if (debug_comments eq 1) then print, ARRAY_INDICES(array, location)
; HERE WE PUT TEMPORARILY THE HJ88 DENSITY INSTEAD OF THE HYDROCODE ONE!! NEEDS
;NORMALIZATION, THOUGH, IT IS EASY TO DO IN A STEADY STATE MODEL!
;array=denshj88

;020815 consider 4D to mathematica and implications

; here we select the hydro thing

;210819 oldie mathem interconnect stuff. now we have dummy var method, much better. 
;************************************************MATHEMATICA STUFF !!
array=float(RHO)
;VELOCITY AND B FIELD SCALARS AS READ BY PLOAD
arraybscalar=((float(BX1))*(float(BX1))+(float(BX2))*(float(BX2))+(float(BX3))*(float(BX3)) )^(0.5)
arrayvscalar=((float(VX1))*(float(VX1))+(float(VX2))*(float(VX2))+(float(VX3))*(float(VX3)) )^(0.5)
; here we select the HJ88 thing
;array=float(denshj88)
;*************************************1D VECTORS TO TRANSFER DATA TO MATHEMATICA
arrayvector = REFORM(array,dimofvector, 1)
arraybscalarvector = REFORM(arraybscalar,dimofvector, 1)
arrayvscalarvector = REFORM(arrayvscalar,dimofvector, 1)
resultvector=arrayvector*0.0
;print, arrayvector,resultvector
; Create some data to store in a file:

; Open a new file for writing as IDL file unit number 1:
CD, datapath+'\databis'
OPENW, 1, 'newfilelong.datab'
POINT_LUN, 1, 0
; Write the data in D to the file:
WRITEU, 1, arrayvector
; Close file unit 1:
CLOSE, 1
;magnetic field scalar value
OPENW, 3, 'newfilelongb.datab'
POINT_LUN, 3, 0
; Write the data in D to the file:
WRITEU, 3, arraybscalarvector
; Close file unit 3:
CLOSE, 3

OPENW, 4, 'newfilelongv.datab'
POINT_LUN, 4, 0
; Write the data in D to the file:
WRITEU, 4, arrayvscalarvector
; Close file unit 4:
CLOSE, 4

OPENR,2,'newfilelong.datab'
POINT_LUN, 2, 0
; ABOVE we replaced resultvector with newfilevector. in reality,
;mathematica acts on arrayvector and produces resultvector. here, as a test, they are identical.
;in reality, they are simply of same dimensions.
READU,2,resultvector
CLOSE,2
;print,'tsa', resultvector
resultvectorfinal=REFORM(resultvector,nx_1+6,ny1+6,nz1+6)
; NEED TO BRING BACK BFIELD DATA FOR LOS RUNS WITH B FIELD STUFF. TO BE DONE!
if (conditional_stop eq 1) then stop
;*****************************END OF MATHEMATICA STUFF
;2101819 here endof mathem interconnect etc. stuff.

; 020815 move all such params such as sfactor to a file, or to the beginning.210819 done that!!!

; 050515 remember to return the CD from the databis to the main data folder if needded.

;020815 here 3d(4D) verification checks
 

;Coordinates of entry point to the grid, of LOS
nx10=2
;070419 Now nx10 is screen pixel, not LOS entry point!
;nx10=2
nx10=slice_x_location
;rest shall be corrcted within loop
ny10=2
nz10=2
;Current coordinates, on our LObS (not neccessarily the ideal LOS, of course)
nx1current=nx10
ny1current=ny10
nz1current=nz10
;arrays of initial grid data: model geometry and the like. couls also be input from hd runs output.
;variable to hold total intensity, the sum or voxel intensities along the los.
in=0.0


;setup the cone axis x1,y1 and the small cone radius r1 and the large one, r2
;the cone is parallel to z.z1=bottom, z2=top of the cone
;r1,r2 must be less than half of the smallest of nx1,ny1
;z1 must be less than z2
;both z1 and z2 must be more than nzo but less than nz1


;251218 next portion are 2008 comments from 3D LOS code.
;;********************************************************
;Define whole grid now. Our arrays are now 3D, way much bigger. For every point in our grid, there are the
;emissivity and abrospivity for the radiation. These may originate from a steady state model, ala HJ88. The
;geometry of such a model may be directly imposed by us (a CAD feat in 3D!), in the definition of the following arrays.
;Or, it may come from the results of hydrodynamic (HD) simulations, using a relation than connects dynamic
;flow properties (pressure, density, velocity, etc) to radiative properties, like e and k.
;Regridding may be necessary, as the grid dimensions of the HD calcs
;are different, in general, from the radiative flow calcs
;(usually bigger, for such distant astronomical systems! what we see
;is fainter than what a decent HD run requires.
;The rest are just working assumptions, for the sake of not using a 40 by 40 HD grid on our nice PCs!)
;For now, we simply define the 3D arrays  at random values, and  in future versions this may be changed to a model 3D
;geometry. We could have bent jets, rings, clouds, whatever.
;*********************************************************

;emissivity
;280919 hhere is dimension of 4D array: reform to THAT! coslosu (angles do it matematica, write to dummy file!) (shotmax+ttt-shotmin,nx_1+6,ny1+6,nz1+6)
if (debug_comments eq 1) then print, nx_1+6,ny1+6, nz1+6
emiss=dblarr(nx_1+6,ny1+6, nz1+6)

emiss=rho
;emiss=emiss+2.0




;  180816 what is this freqfactor thing? isnt freq effect just an extra D^alphaindex effect? 
; 170915 we now assign the frequency shift to the emission SOS

; 180816 mallon edo exoume mia apopeira sysxetisis tis entasis me ti sixnotita, ti allo na paizei pia eleos! freqfactor kiar***ies. gm ti mnimi mou mesa!
;if (freqshiftchoice eq 1.0) then (  freqfactor=(ncalc/2.)^((1.-gammac)/2.))  else  (  freqfactor=(ng/2.)^((1.-gammac)/2.))
;190816 we set a square dependence, in order to better highlight results in the loglog plot
;270618 replaced below n/n exponent from 2 to alphaindex SOS
;290618 NOT NEEDED THE ABOVE REPLACEMENT FREQFACTOR IS A SPECTRUM EFFECT< SOMETHING EXTRA REALLY JUST TURN IT OFF FOR THE APPLICATION AND ALSO DO JUSTIFY IT BY HAND CALCULTION  
; 270718 the ratio ncalc/ng is weakening the whole thing, better choice perhaps ng/ncalc%(ln(a)), or something. It suggests a spectrum countering the extra D^(a) below at dfactor 
;but it is soothing, so keeps the scale it check for the tests say it in paper SOS.

;251218 At long last: freq shift in indirectly, through the implied spectrum, 
;incorporated in the DB formula that follows, it is there.

;300718 this is important bit, so we do it a procedure
;210819 call freqshift pro. 
frequency_shift_effect,freqshiftchoice,freqfactor,ncalc,ng,alphaindex_external,kappa_spectr,kappa_spectr2,jet_spectrum_built_in_function_test_unity_at_108


;SO 290618 freqfactor is not FS on off, it is something else. alpha is already there in Db choice SOS

; 170816 to 1-gammac mas ta xalaei, ta loipa fainontai ok mexri stigmis. nailed it?


;120816 diereynisi
;print, max((ncalc/2.)^((1.-gammac)/2.), /NaN)  this run 0.00017258052
;print, max((ng/2.)^((1.-gammac)/2.), /NaN)  this run     0.00014427007

; 120816 look at this!! it is using gammac!!! but what exactly is this gammac! 
;preset to 1.8!!! should it be gammalorentz??? check!! 

;print, max(((1.-gammac)/2.), /NaN)  -0.400000
;print, min(((1.-gammac)/2.), /NaN)  -0.400000
 ;print,(((1.-gammac)/2.))   -0.40
 
 ;SOSO 120816 FROM HERE CONTINUE TO investigate SOS!!!
 

; 310815 we now assign dopplerboosting to the emission
;for now we keep density as a base for the emission coefficient
;ULTRA CLARIFY alphaindex PLUS OR MINUS? SUPER HERE!!!

;180816 looks like a plus to me now vs wiki pedia page for relbeaming
;180816 keep the old onef
;
;; here we get to perform the emission calculation. 
;if (dopplerchoice eq 1.0) then (  emiss_4d=rho_4d*((dopplerfactor_4d)^(2+alphaindex))*freqfactor  ) else (emiss_4d=rho_4d*freqfactor)
;180816 try out something else
;040718 here we set D^(2+a) for Cawthorn's case of opt. thin cont jet or opt. thick non-lapping blobs. Else, set D^(3+a) for normal case!
;if (dopplerchoice eq 1.0) then (  emiss_4d=rho_4d*((dopplerfactor_4d)^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=rho_4d*freqfactor);; THIS IS THE ONE USED so farSOS
;270718  we hereby check the values of dfactor  THE ABOVE IS THE CORRECT SOS
;; 270718 we may uncomment any of the following, in order to debug the dfactor, esp. when altering the phi1 ange above. phi1 MUST be NEG. and large, 
;if we are to achieve D boosting SOS
;if (dopplerchoice eq 1.0) then (  emiss_4d=((dopplerfactor_4d)^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)
;if (dopplerchoice eq 1.0) then (  emiss_4d=((   (gammalorentz_4d/dopplerfactor_4d)*coslosu_4d)^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)
;if (dopplerchoice eq 1.0) then (  emiss_4d=(   (1/(1-ccccc*coslosu_4d))^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)
;if (dopplerchoice eq 1.0) then (  emiss_4d=(   (gammalorentz_4d)^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)
;nxt is same as dfactor 
;if (dopplerchoice eq 1.0) then (  emiss_4d=(   (1/(gammalorentz_4d*(1-ccccc*coslosu_4d)))^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)
;if (dopplerchoice eq 1.0) then (  emiss_4d=(   (gammalorentz_4d)^(3.0+alphaindex))*freqfactor  ) else (emiss_4d=freqfactor)

;290719 here call the emission cal procedure!
;210819 Indeed!

;280722 here we calc fast rpoton profile 4d array, to be used for sync emiss.
;function fast_proton_profile_function, u_fast_proton_lower_threshold, velocity, z_fast_proton_threshold_index, returnski_4d

;290722 sos here we temporarily define proton profile external params. Eventually, these shall be put in the external param file!
;not anymore, they moved to param file now!
;u_fast_proton_lower_threshold_external=0.03
;z_fast_proton_threshold_index_external=1.0

fast_proton_profile_4d=fast_proton_profile_function(u_fast_proton_lower_threshold_external,v_4d,z_fast_proton_threshold_index_external, returnski_4d)
;stop
emiss_global_calc, dopplerchoice,emiss_4d,rho_4d,dopplerfactor_4d,alphaindex,freqfactor,spectrum_direct_switch,jet_spectrum_built_in_function_test_unity_at_108,debug_spectra_switch,emiss_4d_implied,emiss_4d_direct,dummy2_4d,dummy3_4d,pion_emissivity_switch,neutrino_emissivity_switch,synchrotron_emissivity_switch, alphaindex_external, v_4d, sinlosb_4d, bx1_4d, bx2_4d, bx3_4d, bfield_4d, nobs, c1_pachz, c2_pachz, c3_pachz, c4_pachz,fast_proton_profile_4d, kappa_value_power_law, neutrino_energy_selector, dummy4_4d, dummy5_4d, dummy6_4d, dummy7_4d, dummy8_4d, dummy9_4d, dummy10_4d,dummy11_4d,dummy12_4d, dummy13_4d, dummy14_4d, dummy15_4d
; pro emiss_gal_calc, dopplerchoice,emiss_4d,rho_4d,dopplerfactor_4d,alphaindex,freqfactor,spectrum_direct_switch,jet_spectrum_built_in_function_test_unity_at_108,debug_spectra_switch,emiss_4d_implied,emiss_4d_direct,dummy2_4d_fun,dummy3_4d_fun,pion_emissivity_switch,neutrino_emissivity_switch, synchrotron_emissivity_switch



;160819 series of calling: 
;1.global calculations 
;2. freq shift effect
;3. emiss global calc

;(1-ccccc*coslosu_4d)
;120816 SUPER SOSARA EDO  this shows that something is wrong with the multipication factor really! 
;It should be much larger than one, not this smaller!!! WTF!

if (debug_comments eq 1) then  print, max( ((dopplerfactor_4d)^(2+alphaindex))*freqfactor,/NaN)
 if (debug_comments eq 1) then print, min( ((dopplerfactor_4d)^(2+alphaindex))*freqfactor,/NaN)
 if (debug_comments eq 1) then print, max(dopplerfactor_4d, /NaN)
if (debug_comments eq 1) then print, max(dopplerfactor_4d^(2+alphaindex), /NaN)
 if (debug_comments eq 1) then print, max( ((dopplerfactor_4d)^(2+alphaindex))*freqfactor,/NaN)
; 120816 ayto fenetai na exei to problima!!! ARA DES LIGO PIO PANO GIA DIEREYNISI
if (debug_comments eq 1) then print, max(freqfactor, /NaN)
if (debug_comments eq 1) then print, max( ((2+alphaindex))*freqfactor,/NaN)
; 4D-FY EVERYTHING, IN A NEAT MANNER SOS

;SOS 290622 EDO ETOIMO SOS to sync emiis apo palia, to theloume kai pali SUPER SOS this and pacholczyk SOS
; 
;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.))
;emiss_4d=cp5(0.)*rho_4d*(bfield_4d*sin(theta))^((gammac+1.)/2.)*((ng/2.)/cp1(0.))^((1.-gammac)/2.))

;2101819 Call global absorption pro! Requires editing and adapting for various abs. mechanisms. Work on that routinew for future applications! Some samples to be found commented out in the routine itself!
global_absorption,Temp_4d,PRS_4d,RHO_4d,dopplerfactordivisor,xfrac,gaunt,ng,emiss,resultvectorfinal,kabs_4d,kabs2_4d,NLONG, synchrotron_emissivity_switch, alphaindex_external, v_4d, sinlosb_4d, bx1_4d, bx2_4d, bx3_4d, bfield_4d, nobs, c1_pachz, c2_pachz, c3_pachz, c4_pachz, dopplerfactor_4d, kappa_value_power_law, fast_proton_profile_4d
;pro emiss_global_calc, dopplerchoice,emiss_4d,rho_4d,dopplerfactor_4d,alphaindex,freqfactor,spectrum_direct_switch,jet_spectrum_built_in_function_test_unity_at_108,debug_spectra_switch,emiss_4d_implied,emiss_4d_direct,dummy2_4d_fun,dummy3_4d_fun,pion_emissivity_switch,neutrino_emissivity_switch, synchrotron_emissivity_switch, alphaindex_external, v_4d, sinlosb_4d, bx1_4d, bx2_4d, bx3_4d, bfield_4d, nobs, c1_pachz, c2_pachz, c3_pachz, c4_pachz, fast_proton_profile_4d, kappa_value_power_law




losdraw=dblarr(nx_1+6,ny1+6, nz1+6)
;print, 'tracker pos 8'
;loop along the LOS, up to a maximum size of grid diagonal, or till you reach either side of grid(in the latter case, exit with break condition).
;1st calc of ratios before loop, for optimization!
;
;
;070419 SOS  rethink uc, rc, cc from focal point starting now SOS! 
;160719 uc also from 1, to cover the XZ versions as well!
uc=1
; current 'right' step counter begins at 1, i.e. one first step right, saves div by 0! We presume that it is intrinsically
;accounted for in the loops later on in the code.
rc=1
;similar for current 'climb' counter, z-dimension. (NOTE: This could be made to begin at zero, had we wished to do so.(SOS: AND HAD ACCOUNTED FOR IT IN THE LOOPS)
cc=0;Initialize current ratio of up to right steps, at zero (i.e. (uc/rc)=0).
;This way, 2nd step is always up (first was right), as it has to increase ratioc up from zero, in the if construct.
;similarly for second tan(phi2) ratio2c
ratio1c=0.0D
ratio2c=0.0D
;
;
;
;use real numbers for ratios.
ratio1c=(double(uc)/double(rc))

;print, 'tracker pos 9'
a1=double(cc)
b1=double(rc)
c1=double(uc)
d1=sqrt(b1*b1+c1*c1)
e1=a1/d1
ratio2c=e1
;print, 'tracker pos 10'
;this bit adjusts the corresponding kength along the los.
;the formulae are:
;dll(counter)=dlup*sin(phi1)*cos(phi2) if up(y)
;=dlright*cos(phi1)*cos(phi2) if right(x)
;=dlclimb*sin(phi2) if climb(z)
;phi1=goes from x to y
;phi2=goes from xy plane to z
;define array of lrnghts along the los each element is the correspondig dl for that grid increment along the los.
ll=dblarr(nx_1+ny1+nz1+5)
;define 3 separate scales for 3 axes. They are the same for now, else we must change the angles sin and cod and tan definitios.
;For now, strictly same scales for x, y, z.


;NOT FOR PLUTO GRID SOS! 35/250*100/750*35/250
;i.e. smaller cell side along jet

;301217 WE ADD SCALING FACTOR SFACTOR TO THE dlc, dlu, dlr cell lengths (in cell length units).




sf1=sin(phi1)
sf2=sin(phi2)
cf1=cos(phi1)
cf2=cos(phi2)



;
;161218 FROM HERE BEGINS DIFFERENTIATION BETWEEN xz AND yz VERSIONS.
;***********************************************************


;070419 Now nx10 is screen pixel, not LOS entry point!
;
;nx10=2
;210819 this is left alone. is it used? We leave it as such for now!
nx10=slice_x_location

;160719 SOS the following are defined for the yz case, NOT for the general case SOS CORRECT THAT!


;**********************************************
;160719 copied here for reference this bit(XZ radiograph case!)!
;
;first_coord_min_fun=1
;first_coord_max_fun=nz1-2
;
;;030719 we cut some (minus 1) in order to hopefully prevent it from overshjooting coord
;second_coord_min_fun=1
;second_coord_max_fun=nx_1-2
;
;;SOS 170619 third coord is just location of imaging plane, be it the fiducial screen (camera obscura) or the box side (radiograph).
;third_coord_fun=1 ;meaning y0=1!
;*************************************************
;220619 we hereby call ze function than defines imaging method and screen limits
;020719 the aforementioned function is now called imaging_loop_boundaries. 
;los_origins is now called WITHIN the imaging loop, once for each line of sight!




;2101819 define 2D image arrays of various types here!
;160719 added 6 instead of the original 4, cause we define with minus 2 the maxes of coords!
eikona=dblarr(second_coord_max+6,first_coord_max+6)
;200719 now define a stagnation count for each los, aka a stagnation image! As oppose dto the existing,scalar, global stagnation count!
stagnant_signal=dblarr(second_coord_max+6,first_coord_max+6)
;220719 A separate diagnostic for each ratio criterion!
stagnant_signal2=dblarr(second_coord_max+6,first_coord_max+6)
;200719 We form an -diagnostic- image of geometric los lengths!
llos_2d=dblarr(second_coord_max+6,first_coord_max+6)
eikonatau=dblarr(second_coord_max+6,first_coord_max+6)
;270419 also define reverses 
eikona_reverse=dblarr(second_coord_max+6,first_coord_max+6)
eikona_reverse_unlengthed=dblarr(second_coord_max+6,first_coord_max+6)
eikonatau_reverse=dblarr(second_coord_max+6,first_coord_max+6)
;array to hold the number of voxels of each LOS. This is different foe LOS's near the edges of the grid, at low LOS angles.
; LLOS/counterlast=length per voxel=ll, where counterlast=counter at los exit
counterlast=dblarr(second_coord_max+6,first_coord_max+6)
;array to hold los length as calculated from difference in position from end and start of LOS, according to the formula:
; LLOS=sqrt(((nx-nxo)^2)+((y-y0)^2)+9z-z0)^2))

loslength=dblarr(second_coord_max+6,first_coord_max+6)
;180722 we do this to avert los's with no length oops
;loslength=dblarr(second_coord_max+6,first_coord_max+6)+1.0




;090819 some reversedefs here!
eikona_reverse_unlengthed_fun=eikona*0.0D
eikona_reverse_fun=eikona*0.0D
eikonatau_reverse_fun=eikona*0.0D


;print, 'xe xe '
;*************************************************************************************************************








; according to idl manual, inner part of the loop must be outer part of
;the array indices, in order to access the arrays as they are stored in
;memory. This goes for 2d arrays, we try it 4 3d here?

;060419  TWO THINGS NOW: 
; 1. Make both loops to only run within screen size voxels (i.e. less than originally!). 
; 2. SOS alter the angles within the loops, so that angles are poniting along the direction:
;    FocalPoint to current pixel



;060419 SOS We already have taken care of minus 6 stuff(screen size restricted originally):

;nx_1=NX1-6
;ny_1=NX2-6
;nz_1=NX3-6

;ny1=NX2-6
;nz1=NX3-6

;060419 COOL! screen subsizing seems to work fine for parallel los's. 
;now lets do this for focused beam, whilestill marching FW in time.
;(best use with clight=very high, i.e. non-R, must reverse cos no time symmetry for non parallel rays. Only for parallel ones does parallel transport works OK. )

;200419
;no need for that, now we got shotnumber set to the tpicke done, at the beginning of this code.
;shotnumber=1.0



;2101819 huge indicator array may show each los in a 3D setting! one 3D array per image pixel! only use in small resolutions, else blows memory requirements! Controlled from external param file!
;270419
ll=(ll-ll)+1.0
;110519 SOS onmly employ in small resolutions, else out of mem! HUGE ARRAY including all loses in own 3D space! 
;MUST be a cheaper, in terms of RAM, way of doing this! FIND IT!
;290519 pre set it to a scalar, in order to facilitate parameter passing it to the los function 
rho_indicator_total=0.0
if (use_huge_indicator_array eq 1) then begin
rho_indicator_total=fltarr(ny1+7,nz1+7,nx1+1,ny1+1,nz1+1)
rho_indicator_total=rho_indicator_total*0.0
endif
; 110519 next, define a 3D array where, for each LOS, only the current LOS's voxels are non-zero! (must reset to zero B4 each OS begins!) 
rho_indicator=dblarr(nx1+2,ny1+2,nz1+2)


;describe the LOS:
;Define VECTOR (i.e. 1D) arrays, at a size that is larger than any possible value of the LOS length in the grid.
;A convenient choice for such a size is the grid diagonal.
;Optical depth, so far till current point, along (i.e. at every LOS point) our LOS
lostdepth=dblarr(nx_1+ny1+nz1)
;emission coefficients for each point along our LOS
lose=dblarr(nx_1+ny1+nz1)
;absorption coefficients for every point along our LOS
losk=dblarr(nx_1+ny1+nz1)
;an auxiliary array
;losdraw=dblarr(nx_1,ny1,nz1)
;160719 updated definition, was defined twice, only came up when doing xz radiograph because of resulting long LOS
losdraw=dblarr(nx_1+6,ny1+6, nz1+6)

;220619 old nested loops kept here, commented out, for safety
;for nz10=screen_z_lower_voxel,screen_z_upper_voxel do begin
;for ny10=screen_y_lower_voxel,screen_y_upper_voxel do begin
;160619 SOS here preset, JUST ONCE, the imaging loop limits! adjust the relevant pro to this task!

;180619 replace with these, all over the loop, no more nx1o etc, just first, second, third coord !
;180619 SOS first coord is OUTER-always Z, SECOND COORD IN INNER, either X or Y BUT KEEP GENERALIZED COORDS, in order to preserve GENERALITY (e.g. in future perhaps xy plane, etc etc!)
;220619 no keep both generalized: leave z as is, for simplicity! In future, may only replace z directly with a first coord!
if (debug_comments eq 1) then print,first_coord_min,first_coord_max,second_coord_min,second_coord_max


;080719 this to find out what is the density distro right before the imaging loops!
ivolume, reform(rho_4d[1,*,*,*])

if (debug_comments eq 1) then stop

;210819 IMAGING LOOP of RLOS HERE! 
for nz10=first_coord_min,first_coord_max do begin
;for first_coord_fun=first_coord_min_fun,first_coord_max_fun do begin
for second_coord=second_coord_min,second_coord_max do begin


;if (show_loops_switch__screen_boundaries eq 1.0) then print,'screen_y_lower_voxel,screen_y_upper_voxel',screen_y_lower_voxel,screen_y_upper_voxel,'screen_z_lower_voxel,screen_z_upper_voxel',screen_z_lower_voxel,screen_z_upper_voxel 
;if (show_loops_switch eq 1.0) then print,' nz10: ', nz10,' ny10: ', ny10
;210819 los ORIGINS called here!
los_origins,focal_pointXZ_x,focal_pointXZ_y,focal_pointXZ_z,nz10,los_origin_x,los_origin_y,los_origin_z,focal_point_x,focal_point_y,focal_point_z,slice_x_location,screen_y_lower_voxel,screen_y_upper_voxel,screen_z_lower_voxel,screen_z_upper_voxel, nx_1,ny1,nz1, imaging_geometry_selector,first_coord_min,first_coord_max,second_coord_min,second_coord_max,first_coord,second_coord,third_coord,sliceXZ_y_location

;170719 altered param list: pro los_origins,focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun,nz10_fun,los_origin_x_fun,los_origin_y_fun,los_origin_z_fun, focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,slice_x_location_fun,screen_y_lower_voxel_fun,screen_y_upper_voxel_fun,screen_z_lower_voxel_fun,screen_z_upper_voxel_fun,nx_1,ny1,nz1, imaging_geometry_selector_fun,first_coord_min_fun,first_coord_max_fun,second_coord_min_fun,second_coord_max_fun,first_coord_fun,second_coord_fun,third_coord_fun


if (show_loops_switch eq 1.0) then print,' nz10: ', nz10,' second_coord: ', second_coord

 
;print, 'ok os edo 1'







;060419 SOS now attempt to change the angles according to current pixel vs FPoint. 
;SOS 060419 ONLY ratio1f and ratio2f need be altered, no phi's! supereasy!
;SOS ALSO DO ADD A SECOND STEP TO CHECK RATIOS S O NO EVERY OTHER STEP DO THAT LATER BUT STILL DO IT 

;SUPER SOS 060419 check the ratios calc on paper, ESP. ze geom of the second ratio!



;focal_point_x=0
;focal_point_y=fix(0.500*ny_1)
;focal_point_z=fix(0.500*nz_1)
;slice_x_location




;130419 moved these calcs to functions see beginning of code 
;SOS 100419 this is float so convert it to double SOS else wrong 
;xy_length_unsigned_no_sign_needed_here = double(sqrt( deltay_interim_signed^2 + deltax_interim_signed^2  )) 

;SOS 100419 this is float so convert it to double SOS else wrong 

;ratio1f= double(deltay_interim_signed)/double((slice_x_location-focal_point_x))  
;270619 SUPER SOS! here, ratios are still drawn using ny10, whereas we now have second coord for that. nz10 ok as is! REPLACE NY10!


;2101819 conditionally calc ratio1,2 (tans of azimuth and elevation)  for all four geometry cases.
IF (imaging_geometry_selector EQ 1) OR (imaging_geometry_selector EQ 2)  THEN BEGIN 
;130719 cases 3 and 4 are rasiograph cases

ratio1f=tan(phi1_fixed)
ratio2f=tan(phi2_fixed)

ENDIF ELSE BEGIN 

;170719 split the following into two separate ifs, one for each cam obsc geometry!
IF (imaging_geometry_selector EQ 4)   THEN BEGIN 
;130719 cases 3 and 4 are camera obscura cases

;130719 please note how in case of cam obsc, ratios are re-calced here, not based on globAL tan phi calcs in the angles initialization procedure!
;130719 MIGHT BE A GOOD IDEA TO COMPARE THE TWO, i.e. ratios from here and ratios from there! TRY IT SOS!!!
ratio1f=double(ratio1f_function(slice_x_location,second_coord,nz10,focal_point_x,focal_point_y,focal_point_z,debug_comments))

ratio2f=double(ratio2f_function(slice_x_location,second_coord,nz10,focal_point_x,focal_point_y,focal_point_z,debug_comments))


;FUNCTION ratio2f_function,ny10_fun,nz10_fun,focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,debug_comments_fun

endif

if (imaging_geometry_selector EQ 3) then begin

;130719 cases 3 and 4 are camera obscura cases

;130719 please note how in case of cam obsc, ratios are re-calced here, not based on globAL tan phi calcs in the angles initialization procedure!
;130719 MIGHT BE A GOOD IDEA TO COMPARE THE TWO, i.e. ratios from here and ratios from there! TRY IT SOS!!!
ratio1f=double(ratio1f_functionXZ(sliceXZ_y_location,second_coord,nz10,focal_pointXZ_x,focal_pointXZ_y,focal_pointXZ_z,debug_comments))

ratio2f=double(ratio2f_functionXZ(sliceXZ_y_location,second_coord,nz10,focal_pointXZ_x,focal_pointXZ_y,focal_pointXZ_z,debug_comments))


if (debug_comments eq 1) then print,'ratio1f,ratio2f',ratio1f,ratio2f

;FUNCTION ratio2f_functionXZ,sliceXZ_y_location_fun,nx10_fun,nz10_fun,focal_pointXZ_x_fun,focal_pointXZ_y_fun,focal_pointXZ_z_fun,debug_comments_fun


;FUNCTION ratio2f_function,ny10_fun,nz10_fun,focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,debug_comments_fun



endif



ENDELSE 


if (debug_comments eq 1) then print, 'rc,uc,cc',rc,uc,cc,'tsatsa1'



;070419 Now nx10 is screen pixel, not LOS entry point!
;nx10=2
;220619 commented this out, already defined above!
;nx10=slice_x_location


;print, 'ok os edo 2'
; before every LOS integration, reset los intensity, in, back to zero
in=0.0D
;reset optical depth as well!
taun=0.0D



;SOS 090519 for timed operations, we do not want the LOS to carry on forever till end of the grid!
;COC it may run out of either timesteps, or space!. SO we must relate endlosloop to size and duration of dataset!(perhaps including perp_beaM FUNCTION!)
;sos DOI IT! 090519
;210819 This thingy here was messing with inverse los calc, cos it was not true length of los, disturbing any vector symmetry of inversion. fixed eventually in inverse los, yet careful there!
endlosloop=double((sqrt(nx_1*nx_1 + ny1*ny1 + nz1*nz1) + 1.0)*zigzag_factor)*LOOP_LENGTH_FACTOR_FOR_TIMING_RELATED_TESTS
;110519 B4 each LOS, reset LOS indocator 3D array to zero. Then, only LOS voxels shall have non-zero value. Plot with ivolume, from within a LOS! 
;Employ a conditional stop or something!
rho_indicator=rho_indicator*0.0D

; print, 'ola edo ta params', nx10, ny10,nz10, eikona , eikonatau , eikona_reverse , eikona_reverse_unlengthed , eikonatau_reverse , rho_indicator , rho_indicator_total , use_huge_indicator_array , counterlast , taun ,mikoscell ,in ,ll , nx1currentlast , ny1currentlast , nz1currentlast ,debug_comments , plutocelllength ,lcell ,shotnumber ,clight , emiss_4d ,kabs_4d , nx_1 ,nx1 ,ny1 ,nz1 ,ratio1f ,ratio2f ,cc ,rc ,uc ,shotmin ,shotmax ,t ,shotnumber_tpicked ,endlosloop , nx1current ,ny1current ,nz1current , focal_point_x ,focal_point_y ,focal_point_z ,dlr ,dlc ,dlu 

; stop
 

 print, 'clight', clight

;020719 next procedure does the line of sight 1D loop, which must now employ 
;the results from los_origins, called above, also within the
;2D imaging loops.
;210819 call losloop to do the LOS.
losloop_fun,loslength,path_algo_set,llos_2d,stagnant_signal,stagnant_signal2,lloscurrent,t0los,imaging_geometry_selector,backwards_in_time,los_origin_x,los_origin_y,los_origin_z,first_coord,second_coord,third_coord, nx2,nx3,shotind ,tan_phi1 ,tan_phi2 ,error_stagnant_los ,lostdepth ,lose ,losk ,losdraw  , RHO_INDICATOR_VALUE , nx10 , ny10 ,nz10 , rho_indicator , rho_indicator_total , use_huge_indicator_array , counterlast , taun ,mikoscell ,in ,ll , nx1currentlast , ny1currentlast , nz1currentlast ,debug_comments , plutocelllength ,lcell ,shotnumber ,clight , emiss_4d ,kabs_4d , nx_1 ,nx1 ,ny1 ,nz1 ,ratio1f ,ratio2f ,cc ,rc ,uc ,shotmin ,shotmax ,t ,shotnumber_tpicked ,endlosloop , nx1current ,ny1current ,nz1current , focal_point_x ,focal_point_y ,focal_point_z ,dlr ,dlc ,dlu,focal_pointXZ_x,focal_pointXZ_y,focal_pointXZ_z, bx1_4d, bx2_4d, bx3_4d

print, 'in after losloop, before division with loslength function', in
;pro losloop_fun,  ,los_origin_x,los_origin_y,los_origin_z,first_coord_fun,second_coord_fun,third_coord_fun,nx2_fun,nx3_fun,shotind_fun,tan_phi1_fun,tan_phi2_fun,error_stagnant_los_fun,lostdepth_fun,lose_fun,losk_fun,losdraw_fun,lastcounter_fun, RHO_INDICATOR_VALUE_fun, nx10_fun, ny10_fun,nz10_fun, eikona_fun, eikonatau_fun, eikona_reverse_fun, eikona_reverse_unlengthed_fun, eikonatau_reverse_fun, rho_indicator_fun, rho_indicator_total_fun, use_huge_indicator_array_fun, counterlast_fun, taun_fun,mikoscell_fun,in_fun,ll_fun, nx1currentlast_fun, ny1currentlast_fun, nz1currentlast_fun,debug_comments_fun, plutocelllength_fun,lcell_fun,shotnumber_fun,clight_fun, emiss_4d_fun,kabs_4d_fun, nx_1_fun,nx1_fun,ny1_fun,nz1_fun,ratio1f_fun,ratio2f_fun,cc_fun,rc_fun,uc_fun,shotmin_fun,shotmax_fun,t_fun,shotnumber_tpicked_fun,endlosloop_fun, nx1current_fun,ny1current_fun,nz1current_fun, focal_point_x_fun,focal_point_y_fun,focal_point_z_fun,dlr_fun,dlc_fun,dlu_fun


;170719 WTF is this relic? This only works for YZ cam obscura!
;llos=sqrt( ((dlc*(nz1currentlast-focal_point_x))^2)+((dlu*(ny1currentlast-focal_point_y))^2)+((dlr*(nx1currentlast-focal_point_z))^2) )
;170719 Try this:
;llos=sqrt( ((dlc*(nz1currentlast-los_origin_x))^2)+((dlu*(ny1currentlast-los_origin_y))^2)+((dlr*(nx1currentlast-los_origin_z))^2) )
;180719 NO! Try THIS! (using the general llos function!)
;210819 Had some trouble with which los length is which here! Solved eventually! in was divided by zero at some point!(see a little below in/loslength)
llos=lloscurrent
if (debug_comments eq 1) then print,'SCREEN POINT FORCAMOBSC,OR ELSE RADIOGR. LOSORIGIN: second_coord,nz10',second_coord,nz10
if (debug_comments eq 1) then print,'focal_point_x,focal_point_y,focal_point_z,los_origin_x,los_origin_y,los_origin_z', focal_point_x,focal_point_y,focal_point_z,los_origin_x,los_origin_y,los_origin_z
;170719 SOS all adjustements for length from zig zag were wrong for radiograph cases! SOS!!!
;los_origin_x,los_origin_y,los_origin_z
if (debug_comments eq 1) then print, second_coord, nz10,'tsa'
if (debug_comments eq 1) then print, ny10, nz10,' ny10, nz10',second_coord,third_coord,'second_coord,third_coord'
if (debug_comments eq 1) then print, ny1current, nz1current,'tsa'
if (debug_comments eq 1) then print, counterlast(second_coord,nz10),'edo to counterlast'
if (debug_comments eq 1) then print, llos, 'edo to llos'
;280619 sos here we have infinite loslength, messes with in result!
;280619 check counterlast what is it over gere? is it ok an array? 
;060719 no more last counter!
if (debug_comments eq 1) then print, 'counterlast(second_coord,nz10)',counterlast(second_coord,nz10)
if (debug_comments eq 1) then print, 'counterlast(second_coord,nz10),llos,loslength(second_coord,nz10)',counterlast(second_coord,nz10),llos,loslength(second_coord,nz10)
loslength(second_coord,nz10)=(counterlast(second_coord,nz10)/llos)
;llos=lloscurrent
in=in/loslength(second_coord,nz10)
if (debug_comments eq 1) then print, in, 'INEDOORESOS
if (debug_comments eq 1) then print, taun
eikona(second_coord,nz10)=in

if (debug_comments eq 1) then print, in,eikona(second_coord,nz10), 'teos'
eikonatau(second_coord,nz10)=taun

;030719 just tried removing counterlast from the chain, directly assing counterlast array element to counter, within los loop procedure
;counterlast(second_coord,nz10)=lastcounter

;300519 os edo DONE ! remains to do SEPARATE PROCEDURE for the reverse LOS SOS DO IT !!!


;100819 In main body, after all the reverse calc!Eikona_reverse calcs OUTSIDE of it!
;090819 here attempt to call reverse los when its switch is turned on!
;090819 HERE wemust put the option for reverse calc for back-in-time absoprtion!!! Within the los loop procedure itself! But AFTER the  1D los loop, right before the end of the MOTHER los pro!
if (reverse_los_switch eq 1.0) then begin 
;210819 Conditionally call reverse los.
los_reverse,in_reverse, taun_reverse,llos_2d,lose_reverse,losk_reverse,endlosloop,loslength,nz10,second_coord,lose,losk,nx_1,ny1,nz1,ll,endlosloop,mikoscell,debug_comments
; lose_fun,losk_fun,nx_1_fun,ny1_fun,nz1_fun,ll_fun,endlosloop_fun,mikoscell_fun,debug_comments_fun,eikona_fun, eikonatau_fun,eikona_reverse_unlengthed_fun,eikona_reverse_fun,eikonatau_reverse_fun


eikonatau_reverse(second_coord,nz10)=taun_reverse
eikona_reverse_unlengthed(second_coord,nz10)=in_reverse
;090819 REPLACED THIS WITH THE NEXT LINE,since llos_2d in clearly non-zero here!eikona_reverse_fun(second_coord_fun,nz10_fun)=in_reverse/loslength_fun(second_coord_fun,nz10_fun)
eikona_reverse(second_coord,nz10)=in_reverse/llos_2d(second_coord,nz10)
;if (debug_comments eq 1) then print, in,eikona(ny10,nz10), 'teos'
eikonatau_reverse(second_coord,nz10)=taun_reverse


if (debug_comments eq 1) then print,'in_reverse before division by loslength(second_coord,nz10)',in_reverse
;090819 REPLACED THIS WITH THE NEXT LINE,since llos_2d in clearly non-zero here!eikona_reverse_fun(second_coord_fun,nz10_fun)=in_reverse/loslength_fun(second_coord_fun,nz10_fun)
if (debug_comments eq 1) then print,'in_reverse AFTER division: in_reverse/loslength(second_coord,nz10)/loslength(second_coord,nz10),loslength(second_coord,nz10)',in_reverse/loslength(second_coord,nz10),loslength(second_coord,nz10)
;if (debug_comments eq 1) then print, in,eikona(ny10,nz10), 'teos'
if (debug_comments eq 1) then print,'llos_2d(second_coord,nz10)',llos_2d(second_coord,nz10)

;110819 EXTENDED THE IF OF REVERSE LOS CALC TILL HERE, IN ORDER TO ONLY ACTIVATE ALL THE ABOVE REVERSE STUFF 
;IF WE ACTUALLY WANT IT! NOT ALWAYS!
endif


;if (debug_comments eq 1) then print,lose,lose



openw, 16, datapath+'\databis\eikona1.eikona'
if (debug_comments eq 1) then printf, 16, second_coord,nz10,eikona
close, 16

endfor

endfor
;210819 END OF 2D IMAGING LOOP!
;
;print, ratio1f, ratio2f












;this plots slices of bfield along the jet axis, thus displaying cross
;sections of the cone. Helps see where things are in 3D. For e.g. the bent jet stuff, etc.
!P.MULTI=[0,2,3]
!P.NOERASE=1

;here plot either ndens or bfield
;it works ok for bfield only so far
;for tttt=10, 40, 10 do begin
;tvscl, emiss(*,*,tttt), (tttt/10-1)
;endfor

;tvscl, emiss(*,nx_1/2,*)
!P.MULTI = 0
!P.NOERASE=0
;print, ll
;print, din
; should en absorption be proportional to current intensity? no negs should be possible for intensity!! Check again
;eqn of rad transfer, quietly !

if (steady_state_switch eq 1) then begin
;310719 here plot jpf for hj88 steady state
;310719 Careful, jet is now along y, not z any more!!! Careful!!!
indexed=dindgen(ny1+4)
indexedmas=double(ny1+4)
eikonaread=dblarr(ny1+4,nz1+4)
jetprofile=dblarr(ny1+4)
;2101819 what is this thingy? JPF from HJ88!
 jet_profile_function_steady_state, eikonaread,jetprofile,indexed,indexedmas,deltazmas,eikona,eikona2

endif


;surface, alog(eikona^0.333), ax=75
;surface, ((eikona)), ax=45, az=-190, MAX_VALUE=30, MIN_value=1
;surface, ((eikona)), ax=55, az=-190, /zlog, MAX_VALUE=10, MIN_value=0.0001, xstyle=2, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2

;shade_surf, ((eikona)),  ax=55, az=-190, /zlog, MAX_VALUE=10, MIN_value=0.0001, xstyle=4, $
;ystyle=4, zstyle=4, /NOERASE, background=255, color=0
;surface, ((eikonatau)), ax=55, az=-190, /zlog, MAX_VALUE=10, MIN_value=0.0001

;surface, ((eikonatau)), ax=20, az=-170, /zlog, MAX_VALUE=0.00001, MIN_value=0.00000000000000001, xstyle=4, $
;xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0

;openw, 26, 'C:\code\code\eikonafilehighres.txt'
;printf, 26, eikona
;close, 26
;openr, 26, 'C:\code\code\eikonafiletauhighres.txt'
;readf, 26, eikonatau
;close, 26
;surface, ((eikonatau)), ax=20, az=-170, /zlog, MAX_VALUE=0.00001, MIN_value=0.00000000000000001
;shade_surf, ((eikonatau)), ax=20, az=-170, /zlog, MAX_VALUE=0.00001, MIN_value=0.00000000000000001, xstyle=8, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2



;surface, ((eikonatau)), ax=20, az=-170, /zlog, MAX_VALUE=0.00001, MIN_value=0.00000000000000001, xstyle=8, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2
; edo to teleytaio sxima SOS
;surface, ((eikona)), ax=35, az=-150, /zlog, MAX_VALUE=100, MIN_value=0.01, xstyle=2, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2


;surface, ((eikonatau)), ax=20, az=-160, /zlog, MAX_VALUE=0.00001, MIN_value=0.000000000000001, xstyle=8, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2

;surface, ((eikonatau)), ax=20, az=-170, /zlog, MAX_VALUE=0.00001, MIN_value=0.00000000000000001, xstyle=8, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2
; edo to teleytaio sxima SOS
;surface, ((eikona)), ax=35, az=-150, /zlog, MAX_VALUE=100, MIN_VALUE=0.01, xstyle=2, $
;ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
;ythick=2, zthick=2, xcharsize=2, ycharsize=2, zcharsize=2

;
;********************************************************************************
;211218 In the following we annotate the plot with some of the parameters, 
;in a formatted output mode.
;250419 temporarily stop image cause it gave hang!
;290622 following only goes when case 3 or 4, else lxi are scalars and gives error.
;print, coslosu_4d(1,31,45,44),dummy1_4d(1,31,45,44),dummy2_4d(1,31,45,44),lx1(1,31,45,44),vx1_4d(1,31,45,44),lx2(1,31,45,44),vx2_4d(1,31,45,44),lx3(1,31,45,44),vx3_4d(1,31,45,44)
stop


;140320 some verification stuff here, vs pload4d in mathem.
;140320 vxi are now DISALIGNED vs mathematica all else look OK
print, vx1_4d(2,26,12,28),vx2_4d(2,26,12,28),vx3_4d(2,26,12,28),phi1(2,26,12,28),phi2(2,26,12,28),coslosu_4d(2,26,12,28)
print, vx1_4d(3,0,3,20),vx2_4d(3,0,3,20),vx3_4d(3,0,3,20),phi1(3,0,3,20),phi2(3,0,3,20),coslosu_4d(3,0,3,20)
;210819 from now on do the imaging part! Main calcing is over!
shade_surf, ((eikona)), charsize=1.5, ax=35, az=-150, PIXELS=1024, /zlog, MAX_VALUE=100000000000000, MIN_VALUE=100000, xstyle=2, $
ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
ythick=2, zthick=2, xcharsize=3, ycharsize=3, zcharsize=3.5
 if (debug_comments eq 1) then print, 'total jet gamma ray intensity, yet unnormalized,',total(eikona, /NaN)
  if (debug_comments eq 1) then print, 'max element excluding NaNs', max(eikona, /NaN)

;here some interim vars to trim up things
clight_temp=string(clight, format='(E19.3)')
clight_temp_trim=strtrim(clight_temp,2)

sfactor_temp=string(sfactor, format='(E19.3)')
sfactor_temp_trim=strtrim(sfactor_temp,2)

shotmin_temp=string(shotmin, format='(E19.3)')
shotmin_temp_trim=strtrim(shotmin_temp,2)

shotmax_temp=string(shotmax, format='(E19.3)')
shotmax_temp_trim=strtrim(shotmax_temp,2)
;SOS 250419 phi1, phi2 are now huge 4D arrays! cant try to print them here! NOT in the focused beam version! SOS

;phi1_temp=string(phi1, format='(E19.3)')
;phi1_temp_trim=strtrim(phi1_temp,2)

;phi2_temp=string(phi2, format='(E19.3)')
;phi2_temp_trim=strtrim(phi2_temp,2)

speedtweakfactor_temp=string(speedtweakfactor, format='(E19.3)')
speedtweakfactor_temp_trim=strtrim(speedtweakfactor_temp,2)

dopplerchoice_temp=string(dopplerchoice, format='(E19.3)')
dopplerchoice_temp_trim=strtrim(dopplerchoice_temp,2)

freqshiftchoice_temp=string(freqshiftchoice, format='(E19.3)')
freqshiftchoice_temp_trim=strtrim(freqshiftchoice_temp,2)

plutolength_temp=string(plutolength, format='(E19.3)')
plutolength_temp_trim=strtrim(plutolength_temp,2)

clight_override_temp=string(clight_override, format='(E19.3)')
clight_override_temp_trim=strtrim(clight_override_temp,2)

XYOUTS, charsize=1.8, color=0, 1, 520 ,'clight_override='+clight_override_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 500 ,'length_unit='+plutolength_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 480 ,'FS_switch='+freqshiftchoice_temp_trim, /DEVICE
XYOUTS, charsize=1.8, color=0, 1, 460 ,'clight='+clight_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 440 ,'DB_switch='+dopplerchoice_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 420 ,'ts='+speedtweakfactor_temp_trim, /DEVICE   
XYOUTS, charsize=1.8, color=0, 1, 400, 'sfactor='+sfactor_temp_trim, /DEVICE  
XYOUTS, charsize=1.8, color=0, 1, 80, 'shotmin='+shotmin_temp_trim, /DEVICE 
XYOUTS, charsize=1.8, color=0, 1, 60, 'shotmax='+shotmax_temp_trim, /DEVICE  
;290419 commented these next two out, for the focused beam version of rlos
;XYOUTS, charsize=1.8, color=0, 1, 40, 'phi1='+phi1_temp_trim, /DEVICE  
;XYOUTS, charsize=1.8, color=0, 1, 20, 'phi2='+phi2_temp_trim, /DEVICE 


;clight_temp=string(clight, format='(E19.3)')
;print, strtrim(clight_temp,2)
;******************************************************************
;150419 Estimate of total stagnant los steps (for whole run)
print, 'Estimate of total stagnant los steps (for whole run)', error_stagnant_los
;190320 SOS these allow to begin exploration of dummy 1 (read from mathem output) versus coslos4_4d (calced here!)
;190320 1-2 comparison of random datapoints between rlos and 
;nemiss_pload4d seem to achieve a macth by now, for all the 4 imaging geometries!
;But we need to work with how data are read from rlos, and how they are output from mathem!
;Achieve the final match of dummy 1 and coslosu here!
print, min(coslosu_4d/ dummy1_4d,/NaN)
print, max(coslosu_4d/ dummy1_4d,/NaN)
stop
;280722 here is a test step for the fast proton fraction function dependence on speed, early fproton fn, scalar only, not working now with array-op version!..
;fastutski_proton=fast_proton_profile_function( 0.11, 0.01, 0.000001, 1.0)
surface, eikona, ax=75,/zlog, xcharsize=3, ycharsize=3, zcharsize=3
;170722 find nans in kabs_4d:none for synchrotron pachz

PRINT, WHERE(FINITE(kabs_4d, /nan))
stop
;170722 find nans in emiss_4d: plenty for emiss_4d in synchr pachz
PRINT, WHERE(FINITE(emiss_4d, /nan))
PRINT, WHERE(FINITE(eikona, /nan))
stop
;first part of emiss_4d looks ok 170722
  test_emiss_4d_part1=(c5_pachz(gammac))*rho_4d*( ( bfield_4d*sinlosb_4d )^( (gammac+1.)/2.) )
  PRINT, WHERE(FINITE(test_emiss_4d_part1, /nan))
stop
;second part of emiss_4d has problem 170722

  test_emiss_4d_part2=((nobs/2.)/c1_pachz)^((1.-gammac)/2.)*(dopplerfactor_4d*dopplerfactor_4d)
  PRINT, WHERE(FINITE(test_emiss_4d_part2, /nan))
  stop
  ;170722 third part is ok
test_emiss_4d_part3=((nobs/2.)/c1_pachz)^((1.-gammac)/2.) 
PRINT, WHERE(FINITE(test_emiss_4d_part3, /nan)) 
stop
;part 4 problem dfactor
test_emiss_4d_part4=(dopplerfactor_4d*dopplerfactor_4d)
PRINT, WHERE(FINITE(test_emiss_4d_part4, /nan))
stop

  
  
  stop
shade_surf, eikona, ax=75,/zlog, xcharsize=3, ycharsize=3, zcharsize=3
stop
shade_surf, eikona, ax=75, xcharsize=3, ycharsize=3, zcharsize=0
stop
surface, eikona, ax=75, xcharsize=3, ycharsize=3, zcharsize=0
stop
surface, eikona_reverse, ax=75,/zlog, xcharsize=3, ycharsize=3, zcharsize=3
stop
shade_surf, ((eikona)), ax=35, az=-150, /zlog, MAX_VALUE=1000000000000000, MIN_VALUE=100000000, xstyle=2, $
ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $f
ythick=2, zthick=2, xcharsize=3, ycharsize=3, zcharsize=3
  if (debug_comments eq 1) then print, 'total jet gamma ray intensity, yet unnormalized,',total(eikona, /NaN)
  if (debug_comments eq 1) then print, 'max element excluding NaNs', max(eikona, /NaN)
stop
shade_surf, ((eikona)), ax=35, az=-90, /zlog, MAX_VALUE=100000000000000, MIN_VALUE=100000, xstyle=2, $
ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
ythick=2, zthick=2, xcharsize=3, ycharsize=3, zcharsize=3
stop
;200719 we now may plot counterlast(cell count length of each los) and loslength(los length adjusted for counterlast)
shade_surf, ((counterlast)), ax=35, az=-110, /zlog, xstyle=2, MAX_VALUE=1000, MIN_VALUE=1, $
ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
ythick=2, zthick=2, xcharsize=3, ycharsize=3, zcharsize=3
stop


stop
; 130418 IT WORKS, opens with GIMP2 SUPER  
shade_surf, ((eikona/eikona_reverse)), /zlog, ax=35, az=-150, xstyle=2, $
ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
ythick=2, zthick=2, xcharsize=3, ycharsize=3, zcharsize=3
 if (debug_comments eq 1) then print, 'total jet gamma ray intensity, yet unnormalized,',total(eikona, /NaN)
  if (debug_comments eq 1) then print, 'max element excluding NaNs', max(eikona, /NaN)
  
  stop
  
shade_surf, ((eikona^0.6)), ax=35, az=-150,  MAX_VALUE=1500000, MIN_VALUE=1000, xstyle=2, $
ystyle=2, zstyle=2, xtitle='X', ytitle='Y', ztitle='Z', xticklayout=0, background=255, color=0, xthick=2, $
ythick=2, zthick=2, xcharsize=4, ycharsize=4, zcharsize=4
 if (debug_comments eq 1) then print, 'total jet gamma ray intensity, yet unnormalized,',total(eikona, /NaN)
  if (debug_comments eq 1) then print, 'max element excluding NaNs', max(eikona, /NaN)
  
  
  
 ;*********************************************************************************
;200719 Here we plotcountelast and loslength for each LOS, in order to remove trhe valley across the jet axis from the -now smooth- cam obsc YZ images. 
;200719 We are also looking to fix cam obsc XZ neg angles' Inf's. 
surface, ((counterlast)), ax=35, az=-110, /zlog,MAX_VALUE=1000, MIN_VALUE=1
stop
surface, ((counterlast)), ax=35, az=-110, /zlog,MAX_VALUE=1000, MIN_VALUE=1
stop
 surface, ((loslength))
stop
surface, ((loslength)), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4
stop

;200719 we now plot llos:the result of the following division
;200719 lloscurrent_fun=lloscurrent_function(debug_comments_fun, dlr_fun,dlu_fun,dlc_fun,nx1current_fun,ny1current_fun,nz1current_fun,los_origin_x,los_origin_y,los_origin_z)
;;200719llos=lloscurrent
;200719loslength(second_coord,nz10)=(counterlast(second_coord,nz10)/llos)
surface, ((counterlast/loslength)), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4
stop
surface, ((counterlast/loslength)), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4
shade_surf, ((counterlast/loslength)), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4
;200719 CYCLICAL ARGUMENT?llos,loslength!!!checkitout!!!
surface, (stagnant_signal), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4
stop
surface, (stagnant_signal2), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4
stop
shade_surf, ((counterlast)), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4
stop
shade_surf, ((loslength)), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4
stop
shade_surf, ((llos_2d)), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4
STOP
shade_surf, ((llos_2d)), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4,/zlog,Min_value=1
stop
shade_surf, ((llos_2d)), ax=35, az=-90, xcharsize=4, ycharsize=4, zcharsize=4,/zlog,Min_value=1
stop
;200719 Et voila! Ze unity!
 shade_surf, ((counterlast/loslength)-LLOS_2D), ax=35, az=-110, xcharsize=4, ycharsize=4, zcharsize=4
stop


;*************************************
;;*******************************************************
;200719 So, loslength is the total LOS step count, divided by the actual length of the LOS in 3D space! Makes up for excessive zig zag! Is here from 2008! From the 3D, if notr from the 2D version of the los code!
;We divide total los intensity, in, with that! Now see how that works with stagnation!
;***********************************************200719 So, loslength is the total LOS step count, divided by the actual length of the LOS in 3D space! Makes up for excessive zig zag! Is here from 2008! From the 3D, if notr from the 2D version of the los code!
;We divide total los intensity, in, with that! Now see how that works with stagnation!
;200719 here we go! length correction effects and stagnation in pathfinding! How do they affect in? How does the 'valley' along the jet, in cam obsc YZ appear?
;;REVERSE CALC COURSE, LIKE A HUMAN PUZZLE SOLUTION!
;;in=in/loslength(second_coord,nz10)
;loslength(second_coord,nz10)=(counterlast(second_coord,nz10)/llos)
;llos=lloscurrent
;lloscurrent_fun=lloscurrent_function(debug_comments_fun, dlr_fun,dlu_fun,dlc_fun,nx1current_fun,ny1current_fun,nz1current_fun,los_origin_x,los_origin_y,los_origin_z)

;
;function lloscurrent_function, debug_comments_fun, dlr_fun,dlu_fun,dlc_fun,nx1current_fun,ny1current_fun,nz1current_fun,los_origin_x_fun,los_origin_y_fun,los_origin_z_fun
;;290619 this lloscurrent  function is aimed to operate for all cases of system geometry, as it simply employs start and end of current los position in the grid.
;
;;sos 140619 here add a function to calc llos, one for radiograph, another for camera obscure, such as the case here!
;
;;perhaps define a point of origin coordinate trio, either for radiograph, or for cam_obscura, then do the rest the same, only the coords differ!
;
;;cam_obscura llos:lloscurrent_local=sqrt( ((dlc_fun*(nz1current_fun-focal_point_z_fun))^2)+((dlu_fun*(ny1current_fun-focal_point_y_fun))^2)+((dlr_fun*(nx1current_fun-focal_point_x_fun))^2) )
;lloscurrent_local=sqrt( ((dlc_fun*(nz1current_fun-los_origin_z_fun))^2)+((dlu_fun*(ny1current_fun-los_origin_y_fun))^2)+((dlc_fun*(nx1current_fun-los_origin_x_fun))^2) )
;
;lloscurrent_local=double(lloscurrent_local)
;if (debug_comments_fun eq 1) then  print, 'nx1current_fun,ny1current_fun,nz1current_fun,dlr_fun,dlu_fun,dlc_fun,los_origin_x_fun,los_origin_y_fun,los_origin_z_fun',nx1current_fun,ny1current_fun,nz1current_fun,dlr_fun,dlu_fun,dlc_fun,los_origin_x_fun,los_origin_y_fun,los_origin_z_fun
;return, lloscurrent_local
;end


;200719 So, loslength is the total LOS step count, divided by the actual length of the LOS in 3D space! Makes up for excessive zig zag! Is here from 2008! From the 3D, if notr from the 2D version of the los code!
;We divide total los intensity, in, with that! Now see how that works with stagnation!
;**************************************

;********************************************************************************
;290419 TEST GOOD!
print, (-losk_reverse*ll_reverse*in_reverse+lose_reverse*ll_Reverse)/(-losk*ll*in+lose*ll)
print, median(eikona/eikona_reverse,/nan)
print, max(eikona/eikona_reverse,/nan)
print, min(eikona/eikona_reverse,/nan)  
stop



;010519 REFORM can be used to remove “degenerate” leading dimensions of size one
;REFORM can be used to remove "degenerate" leading dimensions of size one. Such dimensions can appear when a subarray is extracted from an array with more dimensions. For example 

 


;for iggy=1,10,1 do begin 
;ivolume, reform(rho_4d[iggy,*,*,*])
;endfor


print, where(reform(rho_4d[1,*,*,*]) ge 1000000000)
;010519 SOS DO SMOOTH AFTER RENDERING ET VOILA!
ivolume, reform(rho_4d[3,*,*,*])
;010519 SOS!
print, where(reform(rho_4d[1,*,focal_point_y,focal_point_z]) ge 10.0)

stop
 ivolume, reform(rho_4d[203,*,*,*]),AUTO_RENDER,OVERPLOT=1
ivolume, reform(rho_4d[193,*,*,*]),AUTO_RENDER,OVERPLOT=1
 ivolume, reform(rho_4d[183,*,*,*]),AUTO_RENDER,OVERPLOT=1
 ivolume, reform(rho_4d[173,*,*,*]),AUTO_RENDER,OVERPLOT=1



stop


print,'backwards_in_time,t0los,t(shotmax),tpicked,shotnumber_tpicked,count_tpicked', backwards_in_time,t0los,t(shotmax),tpicked,shotnumber_tpicked,count_tpicked

print, max (emiss_4d_implied/emiss_4d_direct,/nan)
print, min (emiss_4d_implied/emiss_4d_direct,/nan)





































;100220 SOS here example 2 from idl help, shows how to transpose an array SPECIFICALLY swapping dims!

;Example 2 
;This example demonstrates multi-dimensional transposition: 
;
;; Create the array:
;A = INDGEN(2, 3, 4)
;
;; Take the transpose, reversing the order of the indices:
;B = TRANSPOSE(A)
;
;; Re-order the dimensions of A, so that the second dimension
;; becomes the first, the third becomes the second, and the first
;; becomes the third:
;C = TRANSPOSE(A, [1, 2, 0])
;
;; View the sizes of the three arrays:
;HELP, A, B, C 
;
;IDL prints: 
;
;A   INT  = Array[2, 3, 4] 
;B   INT  = Array[4, 3, 2] 
;C   INT  = Array[3, 4, 2] 

; 100220 based on the above example, we do the following AND IT FRIGGIN WORKS! vortz is tyransposed so as to only swap x,z. WE GOTTA DO THIS HERE, leave MATHEM out and PERHAPS TURN OFF THOSE xz reversal params in nemiss param file SOS!
trans_vortz_4d=transpose(vortz_4d,[0,3,2,1])
trans_rho_4d=transpose(rho_4d,[0,3,2,1])
print, size(trans_vortz_4d,/dimensions),size(vortz_4d,/dimensions),size(dummy1_4d,/dimensions)
;SOS it works! from mathem array huraaaah, increasing x increments value in thousands, y in units and z in percents. 
;When checking that with the transpose above, it WORKS THE SAME AS IN MATHEM!!! 
;SO WE GOT IT! 
;ALL DUMMIES MUST NOW BE TRANSPOSED
;in rlos, after being re-read, after having being influenced from mathem, in order to have
;the correct form! FRom then on, we may employ em as intented, aka emission coeff, cell metrics, etc!

;210320 seems to work for sample data point, especially using array indices, as above! vs the mathematica corresponding result!
;************************************************************
;IDL> print, where(  (dummy1_4d) eq .60477006)
;     1127305
;IDL> print, where(  abs(  (dummy1_4d)-0.65512) le 0.000001)
;          -1
;IDL> print, where(  abs(  (dummy1_4d)-0.65512) le 0.0001)
;     1558825
;IDL> print, coslosu_4d(1558825)
;      0.65512568
;IDL> print, dummy1_4d(1558825)
;      0.65512568
;IDL> print, dummy2_4d(1558825)
;  1.1470677e+012
;IDL> print, where(  abs(  (dummy1_4d)-0.65512) le 0.0001,/location)
;% Keyword LOCATION not allowed in call to: WHERE
;% Execution halted at: $MAIN$           7398
;  Q:\gitstuff\torblobgoodhope\rlos210_210320.pro
;IDL> location= where(  abs(  (dummy1_4d)-0.65512) le 0.0001)
;IDL> print, location
;     1558825
;IDL> indyc=array_indices(dimms,location,/dimensions)
;IDL> print, indyc
;           1          33          47          32
;**********************************************************




;***********************************************************
;190320 find out non-zero elements of dummy1_4d array (depends on how much wee left it running in mathem!)
print, dummy1_4d(where(  (dummy1_4d) ne 0))
print, where(  (dummy1_4d) ne 0)
print, where(  (dummy1_4d) eq .60477006)

;190320 SOS these give the same position for the same value, for the same array! 
;THIS SUPPOSEDLY SOLVES THE PROBLEM! BUT: why dont those numbers ;
;appear at the sME 3D position?
;190320 1D positioning is the same, but NOT 3D one! 
;CAREFUL HERE, check also with huraaah, how is in 1D terms? TRY to 
;understand how data are transfereed between the codes, 
;based on both hurah and this 1D thingy!!
print, where(  abs(  (coslosu_4d)-0.6047700) le 0.000001)
print, where(  abs(  (dummy10_4d)-0.6047700) le 0.000001)
;200320
print,size(coslosu_4d,/dimensions),size(dummy1_4d,/dimensions)
;***********************************************************

;200320 SOS this finds out location of maximu. 
;Can also be done for finding out location where 
;difference from a given value is almost zero!

;200320 SOS this location thing stores the current position here for the max.
print, max(dummy1_4d,location)
;or:
;;***********************************************************************
;200320B these three here! REPLACE VALUE OF COSLOSU with one read from mathematica output! 
;then pinpoint if resulting coord is same as from mathem!
importiva_coslosu=0.771628

location= where(  abs(  (coslosu_4d)-importiva_coslosu) le 0.000001)
locatione= where(  abs(  (dummy1_4d)-importiva_coslosu) le 0.000001)

print, locatione
print, location
stop
dimms=size(dummy1_4d,/dimensions)
indya=array_indices(dimms,locatione,/dimensions)
indyc=array_indices(dimms,location,/dimensions)

print,dummy1_4d(indya(0),indya(1),indya(2),indya(3))
print,coslosu_4d(indya(0),indya(1),indya(2),indya(3))

print,dummy1_4d(indyc(0),indyc(1),indyc(2),indyc(3))
print,coslosu_4d(indyc(0),indyc(1),indyc(2),indyc(3))
;SOS 200320B diagreement here between coords output from above 3 lines(correct)
;and mathem coords! whereas other arrays are OK! SOS dummy 1, what is wrong? 
;SOS CHECK IT, trnsposing etc. vs huraah, etc!!! dummy1! SOS!
;next we insert mathem coords! they are different than the ones we find here! 
;200320B SOS CORRECT THIS!
print, coslosu_4d(1,33,48,23)


;**********;***********************************************************
;*******************************************
print, total(dummy1_4d),total(dummy2_4d),total(dummy3_4d),total(dummy4_4d),total(dummy5_4d),total(dummy6_4d),total(dummy7_4d),total(dummy8_4d),total(dummy9_4d),total(dummy10_4d),total(dummy11_4d),total(dummy12_4d),total(dummy13_4d),total(dummy14_4d),total(dummy15_4d),total(dummy16_4d)
; VERIFIED! 250.82052  1.0503297e+011       760497.03  2.8625803e+012  8.0730247e+009       39630813.       281857.08       2510.1980       12.652383  2.7487977e+010     0.048752672  1.4107683e-005
;  3.1846782e-006  9.0161236e-008  4.1208800e-010      0.00000000
stop

end