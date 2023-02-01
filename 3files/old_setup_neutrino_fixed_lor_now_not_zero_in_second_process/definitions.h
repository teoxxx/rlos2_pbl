#define  PHYSICS                 RMHD
#define  DIMENSIONS              3
#define  COMPONENTS              3
#define  GEOMETRY                CARTESIAN
#define  BODY_FORCE              NO
#define  COOLING                 POWER_LAW
#define  RECONSTRUCTION          LINEAR
#define  TIME_STEPPING           HANCOCK
#define  DIMENSIONAL_SPLITTING   YES
#define  NTRACER                 0
#define  USER_DEF_PARAMETERS     8

/* -- physics dependent declarations -- */

#define  EOS                     IDEAL
#define  ENTROPY_SWITCH          NO
#define  DIVB_CONTROL            NO

/* -- user-defined parameters (labels) -- */

#define  CS_WIND                 0
#define  RHO_AMB                 1
#define  CS_AMB                  2
#define  V_CSM                   3
#define  RHO_IN                  4
#define  BETA                    5
#define  BM                      6
#define  RM                      7

/* [Beg] user-defined constants (do not change this line) */

#define  UNIT_DENSITY            1.67e-24
#define  UNIT_LENGTH             1.0e10
#define  UNIT_VELOCITY           3.0e10

/* [End] user-defined constants (do not change this line) */

/* -- supplementary constants (user editable) -- */ 

#define  INITIAL_SMOOTHING         YES
#define  WARNING_MESSAGES          NO
#define  PRINT_TO_FILE             NO
#define  INTERNAL_BOUNDARY         YES
#define  SHOCK_FLATTENING          MULTID
#define  CHAR_LIMITING             NO
#define  LIMITER                   MINMOD_LIM
#define  ASSIGN_VECTOR_POTENTIAL   NO
#define  UPDATE_VECTOR_POTENTIAL   NO
#define  PRIMITIVE_HANCOCK         NO
