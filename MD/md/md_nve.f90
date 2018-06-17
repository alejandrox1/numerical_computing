! md_nve.f90
! Molecular DYnamics, NVE Ensemble.
PROGRAM md_nve
    !-------------------------------------------------------------------------!
    ! Copy of md_nve_lf.f90 from "computer Simulations of Liquids" by Allen   !
    ! Tildesley.                                                              !
    !-------------------------------------------------------------------------!
    
    ! Takes as input a configuration of atoms (positions and velocities).
    ! It utilizes cubic periodic boundary conditions.
    ! Conducts moleuclar dynamics using the velocity verlet algorithm.
    ! Uses no special neighbour list.

    ! Reads several variables and options from standard input using a namelist
    ! nml. If namelist is left empty then a set of supplied defaults will be 
    ! used.

    ! Positions, r, are divided by the box length after reading in and we 
    ! assume mass=1 throughout the simulation.
    ! Input configurations, output configurations, and all calculations and 
    ! results are given in simulation units defined by the model. For example,
    ! Lennard-Jones sigma=1 and epsilon=1.

    ! The model is defined in md_module (i.e., Lennard-Jones).

    USE, INTRINSIC :: iso_fortran_env,  ONLY : input_unit, output_unit, error_unit, iostat_end, iostat_eor
    !USE               config_io_module, ONLY : read_cnf_atoms, write_cnf_atoms
    !USE               averages_module,  ONLY : run_begin, run_end, blk_begin, blk_end, blk_add
    USE               md_module,        ONLY : introduction, potential_type !, conclusion, allocate_arrays, deallocate_arrays, &
    !    &                                      force, r, v, f, n, potential_type

    IMPLICIT NONE

    ! Important variables.
    REAL :: box   ! Box lenght.
    REAL :: dt    ! Time step.
    REAL :: r_cut ! Potential cut-off distance.

    ! Composite interaction = pot & cut & vir & lap & over variables.
    TYPE(potential_type) :: total

    INTEGER            :: blk, stp, nstep, nblock, ioerr
    REAL, DIMENSION(3) :: vcm

    CHARACTER(len=4), PARAMETER :: cnf_prefix = "cnf"
    CHARACTER(len=3), PARAMETER :: inp_tag    = "inp"
    CHARACTER(len=3), PARAMETER :: out_tag    = "out"
    CHARACTER(len=3)            :: sav_tag    = "sav" ! May be overwriten by block number.

    NAMELIST /nml/ nblock, nstep, r_cut, dt

    WRITE( unit=output_unit, fmt="(a)" ) "md_nve_lj"
    WRITE( unit=output_unit, fmt="(a)" ) "Molecular Dynamics, NVE Ensemble"
    WRITE( unit=output_unit, fmt="(a)" ) "Particle mass=1 throughout"
    CALL introduction

END PROGRAM md_nve
