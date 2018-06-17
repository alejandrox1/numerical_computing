! md_lj_module.f90
! Force routine for MD simulations, Lennard-Jones atoms.
MODULE md_module

    USE, INTRINSIC :: iso_fortran_env, ONLY : output_unit, error_unit

    IMPLICIT NONE
    PRIVATE

    ! Public routines.
    PUBLIC :: introduction

    ! Public data.
    INTEGER,                           PUBLIC :: n ! Number of atoms.
    REAL, DIMENSION(:,:), ALLOCATABLE, PUBLIC :: r ! Positions (3,n).
    REAL, DIMENSION(:,:), ALLOCATABLE, PUBLIC :: v ! Velocities (3,n).
    REAL, DIMENSION(:,:), ALLOCATABLE, PUBLIC :: f ! Forces (3,n).

    ! Public derived type.
    ! A composite variable for interactions.
    TYPE, PUBLIC :: potential_type
        REAL    :: cut ! Potential energy cut (but not shifted) at r_cut.
        REAL    :: pot ! Potential energy cut-and-shifted at r_cut.
        REAL    :: vir ! Virial.
        REAL    :: lap ! Laplacian.
        LOGICAL :: ovr ! Flag to indicate overlap (i.e., pot to high to use).
    CONTAINS
        PROCEDURE :: add_potential_type
        GENERIC   :: OPERATOR(+) => add_potential_type
    END TYPE potential_type

CONTAINS

    ! add_potential_type returns as a result the sum of the two potentials 
    ! passedas input.
    FUNCTION add_potential_type(a, b) RESULT (c)
        IMPLICIT NONE
        TYPE(potential_type)              :: c
        CLASS(potential_type), INTENT(in) :: a, b
            c%cut = a%cut  +   b%cut
            c%pot = a%pot  +   b%pot
            c%vir = a%vir  +   b%vir
            c%lap = a%lap  +   b%lap
            c%ovr = a%ovr .OR. b%ovr
    END FUNCTION add_potential_type

    ! introduction subroutine prints out information about the simulation.
    SUBROUTINE introduction
        IMPLICIT NONE

        WRITE ( unit=output_unit, fmt="(a)" ) "Lennard-Jones potential"
        WRITE ( unit=output_unit, fmt="(a)" ) "Cut-and-shifted version for dynamics"
        WRITE ( unit=output_unit, fmt="(a)" ) "Cut (but not shifted) version also calculated"
        WRITE ( unit=output_unit, fmt="(a)" ) "Diameter, sigma = 1"
        WRITE ( unit=output_unit, fmt="(a)" ) "Well depth, epsilon = 1"
    END SUBROUTINE introduction


END MODULE md_module
