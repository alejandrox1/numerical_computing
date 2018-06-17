FC=gfortran
FCFLAGS=-g -fbounds-check 
FCFLAGS=-O2
LDFLAGS=

EXEC=md_nve


all: $(EXEC)

$(EXEC): md_lj_module.o 

md_nve.o: md_lj_module.o



%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

.PHONY: clean cleanall

clean: 
	rm -f *.o *.mod

cleanall: clean
	rm -f $(EXEC)
