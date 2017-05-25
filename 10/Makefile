obj = ex1001.o 
ifor=$(shell which ifort)
.PHONY: mr clean e r
ifneq ($(ifor),)
1001: $(obj)
	ifort -o 1001 ex1001.o 
$(obj): ex1001.f90
	ifort ex1001.f90 -c
else
1001: $(obj)
	gfortran -o 1001 ex1001.o 
$(obj): ex1001.f90
	gfortran ex1001.f90 -c
endif
r:
	-@make && ./1001
clean:
	-@rm ./1001
	-@rm *.o
	-@rm data.* result.txt
e:
	@vim ex1001.f90
