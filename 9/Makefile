ex=0901
ex_=ex$(ex)
obj = $(ex_).o 
ifor=$(shell which ifort)
.PHONY: mr clean e r
ifneq ($(ifor),)
$(ex): $(obj)
	ifort -o $(ex) $(ex_).o 
$(obj): $(ex_).f90
	ifort $(ex_).f90 -c
else
$(ex): $(obj)
	gfortran -o $(ex) $(ex_).o 
$(obj): $(ex_).f90
	gfortran $(ex_).f90 -c
endif
r:
	-@make && ./$(ex)
clean:
	-@rm ./$(ex)
e:
	@vim $(ex_).f90
