ex=0901
ex_=ex$(ex)
obj = $(ex_).o 
ifor=$(shell which ifort)
.PHONY: mr clean e r
ifneq ($(ifor),)
1001: $(obj)
	ifort -o $(ex) $(ex_) .o 
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
	-@rm *.o
	-@rm data.* result.txt
e:
	@vim $(ex_).f90
