ex=1001
ex_=ex$(ex)
obj = $(ex_).o 
ifor=$(shell which ifort)
.PHONY: mr clean e r
ifneq ($(ifor),)
$(ex): $(obj)
	ifort -o $(ex) $(obj) 
$(obj): $(ex_).f90
	ifort $(ex_).f90 -c -o $(obj)
else
$(ex): $(obj)
	gfortran -o $(ex) $(obj)
$(obj): $(ex_).f90
	gfortran $(ex_).f90 -c -o $(obj)
endif
r:
	-@make && ./$(ex)
clean:
	-@rm ./$(ex)
	-@rm *.o
	-@rm data.* result.txt
e:
	@vim $(ex_).f90
