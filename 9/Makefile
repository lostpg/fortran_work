obj = ex0901.o 
.PHONY: mr clean e r
0901: $(obj)
	@ifort -o 0901 ex0901.o 
$(obj): ex0901.f90
	@ifort ex0901.f90 -c
r:
	-@make && ./0901
clean:
	-@rm ./0901
	@rm *.mod *.o
e:
	@vim ex0901.f90
