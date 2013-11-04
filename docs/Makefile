all : LagrangePolynomials.pdf

LagrangePolynomials.pdf : ../src/Hascm/LagrangePolynomials.lhs
	pdflatex ../src/Hascm/LagrangePolynomials.lhs
	pdflatex ../src/Hascm/LagrangePolynomials.lhs

clean :
	rm -f *.aux *.log *.pdf
