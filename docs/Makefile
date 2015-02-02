all : LagrangePolynomials.pdf

LagrangePolynomials.pdf : ../src/Dyno/LagrangePolynomials.lhs
	pdflatex ../src/Dyno/LagrangePolynomials.lhs
	pdflatex ../src/Dyno/LagrangePolynomials.lhs

clean :
	rm -f *.aux *.log *.pdf
