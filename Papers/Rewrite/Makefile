all: looppng redundantpng

looppng: loop1.png loop2.png loop3.png loop4.png loop5.png loop6.png loop7.png
redundantpng: redundant1.png redundant2.png redundant3.png redundant4.png redundant5.png redundant6.png redundant7.png redundant8.png

%.png: %.dot
	dot -Tpng $< >$@

clean:
	rm -f *.png *~
