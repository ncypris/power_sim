# price
N = 600
# Player A (take) + Player A (notake) + Player B (notake) + Player B (take/nopun) + Player C (nopun) + Player C (pun/cpun) + Player C (pun/nocpun)
price <- .35*1.5*N/3 + .65*4.5*N/3  + .35*6*N/3 + .5*.65*8.5*N/3 + .5*11.25*N/3 + .5*.35*4.5*N/3 + .5*.65*10.75*N/3
price

# Price pP
price/6