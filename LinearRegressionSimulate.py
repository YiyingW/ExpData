import numpy as np 
from scipy import stats
import pylab as pl 

base = 4.4
r_squared = []
Kd = []
changes = []

for i in range(0,5000):
	change = base+0.001*i
	changes.append(change)
	PIC_conc = np.array([20, 50, 150, 400, 500])
	bound = [0.622, 1.276, 2.572, 4.403, change]
	y = bound/PIC_conc
	regression = np.polyfit(bound, y, 1)


	slope, intercept, r_value, p_value, std_err = stats.linregress(bound, y)
	r_squared.append(r_value**2)
	Kd.append(-1/slope)


	print "If bound at 500 uM is {:f}".format(change)
	print "y = {:f} x + {:f}".format(slope, intercept)
	print "r_squared: {:f}".format(r_value**2)
	print "Kd = {:f}".format(-1/slope)

print max(r_squared)
max_index = r_squared.index(max(r_squared))
print Kd[max_index]


print min(Kd)
min_index = Kd.index(min(Kd))
print r_squared[min_index]
print changes[min_index]

# pl.plot(changes, Kd)
# pl.xlabel('simulated bound concentration for 500 uM PIC')
# pl.ylabel('Kd (uM)')
# pl.show()


pl.plot(changes, r_squared, 'g')
pl.xlabel('simulated bound concentration for 500 uM PIC')
pl.ylabel('R^2')
pl.show()

