	real FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN
	parameter (FIELDM = 10.)	! maximum mag field
	parameter (TMAXFD = 1.00)	! max deflection due to mag field (deg)
	parameter (DEEMAX = 1e-3)	! max energy loss in one step
	parameter (EPSIL  = 0.01)	! tracking accuracy in cm
	parameter (STMIN  = 1e-5)	! minimum step size
	parameter (STEMAX = 1e10)	! maximum step size
