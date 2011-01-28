c -R.T. Jones
c  Versions of trig functions that deal in degrees instead of radians
c -June 7, 2003

	real function cosd(arg)
        real arg
        cosd = cos(arg*0.017453292)
	end

	double precision function dcosd(arg)
        double precision arg
        dcosd = dcos(arg*0.017453292d0)
	end

	real function sind(arg)
        real arg
        sind = sin(arg*0.017453292)
	end

	double precision function dsind(arg)
        double precision arg
        dsind = dsin(arg*0.017453292d0)
	end

	real function tand(arg)
        real arg
        tand = tan(arg*0.017453292)
	end

	double precision function dtand(arg)
        double precision arg
        dtand = dtan(arg*0.017453292d0)
	end

	real function acosd(arg)
        real arg
        acosd = acos(arg)/0.017453292
	end

	double precision function dacosd(arg)
        double precision arg
        dacosd = dacos(arg)/0.017453292d0
	end

	real function asind(arg)
        real arg
        asind = asin(arg)/0.017453292
	end

	double precision function dasind(arg)
        double precision arg
        dasind = dasin(arg)/0.017453292d0
	end

	real function atand(arg)
        real arg
        atand = atan(arg)/0.017453292
        end

	double precision function datand(arg)
        double precision arg
        datand = datan(arg)/0.017453292d0
        end

	real function atan2d(arg1,arg2)
        real arg1,arg2
        atan2d = atan2(arg1,arg2)/0.017453292
        end

	double precision function datan2d(arg1,arg2)
        double precision arg1,arg2
        datan2d = datan2(arg1,arg2)/0.017453292d0
        end
