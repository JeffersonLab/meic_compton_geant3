c Particle id's as found in the routine GPART
	integer gamma_id,electron_id,pi0_id,pip_id,pim_id
	integer neutron_id,proton_id
	
	parameter (gamma_id	= 1)	! particle id for an gamma
	parameter (electron_id	= 3)	! particle id for an electron
	parameter (pi0_id	= 7)	! particle id for an pi0
	parameter (pip_id	= 8)	! particle id for an pi+
	parameter (pim_id	= 9)	! particle id for an pi+
	parameter (neutron_id	= 13)	! particle id for an neutron
	parameter (proton_id	= 14)	! particle id for an proton

c Mass of various particles
	real mass_electron,mass_proton,mass_pi0
	parameter (mass_electron = 0.51099906d-3)	! Electron mass in GeV
	parameter (mass_proton   = 938.27231d-3)	! Proton mass in GeV
	parameter (mass_pi0	= 134.9739d-3)	        ! pi0 mass in GeV

c Materials ids
	integer HYDROGEN_MATE,DEUTERIUM_MATE,HE4_MATE,LITHIUM_MATE
	integer	BERYLLIUM_MATE,CARBON_MATE,NITROGEN_MATE,NEON_MATE
	integer	ALUMINUM_MATE,IRON_MATE,COPPER_MATE,TUNGSTEN_MATE
	integer	LEAD_MATE,URANIUM_MATE,AIR_MATE,VACUUM_MATE,CA40_MATE
	integer	GERMANIUM_MATE,MYLAR_MATE,PYREX_MATE,FREON_MATE,CO2_MATE
	integer	POLYETHYLENE_MATE,NE102_MATE,HAVAR_MATE,KAPTON_MATE
	integer	NaI_MATE,TARGET_MATE,NaBi_MATE,CsI_MATE,MYVAC_MATE
        integer PBWO4_MATE,SILICON_MATE,DIAMOND_MATE

	parameter (HYDROGEN_MATE = 1)		! Standard H
	parameter (DEUTERIUM_MATE= 2)		! Standard D
	parameter (HE4_MATE	= 3)		! Standard Helium-4
	parameter (LITHIUM_MATE	= 4)		! Standard Lithium (6&7)
	parameter (BERYLLIUM_MATE= 5)		! Standard Be
	parameter (CARBON_MATE   = 6)		! Standard C (natural)
	parameter (NITROGEN_MATE = 7)
	parameter (NEON_MATE	= 8)
	parameter (ALUMINUM_MATE = 9)
	parameter (IRON_MATE	= 10)
	parameter (COPPER_MATE	= 11)
	parameter (TUNGSTEN_MATE = 12)
	parameter (LEAD_MATE	= 13)
	parameter (URANIUM_MATE	= 14)
	parameter (AIR_MATE	= 15)
	parameter (VACUUM_MATE	= 16)
	parameter (CA40_MATE	= 17)		! User defined calcium-40
	parameter (GERMANIUM_MATE= 18)
	parameter (MYLAR_MATE    = 19)
	parameter (PYREX_MATE	= 20)
	parameter (FREON_MATE	= 21)
	parameter (CO2_MATE	= 22)
	parameter (POLYETHYLENE_MATE = 23)
	parameter (NE102_MATE    = 24)
	parameter (HAVAR_MATE    = 25)
	parameter (KAPTON_MATE	= 26)
	parameter (NaI_MATE	= 27)
	parameter (TARGET_MATE	= 28)		! the target material is
        parameter (NaBi_MATE	= 29)	
	parameter (CsI_MATE	= 30)
	parameter (MYVAC_MATE	= 31)		! my vaccuum
	parameter (PBWO4_MATE	= 32)		! lead tungstate
	parameter (SILICON_MATE	= 33)
        parameter (DIAMOND_MATE = 34)

c Colors in GEANT
	integer	BLACK,RED,GREEN,BLUE,YELLOW,PURPLE,LIGHTBLUE

	parameter (BLACK = 1)
	parameter (RED	= 2)
	parameter (GREEN = 3)
	parameter (BLUE	= 4)
	parameter (YELLOW= 5)
	parameter (PURPLE= 6)
	parameter (LIGHTBLUE= 7)

c Various physical constants
	real PI,c	

	parameter (PI = 3.14159)
	parameter (c  = 2.99792458d10)		! cm/sec
