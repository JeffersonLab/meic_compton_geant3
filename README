                Compton Polarimeter Simulation for Qweak
		----------------------------------------

	- R.T. Jones, Univ of Connecticut (Qweak maintainer)
	- D. Gaskell, JLab (Hall C modifications)
	- Pat Welch, Univ of Oregon (original author)


I. Simulation Overview

This simulation contains the setup for a Compton chicane and detectors
for a Compton backscatter polarimeter.  It is based on the CERNLIB package
Geant3.  In addition to Compton scattering, the simulation is able to
produce backgrounds from beam-gas interactions, beam halo interactions in
the chicane elements, synchrotron radiation in the chicane dipoles, and
gammas from a radioactive source used for continuous calibration.  The
parameters for the simulation are set in control.in, an ASCII input file
that is the standard way that Geant3 obtains its settings.  In addition
to the standard Geant controls in control.in, a custom set of keys have
been defined to set up specific aspects of the polarimeter.

   A. bremsstrahlung (interactions between primary beam and residual gas)
   B. backscatter (Compton scattering between laser and primary beam)
   C. beam halo (tails of the electron beam that sometimes scrape)
   D. synchrotron (radiation from primary beam in dipoles)
   E. source lines (radioactive source for continuous calibration)

The above five basic simulation modes can be individually turned on/off
using the control.in TYPE line.  In principle they are independent and
can be generated together or in separate runs.  However one thing to keep
in mind is that when bremsstrahlung is turned on, the beam pipes are filled
with background gas (H2 at present) at STP instead of the usual vacuum.
This is done in order to obtain reasonable bremsstrahlung rates, but can
interfere with the simulation of other processes, eg. synchrotron radiation.
As a result it is recommended that bremsstrahlung simulations be carried
out in separate runs from the other processes.


II. Electron Beam

The electron beam is described by a Gaussian core surrounded by a diffuse
halo with a power-law radial and angulr dependence.  The core Gaussian is
described by the beam emittance, beta and radius (independent in x and y) at
the entrance to the chicane.  This generation is done in genbeam().  The halo
is produced in genhalo().


III. Output

The output from the simulation is provided in the form of hbook ntuples.
These ntuples can be interpreted using paw, to produce histograms of the
results.  Separate ntuples are produced for each of the above 5 simulation
modes.  The ntuple titles indicate the mode that produced them.  Each ntuple
cotains the following information.

   A. vertx(1..3) - vertex origin of primary track (cm)
   B. vertp(1..3) - vertex momentum of primary track (cm)
   C. Egamma - total energy deposited in gamma detector (GeV)
   D. nstrips - total number of microstrips hit in electron detector
   E. strip(1..nstrips) - index of microstrip i, closest to beam =1
   F. Estrip(1..nstrips) - energy deposited in microstrip i (GeV)
   G. wXSect - cross section weight factor (see next section)
   H. wLumin - cross section weight factor (see next section)
   I. apower - polarization asymmetry for this process


IV. Normalization

The backscatter simulation produces one Compton gamma per event.  This is
artificial in the sense that you get the same number of backscatters no
matter how intense the laser and beam are, or the quality of the alignment.
This is done to obtain adequate simulation efficiency, and is turned into
actual rates by the use of weight factors.  The power of this technique is
demonstrated by the diversity of information that can be extracted from the
same data set by using different combinations of weights.  In the output
each event is provided with 3 weight factors.

   A. cross section weight (wXSect)
   B. luminosity weight (wLumin)
   C. polarization asymmetry (apower)

The generator produces Compton scattering with a uniform angular distribution
in the cm frame; to correct for the Compton differential cross section you
have to weight the events with wXSect.  Plots of simulation variables with
this weight will show results in units of barns.  Furthemore, weighting them
with an additional factor of wLumin/N will show rates in 1/s, where N is the
number of events in the simulation.  In this way, running more simulation
events improves the statistical quality of the results but not the mean.
To obtain the difference between the rates with opposite beam polarization,
an additional factor of apower*p should be used in the weight, where p is
the product of the electron and laser beam polarization.

For background processes, where the production obeys the physical cross
section, the factor wXSect is unity and the wLumin factor carries the
correction necessary to convert counts to rates.  In general the apower
for background processes is zero.


V. Beam-Laser Intersection

The luminosity of the beam-laser crossing has to be calculated by the Monte
Carlo because it is a numerical integral for the general case.  The transverse
size of the backscatter source is set by the beam radius at the interaction
point.  The longitudinal position of the backscatter vertex is generated
uniformly in a z-interval that is supplied by the user as input (see line
BACKSCATTER in control.in) and it should be verified for any given geometry
of the laser and beam that the z-interval is long enough to contain the entire
crossing zone.  This can be checked by plotting vertx(3) from a Compton
backscatter ntuple.

After the event vertex is generated, it is rotated into the frame of the
laser and the laser intensity is sampled.  This flux becomes a factor in the
event luminosity weight wLumin.  The laser profile is specified as a
cylindrical Gaussian with a Raleigh range (called beta in the code) and
waist size (called sigma in the code).  These parameters together with the
crossing angle and the coordinates of the focus are specified in control.in
on a line beginning LASER.  The laser spectral line width is also supplied.
To generate the scattering event, the incident angles are calculated from
directions sampled from the laser and electron beam distributions.  The cm
scattered angle is then generated on cos(theta) ~ Unif[-1,1] and the event
transformed back to the lab frame.  The differential cross section
d[sigma]/d[Omega] is supplied as a weight factor wXSect for the event.  The
Compton cross section is actually calculated as d[sigma]/d[kf] (see Compton.F)
so the weight actually needs to contain an extra factor of d[kf]/d[Omega].

At the same time as the backscattered gamma is generated, a recoil electron
track is also produced that will be bent by the chicane and perhaps end up
hitting the electron counter.  Note that the backscatter simulation does not
actually follow the electron from the start of the chicane up to the Compton
vertex, but simply produces both gamma and recoil electron at the interaction
point.  This is the only part that is relevant in backscatter mode.  In other
modes such as bremsstrahlung and synchrotron radiation, electrons are tracked
from the beginning of the chicane.


VI. Detector Response

The response of the detector to energy deposited in it is not a part of
this simulation.  It should be included as a part of the subsequent
analysis of the ntuple results.  This way one can study the effects of
changes in the detector response without the expense of having to repeat
the entire simulation.
response


VII. Building Executables

In directory wherein lies this README, locate the Makefile.  Open it in an
editor and check that the declarations of CERN_ROOT is correct for your
environment.  Below this are a number of sections for various unix flavors
that try to set up the compiler switches appropriate to your machine.  If
there are problems on the next step, go back and look at the section that
applies to your environment and make the necessary changes.  To build the
executables, simply type the following commands.

  A. make compton
  B. make compton++

The first is the standard batch version of Geant and the second is the
interactive version.  When you start up the interactive version, instead
of providing the index 1 for the display number, try typing the letter m.
This will start up the windowing interface that is like paw++, with Geant
extensions.  This environment is useful for making plots and drawing
pictures of the geometry.


VIII. Directory Listing

main.F		standard Geant3 main program
uginit.F	standard Geant3 initialization
ugffgo.F	defines Compton data cards, reads from control.in
uglast.F	standard Geant3 finalization

ugeom.F		master materials+media+geometry file
ugmate.F	define materials
ugstmed.F	stub interface to gstmed()
ugsvert.F	stub interface to gsvert()
ugsvolu.F	stub interface to gsvolu()

define_cave.F		defines master volume
define_detector.F	defines detector geometry
define_lattice.F	defines chicane geometry
define_vacuum.F		defines beam pipe and exit port

gukine.F	generates electron events and calls do_compton for backscatter
genbeam.F	generates position and momentum of electron in the beam core
genhalo.F	generates position and momentum of electron in the beam halo
do_compton.F	generates Compton backscatter, luminosity and weight factors
compton.F	computes Compton cross section and asymmetry

gutrev.F	standard Geant event tracking
ugskine.F	stub interface to gskine()
gustep.F	registers hits in detector
gufld.F		uniform fields for dipoles
gudigi.F	generates response in detector

gxcs.F		fix for geant++
gxint321.F	main interactive program

histo_init.F	initialize histograms for output
trig.F		provides trig functions that work in degrees

materials.database	ASCII file containing description of materials
control.in	data cards that control a run (see file for documentation)

params.inc	constants related to particles and materials
trig.inc	header file for functions in trig.F
ugeom.inc	settings for tracking media
user.inc	main collection of customizable simulation settings
