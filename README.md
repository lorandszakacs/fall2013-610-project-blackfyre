Project Blackfyre
====================
Origin of name: Daemon Blackfyre, pretender to the throne of the Seven Kingdoms. I will let the reader decide on the exact interpretation.

How to re-run the experiment:
in the boa-analyses folder you can find two files: gather-effects.boa and gather-effects.boa
These programs can be run at http://boa.cs.iastate.edu/ but you need an account. Alternatively, their results are available at:
gather-classes.boa:		http://boa.cs.iastate.edu/boa/?q=boa/job/public/2750
gather-effects.boa:		http://boa.cs.iastate.edu/boa/?q=boa/job/public/2751

The output of these two programs is processed with the scala-data-aggregator program. It can be built using "Simple build tool". Because of laziness the paths to the data files is hardcoded in Main.scala so you would have to change that. Make sure to give the jvm more memory(~2GB) when running this program, otherwise you will run out of it. The results are written to a file whose name and path are also hardcoded in Main.scala.

