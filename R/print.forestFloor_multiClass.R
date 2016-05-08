
print.forestFloor_multiClass = function(x,...) {
  cat("this is a forestFloor_multiClass object \n
      this object can be plotted in 2D with plot(x), see help(plot.forestFloor) \n
      this object can be plotted in 3D with show3d(x), see help(show3d) \n
      this object can be plotted in 3D with plot_simplex3, see help(plot_simplex3) \n
      \n
      x contains following internal elements: \n ",with(x,ls()))
}