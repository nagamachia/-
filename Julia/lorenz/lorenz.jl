using Pkg
Pkg.add("ParameterizedFunctions")
Pkg.add("DifferentialEquations")
Pkg.add("GR")
Pkg.add("Plots")
Pkg.add("Gnuplot")
Pkg.add("CPUTime")
using ParameterizedFunctions, DifferentialEquations
using Plots
using Gnuplot, LinearAlgebra
using CPUTime

g = @ode_def LorenzExample begin
    dx = p*(y-x) 
    dy = x*(r-z) - y
    dz = x*y - b*z
  end p r b

u0 = [0.0;1.01;0.0]
tspan = (0.0,100.0)
p = [10.0,25.0,7/3]
prob = ODEProblem(g,u0,tspan,p)

@CPUtime sol = solve(prob,Tsit5(),reltol=1e-8,abstol=1e-8)
# @CPUtime sol = solve(prob,Tsit5())
# @CPUtime sol = solve(prob)

# plot using Plots
function plt()
    gr()
    plot(sol,vars=(1,2,3))
    savefig("lorenz.png")
end

# plot using Gnuplot
function pltgnu()
    x, y, z = sol[1,:], sol[2,:], sol[3,:]
    tempo = sol.t
    @gsp "set xyplane at -3" "set auto fix" "set grid" :-
    @gsp :- x y z tempo "w l notit lc palette" #palette(:plasma)
    @gsp :- "set title 'Lorenz attractor'" "set cblabel 'time'"
    @gsp :- xlab = "x" ylab = "y" zlab = "z"
    save(term="pngcairo size 640,480", output="lorenz_gnu.png")
end

# @CPUtime plt()
@CPUtime pltgnu()