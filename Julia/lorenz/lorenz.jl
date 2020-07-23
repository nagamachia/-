using Pkg
Pkg.add("Plots")
Pkg.add("GR")
Pkg.add("ParameterizedFunctions")
Pkg.add("DifferentialEquations")
using Plots
using DifferentialEquations
using ParameterizedFunctions
gr()

g = @ode_def LorenzExample begin
  dx = p*(y-x) 
  dy = x*(r-z) - y
  dz = x*y - b*z
end p r b

# u0 = [1.0;0.0;0.0]
# tspan = (0.0,1.0)
# p = [10.0,28.0,8/3]
u0 = [0.0;1.01;0.0]
tspan = (0.0,100.0)
p = [10.0,25.0,7/3]
@time prob = ODEProblem(g,u0,tspan,p)

# sol = solve(prob,Tsit5(),reltol=1e-8,abstol=1e-8)
sol = solve(prob)

# print(sol)

plot(sol,vars=(1,2,3))
savefig("lorenz.png")
