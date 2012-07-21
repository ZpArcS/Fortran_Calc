module triangleArea
	implicit none
	contains 
		function Area(x, y, z)
			real :: Area
			real, intent(in) :: x, y, z
			real :: theta, height
			
			theta = ACOS ((x**2+y**2-z**2)/(2.0*x*y))
			height = x*SIN(theta)
			Area = 0.5*height*y
		end function Area
end module triangleArea

program Triangle
	use TriangleArea
	implicit none
	real :: a, b, c
	print*, "This program is to calculate the area of triangle using a, b and c"
	print*, "Please enter a, b, c"
	read*, a, b, c
	print*, 'The area of the triangle is ', Area(a, b, c), ' . '
end program Triangle