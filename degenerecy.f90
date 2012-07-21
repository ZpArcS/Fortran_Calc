! Program to calculate Degeneracy in cubic space in Quantum chemitry
! @author ZpArc.S
! @July 14th 2012

!-----------------------------------------------------------------------------------------------------------------
program main 
	implicit none 
	integer :: i, j, k, n, deg
	deg = 0
	print*, "Please input n(Quantum number): "
	read*, n
	
	do i = 1, n
		do j = 1, n
			do k = 1, n
				if (i**2+j**2+k**2 == n**2) then 
					print*, i, j, k
					deg = deg+1
				end if
			end do
		end do
	end do
	print*, "The degeneracy is ", deg, " for the quantum number ", n
end program main