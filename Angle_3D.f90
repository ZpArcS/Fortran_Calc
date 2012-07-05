! Calculate the angle of two vector in 3D
!July 3th 2012
!@author ZpArc.S
!-------------------------------------------------------------------------------------------------------------------------------------------------------------
! main body
program angle_2D
    implicit none
    real, dimension (3) :: v1, v2   ! declare 2 vector 
    real :: angle

    !Initialize two vector 
    !Prompt user input
    print*, "Please enter the x. y. z of  vector 1"
    read*, v1(1), v1(2), v1(3)
    print*, "Please enter the x. y. z of  vector 2"
    read*, v2(1), v2(2), v2(3)

    !Display result
    print*, "The 3D angle between v1 and v2 equals  ", angle(v1, v2)
end program angle_2D    ! End of  program

! calculation part for angle
function angle(vet1, vet2)
    implicit none
    real:: angle
    real , dimension(3), intent(in):: vet1, vet2
    real :: cos, norm
    cos = vet1(1)*vet2(1)+vet1(2)*vet2(2)+vet1(3)*vet2(3)
    cos = cos/(norm(vet1)*norm(vet2))
    angle = acos(cos)
end function angle

! Calculation part for norm in angle
function norm(vet)
implicit none
    real norm
    real, dimension(3) :: vet
    norm  = sqrt(vet(1)*vet(1)+vet(2)*vet(2)+vet(3)*vet(3))
end function norm

