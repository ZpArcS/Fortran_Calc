module gauss
    contains 

    subroutine solve (A, b, x, N)
        implicit real*8(a-z)
        integer :: i, k, N
        real*8 :: A(N, N), b(N), x(N)
        real*8 :: Aup(N, N), bup(N)
        real*8 :: Ab(N, N+1)
        Ab(1:N, 1:N) =A
        Ab(:, N+1) = b

        do k =1, N-1
            do i = k+1, N
                temp = Ab(i, k)/Ab(k, k)
                Ab(i, :) = Ab(i, :)-temp*Ab(k, :)
            end do
        end do

        Aup(:, :) = Ab(1:N, 1:N)
        bup(:) = Ab(:, N+1)
        call uptri(Aup, bup, x, N)
    end subroutine solve

    subroutine uptri(A, b, x, N)
        implicit real*8(a-z)
        integer :: i, j, N
        real*8 :: A(N, N), b(N), x(N)
        x(N) = b(N)/A(N, N)
        
        do i = n-1, 1, -1
            x(i) = b(i)
            do j = i+1, N
                x(i) = x(i) - a(i, j)*x(j)
            end do
            x(i) = x(i)/A(i, i)
        end do
    end subroutine uptri

end module gauss

program main
    use gauss
    implicit real*8(a-z)
    integer, parameter :: N = 4
    integer :: i, j, k
    real*8 :: A(N, N), b(N), x(N)

    open (unit=11, file = 'ina.txt')
    open (unit = 10, file = 'inb.txt')
    open (unit= 12, file = 'out.txt')
    read(11, *)
    read(11, *)((A(i, j), j = 1, N), i = 1,N)
    read(10, *)
    do k = 1, N
        read(10, *)b(k)
    end do
    call solve(A, b, x, N)
    write(12, 101)x
    101 format(T5, 'Result of guassian elimination', /, T4, "x= " , 4(/F12.8))
end program main















        